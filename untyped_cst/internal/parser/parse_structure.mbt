///|
fn token_label(tok : Token) -> String? {
  match tok {
    SEMI(_) => Some("sep")
    EOF => Some("eof")
    _ => Some("item")
  }
}

///|
// Keep this lookahead aligned with parse_visibility_clause, including its
// recovery behavior for invalid and missing `pub(...)` modifiers.
fn top_item_lookahead_after_visibility(
  state : ParserState,
  offset : Int,
  is_pub : Bool,
) -> Int {
  let mut offset = offset + 1
  if !is_pub {
    return offset
  }
  match state.peek(offset~) {
    Some((LPAREN, _, _)) => offset += 1
    _ => return offset
  }
  match state.peek(offset~) {
    Some((LIDENT(_), _, _)) | Some((READONLY, _, _)) | Some((EXTEND, _, _)) => {
      offset += 1
      match state.peek(offset~) {
        Some((RPAREN, _, _)) => offset += 1
        _ => ()
      }
    }
    _ => {
      let mut done = false
      while !done {
        match state.peek(offset~) {
          Some((RPAREN, _, _)) => {
            offset += 1
            done = true
          }
          Some((EOF, _, _)) | None => done = true
          Some(_) => offset += 1
        }
      }
    }
  }
  offset
}

///|
fn peek_top_item_kind(state : ParserState) -> TopItemKind {
  let mut offset = 0
  let mut result = TopItemKind::Invalid
  let mut saw_async = false
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((ATTRIBUTE(_), _, _)) | Some((DECLARE, _, _)) => offset += 1
      Some((PUB, _, _)) =>
        offset = top_item_lookahead_after_visibility(state, offset, true)
      Some((PRIV, _, _)) =>
        offset = top_item_lookahead_after_visibility(state, offset, false)
      Some((ASYNC, _, _)) => {
        saw_async = true
        offset += 1
      }
      Some((EXTERN, _, _)) => {
        result = Function
        done = true
      }
      Some((TEST, _, _)) => {
        result = Test
        done = true
      }
      Some((USING, _, _)) => {
        result = Using
        done = true
      }
      Some((IMPORT, _, _)) => {
        result = Import
        done = true
      }
      Some((EXTEND, _, _)) => {
        result = Extend
        done = true
      }
      Some((LET, _, _)) => {
        result = Let
        done = true
      }
      Some((tok, _, _)) if decl_keyword_expects_name(tok) => {
        result = decl_keyword_kind(tok)
        done = true
      }
      Some((QUESTION, _, _)) => offset += 1
      _ => {
        result = if saw_async {
          TopItemKind::Function
        } else {
          TopItemKind::Invalid
        }
        done = true
      }
    }
  }
  result
}

///|
fn ParserState::node_from_children(
  self : ParserState,
  children : Array[(String?, CstNode)],
) -> (Location, (Int, Int)) {
  if children.length() == 0 {
    let pos = self.current_position()
    (empty_loc(pos), empty_span(self.offset_for_position(pos)))
  } else {
    let (_, first) = children[0]
    let (_, last) = children[children.length() - 1]
    (first.loc.merge(last.loc), (first.source_span.0, last.source_span.1))
  }
}

///|
fn ParserState::construct_from_children(
  self : ParserState,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  children : Array[(String?, CstNode)],
) -> CstNode {
  // Fix the syntax range before the builder appends semantic children.
  let (loc, source_span) = self.node_from_children(children)
  node(builder, loc, source_span, children)
}

///|
fn parse_modern_decl_prefixes(
  state : ParserState,
  children : Array[(String?, CstNode)],
  allow_attrs~ : Bool,
  allow_visibility~ : Bool,
) -> Unit {
  let mut done = false
  let mut saw_visibility = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((ATTRIBUTE(_), start, end)) => {
        if !allow_attrs {
          state.report(
            mk_loc(start, end),
            "Import declarations cannot have attributes or other modifiers.",
          )
        } else if saw_visibility {
          state.report(
            mk_loc(start, end),
            "Attributes must appear before a visibility modifier.",
          )
        }
        if state.consume_any() is Some(attr) {
          push_child(children, Some("attrs"), attr)
        }
      }
      Some((PUB, start, end)) | Some((PRIV, start, end)) => {
        if !allow_visibility {
          state.report(
            mk_loc(start, end),
            "Import declarations cannot have visibility or other modifiers.",
          )
        } else if saw_visibility {
          state.report(
            mk_loc(start, end),
            "Only one visibility modifier is allowed.",
          )
        }
        push_child(children, Some("vis"), parse_visibility_clause(state))
        saw_visibility = true
      }
      Some((DECLARE, start, end)) | Some((ASYNC, start, end)) => {
        state.report(
          mk_loc(start, end),
          "This modifier is not allowed on this declaration.",
        )
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        }
      }
      _ => done = true
    }
  }
}

///|
fn parse_import_package_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((STRING(_), _, _)) =>
      if state.consume_any() is Some(path) {
        push_child(children, Some("path"), path)
      }
    _ =>
      push_child(
        children,
        Some("path"),
        state.missing_token_node_with_expected(TK_STRING, [TK_STRING]),
      )
  }
  push_children(children, state.consume_trivia())
  match state.peek() {
    Some((PACKAGE_NAME(_), _, _)) =>
      if state.consume_any() is Some(alias_) {
        push_child(children, Some("alias"), alias_)
      }
    _ => ()
  }
  if state.current_is(TK_STAR) {
    push_child(children, Some("op"), state.expect_token(TK_STAR))
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::clause_import_package(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_extend_name_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(target) {
        push_child(children, Some("target"), target)
      }
    _ =>
      push_child(
        children,
        Some("target"),
        state.missing_token_node_with_expected(TK_LIDENT, [TK_LIDENT, TK_EXTEND]),
      )
  }
  push_children(children, state.consume_trivia())
  if state.current_is(TK_AS) {
    push_child(children, Some("kw"), state.expect_token(TK_AS))
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        }
      _ =>
        push_child(
          children,
          Some("name"),
          state.missing_token_node_with_expected(TK_LIDENT, [
            TK_LIDENT,
            TK_EXTEND,
          ]),
        )
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_structured_name(
        children,
        "semantic_name",
        fn(loc, source_span, children) {
          let long_ident = @construction.first_child_with_label_from(
            children, "long_ident",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::name_alias_target(loc~, source_span~, long_ident~)
        },
        ["target", "name"],
      )

      let entries = recovery_children(children)
      @node.CstNode::clause_extend_item(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_import_decl(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  parse_modern_decl_prefixes(
    state,
    children,
    allow_attrs=false,
    allow_visibility=false,
  )
  push_child(children, Some("kw"), state.expect_token(TK_IMPORT))
  push_children(children, state.consume_trivia())
  if state.current_is(TK_STRING) {
    let old = state.expect_token(TK_STRING)
    state.report(
      old.loc,
      "Deprecated import syntax; use `import { \"path\" }`.",
    )
    push_child(children, Some("error"), old)
  } else {
    push_child(
      children,
      Some("body"),
      parse_delimited(
        state,
        TK_LBRACE,
        TK_RBRACE,
        parse_import_package_item,
        fn(loc, source_span, children) {
          let entries = list_children(children)
          @node.CstNode::list_import_package_list(loc~, source_span~, entries~)
        },
        sep_kind=TK_COMMA,
      ),
    )
    push_children(children, state.consume_trivia())
    if state.current_is(TK_FOR) {
      push_child(children, Some("kw"), state.expect_token(TK_FOR))
      push_children(children, state.consume_trivia())
      match state.peek() {
        Some((STRING(_), _, _)) =>
          if state.consume_any() is Some(kind) {
            push_child(children, Some("kind"), kind)
          }
        _ =>
          push_child(
            children,
            Some("kind"),
            state.missing_token_node_with_expected(TK_STRING, [TK_STRING]),
          )
      }
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = declaration_children(children)
      @node.CstNode::impl_import(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_extend_decl(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  parse_modern_decl_prefixes(
    state,
    children,
    allow_attrs=true,
    allow_visibility=true,
  )
  push_child(children, Some("kw"), state.expect_token(TK_EXTEND))
  push_children(children, state.consume_trivia())
  push_child(children, Some("type_name"), parse_type_name_node(state))
  push_children(children, state.consume_trivia())
  push_child(children, Some("kw"), state.expect_token(TK_WITH))
  push_children(children, state.consume_trivia())
  push_child(children, Some("trait"), parse_type_name_node(state))
  push_children(children, state.consume_trivia())
  push_child(children, Some("op"), state.expect_token(TK_COLONCOLON))
  push_children(children, state.consume_trivia())
  push_child(
    children,
    Some("body"),
    parse_delimited(
      state,
      TK_LBRACE,
      TK_RBRACE,
      parse_extend_name_item,
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_extend_item_list(loc~, source_span~, entries~)
      },
      sep_kind=TK_COMMA,
    ),
  )
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = declaration_children(children)
      @node.CstNode::impl_extend(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_top_level_item(state : ParserState) -> CstNode {
  let first_kind = peek_top_item_kind(state)
  match first_kind {
    Import => parse_import_decl(state)
    Extend => parse_extend_decl(state)
    _ => parse_balanced_decl_body(state, first_kind)
  }
}

///|
fn parse_source_file(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut saw_eof = false
  let mut done = false
  while !done {
    match state.peek() {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) =>
        for child in state.consume_trivia() {
          children.push(child)
        }
      Some((SEMI(_), _, _)) =>
        if state.consume_any() is Some(sep) {
          children.push((Some("sep"), sep))
        } else {
          ()
        }
      Some((EOF, _, _)) => {
        if state.consume_any() is Some(eof) {
          children.push((Some("eof"), eof))
        } else {
          ()
        }
        saw_eof = true
        done = true
      }
      Some((_, start, end)) => {
        if state.report_next_top_level_start {
          state.report_next_top_level_start = false
          state.report(mk_loc(start, end), "Unexpected token here.")
        }
        children.push((Some("item"), parse_top_level_item(state)))
      }
      None => done = true
    }
  }
  if !saw_eof {
    children.push((Some("eof"), state.missing_eof_node()))
  }
  node(
    fn(loc, source_span, children) {
      let entries = root_children(children)
      @node.CstNode::impls(loc~, source_span~, entries~)
    },
    state.root_loc(),
    state.root_span(),
    children,
  )
}
