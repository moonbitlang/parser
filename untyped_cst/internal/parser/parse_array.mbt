///|
fn parse_array_expr(state : ParserState) -> CstNode {
  let open_kind = match state.peek() {
    Some((LBRACKET_BAR, _, _)) => TokenKind::TK_LBRACKET_BAR
    _ => TK_LBRACKET
  }
  let close_kind = match open_kind {
    TK_LBRACKET_BAR => TokenKind::TK_BAR_RBRACKET
    _ => TK_RBRACKET
  }
  if array_starts_list_comprehension(state) {
    return parse_list_comprehension_expr(state, open_kind, close_kind)
  }
  let list = parse_delimited(
    state,
    open_kind,
    close_kind,
    parse_array_expr_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_array_element_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
  )
  let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if list_has_item_kind(
      list,
      fn(kind) { kind is ArrayElement_Spread },
    ) {
    fn(loc, source_span, children) {
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let array_kind = @construction.first_child_with_label_from(
        children, "array_kind",
      ).unwrap_or(missing_at_end(loc~, source_span~))
      @node.CstNode::expr_array_spread(loc~, source_span~, body~, array_kind~)
    }
  } else {
    fn(loc, source_span, children) {
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let array_kind = @construction.first_child_with_label_from(
        children, "array_kind",
      ).unwrap_or(missing_at_end(loc~, source_span~))
      @node.CstNode::expr_array(loc~, source_span~, body~, array_kind~)
    }
  }
  let children : Array[(String?, CstNode)] = [(Some("body"), list)]
  push_child(
    children,
    Some("array_kind"),
    marker_node(
      if open_kind is TK_LBRACKET_BAR {
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::array_kind_iter(loc~, source_span~)
        }
      } else {
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::array_kind_array(loc~, source_span~)
        }
      },
      list.loc.start,
      list.source_span.0,
    ),
  )
  node(builder, list.loc, list.source_span, children)
}

///|
fn array_starts_list_comprehension(state : ParserState) -> Bool {
  let mut offset = 1
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((FOR, _, _)) => return true
      _ => done = true
    }
  }
  false
}

///|
fn parse_list_comprehension_expr(
  state : ParserState,
  open_kind : TokenKind,
  close_kind : TokenKind,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_children(children, state.consume_trivia())
  push_child(children, Some("open"), state.expect_token(open_kind))
  push_children(children, state.consume_trivia())
  let for_loc = match state.peek() {
    Some((_, start, end)) => mk_loc(start, end)
    None => state.root_loc()
  }
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let is_foreach = list_comprehension_is_foreach(state)
  let invalid_trailing_body = if is_foreach {
    parse_list_comprehension_foreach_header(state, children, close_kind)
  } else {
    parse_list_comprehension_for_header(state, children, close_kind, for_loc)
  }
  if invalid_trailing_body {
    let body = parse_invalid_list_comprehension_body(state, close_kind)
    if !body.children.is_empty() {
      push_child(children, Some("body"), body)
    }
  } else {
    if state.current_is(TK_IF) {
      if state.consume_any() is Some(kw) {
        push_child(children, Some("kw"), kw)
      } else {
        ()
      }
      let guard_expr = parse_expr_until(state, [
        TK_FAT_ARROW,
        close_kind,
        TK_EOF,
      ])
      if !guard_expr.children.is_empty() {
        push_child(children, Some("guard"), guard_expr)
      }
    }
    if state.current_is(TK_FAT_ARROW) {
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
    }
    let body = parse_expr_until(state, [close_kind, TK_EOF])
    if !body.children.is_empty() {
      push_child(children, Some("body"), body)
    }
  }
  push_children(children, state.consume_trivia())
  push_child(children, Some("close"), state.expect_token(close_kind))
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_marker_child(
        children,
        loc,
        source_span,
        "list_comprehension_kind",
        if is_foreach {
          fn(loc, source_span, children) {
            let _ = children
            @node.CstNode::list_comprehension_kind_for_each(loc~, source_span~)
          }
        } else {
          fn(loc, source_span, children) {
            let _ = children
            @node.CstNode::list_comprehension_kind_for(loc~, source_span~)
          }
        },
      )
      push_marker_child(
        children,
        loc,
        source_span,
        "array_kind",
        if open_kind is TokenKind::TK_LBRACKET_BAR {
          fn(loc, source_span, children) {
            let _ = children
            @node.CstNode::array_kind_iter(loc~, source_span~)
          }
        } else {
          fn(loc, source_span, children) {
            let _ = children
            @node.CstNode::array_kind_array(loc~, source_span~)
          }
        },
      )

      let entries = recovery_children(children)
      @node.CstNode::expr_list_comprehension(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn list_comprehension_is_foreach(state : ParserState) -> Bool {
  let mut offset = 0
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((IN, _, _)) => return true
      Some((COMMA, _, _)) => {
        offset += 1
        let mut skipping = true
        while skipping {
          match state.peek(offset~) {
            Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
            _ => skipping = false
          }
        }
        match state.peek(offset~) {
          Some((IN, _, _)) => return true
          Some((LIDENT(_), _, _))
          | Some((EXTEND, _, _))
          | Some((UNDERSCORE, _, _)) => offset += 1
          _ => return false
        }
      }
      Some((EQUAL, _, _))
      | Some((SEMI(_), _, _))
      | Some((FAT_ARROW, _, _))
      | Some((IF, _, _))
      | Some((EOF, _, _)) => done = true
      Some(_) => offset += 1
      None => done = true
    }
  }
  false
}

///|
fn list_comprehension_expr_stops(close_kind : TokenKind) -> Array[TokenKind] {
  [TK_SEMI, TK_NEWLINE, TK_IF, TK_FAT_ARROW, close_kind, TK_EOF]
}

///|
fn list_comprehension_binding_stops(close_kind : TokenKind) -> Array[TokenKind] {
  [TK_COMMA, TK_SEMI, TK_NEWLINE, TK_IF, TK_FAT_ARROW, close_kind, TK_EOF]
}

///|
fn ParserState::current_starts_after_node_line(
  self : ParserState,
  item : CstNode,
) -> Bool {
  match self.peek() {
    Some((_, start, _)) => start.lnum > item.loc.end.lnum
    None => false
  }
}

///|
fn ParserState::current_token_line(self : ParserState) -> Int? {
  match self.peek() {
    Some((_, start, _)) => Some(start.lnum)
    None => None
  }
}

///|
fn ParserState::current_starts_after_line(
  self : ParserState,
  line : Int,
) -> Bool {
  if self.current_token_line() is Some(current_line) {
    current_line > line
  } else {
    false
  }
}

///|
fn parse_list_comprehension_foreach_header(
  state : ParserState,
  children : Array[(String?, CstNode)],
  close_kind : TokenKind,
) -> Bool {
  let pat = parse_foreach_pattern(state)
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  if state.current_is(TK_IN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
  }
  let expr = parse_expr_until(state, list_comprehension_expr_stops(close_kind))
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  if !state.current_is(TK_IF) &&
    !state.current_is(TK_FAT_ARROW) &&
    !state.current_is(close_kind) &&
    !state.current_is(TK_EOF) {
    if state.current_is(TK_SEMI) || state.current_is(TK_NEWLINE) {
      consume_for_header_separator(state)
    }
    push_child(
      children,
      Some("init"),
      parse_list_comprehension_binding_list(state, close_kind),
    )
    if !state.current_is(TK_IF) &&
      !state.current_is(TK_FAT_ARROW) &&
      !state.current_is(close_kind) &&
      !state.current_is(TK_EOF) {
      if state.current_is(TK_SEMI) || state.current_is(TK_NEWLINE) {
        consume_for_header_separator(state)
      }
      push_child(
        children,
        Some("continue"),
        parse_list_comprehension_binding_list(state, close_kind),
      )
    }
  }
  let invalid_trailing_body = state.current_is(TK_SEMI) ||
    state.current_is(TK_NEWLINE)
  if invalid_trailing_body {
    state.report_expected_tokens([TK_COMMA, TK_IF, TK_FAT_ARROW])
    consume_for_header_separator(state)
  }
  invalid_trailing_body
}

///|
fn parse_list_comprehension_for_header(
  state : ParserState,
  children : Array[(String?, CstNode)],
  close_kind : TokenKind,
  for_loc : Location,
) -> Bool {
  let init = parse_list_comprehension_binding_list(state, close_kind)
  push_child(children, Some("init"), init)
  if init.children_with_label("item").is_empty() &&
    (state.current_is(TK_IF) || state.current_is(TK_FAT_ARROW)) {
    state.report(
      for_loc, "A list-comprehension `for` header cannot be empty; use `for ;;` for an infinite header.",
    )
  }
  if !state.current_is(TK_IF) &&
    !state.current_is(TK_FAT_ARROW) &&
    !state.current_is(close_kind) &&
    !state.current_is(TK_EOF) {
    if state.current_is(TK_SEMI) || state.current_is(TK_NEWLINE) {
      consume_for_header_separator(state)
    }
    if !state.current_is(TK_SEMI) &&
      !state.current_is(TK_NEWLINE) &&
      !state.current_is(TK_IF) &&
      !state.current_is(TK_FAT_ARROW) &&
      !state.current_is(close_kind) &&
      !state.current_is(TK_EOF) {
      let cond_expr = parse_expr_infix(
        state,
        list_comprehension_expr_stops(close_kind),
      )
      if !cond_expr.children.is_empty() {
        push_child(children, Some("cond"), cond_expr)
      }
    }
    if !state.current_is(TK_IF) &&
      !state.current_is(TK_FAT_ARROW) &&
      !state.current_is(close_kind) &&
      !state.current_is(TK_EOF) {
      if state.current_is(TK_SEMI) || state.current_is(TK_NEWLINE) {
        consume_for_header_separator(state)
      }
      push_child(
        children,
        Some("continue"),
        parse_list_comprehension_binding_list(state, close_kind),
      )
    }
  }
  let invalid_trailing_body = state.current_is(TK_SEMI) ||
    state.current_is(TK_NEWLINE)
  if invalid_trailing_body {
    state.report_expected_tokens([TK_COMMA, TK_IF, TK_FAT_ARROW])
    consume_for_header_separator(state)
  }
  invalid_trailing_body
}

///|
fn parse_invalid_list_comprehension_body(
  state : ParserState,
  close_kind : TokenKind,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.current_is(TK_IF) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    }
    let cond = parse_expr_until(state, [TK_FAT_ARROW, close_kind, TK_EOF])
    if !cond.children.is_empty() {
      push_child(children, Some("cond"), cond)
    }
    state.report_expected_tokens([TK_LBRACE])
  } else {
    state.report_expected_description("simple expression")
  }
  while !state.current_is(close_kind) &&
        !state.current_is(TK_EOF) &&
        !(state.peek() is None) {
    if state.consume_any() is Some(error) {
      push_child(children, Some("error"), error)
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::error(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_list_comprehension_binding_list(
  state : ParserState,
  close_kind : TokenKind,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_children(children, state.consume_trivia())
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), [
          TK_SEMI,
          TK_NEWLINE,
          TK_IF,
          TK_FAT_ARROW,
          close_kind,
          TK_EOF,
        ]) => done = true
      Some((COMMA, _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          done = true
        }
      Some(_) => {
        let item = parse_list_comprehension_binding(state, close_kind)
        push_child(children, Some("item"), item)
        push_children(children, state.consume_trivia())
        if state.current_is(TK_COMMA) {
          if state.consume_any() is Some(sep) {
            push_child(children, Some("sep"), sep)
          } else {
            ()
          }
        } else {
          done = true
        }
      }
      None => done = true
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_for_each_binding_list(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_list_comprehension_binding(
  state : ParserState,
  close_kind : TokenKind,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => push_child(children, Some("name"), state.missing_token_node(TK_LIDENT))
  }
  if state.current_is(TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
  }
  let expr = parse_expr_until(
    state,
    list_comprehension_binding_stops(close_kind),
  )
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
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
          @node.CstNode::name_binder(loc~, source_span~, long_ident~)
        },
        ["name"],
      )

      let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let semantic_name = @construction.first_child_with_label_from(
        children, "semantic_name",
      )
      @node.CstNode::binding_for_each(
        loc~,
        source_span~,
        name~,
        op~,
        expr~,
        semantic_name?,
      )
    },
    children,
  )
}

///|
fn parse_array_expr_item(state : ParserState) -> CstNode {
  let stops = [TokenKind::TK_COMMA, TK_RBRACKET, TK_BAR_RBRACKET, TK_EOF]
  match state.peek() {
    Some((ELLIPSIS, _, _)) => parse_invalid_simple_expr(state, stops)
    Some((DOTDOT, _, _)) => {
      let children : Array[(String?, CstNode)] = []
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let reports_before_expr = state.reports.length()
      let expr = parse_expr_until(state, stops)
      push_child(
        children,
        Some("expr"),
        state.required_expr_node(expr, reports_before_expr),
      )
      state.construct_from_children(
        fn(loc, source_span, children) {
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          @node.CstNode::array_element_spread(loc~, source_span~, op~, expr~)
        },
        children,
      )
    }
    _ => {
      report_statement_only_expression(state, "array element")
      let reports_before_expr = state.reports.length()
      let expr = state.required_expr_node(
        parse_expr_until(state, stops),
        reports_before_expr,
      )
      state.construct_from_children(
        fn(loc, source_span, children) {
          let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          @node.CstNode::array_element_regular(loc~, source_span~, expr~)
        },
        [(Some("expr"), expr)],
      )
    }
  }
}

///|
fn body_delimited_condition_stops(state : ParserState) -> Array[TokenKind] {
  match state.peek_significant() {
    Some((LBRACE, _, _)) => [TK_EOF]
    _ => [TK_LBRACE, TK_EOF]
  }
}

///|
fn recover_expr_until_statement_boundary(
  state : ParserState,
  children : Array[(String?, CstNode)],
  balance_groups? : Bool = true,
) -> Unit {
  let mut depth = 0
  let mut done = false
  while !done {
    match state.peek() {
      Some((SEMI(_) | RBRACE, _, _)) if depth == 0 => done = true
      Some((EOF, _, _)) | None => done = true
      Some((tok, _, _)) =>
        if state.consume_any() is Some(bad) {
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
          if balance_groups {
            depth = next_depth(depth, tok)
          }
        } else {
          done = true
        }
    }
  }
}

///|
fn recover_to_required_block(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> Bool {
  while !state.current_is(TK_LBRACE) &&
        !state.current_is(TK_SEMI) &&
        !state.current_is(TK_RBRACE) &&
        !state.current_is(TK_EOF) &&
        !(state.peek() is None) {
    if state.consume_any() is Some(bad) {
      push_child(
        children,
        Some("error"),
        error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
      )
    } else {
      ()
    }
  }
  state.current_is(TK_LBRACE)
}

///|
fn report_trailing_recovery_eof(state : ParserState) -> Unit {
  let mut offset = 0
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_) | NEWLINE | SEMI(_), _, _))
      | Some((RPAREN | RBRACKET | RBRACE, _, _)) => offset += 1
      Some((EOF, start, end)) => {
        state.report(mk_loc(start, end), "Unexpected token here.")
        done = true
      }
      Some(_) | None => done = true
    }
  }
}

///|
fn report_next_statement_after_separator(state : ParserState) -> Unit {
  let mut offset = 0
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_) | NEWLINE | SEMI(_), _, _)) => offset += 1
      Some((RBRACE | EOF, _, _)) | None => done = true
      Some((_, start, end)) => {
        state.report(mk_loc(start, end), "Unexpected token here.")
        done = true
      }
    }
  }
}
