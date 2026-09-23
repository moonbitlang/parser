///|
fn brace_starts_record_expr(
  state : ParserState,
  start_offset? : Int = 0,
) -> Bool {
  let mut offset = start_offset + 1
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((DOTDOT, _, _)) => return true
      Some((LIDENT(_), _, _))
      | Some((UIDENT(_), _, _))
      | Some((EXTEND, _, _)) => {
        offset += 1
        let mut after_name = false
        while !after_name {
          match state.peek(offset~) {
            Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
            Some((COLON, _, _)) | Some((COMMA, _, _)) => return true
            _ => after_name = true
          }
        }
        done = true
      }
      _ => done = true
    }
  }
  false
}

///|
fn brace_starts_map_expr(state : ParserState, start_offset? : Int = 0) -> Bool {
  let mut offset = start_offset + 1
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((RBRACE, _, _)) => return true
      Some((MINUS, _, _)) => {
        offset += 1
        match state.peek(offset~) {
          Some((INT(_), _, _))
          | Some((DOUBLE(_), _, _))
          | Some((FLOAT(_), _, _)) => {
            offset += 1
            let mut after_key = false
            while !after_key {
              match state.peek(offset~) {
                Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
                Some((COLON, _, _)) => return true
                _ => after_key = true
              }
            }
          }
          _ => ()
        }
        done = true
      }
      Some((STRING(_), _, _))
      | Some((BYTES(_), _, _))
      | Some((REGEX_LITERAL(_), _, _))
      | Some((CHAR(_), _, _))
      | Some((INT(_), _, _))
      | Some((BYTE(_), _, _))
      | Some((DOUBLE(_), _, _))
      | Some((FLOAT(_), _, _))
      | Some((TRUE, _, _))
      | Some((FALSE, _, _)) => {
        offset += 1
        let mut after_key = false
        while !after_key {
          match state.peek(offset~) {
            Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
            Some((COLON, _, _)) => return true
            _ => after_key = true
          }
        }
        done = true
      }
      _ => done = true
    }
  }
  false
}

///|
fn brace_expr_is_followed_by_range_operator(state : ParserState) -> Bool {
  let mut offset = 0
  let mut depth = 0
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((LBRACE, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RBRACE, _, _)) => {
        depth -= 1
        offset += 1
        if depth == 0 {
          while true {
            match state.peek(offset~) {
              Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
              Some((tok, _, _)) => return range_operator(tok)
              None => return false
            }
          }
        }
      }
      Some((EOF, _, _)) | None => done = true
      Some((_, _, _)) => offset += 1
    }
  }
  false
}

///|
fn parse_record_expr(state : ParserState) -> CstNode {
  let body = parse_record_field_list(state)
  state.construct_from_children(
    if list_has_item_kind(body, fn(kind) { kind is Field_RecordSpread }) {
      fn(loc, source_span, children) {
        let type_ = @construction.first_child_with_label_from(children, "type")
        let op = @construction.first_child_with_label_from(children, "op")
        let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::expr_record_update(
          loc~,
          source_span~,
          type_?,
          op?,
          body~,
        )
      }
    } else {
      fn(loc, source_span, children) {
        let type_ = @construction.first_child_with_label_from(children, "type")
        let op = @construction.first_child_with_label_from(children, "op")
        let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::expr_record(loc~, source_span~, type_?, op?, body~)
      }
    },
    [(Some("body"), body)],
  )
}

///|
fn parse_map_expr(state : ParserState) -> CstNode {
  state.construct_from_children(
    fn(loc, source_span, children) {
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::expr_map(loc~, source_span~, body~)
    },
    [(Some("body"), parse_map_field_list(state))],
  )
}

///|
fn parse_record_field_list(state : ParserState) -> CstNode {
  let list = parse_delimited(
    state,
    TK_LBRACE,
    TK_RBRACE,
    parse_record_field_item,
    fn(loc, source_span, children) {
      push_marker_child(
        children,
        loc,
        source_span,
        "trailing",
        list_trailing_mark_kind(children),
      )
      let entries = list_children(children)
      @node.CstNode::list_record_field_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
  )
  let mut item_index = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), item)) => {
        match item.kind {
          Field_RecordSpread if item_index > 0 =>
            if item.first_child_with_label("op") is Some(op) {
              state.report_expected_tokens_at(DOTDOT, op.loc, [
                TK_LIDENT,
                TK_EXTEND,
              ])
            }
          _ => ()
        }
        item_index += 1
      }
      _ => ()
    }
  }
  list
}

///|
fn parse_map_field_list(state : ParserState) -> CstNode {
  let list = parse_delimited(
    state,
    TK_LBRACE,
    TK_RBRACE,
    parse_map_field_item,
    fn(loc, source_span, children) {
      push_marker_child(
        children,
        loc,
        source_span,
        "trailing",
        list_trailing_mark_kind(children),
      )
      let entries = list_children(children)
      @node.CstNode::list_map_field_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
  )
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), item)) => {
        if item.has_child_or_grandchild_kind(fn(kind) {
            kind is Operator_UnaryMinus
          }) {
          let operators = item.children_with_label("op")
          if operators.length() > 1 {
            let colon = operators[operators.length() - 1]
            state.report_expected_tokens_at(COLON, colon.loc, [
              TK_SEMI,
              TK_RBRACE,
            ])
          }
        }
        break
      }
      _ => ()
    }
  }
  list
}

///|
fn parse_map_field_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut constant : CstNode? = None
  let mut negative = false
  match state.peek() {
    Some((MINUS, _, _)) => {
      negative = true
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
        push_child(
          children,
          Some("operator"),
          operator_node_from_token(
            fn(loc, source_span, children) {
              let value = match @construction.payload_from_children(children) {
                Some(NodePayload::String(value)) => value
                _ => ""
              }
              @node.CstNode::operator_unary_minus(value~, loc~, source_span~)
            },
            MINUS,
            op.loc,
            op.source_span,
          ),
        )
      } else {
        ()
      }
      match state.peek() {
        Some((raw, _, _)) if raw is (INT(_) | DOUBLE(_) | FLOAT(_)) =>
          if state.consume_any() is Some(key) {
            push_child(children, Some("key"), key)
            constant = constant_node_from_token(raw, key.loc, key.source_span)
          } else {
            ()
          }
        Some((found, start, end)) => {
          state.report_expected_description_at(
            found,
            mk_loc(start, end),
            "`-Int` or `-Float` in key of map pattern",
          )
          if state.consume_any() is Some(key) {
            push_child(children, Some("key"), key)
          } else {
            ()
          }
        }
        None => ()
      }
    }
    Some((raw, _, _)) if raw
      is (STRING(_)
      | BYTES(_)
      | REGEX_LITERAL(_)
      | CHAR(_)
      | INT(_)
      | BYTE(_)
      | DOUBLE(_)
      | FLOAT(_)
      | TRUE
      | FALSE) =>
      if state.consume_any() is Some(key) {
        push_child(children, Some("key"), key)
        constant = constant_node_from_token(raw, key.loc, key.source_span)
      } else {
        ()
      }
    Some((found, start, end)) => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TK_TRUE,
        TK_FALSE,
        TK_CHAR,
        TK_INT,
        TK_FLOAT,
        TK_DOUBLE,
        TK_MINUS,
        TK_STRING,
        TK_REGEX_LITERAL,
      ])
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
    None => ()
  }
  if constant is Some(value) {
    push_child(
      children,
      Some("constant"),
      signed_constant_node(value, negative),
    )
  }
  if state.current_is(TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
  }
  let expr = parse_expr_until(state, [TK_COMMA, TK_RBRACE, TK_EOF])
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_direct_semantic_constant(children)

      let entries = recovery_children(children)
      @node.CstNode::field_map_expression(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_record_field_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut field_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let is_pun = children.iter().all(fn(entry) { !(entry.0 is Some("expr")) })
    {
      push_structured_name(
        children,
        "semantic_name",
        fn(loc, source_span, children) {
          let long_ident = @construction.first_child_with_label_from(
            children, "long_ident",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::name_field(loc~, source_span~, long_ident~)
        },
        ["name"],
      )
      push_bool_marker(
        children,
        loc,
        source_span,
        "pun",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_pun(value~, loc~, source_span~)
        },
        is_pun,
      )
    }
    if is_pun {
      push_pun_expr(children)
    } else {
      ()
    }

    let entries = recovery_children(children)
    @node.CstNode::field_definition(loc~, source_span~, entries~)
  }
  if state.current_is(TokenKind::TK_DOTDOT) {
    field_builder = fn(loc, source_span, children) {
      let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::field_record_spread(loc~, source_span~, op~, expr~)
    }
    if state.consume_any() is Some(op) {
      push_child(children, Some("op"), op)
    } else {
      ()
    }
    let expr = parse_expr_until(state, [TK_COMMA, TK_RBRACE, TK_EOF])
    if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
    }
  } else {
    match state.peek() {
      Some((LIDENT(_), _, _))
      | Some((EXTEND, _, _))
      | Some((UIDENT(_), _, _))
      | Some((STRING(_), _, _)) =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
      _ => ()
    }
    if state.current_is(TK_COLON) {
      if state.consume_any() is Some(colon) {
        push_child(children, Some("op"), colon)
      } else {
        ()
      }
      let expr = parse_expr_until(state, [TK_COMMA, TK_RBRACE, TK_EOF])
      if !expr.children.is_empty() {
        push_child(children, Some("expr"), expr)
      }
    } else if children.length() == 0 {
      let expr = parse_expr_until(state, [TK_COMMA, TK_RBRACE, TK_EOF])
      if !expr.children.is_empty() {
        push_child(children, Some("expr"), expr)
      }
    }
  }
  state.construct_from_children(field_builder, children)
}

///|
fn parse_array_get_suffix(state : ParserState, lhs : CstNode) -> CstNode {
  let children : Array[(String?, CstNode)] = [(Some("self"), lhs)]
  let open = state.expect_token(TK_LBRACKET)
  push_child(children, Some("open"), open)
  let mut start_index : CstNode? = None
  let reports_before_index = state.reports.length()
  if !state.current_is(TK_COLON) {
    let index = parse_expr_until(state, [TK_RBRACKET, TK_COLON, TK_EOF])
    start_index = Some(state.required_expr_node(index, reports_before_index))
  }
  if state.current_is(TK_COLON) {
    if start_index is Some(index) {
      push_child(children, Some("start"), index)
    } else {
      ()
    }
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    if !state.current_is(TK_RBRACKET) && !state.current_is(TK_EOF) {
      let end_index = parse_expr_until(state, [TK_RBRACKET, TK_EOF])
      if !end_index.children.is_empty() {
        push_child(children, Some("end"), end_index)
      }
    }
    let close = state.expect_token(TK_RBRACKET)
    push_child(children, Some("close"), close)
    state.construct_from_children(
      fn(loc, source_span, children) {
        let self_ = @construction.first_child_with_label_from(children, "self").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let open = @construction.first_child_with_label_from(children, "open").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let start = @construction.first_child_with_label_from(children, "start")
        let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let end = @construction.first_child_with_label_from(children, "end")
        let close = @construction.first_child_with_label_from(children, "close").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::expr_array_get_slice(
          loc~,
          source_span~,
          self_~,
          open~,
          start?,
          op~,
          end?,
          close~,
        )
      },
      children,
    )
  } else {
    if start_index is Some(index) {
      push_child(children, Some("index"), index)
    } else {
      ()
    }
    let close = if !state.current_is(TK_RBRACKET) &&
      state.reports.length() > reports_before_index {
      state.missing_token_node_silent(TK_RBRACKET)
    } else {
      state.expect_token(TK_RBRACKET)
    }
    push_child(children, Some("close"), close)
    state.construct_from_children(
      fn(loc, source_span, children) {
        let self_ = @construction.first_child_with_label_from(children, "self").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let open = @construction.first_child_with_label_from(children, "open").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let index = @construction.first_child_with_label_from(children, "index").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let close = @construction.first_child_with_label_from(children, "close").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::expr_array_get(
          loc~,
          source_span~,
          self_~,
          open~,
          index~,
          close~,
        )
      },
      children,
    )
  }
}
