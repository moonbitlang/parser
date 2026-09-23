///|
fn parse_type_until(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  parse_type_arrow(state, stop_kinds)
}

///|
fn parse_type_atom(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  match state.peek() {
    Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          let error = @construction.children_with_label_from(children, "error")
          @node.CstNode::type_missing_type(loc~, source_span~, error~)
        },
        [],
      )
    Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => {
      let children = state.consume_trivia()
      let ty = parse_type_atom(state, stop_kinds)
      if !ty.children.is_empty() {
        push_child(children, Some("type"), ty)
      }
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::type_group(loc~, source_span~, entries~)
        },
        children,
      )
    }
    Some((AMPER, _, _)) => parse_object_type_atom(state)
    Some((UIDENT(_), _, _)) | Some((PACKAGE_NAME(_), _, _)) =>
      parse_type_name_atom(state)
    Some((UNDERSCORE, _, _)) =>
      parse_single_token_type(state, fn(loc, source_span, children) {
        let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::type_any(loc~, source_span~, name~)
      })
    Some((LPAREN, _, _)) => parse_paren_type(state, stop_kinds)
    Some((LBRACE, start, end)) => {
      state.report_expected_description_at(LBRACE, mk_loc(start, end), "type")
      let reports_to_keep = state.reports.length()
      let ty = parse_object_type_literal(state)
      while state.reports.length() > reports_to_keep {
        let _ = state.reports.pop()
      }
      ty
    }
    Some((ASYNC, _, _)) => parse_async_type_atom(state, stop_kinds)
    Some((found, start, end)) => {
      state.report_expected_description_at(found, mk_loc(start, end), "type")
      let children : Array[(String?, CstNode)] = []
      let scan = state.recover_until(stop_kinds)
      push_recovered_errors(children, scan.skipped, wrap=false)
      state.construct_from_children(
        fn(loc, source_span, children) {
          let error = @construction.children_with_label_from(children, "error")
          @node.CstNode::type_missing_type(loc~, source_span~, error~)
        },
        children,
      )
    }
    None =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          let error = @construction.children_with_label_from(children, "error")
          @node.CstNode::type_missing_type(loc~, source_span~, error~)
        },
        [],
      )
  }
}

///|
fn parse_single_token_type(
  state : ParserState,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
) -> CstNode {
  if state.consume_any() is Some(token) {
    state.construct_from_children(builder, [(Some("name"), token)])
  } else {
    state.construct_from_children(
      fn(loc, source_span, children) {
        let error = @construction.children_with_label_from(children, "error")
        @node.CstNode::type_missing_type(loc~, source_span~, error~)
      },
      [],
    )
  }
}

///|
fn parse_type_name_atom(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(name) {
    push_child(children, Some("name"), name)
  } else {
    ()
  }
  match state.peek() {
    Some((DOT_LIDENT(_), _, _)) | Some((DOT_UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
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
          @node.CstNode::name_constr_id(loc~, source_span~, long_ident~)
        },
        ["name"],
      )

      let op = @construction.first_child_with_label_from(children, "op")
      let name = @construction.children_with_label_from(children, "name")
      let error = @construction.children_with_label_from(children, "error")
      let question = @construction.first_child_with_label_from(
        children, "question",
      )
      let semantic_name = @construction.first_child_with_label_from(
        children, "semantic_name",
      )
      let object = @construction.first_child_with_label_from(children, "object")
      @node.CstNode::type_name(
        loc~,
        source_span~,
        op?,
        name~,
        error~,
        question?,
        semantic_name?,
        object?,
      )
    },
    children,
  )
}

///|
fn parse_object_type_atom(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(op) {
    push_child(children, Some("op"), op)
  } else {
    ()
  }
  match state.peek() {
    Some((UIDENT(_), _, _)) | Some((PACKAGE_NAME(_), _, _)) => {
      let name = parse_type_name_atom(state)
      if !name.children.is_empty() {
        push_child(children, Some("type"), name)
      }
    }
    _ => push_child(children, Some("type"), state.missing_token_node(TK_UIDENT))
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let body = @construction.first_child_with_label_from(children, "body")
      let op = @construction.first_child_with_label_from(children, "op")
      let type_ = @construction.first_child_with_label_from(children, "type")
      @node.CstNode::type_object(loc~, source_span~, body?, op?, type_?)
    },
    children,
  )
}

///|
fn parse_async_type_atom(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  match state.peek_significant() {
    Some((LPAREN, _, _)) => ()
    Some((found, start, end)) => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [TK_LPAREN])
      if top_level_decl_can_start(found) {
        push_children(children, state.consume_trivia())
        return state.construct_from_children(
          fn(loc, source_span, children) {
            push_bool_marker(
              children,
              loc,
              source_span,
              "async",
              fn(loc, source_span, children) {
                let value = match
                  @construction.payload_from_children(children) {
                  Some(NodePayload::Bool(value)) => value
                  _ => false
                }
                @node.CstNode::flag_async(value~, loc~, source_span~)
              },
              true,
            )

            let entries = recovery_children(children)
            @node.CstNode::type_arrow(loc~, source_span~, entries~)
          },
          children,
        )
      }
      let scan = state.recover_until(stop_kinds)
      push_recovered_errors(children, scan.skipped, wrap=false)
      return state.construct_from_children(
        fn(loc, source_span, children) {
          push_bool_marker(
            children,
            loc,
            source_span,
            "async",
            fn(loc, source_span, children) {
              let value = match @construction.payload_from_children(children) {
                Some(NodePayload::Bool(value)) => value
                _ => false
              }
              @node.CstNode::flag_async(value~, loc~, source_span~)
            },
            true,
          )

          let entries = recovery_children(children)
          @node.CstNode::type_arrow(loc~, source_span~, entries~)
        },
        children,
      )
    }
    None => state.report_expected_tokens([TK_LPAREN])
  }
  let ty = parse_type_arrow(state, stop_kinds)
  if !ty.children.is_empty() {
    push_child(children, Some("type"), ty)
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_bool_marker(
        children,
        loc,
        source_span,
        "async",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_async(value~, loc~, source_span~)
        },
        true,
      )

      let entries = recovery_children(children)
      @node.CstNode::type_arrow(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_paren_type_item(state : ParserState) -> CstNode {
  parse_type_arrow(state, [TK_COMMA, TK_RPAREN, TK_EOF])
}

///|
fn parse_paren_type(
  state : ParserState,
  _stop_kinds : Array[TokenKind],
) -> CstNode {
  let list = parse_delimited(
    state,
    TK_LPAREN,
    TK_RPAREN,
    parse_paren_type_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_tuple_type_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
  )
  let mut item_count = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), _)) => item_count += 1
      _ => ()
    }
  }
  let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if item_count ==
    1 {
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::type_group(loc~, source_span~, entries~)
    }
  } else {
    fn(loc, source_span, children) {
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::type_tuple(loc~, source_span~, body~)
    }
  }
  if item_count == 0 && !state.current_is(TokenKind::TK_THIN_ARROW) {
    state.report_expected_tokens([TokenKind::TK_THIN_ARROW])
  }
  state.construct_from_children(builder, [(Some("body"), list)])
}

///|
fn parse_object_field_type_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) =>
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
    let ty = parse_type_arrow(state, [TK_COMMA, TK_RBRACE, TK_EOF])
    if !ty.children.is_empty() {
      push_child(children, Some("type"), ty)
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
          @node.CstNode::name_field(loc~, source_span~, long_ident~)
        },
        ["name"],
      )
      push_bool_marker(
        children,
        loc,
        source_span,
        "mutable",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_mutable(value~, loc~, source_span~)
        },
        false,
      )
      push_marker_child(children, loc, source_span, "vis", fn(
        loc,
        source_span,
        children,
      ) {
        let _ = children
        @node.CstNode::visibility_default(loc~, source_span~)
      })

      let entries = recovery_children(children)
      @node.CstNode::field_declaration(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_object_type_literal(state : ParserState) -> CstNode {
  let list = parse_delimited(
    state,
    TK_LBRACE,
    TK_RBRACE,
    parse_object_field_type_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_object_field_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
  )
  state.construct_from_children(
    fn(loc, source_span, children) {
      let body = @construction.first_child_with_label_from(children, "body")
      let op = @construction.first_child_with_label_from(children, "op")
      let type_ = @construction.first_child_with_label_from(children, "type")
      @node.CstNode::type_object(loc~, source_span~, body?, op?, type_?)
    },
    [(Some("body"), list)],
  )
}

///|
fn parse_type_argument_item(state : ParserState) -> CstNode {
  parse_type_arrow(state, [TK_COMMA, TK_RBRACKET, TK_EOF])
}

///|
fn parse_type_argument_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LBRACKET,
    TK_RBRACKET,
    parse_type_argument_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_type_argument_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
    follow_set=[TokenKind::TK_WITH, TokenKind::TK_LBRACE, TokenKind::TK_SEMI],
    require_nonempty=true,
  )
}

///|
fn parse_legacy_type_tail(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  head : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = [(Some("head"), head)]
  let mut depth = 0
  let mut done = false
  while !done {
    match state.peek() {
      Some((tok, _, _)) if depth == 0 && kind_in(tok.kind(), stop_kinds) =>
        done = true
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) =>
        push_children(children, state.consume_trivia())
      Some((tok, _, _)) => {
        if state.consume_any() is Some(consumed) {
          push_child(children, token_label(tok), consumed)
        } else {
          done = true
        }
        depth = next_depth(depth, tok)
      }
      None => done = true
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::type_application(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_simple_type(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  parse_type_atom(state, stop_kinds)
}

///|
fn parse_type_postfix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut ty = parse_simple_type(state, stop_kinds)
  let mut done = false
  while !done {
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((LBRACKET, _, _)) => {
        let children : Array[(String?, CstNode)] = [(Some("func"), ty)]
        push_child(children, Some("arg"), parse_type_argument_list(state))
        ty = state.construct_from_children(
          fn(loc, source_span, children) {
            let entries = recovery_children(children)
            @node.CstNode::type_application(loc~, source_span~, entries~)
          },
          children,
        )
      }
      Some((QUESTION, start, _)) if ty.loc.end.cnum == start.cnum => {
        let children : Array[(String?, CstNode)] = [(Some("type"), ty)]
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
          push_direct_operator(children, op)
        } else {
          ()
        }
        ty = state.construct_from_children(
          fn(loc, source_span, children) {
            let type_ = @construction.first_child_with_label_from(
              children, "type",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
              missing_at_end(loc~, source_span~),
            )
            let operator = @construction.first_child_with_label_from(
              children, "operator",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            @node.CstNode::type_option(
              loc~,
              source_span~,
              type_~,
              op~,
              operator~,
            )
          },
          children,
        )
      }
      _ => done = true
    }
  }
  match state.peek() {
    Some((tok, _, _)) if !kind_in(tok.kind(), stop_kinds) =>
      match tok {
        DOT_UIDENT(_) | DOT_LIDENT(_) | COLONCOLON =>
          parse_legacy_type_tail(state, stop_kinds, ty)
        _ => ty
      }
    _ => ty
  }
}

///|
fn parse_type_arrow(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let lhs = parse_type_postfix(state, stop_kinds)
  match state.peek() {
    Some((THIN_ARROW, start, end)) if !kind_in(TK_THIN_ARROW, stop_kinds) => {
      let invalid_unparenthesized = match lhs.kind {
        Type_Group | Type_Tuple => false
        _ => true
      }
      let _ = (start, end)
      let children : Array[(String?, CstNode)] = [(Some("param"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let rhs = parse_type_arrow(state, stop_kinds)
      if !rhs.children.is_empty() {
        push_child(children, Some("result"), rhs)
      }
      match state.peek() {
        Some((RAISE, _, _)) if !kind_in(TK_RAISE, stop_kinds) =>
          parse_effect_annotation_into(state, children)
        Some((NORAISE, _, _)) if !kind_in(TK_NORAISE, stop_kinds) =>
          parse_effect_annotation_into(state, children)
        Some((NOCANCEL, _, _)) if !kind_in(TK_NOCANCEL, stop_kinds) =>
          parse_effect_annotation_into(state, children)
        _ => ()
      }
      if invalid_unparenthesized {
        state.report(
          empty_loc(state.current_position()),
          "Unexpected token here.",
        )
      }
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::type_arrow(loc~, source_span~, entries~)
        },
        children,
      )
    }
    _ => lhs
  }
}
