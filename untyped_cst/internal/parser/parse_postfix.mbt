///|
fn parse_expr_postfix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let lhs = parse_expr_range(state, stop_kinds)
  discard_expr_trivia(state)
  match state.peek() {
    Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => lhs
    Some((AS, _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("expr"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let ty = parse_type_name_node(state)
      if !ty.children.is_empty() {
        push_child(children, Some("type"), ty)
      }
      state.construct_from_children(
        fn(loc, source_span, children) {
          let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let type_ = @construction.first_child_with_label_from(
            children, "type",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::expr_as(loc~, source_span~, expr~, op~, type_~)
        },
        children,
      )
    }
    Some((IS, _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("expr"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let reports_before_pattern = state.reports.length()
      let pat = parse_pattern_range(
        state,
        stop_kinds,
        allow_brace_pattern_start=true,
      )
      push_child(
        children,
        Some("pattern"),
        state.required_pattern_node(pat, reports_before_pattern),
      )
      state.construct_from_children(
        fn(loc, source_span, children) {
          let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let pattern = @construction.first_child_with_label_from(
            children, "pattern",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::expr_is(loc~, source_span~, expr~, op~, pattern~)
        },
        children,
      )
    }
    Some((EQ_TILDE, _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("expr"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let reports_before_pattern = state.reports.length()
      let pat = parse_regex_pattern_until(state, stop_kinds)
      push_child(
        children,
        Some("pattern"),
        state.required_pattern_node(
          pat,
          reports_before_pattern,
          expected="regex pattern",
        ),
      )
      state.construct_from_children(
        fn(loc, source_span, children) {
          let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let pattern = @construction.first_child_with_label_from(
            children, "pattern",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::expr_regex_match(
            loc~,
            source_span~,
            expr~,
            op~,
            pattern~,
          )
        },
        children,
      )
    }
    Some((LT_PLUS, _, _)) | Some((LT_QUESTION, _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("expr"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      report_invalid_template_literal_start(state)
      let reports_before_template = state.reports.length()
      let template = parse_expr_until(state, template_operand_stops(stop_kinds))
      push_child(
        children,
        Some("template"),
        state.required_expr_node(template, reports_before_template),
      )
      state.construct_from_children(
        fn(loc, source_span, children) {
          push_bool_marker(
            children,
            loc,
            source_span,
            "conditional",
            fn(loc, source_span, children) {
              let value = match @construction.payload_from_children(children) {
                Some(NodePayload::Bool(value)) => value
                _ => false
              }
              @node.CstNode::flag_conditional(value~, loc~, source_span~)
            },
            children
            .iter()
            .any(fn(entry) {
              entry.0 is Some("op") &&
              entry.1.has_child_kind(fn(kind) { kind is Flag_Conditional })
            }),
          )

          let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let template = @construction.first_child_with_label_from(
            children, "template",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let conditional = @construction.first_child_with_label_from(
            children, "conditional",
          )
          @node.CstNode::expr_template_writing(
            loc~,
            source_span~,
            expr~,
            op~,
            template~,
            conditional?,
          )
        },
        children,
      )
    }
    _ => lhs
  }
}

///|
fn report_invalid_template_literal_start(state : ParserState) -> Unit {
  match state.peek_significant() {
    Some((LBRACE, _, _)) =>
      match state.peek_nth_significant(1) {
        Some((RBRACE, _, _)) | None => ()
        Some((STRING(_), _, _)) => ()
        Some((found, start, end)) =>
          state.report_expected_tokens_at(found, mk_loc(start, end), [TK_STRING])
      }
    Some(
      (
        STRING(_)
        | BYTES(_)
        | INTERP(_)
        | BYTES_INTERP(_)
        | MULTILINE_STRING(_)
        | MULTILINE_INTERP(_),
        _,
        _,
      )
    ) => ()
    Some((found, start, end)) =>
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "template string",
      )
    None => ()
  }
}

///|
fn parse_expr_simple_postfix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut lhs = parse_expr_simple(state, stop_kinds)
  let mut done = false
  while !done {
    discard_expr_trivia(state)
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((_, _, _)) =>
        if parse_expr_postfix_suffix(state, lhs) is Some(next) {
          lhs = next
        } else {
          done = true
        }
      None => done = true
    }
  }
  lhs
}

///|
fn template_operand_stops(stop_kinds : Array[TokenKind]) -> Array[TokenKind] {
  let stops = stop_kinds.copy()
  stops.push(TK_IMPLIES)
  stops.push(TK_BARBAR)
  stops.push(TK_AMPERAMPER)
  stops.push(TK_BAR)
  stops.push(TK_CARET)
  stops.push(TK_AMPER)
  stops.push(TK_INFIX1)
  stops.push(TK_INFIX2)
  stops.push(TK_PLUS)
  stops.push(TK_MINUS)
  stops.push(TK_INFIX3)
  stops.push(TK_INFIX4)
  stops
}

///|
fn parse_expr_postfix_suffix(state : ParserState, lhs : CstNode) -> CstNode? {
  match state.peek() {
    Some((LPAREN, _, _)) => {
      let args = parse_argument_list(state)
      Some(
        state.construct_from_children(
          fn(loc, source_span, children) {
            let op = @construction.first_child_with_label_from(children, "op")
            let operator = @construction.first_child_with_label_from(
              children, "operator",
            )
            let func = @construction.first_child_with_label_from(
              children, "func",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            let arg = @construction.first_child_with_label_from(children, "arg").unwrap_or(
              missing_at_end(loc~, source_span~),
            )
            @node.CstNode::expr_apply(
              loc~,
              source_span~,
              op?,
              operator?,
              func~,
              arg~,
            )
          },
          [(Some("func"), lhs), (Some("arg"), args)],
        ),
      )
    }
    Some((LBRACKET, _, _)) => Some(parse_array_get_suffix(state, lhs))
    Some((DOT_LIDENT(_), _, _)) | Some((DOT_INT(_), _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("self"), lhs)]
      if state.consume_any() is Some(field) {
        push_child(children, Some("token"), field)
        push_child(children, Some("field"), accessor_node_from_token(field))
      } else {
        ()
      }
      if state.current_is(TK_LPAREN) {
        push_child(children, Some("arg"), parse_argument_list(state))
        Some(
          state.construct_from_children(
            fn(loc, source_span, children) {
              push_bool_marker(
                children,
                loc,
                source_span,
                "return_self",
                fn(loc, source_span, children) {
                  let value = match
                    @construction.payload_from_children(children) {
                    Some(NodePayload::Bool(value)) => value
                    _ => false
                  }
                  @node.CstNode::flag_return_self(value~, loc~, source_span~)
                },
                false,
              )

              let self_ = @construction.first_child_with_label_from(
                children, "self",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              let op = @construction.first_child_with_label_from(children, "op")
              let error = @construction.children_with_label_from(
                children, "error",
              )
              let token = @construction.first_child_with_label_from(
                children, "token",
              )
              let field = @construction.first_child_with_label_from(
                children, "field",
              )
              let arg = @construction.first_child_with_label_from(
                children, "arg",
              )
              let return_self = @construction.first_child_with_label_from(
                children, "return_self",
              )
              @node.CstNode::expr_dot_apply(
                loc~,
                source_span~,
                self_~,
                op?,
                error~,
                token?,
                field?,
                arg?,
                return_self?,
              )
            },
            children,
          ),
        )
      } else {
        Some(
          state.construct_from_children(
            fn(loc, source_span, children) {
              let self_ = @construction.first_child_with_label_from(
                children, "self",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              let token = @construction.first_child_with_label_from(
                children, "token",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              let field = @construction.first_child_with_label_from(
                children, "field",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              @node.CstNode::expr_field(
                loc~,
                source_span~,
                self_~,
                token~,
                field~,
              )
            },
            children,
          ),
        )
      }
    }
    Some((COLONCOLON, _, _)) => Some(parse_coloncolon_suffix(state, lhs))
    Some((DOTDOT, _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("self"), lhs)]
      let mut valid_field = true
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      match state.peek() {
        Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
          if state.consume_any() is Some(field) {
            push_child(children, Some("token"), field)
            push_child(children, Some("field"), accessor_node_from_token(field))
          } else {
            ()
          }
        Some((found, start, end)) => {
          valid_field = false
          state.report_expected_tokens_at(found, mk_loc(start, end), [
            TK_LIDENT,
            TK_EXTEND,
          ])
          let scan = state.recover_until([TK_SEMI, TK_RBRACE])
          push_recovered_errors(children, scan.skipped, wrap=false)
        }
        None => valid_field = false
      }
      if valid_field && state.current_is(TK_LPAREN) {
        push_child(children, Some("arg"), parse_argument_list(state))
      } else if valid_field {
        push_child(children, Some("arg"), state.missing_token_node(TK_LPAREN))
      }
      Some(
        state.construct_from_children(
          fn(loc, source_span, children) {
            push_bool_marker(
              children,
              loc,
              source_span,
              "return_self",
              fn(loc, source_span, children) {
                let value = match
                  @construction.payload_from_children(children) {
                  Some(NodePayload::Bool(value)) => value
                  _ => false
                }
                @node.CstNode::flag_return_self(value~, loc~, source_span~)
              },
              true,
            )

            let self_ = @construction.first_child_with_label_from(
              children, "self",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            let op = @construction.first_child_with_label_from(children, "op")
            let error = @construction.children_with_label_from(
              children, "error",
            )
            let token = @construction.first_child_with_label_from(
              children, "token",
            )
            let field = @construction.first_child_with_label_from(
              children, "field",
            )
            let arg = @construction.first_child_with_label_from(children, "arg")
            let return_self = @construction.first_child_with_label_from(
              children, "return_self",
            )
            @node.CstNode::expr_dot_apply(
              loc~,
              source_span~,
              self_~,
              op?,
              error~,
              token?,
              field?,
              arg?,
              return_self?,
            )
          },
          children,
        ),
      )
    }
    _ => None
  }
}

///|
fn parse_coloncolon_suffix(state : ParserState, lhs : CstNode) -> CstNode {
  let children : Array[(String?, CstNode)] = [(Some("type"), lhs)]
  if state.consume_any() is Some(op) {
    push_child(children, Some("op"), op)
  } else {
    ()
  }
  push_children(children, state.consume_trivia())
  if state.current_is(TK_LBRACE) {
    let body = parse_record_field_list(state)
    let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if list_has_item_kind(
        body,
        fn(kind) { kind is Field_RecordSpread },
      ) {
      fn(loc, source_span, children) {
        let type_ = @construction.first_child_with_label_from(children, "type")
        let op = @construction.first_child_with_label_from(children, "op")
        let trivia = @construction.children_with_label_from(children, "trivia")
        let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::expr_record_update(
          loc~,
          source_span~,
          type_?,
          op?,
          trivia~,
          body~,
        )
      }
    } else {
      fn(loc, source_span, children) {
        let type_ = @construction.first_child_with_label_from(children, "type")
        let op = @construction.first_child_with_label_from(children, "op")
        let trivia = @construction.children_with_label_from(children, "trivia")
        let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::expr_record(
          loc~,
          source_span~,
          type_?,
          op?,
          trivia~,
          body~,
        )
      }
    }
    push_child(children, Some("body"), body)
    state.construct_from_children(builder, children)
  } else {
    let mut field_is_constructor = false
    match state.peek() {
      Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("field"), name)
        } else {
          ()
        }
      Some((UIDENT(_), _, _)) =>
        if state.consume_any() is Some(name) {
          field_is_constructor = true
          push_child(children, Some("field"), name)
        } else {
          ()
        }
      Some((PACKAGE_NAME(_), _, _)) => {
        if state.consume_any() is Some(pkg) {
          push_child(children, Some("pkg"), pkg)
        } else {
          ()
        }
        match state.peek() {
          Some((DOT_UIDENT(_), _, _)) =>
            if state.consume_any() is Some(name) {
              field_is_constructor = true
              push_child(children, Some("field"), name)
            } else {
              ()
            }
          _ =>
            push_child(
              children,
              Some("field"),
              state.missing_token_node(TK_UIDENT),
            )
        }
      }
      Some((found, start, end)) => {
        state.report(
          mk_loc(start, end),
          "Unexpected token \{found.to_expect_string()}, you may expect method name, constructor name, \"[\" or \"{\".",
        )
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
      None =>
        push_child(
          children,
          Some("field"),
          state.missing_token_node_silent(TK_LIDENT),
        )
    }
    if field_is_constructor {
      state.construct_from_children(
        fn(loc, source_span, children) {
          push_structured_name(
            children,
            "semantic_name",
            fn(loc, source_span, children) {
              let long_ident = @construction.first_child_with_label_from(
                children, "long_ident",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              @node.CstNode::name_constructor(loc~, source_span~, long_ident~)
            },
            ["pkg", "type", "name", "field"],
          )
          push_constructor_extra_info(children, loc, source_span)

          let entries = recovery_children(children)
          @node.CstNode::expr_constr(loc~, source_span~, entries~)
        },
        children,
      )
    } else {
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_method_from_entries(loc~, source_span~, entries~)
        },
        children,
      )
    }
  }
}
