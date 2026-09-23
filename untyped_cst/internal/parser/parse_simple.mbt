///|
fn range_operator(tok : Token) -> Bool {
  match tok {
    RANGE_EXCLUSIVE
    | RANGE_INCLUSIVE
    | RANGE_LT_INCLUSIVE
    | RANGE_INCLUSIVE_REV
    | RANGE_EXCLUSIVE_REV => true
    _ => false
  }
}

///|
fn parse_expr_range(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut lhs = parse_expr_prefix(state, stop_kinds)
  match state.peek() {
    Some((tok, _, _)) if !kind_in(tok.kind(), stop_kinds) && range_operator(tok) => {
      let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
        push_direct_operator(children, op)
      } else {
        ()
      }
      let reports_before_rhs = state.reports.length()
      let rhs = parse_expr_prefix(state, stop_kinds)
      push_child(
        children,
        Some("rhs"),
        state.required_expr_node(rhs, reports_before_rhs),
      )
      lhs = state.construct_from_children(
        fn(loc, source_span, children) {
          let lhs = @construction.first_child_with_label_from(children, "lhs").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let operator = @construction.first_child_with_label_from(
            children, "operator",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let rhs = @construction.first_child_with_label_from(children, "rhs").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          @node.CstNode::expr_infix(
            loc~,
            source_span~,
            lhs~,
            op~,
            operator~,
            rhs~,
          )
        },
        children,
      )
    }
    _ => ()
  }
  lhs
}

///|
fn parse_expr_prefix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  discard_expr_trivia(state)
  match state.peek() {
    Some((operator, _, _)) if operator is (PLUS | MINUS | EXCLAMATION) => {
      let op = state
        .consume_any()
        .unwrap_or(state.missing_token_node_silent(operator.kind()))
      let semantic_operator = operator_node_from_token(
        match operator {
          PLUS =>
            fn(loc, source_span, children) {
              let value = match @construction.payload_from_children(children) {
                Some(NodePayload::String(value)) => value
                _ => ""
              }
              @node.CstNode::operator_unary_plus(value~, loc~, source_span~)
            }
          MINUS =>
            fn(loc, source_span, children) {
              let value = match @construction.payload_from_children(children) {
                Some(NodePayload::String(value)) => value
                _ => ""
              }
              @node.CstNode::operator_unary_minus(value~, loc~, source_span~)
            }
          _ =>
            fn(loc, source_span, children) {
              let value = match @construction.payload_from_children(children) {
                Some(NodePayload::String(value)) => value
                _ => ""
              }
              @node.CstNode::operator_unary_not(value~, loc~, source_span~)
            }
        },
        operator,
        op.loc,
        op.source_span,
      )
      let reports_before_expr = state.reports.length()
      let operand = state.required_expr_node(
        parse_expr_prefix(state, stop_kinds),
        reports_before_expr,
      )
      let constant = match operand.first_child_with_label("semantic_constant") {
        Some(constant) => Some(constant)
        None => operand.first_child_of_kind(NodeKind::is_constant)
      }
      match constant {
        Some(constant) if constant_can_fold_prefix(constant, operator) =>
          folded_prefix_constant_node(
            op,
            semantic_operator,
            operand,
            constant,
            operator is MINUS,
          )
        _ if operator is PLUS =>
          unary_plus_apply_node(op, semantic_operator, operand)
        _ =>
          state.construct_from_children(
            fn(loc, source_span, children) {
              let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
                missing_at_end(loc~, source_span~),
              )
              let operator = @construction.first_child_with_label_from(
                children, "operator",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              let expr = @construction.first_child_with_label_from(
                children, "expr",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              @node.CstNode::expr_unary(
                loc~,
                source_span~,
                op~,
                operator~,
                expr~,
              )
            },
            [
              (Some("op"), op),
              (Some("operator"), semantic_operator),
              (Some("expr"), operand),
            ],
          )
      }
    }
    _ => parse_expr_simple_postfix(state, stop_kinds)
  }
}

///|
fn parse_invalid_simple_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut pending_function_body = false
  let mut report_invalid_continue = false
  let mut invalid_continue_start = false
  let mut reported_promoted_function = false
  let mut invalid_pub_start = false
  let mut saw_local_let = false
  let invalid_top_level_start = match state.peek() {
    Some((found, start, end)) => {
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "simple expression",
      )
      report_invalid_continue = found is LOOP
      invalid_continue_start = found is CONTINUE
      invalid_pub_start = found is PUB
      top_level_decl_can_start(found)
    }
    None => false
  }
  while !state.current_in(stop_kinds) &&
        !state.current_is(TK_EOF) &&
        !(state.peek() is None) {
    match state.peek() {
      Some((found, _, _)) =>
        if state.consume_any() is Some(bad) {
          match found {
            FN => {
              pending_function_body = true
              if invalid_continue_start &&
                !reported_promoted_function &&
                state.peek_significant().map(item => item.0) is Some(LPAREN) {
                report_promoted_anonymous_function(state)
                reported_promoted_function = true
              }
            }
            LET | LETREC => saw_local_let = true
            LBRACE => pending_function_body = false
            _ => ()
          }
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        }
      None => ()
    }
  }
  if invalid_top_level_start &&
    pending_function_body &&
    state.current_is(TK_EOF) {
    state.report_expected_tokens([TK_LBRACE])
  }
  if report_invalid_continue && saw_local_let && state.current_is(TK_SEMI) {
    state.abort_current_decl = true
  }
  if report_invalid_continue && state.current_is(TK_RPAREN) {
    report_trailing_recovery_eof(state)
  }
  if invalid_pub_start {
    match state.peek() {
      Some((RBRACE, start, end)) =>
        state.report_expected_tokens_at(
          RBRACE,
          mk_loc(start, end),
          top_level_decl_expected_tokens(),
        )
      _ => ()
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_missing_expr_from_entries(
        loc~,
        source_span~,
        entries~,
      )
    },
    children,
  )
}

///|
fn report_promoted_anonymous_function(state : ParserState) -> Unit {
  match state.peek_significant() {
    Some((LPAREN, start, initial_end)) => {
      let mut recovery_end = initial_end
      let mut offset = 0
      let mut done = false
      while !done {
        match state.peek(offset~) {
          Some((tok, token_start, _)) if offset > 0 &&
            token_start.column() == 1 &&
            top_level_decl_can_start(tok) => done = true
          Some((EOF, _, _)) | None => done = true
          Some((COMMENT(_) | NEWLINE | SEMI(_), _, _)) => offset += 1
          Some((_, _, token_end)) => {
            recovery_end = token_end
            offset += 1
          }
        }
      }
      state.report_expected_tokens_at(LPAREN, mk_loc(start, recovery_end), [
        TK_LIDENT,
        TK_EXTEND,
        TK_PACKAGE_NAME,
      ])
      state.suppress_next_top_level_recovery_report = true
    }
    _ => ()
  }
}

///|
fn parse_expr_simple(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  match state.peek() {
    Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_missing_expr_from_entries(
            loc~,
            source_span~,
            entries~,
          )
        },
        [],
      )
    Some((LOOP, _, _)) | Some((THROW, _, _)) | Some((QUESTION, _, _)) =>
      parse_invalid_simple_expr(state, stop_kinds)
    Some((IF, _, _)) => parse_if_expr(state)
    Some((MATCH, _, _)) => parse_match_expr(state)
    Some((FORALL, _, _)) | Some((EXISTS, _, _)) =>
      parse_quantifier_expr(state, stop_kinds)
    Some((LET, _, _)) => parse_let_statement(state)
    Some((LETREC, _, _)) => parse_letrec_statement(state)
    Some((GUARD, _, _)) | Some((GUARD_EXCLAMATION, _, _)) =>
      parse_guard_statement(state)
    Some((DEFER, _, _)) | Some((ERRDEFER, _, _)) => parse_defer_statement(state)
    Some((PROOF_ASSERT, _, _)) => parse_proof_assert_statement(state)
    Some((PROOF_LET, _, _)) => parse_proof_let_statement(state)
    Some((TRY, _, _)) | Some((TRY_EXCLAMATION, _, _)) => parse_try_expr(state)
    Some((RETURN, _, _)) =>
      parse_keyword_value_expr(
        state,
        fn(loc, source_span, children) {
          let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let expr = @construction.first_child_with_label_from(children, "expr")
          @node.CstNode::expr_return(loc~, source_span~, kw~, expr?)
        },
        stop_kinds,
        value_is_required=false,
      )
    Some((RAISE, _, _)) =>
      parse_keyword_value_expr(
        state,
        fn(loc, source_span, children) {
          let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          @node.CstNode::expr_raise(loc~, source_span~, kw~, expr~)
        },
        stop_kinds,
        value_is_required=true,
      )
    Some((BREAK, _, _)) => parse_break_expr(state, stop_kinds)
    Some((CONTINUE, _, _)) => parse_continue_expr(state, stop_kinds)
    Some((WHILE, _, _)) =>
      parse_keyword_body_expr(
        state,
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_while(loc~, source_span~, entries~)
        },
        is_while=true,
      )
    Some((FOR, _, _)) => parse_for_expr(state)
    Some((LEXMATCH, _, _)) =>
      parse_lex_block_expr(state, fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::expr_lex_match(loc~, source_span~, entries~)
      })
    Some((LEXSCAN, _, _)) =>
      parse_lex_block_expr(state, fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::expr_lex_scan(loc~, source_span~, entries~)
      })
    Some((FN, _, _)) | Some((ASYNC, _, _)) =>
      parse_function_expr(state, stop_kinds)
    Some((LBRACE, _, _)) =>
      if brace_starts_record_expr(state) {
        parse_record_expr(state)
      } else if brace_starts_map_expr(state) {
        parse_map_expr(state)
      } else {
        parse_block_expr(state, classify_record_pun=true)
      }
    Some((LPAREN, _, _)) => parse_paren_expr(state, stop_kinds)
    Some((LBRACKET, _, _)) | Some((LBRACKET_BAR, _, _)) =>
      parse_array_expr(state)
    Some((AMPER, _, _)) =>
      match state.peek(offset=1) {
        Some((UIDENT(_), _, _)) | Some((PACKAGE_NAME(_), _, _)) =>
          parse_type_name_node(state)
        _ =>
          if state.consume_any() is Some(token) {
            state.construct_from_children(
              fn(loc, source_span, children) {
                push_structured_name(
                  children,
                  "semantic_name",
                  fn(loc, source_span, children) {
                    let long_ident = @construction.first_child_with_label_from(
                      children, "long_ident",
                    ).unwrap_or(missing_at_end(loc~, source_span~))
                    @node.CstNode::name_var(loc~, source_span~, long_ident~)
                  },
                  ["name"],
                )

                let name = @construction.children_with_label_from(
                  children, "name",
                )
                let token = @construction.first_child_with_label_from(
                  children, "token",
                )
                let semantic_name = @construction.first_child_with_label_from(
                  children, "semantic_name",
                )
                @node.CstNode::expr_ident(
                  loc~,
                  source_span~,
                  name~,
                  token?,
                  semantic_name?,
                )
              },
              [(Some("op"), token)],
            )
          } else {
            state.construct_from_children(
              fn(loc, source_span, children) {
                let entries = recovery_children(children)
                @node.CstNode::expr_missing_expr_from_entries(
                  loc~,
                  source_span~,
                  entries~,
                )
              },
              [],
            )
          }
      }
    Some((tok, _, _)) if tok is (ELLIPSIS | UNDERSCORE) =>
      if state.consume_any() is Some(token) {
        hole_node_from_children(
          state,
          [(Some("token"), token)],
          todo=tok is ELLIPSIS,
        )
      } else {
        hole_node_from_children(state, [])
      }
    Some((STRING(_), _, _))
    | Some((BYTES(_), _, _))
    | Some((REGEX_LITERAL(_), _, _))
    | Some((INTERP(_), _, _))
    | Some((BYTES_INTERP(_), _, _))
    | Some((MULTILINE_STRING(_), _, _))
    | Some((MULTILINE_INTERP(_), _, _)) => parse_string_literal_node(state)
    Some((REGEX_INTERP(_), _, _)) => parse_regex_interp_lex_pattern_node(state)
    Some((TRUE, _, _))
    | Some((FALSE, _, _))
    | Some((CHAR(_), _, _))
    | Some((INT(_), _, _))
    | Some((BYTE(_), _, _))
    | Some((DOUBLE(_), _, _))
    | Some((FLOAT(_), _, _)) =>
      parse_single_token_expr(
        state,
        fn(loc, source_span, children) {
          push_direct_semantic_constant(children)

          let entries = recovery_children(children)
          @node.CstNode::expr_constant(loc~, source_span~, entries~)
        },
        "token",
      )
    Some((LIDENT(_), _, _))
    | Some((EXTEND, _, _))
    | Some((PACKAGE_NAME(_), _, _)) => parse_ident_or_qualified_expr(state)
    Some((UIDENT(_), _, _)) =>
      parse_single_token_expr(
        state,
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_constr(loc~, source_span~, entries~)
        },
        "name",
      )
    Some(_) => parse_invalid_simple_expr(state, stop_kinds)
    None =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_missing_expr_from_entries(
            loc~,
            source_span~,
            entries~,
          )
        },
        [],
      )
  }
}

///|
fn parse_keyword_value_expr(
  state : ParserState,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  stop_kinds : Array[TokenKind],
  value_is_required~ : Bool,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  if value_is_required ||
    (
      !state.current_in(stop_kinds) &&
      !state.current_is(TK_SEMI) &&
      !state.current_is(TK_RBRACE) &&
      !state.current_is(TK_EOF)
    ) {
    let reports_before_expr = state.reports.length()
    let expr = parse_expr_until(state, stop_kinds)
    if value_is_required {
      push_child(
        children,
        Some("expr"),
        state.required_expr_node(expr, reports_before_expr),
      )
    } else if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
    }
  }
  state.construct_from_children(builder, children)
}

///|
fn parse_break_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  match state.peek() {
    Some((POST_LABEL(_), _, _)) =>
      if state.consume_any() is Some(label) {
        push_child(children, Some("label"), label)
      } else {
        ()
      }
    Some((POST_LABEL_TRANSPARENT(name), _, _)) =>
      if state.consume_any() is Some(label) {
        push_child(children, Some("label"), transparent_label_node(label, name))
      } else {
        ()
      }
    _ => ()
  }
  if !state.current_in(stop_kinds) &&
    !state.current_is(TK_SEMI) &&
    !state.current_is(TK_RBRACE) &&
    !state.current_is(TK_EOF) {
    let expr = parse_expr_until(state, stop_kinds)
    if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let label = @construction.first_child_with_label_from(children, "label")
      let expr = @construction.first_child_with_label_from(children, "expr")
      @node.CstNode::expr_break(loc~, source_span~, kw~, label?, expr?)
    },
    children,
  )
}

///|
fn parse_continue_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut anchor : CstNode? = None
  if state.consume_any() is Some(kw) {
    anchor = Some(kw)
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  match state.peek() {
    Some((POST_LABEL(_), _, _)) if !(state.peek(offset=1).map(item => item.0)
      is Some(COLON)) =>
      if state.consume_any() is Some(label) {
        anchor = Some(label)
        push_child(children, Some("label"), label)
      } else {
        ()
      }
    Some((POST_LABEL_TRANSPARENT(name), _, _)) if !(state
      .peek(offset=1)
      .map(item => item.0)
      is Some(COLON)) =>
      if state.consume_any() is Some(label) {
        anchor = Some(label)
        push_child(children, Some("label"), transparent_label_node(label, name))
      } else {
        ()
      }
    _ => ()
  }
  let has_same_line_arg = if anchor is Some(item) {
    !state.current_starts_after_node_line(item) && !state.current_is(TK_NEWLINE)
  } else {
    false
  }
  if has_same_line_arg &&
    !state.current_in(stop_kinds) &&
    !state.current_is(TK_SEMI) &&
    !state.current_is(TK_RBRACE) &&
    !state.current_is(TK_EOF) {
    let args = parse_continue_argument_list(state, stop_kinds)
    if !args.children.is_empty() {
      push_child(children, Some("arg"), args)
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let label = @construction.first_child_with_label_from(children, "label")
      let arg = @construction.first_child_with_label_from(children, "arg")
      @node.CstNode::expr_continue(loc~, source_span~, kw~, label?, arg?)
    },
    children,
  )
}

///|
fn labelled_loop_prefix_starts(state : ParserState) -> Bool {
  match (state.peek(), state.peek(offset=1)) {
    (Some((POST_LABEL(_), _, _)), Some((COLON, _, _))) => {
      let mut offset = 2
      let mut done = false
      while !done {
        match state.peek(offset~) {
          Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
          Some((LBRACE, _, _)) | Some((FOR, _, _)) | Some((WHILE, _, _)) =>
            return true
          _ => done = true
        }
      }
      false
    }
    _ => false
  }
}

///|
fn transparent_label_node(label : CstNode, name : String) -> CstNode {
  node(
    fn(loc, source_span, children) {
      let long_ident = @construction.first_child_with_label_from(
        children, "long_ident",
      )
      let payload = @construction.first_child_with_label_from(
        children, "payload",
      )
      let syntax = @construction.first_child_with_label_from(children, "syntax")
      let name_value = @construction.first_child_with_label_from(
        children, "name_value",
      )
      @node.CstNode::name_label(
        loc~,
        source_span~,
        long_ident?,
        payload?,
        syntax?,
        name_value?,
      )
    },
    label.loc,
    label.source_span,
    [
      (Some("syntax"), label),
      (
        Some("name_value"),
        scalar_node(
          fn(loc, source_span, children) {
            let long_ident = @construction.first_child_with_label_from(
              children, "long_ident",
            )
            let payload = @construction.first_child_with_label_from(
              children, "payload",
            )
            let syntax = @construction.first_child_with_label_from(
              children, "syntax",
            )
            let name_value = @construction.first_child_with_label_from(
              children, "name_value",
            )
            @node.CstNode::name_label(
              loc~,
              source_span~,
              long_ident?,
              payload?,
              syntax?,
              name_value?,
            )
          },
          label.loc,
          label.source_span,
          String("\{name}~"),
        ),
      ),
    ],
  )
}

///|
fn parse_labelled_loop_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let transparent_name = match state.peek() {
    Some((POST_LABEL_TRANSPARENT(name), _, _)) => Some(name)
    _ => None
  }
  if state.consume_any() is Some(label) {
    let semantic_label = match transparent_name {
      Some(name) => transparent_label_node(label, name)
      None => label
    }
    push_child(children, Some("label"), semantic_label)
  } else {
    ()
  }
  if state.consume_any() is Some(op) {
    push_child(children, Some("op"), op)
  } else {
    ()
  }
  push_children(children, state.consume_trivia())
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
    return state.construct_from_children(
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::expr_labelled_block(loc~, source_span~, entries~)
      },
      children,
    )
  }
  let loop_node = match state.peek() {
    Some((FOR, _, _)) if transparent_name is None => parse_for_expr(state)
    Some((WHILE, _, _)) if transparent_name is None =>
      parse_keyword_body_expr(
        state,
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_while(loc~, source_span~, entries~)
        },
        is_while=true,
      )
    Some((found, start, end)) => {
      let expected : Array[TokenKind] = if transparent_name is Some(_) {
        [TK_LBRACE]
      } else {
        [TK_LBRACE, TK_FOR, TK_WHILE]
      }
      state.report_expected_tokens_at(found, mk_loc(start, end), expected)
      let invalid : Array[(String?, CstNode)] = []
      while !state.current_in(stop_kinds) &&
            !state.current_is(TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(
            invalid,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          ()
        }
      }
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_missing_expr_from_entries(
            loc~,
            source_span~,
            entries~,
          )
        },
        invalid,
      )
    }
    None =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_missing_expr_from_entries(
            loc~,
            source_span~,
            entries~,
          )
        },
        [],
      )
  }
  for i in 0..<loop_node.children.length() {
    if loop_node.children.get(i) is Some(child) {
      children.push(child)
    } else {
      ()
    }
  }
  let (_, first) = children[0]
  let loc = first.loc.merge(loop_node.loc)
  let source_span = (first.source_span.0, loop_node.source_span.1)
  let entries = recovery_children(children)
  match loop_node.kind {
    Expr_For => @node.CstNode::expr_for(loc~, source_span~, entries~)
    Expr_ForEach => @node.CstNode::expr_for_each(loc~, source_span~, entries~)
    Expr_While => @node.CstNode::expr_while(loc~, source_span~, entries~)
    Expr_MissingExpr =>
      @node.CstNode::expr_missing_expr_from_entries(
        loc~,
        source_span~,
        entries~,
      )
    _ => abort("expected a labelled loop or its recovery node")
  }
}

///|
fn parse_continue_argument_list(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let item_stops = stop_kinds.copy()
  item_stops.push(TK_COMMA)
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((SEMI(_), _, _)) | Some((RBRACE, _, _)) | Some((EOF, _, _)) | None =>
        done = true
      Some((COMMA, _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          done = true
        }
      Some(_) => {
        let expr = parse_expr_until(state, item_stops)
        if !expr.children.is_empty() {
          push_child(children, Some("item"), expr)
        }
        push_children(children, state.consume_trivia())
        if state.current_is(TK_COMMA) {
          if state.consume_any() is Some(sep) {
            push_child(children, Some("sep"), sep)
          } else {
            ()
          }
        }
      }
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_continue_argument_list(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn discard_expr_trivia(state : ParserState) -> Unit {
  match state.peek() {
    Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => {
      let _ = state.consume_trivia()
    }
    _ => ()
  }
}

///|
fn parse_single_token_expr(
  state : ParserState,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  label : String,
) -> CstNode {
  match state.peek() {
    Some((raw, _, _)) =>
      if state.consume_any() is Some(token) {
        let children : Array[(String?, CstNode)] = [(Some(label), token)]
        state.construct_from_children(
          fn(loc, source_span, children) {
            if label == "token" {
              if constant_node_from_token(raw, token.loc, token.source_span)
                is Some(constant) {
                push_child(children, Some("constant"), constant)
              }
            } else {
              push_structured_name(
                children,
                "semantic_name",
                fn(loc, source_span, children) {
                  let long_ident = @construction.first_child_with_label_from(
                    children, "long_ident",
                  ).unwrap_or(missing_at_end(loc~, source_span~))
                  @node.CstNode::name_constructor(
                    loc~,
                    source_span~,
                    long_ident~,
                  )
                },
                ["pkg", "type", "name", "field"],
              )
              push_constructor_extra_info(children, loc, source_span)
            }
            builder(loc, source_span, children)
          },
          children,
        )
      } else {
        state.construct_from_children(
          fn(loc, source_span, children) {
            let entries = recovery_children(children)
            @node.CstNode::expr_missing_expr_from_entries(
              loc~,
              source_span~,
              entries~,
            )
          },
          [],
        )
      }
    None =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_missing_expr_from_entries(
            loc~,
            source_span~,
            entries~,
          )
        },
        [],
      )
  }
}

///|
fn parse_ident_or_qualified_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let allow_qualified = match state.peek() {
    Some((PACKAGE_NAME(_), _, _)) => true
    _ => false
  }
  if state.consume_any() is Some(first) {
    push_child(children, Some("name"), first)
  } else {
    ()
  }
  match state.peek() {
    Some((DOT_LIDENT(_), _, _)) if allow_qualified => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
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
              @node.CstNode::name_var(loc~, source_span~, long_ident~)
            },
            ["name"],
          )

          let name = @construction.children_with_label_from(children, "name")
          let token = @construction.first_child_with_label_from(
            children, "token",
          )
          let semantic_name = @construction.first_child_with_label_from(
            children, "semantic_name",
          )
          @node.CstNode::expr_ident(
            loc~,
            source_span~,
            name~,
            token?,
            semantic_name?,
          )
        },
        children,
      )
    }
    Some((DOT_UIDENT(_), _, _)) if allow_qualified => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
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
    }
    _ =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          push_structured_name(
            children,
            "semantic_name",
            fn(loc, source_span, children) {
              let long_ident = @construction.first_child_with_label_from(
                children, "long_ident",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              @node.CstNode::name_var(loc~, source_span~, long_ident~)
            },
            ["name"],
          )

          let name = @construction.children_with_label_from(children, "name")
          let token = @construction.first_child_with_label_from(
            children, "token",
          )
          let semantic_name = @construction.first_child_with_label_from(
            children, "semantic_name",
          )
          @node.CstNode::expr_ident(
            loc~,
            source_span~,
            name~,
            token?,
            semantic_name?,
          )
        },
        children,
      )
  }
}

///|
fn parse_type_name_node(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut is_object = false
  if state.current_is(TK_AMPER) {
    is_object = true
    if state.consume_any() is Some(op) {
      push_child(children, Some("op"), op)
    } else {
      ()
    }
  }
  match state.peek() {
    Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    Some((LIDENT(name), start, end)) => {
      state.report(
        mk_loc(start, end),
        "Unexpected lowercase identifier \{name}, expected uppercase identifier in type name.",
      )
      if state.consume_any() is Some(bad) {
        push_child(
          children,
          Some("name"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
    }
    Some((PACKAGE_NAME(_), _, _)) => {
      if state.consume_any() is Some(pkg) {
        push_child(children, Some("name"), pkg)
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
        _ =>
          push_child(
            children,
            Some("name"),
            state.missing_token_node_with_expected(TK_UIDENT, [
              TK_DOT_LIDENT,
              TK_DOT_UIDENT,
            ]),
          )
      }
    }
    _ =>
      push_child(
        children,
        Some("name"),
        state.missing_token_node_with_expected(TK_UIDENT, [
          TK_UIDENT,
          TK_PACKAGE_NAME,
        ]),
      )
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
      push_bool_marker(
        children,
        loc,
        source_span,
        "object",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_object(value~, loc~, source_span~)
        },
        is_object,
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
fn parse_paren_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let arrow_params = !kind_in(TK_FAT_ARROW, stop_kinds) &&
    paren_expr_is_followed_by_fat_arrow(state)
  if !arrow_params && paren_expr_has_top_level_constraint(state) {
    return parse_constraint_paren_expr(state)
  }
  let list = if arrow_params {
    parse_delimited(
      state,
      TK_LPAREN,
      TK_RPAREN,
      parse_arrow_parameter_expr_item,
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_tuple_element_list(loc~, source_span~, entries~)
      },
      sep_kind=TokenKind::TK_COMMA,
      item_expected="simple expression",
      reports_else_recovery=true,
    )
  } else {
    parse_delimited(
      state,
      TK_LPAREN,
      TK_RPAREN,
      parse_tuple_expr_item,
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_tuple_element_list(loc~, source_span~, entries~)
      },
      sep_kind=TokenKind::TK_COMMA,
      reports_else_recovery=true,
    )
  }
  if arrow_params {
    for entry in list.children {
      match entry {
        (Some("item"), item) =>
          if invalid_arrow_parameter_expr(item) is Some(invalid) {
            state.report(
              invalid.loc,
              "Unexpected expression. You may expect parameter here.",
            )
          } else {
            ()
          }
        _ => ()
      }
    }
  }
  let item_count = list.children_with_label("item").length()
  let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if item_count ==
    0 {
    fn(loc, source_span, children) {
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let faked = @construction.first_child_with_label_from(children, "faked").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::expr_unit(loc~, source_span~, body~, faked~)
    }
  } else if item_count == 1 {
    fn(loc, source_span, children) {
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let group = @construction.first_child_with_label_from(children, "group").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::expr_group(loc~, source_span~, body~, group~)
    }
  } else {
    fn(loc, source_span, children) {
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::expr_tuple(loc~, source_span~, body~)
    }
  }
  let children : Array[(String?, CstNode)] = [(Some("body"), list)]
  if item_count == 0 {
    push_child(
      children,
      Some("faked"),
      bool_marker_node(
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_faked(value~, loc~, source_span~)
        },
        list.loc.end,
        list.source_span.1,
        false,
      ),
    )
  } else if item_count == 1 {
    push_child(
      children,
      Some("group"),
      marker_node(
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::group_paren(loc~, source_span~)
        },
        list.loc.start,
        list.source_span.0,
      ),
    )
  }
  node(builder, list.loc, list.source_span, children)
}

///|
fn parse_arrow_parameter_expr_item(state : ParserState) -> CstNode {
  let expr = parse_tuple_expr_item(state)
  if !state.current_is(TK_COLON) {
    return expr
  }
  let children : Array[(String?, CstNode)] = [(Some("expr"), expr)]
  if state.consume_any() is Some(colon) {
    push_child(children, Some("op"), colon)
  } else {
    ()
  }
  let reports_before_type = state.reports.length()
  let ty = parse_type_arrow(state, [TK_COMMA, TK_RPAREN, TK_EOF])
  push_child(
    children,
    Some("type"),
    state.required_type_node(ty, reports_before_type),
  )
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_constraint(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn paren_expr_is_followed_by_fat_arrow(state : ParserState) -> Bool {
  let mut offset = 1
  let mut depth = 0
  while true {
    match state.peek(offset~) {
      Some((LPAREN, _, _))
      | Some((LBRACKET, _, _))
      | Some((LBRACE, _, _))
      | Some((LBRACKET_BAR, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RPAREN, _, _)) if depth == 0 => {
        offset += 1
        while true {
          match state.peek(offset~) {
            Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
            Some((FAT_ARROW, _, _)) => return true
            _ => return false
          }
        }
      }
      Some((RPAREN, _, _))
      | Some((RBRACKET, _, _))
      | Some((RBRACE, _, _))
      | Some((BAR_RBRACKET, _, _)) => {
        if depth > 0 {
          depth -= 1
        }
        offset += 1
      }
      Some((EOF, _, _)) | None => return false
      Some(_) => offset += 1
    }
  }
  false
}

///|
fn parse_tuple_expr_item(state : ParserState) -> CstNode {
  // Tuple entries retain their recovery shape. The shared argument parsers
  // materialize pun locations and values only when parsing call arguments.
  let item_stops = [
    TokenKind::TK_COLON,
    TokenKind::TK_COMMA,
    TokenKind::TK_RPAREN,
    TokenKind::TK_EOF,
  ]
  if state.current_argument_starts_optional_forwarding() {
    parse_optional_forwarding_argument(state)
  } else if state.current_argument_starts_labelled() {
    parse_labelled_argument(state)
  } else {
    match state.peek_significant() {
      Some((tok, _, _)) if statement_only_expression_token(tok) =>
        parse_invalid_simple_expr(state, item_stops)
      _ => {
        let reports_before_expr = state.reports.length()
        state.required_expr_node(
          parse_expr_until(
            state,
            item_stops,
            allow_argument_label_question=true,
          ),
          reports_before_expr,
        )
      }
    }
  }
}

///|
fn paren_expr_has_top_level_constraint(state : ParserState) -> Bool {
  let mut offset = 1
  let mut depth = 0
  let mut saw_colon = false
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((POST_LABEL(_), _, _)) | Some((POST_LABEL_TRANSPARENT(_), _, _)) if depth ==
        0 &&
        state.peek(offset=offset + 1).map(item => item.0) is Some(COLON) =>
        // This colon starts a labelled expression; it is part of the
        // expression rather than the `expr : Type` separator sought here.
        offset += 2
      Some((LPAREN, _, _))
      | Some((LBRACKET, _, _))
      | Some((LBRACE, _, _))
      | Some((LBRACKET_BAR, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RPAREN, _, _)) =>
        if depth == 0 {
          return saw_colon
        } else {
          depth -= 1
          offset += 1
        }
      Some((RBRACKET, _, _))
      | Some((BAR_RBRACKET, _, _))
      | Some((RBRACE, _, _)) => {
        if depth > 0 {
          depth -= 1
        }
        offset += 1
      }
      Some((COMMA, _, _)) if depth == 0 => return false
      Some((COLON, _, _)) if depth == 0 => {
        saw_colon = true
        offset += 1
      }
      Some((EOF, _, _)) | None => done = true
      Some(_) => offset += 1
    }
  }
  false
}

///|
fn parse_constraint_paren_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(open) {
    push_child(children, Some("open"), open)
  } else {
    ()
  }
  let expr = parse_expr_until(state, [TK_COLON, TK_RPAREN, TK_EOF])
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  if state.current_is(TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
  }
  let ty = parse_type_until(state, [TK_RPAREN, TK_EOF])
  if !ty.children.is_empty() {
    push_child(children, Some("type"), ty)
  }
  push_children(children, state.consume_trivia())
  push_child(children, Some("close"), state.expect_token(TokenKind::TK_RPAREN))
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_constraint(loc~, source_span~, entries~)
    },
    children,
  )
}
