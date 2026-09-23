///|
fn parse_quantifier_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let quantifier_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if state.current_is(
      TokenKind::TK_EXISTS,
    ) {
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::quantifier_kind_exists(loc~, source_span~)
    }
  } else {
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::quantifier_kind_forall(loc~, source_span~)
    }
  }
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
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
    let ty = parse_type_arrow(state, [TK_COMMA, TK_EOF])
    if !ty.children.is_empty() {
      push_child(children, Some("type"), ty)
    }
  }
  if state.current_is(TK_COMMA) {
    if state.consume_any() is Some(sep) {
      push_child(children, Some("sep"), sep)
    } else {
      ()
    }
  }
  let body = parse_expr_until(state, stop_kinds)
  if !body.children.is_empty() {
    push_child(children, Some("body"), body)
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_marker_child(
        children, loc, source_span, "quantifier_kind", quantifier_builder,
      )

      let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let type_ = @construction.first_child_with_label_from(children, "type").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let sep = @construction.first_child_with_label_from(children, "sep").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let quantifier_kind = @construction.first_child_with_label_from(
        children, "quantifier_kind",
      )
      @node.CstNode::expr_quantifier(
        loc~,
        source_span~,
        kw~,
        name~,
        op~,
        type_~,
        sep~,
        body~,
        quantifier_kind?,
      )
    },
    children,
  )
}

///|
fn parse_lex_block_expr(
  state : ParserState,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let expr = parse_expr_until(state, [TK_LBRACE, TK_WITH, TK_EOF])
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  if state.current_is(TK_WITH) {
    if state.consume_any() is Some(with_kw) {
      push_child(children, Some("kw"), with_kw)
    } else {
      ()
    }
    if state.consume_any() is Some(strategy) {
      push_child(children, Some("name"), strategy)
    } else {
      ()
    }
  }
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("case"), parse_lexscan_case_block(state))
  }
  state.construct_from_children(builder, children)
}

///|
fn parse_lexscan_case_block(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("open"), state.expect_token(TK_LBRACE))
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((RBRACE, _, _)) => {
        push_child(children, Some("close"), state.expect_token(TK_RBRACE))
        done = true
      }
      Some((EOF, _, _)) | None => {
        push_child(children, Some("close"), state.missing_token_node(TK_RBRACE))
        done = true
      }
      Some((SEMI(_), _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          ()
        }
      Some((_, _, _)) =>
        push_child(children, Some("case"), parse_lexscan_case(state))
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_lex_scan_case_list(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_lexscan_case_pattern(state : ParserState) -> CstNode {
  let stops = [TokenKind::TK_FAT_ARROW, TK_SEMI, TK_RBRACE, TK_EOF]
  match state.peek() {
    Some((UNDERSCORE, _, _)) | Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      parse_pattern_until(state, stops)
    _ => parse_regex_as_pattern(state, stops)
  }
}

///|
fn lexscan_paren_starts_bindings(state : ParserState) -> Bool {
  let mut depth = 0
  let mut offset = 0
  while true {
    match state.peek(offset~) {
      Some((LPAREN, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RPAREN, _, _)) => {
        if depth <= 1 {
          return false
        }
        depth -= 1
        offset += 1
      }
      Some((COMMA, _, _)) if depth == 1 => return true
      Some((EOF, _, _)) | None => return false
      Some(_) => offset += 1
    }
  }
  false
}

///|
fn parse_lexscan_case(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((ELLIPSIS, _, _)) => {
      if state.consume_any() is Some(todo) {
        let wildcard = state.construct_from_children(
          fn(loc, source_span, children) {
            push_marker_child(
              children,
              loc,
              source_span,
              "lex_scan_case_pattern",
              fn(loc, source_span, children) {
                let _ = children
                @node.CstNode::lex_scan_case_pattern_wildcard(
                  loc~,
                  source_span~,
                )
              },
            )

            let entries = recovery_children(children)
            @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
          },
          [(Some("token"), todo)],
        )
        push_child(children, Some("pattern"), wildcard)
        push_child(children, Some("op"), todo)
        push_child(
          children,
          Some("body"),
          hole_node_from_children(state, [(Some("token"), todo)], todo=true),
        )
      } else {
        ()
      }
      return state.construct_from_children(
        fn(loc, source_span, children) {
          let pattern = @construction.first_child_with_label_from(
            children, "pattern",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          @node.CstNode::case_lex_scan_case(
            loc~,
            source_span~,
            pattern~,
            op~,
            body~,
          )
        },
        children,
      )
    }
    _ => ()
  }
  let parsed_pat = if state.current_is(TK_LPAREN) &&
    lexscan_paren_starts_bindings(state) {
    parse_regex_match_rhs(state, [TK_FAT_ARROW, TK_SEMI, TK_RBRACE, TK_EOF])
  } else {
    parse_lexscan_case_pattern(state)
  }
  let pat = finish_lexscan_case_pattern(parsed_pat)
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  push_child(children, Some("op"), state.expect_token(TK_FAT_ARROW))
  let body = parse_expr_with_context_restriction(
    state,
    [TK_SEMI, TK_RBRACE, TK_EOF],
    "the action part of a matching case",
    token_disallowed_in_match_action,
  )
  if !body.children.is_empty() {
    push_child(children, Some("body"), body)
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let pattern = @construction.first_child_with_label_from(
        children, "pattern",
      ).unwrap_or(missing_at_end(loc~, source_span~))
      let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::case_lex_scan_case(
        loc~,
        source_span~,
        pattern~,
        op~,
        body~,
      )
    },
    children,
  )
}

///|
fn parse_function_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.current_is(TK_ASYNC) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
  }
  if state.current_is(TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
  }
  if state.current_is(TK_EXCLAMATION) {
    if state.consume_error_for_progress() is Some(error) {
      push_child(children, Some("error"), error)
    } else {
      ()
    }
  }
  match state.peek() {
    Some((tok, start, end)) if !(tok is LPAREN) => {
      state.report(
        mk_loc(start, end),
        "Failed to parse func: expected '(' or '{', got \{@debug.render(Debug::to_repr(tok))}",
      )
      let scan = state.recover_until(stop_kinds)
      push_recovered_errors(children, scan.skipped)
      return function_node_from_children(state, children, fn(
        loc,
        source_span,
        children,
      ) {
        let _ = children
        @node.CstNode::fn_kind_lambda(loc~, source_span~)
      })
    }
    _ => ()
  }
  let reports_before_signature = state.reports.length()
  if state.current_is(TK_LPAREN) {
    push_child(children, Some("params"), parse_parameter_list(state))
  }
  if state.current_is(TK_THIN_ARROW) ||
    state.current_is(TK_RAISE) ||
    state.current_is(TK_NORAISE) ||
    state.current_is(TK_NOCANCEL) {
    push_child(children, Some("type"), parse_func_return_type(state))
  }
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    let body = if state.reports.length() > reports_before_signature {
      state.missing_token_node_silent(TK_LBRACE)
    } else {
      state.missing_token_node_with_expected(TK_LBRACE, [TK_LBRACE])
    }
    push_child(children, Some("body"), body)
  }
  function_node_from_children(state, children, fn(loc, source_span, children) {
    let _ = children
    @node.CstNode::fn_kind_lambda(loc~, source_span~)
  })
}

///|
fn parse_case_block(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("open"), state.expect_token(TK_LBRACE))
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((RBRACE, _, _)) => {
        push_child(children, Some("close"), state.expect_token(TK_RBRACE))
        done = true
      }
      Some((EOF, _, _)) | None => {
        push_child(children, Some("close"), state.missing_token_node(TK_RBRACE))
        done = true
      }
      Some((SEMI(_), _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          ()
        }
      Some((COMMA, _, _)) =>
        if state.consume_invalid_series_delimiter() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      Some((_, _, _)) =>
        push_child(children, Some("case"), parse_match_case(state))
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_match_case_list(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_match_case(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((ELLIPSIS, _, _)) => {
      if state.consume_any() is Some(todo) {
        push_child(
          children,
          Some("pattern"),
          state.construct_from_children(
            fn(loc, source_span, children) {
              let entries = recovery_children(children)
              @node.CstNode::pattern_missing_pattern(
                loc~,
                source_span~,
                entries~,
              )
            },
            [(Some("token"), todo)],
          ),
        )
        push_child(
          children,
          Some("body"),
          hole_node_from_children(state, [(Some("token"), todo)], todo=true),
        )
      } else {
        ()
      }
      return state.construct_from_children(
        fn(loc, source_span, children) {
          let pattern = @construction.first_child_with_label_from(
            children, "pattern",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let kw = @construction.first_child_with_label_from(children, "kw")
          let cond = @construction.first_child_with_label_from(children, "cond")
          let op = @construction.first_child_with_label_from(children, "op")
          let error = @construction.children_with_label_from(children, "error")
          let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          @node.CstNode::case_match_case(
            loc~,
            source_span~,
            pattern~,
            kw?,
            cond?,
            op?,
            error~,
            body~,
          )
        },
        children,
      )
    }
    _ => ()
  }
  let reports_before_pattern = state.reports.length()
  let pat = parse_pattern_until(state, [
    TK_IF,
    TK_FAT_ARROW,
    TK_SEMI,
    TK_RBRACE,
    TK_EOF,
  ])
  let pat = state.required_pattern_node(pat, reports_before_pattern)
  if !pat.children.is_empty() {
    if pat.kind is Pattern_Constraint {
      if pat.first_child_with_label("op") is Some(op) {
        state.report_expected_tokens_at(COLON, op.loc, [TK_FAT_ARROW])
      } else {
        ()
      }
    }
  }
  push_child(children, Some("pattern"), pat)
  let mut saw_guard = false
  if state.current_is(TK_IF) {
    saw_guard = true
    if state.consume_any() is Some(if_token) {
      push_child(children, Some("kw"), if_token)
    } else {
      ()
    }
    let guard_expr = parse_pattern_guard_expr(state, [TK_FAT_ARROW, TK_EOF])
    if !guard_expr.children.is_empty() {
      push_child(children, Some("cond"), guard_expr)
    }
  }
  let reports_before_body = reports_before_pattern
  let has_arrow = state.current_is(TK_FAT_ARROW)
  let arrow = if !has_arrow &&
    state.reports.length() > reports_before_pattern &&
    !saw_guard &&
    !pat.has_child_or_grandchild_kind(fn(kind) { kind is Pattern_With }) {
    state.missing_token_node_silent(TokenKind::TK_FAT_ARROW)
  } else {
    state.expect_token(TK_FAT_ARROW)
  }
  push_child(children, Some("op"), arrow)
  if has_arrow {
    let body = parse_expr_with_context_restriction(
      state,
      [TK_SEMI, TK_RBRACE, TK_EOF],
      "the action part of a matching case",
      token_disallowed_in_match_action,
    )
    push_child(
      children,
      Some("body"),
      state.required_expr_node(body, reports_before_body),
    )
  } else {
    recover_expr_until_statement_boundary(state, children, balance_groups=false)
    push_child(children, Some("body"), state.missing_expr_node())
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let pattern = @construction.first_child_with_label_from(
        children, "pattern",
      ).unwrap_or(missing_at_end(loc~, source_span~))
      let kw = @construction.first_child_with_label_from(children, "kw")
      let cond = @construction.first_child_with_label_from(children, "cond")
      let op = @construction.first_child_with_label_from(children, "op")
      let error = @construction.children_with_label_from(children, "error")
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::case_match_case(
        loc~,
        source_span~,
        pattern~,
        kw?,
        cond?,
        op?,
        error~,
        body~,
      )
    },
    children,
  )
}

///|
fn parse_match_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut has_open = false
  if state.consume_any() is Some(match_token) {
    push_child(children, Some("kw"), match_token)
  } else {
    ()
  }
  let reports_before_scrutinee = state.reports.length()
  let scrutinee = parse_expr_with_context_restriction(
    state,
    body_delimited_condition_stops(state),
    "condition",
    token_disallowed_in_condition,
  )
  push_child(
    children,
    Some("expr"),
    state.required_expr_node(scrutinee, reports_before_scrutinee),
  )
  match state.peek() {
    Some((LBRACE, _, _)) =>
      if state.consume_any() is Some(open) {
        has_open = true
        push_child(children, Some("open"), open)
      } else {
        ()
      }
    _ => {
      push_child(children, Some("open"), state.missing_token_node(TK_LBRACE))
      let scan = state.recover_until([TK_LBRACE, TK_SEMI, TK_RBRACE])
      push_recovered_errors(children, scan.skipped)
      if state.current_is(TK_LBRACE) {
        if state.consume_any() is Some(open) {
          has_open = true
          push_child(children, Some("open"), open)
        } else {
          ()
        }
      } else {
        return state.construct_from_children(
          fn(loc, source_span, children) {
            let entries = recovery_children(children)
            @node.CstNode::expr_match(loc~, source_span~, entries~)
          },
          children,
        )
      }
    }
  }
  let reports_before_cases = state.reports.length()
  let mut saw_case = false
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((RBRACE, _, _)) => {
        if state.consume_any() is Some(close) {
          push_child(children, Some("close"), close)
        } else {
          ()
        }
        done = true
      }
      Some((EOF, _, _)) | None => {
        if has_open &&
          !saw_case &&
          state.reports.length() == reports_before_cases {
          push_child(children, Some("case"), parse_match_case(state))
          saw_case = true
        }
        if has_open {
          let close = if state.reports.length() == reports_before_cases &&
            saw_case {
            state.missing_token_node(TK_RBRACE)
          } else {
            state.missing_token_node_silent(TK_RBRACE)
          }
          push_child(children, Some("close"), close)
        }
        done = true
      }
      Some((SEMI(_), _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          ()
        }
      Some((COMMA, _, _)) =>
        if state.consume_invalid_series_delimiter() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      _ => {
        push_child(children, Some("case"), parse_match_case(state))
        saw_case = true
      }
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_match(loc~, source_span~, entries~)
    },
    children,
  )
}
