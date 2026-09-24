///|
fn parse_if_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let reports_before_cond = state.reports.length()
  let cond = parse_expr_with_context_restriction(
    state,
    body_delimited_condition_stops(state),
    "condition",
    token_disallowed_in_condition,
  )
  push_child(
    children,
    Some("cond"),
    state.required_expr_node(cond, reports_before_cond),
  )
  let mut report_recovered_body_eof = false
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    state.report_expected_tokens([TK_LBRACE])
    report_recovered_body_eof = state.current_is(TK_LET) ||
      state.current_is(TK_LETREC)
    if recover_to_required_block(state, children) {
      push_child(children, Some("body"), parse_block_expr(state))
    } else {
      push_child(children, Some("body"), state.missing_expr_node())
    }
  }
  if report_recovered_body_eof {
    report_trailing_recovery_eof(state)
  }
  match (state.peek_significant(), state.peek_nth_significant(1)) {
    (Some((ELSE, _, _)), _) => push_children(children, state.consume_trivia())
    (Some((SEMI(false), _, _)), Some((ELSE, _, _))) => {
      push_children(children, state.consume_trivia())
      if state.consume_any() is Some(inserted) {
        push_child(children, Some("trivia"), inserted)
      } else {
        ()
      }
      push_children(children, state.consume_trivia())
    }
    _ => ()
  }
  if state.current_is(TK_ELSE) {
    if state.consume_any() is Some(else_kw) {
      push_child(children, Some("else"), else_kw)
    } else {
      ()
    }
    let reports_before_else = state.reports.length()
    let else_body = match state.peek() {
      Some((IF, _, _)) => parse_if_expr(state)
      Some((LBRACE, _, _)) => parse_block_expr(state)
      Some((found, start, end)) => {
        state.report_expected_tokens_at(found, mk_loc(start, end), [TK_LBRACE])
        parse_expr_until(state, [TK_SEMI, TK_EOF])
      }
      None => parse_expr_until(state, [TK_SEMI, TK_EOF])
    }
    push_child(
      children,
      Some("else"),
      state.required_expr_node(else_body, reports_before_else),
    )
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_if(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_try_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let is_try_operator = state.current_is(TokenKind::TK_TRY_EXCLAMATION)
  let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if is_try_operator {
    fn(loc, source_span, children) {
      let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let try_operator_kind = @construction.first_child_with_label_from(
        children, "try_operator_kind",
      )
      @node.CstNode::expr_try_operator(
        loc~,
        source_span~,
        kw~,
        body~,
        try_operator_kind?,
      )
    }
  } else {
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_try(loc~, source_span~, entries~)
    }
  }
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let reports_before_body = state.reports.length()
  let try_body_stops = [TokenKind::TK_CATCH, TK_SEMI, TK_RBRACE, TK_EOF]
  let panic_body_stops = [TokenKind::TK_SEMI, TK_RBRACE, TK_EOF]
  let body = match (is_try_operator, state.peek_significant()) {
    (false, Some((TRY_EXCLAMATION | LOOP, _, _))) =>
      parse_invalid_simple_expr(state, panic_body_stops)
    (true, Some((tok, _, _))) if token_disallowed_in_try_body(tok) =>
      parse_invalid_simple_expr(state, panic_body_stops)
    _ =>
      parse_expr_with_context_restriction(
        state, try_body_stops, "the try body", token_disallowed_in_try_body,
      )
  }
  let body = state.required_expr_node(body, reports_before_body)
  push_child(children, Some("body"), body)
  let mut saw_catch = false
  if state.current_is(TK_CATCH) {
    saw_catch = true
    if state.consume_any() is Some(catch_kw) {
      push_child(children, Some("kw"), catch_kw)
    } else {
      ()
    }
    if state.current_is(TK_LBRACE) {
      push_child(children, Some("case"), parse_case_block(state))
    } else {
      state.report_expected_tokens([TK_LBRACE])
      push_child(children, Some("case"), state.missing_expr_node())
    }
  }
  if state.current_is(TK_NORAISE) {
    if state.consume_any() is Some(else_kw) {
      push_child(children, Some("else_kw"), else_kw)
    } else {
      ()
    }
    if state.current_is(TK_LBRACE) {
      push_child(children, Some("else"), parse_case_block(state))
    }
  }
  if !is_try_operator &&
    !saw_catch &&
    state.reports.length() == reports_before_body {
    state.report_expected_tokens([TK_CATCH])
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      if is_try_operator {
        push_child(
          children,
          Some("try_operator_kind"),
          @node.CstNode::try_operator_kind_exclamation(
            loc=empty_loc(loc.end),
            source_span=empty_span(source_span.1),
          ),
        )
      } else {
        push_child(
          children,
          Some("has_try"),
          @node.CstNode::flag_has_try(
            value=true,
            loc=empty_loc(loc.end),
            source_span=empty_span(source_span.1),
          ),
        )
      }
      builder(loc, source_span, children)
    },
    children,
  )
}

///|
fn parse_keyword_body_expr(
  state : ParserState,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  is_while~ : Bool,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let reports_before_cond = state.reports.length()
  let cond = parse_expr_with_context_restriction(
    state,
    body_delimited_condition_stops(state),
    "condition",
    token_disallowed_in_condition,
  )
  push_child(
    children,
    Some("cond"),
    state.required_expr_node(cond, reports_before_cond),
  )
  let mut body_had_error = false
  if state.current_is(TK_LBRACE) {
    let reports_before_body = state.reports.length()
    push_child(children, Some("body"), parse_block_expr(state))
    body_had_error = state.reports.length() > reports_before_body
  } else {
    state.report_expected_tokens([TK_LBRACE])
    if recover_to_required_block(state, children) {
      let reports_before_body = state.reports.length()
      push_child(children, Some("body"), parse_block_expr(state))
      body_had_error = state.reports.length() > reports_before_body
    } else {
      push_child(children, Some("body"), state.missing_expr_node())
    }
  }
  if is_while && body_had_error {
    if state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("error"), parse_block_expr(state))
    }
    report_next_statement_after_separator(state)
  }
  if state.current_is(TK_NOBREAK) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    if state.current_is(TK_LBRACE) {
      push_child(children, Some("else"), parse_block_expr(state))
    }
  }
  state.construct_from_children(builder, children)
}

///|
fn parse_for_expr(state : ParserState) -> CstNode {
  if for_header_has_in(state, 1) {
    return parse_foreach_expr(state)
  }
  match state.peek_nth_significant(1) {
    Some((SEMI(_) | LBRACE, _, _)) => parse_c_style_for_expr(state)
    Some((UNDERSCORE | IN, _, _)) => parse_foreach_expr(state)
    Some((LIDENT(_) | EXTEND, _, _)) =>
      match state.peek_nth_significant(2) {
        Some((EQUAL, _, _)) => parse_c_style_for_expr(state)
        Some((COMMA | IN, _, _)) => parse_foreach_expr(state)
        _ => parse_invalid_for_header(state, true)
      }
    _ => parse_invalid_for_header(state, false)
  }
}

///|
fn for_header_has_in(state : ParserState, start_offset : Int) -> Bool {
  let mut offset = start_offset
  let mut depth = 0
  while true {
    let token = match state.peek_nth_significant(offset) {
      Some((token, _, _)) => token
      None => return false
    }
    if depth == 0 {
      match token {
        IN => return true
        EQUAL | SEMI(_) | FAT_ARROW | IF | EOF => return false
        _ => ()
      }
    }
    match token {
      LPAREN | LBRACKET | LBRACKET_BAR | LBRACE => depth += 1
      RPAREN | RBRACKET | BAR_RBRACKET | RBRACE => {
        if depth == 0 {
          return false
        }
        depth -= 1
      }
      EOF => return false
      _ => ()
    }
    offset += 1
  }
  false
}

///|
fn parse_invalid_for_header(state : ParserState, skip_binder : Bool) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  push_children(children, state.consume_trivia())
  if skip_binder {
    if state.consume_any() is Some(binder) {
      push_child(children, Some("error"), binder)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  match state.peek() {
    Some((found, start, end)) => {
      let expected = if skip_binder {
        [TokenKind::TK_EQUAL, TK_COMMA, TK_IN]
      } else {
        [TK_LIDENT, TK_EXTEND, TK_SEMI, TK_LBRACE]
      }
      state.report_expected_tokens_at(found, mk_loc(start, end), expected)
    }
    None => ()
  }
  let mut saw_header_separator = false
  let mut reported_missing_statement = false
  while !state.current_is(TK_LBRACE) &&
        !state.current_is(TK_EOF) &&
        !(state.peek() is None) {
    if state.current_is(TK_SEMI) {
      if saw_header_separator && !reported_missing_statement {
        state.report_expected_description("simple expression")
        reported_missing_statement = true
      }
      saw_header_separator = true
    }
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
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    push_child(children, Some("body"), state.missing_expr_node())
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_for(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_foreach_pattern(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut first_pattern : CstNode? = None
  let mut item_count = 0
  let mut saw_sep = false
  push_children(children, state.consume_trivia())
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), [TK_IN, TK_EOF]) => done = true
      Some((COMMA, _, _)) => {
        saw_sep = true
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          done = true
        }
      }
      Some(_) => {
        match state.peek() {
          Some((found, _, _)) if pattern_can_start(found) => ()
          Some((found, start, end)) =>
            state.report_expected_tokens_at(found, mk_loc(start, end), [
              TK_LIDENT,
              TK_EXTEND,
              TK_SEMI,
              TK_LBRACE,
            ])
          None => ()
        }
        let pat = parse_pattern_until(state, [TK_COMMA, TK_IN, TK_EOF])
        if !pat.children.is_empty() {
          if first_pattern is None {
            first_pattern = Some(pat)
          }
          item_count += 1
        }
        push_child(children, Some("item"), pat)
        push_children(children, state.consume_trivia())
        if state.current_is(TK_COMMA) {
          saw_sep = true
          if state.consume_any() is Some(sep) {
            push_child(children, Some("sep"), sep)
          } else {
            ()
          }
        }
      }
      None => done = true
    }
  }
  if item_count == 0 {
    state.report_expected_tokens([
      TK_TRUE,
      TK_FALSE,
      TK_CHAR,
      TK_INT,
      TK_BYTE,
      TK_FLOAT,
      TK_DOUBLE,
      TK_MINUS,
      TK_BYTES,
      TK_STRING,
      TK_UNDERSCORE,
      TK_LIDENT,
      TK_EXTEND,
      TK_PACKAGE_NAME,
      TK_UIDENT,
      TK_AMPER,
      TK_LPAREN,
      TK_LBRACKET,
      TK_LBRACE,
    ])
  }
  if item_count == 1 && !saw_sep {
    if first_pattern is Some(pat) {
      pat
    } else {
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
        },
        [],
      )
    }
  } else {
    state.construct_from_children(
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_tuple_pattern_list(loc~, source_span~, entries~)
      },
      children,
    )
  }
}

///|
fn parse_foreach_iter_expr(state : ParserState) -> CstNode {
  if state.current_is(TK_LBRACE) &&
    !brace_starts_record_expr(state) &&
    !brace_starts_map_expr(state) &&
    !brace_expr_is_followed_by_range_operator(state) {
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
  } else {
    parse_expr_until(state, foreach_iter_expr_stops())
  }
}

///|
fn foreach_iter_expr_stops() -> Array[TokenKind] {
  [TK_SEMI, TK_NOBREAK, TK_ELSE, TK_EOF]
}

///|
fn parse_foreach_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
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
    let expr = parse_foreach_iter_expr(state)
    if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
    }
  }
  if state.current_is(TK_SEMI) {
    consume_for_header_separator(state)
    push_child(children, Some("init"), parse_for_binding_list(state))
    if state.current_is(TK_SEMI) {
      consume_for_header_separator(state)
      push_child(children, Some("continue"), parse_for_binding_list(state))
      if state.current_is(TK_SEMI) || state.current_is(TK_NEWLINE) {
        state.report_expected_tokens([TK_COMMA, TK_LBRACE])
      }
    }
  }
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  }
  if state.current_is(TK_NOBREAK) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    if state.current_is(TK_LBRACE) {
      push_child(children, Some("else"), parse_block_expr(state))
    }
  }
  if state.current_is(TK_WHERE) {
    push_child(children, Some("where"), parse_where_clause(state))
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_for_each(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn for_clause_stops() -> Array[TokenKind] {
  [TK_COMMA, TK_SEMI, TK_NEWLINE, TK_LBRACE, TK_EOF]
}

///|
fn for_binding_expr_stops(state : ParserState) -> Array[TokenKind] {
  let mut start_offset = 0
  while state.peek(offset=start_offset) is Some((COMMENT(_) | NEWLINE, _, _)) {
    start_offset += 1
  }
  if state.peek(offset=start_offset) is Some((LBRACE, _, _)) &&
    (
      brace_starts_record_expr(state, start_offset~) ||
      brace_starts_map_expr(state, start_offset~)
    ) {
    [TK_COMMA, TK_SEMI, TK_NEWLINE, TK_EOF]
  } else {
    for_clause_stops()
  }
}

///|
fn for_header_stops() -> Array[TokenKind] {
  [TK_SEMI, TK_NEWLINE, TK_LBRACE, TK_RBRACE, TK_EOF]
}

///|
fn invalid_for_binding_series_has_underscore(state : ParserState) -> Bool {
  let mut offset = 0
  let mut depth = 0
  while true {
    match state.peek(offset~) {
      Some((UNDERSCORE, _, _)) => return true
      Some((LBRACE, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RBRACE, _, _)) if depth == 0 => return false
      Some((RBRACE, _, _)) => {
        depth -= 1
        offset += 1
      }
      Some((EOF, _, _)) | None => return false
      Some(_) => offset += 1
    }
  }
  false
}

///|
fn parse_for_binding(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut invalid_name = false
  let mut recover_invalid_statement_series = false
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    Some((UIDENT(name), start, end)) => {
      state.report(
        mk_loc(start, end),
        "Unexpected uppercase identifier \{name}, expected lowercase identifier in loop variable name of `for`.",
      )
      if state.consume_any() is Some(name_node) {
        push_child(children, Some("name"), name_node)
      } else {
        ()
      }
    }
    Some((found, start, end)) => {
      invalid_name = true
      recover_invalid_statement_series = found is LET &&
        invalid_for_binding_series_has_underscore(state)
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TK_LIDENT,
        TK_EXTEND,
      ])
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TK_LIDENT),
      )
    }
    None => invalid_name = true
  }
  if invalid_name {
    if recover_invalid_statement_series {
      let mut depth = 0
      let mut done = false
      while !done {
        match state.peek() {
          Some((RBRACE, _, _)) if depth == 0 => done = true
          Some((EOF, _, _)) | None => done = true
          Some((UNDERSCORE, start, end)) => {
            state.report_expected_tokens_at(UNDERSCORE, mk_loc(start, end), [
              TK_LIDENT,
              TK_EXTEND,
            ])
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            }
          }
          Some((LBRACE, _, _)) => {
            depth += 1
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            }
          }
          Some((RBRACE, _, _)) => {
            depth -= 1
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            }
          }
          Some(_) =>
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            }
        }
      }
      return state.construct_from_children(
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
          let error = @construction.children_with_label_from(children, "error")
          let op = @construction.first_child_with_label_from(children, "op")
          let expr = @construction.first_child_with_label_from(children, "expr")
          let semantic_name = @construction.first_child_with_label_from(
            children, "semantic_name",
          )
          @node.CstNode::binding_for(
            loc~,
            source_span~,
            name~,
            error~,
            op?,
            expr?,
            semantic_name?,
          )
        },
        children,
      )
    }
    let scan = state.recover_until([TK_COMMA, TK_LBRACE])
    push_recovered_errors(children, scan.skipped)
    match state.peek() {
      Some((EOF, start, end)) =>
        state.report(mk_loc(start, end), "Unexpected token here.")
      _ if scan.owner is RecoveryScopeOwner(_) => {
        // Keep the diagnostic that the former unbounded scan emitted at EOF.
        // The enclosing block still owns its closing token.
        let mut offset = 0
        let mut done = false
        while !done {
          match state.peek(offset~) {
            Some((COMMA | LBRACE, _, _)) | None => done = true
            Some((EOF, start, end)) => {
              state.report(mk_loc(start, end), "Unexpected token here.")
              done = true
            }
            Some(_) => offset += 1
          }
        }
      }
      _ => ()
    }
    return state.construct_from_children(
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
        let error = @construction.children_with_label_from(children, "error")
        let op = @construction.first_child_with_label_from(children, "op")
        let expr = @construction.first_child_with_label_from(children, "expr")
        let semantic_name = @construction.first_child_with_label_from(
          children, "semantic_name",
        )
        @node.CstNode::binding_for(
          loc~,
          source_span~,
          name~,
          error~,
          op?,
          expr?,
          semantic_name?,
        )
      },
      children,
    )
  }
  if state.current_is(TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
  } else {
    state.report_expected_tokens([TK_EQUAL])
    let scan = state.recover_until(for_clause_stops())
    push_recovered_errors(children, scan.skipped)
    return state.construct_from_children(
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
        let error = @construction.children_with_label_from(children, "error")
        let op = @construction.first_child_with_label_from(children, "op")
        let expr = @construction.first_child_with_label_from(children, "expr")
        let semantic_name = @construction.first_child_with_label_from(
          children, "semantic_name",
        )
        @node.CstNode::binding_for(
          loc~,
          source_span~,
          name~,
          error~,
          op?,
          expr?,
          semantic_name?,
        )
      },
      children,
    )
  }
  let expr = parse_expr_until(state, for_binding_expr_stops(state))
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
      let error = @construction.children_with_label_from(children, "error")
      let op = @construction.first_child_with_label_from(children, "op")
      let expr = @construction.first_child_with_label_from(children, "expr")
      let semantic_name = @construction.first_child_with_label_from(
        children, "semantic_name",
      )
      @node.CstNode::binding_for(
        loc~,
        source_span~,
        name~,
        error~,
        op?,
        expr?,
        semantic_name?,
      )
    },
    children,
  )
}

///|
fn parse_for_binding_list(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_children(children, state.consume_trivia())
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), for_header_stops()) =>
        done = true
      Some((COMMA, _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          done = true
        }
      Some(_) => {
        push_child(children, Some("item"), parse_for_binding(state))
        push_children(children, state.consume_trivia())
        if state.current_is(TK_COMMA) {
          if state.consume_any() is Some(sep) {
            push_child(children, Some("sep"), sep)
          } else {
            ()
          }
        } else if !state.current_in(for_header_stops()) &&
          !(state.peek() is None) {
          state.report_expected_tokens([TK_COMMA, TK_SEMI, TK_LBRACE])
          let scan = state.recover_until([TK_COMMA, ..for_header_stops()])
          push_recovered_errors(children, scan.skipped)
          if state.current_is(TK_COMMA) {
            if state.consume_any() is Some(sep) {
              push_child(children, Some("sep"), sep)
            } else {
              done = true
            }
          } else {
            done = true
          }
        }
      }
      None => done = true
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_for_binding_list(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn consume_for_header_separator(state : ParserState) -> Unit {
  match state.peek() {
    Some((SEMI(_), _, _)) => {
      let _ = state.consume_any()
      let _ = state.consume_trivia()
    }
    Some((NEWLINE, _, _)) => {
      let _ = state.consume_trivia()
    }
    _ => ()
  }
}

///|
fn token_can_start_for_condition(tok : Token) -> Bool {
  match tok {
    TRUE
    | FALSE
    | INT(_)
    | FLOAT(_)
    | DOUBLE(_)
    | CHAR(_)
    | STRING(_)
    | REGEX_LITERAL(_)
    | MULTILINE_STRING(_)
    | MULTILINE_INTERP(_)
    | BYTE(_)
    | BYTES_INTERP(_)
    | INTERP(_)
    | PACKAGE_NAME(_)
    | ASYNC
    | FN
    | LIDENT(_)
    | EXTEND
    | UIDENT(_)
    | LPAREN
    | LBRACKET
    | LBRACKET_BAR
    | LBRACE
    | UNDERSCORE
    | EXCLAMATION
    | AMPER
    | IF
    | MATCH
    | PLUS
    | MINUS
    | LOOP
    | WHILE
    | FOR
    | POST_LABEL(_)
    | GUARD
    | GUARD_EXCLAMATION
    | TRY
    | DEFER
    | ERRDEFER
    | TRY_EXCLAMATION
    | FORALL
    | EXISTS
    | LEXSCAN => true
    _ => false
  }
}

///|
fn parse_for_condition(state : ParserState) -> CstNode {
  let first = state.peek().map(item => item.0)
  let cond = parse_expr_infix(state, [TK_SEMI, TK_NEWLINE, TK_EOF])
  match first {
    Some(tok) if token_disallowed_in_condition(tok) =>
      if unexpected_expr_or_statement_name(tok) is Some(actual) {
        state.report(cond.loc, "Unexpected \{actual} in condition")
      } else {
        ()
      }
    _ => ()
  }
  cond
}

///|
fn recover_for_missing_separator(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> Unit {
  let scan = state.recover_until([TK_LBRACE, TK_RBRACE])
  push_recovered_errors(children, scan.skipped)
}

///|
fn parse_c_style_for_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  if state.current_is(TK_LBRACE) {
    match state.peek() {
      Some((_, start, end)) =>
        state.report(
          mk_loc(start, end),
          "Deprecated `for { ... }` syntax is not supported; use `for ;; { ... }`.",
        )
      None => ()
    }
  }
  let reports_before_init = state.reports.length()
  push_child(children, Some("init"), parse_for_binding_list(state))
  let init_had_error = state.reports.length() > reports_before_init
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    let had_first_separator = state.current_is(TK_SEMI) ||
      state.current_is(TK_NEWLINE)
    consume_for_header_separator(state)
    let has_condition = had_first_separator &&
      (match state.peek() {
        Some((tok, _, _)) => token_can_start_for_condition(tok)
        None => false
      })
    if has_condition {
      let cond = parse_for_condition(state)
      if !cond.children.is_empty() {
        push_child(children, Some("cond"), cond)
      }
    }
    if init_had_error && state.current_is(TK_SEMI) {
      state.report_expected_description("simple expression")
    }
    if has_condition && state.current_is(TK_LBRACE) {
      push_child(children, Some("body"), parse_block_expr(state))
    } else if state.current_is(TK_SEMI) || state.current_is(TK_NEWLINE) {
      consume_for_header_separator(state)
      push_child(children, Some("continue"), parse_for_binding_list(state))
      if state.current_is(TK_SEMI) {
        state.report_expected_tokens([TK_COMMA, TK_LBRACE])
      }
      if state.current_is(TK_LBRACE) {
        push_child(children, Some("body"), parse_block_expr(state))
      }
    } else if had_first_separator {
      state.report_expected_tokens([TK_SEMI])
      recover_for_missing_separator(state, children)
      if state.current_is(TK_LBRACE) {
        push_child(children, Some("body"), parse_block_expr(state))
      }
    }
  }
  if state.current_is(TK_NOBREAK) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    if state.current_is(TK_LBRACE) {
      push_child(children, Some("else"), parse_block_expr(state))
    }
  }
  if state.current_is(TK_WHERE) {
    push_child(children, Some("where"), parse_where_clause(state))
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_for(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_where_clause(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  if state.current_is(TK_LBRACE) {
    push_child(
      children,
      Some("body"),
      parse_delimited(
        state,
        TK_LBRACE,
        TK_RBRACE,
        parse_where_field_item,
        fn(loc, source_span, children) {
          let entries = list_children(children)
          @node.CstNode::list_where_field_list(loc~, source_span~, entries~)
        },
        sep_kind=TokenKind::TK_COMMA,
      ),
    )
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::clause_where(loc~, source_span~, kw~, body~)
    },
    children,
  )
}

///|
fn parse_where_field_item(state : ParserState) -> CstNode {
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
  }
  let expr = parse_expr_until(state, [TK_COMMA, TK_RBRACE, TK_EOF])
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
          @node.CstNode::name_field(loc~, source_span~, long_ident~)
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
      @node.CstNode::field_where(
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
