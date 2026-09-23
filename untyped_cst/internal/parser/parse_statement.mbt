///|
fn parse_statement_item(state : ParserState) -> CstNode {
  let reports_before_statement = state.reports.length()
  state.required_expr_node(parse_statement(state), reports_before_statement)
}

///|
fn parse_let_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut expr_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let entries = recovery_children(children)
    @node.CstNode::expr_let(loc~, source_span~, entries~)
  }
  match state.peek() {
    Some((LET, _, _)) =>
      if state.consume_any() is Some(let_token) {
        push_child(children, Some("kw"), let_token)
      } else {
        ()
      }
    _ => ()
  }
  match state.peek() {
    Some((MUTABLE, _, _)) =>
      if state.consume_any() is Some(mut_token) {
        push_child(children, Some("kw"), mut_token)
        expr_builder = fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_let_mut(loc~, source_span~, entries~)
        }
      } else {
        ()
      }
    _ => ()
  }
  let reports_before_pattern = state.reports.length()
  let pat = parse_pattern_until(state, [
    TK_COLON,
    TK_EQUAL,
    TK_SEMI,
    TK_RBRACE,
    TK_EOF,
  ])
  push_child(
    children,
    Some("pattern"),
    state.required_pattern_node(pat, reports_before_pattern),
  )
  if state.current_is(TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let reports_before_type = state.reports.length()
    let ty = parse_type_arrow(state, [TK_EQUAL, TK_SEMI, TK_RBRACE, TK_EOF])
    push_child(
      children,
      Some("type"),
      state.required_type_node(ty, reports_before_type),
    )
  }
  let reports_before_value = state.reports.length()
  let has_equal = state.current_is(TK_EQUAL)
  push_child(children, Some("op"), state.expect_token(TK_EQUAL))
  if !has_equal {
    while !state.current_is(TK_EQUAL) &&
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
    if state.current_is(TK_EQUAL) {
      if state.consume_any() is Some(equal) {
        push_child(children, Some("op"), equal)
      } else {
        ()
      }
    }
  }
  let expr = parse_expr_until(state, [TK_SEMI, TK_RBRACE, TK_EOF])
  push_child(
    children,
    Some("expr"),
    state.required_expr_node(expr, reports_before_value),
  )
  state.construct_from_children(expr_builder, children)
}

///|
fn local_function_statement_can_start(state : ParserState) -> Bool {
  match state.peek_significant() {
    Some((FN, _, _)) =>
      match state.peek_nth_significant(1) {
        Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) => true
        _ => false
      }
    Some((ASYNC, _, _)) =>
      match (state.peek_nth_significant(1), state.peek_nth_significant(2)) {
        (Some((FN, _, _)), Some((LIDENT(_), _, _)))
        | (Some((FN, _, _)), Some((EXTEND, _, _))) => true
        _ => false
      }
    _ => false
  }
}

///|
fn parse_statement(state : ParserState) -> CstNode {
  match state.peek() {
    Some((LET, _, _)) => parse_let_statement(state)
    Some((LETREC, _, _)) => parse_letrec_statement(state)
    Some((FN, _, _)) | Some((ASYNC, _, _)) if local_function_statement_can_start(
        state,
      ) => parse_local_function_statement(state, [TK_SEMI, TK_RBRACE, TK_EOF])
    Some((GUARD, _, _)) | Some((GUARD_EXCLAMATION, _, _)) =>
      parse_guard_statement(state)
    Some((DEFER, _, _)) | Some((ERRDEFER, _, _)) => parse_defer_statement(state)
    Some((PROOF_ASSERT, _, _)) => parse_proof_assert_statement(state)
    Some((PROOF_LET, _, _)) => parse_proof_let_statement(state)
    Some((TYPE | STRUCT | ENUM | TRAIT | SUBERROR | EXTENUM, _, _)) => {
      state.abort_current_decl = true
      parse_invalid_local_decl(state)
    }
    _ => parse_expr_until(state, [TK_SEMI, TK_RBRACE, TK_EOF])
  }
}

///|
fn parse_invalid_local_decl(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((found, start, end)) => {
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "simple expression",
      )
      if found is STRUCT {
        match state.peek_nth_significant(1) {
          Some((SEMI(explicit), next_start, next_end)) =>
            state.report_expected_tokens_at(
              SEMI(explicit),
              mk_loc(next_start, next_end),
              [TK_LIDENT],
            )
          _ => ()
        }
      }
    }
    None => ()
  }
  let mut depth = 0
  let mut completed_group = false
  let mut done = false
  while !done {
    if depth == 0 && completed_group {
      match state.peek_significant() {
        Some((LBRACE | LPAREN | LBRACKET | DERIVE, _, _)) => ()
        _ => {
          done = true
          continue
        }
      }
    }
    match state.peek() {
      Some((SEMI(_), _, _)) if depth == 0 && !completed_group => done = true
      Some((RBRACE, _, _)) if depth == 0 => done = true
      Some((EOF, _, _)) | None => done = true
      Some((tok, _, _)) =>
        if state.consume_any() is Some(bad) {
          let old_depth = depth
          depth = next_depth(depth, tok)
          if old_depth > 0 && depth == 0 {
            completed_group = true
          }
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          done = true
        }
    }
  }
  let mut trailing_offset = 0
  let mut trailing_only_closers = true
  let mut trailing_eof : Location? = None
  while trailing_only_closers && trailing_eof is None {
    match state.peek(offset=trailing_offset) {
      Some((COMMENT(_) | NEWLINE | SEMI(_) | RBRACE, _, _)) =>
        trailing_offset += 1
      Some((EOF, start, end)) => trailing_eof = Some(mk_loc(start, end))
      _ => trailing_only_closers = false
    }
  }
  if trailing_eof is Some(loc) {
    state.report(loc, "Unexpected token here.")
  } else {
    ()
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
fn parse_guard_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let exclamation = state.current_is(TK_GUARD_EXCLAMATION)
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let reports_before_cond = state.reports.length()
  let cond = parse_expr_with_context_restriction(
    state,
    [TK_ELSE, TK_SEMI, TK_RBRACE, TK_EOF],
    "condition",
    token_disallowed_in_condition,
  )
  push_child(
    children,
    Some("cond"),
    state.required_expr_node(cond, reports_before_cond),
  )
  if state.current_is(TK_ELSE) {
    if state.consume_any() is Some(else_kw) {
      push_child(children, Some("kw"), else_kw)
    } else {
      ()
    }
    if state.current_is(TK_LBRACE) {
      push_child(children, Some("else"), parse_block_expr(state))
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_bool_marker(
        children,
        loc,
        source_span,
        "exclamation",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_exclamation(value~, loc~, source_span~)
        },
        exclamation,
      )

      let entries = recovery_children(children)
      @node.CstNode::expr_guard(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_defer_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if state.current_is(
      TokenKind::TK_ERRDEFER,
    ) {
    fn(loc, source_span, children) {
      let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::expr_err_defer(loc~, source_span~, kw~, expr~)
    }
  } else {
    fn(loc, source_span, children) {
      let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::expr_defer(loc~, source_span~, kw~, expr~)
    }
  }
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let reports_before_expr = state.reports.length()
  let expr = parse_expr_until(state, [TK_SEMI, TK_RBRACE, TK_EOF])
  push_child(
    children,
    Some("expr"),
    state.required_expr_node(expr, reports_before_expr),
  )
  state.construct_from_children(builder, children)
}

///|
fn parse_proof_assert_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let expr = parse_expr_until(state, [TK_SEMI, TK_RBRACE, TK_EOF])
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      @node.CstNode::expr_proof_assert(loc~, source_span~, kw~, expr~)
    },
    children,
  )
}

///|
fn parse_proof_let_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
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
  if state.current_is(TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
    let expr = parse_expr_until(state, [TK_SEMI, TK_RBRACE, TK_EOF])
    if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
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
          @node.CstNode::name_binder(loc~, source_span~, long_ident~)
        },
        ["name"],
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
      let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let semantic_name = @construction.first_child_with_label_from(
        children, "semantic_name",
      )
      @node.CstNode::expr_proof_let(
        loc~,
        source_span~,
        kw~,
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
fn parse_letrec_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  push_child(children, Some("binding"), parse_letrec_binding(state))
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((AND, _, _)) => {
        if state.consume_any() is Some(and_kw) {
          push_child(children, Some("kw"), and_kw)
        } else {
          ()
        }
        push_child(children, Some("binding"), parse_letrec_binding(state))
      }
      _ => done = true
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_let_and(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_letrec_binding(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
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
    let ty = parse_type_arrow(state, [TK_EQUAL, TK_SEMI, TK_RBRACE, TK_EOF])
    if !ty.children.is_empty() {
      push_child(children, Some("binding_type"), ty)
    }
  }
  let has_equal = state.current_is(TK_EQUAL)
  if has_equal {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
  } else {
    push_child(
      children,
      Some("op"),
      state.missing_token_node_with_expected(TK_EQUAL, [TK_EQUAL]),
    )
    while !state.current_is(TK_AND) &&
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
          false,
        )

        let entries = recovery_children(children)
        @node.CstNode::binding_let_rec(loc~, source_span~, entries~)
      },
      children,
    )
  }
  let mut saw_fn = false
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
      saw_fn = true
    } else {
      ()
    }
  }
  if saw_fn {
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
    }
  } else if !state.current_in([TK_AND, TK_SEMI, TK_RBRACE, TK_EOF]) {
    let expr = parse_expr_until(state, [TK_AND, TK_SEMI, TK_RBRACE, TK_EOF])
    if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
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
          @node.CstNode::name_binder(loc~, source_span~, long_ident~)
        },
        ["name"],
      )
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
        children
        .iter()
        .any(fn(entry) {
          entry.0 is Some("kw") &&
          entry.1.has_child_kind(fn(kind) { kind is Flag_Async })
        }),
      )

      let entries = recovery_children(children)
      @node.CstNode::binding_let_rec(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_local_function_statement(
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
    push_children(children, state.consume_trivia())
  }
  if state.current_is(TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  match state.peek() {
    Some((tok, start, end)) if !(tok is LPAREN) => {
      state.report(
        mk_loc(start, end),
        "Failed to parse func: expected '(' or '{', got \{@debug.render(Debug::to_repr(tok))}",
      )
      while !state.current_in(stop_kinds) &&
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
            children
            .iter()
            .any(fn(entry) {
              entry.0 is Some("kw") &&
              entry.1.has_child_kind(fn(kind) { kind is Flag_Async })
            }),
          )

          let entries = recovery_children(children)
          @node.CstNode::expr_let_fn(loc~, source_span~, entries~)
        },
        children,
      )
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
        children
        .iter()
        .any(fn(entry) {
          entry.0 is Some("kw") &&
          entry.1.has_child_kind(fn(kind) { kind is Flag_Async })
        }),
      )

      let entries = recovery_children(children)
      @node.CstNode::expr_let_fn(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_argument_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_argument_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_argument_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
    follow_set=[TokenKind::TK_SEMI, TokenKind::TK_RBRACE],
    post_item_expected=[TokenKind::TK_COMMA, TokenKind::TK_RPAREN],
    reports_else_recovery=true,
  )
}

///|
fn parse_statement_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LBRACE,
    TK_RBRACE,
    parse_statement_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_block_statement_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_SEMI,
    item_expected="simple expression",
    is_block_statement_list=true,
  )
}

///|
fn parse_block_expr(
  state : ParserState,
  classify_record_pun? : Bool = false,
) -> CstNode {
  match state.peek_nth_significant(1) {
    Some((EOF, start, end)) =>
      state.report_expected_description_at(
        EOF,
        mk_loc(start, end),
        "simple expression",
      )
    _ => ()
  }
  let list = parse_statement_list(state)
  let items = list.children_with_label("item")
  if classify_record_pun && items.length() == 1 && items[0].kind is Expr_Ident {
    if items[0].first_child_with_label("name") is Some(name) {
      let field = state.construct_from_children(
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
            "pun",
            fn(loc, source_span, children) {
              let value = match @construction.payload_from_children(children) {
                Some(NodePayload::Bool(value)) => value
                _ => false
              }
              @node.CstNode::flag_pun(value~, loc~, source_span~)
            },
            true,
          )
          push_pun_expr(children)

          let entries = recovery_children(children)
          @node.CstNode::field_definition(loc~, source_span~, entries~)
        },
        [(Some("name"), name)],
      )
      let record_children : Array[(String?, CstNode)] = []
      for entry in list.children {
        let (label, child) = entry
        if label == Some("item") {
          push_child(record_children, label, field)
        } else {
          push_child(record_children, label, child)
        }
      }
      let record_body = node(
        fn(loc, source_span, children) {
          push_marker_child(
            children,
            loc,
            source_span,
            "trailing",
            list_trailing_mark_kind(list.children[:]),
          )

          let entries = list_children(children)
          @node.CstNode::list_record_field_list(loc~, source_span~, entries~)
        },
        list.loc,
        list.source_span,
        record_children,
      )
      return state.construct_from_children(
        fn(loc, source_span, children) {
          let type_ = @construction.first_child_with_label_from(
            children, "type",
          )
          let op = @construction.first_child_with_label_from(children, "op")
          let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          @node.CstNode::expr_record(loc~, source_span~, type_?, op?, body~)
        },
        [(Some("body"), record_body)],
      )
    }
  }
  let body = if items.length() >= 2 &&
    !(items[0].kind
    is (Expr_Let
    | Expr_LetMut
    | Expr_LetFn
    | Expr_LetAnd
    | Expr_Guard
    | Expr_Defer
    | Expr_ErrDefer)) {
    state.construct_from_children(
      fn(loc, source_span, children) {
        let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::expr_sequence(loc~, source_span~, body~)
      },
      [(Some("body"), list)],
    )
  } else {
    list
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_marker_child(children, loc, source_span, "group", fn(
        loc,
        source_span,
        children,
      ) {
        let _ = children
        @node.CstNode::group_brace(loc~, source_span~)
      })

      let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let group = @construction.first_child_with_label_from(children, "group")
      @node.CstNode::expr_block(loc~, source_span~, body~, group?)
    },
    [(Some("body"), body)],
  )
}
