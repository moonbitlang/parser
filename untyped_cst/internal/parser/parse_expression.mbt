///|
fn statement_only_expression_token(tok : Token) -> Bool {
  match tok {
    RAISE
    | BREAK
    | CONTINUE
    | RETURN
    | LET
    | LETREC
    | GUARD
    | GUARD_EXCLAMATION
    | DEFER
    | ERRDEFER
    | PROOF_ASSERT
    | PROOF_LET
    | ELLIPSIS => true
    _ => false
  }
}

///|
fn report_statement_only_expression(
  state : ParserState,
  context : String,
) -> Unit {
  match state.peek_significant() {
    Some((tok, start, end)) if statement_only_expression_token(tok) =>
      state.report(
        mk_loc(start, end),
        "statement expression is not allowed in \{context}",
      )
    _ => ()
  }
}

///|
fn unexpected_expr_or_statement_name(tok : Token) -> String? {
  match tok {
    IF => Some("if expression")
    MATCH => Some("match expression")
    WHILE => Some("while expression")
    FOR => Some("for expression")
    TRY => Some("try expression")
    TRY_EXCLAMATION => Some("`try!` expression")
    LET | LETREC => Some("let statement")
    GUARD | GUARD_EXCLAMATION => Some("guard statement")
    DEFER | ERRDEFER => Some("defer statement")
    BREAK => Some("break statement")
    CONTINUE => Some("break statement")
    RETURN => Some("return statement")
    _ => None
  }
}

///|
fn token_disallowed_in_condition(tok : Token) -> Bool {
  match tok {
    IF
    | MATCH
    | WHILE
    | FOR
    | TRY
    | TRY_EXCLAMATION
    | LET
    | LETREC
    | GUARD
    | GUARD_EXCLAMATION
    | BREAK
    | RETURN
    | CONTINUE => true
    _ => false
  }
}

///|
fn token_disallowed_in_try_body(tok : Token) -> Bool {
  match tok {
    IF
    | MATCH
    | WHILE
    | FOR
    | TRY
    | LET
    | GUARD
    | GUARD_EXCLAMATION
    | BREAK
    | RETURN
    | CONTINUE => true
    _ => false
  }
}

///|
fn token_disallowed_in_pattern_guard(tok : Token) -> Bool {
  match tok {
    IF
    | MATCH
    | WHILE
    | FOR
    | TRY
    | LET
    | GUARD
    | GUARD_EXCLAMATION
    | BREAK
    | RETURN
    | CONTINUE => true
    _ => false
  }
}

///|
fn token_disallowed_in_match_action(tok : Token) -> Bool {
  match tok {
    LET | LETREC | GUARD | GUARD_EXCLAMATION | DEFER | ERRDEFER => true
    _ => false
  }
}

///|
fn assignment_lhs_is_valid(node : CstNode) -> Bool {
  match node.kind {
    Expr_Ident | Expr_Field | Expr_ArrayGet => true
    _ => false
  }
}

///|
fn assignment_lhs_kind(node : CstNode) -> NodeKind? {
  match node.kind {
    Expr_Ident | Expr_Field | Expr_ArrayGet => Some(node.kind)
    _ => None
  }
}

///|
fn assignment_node_builder(
  lhs : CstNode,
  is_augmented : Bool,
) -> (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode {
  match assignment_lhs_kind(lhs) {
    Some(Expr_ArrayGet) =>
      if is_augmented {
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_array_augmented_set(loc~, source_span~, entries~)
        }
      } else {
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_array_set(loc~, source_span~, entries~)
        }
      }
    Some(Expr_Field) =>
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::expr_mutate_from_entries(loc~, source_span~, entries~)
      }
    _ =>
      fn(loc, source_span, children) {
        let lhs = @construction.first_child_with_label_from(children, "lhs")
        let name = @construction.children_with_label_from(children, "name")
        let semantic_name = @construction.first_child_with_label_from(
          children, "semantic_name",
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
        let augmented = @construction.first_child_with_label_from(
          children, "augmented",
        )
        @node.CstNode::expr_assign(
          loc~,
          source_span~,
          lhs?,
          name~,
          semantic_name?,
          op~,
          operator~,
          rhs~,
          augmented?,
        )
      }
  }
}

///|
fn push_assignment_lhs_children(
  children : Array[(String?, CstNode)],
  lhs : CstNode,
  normalize : Bool,
) -> Unit {
  if normalize {
    for entry in lhs.children {
      children.push(entry)
    }
  } else {
    push_child(children, Some("lhs"), lhs)
  }
}

///|
fn function_node_from_children(
  state : ParserState,
  children : Array[(String?, CstNode)],
  fn_kind_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
) -> CstNode {
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
      push_marker_child(children, loc, source_span, "fn_kind", fn_kind_builder)

      let kw = @construction.children_with_label_from(children, "kw")
      let error = @construction.children_with_label_from(children, "error")
      let params = @construction.first_child_with_label_from(children, "params")
      let type_ = @construction.first_child_with_label_from(children, "type")
      let op = @construction.first_child_with_label_from(children, "op")
      let pipe = @construction.first_child_with_label_from(children, "pipe")
      let body = @construction.first_child_with_label_from(children, "body")
      let async_ = @construction.first_child_with_label_from(children, "async")
      let fn_kind = @construction.first_child_with_label_from(
        children, "fn_kind",
      )
      @node.CstNode::expr_function(
        loc~,
        source_span~,
        kw~,
        error~,
        params?,
        type_?,
        op?,
        pipe?,
        body?,
        async_?,
        fn_kind?,
      )
    },
    children,
  )
}

///|
fn hole_node_from_children(
  state : ParserState,
  children : Array[(String?, CstNode)],
  todo? : Bool = false,
) -> CstNode {
  let hole_kind_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if todo {
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::hole_kind_todo(loc~, source_span~)
    }
  } else {
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::hole_kind_incomplete(loc~, source_span~)
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_marker_child(
        children, loc, source_span, "hole_kind", hole_kind_builder,
      )

      let token = @construction.first_child_with_label_from(children, "token").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let hole_kind = @construction.first_child_with_label_from(
        children, "hole_kind",
      )
      @node.CstNode::expr_hole(loc~, source_span~, token~, hole_kind?)
    },
    children,
  )
}

///|
fn invalid_arrow_parameter_expr(node : CstNode) -> CstNode? {
  match node.kind {
    Expr_Ident | Expr_Hole | Expr_MissingExpr => None
    kind if kind.is_argument() => None
    Expr_Constraint =>
      if node.first_child_with_label("expr") is Some(inner) {
        invalid_arrow_parameter_expr(inner)
      } else {
        None
      }
    _ => Some(node)
  }
}

///|
fn arrow_lhs_can_start_function(node : CstNode) -> Bool {
  match node.kind {
    Expr_Unit | Expr_Group | Expr_Tuple => true
    _ => invalid_arrow_parameter_expr(node) is None
  }
}

///|
fn parse_arrow_fn_body_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  match state.peek_significant() {
    Some((LBRACE, _, _)) =>
      if brace_starts_record_expr(state) {
        parse_record_expr(state)
      } else if brace_starts_map_expr(state) {
        parse_map_expr(state)
      } else {
        parse_block_expr(state)
      }
    Some((CONTINUE, _, _)) if state.peek_nth_significant(1).map(item => item.0)
      is Some(POST_LABEL(_)) &&
      state.peek_nth_significant(2).map(item => item.0) is Some(COLON) =>
      parse_expr_until(state, stop_kinds)
    Some((BREAK | CONTINUE | RETURN, _, _)) =>
      parse_invalid_simple_expr(state, stop_kinds)
    _ => parse_expr_until(state, stop_kinds)
  }
}

///|
fn parse_expr_with_context_restriction(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  context : String,
  disallowed : (Token) -> Bool,
) -> CstNode {
  let first = state.peek_significant().map(triple => triple.0)
  let expr = parse_expr_until(state, stop_kinds)
  match first {
    Some(tok) if disallowed(tok) =>
      if unexpected_expr_or_statement_name(tok) is Some(actual) {
        state.report(expr.loc, "Unexpected \{actual} in \{context}")
      } else {
        ()
      }
    _ => ()
  }
  expr
}

///|
fn parse_pattern_guard_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  match state.peek_significant() {
    Some((LETREC, _, _)) => parse_invalid_guard_letrec(state, stop_kinds)
    Some((TRY_EXCLAMATION, _, _)) =>
      parse_invalid_simple_expr(state, stop_kinds)
    _ =>
      parse_expr_with_context_restriction(
        state, stop_kinds, "pattern guard", token_disallowed_in_pattern_guard,
      )
  }
}

///|
fn parse_invalid_guard_letrec(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut offset = 0
  let mut depth = 0
  let mut last_arrow = -1
  let mut scanning = true
  while scanning {
    match state.peek(offset~) {
      Some((SEMI(_) | RBRACE, _, _)) if depth == 0 => scanning = false
      Some((tok, _, _)) if depth == 0 &&
        kind_in(tok.kind(), stop_kinds) &&
        !(tok is FAT_ARROW) => scanning = false
      Some((FAT_ARROW, _, _)) if depth == 0 => {
        last_arrow = offset
        offset += 1
      }
      Some((EOF, _, _)) | None => scanning = false
      Some((tok, _, _)) => {
        depth = next_depth(depth, tok)
        offset += 1
      }
    }
  }
  if last_arrow <= 0 {
    return parse_invalid_simple_expr(state, stop_kinds)
  }
  match state.peek_significant() {
    Some((found, start, end)) =>
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "simple expression",
      )
    None => ()
  }
  let children : Array[(String?, CstNode)] = []
  for _ in 0..<last_arrow {
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
fn parse_expr_fragment(state : ParserState) -> CstNode {
  let reports_before_expr = state.reports.length()
  let expr = state.required_expr_node(
    parse_expr_fragment_item(state),
    reports_before_expr,
  )
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("expr"), expr)
  push_children(children, state.consume_trivia())
  while state.current_is(TK_SEMI) {
    if state.consume_any() is Some(sep) {
      push_child(children, Some("sep"), sep)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
    if !state.current_is(TK_EOF) && !(state.peek() is None) {
      let next = parse_expr_fragment_item(state)
      if !next.children.is_empty() {
        push_child(children, Some("expr"), next)
      }
      push_children(children, state.consume_trivia())
    }
  }
  if !state.current_is(TK_EOF) && !(state.peek() is None) {
    state.report_expected_tokens([TK_SEMI, TK_EOF])
    let scan = state.recover_until([TK_EOF])
    push_recovered_errors(children, scan.skipped)
  }
  match state.peek() {
    Some((EOF, _, _)) =>
      if state.consume_any() is Some(eof) {
        push_child(children, Some("eof"), eof)
      } else {
        ()
      }
    None => push_child(children, Some("eof"), state.missing_eof_node())
    _ => ()
  }
  let root = node(
    fn(loc, source_span, children) {
      let entries = root_children(children)
      @node.CstNode::expression(loc~, source_span~, entries~)
    },
    state.root_loc(),
    state.root_span(),
    children,
  )
  root
}

///|
fn parse_expr_fragment_item(state : ParserState) -> CstNode {
  if local_function_statement_can_start(state) {
    discard_expr_trivia(state)
    parse_local_function_statement(state, [TK_SEMI, TK_EOF])
  } else {
    parse_expr_until(state, [TK_EOF])
  }
}

///|
fn parse_expr_until(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  allow_argument_label_question? : Bool = false,
) -> CstNode {
  let _ = allow_argument_label_question
  discard_expr_trivia(state)
  match (state.peek(), state.peek(offset=1)) {
    (Some((POST_LABEL(_), _, _)), Some((COLON, _, _)))
    | (Some((POST_LABEL_TRANSPARENT(_), _, _)), Some((COLON, _, _))) =>
      parse_labelled_loop_expr(state, stop_kinds)
    _ => {
      let lhs = parse_expr_pipe(state, stop_kinds)
      parse_expr_catch_suffix(state, stop_kinds, lhs)
    }
  }
}

///|
fn parse_expr_catch_suffix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  lhs : CstNode,
) -> CstNode {
  if kind_in(TK_CATCH, stop_kinds) ||
    lhs.kind is Expr_Try ||
    !state.current_is(TK_CATCH) {
    return lhs
  }
  let children : Array[(String?, CstNode)] = [(Some("body"), lhs)]
  if state.consume_any() is Some(catch_kw) {
    push_child(children, Some("kw"), catch_kw)
  } else {
    ()
  }
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("case"), parse_case_block(state))
  } else {
    push_child(
      children,
      Some("case"),
      state.missing_token_node_with_expected(TK_LBRACE, [TK_LBRACE]),
    )
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
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_bool_marker(
        children,
        loc,
        source_span,
        "has_try",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_has_try(value~, loc~, source_span~)
        },
        false,
      )

      let entries = recovery_children(children)
      @node.CstNode::expr_try(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_expr_pipe(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let reports_before_lhs = state.reports.length()
  let mut lhs = parse_expr_infix(state, stop_kinds)
  let mut done = false
  while !done {
    discard_expr_trivia(state)
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((COLON, _, _)) => done = true
      Some((EQUAL, _, _))
      | Some((PLUS_EQUAL, _, _))
      | Some((AUGMENTED_ASSIGNMENT(_), _, _)) => {
        let normalize_lhs = state.reports.length() == reports_before_lhs &&
          assignment_lhs_is_valid(lhs)
        if state.reports.length() == reports_before_lhs && !normalize_lhs {
          state.report(lhs.loc, "Invalid left value in assignment")
        }
        let is_augmented = !state.current_is(TokenKind::TK_EQUAL)
        let assignment_builder = assignment_node_builder(lhs, is_augmented)
        let children : Array[(String?, CstNode)] = []
        push_assignment_lhs_children(children, lhs, normalize_lhs)
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
          push_direct_operator(children, op)
        } else {
          ()
        }
        let reports_before_rhs = state.reports.length()
        let rhs = parse_expr_until(state, stop_kinds)
        push_child(
          children,
          Some("rhs"),
          state.required_expr_node(rhs, reports_before_rhs),
        )
        lhs = state.construct_from_children(
          fn(loc, source_span, children) {
            push_bool_marker(
              children,
              loc,
              source_span,
              "augmented",
              fn(loc, source_span, children) {
                let value = match
                  @construction.payload_from_children(children) {
                  Some(NodePayload::Bool(value)) => value
                  _ => false
                }
                @node.CstNode::flag_augmented(value~, loc~, source_span~)
              },
              is_augmented,
            )
            assignment_builder(loc, source_span, children)
          },
          children,
        )
      }
      Some((PIPE, _, _)) | Some((PIPE_LEFT, _, _)) => {
        let pipe_kind = if state.current_is(TK_PIPE_LEFT) {
          TokenKind::TK_PIPE_LEFT
        } else {
          TK_PIPE
        }
        let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
        } else {
          ()
        }
        let reports_before_rhs = state.reports.length()
        let rhs = parse_pipe_rhs(state, stop_kinds, pipe_kind)
        push_child(
          children,
          Some("rhs"),
          state.required_expr_node(rhs, reports_before_rhs),
        )
        lhs = state.construct_from_children(
          if pipe_kind is TokenKind::TK_PIPE_LEFT {
            fn(loc, source_span, children) {
              let lhs = @construction.first_child_with_label_from(
                children, "lhs",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
                missing_at_end(loc~, source_span~),
              )
              let rhs = @construction.first_child_with_label_from(
                children, "rhs",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              @node.CstNode::expr_rev_pipe(loc~, source_span~, lhs~, op~, rhs~)
            }
          } else {
            fn(loc, source_span, children) {
              let lhs = @construction.first_child_with_label_from(
                children, "lhs",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
                missing_at_end(loc~, source_span~),
              )
              let rhs = @construction.first_child_with_label_from(
                children, "rhs",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              @node.CstNode::expr_pipe(loc~, source_span~, lhs~, op~, rhs~)
            }
          },
          children,
        )
      }
      Some((FAT_ARROW, _, _)) if arrow_lhs_can_start_function(lhs) => {
        if lhs.kind is Expr_Constraint &&
          lhs.first_child_with_label("open") is None {
          if lhs.first_child_with_label("op") is Some(op) {
            state.report_expected_tokens_at(COLON, op.loc, [TK_SEMI, TK_EOF])
          } else {
            ()
          }
        }
        let children : Array[(String?, CstNode)] = [
          (Some("params"), parse_arrow_params_from_expr(lhs)),
        ]
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
        } else {
          ()
        }
        let body = parse_arrow_fn_body_expr(state, stop_kinds)
        if !body.children.is_empty() {
          push_child(children, Some("body"), body)
        }
        lhs = function_node_from_children(state, children, fn(
          loc,
          source_span,
          children,
        ) {
          let _ = children
          @node.CstNode::fn_kind_arrow(loc~, source_span~)
        })
      }
      _ => done = true
    }
  }
  lhs
}

///|
fn stop_kinds_with_pipe(stop_kinds : Array[TokenKind]) -> Array[TokenKind] {
  let stops : Array[TokenKind] = [TK_PIPE, TK_PIPE_LEFT]
  for kind in stop_kinds {
    stops.push(kind)
  }
  stops
}

///|
fn parse_pipe_rhs(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  pipe_kind : TokenKind,
) -> CstNode {
  let head = parse_expr_infix(state, stop_kinds)
  discard_expr_trivia(state)
  match state.peek() {
    Some((FAT_ARROW, _, _)) => {
      let children : Array[(String?, CstNode)] = [
        (Some("params"), parse_arrow_params_from_expr(head)),
      ]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
        push_child(children, Some("pipe"), op)
      } else {
        ()
      }
      if pipe_kind is TK_PIPE {
        match state.peek_significant() {
          Some((LBRACE, _, _)) => ()
          Some((found, start, end)) =>
            state.report_expected_description_at(
              found,
              mk_loc(start, end),
              "`{ ... }`",
            )
          None => state.report_expected_description("`{ ... }`")
        }
      }
      let reports_before_body = state.reports.length()
      let body = parse_arrow_fn_body_expr(
        state,
        stop_kinds_with_pipe(stop_kinds),
      )
      push_child(
        children,
        Some("body"),
        state.required_expr_node(body, reports_before_body),
      )
      function_node_from_children(state, children, fn(
        loc,
        source_span,
        children,
      ) {
        let _ = children
        @node.CstNode::fn_kind_arrow(loc~, source_span~)
      })
    }
    _ => head
  }
}

///|
fn parse_arrow_params_from_expr(expr : CstNode) -> CstNode {
  match expr.kind {
    Expr_Unit | Expr_Group | Expr_Tuple =>
      if expr.first_child_with_label("body") is Some(body) {
        let children : Array[(String?, CstNode)] = []
        for entry in body.children {
          let (label, child) = entry
          if label is Some("item") {
            push_child(
              children,
              Some("item"),
              parse_arrow_param_from_expr(child),
            )
          } else {
            children.push(entry)
          }
        }
        node(
          fn(loc, source_span, children) {
            let entries = list_children(children)
            @node.CstNode::list_parameter_list(loc~, source_span~, entries~)
          },
          body.loc,
          body.source_span,
          children,
        )
      } else {
        parse_single_arrow_param_list(expr)
      }
    _ => parse_single_arrow_param_list(expr)
  }
}

///|
fn parse_single_arrow_param_list(expr : CstNode) -> CstNode {
  node(
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_parameter_list(loc~, source_span~, entries~)
    },
    expr.loc,
    expr.source_span,
    [(Some("item"), parse_arrow_param_from_expr(expr))],
  )
}

///|
fn arrow_parameter_builder(
  expr : CstNode,
) -> (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode {
  match expr.kind {
    Expr_Hole | Expr_MissingExpr =>
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::parameter_discard_positional(
          loc~,
          source_span~,
          entries~,
        )
      }
    Argument_Labelled | Argument_LabelledPun =>
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::parameter_labelled(loc~, source_span~, entries~)
      }
    Argument_LabelledOption | Argument_LabelledOptionPun =>
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::parameter_question_optional(loc~, source_span~, entries~)
      }
    _ =>
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::parameter_positional(loc~, source_span~, entries~)
      }
  }
}

///|
fn arrow_var_pattern_from_ident(expr : CstNode) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for entry in expr.children {
    if !(entry.0 is Some("semantic_name")) {
      children.push(entry)
    }
  }
  node(
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

      let entries = recovery_children(children)
      @node.CstNode::pattern_var(loc~, source_span~, entries~)
    },
    expr.loc,
    expr.source_span,
    children,
  )
}

///|
fn arrow_any_pattern_from_hole(expr : CstNode) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for entry in expr.children {
    let (label, child) = entry
    if child.kind.is_syntax_token() || child.kind is (Error | Missing) {
      children.push(
        (if label is Some("token") { Some("name") } else { label }, child),
      )
    }
  }
  node(
    fn(loc, source_span, children) {
      let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let lex_scan_case_pattern = @construction.first_child_with_label_from(
        children, "lex_scan_case_pattern",
      )
      @node.CstNode::pattern_any(
        loc~,
        source_span~,
        name~,
        lex_scan_case_pattern?,
      )
    },
    expr.loc,
    expr.source_span,
    children,
  )
}

///|
fn arrow_var_pattern_from_argument(expr : CstNode) -> CstNode? {
  guard expr.first_child_with_label("name") is Some(name) else { return None }
  if expr.kind is (Argument_LabelledPun | Argument_LabelledOptionPun) {
    let children : Array[(String?, CstNode)] = [(Some("name"), name)]
    Some(
      node(
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

          let entries = recovery_children(children)
          @node.CstNode::pattern_var(loc~, source_span~, entries~)
        },
        name.loc,
        name.source_span,
        children,
      ),
    )
  } else {
    semantic_var_pattern_from_name(name)
  }
}

///|
fn arrow_pattern_from_expr(expr : CstNode) -> CstNode? {
  match expr.kind {
    Expr_Ident => Some(arrow_var_pattern_from_ident(expr))
    Expr_Hole => Some(arrow_any_pattern_from_hole(expr))
    Expr_MissingExpr => {
      let children : Array[(String?, CstNode)] = []
      for entry in expr.children {
        children.push(entry)
      }
      Some(
        node(
          fn(loc, source_span, children) {
            let entries = recovery_children(children)
            @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
          },
          expr.loc,
          expr.source_span,
          children,
        ),
      )
    }
    kind if kind.is_argument() => arrow_var_pattern_from_argument(expr)
    _ => None
  }
}

///|
fn append_arrow_argument_syntax(
  children : Array[(String?, CstNode)],
  argument : CstNode,
) -> Unit {
  if argument.kind is (Argument_LabelledOption | Argument_LabelledOptionPun) {
    for entry in argument.children {
      let (label, child) = entry
      if label is Some("op") &&
        @construction.first_operator_descendant(child)
        .map(operator => operator.string_payload().unwrap_or("") == "?")
        .unwrap_or(false) {
        push_child(children, Some("question"), child)
      }
    }
  }
  if argument.kind is (Argument_Labelled | Argument_LabelledOption) {
    push_child(children, Some("error"), argument)
  }
}

///|
fn parse_arrow_param_from_expr(expr : CstNode) -> CstNode {
  let source = if expr.kind is Expr_Constraint {
    expr
    .first_child_with_label("expr")
    .unwrap_or(
      node(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_missing_expr_from_entries(
            loc~,
            source_span~,
            entries~,
          )
        },
        expr.loc,
        expr.source_span,
        [],
      ),
    )
  } else {
    expr
  }
  let parameter_builder = arrow_parameter_builder(source)
  let children : Array[(String?, CstNode)] = []
  match arrow_pattern_from_expr(source) {
    Some(pattern) => push_child(children, Some("pattern"), pattern)
    None => push_child(children, Some("error"), source)
  }
  if source.kind.is_argument() {
    append_arrow_argument_syntax(children, source)
  }
  if expr.kind is Expr_Constraint {
    for entry in expr.children {
      let (label, _) = entry
      if !(label is Some("expr")) {
        children.push(entry)
      } else {
        ()
      }
    }
  }
  node(parameter_builder, expr.loc, expr.source_span, children)
}

///|
fn parse_expr_infix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  state.abort_infix_chain = false
  let expr = parse_expr_binary_prec(state, stop_kinds, 1)
  state.abort_infix_chain = false
  expr
}

///|
fn parse_invalid_infix_operand(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut skipped_line_break = false
  while state.peek() is (Some((SEMI(false), _, _)) | Some((NEWLINE, _, _))) {
    skipped_line_break = true
    if state.consume_any() is Some(line_break) {
      push_child(children, Some("trivia"), line_break)
    }
  }
  if skipped_line_break && state.current_is(TK_LET) {
    state.report_next_top_level_start = true
  }
  push_child(
    children,
    Some("expr"),
    parse_invalid_simple_expr(state, stop_kinds),
  )
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
fn infix_precedence(tok : Token) -> (Int, Bool)? {
  match tok {
    IMPLIES => Some((1, true))
    BARBAR => Some((2, true))
    AMPERAMPER => Some((3, true))
    BAR => Some((4, false))
    CARET => Some((5, false))
    AMPER => Some((6, false))
    INFIX1(_) => Some((7, false))
    INFIX2(_) => Some((8, false))
    PLUS | MINUS => Some((9, false))
    INFIX3(_) => Some((10, false))
    INFIX4(_) => Some((11, false))
    _ => None
  }
}

///|
fn token_invalid_as_infix_operand(tok : Token) -> Bool {
  match tok {
    IF
    | MATCH
    | WHILE
    | FOR
    | TRY
    | TRY_EXCLAMATION
    | LET
    | LETREC
    | GUARD
    | GUARD_EXCLAMATION
    | DEFER
    | BREAK
    | CONTINUE
    | RETURN
    | RAISE
    | LOOP => true
    _ => false
  }
}

///|
fn expr_stops_infix_chain(expr : CstNode) -> Bool {
  match expr.kind {
    Expr_If
    | Expr_Match
    | Expr_Try
    | Expr_TryOperator
    | Expr_For
    | Expr_ForEach
    | Expr_While
    | Expr_LexMatch
    | Expr_LexScan
    | Expr_Let
    | Expr_LetMut
    | Expr_LetAnd
    | Expr_LetFn
    | Expr_Return
    | Expr_Raise
    | Expr_Break
    | Expr_Continue
    | Expr_Defer
    | Expr_ErrDefer
    | Expr_Guard
    | Expr_Quantifier => true
    _ => false
  }
}

///|
fn parse_expr_binary_prec(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  min_prec : Int,
) -> CstNode {
  let mut lhs = parse_expr_postfix(state, stop_kinds)
  let mut done = expr_stops_infix_chain(lhs)
  while !done {
    discard_expr_trivia(state)
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((tok, _, _)) =>
        match infix_precedence(tok) {
          Some((prec, right_assoc)) if prec >= min_prec => {
            let expr_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if tok
              is IMPLIES {
              fn(loc, source_span, children) {
                let lhs = @construction.first_child_with_label_from(
                  children, "lhs",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                let op = @construction.first_child_with_label_from(
                  children, "op",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                let operator = @construction.first_child_with_label_from(
                  children, "operator",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                let rhs = @construction.first_child_with_label_from(
                  children, "rhs",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                @node.CstNode::expr_implies(
                  loc~,
                  source_span~,
                  lhs~,
                  op~,
                  operator~,
                  rhs~,
                )
              }
            } else {
              fn(loc, source_span, children) {
                let lhs = @construction.first_child_with_label_from(
                  children, "lhs",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                let op = @construction.first_child_with_label_from(
                  children, "op",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                let operator = @construction.first_child_with_label_from(
                  children, "operator",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                let rhs = @construction.first_child_with_label_from(
                  children, "rhs",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                @node.CstNode::expr_infix(
                  loc~,
                  source_span~,
                  lhs~,
                  op~,
                  operator~,
                  rhs~,
                )
              }
            }
            let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
            if state.consume_any() is Some(op) {
              push_child(children, Some("op"), op)
              push_direct_operator(children, op)
            } else {
              ()
            }
            let next_min = if right_assoc { prec } else { prec + 1 }
            let reports_before_rhs = state.reports.length()
            let rhs = match state.peek_significant() {
              Some((next, _, _)) if token_invalid_as_infix_operand(next) => {
                state.abort_infix_chain = true
                parse_invalid_infix_operand(state, stop_kinds)
              }
              _ => parse_expr_binary_prec(state, stop_kinds, next_min)
            }
            push_child(
              children,
              Some("rhs"),
              state.required_expr_node(rhs, reports_before_rhs),
            )
            lhs = state.construct_from_children(expr_builder, children)
            if state.abort_infix_chain {
              done = true
            }
          }
          _ => done = true
        }
      None => done = true
    }
  }
  lhs
}
