///|
fn token_label_from_token(token : Token) -> String? {
  if token is SEMI(_) {
    Some("sep")
  } else {
    expr_token_kind_label(token.kind())
  }
}

///|
fn expr_token_kind_label(kind : TokenKind) -> String? {
  match kind {
    TK_TRY_QUESTION
    | TK_TRY_EXCLAMATION
    | TK_NORAISE
    | TK_NOCANCEL
    | TK_IS
    | TK_IN
    | TK_FOR
    | TK_DEFER
    | TK_ERRDEFER
    | TK_GUARD
    | TK_GUARD_EXCLAMATION
    | TK_RETURN
    | TK_WHILE
    | TK_THIN_ARROW
    | TK_FAT_ARROW
    | TK_PROOF_ASSERT
    | TK_PROOF_LET
    | TK_IF
    | TK_ELSE
    | TK_PIPE
    | TK_PIPE_LEFT
    | TK_LT_PLUS
    | TK_LT_QUESTION
    | TK_AS
    | TK_BARBAR
    | TK_CARET
    | TK_AMPER
    | TK_AMPERAMPER
    | TK_BAR
    | TK_PLUS
    | TK_EXCLAMATION
    | TK_QUESTION
    | TK_MINUS
    | TK_EQUAL
    | TK_EQ_TILDE
    | TK_CATCH
    | TK_TRY
    | TK_RAISE
    | TK_WITH
    | TK_CONTINUE
    | TK_BREAK
    | TK_AUGMENTED_ASSIGNMENT
    | TK_PLUS_EQUAL
    | TK_INFIX4
    | TK_INFIX3
    | TK_STAR
    | TK_INFIX2
    | TK_INFIX1
    | TK_LEXMATCH
    | TK_LEXSCAN
    | TK_RANGE_EXCLUSIVE
    | TK_RANGE_INCLUSIVE
    | TK_RANGE_EXCLUSIVE_REV
    | TK_RANGE_INCLUSIVE_REV
    | TK_RANGE_LT_INCLUSIVE
    | TK_WHERE
    | TK_NOBREAK
    | TK_FORALL
    | TK_EXISTS
    | TK_IMPLIES => Some("op")
    _ => Some("token")
  }
}

///|
fn parse_argument_item(state : ParserState) -> CstNode {
  let item_stops = [TokenKind::TK_COMMA, TK_RPAREN, TK_EOF]
  if state.current_argument_starts_optional_forwarding() {
    parse_optional_forwarding_argument(state, complete_pun=true)
  } else if state.current_argument_starts_labelled() {
    parse_labelled_argument(state, complete_pun=true)
  } else {
    match state.peek_significant() {
      Some((tok, _, _)) if statement_only_expression_token(tok) =>
        parse_invalid_simple_expr(state, item_stops)
      _ => {
        let reports_before_expr = state.reports.length()
        let expr = state.required_expr_node(
          parse_expr_until(
            state,
            item_stops,
            allow_argument_label_question=true,
          ),
          reports_before_expr,
        )
        state.construct_from_children(
          fn(loc, source_span, children) {
            let expr = @construction.first_child_with_label_from(
              children, "expr",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            @node.CstNode::argument_positional(loc~, source_span~, expr~)
          },
          [(Some("expr"), expr)],
        )
      }
    }
  }
}

///|
fn ParserState::current_argument_starts_labelled(self : ParserState) -> Bool {
  match (self.peek(), self.peek(offset=1)) {
    (Some((LIDENT(_), _, _)), Some((EQUAL, _, _)))
    | (Some((EXTEND, _, _)), Some((EQUAL, _, _)))
    | (Some((UIDENT(_), _, _)), Some((EQUAL, _, _)))
    | (Some((POST_LABEL(_), _, _)), Some((EQUAL, _, _)))
    | (Some((POST_LABEL(_), _, _)), Some((COMMA, _, _)))
    | (Some((POST_LABEL(_), _, _)), Some((RPAREN, _, _)))
    | (Some((POST_LABEL(_), _, _)), Some((EOF, _, _))) => true
    (Some((POST_LABEL(_), _, _)), Some((COLON, _, _))) =>
      !labelled_loop_prefix_starts(self)
    _ => false
  }
}

///|
fn ParserState::current_argument_starts_optional_forwarding(
  self : ParserState,
) -> Bool {
  match (self.peek(), self.peek(offset=1)) {
    (Some((LIDENT(_), _, _)), Some((QUESTION, _, _)))
    | (Some((EXTEND, _, _)), Some((QUESTION, _, _)))
    | (Some((UIDENT(_), _, _)), Some((QUESTION, _, _))) => true
    _ => false
  }
}

///|
fn parse_labelled_argument(
  state : ParserState,
  complete_pun? : Bool = false,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let invalid_tilde_label_start = match (state.peek(), state.peek(offset=1)) {
    (Some((POST_LABEL(_), start, _)), Some((EQUAL, _, _))) => Some(start)
    _ => None
  }
  match state.peek() {
    Some((UIDENT(name), start, end)) =>
      state.report(
        mk_loc(start, end),
        "Unexpected uppercase identifier \{name}, expected lowercase identifier in .",
      )
    _ => ()
  }
  if state.consume_any() is Some(name) {
    push_child(children, Some("name"), name)
  } else {
    ()
  }
  let mut is_pun = false
  let mut argument_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
      missing_at_end(loc~, source_span~),
    )
    let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
      missing_at_end(loc~, source_span~),
    )
    let expr = @construction.first_child_with_label_from(children, "expr")
    let pattern = @construction.first_child_with_label_from(children, "pattern")
    let semantic_name = @construction.first_child_with_label_from(
      children, "semantic_name",
    )
    let pun = @construction.first_child_with_label_from(children, "pun")
    @node.CstNode::argument_labelled(
      loc~,
      source_span~,
      name~,
      op~,
      expr?,
      pattern?,
      semantic_name?,
      pun?,
    )
  }
  match state.peek() {
    Some((COMMA, _, _)) | Some((RPAREN, _, _)) | Some((EOF, _, _)) => {
      is_pun = true
      argument_builder = fn(loc, source_span, children) {
        let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let semantic_name = @construction.first_child_with_label_from(
          children, "semantic_name",
        )
        let pun = @construction.first_child_with_label_from(children, "pun")
        let expr = @construction.first_child_with_label_from(children, "expr")
        let pattern = @construction.first_child_with_label_from(
          children, "pattern",
        )
        @node.CstNode::argument_labelled_pun(
          loc~,
          source_span~,
          name~,
          semantic_name?,
          pun?,
          expr?,
          pattern?,
        )
      }
    }
    _ => {
      if state.consume_any() is Some(equal) {
        push_child(children, Some("op"), equal)
      } else {
        ()
      }
      report_statement_only_expression(state, "labelled call argument")
      let reports_before_expr = state.reports.length()
      let expr = parse_expr_until(state, [TK_COMMA, TK_RPAREN, TK_EOF])
      if invalid_tilde_label_start is Some(start) {
        state.report(
          mk_loc(start, expr.loc.end),
          "Invalid syntax `label~=value`. Do you mean `label=value`?",
        )
      } else {
        ()
      }
      push_child(
        children,
        Some("expr"),
        state.required_expr_node(expr, reports_before_expr),
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
      if is_pun && complete_pun {
        push_pun_expr(children)
      }
      argument_builder(loc, source_span, children)
    },
    children,
  )
}

///|
fn parse_optional_forwarding_argument(
  state : ParserState,
  complete_pun? : Bool = false,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((UIDENT(name), start, end)) =>
      state.report(
        mk_loc(start, end),
        "Unexpected uppercase identifier \{name}, expected lowercase identifier in .",
      )
    _ => ()
  }
  if state.consume_any() is Some(name) {
    push_child(children, Some("name"), name)
  } else {
    ()
  }
  if state.consume_any() is Some(question) {
    push_child(children, Some("op"), question)
  } else {
    ()
  }
  let mut is_pun = true
  let mut argument_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
      missing_at_end(loc~, source_span~),
    )
    let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
      missing_at_end(loc~, source_span~),
    )
    let semantic_name = @construction.first_child_with_label_from(
      children, "semantic_name",
    )
    let pun = @construction.first_child_with_label_from(children, "pun")
    let expr = @construction.first_child_with_label_from(children, "expr")
    @node.CstNode::argument_labelled_option_pun(
      loc~,
      source_span~,
      name~,
      op~,
      semantic_name?,
      pun?,
      expr?,
    )
  }
  if state.current_is(TokenKind::TK_EQUAL) {
    is_pun = false
    argument_builder = fn(loc, source_span, children) {
      let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let op = @construction.children_with_label_from(children, "op")
      let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let semantic_name = @construction.first_child_with_label_from(
        children, "semantic_name",
      )
      let pun = @construction.first_child_with_label_from(children, "pun")
      @node.CstNode::argument_labelled_option(
        loc~,
        source_span~,
        name~,
        op~,
        expr~,
        semantic_name?,
        pun?,
      )
    }
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
    report_statement_only_expression(state, "optional call argument")
    let reports_before_expr = state.reports.length()
    let expr = parse_expr_until(state, [TK_COMMA, TK_RPAREN, TK_EOF])
    push_child(
      children,
      Some("expr"),
      state.required_expr_node(expr, reports_before_expr),
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

      if is_pun && complete_pun {
        push_pun_expr(children)
      }
      argument_builder(loc, source_span, children)
    },
    children,
  )
}
