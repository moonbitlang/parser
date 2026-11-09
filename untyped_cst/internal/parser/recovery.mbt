///|
fn report_from_lex_error(
  start : Position,
  end : Position,
  err : @lexer.LexicalError,
) -> Report {
  { loc: mk_loc(start, end), msg: Show::to_string(err), }
}

///|
fn reports_from_lex_errors(
  errors : Array[(Position, Position, @lexer.LexicalError)],
) -> Array[Report] {
  let reports : Array[Report] = []
  for error in errors {
    let (start, end, err) = error
    match err {
      Reserved_keyword(_) => ()
      _ => reports.push(report_from_lex_error(start, end, err))
    }
  }
  reports
}

///|
fn ParserState::expect_token(self : ParserState, kind : TokenKind) -> CstNode {
  match self.peek() {
    Some((tok, _, _)) if tok.kind() == kind =>
      if self.consume_any() is Some(node) {
        node
      } else {
        self.missing_token_node(kind)
      }
    _ => self.missing_token_node(kind)
  }
}

///|
fn parse_required_binder(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> Unit {
  push_children(children, state.consume_trivia())
  let name = match state.peek() {
    Some((LIDENT(_), _, _)) => state.expect_token(TK_LIDENT)
    Some((EXTEND, _, _)) => state.expect_token(TK_EXTEND)
    Some((UIDENT(name), start, end)) => {
      state.report(
        mk_loc(start, end),
        "Unexpected uppercase identifier \{name}, expected lowercase identifier.",
      )
      state.expect_token(TK_UIDENT)
    }
    _ =>
      state.missing_token_node_with_expected(TK_LIDENT, [TK_LIDENT, TK_EXTEND])
  }
  push_child(children, Some("name"), name)
}

///|
fn push_recovered_errors(
  children : Array[(String?, CstNode)],
  skipped : Array[CstNode],
  wrap~ : Bool = true,
) -> Unit {
  for token in skipped {
    let error = if wrap {
      error_node(token.loc, token.source_span, [(Some("item"), token)])
    } else {
      token
    }
    push_child(children, Some("error"), error)
  }
}
