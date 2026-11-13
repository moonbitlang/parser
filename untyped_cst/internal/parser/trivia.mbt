///|
// Commit trivia only after lookahead has selected this grammar branch.
// Both explicit and automatically inserted semicolons remain significant.
fn ParserState::consume_trivia_before(
  self : ParserState,
  children : Array[(String?, CstNode)],
  kinds : Array[TokenKind],
) -> Bool {
  match self.peek_significant() {
    Some((token, _, _)) if kind_in(token.kind(), kinds) => {
      push_children(children, self.consume_trivia())
      true
    }
    _ => false
  }
}

///|
// Leave trivia at branch and closing boundaries for the enclosing parser.
fn discard_expr_suffix_trivia(state : ParserState) -> Unit {
  match state.peek_significant() {
    Some(
      (
        CATCH
        | ELSE
        | NORAISE
        | SEMI(_)
        | RPAREN
        | RBRACKET
        | BAR_RBRACKET
        | RBRACE
        | EOF,
        _,
        _,
      ),
    ) => ()
    _ => discard_expr_trivia(state)
  }
}
