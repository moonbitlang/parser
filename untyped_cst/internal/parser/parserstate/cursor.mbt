///|
pub fn kind_in(kind : TokenKind, kinds : Array[TokenKind]) -> Bool {
  for candidate in kinds {
    if kind == candidate {
      return true
    }
  }
  false
}

///|
pub fn ParserState::current_is(self : ParserState, kind : TokenKind) -> Bool {
  match self.peek() {
    Some((tok, _, _)) => tok.kind() == kind
    None => false
  }
}

///|
pub fn ParserState::current_in(
  self : ParserState,
  kinds : Array[TokenKind],
) -> Bool {
  match self.peek() {
    Some((tok, _, _)) => kind_in(tok.kind(), kinds)
    None => false
  }
}
