///|
fn Mapper::cursor_advance(self : Self, pos : Position) -> Unit {
  while self.tokens.get(self.cursor) is Some((tok, start, _)) && !(tok is EOF) {
    log("advance pos: \{pos}, \{tok.kind()}, \{start}")
    if start < pos {
      self.cursor += 1
      continue
    } else {
      break
    }
  }
}

///|
fn Mapper::cursor_find(
  self : Self,
  tok : TokenKind,
  before_pos? : Position,
  before_token? : TokenKind,
) -> Key? {
  match self.cursor_find_aux(tok, before_pos?, before_token?) {
    None => None
    Some(k) as r => {
      self.attach(k)
      r
    }
  }
}

///|
fn Mapper::cursor_find_aux(
  self : Self,
  tok : TokenKind,
  before_pos? : Position,
  before_token? : TokenKind,
) -> Key? {
  if before_pos is Some(pos) {
    while self.tokens.get(self.cursor) is Some(triple) &&
          !(triple.0 is EOF) &&
          triple.1 < pos {
      if triple.0.kind() != tok {
        log("advance tok before: \{triple.0.kind()} \{triple.1} \{triple.2}")
        self.cursor += 1
        continue
      } else {
        log("consume tok: \{triple.0.kind()} \{triple.1} \{triple.2}")
        self.cursor += 1
        return Some(Token({ start: triple.1, end: triple.2 }))
      }
    }
    None
  } else if before_token is Some(before) {
    while self.tokens.get(self.cursor) is Some(triple) &&
          !(triple.0 is EOF) &&
          triple.0.kind() != before {
      if triple.0.kind() != tok {
        log("advance tok before: \{triple.0.kind()} \{triple.1} \{triple.2}")
        self.cursor += 1
        continue
      } else {
        log("consume tok: \{triple.0.kind()} \{triple.1} \{triple.2}")
        self.cursor += 1
        return Some(Token({ start: triple.1, end: triple.2 }))
      }
    }
    None
  } else {
    while self.tokens.get(self.cursor) is Some(triple) && !(triple.0 is EOF) {
      if triple.0.kind() != tok {
        log("advance tok: \{triple.0.kind()} \{triple.1} \{triple.2}")
        self.cursor += 1
        continue
      } else {
        log("consume tok: \{triple.0.kind()} \{triple.1} \{triple.2}")
        self.cursor += 1
        return Some(Token({ start: triple.1, end: triple.2 }))
      }
    }
    None
  }
}
