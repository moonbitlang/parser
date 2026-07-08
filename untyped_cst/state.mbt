///|
priv struct ParserState {
  tokens : Triples
  mut pos : Int
  reports : Array[Report]
  mut abort_current_decl : Bool
  mut abort_infix_chain : Bool
  mut suppress_next_top_level_recovery_report : Bool
  source : String
  cnum_to_source_offset : (Int) -> Int
}

///|
fn default_location_mapper(source : String) -> (Int) -> Int {
  let view = source.view()
  let source_len = view.length()
  let table = Array::make(source_len + 1, source_len)
  let mut source_offset = 0
  let mut surrogate_pairs = 0
  while source_offset <= source_len {
    let cnum = source_offset - surrogate_pairs
    if cnum >= 0 && cnum < table.length() {
      table[cnum] = source_offset
    }
    if source_offset < source_len &&
      view[source_offset].is_leading_surrogate() &&
      source_offset + 1 < source_len &&
      view[source_offset + 1].is_trailing_surrogate() {
      source_offset += 2
      surrogate_pairs += 1
    } else {
      source_offset += 1
    }
  }
  fn(cnum : Int) -> Int {
    if cnum < 0 {
      0
    } else if cnum >= table.length() {
      source_len
    } else {
      table[cnum]
    }
  }
}

///|
fn ParserState::peek(
  self : ParserState,
  offset? : Int = 0,
) -> (Token, Position, Position)? {
  self.tokens.get(self.pos + offset)
}

///|
fn ParserState::peek_significant(
  self : ParserState,
) -> (Token, Position, Position)? {
  let mut offset = 0
  while self.pos + offset < self.tokens.length() {
    match self.tokens[self.pos + offset] {
      (COMMENT(_), _, _) | (NEWLINE, _, _) => {
        offset += 1
        continue
      }
      triple => return Some(triple)
    }
  }
  None
}

///|
fn ParserState::peek_nth_significant(
  self : ParserState,
  nth : Int,
) -> (Token, Position, Position)? {
  let mut offset = 0
  let mut seen = 0
  while self.pos + offset < self.tokens.length() {
    match self.tokens[self.pos + offset] {
      (COMMENT(_), _, _) | (NEWLINE, _, _) => {
        offset += 1
        continue
      }
      triple => {
        if seen == nth {
          return Some(triple)
        }
        seen += 1
        offset += 1
      }
    }
  }
  None
}

///|
fn ParserState::token_source_span(
  self : ParserState,
  start : Position,
  end : Position,
) -> (Int, Int) {
  mk_span(
    (self.cnum_to_source_offset)(start.cnum),
    (self.cnum_to_source_offset)(end.cnum),
  )
}

///|
fn ParserState::consume_any(self : ParserState) -> CstNode? {
  match self.peek() {
    Some((tok, start, end)) => {
      self.pos += 1
      Some(token_node(tok, start, end, self.token_source_span(start, end)))
    }
    None => None
  }
}

///|
fn ParserState::consume_trivia(self : ParserState) -> Array[(String?, CstNode)] {
  let children : Array[(String?, CstNode)] = []
  let mut done = false
  while !done {
    match self.peek() {
      Some((COMMENT(_), _, _)) =>
        if self.consume_any() is Some(comment) {
          children.push((Some("trivia"), comment))
        } else {
          done = true
        }
      Some((NEWLINE, _, _)) => self.pos += 1
      _ => done = true
    }
  }
  children
}

///|
fn ParserState::previous_end(self : ParserState) -> Position {
  if self.pos > 0 {
    let (_, _, end) = self.tokens[self.pos - 1]
    end
  } else if self.tokens.length() > 0 {
    let (_, start, _) = self.tokens[0]
    start
  } else {
    initial_position("")
  }
}

///|
fn ParserState::missing_eof_offset(self : ParserState) -> Int {
  self.source.length()
}

///|
fn ParserState::missing_eof_node(self : ParserState) -> CstNode {
  let pos = self.previous_end()
  let offset = self.missing_eof_offset()
  let missing = missing_node(TokenKind::TK_EOF, pos, offset)
  self.reports.push(Report::{ loc: empty_loc(pos), msg: "expected EOF" })
  missing
}

///|
fn ParserState::root_loc(self : ParserState) -> Location {
  if self.tokens.length() == 0 {
    empty_loc(initial_position(""))
  } else {
    let (_, start, _) = self.tokens[0]
    let (_, _, end) = self.tokens[self.tokens.length() - 1]
    mk_loc(start, end)
  }
}

///|
fn ParserState::root_span(self : ParserState) -> (Int, Int) {
  mk_span(0, self.source.length())
}

///|
fn new_state(
  tokens : Triples,
  source : String,
  reports : Array[Report],
) -> ParserState {
  let mapper = default_location_mapper(source)
  ParserState::{
    tokens,
    pos: 0,
    reports,
    abort_current_decl: false,
    abort_infix_chain: false,
    suppress_next_top_level_recovery_report: false,
    source,
    cnum_to_source_offset: mapper,
  }
}
