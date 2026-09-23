///|
fn initial_position(name : String) -> Position {
  { fname: name, lnum: 1, bol: 0, cnum: 0, }
}

///|
pub(all) struct ParserState {
  tokens : Triples
  mut pos : Int
  reports : Array[Report]
  enable_metavar : Bool
  mut abort_current_decl : Bool
  mut abort_infix_chain : Bool
  mut report_next_top_level_start : Bool
  mut suppress_next_top_level_recovery_report : Bool
  source : String
  cnum_to_source_offset : (Int) -> Int
  parse_interp_segment : (ParserState, @tokens.InterpElem) -> CstNode
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
pub fn ParserState::peek(
  self : ParserState,
  offset? : Int = 0,
) -> (Token, Position, Position)? {
  self.tokens.get(self.pos + offset)
}

///|
pub fn ParserState::peek_significant(
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
pub fn ParserState::peek_nth_significant(
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
  (
    (self.cnum_to_source_offset)(start.cnum),
    (self.cnum_to_source_offset)(end.cnum),
  )
}

///|
pub fn ParserState::previous_end(self : ParserState) -> Position {
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
pub fn ParserState::missing_eof_node(self : ParserState) -> CstNode {
  let pos = self.previous_end()
  let offset = self.missing_eof_offset()
  let missing = missing_node(TK_EOF, pos, offset)
  self.reports.push({ loc: empty_loc(pos), msg: "expected EOF", })
  missing
}

///|
pub fn ParserState::root_loc(self : ParserState) -> Location {
  if self.tokens.length() == 0 {
    empty_loc(initial_position(""))
  } else {
    let (_, start, _) = self.tokens[0]
    let (_, _, end) = self.tokens[self.tokens.length() - 1]
    mk_loc(start, end)
  }
}

///|
pub fn ParserState::root_span(self : ParserState) -> (Int, Int) {
  (0, self.source.length())
}

///|
pub fn new_state(
  tokens : Triples,
  source : String,
  reports : Array[Report],
  enable_metavar : Bool,
  parse_interp_segment : (ParserState, @tokens.InterpElem) -> CstNode,
) -> ParserState {
  let mapper = default_location_mapper(source)
  {
    tokens,
    pos: 0,
    reports,
    enable_metavar,
    abort_current_decl: false,
    abort_infix_chain: false,
    report_next_top_level_start: false,
    suppress_next_top_level_recovery_report: false,
    source,
    cnum_to_source_offset: mapper,
    parse_interp_segment,
  }
}
