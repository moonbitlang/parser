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
fn advance_position(pos : Position, text : String) -> Position {
  let mut lnum = pos.lnum
  let mut bol = pos.bol
  let mut cnum = pos.cnum
  for ch in text {
    cnum += 1
    if ch == '\n' {
      lnum += 1
      bol = cnum
    }
  }
  Position::{ fname: pos.fname, lnum, bol, cnum }
}

///|
fn scalar_subtoken_node(
  kind : NodeKind,
  token_start : Position,
  token_span : (Int, Int),
  prefix : String,
  value : String,
) -> CstNode {
  let start = advance_position(token_start, prefix)
  let end = advance_position(start, value)
  let span_start = token_span.0 + prefix.length()
  scalar_node(
    kind,
    mk_loc(start, end),
    mk_span(span_start, span_start + value.length()),
    String(value),
  )
}

///|
fn attribute_metadata_node(
  content : (String, String?, String),
  start : Position,
  end : Position,
  source_span : (Int, Int),
) -> CstNode {
  let (first, qualified_name, raw_payload) = content
  let children : Array[(String?, CstNode)] = []
  let kind = match qualified_name {
    Some(name) => {
      push_child(
        children,
        Some("qualifier"),
        scalar_subtoken_node(
          Attribute_Qualifier,
          start,
          source_span,
          "#",
          first,
        ),
      )
      push_child(
        children,
        Some("name"),
        scalar_subtoken_node(
          Attribute_Name,
          start,
          source_span,
          "#\{first}.",
          name,
        ),
      )
      push_child(
        children,
        Some("raw_payload"),
        scalar_subtoken_node(
          Attribute_RawPayload,
          start,
          source_span,
          "#\{first}.\{name}",
          raw_payload,
        ),
      )
      Attribute_Qualified
    }
    None => {
      push_child(
        children,
        Some("name"),
        scalar_subtoken_node(Attribute_Name, start, source_span, "#", first),
      )
      push_child(
        children,
        Some("raw_payload"),
        scalar_subtoken_node(
          Attribute_RawPayload,
          start,
          source_span,
          "#\{first}",
          raw_payload,
        ),
      )
      Attribute_Simple
    }
  }
  node(kind, mk_loc(start, end), source_span, children)
}

///|
fn comment_metadata_node(
  comment : Comment,
  start : Position,
  end : Position,
  source_span : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = [
    (
      Some("content"),
      scalar_node(
        Comment_Content,
        mk_loc(start, end),
        source_span,
        String(comment.content),
      ),
    ),
  ]
  let kind = match comment.kind {
    @tokens.InlineTrailing => Comment_InlineTrailing
    @tokens.Ownline(leading_blank_line~, trailing_blank_line~) => {
      push_child(
        children,
        Some("leading_blank_line"),
        bool_marker_node(
          Comment_LeadingBlankLine,
          start,
          source_span.0,
          leading_blank_line,
        ),
      )
      push_child(
        children,
        Some("trailing_blank_line"),
        bool_marker_node(
          Comment_TrailingBlankLine,
          start,
          source_span.0,
          trailing_blank_line,
        ),
      )
      Comment_Ownline
    }
  }
  push_child(
    children,
    Some("consumed_by_docstring"),
    bool_marker_node(
      Comment_ConsumedByDocstring,
      start,
      source_span.0,
      comment.consumed_by_docstring,
    ),
  )
  node(kind, mk_loc(start, end), source_span, children)
}

///|
fn ParserState::token_metadata_children(
  self : ParserState,
  token : Token,
  start : Position,
  end : Position,
  source_span : (Int, Int),
) -> Array[(String?, CstNode)] {
  let children : Array[(String?, CstNode)] = []
  match token {
    ATTRIBUTE(content) =>
      push_child(
        children,
        Some("attribute"),
        attribute_metadata_node(content, start, end, source_span),
      )
    COMMENT(comment) =>
      push_child(
        children,
        Some("comment"),
        comment_metadata_node(comment, start, end, source_span),
      )
    INTERP(elems)
    | BYTES_INTERP(elems)
    | MULTILINE_INTERP(elems)
    | REGEX_INTERP(elems) =>
      for elem in elems {
        push_child(children, Some("segment"), parse_interp_segment(self, elem))
      }
    _ => ()
  }
  push_children(
    children,
    raw_token_semantic_children(token, mk_loc(start, end), source_span),
  )
  children
}

///|
fn ParserState::consume_any(self : ParserState) -> CstNode? {
  match self.peek() {
    Some((tok, start, end)) => {
      self.pos += 1
      let source_span = self.token_source_span(start, end)
      Some(
        token_node_with_children(
          tok,
          start,
          end,
          source_span,
          self.token_metadata_children(tok, start, end, source_span),
        ),
      )
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
