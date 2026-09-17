///|
priv struct ParserState {
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
  Position::{ fname: pos.fname, lnum, bol, cnum, }
}

///|
fn scalar_subtoken_node(
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  token_start : Position,
  token_span : (Int, Int),
  prefix : String,
  value : String,
) -> CstNode {
  let start = advance_position(token_start, prefix)
  let end = advance_position(start, value)
  let span_start = token_span.0 + prefix.length()
  scalar_node(
    builder,
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
  let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = match
    qualified_name {
    Some(name) => {
      push_child(
        children,
        Some("qualifier"),
        scalar_subtoken_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::attribute_qualifier(value~, loc~, source_span~)
          },
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
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::attribute_name(value~, loc~, source_span~)
          },
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
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::attribute_raw_payload(value~, loc~, source_span~)
          },
          start,
          source_span,
          "#\{first}.\{name}",
          raw_payload,
        ),
      )
      fn(loc, source_span, children) {
        let qualifier = CstNode::first_child_with_label_from(
          children, "qualifier",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let name = CstNode::first_child_with_label_from(children, "name").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let raw_payload = CstNode::first_child_with_label_from(
          children, "raw_payload",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        @node.CstNode::attribute_qualified(
          loc~,
          source_span~,
          qualifier~,
          name~,
          raw_payload~,
        )
      }
    }
    None => {
      push_child(
        children,
        Some("name"),
        scalar_subtoken_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::attribute_name(value~, loc~, source_span~)
          },
          start,
          source_span,
          "#",
          first,
        ),
      )
      push_child(
        children,
        Some("raw_payload"),
        scalar_subtoken_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::attribute_raw_payload(value~, loc~, source_span~)
          },
          start,
          source_span,
          "#\{first}",
          raw_payload,
        ),
      )
      fn(loc, source_span, children) {
        let name = CstNode::first_child_with_label_from(children, "name").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let raw_payload = CstNode::first_child_with_label_from(
          children, "raw_payload",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        @node.CstNode::attribute_simple(loc~, source_span~, name~, raw_payload~)
      }
    }
  }
  node(builder, mk_loc(start, end), source_span, children)
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
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::comment_content(value~, loc~, source_span~)
        },
        mk_loc(start, end),
        source_span,
        String(comment.content),
      ),
    ),
  ]
  let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = match
    comment.kind {
    @tokens.InlineTrailing =>
      fn(loc, source_span, children) {
        let content = CstNode::first_child_with_label_from(children, "content").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let consumed_by_docstring = CstNode::first_child_with_label_from(
          children, "consumed_by_docstring",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        @node.CstNode::comment_inline_trailing(
          loc~,
          source_span~,
          content~,
          consumed_by_docstring~,
        )
      }
    @tokens.Ownline(leading_blank_line~, trailing_blank_line~) => {
      push_child(
        children,
        Some("leading_blank_line"),
        bool_marker_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::Bool(value)) => value
              _ => false
            }
            @node.CstNode::comment_leading_blank_line(
              value~,
              loc~,
              source_span~,
            )
          },
          start,
          source_span.0,
          leading_blank_line,
        ),
      )
      push_child(
        children,
        Some("trailing_blank_line"),
        bool_marker_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::Bool(value)) => value
              _ => false
            }
            @node.CstNode::comment_trailing_blank_line(
              value~,
              loc~,
              source_span~,
            )
          },
          start,
          source_span.0,
          trailing_blank_line,
        ),
      )
      fn(loc, source_span, children) {
        let content = CstNode::first_child_with_label_from(children, "content").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let leading_blank_line = CstNode::first_child_with_label_from(
          children, "leading_blank_line",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let trailing_blank_line = CstNode::first_child_with_label_from(
          children, "trailing_blank_line",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let consumed_by_docstring = CstNode::first_child_with_label_from(
          children, "consumed_by_docstring",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        @node.CstNode::comment_ownline(
          loc~,
          source_span~,
          content~,
          leading_blank_line~,
          trailing_blank_line~,
          consumed_by_docstring~,
        )
      }
    }
  }
  push_child(
    children,
    Some("consumed_by_docstring"),
    bool_marker_node(
      fn(loc, source_span, children) {
        let value = match CstNode::payload_from_children(children) {
          Some(NodePayload::Bool(value)) => value
          _ => false
        }
        @node.CstNode::comment_consumed_by_docstring(value~, loc~, source_span~)
      },
      start,
      source_span.0,
      comment.consumed_by_docstring,
    ),
  )
  node(builder, mk_loc(start, end), source_span, children)
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
  self.reports.push(Report::{ loc: empty_loc(pos), msg: "expected EOF", })
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
  enable_metavar : Bool,
) -> ParserState {
  let mapper = default_location_mapper(source)
  ParserState::{
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
  }
}
