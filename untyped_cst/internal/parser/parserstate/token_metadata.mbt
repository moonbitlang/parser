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
  { fname: pos.fname, lnum, bol, cnum, }
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
    (span_start, span_start + value.length()),
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
            let value = match @construction.payload_from_children(children) {
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
            let value = match @construction.payload_from_children(children) {
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
            let value = match @construction.payload_from_children(children) {
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
        let qualifier = @construction.first_child_with_label_from(
          children, "qualifier",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let raw_payload = @construction.first_child_with_label_from(
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
            let value = match @construction.payload_from_children(children) {
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
            let value = match @construction.payload_from_children(children) {
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
        let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let raw_payload = @construction.first_child_with_label_from(
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
          let value = match @construction.payload_from_children(children) {
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
        let content = @construction.first_child_with_label_from(
          children, "content",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let consumed_by_docstring = @construction.first_child_with_label_from(
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
            let value = match @construction.payload_from_children(children) {
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
            let value = match @construction.payload_from_children(children) {
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
        let content = @construction.first_child_with_label_from(
          children, "content",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let leading_blank_line = @construction.first_child_with_label_from(
          children, "leading_blank_line",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let trailing_blank_line = @construction.first_child_with_label_from(
          children, "trailing_blank_line",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let consumed_by_docstring = @construction.first_child_with_label_from(
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
        let value = match @construction.payload_from_children(children) {
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
        push_child(
          children,
          Some("segment"),
          (self.parse_interp_segment)(self, elem),
        )
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
pub fn ParserState::consume_any(self : ParserState) -> CstNode? {
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
pub fn ParserState::consume_trivia(
  self : ParserState,
) -> Array[(String?, CstNode)] {
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
