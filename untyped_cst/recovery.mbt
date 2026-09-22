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
fn ParserState::current_position(self : ParserState) -> Position {
  match self.peek_significant() {
    Some((_, start, _)) => start
    None => self.previous_end()
  }
}

///|
fn expected_token_kinds_to_string(kinds : Array[TokenKind]) -> String {
  match kinds {
    [] => ""
    [kind] => kind.to_expect_string()
    _ => kinds.iter().map(kind => kind.to_expect_string()).join(", ")
  }
}

///|
fn ParserState::report_expected_description_at(
  self : ParserState,
  found : Token,
  loc : Location,
  expected : String,
) -> Unit {
  let msg = match found {
    SEMI(false) =>
      "Unexpected line break here, missing \{expected} at the end of this line."
    EOF => "Unexpected end of file, missing \{expected} here."
    _ =>
      "Unexpected token \{found.to_expect_string()}, you may expect \{expected}."
  }
  self.report(loc, msg)
}

///|
fn ParserState::report_expected_tokens_at(
  self : ParserState,
  found : Token,
  loc : Location,
  expected : Array[TokenKind],
) -> Unit {
  self.report_expected_description_at(
    found,
    loc,
    expected_token_kinds_to_string(expected),
  )
}

///|
fn ParserState::report_expected_description(
  self : ParserState,
  expected : String,
) -> Unit {
  match self.peek_significant() {
    Some((found, start, end)) =>
      self.report_expected_description_at(found, mk_loc(start, end), expected)
    None => {
      let pos = self.previous_end()
      self.report(
        empty_loc(pos),
        "Unexpected end of file, missing \{expected} here.",
      )
    }
  }
}

///|
fn ParserState::report_expected_tokens(
  self : ParserState,
  expected : Array[TokenKind],
) -> Unit {
  self.report_expected_description(expected_token_kinds_to_string(expected))
}

///|
fn ParserState::offset_for_position(self : ParserState, pos : Position) -> Int {
  if self.peek() is None {
    self.source.length()
  } else {
    (self.cnum_to_source_offset)(pos.cnum)
  }
}

///|
fn ParserState::missing_token_node(
  self : ParserState,
  kind : TokenKind,
) -> CstNode {
  let pos = self.current_position()
  self.report_expected_tokens([kind])
  missing_node(kind, pos, self.offset_for_position(pos))
}

///|
fn ParserState::missing_token_node_with_expected(
  self : ParserState,
  kind : TokenKind,
  expected : Array[TokenKind],
) -> CstNode {
  let pos = self.current_position()
  self.report_expected_tokens(expected)
  missing_node(kind, pos, self.offset_for_position(pos))
}

///|
fn ParserState::missing_token_node_with_description(
  self : ParserState,
  kind : TokenKind,
  expected : String,
) -> CstNode {
  let pos = self.current_position()
  self.report_expected_description(expected)
  missing_node(kind, pos, self.offset_for_position(pos))
}

///|
fn ParserState::missing_token_node_silent(
  self : ParserState,
  kind : TokenKind,
) -> CstNode {
  let pos = self.current_position()
  missing_node(kind, pos, self.offset_for_position(pos))
}

///|
fn ParserState::missing_syntax_node(
  self : ParserState,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
) -> CstNode {
  let pos = self.current_position()
  node(builder, empty_loc(pos), empty_span(self.offset_for_position(pos)), [])
}

///|
fn ParserState::missing_type_node(self : ParserState) -> CstNode {
  self.missing_syntax_node(fn(loc, source_span, children) {
    let error = CstNode::children_with_label_from(children, "error")
    @node.CstNode::type_missing_type(loc~, source_span~, error~)
  })
}

///|
fn ParserState::missing_expr_node(self : ParserState) -> CstNode {
  self.missing_syntax_node(fn(loc, source_span, children) {
    let entries = recovery_children(children)
    @node.CstNode::expr_missing_expr_from_entries(loc~, source_span~, entries~)
  })
}

///|
fn ParserState::missing_pattern_node(self : ParserState) -> CstNode {
  self.missing_syntax_node(fn(loc, source_span, children) {
    let entries = recovery_children(children)
    @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
  })
}

///|
fn ParserState::required_type_node(
  self : ParserState,
  parsed : CstNode,
  reports_before : Int,
) -> CstNode {
  if !parsed.children.is_empty() {
    parsed
  } else {
    if self.reports.length() == reports_before {
      self.report_expected_description("type")
    }
    self.missing_type_node()
  }
}

///|
fn ParserState::required_expr_node(
  self : ParserState,
  parsed : CstNode,
  reports_before : Int,
) -> CstNode {
  if !parsed.children.is_empty() {
    parsed
  } else {
    if self.reports.length() == reports_before {
      self.report_expected_description("simple expression")
    }
    self.missing_expr_node()
  }
}

///|
fn required_pattern_start_tokens() -> Array[TokenKind] {
  [
    TK_TRUE,
    TK_FALSE,
    TK_CHAR,
    TK_INT,
    TK_BYTE,
    TK_FLOAT,
    TK_DOUBLE,
    TK_MINUS,
    TK_BYTES,
    TK_STRING,
    TK_UNDERSCORE,
    TK_LIDENT,
    TK_EXTEND,
    TK_PACKAGE_NAME,
    TK_UIDENT,
    TK_AMPER,
    TK_LPAREN,
    TK_LBRACKET,
    TK_LBRACE,
  ]
}

///|
fn ParserState::required_pattern_node(
  self : ParserState,
  parsed : CstNode,
  reports_before : Int,
  expected? : String = "",
) -> CstNode {
  if !parsed.children.is_empty() {
    parsed
  } else {
    if self.reports.length() == reports_before {
      if expected == "" {
        self.report_expected_tokens(required_pattern_start_tokens())
      } else {
        self.report_expected_description(expected)
      }
    }
    self.missing_pattern_node()
  }
}

///|
fn ParserState::report(
  self : ParserState,
  loc : Location,
  msg : String,
) -> Unit {
  self.reports.push({ loc, msg, })
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
