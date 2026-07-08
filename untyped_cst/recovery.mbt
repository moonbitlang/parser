///|
fn report_from_lex_error(
  start : Position,
  end : Position,
  err : @lexer.LexicalError,
) -> Report {
  Report::{ loc: mk_loc(start, end), msg: err.to_string() }
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
/// Captures the handrolled parser's next-token location without adding source
/// text to the enclosing CST node. This is lowering-only compatibility data.
fn ParserState::current_significant_anchor(self : ParserState) -> CstNode {
  match self.peek_significant() {
    Some((_, start, end)) =>
      error_node(
        mk_loc(start, end),
        empty_span(self.offset_for_position(start)),
        [],
      )
    None => {
      let pos = self.previous_end()
      error_node(empty_loc(pos), empty_span(self.offset_for_position(pos)), [])
    }
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
  kind : NodeKind,
) -> CstNode {
  let pos = self.current_position()
  node(kind, empty_loc(pos), empty_span(self.offset_for_position(pos)), [])
}

///|
fn ParserState::missing_type_node(self : ParserState) -> CstNode {
  self.missing_syntax_node(Type_MissingType)
}

///|
fn ParserState::missing_expr_node(self : ParserState) -> CstNode {
  self.missing_syntax_node(Expr_MissingExpr)
}

///|
fn ParserState::missing_pattern_node(self : ParserState) -> CstNode {
  self.missing_syntax_node(Pattern_MissingPattern)
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
    TokenKind::TK_TRUE,
    TokenKind::TK_FALSE,
    TokenKind::TK_CHAR,
    TokenKind::TK_INT,
    TokenKind::TK_BYTE,
    TokenKind::TK_FLOAT,
    TokenKind::TK_DOUBLE,
    TokenKind::TK_MINUS,
    TokenKind::TK_BYTES,
    TokenKind::TK_STRING,
    TokenKind::TK_UNDERSCORE,
    TokenKind::TK_LIDENT,
    TokenKind::TK_EXTEND,
    TokenKind::TK_PACKAGE_NAME,
    TokenKind::TK_UIDENT,
    TokenKind::TK_LPAREN,
    TokenKind::TK_LBRACKET,
    TokenKind::TK_LBRACE,
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
  self.reports.push(Report::{ loc, msg })
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
