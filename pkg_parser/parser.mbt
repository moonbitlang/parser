///|
priv struct State {
  tokens : Triples
  diagnostics : Array[Report]
  mut next : Int
  mut parsed_position : Position
}

///|
let dummy_pos : Position = { fname: "", lnum: 0, bol: 0, cnum: 0 }

///|
fn is_layout(token : Token) -> Bool {
  match token {
    NEWLINE | COMMENT(_) => true
    _ => false
  }
}

///|
fn State::peek(state : Self, nth? : Int = 0) -> Triple {
  let mut offset = 0
  let mut step = 0
  while state.tokens.get(state.next + offset) is Some((token, _, _) as triple) {
    if is_layout(token) {
      offset += 1
      continue
    }
    if step == nth {
      return triple
    }
    step += 1
    offset += 1
  }
  match state.tokens.last() {
    Some(last) => last
    None => (EOF, dummy_pos, dummy_pos)
  }
}

///|
fn State::consume(state : Self) -> Triple {
  while state.tokens.get(state.next) is Some((token, _, _) as triple) {
    state.next += 1
    if is_layout(token) {
      continue
    }
    state.parsed_position = triple.2
    return triple
  }
  panic()
}

///|
fn State::skip(state : Self) -> Unit {
  ignore(state.consume())
}

///|
fn State::peek_token(state : Self, nth? : Int = 0) -> Token {
  state.peek(nth~).0
}

///|
fn State::peek_kind(state : Self, nth? : Int = 0) -> TokenKind {
  state.peek(nth~).0.kind()
}

///|
fn State::peek_spos(state : Self, nth? : Int = 0) -> Position {
  state.peek(nth~).1
}

///|
fn State::peek_location(state : Self) -> Location {
  let (_, start, end) = state.peek()
  { start, end }
}

///|
fn State::loc_start_with(state : Self, start : Position) -> Location {
  { start, end: state.parsed_position }
}

///|
fn State::consume_if(state : Self, expected : TokenKind) -> Bool {
  if state.peek_kind() == expected {
    state.skip()
    true
  } else {
    false
  }
}

///|
fn State::report_failed_to_parse(
  state : Self,
  found : Token,
  expected : String,
  loc : Location,
) -> Unit {
  let msg = match found {
    EOF => "Unexpected end of file, missing \{expected} here."
    _ =>
      "Unexpected token \{found.to_expect_string()}, you may expect \{expected}."
  }
  state.diagnostics.push(Report::{ loc, msg })
}

///|
fn State::report_expected_here(state : Self, expected : String) -> Unit {
  state.report_failed_to_parse(
    state.peek_token(),
    expected,
    state.peek_location(),
  )
}

///|
fn State::skip_until(state : Self, kinds : Array[TokenKind]) -> Unit {
  while state.peek_kind() != TK_EOF && !kinds.contains(state.peek_kind()) {
    state.skip()
  }
}

///|
fn State::skip_until_statement_end(state : Self) -> Unit {
  state.skip_until([TK_SEMI, TK_EOF])
}

///|
fn State::expect_lident(state : Self, context~ : String) -> String {
  match state.peek_token() {
    LIDENT(name) => {
      state.skip()
      name
    }
    UIDENT(name) => {
      let loc = state.peek_location()
      state.skip()
      state.diagnostics.push(Report::{
        loc,
        msg: "Unexpected uppercase identifier \{name}, expected lowercase identifier in \{context}.",
      })
      ""
    }
    other => {
      let loc = state.peek_location()
      state.report_failed_to_parse(
        other,
        "lowercase identifier in \{context}",
        loc,
      )
      if other.kind() != TK_EOF {
        state.skip()
      }
      ""
    }
  }
}

///|
priv enum ImportKind {
  Regular
  Test
  Wbtest
}

///|
fn import_kind_to_key(kind : ImportKind) -> String {
  match kind {
    Regular => "import"
    Test => "test-import"
    Wbtest => "wbtest-import"
  }
}

///|
fn null_here(state : State) -> Ast {
  Null(state.peek_location())
}

///|
fn has_invalid_pkg_int_suffix(integer : String) -> Bool {
  integer.has_suffix("U") || integer.has_suffix("L")
}

///|
fn[A] parse_comma_separated(
  state : State,
  right~ : TokenKind,
  f : (State) -> A,
) -> Array[A] {
  let items = []
  if state.consume_if(right) {
    return items
  }
  if state.peek_kind() == TK_EOF {
    state.report_expected_here(right.to_expect_string())
    return items
  }
  while state.peek_kind() != TK_EOF {
    if state.peek_kind() == right {
      state.skip()
      break
    }
    let before = state.next
    items.push(f(state))
    if state.peek_kind() == TK_COMMA {
      state.skip()
      if state.peek_kind() == right {
        state.skip()
        break
      }
      continue
    }
    if state.peek_kind() == right {
      state.skip()
      break
    }
    if state.peek_kind() == TK_SEMI {
      state.report_expected_here(right.to_expect_string())
      break
    }
    if state.peek_kind() == TK_EOF {
      state.report_expected_here(right.to_expect_string())
      break
    }
    state.report_failed_to_parse(
      state.peek_token(),
      "`,` or \{right.to_expect_string()}",
      state.peek_location(),
    )
    if state.next == before && state.peek_kind() != TK_EOF {
      state.skip()
    }
    state.skip_until([TK_COMMA, right, TK_SEMI])
    if state.peek_kind() == TK_COMMA {
      state.skip()
      if state.peek_kind() == right {
        state.skip()
        break
      }
      continue
    }
    if state.peek_kind() == right {
      state.skip()
      break
    }
    if state.peek_kind() == TK_SEMI {
      break
    }
  }
  items
}

///|
fn parse_array(state : State) -> Ast {
  let start = state.peek_spos()
  guard state.consume_if(TK_LBRACKET) else {
    state.report_expected_here("\"[\"")
    return null_here(state)
  }
  let content = parse_comma_separated(state, right=TK_RBRACKET, parse_expr)
  Arr(content~, loc=state.loc_start_with(start))
}

///|
fn parse_map_entry(state : State) -> (String, Ast) {
  let key = match state.peek_token() {
    STRING(str) => {
      state.skip()
      str
    }
    other => {
      state.report_failed_to_parse(
        other,
        "string literal as map key",
        state.peek_location(),
      )
      if other.kind() != TK_EOF {
        state.skip()
      }
      ""
    }
  }
  if !state.consume_if(TK_COLON) {
    state.report_expected_here("\":\"")
    match state.peek_kind() {
      TK_COMMA | TK_RBRACE | TK_EOF => return (key, null_here(state))
      _ => ()
    }
  }
  (key, parse_expr(state))
}

///|
fn parse_map(state : State) -> Ast {
  let start = state.peek_spos()
  guard state.consume_if(TK_LBRACE) else {
    state.report_expected_here("\"{\"")
    return null_here(state)
  }
  let entries = parse_comma_separated(state, right=TK_RBRACE, parse_map_entry)
  Obj(map=Map::from_array(entries), loc=state.loc_start_with(start))
}

///|
fn parse_argument(state : State) -> (String, Ast) {
  if state.peek_kind(nth=1) == TK_COLON {
    match state.peek_token() {
      STRING(key) => {
        state.skip()
        ignore(state.consume_if(TK_COLON))
        (key, parse_expr(state))
      }
      LIDENT(key) => {
        state.skip()
        ignore(state.consume_if(TK_COLON))
        (key, parse_expr(state))
      }
      _ => {
        state.report_failed_to_parse(
          state.peek_token(),
          "labeled argument",
          state.peek_location(),
        )
        ("", parse_expr(state))
      }
    }
  } else {
    state.report_failed_to_parse(
      state.peek_token(),
      "labeled argument",
      state.peek_location(),
    )
    ("", parse_expr(state))
  }
}

///|
fn parse_apply(state : State) -> (String, Ast) {
  let name = state.expect_lident(context="name")
  let args_start = state.peek_spos()
  if !state.consume_if(TK_LPAREN) {
    state.report_expected_here("\"(\"")
    state.skip_until_statement_end()
    let loc = Location::{ start: args_start, end: args_start }
    return (name, Obj(map=Map::new(), loc~))
  }
  let args = parse_comma_separated(state, right=TK_RPAREN, parse_argument)
  (name, Obj(map=Map::from_array(args), loc=state.loc_start_with(args_start)))
}

///|
fn is_statement_start_token(token : Token) -> Bool {
  match token {
    IMPORT | LIDENT(_) => true
    _ => false
  }
}

///|
fn is_statement_start_after_newline(state : State) -> Bool {
  state.peek_spos().lnum > state.parsed_position.lnum &&
  is_statement_start_token(state.peek_token())
}

///|
fn parse_assign(state : State) -> (String, Ast) {
  let name = state.expect_lident(context="name")
  ignore(state.consume_if(TK_EQUAL))
  if is_statement_start_after_newline(state) {
    let loc = state.peek_location()
    state.report_failed_to_parse(state.peek_token(), "expression", loc)
    return (name, Null(loc))
  }
  (name, parse_expr(state))
}

///|
fn parse_import_item(state : State) -> Ast {
  let path_loc = state.peek_location()
  let path = match state.peek_token() {
    STRING(path) => {
      state.skip()
      path
    }
    other => {
      state.report_failed_to_parse(
        other,
        "package path in string",
        state.peek_location(),
      )
      if other.kind() != TK_EOF {
        state.skip()
      }
      ""
    }
  }
  let package_alias = match state.peek_token() {
    PACKAGE_NAME(alias_name) => {
      let alias_loc = state.peek_location()
      state.skip()
      Some((alias_name, alias_loc))
    }
    AS => {
      let alias_loc = state.peek_location()
      state.skip()
      state.diagnostics.push(Report::{
        loc: alias_loc,
        msg: "Old import alias syntax is no longer supported; use `\"...\" @alias` instead of `\"...\" as @alias`.",
      })
      match state.peek_kind() {
        TK_PACKAGE_NAME => state.skip()
        TK_COMMA | TK_RBRACE | TK_EOF => ()
        _ => state.skip()
      }
      None
    }
    _ => None
  }
  match package_alias {
    None => Str(str=path, loc=path_loc)
    Some((alias_name, alias_loc)) =>
      Obj(
        map=Map::from_array([
          ("path", Str(str=path, loc=path_loc)),
          ("alias", Str(str=alias_name, loc=alias_loc)),
        ]),
        loc=path_loc,
      )
  }
}

///|
fn parse_import_kind_string(state : State) -> ImportKind {
  match state.peek_token() {
    STRING("test") => {
      state.skip()
      Test
    }
    STRING("wbtest") => {
      state.skip()
      Wbtest
    }
    STRING(_) => {
      state.report_failed_to_parse(
        state.peek_token(),
        "\"test\" or \"wbtest\"",
        state.peek_location(),
      )
      state.skip()
      Regular
    }
    other => {
      state.report_failed_to_parse(
        other,
        "string literal \"test\" or \"wbtest\"",
        state.peek_location(),
      )
      Regular
    }
  }
}

///|
fn parse_import_statement(state : State) -> (String, Ast) {
  let start = state.peek_spos()
  ignore(state.consume_if(TK_IMPORT))
  let mut kind = Regular
  if state.peek_token() is STRING(_) {
    let legacy_loc = state.peek_location()
    state.skip()
    state.diagnostics.push(Report::{
      loc: legacy_loc,
      msg: "Old import syntax is no longer supported; use `import { ... }`, `import { ... } for \"test\"`, or `import { ... } for \"wbtest\"`.",
    })
  }
  if !state.consume_if(TK_LBRACE) {
    state.report_expected_here("\"{\"")
    state.skip_until_statement_end()
    return (
      import_kind_to_key(kind),
      Arr(content=[], loc=state.loc_start_with(start)),
    )
  }
  let packages = parse_comma_separated(
    state,
    right=TK_RBRACE,
    parse_import_item,
  )
  if state.consume_if(TK_FOR) {
    kind = parse_import_kind_string(state)
  }
  (
    import_kind_to_key(kind),
    Arr(content=packages, loc=state.loc_start_with(start)),
  )
}

///|
fn parse_expr(state : State) -> Ast {
  match state.peek_token() {
    TRUE => {
      let loc = state.peek_location()
      state.skip()
      True(loc)
    }
    FALSE => {
      let loc = state.peek_location()
      state.skip()
      False(loc)
    }
    STRING(str) => {
      let loc = state.peek_location()
      state.skip()
      Str(str~, loc~)
    }
    INT(integer) as token => {
      let loc = state.peek_location()
      state.skip()
      if has_invalid_pkg_int_suffix(integer) {
        state.report_failed_to_parse(
          token, "integer literals without U or L suffixes", loc,
        )
        Null(loc)
      } else {
        Float(float=integer, loc~)
      }
    }
    FLOAT(_) | DOUBLE(_) as token => {
      let loc = state.peek_location()
      state.skip()
      state.report_failed_to_parse(token, "expression", loc)
      Null(loc)
    }
    LBRACE => parse_map(state)
    LBRACKET => parse_array(state)
    other => {
      let loc = state.peek_location()
      state.report_failed_to_parse(other, "expression", loc)
      if other.kind() != TK_EOF && other.kind() != TK_SEMI {
        state.skip()
      }
      Null(loc)
    }
  }
}

///|
fn parse_statement(state : State) -> (String, Ast) {
  match state.peek_token() {
    IMPORT => parse_import_statement(state)
    LIDENT(_) if state.peek_kind(nth=1) == TK_EQUAL => parse_assign(state)
    LIDENT(_) => parse_apply(state)
    other => {
      let loc = state.peek_location()
      state.report_failed_to_parse(other, "package statement", loc)
      if other.kind() != TK_EOF {
        state.skip()
      }
      ("", Null(loc))
    }
  }
}

///|
fn skip_statement_separators(state : State) -> Unit {
  while state.peek_token() is SEMI(_) {
    state.skip()
  }
}

///|
fn parse_statements(state : State) -> Array[(String, Ast)] {
  let statements = []
  skip_statement_separators(state)
  while state.peek_kind() != TK_EOF {
    let before = state.next
    let diagnostics_before = state.diagnostics.length()
    let stmt = parse_statement(state)
    if stmt.0 != "" {
      statements.push(stmt)
    }
    if state.next == before && state.peek_kind() != TK_EOF {
      state.skip()
    }
    if state.peek_kind() == TK_EOF {
      break
    }
    if state.peek_kind() == TK_SEMI {
      skip_statement_separators(state)
      continue
    }
    if state.diagnostics.length() > diagnostics_before &&
      is_statement_start_after_newline(state) {
      continue
    }
    state.report_failed_to_parse(
      state.peek_token(),
      "`;` or EOF",
      state.peek_location(),
    )
    state.skip_until_statement_end()
    skip_statement_separators(state)
  }
  statements
}

///|
pub fn moon_pkg_of_tokens(tokens : Triples) -> (Ast, Array[Report]) {
  if tokens.is_empty() {
    let loc = Location::{ start: dummy_pos, end: dummy_pos }
    return (Obj(map=Map::new(), loc~), [])
  }
  let start = tokens[0].1
  let state = State::{
    tokens,
    diagnostics: [],
    next: 0,
    parsed_position: start,
  }
  let object_start = state.peek_spos()
  state.parsed_position = object_start
  let statements = parse_statements(state)
  let ast = Obj(
    map=Map::from_array(statements),
    loc=state.loc_start_with(object_start),
  )
  (ast, state.diagnostics)
}

///|
pub fn parse_string(
  source : String,
  name? : String = "",
) -> (Ast, Array[Report]) {
  let lex_result = @lexer.tokens_from_string(source, comment=false, name~)
  let (ast, reports) = moon_pkg_of_tokens(lex_result.tokens)
  let diagnostics = []
  lex_result.errors.each(fn(err_triple) {
    let (start, end, err) = err_triple
    diagnostics.push(Report::{ loc: { start, end }, msg: err.to_string() })
  })
  reports.each(report => diagnostics.push(report))
  (ast, diagnostics)
}

///|
pub fn parse_file(path : String) -> (Ast, Array[Report]) raise @fs.IOError {
  let source = @fs.read_file_to_string(path)
  parse_string(source, name=path)
}
