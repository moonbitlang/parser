///|
priv struct State {
  tokens : Triples
  diagnostics : Array[Report]
  mut next : Int32
  mut parsed_position : Position
}

///|
let dummy_pos : Position = { fname: "", lnum: 0, bol: 0, cnum: 0, }

///|
fn is_layout(token : Token) -> Bool {
  match token {
    NEWLINE | COMMENT(_) => true
    _ => false
  }
}

///|
fn State::peek(state : Self, nth~ : Int32 = 0) -> Triple {
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
fn State::peek_token(state : Self, nth~ : Int32 = 0) -> Token {
  state.peek(nth~).0
}

///|
fn State::peek_kind(state : Self, nth~ : Int32 = 0) -> TokenKind {
  state.peek(nth~).0.kind()
}

///|
fn State::peek_spos(state : Self, nth~ : Int32 = 0) -> Position {
  state.peek(nth~).1
}

///|
fn State::peek_location(state : Self) -> Location {
  let (_, start, end) = state.peek()
  { start, end, }
}

///|
fn State::loc_start_with(state : Self, start : Position) -> Location {
  { start, end: state.parsed_position, }
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
  state.diagnostics.push({ loc, msg, })
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
      state.diagnostics.push({
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
fn import_kind_to_key(kind : ImportKind) -> String {
  match kind {
    Normal => "import"
    Test => "test-import"
    Wbtest => "wbtest-import"
  }
}

///|
fn hole_here(state : State) -> Expr {
  Hole(state.peek_location())
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
  while true {
    if state.peek_kind() == TK_EOF {
      state.report_expected_here(right.to_expect_string())
      break
    }
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
fn parse_array(state : State) -> Expr {
  let start = state.peek_spos()
  ignore(state.consume_if(TK_LBRACKET))
  let items = parse_comma_separated(state, right=TK_RBRACKET, parse_expr)
  Array(items~, loc=state.loc_start_with(start))
}

///|
fn parse_map_entry(state : State) -> Field {
  let key_loc = state.peek_location()
  let key = match state.peek_token() {
    STRING(str) => {
      state.skip()
      str
    }
    other => {
      state.report_failed_to_parse(other, "string literal as map key", key_loc)
      if other.kind() != TK_EOF {
        state.skip()
      }
      ""
    }
  }
  if !state.consume_if(TK_COLON) {
    state.report_expected_here("\":\"")
    match state.peek_kind() {
      TK_COMMA | TK_RBRACE | TK_EOF =>
        return { key, key_loc, value: hole_here(state), }
      _ => ()
    }
  }
  { key, key_loc, value: parse_expr(state), }
}

///|
fn parse_map(state : State) -> Expr {
  let start = state.peek_spos()
  ignore(state.consume_if(TK_LBRACE))
  let fields = parse_comma_separated(state, right=TK_RBRACE, parse_map_entry)
  Map(fields~, loc=state.loc_start_with(start))
}

///|
fn parse_argument(state : State) -> Field {
  let key_loc = state.peek_location()
  if state.peek_kind(nth=1) == TK_COLON {
    match state.peek_token() {
      STRING(key) | LIDENT(key) => {
        state.skip()
        ignore(state.consume_if(TK_COLON))
        return { key, key_loc, value: parse_expr(state), }
      }
      _ => ()
    }
  }
  state.report_failed_to_parse(state.peek_token(), "labeled argument", key_loc)
  { key: "", key_loc, value: parse_expr(state), }
}

///|
fn parse_apply(state : State) -> Expr {
  let name_loc = state.peek_location()
  let name = state.expect_lident(context="name")
  let args_start = state.peek_spos()
  let args = if state.consume_if(TK_LPAREN) {
    parse_comma_separated(state, right=TK_RPAREN, parse_argument)
  } else {
    state.report_expected_here("\"(\"")
    state.skip_until_statement_end()
    []
  }
  Apply(
    name~,
    name_loc~,
    args~,
    args_loc=state.loc_start_with(args_start),
    loc=state.loc_start_with(name_loc.start),
  )
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
fn parse_assign(state : State) -> Field {
  let key_loc = state.peek_location()
  let key = state.expect_lident(context="name")
  ignore(state.consume_if(TK_EQUAL))
  if is_statement_start_after_newline(state) {
    let loc = state.peek_location()
    state.report_failed_to_parse(state.peek_token(), "expression", loc)
    return { key, key_loc, value: Hole(loc), }
  }
  { key, key_loc, value: parse_expr(state), }
}

///|
fn parse_import_item(state : State) -> ImportItem {
  let path_loc = state.peek_location()
  let path = match state.peek_token() {
    STRING(path) => {
      state.skip()
      path
    }
    other => {
      state.report_failed_to_parse(other, "package path in string", path_loc)
      if other.kind() != TK_EOF {
        state.skip()
      }
      ""
    }
  }
  let alias_ = match state.peek_token() {
    PACKAGE_NAME(alias_name) => {
      let alias_loc = state.peek_location()
      state.skip()
      if state.consume_if(TK_STAR) {
        Some(("*" + alias_name, state.loc_start_with(alias_loc.start)))
      } else {
        Some((alias_name, alias_loc))
      }
    }
    STAR => {
      let star_loc = state.peek_location()
      state.skip()
      Some(("*", star_loc))
    }
    AS => {
      let alias_loc = state.peek_location()
      state.skip()
      state.diagnostics.push({
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
  { path, path_loc, alias_, loc: state.loc_start_with(path_loc.start), }
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
      Normal
    }
    other => {
      state.report_failed_to_parse(
        other,
        "string literal \"test\" or \"wbtest\"",
        state.peek_location(),
      )
      Normal
    }
  }
}

///|
fn parse_import_statement(state : State) -> Statement {
  let keyword_loc = state.peek_location()
  ignore(state.consume_if(TK_IMPORT))
  if state.peek_token() is STRING(_) {
    let legacy_loc = state.peek_location()
    state.skip()
    state.diagnostics.push({
      loc: legacy_loc,
      msg: "Old import syntax is no longer supported; use `import { ... }`, `import { ... } for \"test\"`, or `import { ... } for \"wbtest\"`.",
    })
  }
  let list_start = state.peek_spos()
  let items = if state.consume_if(TK_LBRACE) {
    parse_comma_separated(state, right=TK_RBRACE, parse_import_item)
  } else {
    state.report_expected_here("\"{\"")
    state.skip_until_statement_end()
    []
  }
  let list_loc = state.loc_start_with(list_start)
  let kind = if state.consume_if(TK_FOR) {
    parse_import_kind_string(state)
  } else {
    Normal
  }
  Import(
    items~,
    kind~,
    keyword_loc~,
    list_loc~,
    loc=state.loc_start_with(keyword_loc.start),
  )
}

///|
fn parse_expr(state : State) -> Expr {
  match state.peek_token() {
    TRUE | FALSE as token => {
      let loc = state.peek_location()
      state.skip()
      Bool(value=token is TRUE, loc~)
    }
    STRING(str) => {
      let loc = state.peek_location()
      state.skip()
      String(str~, loc~)
    }
    INT(repr) as token => {
      let loc = state.peek_location()
      state.skip()
      if has_invalid_pkg_int_suffix(repr) {
        state.report_failed_to_parse(
          token, "integer literals without U or L suffixes", loc,
        )
        Hole(loc)
      } else {
        Int(repr~, loc~)
      }
    }
    LBRACE => parse_map(state)
    LBRACKET => parse_array(state)
    other => {
      let loc = state.peek_location()
      state.report_failed_to_parse(other, "expression", loc)
      if other.kind() != TK_EOF && other.kind() != TK_SEMI {
        state.skip()
      }
      Hole(loc)
    }
  }
}

///|
fn parse_statement(state : State) -> Statement {
  match state.peek_token() {
    IMPORT => parse_import_statement(state)
    LIDENT(_) if state.peek_kind(nth=1) == TK_EQUAL =>
      Assign(parse_assign(state))
    LIDENT(_) => Expr(parse_apply(state))
    other => {
      let loc = state.peek_location()
      state.report_failed_to_parse(other, "package statement", loc)
      if other.kind() != TK_EOF && other.kind() != TK_SEMI {
        state.skip()
      }
      Expr(Hole(loc))
    }
  }
}

///|
fn parse_statements(state : State) -> Ast {
  let statements = []
  while state.peek_kind() != TK_EOF {
    let before = state.next
    let diagnostics_before = state.diagnostics.length()
    statements.push(parse_statement(state))
    if state.consume_if(TK_SEMI) {
      continue
    }
    if state.peek_kind() == TK_EOF {
      break
    }
    if state.next == before {
      state.skip()
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
    ignore(state.consume_if(TK_SEMI))
  }
  Ast(Vector(statements))
}

///|
fn moon_pkg_of_tokens(tokens : Triples) -> (Ast, Array[Report]) {
  if tokens.is_empty() {
    return (Ast(Vector([])), [])
  }
  let state = State::{
    tokens,
    diagnostics: [],
    next: 0,
    parsed_position: tokens[0].1,
  }
  (parse_statements(state), state.diagnostics)
}

///|
fn parse_string(
  source : String,
  name~ : String = "",
  keep_comments~ : Bool = false,
  allow_reserved~ : Bool = false,
) -> ParseResult {
  let lexed = @lexer.tokens_from_string(source, comment=keep_comments, name~)
  let (ast, errors) = moon_pkg_of_tokens(lexed.tokens)
  let diagnostics = Diagnostics::{ errors: [], warnings: [], }
  for (start, end, error) in lexed.errors {
    let report = Report::{ loc: { start, end, }, msg: Show::to_string(error), }
    if error is @lexer.Reserved_keyword(_) {
      if !allow_reserved {
        diagnostics.warnings.push(report)
      }
    } else {
      diagnostics.errors.push(report)
    }
  }
  diagnostics.errors.append(errors)
  { ast, tokens: lexed.tokens, diagnostics, }
}
