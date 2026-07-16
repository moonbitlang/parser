///|
priv struct IterArrayFinder {
  mut loc : Location?
}

///|
extend IterArrayFinder with IterVisitor::{base, visit_Impl}

///|
impl IterVisitor for IterArrayFinder with fn visit_Expr_Array(
  self,
  exprs~,
  is_iter~,
  loc~,
) {
  if is_iter {
    self.loc = Some(loc)
  }
  self.base().visit_Expr_Array(exprs~, is_iter~, loc~)
}

///|
fn parse_impls(source : String) -> (@syntax.Impls, @tokens.Triples) raise {
  let { tokens, errors, .. } = @lexer.tokens_from_string(comment=true, source)
  if errors.length() != 0 {
    fail("expected fixture to lex without diagnostics")
  }
  let (impls, reports) = @handrolled_parser.parse(tokens)
  if reports.length() != 0 {
    fail("expected fixture to parse without diagnostics")
  }
  (impls, tokens)
}

///|
fn find_iter_array_loc(impls : @syntax.Impls) -> Location raise {
  let finder = { loc: None }
  impls.map(item => finder.visit_Impl(item)) |> ignore
  if finder.loc is Some(loc) {
    loc
  } else {
    fail("expected fixture to contain iter array")
  }
}

///|
fn token_kind_at(tokens : @tokens.Triples, key : Key) -> TokenKind? {
  let loc = key.loc()
  for triple in tokens {
    match triple {
      (tok, start, end) =>
        if loc.start == start && loc.end == end {
          return Some(tok.kind())
        }
    }
  }
  None
}

///|
fn assert_token_kind(
  tokens : @tokens.Triples,
  key : Key?,
  expected : TokenKind,
  name : String,
) -> Unit raise {
  if key is Some(key) {
    match token_kind_at(tokens, key) {
      Some(kind) if kind == expected => ()
      Some(kind) => fail("expected \{name} to be \{expected}, got \{kind}")
      None => fail("expected \{name} to point at a source token")
    }
  } else {
    fail("expected \{name} token to be recorded")
  }
}

///|
test "iter array token table records iter delimiters" {
  let source =
    #|fn f() -> Unit {
    #|  [| // open
    #|    a, // item
    #|  |] // close
    #|}
    #|
  let (impls, tokens) = parse_impls(source)
  let loc = find_iter_array_loc(impls)
  let table = Mapper::new(tokens, impls).get_table(loc)
  assert_token_kind(
    tokens,
    table.get_token("lbracket"),
    TK_LBRACKET_BAR,
    "lbracket",
  )
  assert_token_kind(tokens, table.get_array("comma")[0], TK_COMMA, "comma")
  assert_token_kind(
    tokens,
    table.get_token("rbracket"),
    TK_BAR_RBRACKET,
    "rbracket",
  )
}
