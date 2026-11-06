///|
test "byte regex map keys lexscan and return agree across parsers" {
  let source =
    #|const HEADER : BytesRegex = reb"^GET"
    #|let byte_keys = { reb"\xff": 1 }
    #|fn read(data) {
    #|  let _ = data =~ ((HEADER + reb" " + (reb"[^ ]+" as path)), after=rest)
    #|  match data { {reb"x": value, ..} => value; _ => 0 }
    #|  lexscan data {
    #|    (reb"[a-z]+" as word, after=rest) => word
    #|    _ => ()
    #|  }
    #|  return reb"\x00"
    #|}
  assert_parser_sync(source, "byte_regex_all_contexts.mbt")
}

///|
test "descending ranges preserve endpoint order and original locations" {
  for (op, inclusive) in [(">=..", true), (">..", false)] {
    let source = "fn f(x) { x is 9\{op}1 }"
    assert_parser_sync(source, "descending.mbt")
    let (impls, _) = @parser.parse_string(source)
    guard impls.head()
      is Some(
        TopFuncDef(
          decl_body=DeclBody(expr=Is(pat=Range(lhs~, rhs~, kind~, loc~), ..)),
          ..
        )
      ) else {
      fail("expected a range pattern in a function body")
    }
    guard lhs is Constant(c=Int("9"), ..) && rhs is Constant(c=Int("1"), ..) else {
      fail("descending endpoints were reordered")
    }
    assert_eq(kind is InclusiveReverse, inclusive)
    assert_eq(kind is ExclusiveReverse, !inclusive)
    assert_eq(loc.start.column(), 16)
    assert_eq(loc.end.column(), 18 + op.length())
  }
}

///|
test "byte regex interpolation is lexed but rejected by all source parsers" {
  let source = "fn main { let _ = reb\"a\\{x}b\" }"
  assert_true(@lexer.tokens_from_string(source, comment=true).errors.is_empty())
  for parser in [@parser.Handrolled, @parser.MoonYacc] {
    assert_true(!@parser.parse_string(source, parser~).1.is_empty())
  }
  assert_true(
    !@untyped_cst.parse_structure(source).diagnostics_view().is_empty(),
  )
}

///|
test "unterminated byte regex recovers to a following declaration" {
  let source = "let broken = reb\"abc\nlet intact = 1\n"
  let (hand, reports) = @parser.parse_string(source)
  assert_eq(reports.length(), 1)
  assert_true(
    hand
    .to_array()
    .any(impl_ => impl_ is TopLetDef(binder={ name: "intact", .. }, ..)),
  )
  let cst = @untyped_cst.parse_structure(source)
  assert_eq(cst.diagnostics_view().length(), 1)
  let mut recovered = false
  for (_, child) in cst.root().children {
    if child.kind is Impl_Let &&
      source[child.source_span.0:child.source_span.1].contains("let intact = 1") {
      recovered = true
    }
  }
  assert_true(recovered)
}
