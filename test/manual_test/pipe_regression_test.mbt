///|
test "source parsers reject mixed and chained reverse pipes" {
  for
    source in [
      "fn f() { a |> b <| c }", "fn f() { a <| b |> c }", "fn f() { a <| b <| c }",
      "fn run() { f() <| x => x + 1 }", "fn run() { f() <| x => g(x, y) }",
    ] {
    let (_, hand_reports) = @parser.parse_string(source, parser=Handrolled)
    let (_, yacc_reports) = @parser.parse_string(source, parser=MoonYacc)
    let cst = @untyped_cst.parse_structure(source)
    if hand_reports.is_empty() ||
      yacc_reports.is_empty() ||
      cst.diagnostics_view().is_empty() {
      fail("a source parser accepted an invalid pipe expression: \{source}")
    }
  }
}

///|
test "source parsers keep valid pipe combinations" {
  for
    source in [
      "fn f() { a |> b |> c }", "fn f() { a <| b }", "fn f() { (a |> b) <| c }",
      "fn f() { (a <| b) |> c }", "fn run() { f() <| x => { x + 1 } }", "fn run() { f() <| (x, y) => { x + y } }",
    ] {
    assert_parser_sync(source, "valid_pipe.mbt")
  }
}
