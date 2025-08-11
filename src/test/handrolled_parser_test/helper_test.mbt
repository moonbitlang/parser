///|
fn @test.T::run(t : Self, name : String) -> Unit raise Error {
  let source_file = "src/test/handrolled_parser_test/fixtures/\{name}.mbt"
  let (impls, diag) = @parser.parse_file(source_file, parser=Handrolled) catch {
    err => fail("failed to read file \{name}.mbt: \{err}")
  }
  let json : Json = if diag is [] {
    { "type": "Program", "body": impls }
  } else {
    {
      "type": "Program",
      "body": impls,
      "diagnostics": @basic.show_loc.protect(true, () => diag.to_json()),
    }
  }
  t.write(json.stringify(indent=2))
  t.snapshot(filename="\{name}.mbt.json")
}
