///|
test "source parse reports lexical errors in both parser modes" {
  let source = "fn f() { let _ : Byte = b'\\q' }"
  for parser in [@parser.Parser::Handrolled, MoonYacc] {
    let (_, reports) = @parser.parse_string(
      source,
      name="invalid_byte_escape.mbt",
      parser~,
    )
    if !reports
      .iter()
      .any(fn(report) { report.msg.contains("invalid byte literal") }) {
      fail("lexical error was not reported by a source parser")
    }
  }
}

///|
test "escaped space in char reaches source parse diagnostics" {
  let source = "fn f() { let _ : Char = '\\ ' }"
  for parser in [@parser.Parser::Handrolled, MoonYacc] {
    let (_, reports) = @parser.parse_string(source, parser~)
    if !reports
      .iter()
      .any(fn(report) { report.msg.contains("unrecognized character") }) {
      fail("character escape lexical error was not reported by a source parser")
    }
  }
}
