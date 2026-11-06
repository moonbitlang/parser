///|
test "mbtx formatting preserves headers and reaches a fixed point" {
  for
    source in [
      "---\nexperimental=true\n---\nlet r=reb\"[a-z]+\"", "\u{FEFF}----\r\n#cfg(target=\"js\")\r\nimport {\"a/b\"}\r\n-----\r\nlet r=reb\"x\"",
      "---\n---\nlet r=reb\"x\"", "---\nexperimental=true", "---", "let r=reb\"x\"",
    ] {
    let formatted = @fmt.format_mbtx(source, block_line=false)
    let result = @parser.parse_mbtx(formatted)
    assert_eq(result.diagnostics.length(), 0)
    assert_eq(
      result.header is Some(_),
      @parser.parse_mbtx(source).header is Some(_),
    )
    assert_eq(@fmt.format_mbtx(formatted, block_line=false), formatted)
  }
}
