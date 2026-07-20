///|
test "invalid enum delimiter reports actual token" {
  let source = "enum Foo{Bar,Baz} //no trailing newline"
  let tokens = @lexer.tokens_from_string(source, comment=true).tokens
  let (_, reports) = parse(tokens)
  guard reports is [report] else {
    fail("expected exactly one report, got \{reports.length()}")
  }
  inspect(report.loc, content="-1:13-1:14")
  inspect(
    report.msg,
    content="Expecting a newline or `;` here, but encountered `,`.",
  )
}
