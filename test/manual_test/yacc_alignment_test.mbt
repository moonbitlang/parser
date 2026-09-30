// Syntax aligned with the upstream `parsing_parser.mly` grammar that is only
// implemented by the moonyacc parser so far.

///|
fn expect_yacc_rejected(source : String) -> Unit raise {
  let (_, reports) = @parser.parse_string(source, parser=MoonYacc)
  if reports.is_empty() {
    fail("moonyacc parser unexpectedly accepted:\n\{source}")
  }
}

///|
test "yacc: `extend` is a keyword" {
  expect_yacc_rejected("fn f(extend : Int) -> Unit {}")
  expect_yacc_rejected("fn f() -> Unit { let extend = 1 }")
  expect_yacc_rejected("fn f(x : X) -> Unit { x..extend() }")
  expect_yacc_rejected("pub(extend) fn g() -> Unit {}")
}

///|
test "yacc: array elements cannot be bare `for` loops" {
  expect_yacc_rejected("fn f() -> Unit { let _ = [for x in xs { }] }")
}
