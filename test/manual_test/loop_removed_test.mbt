///|
test "loop expression is rejected" {
  let source = "fn main { loop 0 { _ => 0 } }"
  let (_, hand_reports) = @parser.parse_string(
    source,
    name="loop_removed.mbt",
    parser=Handrolled,
  )
  if hand_reports.is_empty() {
    fail("handrolled parser unexpectedly accepted loop expression")
  }
  let (_, yacc_reports) = @parser.parse_string(
    source,
    name="loop_removed.mbt",
    parser=MoonYacc,
  )
  if yacc_reports.is_empty() {
    fail("moonyacc parser unexpectedly accepted loop expression")
  }
  expect_cst_rejected(source, "loop_removed.mbt")
}
