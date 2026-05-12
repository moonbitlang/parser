///|
test "single semicolon for condition is accepted" {
  let source =
    #|fn main(n : Int) {
    #|  for i = 0; i < n {
    #|    ignore(i)
    #|  }
    #|}
  let (_, hand_reports) = @parser.parse_string(
    source,
    name="for_condition.mbt",
    parser=Handrolled,
  )
  if !hand_reports.is_empty() {
    fail("handrolled parser rejected single-semicolon for condition")
  }
  let (_, yacc_reports) = @parser.parse_string(
    source,
    name="for_condition.mbt",
    parser=MoonYacc,
  )
  if !yacc_reports.is_empty() {
    fail("moonyacc parser rejected single-semicolon for condition")
  }
}
