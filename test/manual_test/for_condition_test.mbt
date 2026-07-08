///|
test "single semicolon for condition is accepted" {
  let source =
    #|fn main(n : Int) {
    #|  for i = 0; i < n {
    #|    ignore(i)
    #|  }
    #|}
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    name="for_condition.mbt",
    parser=Handrolled,
  )
  if !hand_reports.is_empty() {
    fail("handrolled parser rejected single-semicolon for condition")
  }
  let (yacc_impls, yacc_reports) = @parser.parse_string(
    source,
    name="for_condition.mbt",
    parser=MoonYacc,
  )
  if !yacc_reports.is_empty() {
    fail("moonyacc parser rejected single-semicolon for condition")
  }
  let hand_json = hand_impls.map(impl_ => impl_.json_repr()).to_json()
  let yacc_json = yacc_impls.map(impl_ => impl_.json_repr()).to_json()
  let cst_json = cst_impls_json(source, "for_condition.mbt")
  if hand_json != yacc_json || hand_json != cst_json {
    fail(
      "single-semicolon for condition produced different ASTs between moonyacc, handrolled, and CST parsers",
    )
  }
}
