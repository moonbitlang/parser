///|
test "parse template object rhs" {
  let source =
    #|fn f(writer : Writer, value : Int) -> Unit {
    #|  writer <+ { "key": value }
    #|  writer <? { "maybe": value }
    #|}
  let (yacc_impls, yacc_reports) = @parser.parse_string(
    source,
    name="template_object_rhs.mbt",
    parser=MoonYacc,
  )
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    name="template_object_rhs.mbt",
    parser=Handrolled,
  )
  if !yacc_reports.is_empty() {
    fail(
      "moonyacc parser rejected template object rhs: \{yacc_reports.to_json().stringify(indent=2)}",
    )
  }
  if !hand_reports.is_empty() {
    fail(
      "handrolled parser rejected template object rhs: \{hand_reports.to_json().stringify(indent=2)}",
    )
  }
  let yacc_json = yacc_impls.map(impl_ => impl_.json_repr()).to_json()
  let hand_json = hand_impls.map(impl_ => impl_.json_repr()).to_json()
  let cst_json = cst_impls_json(source, "template_object_rhs.mbt")
  if yacc_json != hand_json || hand_json != cst_json {
    fail(
      "template object rhs produced different ASTs between moonyacc, handrolled, and CST parsers",
    )
  }
}
