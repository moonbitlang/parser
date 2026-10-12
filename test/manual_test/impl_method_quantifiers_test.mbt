///|
test "parse impl method quantifiers" {
  let source =
    #|trait Trait {
    #|  method(Self) -> Unit
    #|}
    #|
    #|impl Trait for Int with fn[A] method(self : Int, value : A) -> Unit {
    #|  ignore(value)
    #|}
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    name="impl_method_quantifiers.mbt",
    parser=Handrolled,
  )
  if !hand_reports.is_empty() {
    fail(
      "handrolled parser rejected impl method quantifiers: \{Json(hand_reports).stringify(indent=2)}",
    )
  }
  let (yacc_impls, yacc_reports) = @parser.parse_string(
    source,
    name="impl_method_quantifiers.mbt",
    parser=MoonYacc,
  )
  if !yacc_reports.is_empty() {
    fail(
      "moonyacc parser rejected impl method quantifiers: \{Json(yacc_reports).stringify(indent=2)}",
    )
  }
  let hand_json = Json(hand_impls.map(impl_ => impl_.json_repr()))
  let yacc_json = Json(yacc_impls.map(impl_ => impl_.json_repr()))
  let cst_json = cst_impls_json(source, "impl_method_quantifiers.mbt")
  if hand_json != yacc_json || hand_json != cst_json {
    fail(
      "impl method quantifiers produced different ASTs between moonyacc, handrolled, and CST parsers",
    )
  }
}
