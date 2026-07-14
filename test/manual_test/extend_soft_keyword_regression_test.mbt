///|
test "extend soft keyword in remaining lower identifier positions" {
  let source =
    #|using @pkg { extend, extend as extend_alias }
    #|
    #|pub(extend) fn g() -> Unit {}
    #|
    #|fn f(x : X, s : String, token : T) -> Unit {
    #|  x..extend()
    #|  s =~ (re"[a-z]+", extend=token)
    #|}
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    name="extend_soft_keyword_regression.mbt",
    parser=Handrolled,
  )
  let (yacc_impls, yacc_reports) = @parser.parse_string(
    source,
    name="extend_soft_keyword_regression.mbt",
    parser=MoonYacc,
  )
  if !hand_reports.is_empty() || !yacc_reports.is_empty() {
    fail(
      "soft keyword extend should parse in all lowercase identifier positions; hand=\{hand_reports.to_json().stringify(indent=2)}, yacc=\{yacc_reports.to_json().stringify(indent=2)}",
    )
  }
  let hand_json = hand_impls.map(impl_ => impl_.json_repr()).to_json()
  let yacc_json = yacc_impls.map(impl_ => impl_.json_repr()).to_json()
  if hand_json != yacc_json {
    fail(
      "soft keyword extend produced different ASTs between moonyacc and handrolled parsers",
    )
  }
}

///|
test "extend special constructor pattern location matches yacc" {
  let source =
    #|fn f(x : X) -> Unit {
    #|  match x {
    #|    extend(value) => ignore(value)
    #|  }
    #|}
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    name="extend_special_constructor_pattern_loc.mbt",
    parser=Handrolled,
  )
  let (yacc_impls, yacc_reports) = @parser.parse_string(
    source,
    name="extend_special_constructor_pattern_loc.mbt",
    parser=MoonYacc,
  )
  if !hand_reports.is_empty() || !yacc_reports.is_empty() {
    fail(
      "extend special constructor pattern should parse; hand=\{hand_reports.to_json().stringify(indent=2)}, yacc=\{yacc_reports.to_json().stringify(indent=2)}",
    )
  }
  let hand_json = @basic.show_loc.protect(String, fn() {
    hand_impls.map(impl_ => impl_.json_repr()).to_json()
  })
  let yacc_json = @basic.show_loc.protect(String, fn() {
    yacc_impls.map(impl_ => impl_.json_repr()).to_json()
  })
  if hand_json != yacc_json {
    fail(
      "extend special constructor pattern location differs between moonyacc and handrolled parsers:\nhand=\{hand_json.stringify(indent=2)}\nyacc=\{yacc_json.stringify(indent=2)}",
    )
  }
}
