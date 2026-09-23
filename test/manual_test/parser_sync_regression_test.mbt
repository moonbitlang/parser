///|
fn assert_parser_sync(source : String, name : String) -> Unit raise Error {
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    name~,
    parser=Handrolled,
  )
  if !hand_reports.is_empty() {
    fail(
      "handrolled parser rejected \{name}: \{hand_reports.to_json().stringify(indent=2)}",
    )
  }
  let (yacc_impls, yacc_reports) = @parser.parse_string(
    source,
    name~,
    parser=MoonYacc,
  )
  if !yacc_reports.is_empty() {
    fail(
      "moonyacc parser rejected \{name}: \{yacc_reports.to_json().stringify(indent=2)}",
    )
  }
  let hand_json = hand_impls.map(impl_ => impl_.json_repr()).to_json()
  let yacc_json = yacc_impls.map(impl_ => impl_.json_repr()).to_json()
  let cst_json = cst_impls_json(source, name)
  if hand_json != yacc_json || hand_json != cst_json {
    fail(
      "\{name} produced different ASTs between moonyacc, handrolled, and CST parsers",
    )
  }
}

///|
test "foreach binders accept a trailing comma" {
  let source =
    #|fn foreach_trailing(xs, pairs) {
    #|  for x, in xs {}
    #|  for x, y, in pairs {}
    #|  let array_single = [for x, in xs => x]
    #|  let array_multiple = [for x, y, in pairs => (x, y)]
    #|  let iter_single = [|for x, in xs => x|]
    #|  let iter_multiple = [|for x, y, in pairs => (x, y)|]
    #|}
  assert_parser_sync(source, "foreach_trailing_comma.mbt")
}

///|
test "object method references start optional expressions" {
  let source =
    #|fn return_method_ref() {
    #|  return &I::method()
    #|}
    #|fn break_method_ref() {
    #|  while true {
    #|    break &I::method()
    #|  }
    #|}
    #|fn continue_method_ref() {
    #|  while true {
    #|    continue &I::method()
    #|  }
    #|}
    #|fn for_condition_method_ref() {
    #|  for ; &I::method(); {}
    #|}
    #|fn slice_method_ref(xs) {
    #|  let _ = xs[:&I::method()]
    #|  let _ = xs[0:&I::method()]
    #|}
  assert_parser_sync(source, "optional_object_method_ref.mbt")
}

///|
fn collect_regex_literals(
  pat : @syntax.RegexPattern,
  literals : Array[(String, Bool)],
) -> Unit {
  match pat {
    Literal(lit~, recovered_~, loc=_) => literals.push((lit, recovered_))
    Reference(_) => ()
    Sequence(pat1~, pat2~, loc=_) | Alternation(pat1~, pat2~, loc=_) => {
      collect_regex_literals(pat1, literals)
      collect_regex_literals(pat2, literals)
    }
    Alias(pat~, binder=_, loc=_) => collect_regex_literals(pat, literals)
  }
}

///|
fn regex_literals_from_impls(impls : @syntax.Impls) -> Array[(String, Bool)] {
  let literals : Array[(String, Bool)] = []
  for impl_ in impls {
    match impl_ {
      TopLetDef(expr=RegexMatch(pat~, ..), ..) =>
        collect_regex_literals(pat, literals)
      TopLetDef(expr=LexScan(cases~, ..), ..) =>
        for case in cases {
          match case.pat {
            Pattern(pat) => collect_regex_literals(pat, literals)
            Binder(_) | Wildcard(_) => ()
          }
        }
      _ => ()
    }
  }
  literals
}

///|
test "regular regex literals are not recovered" {
  let source = "let regular = subject =~ re\"regular\""
  assert_parser_sync(source, "regular_regex_literal.mbt")
  let (hand_impls, _) = @parser.parse_string(
    source,
    name="regular_regex_literal.mbt",
    parser=Handrolled,
  )
  let (yacc_impls, _) = @parser.parse_string(
    source,
    name="regular_regex_literal.mbt",
    parser=MoonYacc,
  )
  let cst = @untyped_cst.parse_structure(
    source,
    name="regular_regex_literal.mbt",
  )
  let (cst_impls, cst_reports) = cst.to_impls()
  if !cst_reports.is_empty() {
    fail("CST lowering rejected a regular regex literal")
  }
  @test.assert_eq(regex_literals_from_impls(hand_impls), [("regular", false)])
  @test.assert_eq(regex_literals_from_impls(yacc_impls), [("regular", false)])
  @test.assert_eq(regex_literals_from_impls(cst_impls), [("regular", false)])
}
