///|
fn expect_rejected(source : String, name : String) -> Unit raise Error {
  let (_, hand_reports) = @parser.parse_string(source, name~, parser=Handrolled)
  if hand_reports.is_empty() {
    fail("handrolled parser unexpectedly accepted \{name}")
  }
  let (_, yacc_reports) = @parser.parse_string(source, name~, parser=MoonYacc)
  if yacc_reports.is_empty() {
    fail("moonyacc parser unexpectedly accepted \{name}")
  }
  expect_cst_rejected(source, name)
}

///|
fn expect_accepted(source : String, name : String) -> Unit raise Error {
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    name~,
    parser=Handrolled,
  )
  if !hand_reports.is_empty() {
    fail("handrolled parser unexpectedly rejected \{name}")
  }
  let (yacc_impls, yacc_reports) = @parser.parse_string(
    source,
    name~,
    parser=MoonYacc,
  )
  if !yacc_reports.is_empty() {
    fail("moonyacc parser unexpectedly rejected \{name}")
  }
  let hand_json = hand_impls.map(impl_ => impl_.json_repr()).to_json()
  let yacc_json = yacc_impls.map(impl_ => impl_.json_repr()).to_json()
  let cst_json = cst_impls_json(source, name)
  if hand_json != yacc_json || hand_json != cst_json {
    fail(
      "parsing \{name} produced different ASTs between moonyacc, handrolled, and CST parsers",
    )
  }
}

///|
test "outdated syntax is rejected" {
  let local_type_source =
    #|fn main {
    #|  struct Local {
    #|    value : Int
    #|  }
    #|}
  expect_rejected(local_type_source, "local_type_decl_removed.mbt")
  expect_rejected("fn main!() { () }", "fn_bang_removed.mbt")
  expect_rejected("fn f[T]() { () }", "post_name_generic_removed.mbt")
  let local_fn_bang_source =
    #|fn main {
    #|  fn f!() { () }
    #|}
  expect_rejected(local_fn_bang_source, "local_fn_bang_removed.mbt")
  let anonymous_fn_bang_source =
    #|fn main {
    #|  let f = fn!() { () }
    #|}
  expect_rejected(anonymous_fn_bang_source, "anonymous_fn_bang_removed.mbt")
  expect_rejected("type lower", "lowercase_type_decl_removed.mbt")
  expect_rejected("struct lower { }", "lowercase_struct_decl_removed.mbt")
  expect_rejected("enum lower { A }", "lowercase_enum_decl_removed.mbt")
  expect_rejected("trait lower { }", "lowercase_trait_decl_removed.mbt")
  expect_rejected("type Box[lower]", "lowercase_type_param_removed.mbt")
  expect_rejected(
    "fn f(x : lower) { () }", "lowercase_type_reference_removed.mbt",
  )
  expect_rejected(
    "fn f(x : &lower) { () }", "lowercase_object_type_removed.mbt",
  )
  expect_rejected(
    "fn f() raise lower { () }", "lowercase_error_type_removed.mbt",
  )
  expect_rejected(
    "extenum @pkg.lower += { A }", "lowercase_extenum_extension_target_removed.mbt",
  )
  expect_rejected(
    (
      #|fn f(s : String) {
      #|  let _ = s lexmatch? "a"
      #|}
    ),
    "lexmatch_question_removed.mbt",
  )
  expect_rejected(
    (
      #|fn f(s : String) {
      #|  let _ = s lexmatch? "a" with longest
      #|}
    ),
    "lexmatch_question_with_strategy_removed.mbt",
  )
  expect_rejected(
    (
      #|fn f(s : String, x : String) {
      #|  let _ = s lexmatch? re"a\{x}"
      #|}
    ),
    "lexmatch_question_interpolation_removed.mbt",
  )
  expect_rejected(
    "struct S { fn new() -> S }", "struct_constructor_removed.mbt",
  )
  expect_rejected(
    "struct S { value : Int; fn new(value : Int) -> S }", "struct_constructor_after_field_removed.mbt",
  )
  expect_accepted(
    "fn f(x : @pkg.lower) { () }", "qualified_lowercase_type_reference_retained.mbt",
  )
  expect_accepted(
    (
      #|fn f(s : String) {
      #|  lexmatch s { _ => () }
      #|}
    ),
    "lexmatch_expression_retained.mbt",
  )
  expect_accepted(
    (
      #|const DIGIT = re"[0-9]"
      #|fn f(s : String) {
      #|  lexmatch s {
      #|    DIGIT => ()
      #|    _ => ()
      #|  }
      #|}
    ),
    "lexmatch_constant_reference_retained.mbt",
  )
  expect_accepted(
    (
      #|fn f(s : String) {
      #|  let _ = s =~ re"a"
      #|}
    ),
    "regex_match_expression_retained.mbt",
  )
  expect_accepted(
    (
      #|struct S {}
      #|fn S::new() -> S { ... }
    ),
    "out_of_type_new_retained.mbt",
  )
}
