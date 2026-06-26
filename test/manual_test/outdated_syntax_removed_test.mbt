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
}

///|
fn expect_accepted(source : String, name : String) -> Unit raise Error {
  let (_, hand_reports) = @parser.parse_string(source, name~, parser=Handrolled)
  if !hand_reports.is_empty() {
    fail("handrolled parser unexpectedly rejected \{name}")
  }
  let (_, yacc_reports) = @parser.parse_string(source, name~, parser=MoonYacc)
  if !yacc_reports.is_empty() {
    fail("moonyacc parser unexpectedly rejected \{name}")
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
  expect_accepted(
    "fn f(x : @pkg.lower) { () }", "qualified_lowercase_type_reference_retained.mbt",
  )
}
