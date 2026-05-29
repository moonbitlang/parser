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
  expect_rejected("struct T ()", "empty_tuple_struct_removed.mbt")
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
}
