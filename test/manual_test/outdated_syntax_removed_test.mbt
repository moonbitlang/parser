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
fn expect_rejected_cases(cases : Array[(String, String)]) -> Unit raise Error {
  let failures = StringBuilder()
  let mut failed = false
  for case in cases {
    let (source, name) = case
    let (_, hand_reports) = @parser.parse_string(
      source,
      name~,
      parser=Handrolled,
    )
    if hand_reports.is_empty() {
      failed = true
      failures.write_string("handrolled accepted \{name}\n") |> ignore
    }
    let (_, yacc_reports) = @parser.parse_string(source, name~, parser=MoonYacc)
    if yacc_reports.is_empty() {
      failed = true
      failures.write_string("moonyacc accepted \{name}\n") |> ignore
    }
    let cst_result = @untyped_cst.parse_structure(source, name~)
    if cst_result.diagnostics_view().is_empty() {
      failed = true
      failures.write_string("CST accepted \{name}\n") |> ignore
    }
  }
  if failed {
    fail(failures.to_string())
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

///|
test "deprecated syntax rejection matrix" {
  expect_rejected_cases([
    ("fn f() { try? g() }", "try_question_removed.mbt"),
    ("fn f() { for { break } }", "dead_for_removed.mbt"),
    ("fn f() { let _ = [for => 1] }", "dead_list_comprehension_removed.mbt"),
    ("fn f() { loop 0 { value => value } }", "functional_loop_removed.mbt"),
    ("fn f() { loop 0 { value => value } else { 0 } }", "loop_else_removed.mbt"),
    ("fn f() { g!() }", "call_bang_removed.mbt"),
    ("fn f(x : X) { x.g!() }", "method_call_bang_removed.mbt"),
    ("fn f() { 1 catch! { _ => 0 } }", "postfix_catch_bang_removed.mbt"),
    ("fn f() { try 1 catch! { _ => 0 } }", "try_catch_bang_removed.mbt"),
    ("fn f() -> Int! { 0 }", "old_default_error_annotation_removed.mbt"),
    ("fn f() -> Int!E { 0 }", "old_error_annotation_removed.mbt"),
    ("fn f() -> Int?Error { 0 }", "old_maybe_error_annotation_removed.mbt"),
    ("trait T { fn f!(Self) -> Unit }", "trait_method_bang_removed.mbt"),
    (
      "trait T { fn f(Self) -> Unit } impl T for Int with fn f!(self : Int) -> Unit {}",
      "impl_method_bang_removed.mbt",
    ),
    ("fnalias @pkg.f", "fnalias_removed.mbt"),
    ("typealias @pkg.T", "typealias_removed.mbt"),
    ("traitalias @pkg.T", "traitalias_removed.mbt"),
    ("type! E", "type_bang_removed.mbt"),
    ("extern type T", "extern_type_removed.mbt"),
    ("suberror E Int", "single_payload_suberror_removed.mbt"),
    ("fn f() { enum Local { A } }", "local_enum_removed.mbt"),
  ])
  let source = "fn f() { try? g() }"
  let removed_hint = "Deprecated `try?` syntax is not supported; use `try!` or `try ... catch` instead."
  let (_, hand_reports) = @parser.parse_string(
    source,
    name="try_question_removed.mbt",
    parser=Handrolled,
  )
  if hand_reports.is_empty() {
    fail("handrolled parser unexpectedly accepted try_question_removed.mbt")
  }
  for report in hand_reports {
    if report.msg == removed_hint {
      fail("handrolled parser emitted the removed `try?` diagnostic")
    }
  }
  let cst_reports = @untyped_cst.parse_structure(
    source,
    name="try_question_removed.mbt",
  ).diagnostics_view()
  if cst_reports.is_empty() {
    fail("CST parser unexpectedly accepted try_question_removed.mbt")
  }
  for report in cst_reports {
    if report.msg == removed_hint {
      fail("CST parser emitted the removed `try?` diagnostic")
    }
  }
}

///|
test "modern replacements and retained underscore contexts are accepted" {
  expect_accepted("fn f() { try! g() }", "try_exclamation_retained.mbt")
  expect_accepted(
    "fn f() { for ;; { break } }", "explicit_dead_for_retained.mbt",
  )
  expect_accepted(
    "fn f() { let values = [for ;; => 1] }", "explicit_dead_list_comprehension_retained.mbt",
  )
  expect_accepted(
    (
      #|trait T {
      #|  fn default(Self) -> Unit = _
      #|}
      #|fn f(_ : _, value : Int) {
      #|  let _ = value
      #|  let arrow = _ => value
      #|  match value { _ => arrow(()) }
      #|}
    ),
    "retained_underscore_contexts.mbt",
  )
  expect_accepted(
    (
      #|fn f() {
      #|  for i = 0; i < 1; i = i + 1 {} where {
      #|    proof_reasoning: "done"
      #|  }
      #|}
    ),
    "proof_reasoning_retained.mbt",
  )
  expect_accepted(
    (
      #|#callsite(autofill(args_loc))
      #|fn f(args_loc~ : ArgsLoc) {}
    ),
    "callsite_autofill_retained.mbt",
  )
  expect_accepted(
    (
      #|fn f() {
      #|  for i = 0; i < 1; i = (if true { 1 } else { 2 }) {}
      #|  for i = 0; i < 1; i = (x => x) {}
      #|  for x in (if true { [1] } else { [2] }) {}
      #|  for x in (y => y) {}
      #|}
    ),
    "parenthesized_loop_header_exprs_retained.mbt",
  )
  expect_accepted(
    (
      #|let top =
      #|  #|top
      #|fn f(writer : Writer) {
      #|  let local =
      #|    #|local
      #|  let mut mutable =
      #|    #|mutable
      #|  let grouped = (
      #|    #|grouped
      #|  )
      #|  writer <+
      #|    #|template
      #|  for ;; {} where {
      #|    proof_reasoning:
      #|      #|reasoning
      #|  }
      #|}
    ),
    "allowed_multiline_contexts.mbt",
  )
  expect_accepted("#deprecated\nfn f() {}", "deprecated_attribute_retained.mbt")
}
