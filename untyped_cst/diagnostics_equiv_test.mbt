///|
fn diagnostics_equiv_reports_from_view(
  view : ArrayView[@basic.Report],
) -> Array[@basic.Report] {
  let reports : Array[@basic.Report] = []
  for report in view {
    reports.push(report)
  }
  reports
}

///|
fn diagnostics_equiv_reports_json(reports : Array[@basic.Report]) -> Json {
  @basic.show_loc.protect(@basic.Json, fn() { reports.to_json() })
}

///|
fn diagnostics_equiv_assert_reports(
  label : String,
  source : String,
  accepted : Bool,
  hand : Array[@basic.Report],
  actual : Array[@basic.Report],
) -> Unit raise {
  let hand_json = diagnostics_equiv_reports_json(hand)
  let actual_json = diagnostics_equiv_reports_json(actual)
  let actual_accepted = actual.is_empty()
  if hand_json != actual_json || actual_accepted != accepted {
    fail(
      StringBuilder::new()
      ..write_string("diagnostics mismatch for ")
      ..write_string(label)
      ..write_string(": expected accepted=")
      ..write_string(accepted.to_string())
      ..write_string("\nsource:\n")
      ..write_string(source)
      ..write_string("\nhandrolled:\n")
      ..write_string(hand_json.stringify(indent=2))
      ..write_string("\nCST:\n")
      ..write_string(actual_json.stringify(indent=2))
      .to_string(),
    )
  }
}

///|
fn assert_structure_acceptance(
  label : String,
  source : String,
  accepted : Bool,
) -> Unit raise {
  let (_, hand_reports) = @parser.parse_string(
    source,
    name="diagnostics_equiv.mbt",
    parser=@parser.Handrolled,
  )
  let cst = @untyped_cst.parse_structure(source, name="diagnostics_equiv.mbt")
  let cst_reports = diagnostics_equiv_reports_from_view(cst.diagnostics_view())
  diagnostics_equiv_assert_reports(
    label, source, accepted, hand_reports, cst_reports,
  )
  if cst.root().text(source) != source[:] {
    fail("source text mismatch for \{label}")
  }
}

///|
fn assert_expression_acceptance(
  label : String,
  source : String,
  accepted : Bool,
) -> Unit raise {
  let lex = @lexer.tokens_from_string(
    source,
    name="diagnostics_equiv.mbt",
    comment=true,
  )
  let (_, hand_reports) = @handrolled_parser.parse_expr(lex.tokens)
  let cst = @untyped_cst.parse_expression(source, name="diagnostics_equiv.mbt")
  let cst_reports = diagnostics_equiv_reports_from_view(cst.diagnostics_view())
  diagnostics_equiv_assert_reports(
    label, source, accepted, hand_reports, cst_reports,
  )
  if cst.root().text(source) != source[:] {
    fail("source text mismatch for \{label}")
  }
}

///|
test "structure diagnostics come from CST parser recovery" {
  let cases = [
    ("missing parameter close", "fn f("),
    ("bad leading parameter separator", "fn f(, x)"),
    ("missing let pattern", "let = 1"),
    ("top-level recovery", "fn f(\nlet y = 1"),
    ("invalid question in block", "fn f() { ? }"),
    ("removed struct constructor", "struct S { fn new() -> S }"),
  ]
  for case in cases {
    let (label, source) = case
    assert_structure_acceptance(label, source, false)
  }
}

///|
test "expression diagnostics come from CST parser recovery" {
  let cases = [
    ("missing call close", "f(1, 2"),
    ("missing block close", "{ let x = 1; x"),
    ("loop expression", "loop { 1 }"),
    ("throw expression", "throw err"),
    ("removed lexmatch question", "s lexmatch? \"a\""),
  ]
  for case in cases {
    let (label, source) = case
    assert_expression_acceptance(label, source, false)
  }
  assert_expression_acceptance("line break separator", "1\n2", true)
}

///|
test "acceptance equivalence: labelled arguments" {
  let invalid = [
    ("uppercase label", "f(X=1)"),
    ("uppercase optional label", "f(X?=1)"),
  ]
  for case in invalid {
    let (label, source) = case
    assert_expression_acceptance(label, source, false)
  }
  let valid = [
    ("lowercase label", "f(x=1)"),
    ("lowercase optional label", "f(x?=1)"),
    ("label puns", "f(x~, y?)"),
  ]
  for case in valid {
    let (label, source) = case
    assert_expression_acceptance(label, source, true)
  }
}

///|
test "acceptance equivalence: delimited lists" {
  let invalid = [
    ("enum separator", "enum E { A B }"),
    ("struct separator", "struct S { x : Int y : Int }"),
    ("trait separator", "trait T { f(Self) g(Self) }"),
    ("using separator", "using @p { x y }"),
    ("parameter separator", "fn f(x : Int y : Int) {}"),
    ("type parameter separator", "type T[A B]"),
    ("derive separator", "type T derive(Eq Show)"),
    ("empty tuple struct", "struct S()"),
    ("top-level separator", "fn f() {} fn g() {}"),
  ]
  for case in invalid {
    let (label, source) = case
    assert_structure_acceptance(label, source, false)
  }
  let valid = [
    ("enum semicolon", "enum E { A; B }"),
    ("struct semicolon", "struct S { x : Int; y : Int }"),
    ("trait semicolon", "trait T { f(Self); g(Self) }"),
    ("using comma", "using @p { x, y }"),
    ("parameter comma", "fn f(x : Int, y : Int) {}"),
    ("type parameter comma", "type T[A, B]"),
    ("derive comma", "type T derive(Eq, Show)"),
    ("nonempty tuple struct", "struct S(Int)"),
    ("top-level semicolon", "fn f() {}; fn g() {}"),
  ]
  for case in valid {
    let (label, source) = case
    assert_structure_acceptance(label, source, true)
  }
}

///|
test "acceptance equivalence: declaration completion" {
  let invalid = [
    ("bare fn", "fn"),
    ("fn name only", "fn f"),
    ("fn parameters only", "fn f()"),
    ("bare let", "let"),
    ("let pattern only", "let x"),
    ("const name only", "const X"),
    ("bare impl", "impl"),
    ("impl trait only", "impl Trait"),
    ("bare test", "test"),
    ("bare extern", "extern"),
    ("type bang", "type! E"),
    ("numeric function name", "fn 1() {}"),
    ("numeric function parameter", "fn f(1) {}"),
    ("missing return type", "fn f() -> {}"),
    ("using without body", "using @p"),
    ("declare using", "declare using @p { x }"),
    ("extern without language", "extern fn f() = \"x\""),
    ("extern type", "extern \"C\" type T"),
  ]
  for case in invalid {
    let (label, source) = case
    assert_structure_acceptance(label, source, false)
  }
  let valid = [
    ("function body", "fn f() {}"),
    ("function stub", "fn f() = \"x\""),
    ("declare function", "declare fn f()"),
    ("declare function return type", "declare fn f() -> Int"),
    ("declare method return type", "declare fn T::f(x : T) -> Int"),
    ("declare function error annotation", "declare fn f() -> Int noraise"),
    ("let value", "let x = 1"),
    ("constant value", "const X = 1"),
    ("impl relation", "impl Trait for T"),
    ("impl method", "impl Trait with f(self) { self }"),
    ("test body", "test {}"),
    ("extern function", "extern \"C\" fn f() = \"x\""),
    ("using body", "using @p {}"),
  ]
  for case in valid {
    let (label, source) = case
    assert_structure_acceptance(label, source, true)
  }
}

///|
test "acceptance equivalence: declaration constraints" {
  let invalid = [
    ("type parameter constraint", "type T[A : Eq]"),
    ("suberror type parameter", "suberror E[A]"),
    ("enum label without tilde", "enum E { A(x : Int) }"),
    ("destructuring function parameter", "fn f((x, y) : (Int, Int)) { x }"),
  ]
  for case in invalid {
    let (label, source) = case
    assert_structure_acceptance(label, source, false)
  }
  let valid = [
    ("plain type parameter", "type T[A]"),
    ("function type constraint", "fn[A : Eq] f(x : A) {}"),
    ("impl type constraint", "impl[A : Eq] Trait for T[A]"),
    ("plain suberror", "suberror E"),
    ("enum labelled parameter", "enum E { A(x~ : Int) }"),
    (
      "supported function parameters", "fn f(x : Int, y~ : Int, z? : Int, _ : Int) {}",
    ),
  ]
  for case in valid {
    let (label, source) = case
    assert_structure_acceptance(label, source, true)
  }
}

///|
test "acceptance equivalence: expressions patterns and types" {
  let invalid_exprs = [
    ("regex pattern", "match x { re\"a\" => 0 }"),
    ("regex map pattern key", "match x { {re\"a\" : y} => y }"),
    ("top-level typed pattern", "match x { y : Int => y }"),
    ("ellipsis array elements", "[...a, ...b]"),
    ("dotdot range", "1 .. 2"),
    ("leading negative map key", "{-1 : x}"),
    ("late record spread", "{x, ..r}"),
    ("try without catch", "try f()"),
    ("raise call argument", "f(raise e)"),
    ("non-block else", "if x {} else 1"),
    ("parenthesized foreach binder", "for (x) in xs {}"),
    ("removed for else", "for x in xs {} else {}"),
    ("unparenthesized typed arrow", "x : Int => x"),
    ("unbraced pipeline arrow", "a |> x => x"),
    ("uppercase lex alias binder", "lexmatch s { (re\"a\" as X) => 0 }"),
    ("missing lex alias binder", "lexmatch s { (re\"a\" as) => 0 }"),
  ]
  for case in invalid_exprs {
    let (label, source) = case
    assert_expression_acceptance(label, source, false)
  }
  let invalid_types = [
    ("unparenthesized function type", "type T = Int -> Int"),
    ("async non-function type", "type T = async Int -> Int"),
    ("object type literal", "type T = {a : Int}"),
    ("standalone unit type", "type T = ()"),
    ("parameterized error type", "fn f() -> Unit raise E[Int] {}"),
    ("object error type", "fn f() -> Unit raise &E {}"),
  ]
  for case in invalid_types {
    let (label, source) = case
    assert_structure_acceptance(label, source, false)
  }
  let valid_exprs = [
    ("lex regex pattern", "lexmatch s { re\"a\" => 0 }"),
    ("parenthesized typed pattern", "match x { (y : Int) => y }"),
    ("array spreads", "[..a, ..b]"),
    ("record update", "{..r, x: 1}"),
    ("try catch", "try f() catch { _ => 0 }"),
    ("block else", "if x {} else {}"),
    ("nobreak", "for x in xs {} nobreak {}"),
    ("parenthesized typed arrow", "(x : Int) => x"),
    ("braced pipeline arrow", "a |> x => { x }"),
    ("later negative map key", "{\"x\": 1, -1: 2}"),
    ("lowercase lex alias binder", "lexmatch s { (re\"a\" as x) => 0 }"),
    ("standalone raise statement", "raise e"),
    ("standalone return statement", "return e"),
    ("standalone defer statement", "defer f()"),
    ("standalone let statement", "let x = 1"),
    ("standalone todo statement", "..."),
    ("statement expressions in block", "{ raise e; defer f(); () }"),
  ]
  for case in valid_exprs {
    let (label, source) = case
    assert_expression_acceptance(label, source, true)
  }
  let valid_types = [
    ("parenthesized function type", "type T = (Int) -> Int"),
    ("async function type", "type T = async (Int) -> Int"),
    ("zero argument function type", "type T = () -> Int"),
    ("tuple type", "type T = (Int, String)"),
    ("named object type", "type T = &E"),
    ("simple error type", "fn f() -> Unit raise E {}"),
  ]
  for case in valid_types {
    let (label, source) = case
    assert_structure_acceptance(label, source, true)
  }
}

///|
test "diagnostics equivalence: reported declaration boundary cases" {
  let cases = [
    ("impl missing self type", "impl Trait for"),
    (
      "impl method after missing self type", "impl Trait for with f(self) { self }",
    ),
    ("enumview missing source type", "enumview V { C } for with view(x) { x }"),
    ("type alias missing target", "type T ="),
    ("trait missing supertrait", "trait T : {}"),
    ("let annotation missing type", "let x : = 1"),
    ("parameter annotation missing type", "fn f(x~ : = 1) {}"),
  ]
  for case in cases {
    let (label, source) = case
    assert_structure_acceptance(label, source, false)
  }
}

///|
test "diagnostics equivalence: reported expression boundary cases" {
  let cases = [
    ("catch missing block", "try f() catch"),
    ("try missing body", "try catch { _ => 0 }"),
    ("if missing body", "if x"),
    ("while missing body", "while x"),
    ("uppercase lex alias", "lexmatch s { re\"a\" as X => 0 }"),
    ("missing lex alias", "lexmatch s { re\"a\" as => 0 }"),
    ("else missing block", "if x {} else"),
  ]
  for case in cases {
    let (label, source) = case
    assert_expression_acceptance(label, source, false)
  }
}

///|
test "declaration truncation matrix: functions and bindings" {
  let invalid = [
    ("function body at EOF", "fn f()"),
    ("function body after newline", "fn f()\n"),
    ("function body before next declaration", "fn f()\nlet y = 1"),
    ("return type at EOF", "fn f() ->"),
    ("return type after newline", "fn f() ->\n"),
    ("return type before body", "fn f() -> {}"),
    ("parameter type before close", "fn f(x :) {}"),
    ("parameter type after newline", "fn f(x :\n) {}"),
    ("parameter type before default", "fn f(x~ : = 1) {}"),
    ("let pattern at EOF", "let"),
    ("let pattern after newline", "let\n"),
    ("let pattern before equal", "let = 1"),
    ("let value at EOF", "let x ="),
    ("let value after newline", "let x =\n"),
    ("let value before next declaration", "let x =\nfn f() {}"),
    ("constant name at EOF", "const"),
    ("constant value at EOF", "const X ="),
    ("constant value before next declaration", "const X =\nfn f() {}"),
  ]
  for case in invalid {
    let (label, source) = case
    assert_structure_acceptance(label, source, false)
  }
  let valid = [
    ("function body", "fn f() {}"),
    ("function return type", "fn f() -> Unit {}"),
    ("typed parameter", "fn f(x : Int) {}"),
    ("let value", "let x = 1"),
    ("constant value", "const X = 1"),
  ]
  for case in valid {
    let (label, source) = case
    assert_structure_acceptance(label, source, true)
  }
}

///|
test "declaration truncation matrix: type declarations" {
  let invalid = [
    ("type name at EOF", "type"),
    ("type target at EOF", "type T ="),
    ("type target after newline", "type T =\n"),
    ("type target before next declaration", "type T =\nfn f() {}"),
    ("struct name at EOF", "struct"),
    ("struct body at EOF", "struct S"),
    ("struct body before next declaration", "struct S\nfn f() {}"),
    ("struct field type", "struct S { x : }"),
    ("enum name at EOF", "enum"),
    ("enum body at EOF", "enum E"),
    ("enum body before next declaration", "enum E\nfn f() {}"),
    ("constructor parameter type", "enum E { A(x~ :) }"),
    ("constructor tag", "enum E { A = }"),
    ("suberror name at EOF", "suberror"),
    ("trait name at EOF", "trait"),
    ("trait body at EOF", "trait T"),
    ("trait body before next declaration", "trait T\nfn f() {}"),
    ("supertrait before body", "trait T : {}"),
    ("trait method parameter type", "trait T { f(x :) }"),
  ]
  for case in invalid {
    let (label, source) = case
    assert_structure_acceptance(label, source, false)
  }
  let valid = [
    ("abstract type", "type T"),
    ("type alias", "type T = Int"),
    ("record struct", "struct S { x : Int }"),
    ("enum", "enum E { A }"),
    ("suberror", "suberror E"),
    ("trait", "trait T { f(Self) }"),
  ]
  for case in valid {
    let (label, source) = case
    assert_structure_acceptance(label, source, true)
  }
}

///|
test "declaration truncation matrix: impl and enumview" {
  let invalid = [
    ("impl trait at EOF", "impl"),
    ("impl relation marker at EOF", "impl Trait"),
    ("impl self type at EOF", "impl Trait for"),
    ("impl self type before with", "impl Trait for with f(self) { self }"),
    ("impl method name", "impl Trait with"),
    ("impl method parameters", "impl Trait with f"),
    ("impl method body", "impl Trait with f(self)"),
    ("impl method return type", "impl Trait with f(self) -> {}"),
    ("enumview name", "enumview"),
    ("enumview constructors", "enumview V"),
    ("enumview for marker", "enumview V { C }"),
    ("enumview source type", "enumview V { C } for"),
    (
      "enumview source type before with", "enumview V { C } for with view(x) { x }",
    ),
    ("enumview with marker", "enumview V { C } for T"),
    ("enumview method name", "enumview V { C } for T with"),
    ("enumview method parameters", "enumview V { C } for T with view"),
    ("enumview body", "enumview V { C } for T with view(x)"),
  ]
  for case in invalid {
    let (label, source) = case
    assert_structure_acceptance(label, source, false)
  }
  let valid = [
    ("impl relation", "impl Trait for T"),
    ("impl method", "impl Trait with f(self) { self }"),
    ("enumview", "enumview V { C } for T with view(x) { x }"),
  ]
  for case in valid {
    let (label, source) = case
    assert_structure_acceptance(label, source, true)
  }
}

///|
test "declaration truncation matrix: test using and extern" {
  let invalid = [
    ("test body at EOF", "test"),
    ("test body after newline", "test\n"),
    ("test body before next declaration", "test\nfn f() {}"),
    ("using package at EOF", "using"),
    ("using body at EOF", "using @p"),
    ("using body after newline", "using @p\n"),
    ("using body before next declaration", "using @p\nfn f() {}"),
    ("extern language at EOF", "extern"),
    ("extern function marker", "extern \"C\""),
    ("extern function name", "extern \"C\" fn"),
    ("extern equal", "extern \"C\" fn f()"),
    ("extern stub", "extern \"C\" fn f() ="),
  ]
  for case in invalid {
    let (label, source) = case
    assert_structure_acceptance(label, source, false)
  }
  let valid = [
    ("test", "test {}"),
    ("using", "using @p {}"),
    ("extern", "extern \"C\" fn f() = \"f\""),
  ]
  for case in valid {
    let (label, source) = case
    assert_structure_acceptance(label, source, true)
  }
}
