///|
fn impls_json(impls : @syntax.Impls) -> Json {
  @basic.show_loc.protect(String, fn() {
    impls.map(impl_ => impl_.json_repr()).to_json()
  })
}

///|
fn expr_json(expr : @syntax.Expr) -> Json {
  @basic.show_loc.protect(String, fn() { expr.json_repr() })
}

///|
fn reports_from_view(view : ArrayView[@basic.Report]) -> Array[@basic.Report] {
  let reports : Array[@basic.Report] = []
  for report in view {
    reports.push(report)
  }
  reports
}

///|
fn fail_parser_reports(
  source : String,
  hand_reports : Array[@basic.Report],
  yacc_reports : Array[@basic.Report],
) -> Unit raise {
  let buf = StringBuilder::new()
  let _ : Unit = @basic.show_loc.protect(String, fn() {
    buf
    ..write_string("parsing focused AST equivalence case failed.\n")
    ..write_string("Source:\n")
    ..write_string(source)
    ..write_string("\nHandrolled parser diagnostics: ")
    ..write_string(hand_reports.to_json().stringify(indent=2))
    ..write_string("\nYacc parser diagnostics: ")
    ..write_string(yacc_reports.to_json().stringify(indent=2))
    .write_string("\n")
  })
  fail(buf.to_string())
}

///|
fn assert_ast_equiv(
  source : String,
  label? : String = "<source>",
) -> Unit raise {
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    parser=@parser.Handrolled,
  )
  let (yacc_impls, yacc_reports) = @parser.parse_string(
    source,
    parser=@parser.MoonYacc,
  )
  let cst_result = @untyped_cst.parse_structure(source)
  if !(hand_reports.is_empty() && yacc_reports.is_empty()) {
    fail_parser_reports(source, hand_reports, yacc_reports)
  }
  if cst_result.diagnostics_view().length() != 0 {
    let reports = reports_from_view(cst_result.diagnostics_view())
    let rendered = @basic.show_loc.protect(String, fn() {
      reports.to_json().stringify(indent=2)
    })
    fail("CST diagnostics for \{label}: \{rendered}")
  }
  if cst_result.root().text(source) != source[:] {
    fail("source text mismatch for \{label}")
  }
  let (cst_impls, conversion_reports) = cst_result.to_impls()
  if conversion_reports.length() != 0 {
    fail("conversion diagnostics for \{label}: \{conversion_reports.length()}")
  }
  if impls_json(cst_impls) != impls_json(hand_impls) {
    fail("handrolled AST mismatch for \{label}")
  }
  if impls_json(cst_impls) != impls_json(yacc_impls) {
    fail("yacc AST mismatch for \{label}")
  }
}

///|
fn assert_handrolled_cst_ast_equiv(source : String) -> Unit raise {
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    parser=@parser.Handrolled,
  )
  let cst_result = @untyped_cst.parse_structure(source)
  if !hand_reports.is_empty() {
    fail("handrolled diagnostics for focused CST equivalence case")
  }
  if cst_result.diagnostics_view().length() != 0 {
    fail("CST diagnostics for focused handrolled equivalence case")
  }
  if cst_result.root().text(source) != source[:] {
    fail("source text mismatch for focused handrolled equivalence case")
  }
  let (cst_impls, conversion_reports) = cst_result.to_impls()
  if conversion_reports.length() != 0 {
    fail("conversion diagnostics for focused handrolled equivalence case")
  }
  if impls_json(cst_impls) != impls_json(hand_impls) {
    fail("handrolled AST mismatch for focused CST equivalence case")
  }
}

///|
fn assert_expr_ast_equiv(source : String) -> Unit raise {
  let lex = @lexer.tokens_from_string(source, comment=true)
  let (expected, expected_reports) = @handrolled_parser.parse_expr(lex.tokens)
  let cst_result = @untyped_cst.parse_expression(source)
  @test.assert_eq(expected_reports.length(), 0)
  @test.assert_eq(cst_result.diagnostics_view().length(), 0)
  let (actual, conversion_reports) = cst_result.to_expr()
  @test.assert_eq(conversion_reports.length(), 0)
  guard actual is Some(expr) else { fail("expected expression for \{source}") }
  @test.assert_eq(expr_json(expr), expr_json(expected))
}

///|
fn read_snapshot(name : String) -> String raise {
  @fs.read_file_to_string("test/sync_test/__snapshot__/\{name}") catch {
    _ => fail("failed to read snapshot \{name}")
  }
}

///|
fn docstring_source() -> String {
  let source =
    #|/// docs for fn f
    #|fn f() { () }
    #|
    #|/// docs for type T
    #|type T
    #|
    #|/// docs for enum E
    #|enum E {
    #|  /// docs for constructor A
    #|  A
    #|}
    #|
    #|/// docs for struct S
    #|struct S {
    #|  /// docs for field x
    #|  x : Int
    #|}
  source
}

///|
test "representative snapshots are AST equivalent" {
  let snapshots = [
    "parse_test_toplevel_test.mbt", "parse_test_fn_exp_test.mbt", "pipeline_test_match_test.mbt",
    "parse_test_async_test.mbt", "parse_test_text_segment_test.mbt", "pipeline_test_pattern_test.mbt",
    "parse_test_pipe_arrow_parse.mbt", "parse_test_return.mbt", "pipeline_test_while_test.mbt",
    "pipeline_test_for.mbt", "parse_test_lexmatch.mbt", "pipeline_test_option.mbt",
    "pipeline_test_builtin_test.mbt", "pipeline_test_array.mbt", "pipeline_test_obj_in_struct.mbt",
    "pipeline_test_record_update_typing.mbt", "parse_test_unicode_test.mbt", "pipeline_test_async_misc.mbt",
    "pipeline_test_record.mbt", "pipeline_test_pipe.mbt", "pipeline_test_foreach.mbt",
    "pipeline_test_return_test.mbt",
  ]
  for snapshot in snapshots {
    assert_ast_equiv(read_snapshot(snapshot), label=snapshot)
  }
}

///|
test "reverse pipe snapshots match handrolled AST" {
  let snapshots = [
    "parse_test_reverse_pipe.mbt", "parse_test_reverse_pipe_error.mbt",
  ]
  for snapshot in snapshots {
    assert_handrolled_cst_ast_equiv(read_snapshot(snapshot))
  }
}

///|
test "focused declaration AST equivalence cases" {
  let cases = [
    "fn f(x : Int) -> Int { x }", "const X = 1", "type T[A] = Array[A]", "struct S { pub mut x : Int }",
    "enum E { A(x~ : Int) }", "trait Show { show(Self) -> String }", "impl Show for S with show(self) { \"S\" }",
    "test { inspect(1, content=\"1\") }", "using @pkg {func as func2}", "type U derive(Eq, @pkg.Interface)",
  ]
  for source in cases {
    assert_ast_equiv(source)
  }
}

///|
test "question-suffixed error type matches handrolled AST" {
  assert_handrolled_cst_ast_equiv("fn f() -> Unit raise E? {}")
}

///|
test "single-element tuple pattern trailing comma matches handrolled AST" {
  assert_expr_ast_equiv("match x { (y,) => y }")
}

///|
test "single-element tuple type trailing comma matches handrolled AST" {
  assert_handrolled_cst_ast_equiv("fn f(x : (Int,)) {}")
}

///|
test "float unary plus matches handrolled AST" {
  assert_expr_ast_equiv("+1.0F")
}

///|
test "qualified trait impl matches handrolled AST" {
  let cases = [
    "impl @debug.Debug for T", "impl @debug.Debug for T with to_repr(self) { self }",
  ]
  for source in cases {
    assert_handrolled_cst_ast_equiv(source)
  }
}

///|
test "impl method error annotations and bodies match handrolled AST" {
  let cases = [
    "impl T for S with f() raise E { 1 }", "impl T for S with f() noraise { 2 }",
  ]
  for source in cases {
    assert_handrolled_cst_ast_equiv(source)
  }
}

///|
test "optional types and existing error annotations remain AST equivalent" {
  let cases = [
    "fn f(x : E?) -> E? { x }", "fn f() -> Unit raise E {}", "fn f() -> Unit raise ? {}",
  ]
  for source in cases {
    assert_ast_equiv(source)
  }
}

///|
test "attributed generic struct optional field type AST equivalence" {
  let source =
    #|#valtype
    #|struct T[A, B] {
    #|  a : (A, B)
    #|  b : A?
    #|  c : FixedArray[B]
    #|} derive(Debug)
  assert_ast_equiv(source)
}

///|
test "source-backed root covers leading and trailing whitespace" {
  assert_ast_equiv("\n  fn spaced() { () }\n\n")
}

///|
test "expression conversion produces expression AST" {
  let source = "{ let mut x = 1; x }"
  let result = @untyped_cst.parse_expression(source)
  @test.assert_eq(result.diagnostics_view().length(), 0)
  let (expr, reports) = result.to_expr()
  @test.assert_eq(reports.length(), 0)
  guard expr is Some(_) else { fail("expected expression") }
}

///|
test "string and labelled argument AST equivalence cases" {
  let cases = [
    "\"hello \\{name}\"", "b\"hello \\{name}\"", "#| Hello\n#| World", "$| hello \\{name}\n#| tail",
    "f(label~)", "f(label?, other?=value)",
  ]
  for source in cases {
    assert_expr_ast_equiv(source)
  }
}

///|
test "special postfix operators bind after range and prefix expressions" {
  let cases = [
    "1 ..< 2 as T", "-x as T", "1 ..< 2 is _", "-x is _", "1 ..< 2 =~ re\"a\"", "-x =~ re\"a\"",
    "1 ..< 2 <+ \"value\"", "-x <+ \"value\"", "1 ..< 2 <? \"value\"", "-x <? \"value\"",
  ]
  for source in cases {
    assert_expr_ast_equiv(source)
  }
}

///|
test "is RHS stops before an unparenthesized OR pattern" {
  assert_expr_ast_equiv("x is 1 | 2")
  assert_expr_ast_equiv("x is (1 | 2)")
}

///|
test "block statement AST equivalence cases" {
  let cases = [
    "{ let mut x = 1; x }", "{ fn f() { 1 }; f() }", "{ letrec f = fn() { 1 }; f() }",
    "{ letrec f = fn() { 1 }\n  and g = fn() { f() }; g() }", "{ guard true else { 0 }; 1 }",
    "{ defer f(); 1 }", "{ proof_assert a == b; 1 }",
  ]
  for source in cases {
    assert_expr_ast_equiv(source)
  }
}

///|
test "location-sensitive expression AST equivalence cases" {
  let cases = [
    "fn f() { let x : Int = 1; x }", "fn f() { (g)(1) }", "fn f() { !x }",
  ]
  for source in cases {
    assert_ast_equiv(source, label=source)
  }
}

///|
test "owner, labelled binder, and negative pattern locations match handrolled" {
  let cases = [
    "fn &T::f() {}", "fn f(x~ : Int) {}", "trait I { f(x~ : Int) -> Unit }", "fn f(x : Int) { match x { -1 => 0; -3.14 => 1; _ => 2 } }",
  ]
  for source in cases {
    assert_handrolled_cst_ast_equiv(source)
  }
}

///|
test "annotated let constraint location excludes statement syntax" {
  let source = "let x : Int = 1; x"
  let lex = @lexer.tokens_from_string(source, comment=true)
  let (expr, reports) = @handrolled_parser.parse_expr(lex.tokens)
  @test.assert_eq(lex.errors.length(), 0)
  @test.assert_eq(reports.length(), 0)
  guard expr
    is @syntax.Expr::Let(
      pattern=@syntax.Pattern::Constraint(
        pat=inner_pattern,
        ty~,
        loc=constraint_loc
      ),
      expr=rhs,
      ..
    ) else {
    fail("expected an annotated let expression")
  }
  @test.assert_eq(constraint_loc, inner_pattern.loc().merge(ty.loc()))
  @test.assert_eq(constraint_loc.start.column(), 5)
  @test.assert_eq(constraint_loc.end.column(), 12)
  @test.assert_eq(rhs.loc().start.column(), 15)
}

///|
test "soft keyword extend in function binder positions is AST equivalent" {
  let cases = [
    "fn main() { fn extend() {} }", "fn main() { async fn extend() {} }", "fn main() { letrec extend = fn() {} }",
    "fn main() { letrec f = fn() {} and extend = fn() {} }", "fn T::extend() {}",
  ]
  for source in cases {
    assert_ast_equiv(source)
  }
}

///|
test "docstring AST equivalence cases" {
  assert_ast_equiv(docstring_source())
}

///|
test "structure docstring AST conversion preserves docstrings" {
  let source = docstring_source()
  let result = @untyped_cst.parse_structure(source)
  let (impls, reports) = result.to_impls()
  @test.assert_eq(reports.length(), 0)
  let (expected, expected_reports) = @parser.parse_string(
    source,
    parser=@parser.Handrolled,
  )
  @test.assert_eq(expected_reports.length(), 0)
  @test.assert_eq(impls_json(impls), impls_json(expected))
}

///|
test "docstring conversion is idempotent" {
  let source = docstring_source()
  let result = @untyped_cst.parse_structure(source)
  let (first_impls, first_reports) = result.to_impls()
  let (second_impls, second_reports) = result.to_impls()
  @test.assert_eq(first_reports.length(), 0)
  @test.assert_eq(second_reports.length(), 0)
  @test.assert_eq(impls_json(first_impls), impls_json(second_impls))
}

///|
fn array_has_string(items : Array[String], expected : String) -> Bool {
  for item in items {
    if item == expected {
      return true
    }
  }
  false
}

///|
test "copied AST parser bridge files are removed from package sources" {
  let sources = @fs.read_dir("untyped_cst") catch {
    _ => fail("failed to read untyped_cst directory")
  }
  let removed_files = [
    "ast_bridge.mbt", "ast_parser.mbt", "ast_parser_core.mbt", "ast_parser_first_set.mbt",
  ]
  for path in removed_files {
    @test.assert_eq(array_has_string(sources, path), false)
  }
}

///|
fn handrolled_cst_reports_json(reports : Array[@basic.Report]) -> Json {
  @basic.show_loc.protect(Json, fn() { reports.to_json() })
}

///|
fn handrolled_cst_expression_issue(source : String) -> String? {
  let lex = @lexer.tokens_from_string(
    source,
    name="handrolled_cst_expression.mbt",
    comment=true,
  )
  if !lex.errors.is_empty() {
    return Some("unexpected lexer diagnostics")
  }
  let (hand_expr, hand_reports) = @handrolled_parser.parse_expr(lex.tokens)
  let cst = @untyped_cst.parse_expression(
    source,
    name="handrolled_cst_expression.mbt",
  )
  let cst_reports = reports_from_view(cst.diagnostics_view())
  if handrolled_cst_reports_json(hand_reports) !=
    handrolled_cst_reports_json(cst_reports) {
    if hand_reports.is_empty() {
      return Some("handrolled accepts, CST reports")
    }
    if cst_reports.is_empty() {
      return Some("handrolled reports, CST accepts")
    }
    return Some("different reports")
  }
  if cst.root().text(source) != source[:] {
    return Some("source text")
  }
  if hand_reports.is_empty() {
    let (cst_expr, conversion_reports) = cst.to_expr()
    if !conversion_reports.is_empty() {
      return Some("conversion diagnostics")
    }
    match cst_expr {
      Some(actual) if expr_json(hand_expr) != expr_json(actual) =>
        return Some("AST")
      Some(_) => ()
      None => return Some("missing AST")
    }
  }
  None
}

///|
test "handrolled/CST expression operator matrix" {
  let atoms = [
    "x", "1", "1.5", "true", "None", "Some(1)", "()", "(1,)", "(1, 2)", "[1, 2]",
    "\"text\"", "b\"bytes\"",
  ]
  let operators = [
    " + ", " - ", " * ", " / ", " % ", " == ", " != ", " < ", " <= ", " > ", " >= ",
    " && ", " || ", " & ", " | ", " ^ ", " << ", " >> ", " |> ",
  ]
  let issues : Array[String] = []
  for lhs in atoms {
    for op in operators {
      for rhs in atoms {
        let source = "\{lhs}\{op}\{rhs}"
        if handrolled_cst_expression_issue(source) is Some(kind) {
          issues.push("\{source}: \{kind}")
        } else {
          ()
        }
      }
    }
  }
  let focused = [
    "-x", "+1.0F", "!x", "f(1, x=2, y~)", "x.field[0:1]", "1 ..< 2", "1 ..= 2", "x as T",
    "x is 1 | 2", "x =~ re\"a\"", "if true { 1 } else { 2 }", "match x { Some(y) => y; None => 0 }",
    "while true { break }", "for x in xs { x } nobreak { 0 }", "loop x { y => continue y + 1 }",
    "try f() catch { _ => 0 }", "(x : Int) => x", "{ let x = 1; x }",
  ]
  for source in focused {
    if handrolled_cst_expression_issue(source) is Some(kind) {
      issues.push("\{source}: \{kind}")
    } else {
      ()
    }
  }
  if !issues.is_empty() {
    let details = issues.join("\n")
    fail(
      "expression equivalence failed for \{issues.length()} cases:\n\{details}",
    )
  }
}

///|
test "handrolled/CST truncated expression matrix" {
  let cases = [
    "x +", "x -", "x *", "x /", "x %", "x ==", "x &&", "x ||", "x |>", "x ..<", "x is",
    "x as", "x =~", "-", "!", "raise", "return", "defer", "(", "(x", "[", "[x", "{",
    "{ x", "f(", "f(x", "x[", "x[0", "if", "if x", "if x {", "if x {} else", "match",
    "match x", "match x {", "match x { _", "match x { _ =>", "while", "while x",
    "while x {", "loop", "loop x", "loop x {", "try", "try f()", "try f() catch",
    "fn", "fn(", "fn(x", "fn(x)",
  ]
  let issues : Array[String] = []
  for source in cases {
    if handrolled_cst_expression_issue(source) is Some(kind) {
      issues.push("\{source}: \{kind}")
    } else {
      ()
    }
  }
  if !issues.is_empty() {
    let details = issues.join("\n")
    fail(
      "truncated expression equivalence failed for \{issues.length()} cases:\n\{details}",
    )
  }
}

///|
fn handrolled_cst_snapshot_issue(
  path : String,
  source : String,
) -> (String?, Bool) {
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    name=path,
    parser=Handrolled,
  )
  let cst = @untyped_cst.parse_structure(source, name=path)
  let cst_reports = reports_from_view(cst.diagnostics_view())
  let hand_accepts = hand_reports.is_empty()
  if handrolled_cst_reports_json(hand_reports) !=
    handrolled_cst_reports_json(cst_reports) {
    if hand_accepts {
      return (Some("handrolled accepts, CST reports"), hand_accepts)
    }
    if cst_reports.is_empty() {
      return (Some("handrolled reports, CST accepts"), hand_accepts)
    }
    if hand_reports.length() != cst_reports.length() {
      if cst_reports.length() > hand_reports.length() {
        return (Some("CST has more reports"), hand_accepts)
      }
      return (Some("CST has fewer reports"), hand_accepts)
    }
    let mut same_locations = true
    let mut same_messages = true
    for i in 0..<hand_reports.length() {
      if hand_reports[i].loc != cst_reports[i].loc {
        same_locations = false
      }
      if hand_reports[i].msg != cst_reports[i].msg {
        same_messages = false
      }
    }
    if same_messages {
      return (Some("different report locations"), hand_accepts)
    }
    if same_locations {
      return (Some("different report messages"), hand_accepts)
    }
    return (Some("different report messages and locations"), hand_accepts)
  }
  if cst.root().text(source) != source[:] {
    return (Some("source text"), hand_accepts)
  }
  if hand_accepts {
    let (cst_impls, conversion_reports) = cst.to_impls()
    if !conversion_reports.is_empty() {
      return (Some("conversion diagnostics"), hand_accepts)
    }
    if impls_json(hand_impls) != impls_json(cst_impls) {
      return (Some("AST"), hand_accepts)
    }
  }
  (None, hand_accepts)
}

///|
fn assert_handrolled_cst_node_source_invariants(
  label : String,
  path : String,
  node : @untyped_cst.CstNode,
  source : String,
  name : String,
) -> Unit raise {
  let loc = node.loc
  if loc.start.fname != loc.end.fname ||
    (loc.start.fname != "" && loc.start.fname != name) {
    fail(
      "inconsistent CST location filename for \{label} at \{path} (\{to_repr(node.kind)}): expected empty or \{name}, got \{loc.start.fname}..\{loc.end.fname}",
    )
  }
  let span = node.source_span
  if span.0 < 0 || span.0 > span.1 || span.1 > source.length() {
    fail(
      "invalid CST source span for \{label} at \{path}: \{span.0}..\{span.1}, source length \{source.length()}",
    )
  }
  ignore(node.text(source))
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) =>
        assert_handrolled_cst_node_source_invariants(
          label,
          "\{path}/\{i}",
          child,
          source,
          name,
        )
      None => fail("missing CST child for \{label} at \{path}/\{i}")
    }
  }
}

///|
fn assert_handrolled_cst_structure_equiv(
  label : String,
  source : String,
) -> Unit raise {
  let name = "handrolled_cst_equiv.mbt"
  let (hand_impls, hand_reports) = @parser.parse_string(
    source,
    name~,
    parser=Handrolled,
  )
  let cst = @untyped_cst.parse_structure(source, name~)
  assert_handrolled_cst_node_source_invariants(
    label,
    "root",
    cst.root(),
    source,
    name,
  )
  let cst_reports = reports_from_view(cst.diagnostics_view())
  let hand_json = handrolled_cst_reports_json(hand_reports)
  let cst_json = handrolled_cst_reports_json(cst_reports)
  if hand_json != cst_json {
    fail(
      "diagnostics mismatch for \{label}:\nsource:\n\{source}\nhandrolled:\n\{hand_json.stringify(indent=2)}\nCST:\n\{cst_json.stringify(indent=2)}",
    )
  }
  if hand_reports.is_empty() {
    let (cst_impls, conversion_reports) = cst.to_impls()
    if !conversion_reports.is_empty() {
      fail("conversion diagnostics for \{label}")
    }
    if impls_json(hand_impls) != impls_json(cst_impls) {
      fail("AST mismatch for \{label}")
    }
  }
}

///|
test "handrolled/CST regression: malformed lexmatch pattern does not crash" {
  assert_handrolled_cst_structure_equiv(
    "malformed lexmatch pattern", "fn f() { lexmatch s { 1 => () } }",
  )
}

///|
test "handrolled/CST regression: malformed regex alternation does not crash" {
  assert_handrolled_cst_structure_equiv(
    "malformed regex alternation", "fn f() { let _ = s =~ ((re\"a\" |), before=head) }",
  )
}

///|
test "handrolled/CST regression: malformed C-style for does not crash" {
  assert_handrolled_cst_structure_equiv(
    "malformed C-style for", "fn f() { for i = 0 if true {} }",
  )
}

///|
test "handrolled/CST regression: missing let value diagnostics" {
  assert_handrolled_cst_structure_equiv(
    "missing let value", "fn f() { let x =; }",
  )
}

///|
test "handrolled/CST regression: lowercase error type diagnostics" {
  assert_handrolled_cst_structure_equiv(
    "lowercase error type", "fn f() raise foo {}",
  )
}

///|
test "handrolled/CST regression: enum comma diagnostics at EOF" {
  assert_handrolled_cst_structure_equiv(
    "enum comma at EOF", "enum Foo{Bar,Baz} //no trailing newline",
  )
}

///|
fn handrolled_cst_snapshot_requires_isolated_check(path : String) -> Bool {
  match path {
    // The reference handrolled parser itself aborts on this fixture.
    "pipeline_test_functional_loop4.mbt" => true
    _ => false
  }
}

///|
test "formerly crashing CST snapshots are handrolled equivalent" {
  let dir = "test/sync_test/__snapshot__"
  let paths = [
    "parse_test_lexmatch_regex_literals.mbt", "parse_test_invalid_regex_match_expr.mbt",
    "parse_test_for_loop_unexpected_token.mbt", "parse_test_try_with_while_and_for_body_error.mbt",
  ]
  for path in paths {
    let source = @fs.read_file_to_string("\{dir}/\{path}") catch {
      _ => fail("failed to read \{dir}/\{path}")
    }
    assert_handrolled_cst_structure_equiv(path, source)
  }
}

///|
test "handrolled/CST equivalence across all parser snapshots" {
  let dir = "test/sync_test/__snapshot__"
  let paths = @fs.read_dir(dir) catch { _ => fail("failed to read \{dir}") }
  paths.sort()
  let issues : Array[String] = []
  let mut checked = 0
  let mut hand_accepted = 0
  for path in paths {
    if path.has_suffix(".mbt") &&
      !handrolled_cst_snapshot_requires_isolated_check(path) {
      let source = @fs.read_file_to_string("\{dir}/\{path}") catch {
        _ => fail("failed to read \{dir}/\{path}")
      }
      let (issue, accepted) = handrolled_cst_snapshot_issue(path, source)
      checked += 1
      if accepted {
        hand_accepted += 1
      }
      if issue is Some(kind) {
        issues.push("\{path}: \{kind}")
      } else {
        ()
      }
    }
  }
  if !issues.is_empty() {
    let details = issues.join("\n")
    fail(
      "handrolled/CST equivalence failed for \{issues.length()} of \{checked} checked snapshots (handrolled accepted \{hand_accepted}):\n\{details}",
    )
  }
}
