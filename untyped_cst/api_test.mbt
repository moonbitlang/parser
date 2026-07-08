///|
fn api_find_node(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.CstNode) -> Bool,
) -> @untyped_cst.CstNode? {
  if pred(node) {
    return Some(node)
  }
  for entry in node.children {
    let (_, child) = entry
    if api_find_node(child, pred) is Some(found) {
      return Some(found)
    }
  }
  None
}

///|
test "parse entry points expose distinct roots and propagate names" {
  let structure = @untyped_cst.parse_structure(
    "let x = 1",
    name="structure.mbt",
  )
  guard structure.root().kind is @untyped_cst.SourceFile else {
    fail("expected SourceFile")
  }
  @test.assert_eq(structure.root().loc.start.fname, "structure.mbt")
  @test.assert_eq(structure.root().loc.end.fname, "structure.mbt")
  let expression = @untyped_cst.parse_expression("1 + 2", name="expression.mbt")
  guard expression.root().kind is @untyped_cst.ExpressionFragment else {
    fail("expected ExpressionFragment")
  }
  @test.assert_eq(expression.root().loc.start.fname, "expression.mbt")
  @test.assert_eq(expression.root().loc.end.fname, "expression.mbt")
}

///|
test "parse entry points preserve diagnostics and AST lowering" {
  let structure = @untyped_cst.parse_structure("let x = 1")
  @test.assert_eq(structure.diagnostics_view().is_empty(), true)
  let (_, structure_reports) = structure.to_impls()
  @test.assert_eq(structure_reports.is_empty(), true)
  let expression = @untyped_cst.parse_expression("1 + 2")
  @test.assert_eq(expression.diagnostics_view().is_empty(), true)
  let (expr, expression_reports) = expression.to_expr()
  @test.assert_eq(expression_reports.is_empty(), true)
  guard expr is Some(_) else { fail("expected lowered expression") }
  @test.assert_eq(
    @untyped_cst.parse_structure("fn f(").diagnostics_view().is_empty(),
    false,
  )
  @test.assert_eq(
    @untyped_cst.parse_expression("1 +").diagnostics_view().is_empty(),
    false,
  )
}

///|
test "AST conversion rejects the other parse result kind" {
  let (_, structure_reports) = @untyped_cst.parse_expression("1").to_impls()
  @test.assert_eq(structure_reports.length(), 1)
  @test.assert_eq(structure_reports[0].msg, "expected structure parse result")
  let (_, expression_reports) = @untyped_cst.parse_structure("let x = 1").to_expr()
  @test.assert_eq(expression_reports.length(), 1)
  @test.assert_eq(expression_reports[0].msg, "expected expression parse result")
}

///|
test "source spans and node text use UTF-16 slicing offsets" {
  let source = "let s = \"😀\"\nlet x = 1"
  let result = @untyped_cst.parse_structure(source)
  guard api_find_node(result.root(), fn(node) {
      node.kind is @untyped_cst.Token(@tokens.LIDENT("x"))
    })
    is Some(x) else {
    fail("expected x token")
  }
  @test.assert_eq(x.source_span.0, 17)
  @test.assert_eq(x.source_span.1, 18)
  let text : StringView = x.text(source)
  @test.assert_eq(text, "x"[:])
}

///|
test "missing nodes return an empty StringView" {
  let source = "fn f("
  let result = @untyped_cst.parse_structure(source)
  guard api_find_node(result.root(), fn(node) {
      node.kind is @untyped_cst.Missing(@tokens.TokenKind::TK_RPAREN)
    })
    is Some(missing) else {
    fail("expected missing right parenthesis")
  }
  let text : StringView = missing.text(source)
  @test.assert_eq(text, source[source.length():])
}
