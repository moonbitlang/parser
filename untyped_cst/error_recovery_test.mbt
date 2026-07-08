///|
fn recovery_any_node(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.CstNode) -> Bool,
) -> Bool {
  if pred(node) {
    return true
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) if recovery_any_node(child, pred) => return true
      _ => ()
    }
  }
  false
}

///|
fn recovery_is_missing(
  kind : @tokens.TokenKind,
) -> (@untyped_cst.CstNode) -> Bool {
  fn(node) {
    match node.kind {
      @untyped_cst.Missing(found) => found == kind
      _ => false
    }
  }
}

///|
fn recovery_is_error(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.Error => true
    _ => false
  }
}

///|
fn recovery_is_lident(name : String) -> (@untyped_cst.CstNode) -> Bool {
  fn(node) {
    match node.kind {
      @untyped_cst.Token(@tokens.LIDENT(found)) => found == name
      _ => false
    }
  }
}

///|
fn recovery_is_int(value : String) -> (@untyped_cst.CstNode) -> Bool {
  fn(node) {
    match node.kind {
      @untyped_cst.Token(@tokens.INT(found)) => found == value
      _ => false
    }
  }
}

///|
test "missing function parameter close recovers" {
  let source = "fn f("
  assert_structure_acceptance("missing function parameter close", source, false)
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
  @test.assert_eq(
    recovery_any_node(
      result.root(),
      recovery_is_missing(@tokens.TokenKind::TK_RPAREN),
    ),
    true,
  )
}

///|
test "bad leading parameter separator keeps following parameter" {
  let source = "fn f(, x)"
  assert_structure_acceptance("bad leading parameter separator", source, false)
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
  @test.assert_eq(
    recovery_any_node(result.root(), recovery_is_lident("x")),
    true,
  )
}

///|
test "missing let pattern is represented" {
  let source = "let = 1"
  assert_structure_acceptance("missing let pattern", source, false)
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
  @test.assert_eq(
    recovery_any_node(
      result.root(),
      recovery_is_missing(@tokens.TokenKind::TK_LIDENT),
    ),
    true,
  )
}

///|
test "invalid question token in block becomes error node" {
  let source = "fn f() { ? }"
  assert_structure_acceptance("invalid question token in block", source, false)
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
  @test.assert_eq(recovery_any_node(result.root(), recovery_is_error), true)
}

///|
test "statement recovery continues after semicolon" {
  let source = "fn f() { ?; 1 }"
  assert_structure_acceptance(
    "statement recovery after semicolon", source, false,
  )
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
  @test.assert_eq(recovery_any_node(result.root(), recovery_is_error), true)
  @test.assert_eq(recovery_any_node(result.root(), recovery_is_int("1")), true)
}

///|
test "lexical diagnostics are preserved for absent bad tokens" {
  let source = "fn f() { @ }"
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
}

///|
test "top-level recovery keeps following declaration" {
  let source = "fn f(\nlet y = 1"
  assert_structure_acceptance("top-level recovery", source, false)
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
  @test.assert_eq(
    recovery_any_node(result.root(), recovery_is_lident("y")),
    true,
  )
}

///|
test "function literal does not split top-level declaration" {
  let source = "let f = fn(x) { x }\nlet y = 1"
  assert_structure_acceptance(
    "function literal stays in declaration", source, true,
  )
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length(), 0)
  @test.assert_eq(
    recovery_any_node(result.root(), recovery_is_lident("y")),
    true,
  )
}

///|
test "declare prefix survives recovery" {
  let source = "declare fn f()\n?;\ndeclare type T"
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(
    recovery_any_node(result.root(), recovery_is_lident("f")),
    true,
  )
  @test.assert_eq(
    recovery_any_node(result.root(), recovery_is_lident("T")),
    false,
  )
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
}
