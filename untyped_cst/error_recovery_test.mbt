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
fn recovery_is_missing() -> (@untyped_cst.CstNode) -> Bool {
  fn(node) { node.kind is @untyped_cst.Missing }
}

///|
fn recovery_is_error(node : @untyped_cst.CstNode) -> Bool {
  match node.kind {
    @untyped_cst.Error => true
    _ => false
  }
}

///|
fn recovery_root_item_at(
  root : @untyped_cst.CstNode,
  index : Int,
) -> @untyped_cst.CstNode? {
  let mut item_index = 0
  for entry in root.children {
    match entry {
      (Some("item"), child) if child.kind.is_impl() => {
        if item_index == index {
          return Some(child)
        }
        item_index += 1
      }
      _ => ()
    }
  }
  None
}

///|
fn recovery_leaf_payload(
  node : @untyped_cst.CstNode,
) -> @untyped_cst.NodePayload? {
  for entry in node.children {
    let (label, child) = entry
    if label is Some("payload") {
      match child.kind {
        @untyped_cst.Leaf(payload) => return Some(payload)
        _ => ()
      }
    }
  }
  None
}

///|
fn recovery_is_syntax_string(value : String) -> (@untyped_cst.CstNode) -> Bool {
  fn(node) {
    node.kind
    is (@untyped_cst.Syntax_Keyword
    | @untyped_cst.Syntax_Delimiter
    | @untyped_cst.Syntax_Separator
    | @untyped_cst.Syntax_Operator
    | @untyped_cst.Syntax_Identifier
    | @untyped_cst.Syntax_Label
    | @untyped_cst.Syntax_Accessor
    | @untyped_cst.Syntax_Literal
    | @untyped_cst.Syntax_Interpolation
    | @untyped_cst.Syntax_Attribute
    | @untyped_cst.Syntax_Comment
    | @untyped_cst.Syntax_Eof
    | @untyped_cst.Syntax_Unexpected) &&
    recovery_leaf_payload(node) is Some(@untyped_cst.NodePayload::String(found)) &&
    found == value
  }
}

///|
fn recovery_has_direct_child(
  node : @untyped_cst.CstNode,
  label : String,
  pred : (@untyped_cst.NodeKind) -> Bool,
) -> Bool {
  for entry in node.children {
    let (child_label, child) = entry
    if child_label is Some(found) && found == label && pred(child.kind) {
      return true
    }
  }
  false
}

///|
test "missing function parameter close recovers" {
  let source = "fn f("
  assert_structure_acceptance("missing function parameter close", source, false)
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
  @test.assert_eq(recovery_any_node(result.root(), recovery_is_missing()), true)
}

///|
test "invalid normalized forms retain their diagnostic source subtrees" {
  let assignment = @untyped_cst.parse_expression("(x + y) = z")
  @test.assert_eq(assignment.diagnostics_view().is_empty(), false)
  @test.assert_eq(
    recovery_any_node(assignment.root(), fn(node) {
      node.kind is @untyped_cst.Expr_Assign &&
      recovery_has_direct_child(node, "lhs", fn(kind) {
        kind is @untyped_cst.Expr_Group
      })
    }),
    true,
  )

  let arrow = @untyped_cst.parse_expression("(x + y) => x")
  @test.assert_eq(arrow.diagnostics_view().is_empty(), false)
  @test.assert_eq(
    recovery_any_node(arrow.root(), fn(node) {
      node.kind is @untyped_cst.Parameter_Positional &&
      recovery_has_direct_child(node, "error", fn(kind) {
        kind is @untyped_cst.Expr_Infix
      })
    }),
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
    recovery_any_node(result.root(), recovery_is_syntax_string("x")),
    true,
  )
}

///|
test "missing let pattern is represented" {
  let source = "let = 1"
  assert_structure_acceptance("missing let pattern", source, false)
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
  @test.assert_eq(recovery_any_node(result.root(), recovery_is_missing()), true)
}

///|
test "invalid question token in block becomes error node" {
  let source = "fn f() { ? }"
  assert_structure_acceptance("invalid question token in block", source, false)
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
  @test.assert_eq(recovery_any_node(result.root(), recovery_is_error), true)
  @test.assert_eq(
    recovery_any_node(result.root(), fn(node) {
      node.kind is @untyped_cst.Syntax_Unexpected
    }),
    true,
  )
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
  @test.assert_eq(
    recovery_any_node(result.root(), recovery_is_syntax_string("1")),
    true,
  )
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
    recovery_any_node(result.root(), recovery_is_syntax_string("y")),
    true,
  )
}

///|
test "removed try question recovery keeps following top-level declaration" {
  let source =
    #|fn broken() {
    #|  let _ = try try? 1 catch { _ => 0 }
    #|}
    #|fn kept() { 42 }
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
  @test.assert_eq(result.root().text(source), source[:])
  @test.assert_eq(count_root_items(result.root()), 2)
  guard recovery_root_item_at(result.root(), 1) is Some(following) else {
    fail("expected a following top-level declaration")
  }
  @test.assert_eq(following.kind is @untyped_cst.Impl_Function, true)
  @test.assert_eq(
    recovery_any_node(following, recovery_is_syntax_string("kept")),
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
    recovery_any_node(result.root(), recovery_is_syntax_string("y")),
    true,
  )
}

///|
test "declare prefix survives recovery" {
  let source = "declare fn f()\n?;\ndeclare type T"
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(
    recovery_any_node(result.root(), recovery_is_syntax_string("f")),
    true,
  )
  @test.assert_eq(
    recovery_any_node(result.root(), recovery_is_syntax_string("T")),
    true,
  )
  @test.assert_eq(
    recovery_any_node(result.root(), fn(node) {
      node.kind is @untyped_cst.Syntax_Unexpected &&
      node.text(source) == "\n"[:]
    }),
    true,
  )
  @test.assert_eq(result.diagnostics_view().length() > 0, true)
}
