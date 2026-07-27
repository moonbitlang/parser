///|
fn any_node(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.NodeKind) -> Bool,
) -> Bool {
  if pred(node.kind) {
    return true
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) => if any_node(child, pred) { return true }
      None => ()
    }
  }
  false
}

///|
fn count_nodes(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.NodeKind) -> Bool,
) -> Int {
  let mut count = if pred(node.kind) { 1 } else { 0 }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) => count += count_nodes(child, pred)
      None => ()
    }
  }
  count
}

///|
fn first_node(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.CstNode) -> Bool,
) -> @untyped_cst.CstNode? {
  if pred(node) {
    return Some(node)
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) =>
        if first_node(child, pred) is Some(found) {
          return Some(found)
        } else {
          ()
        }
      None => ()
    }
  }
  None
}

///|
fn is_comment(kind : @untyped_cst.NodeKind) -> Bool {
  kind is @untyped_cst.Comment_Content
}

///|
fn state_leaf_payload(node : @untyped_cst.CstNode) -> @untyped_cst.NodePayload? {
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
fn is_asi_semi(node : @untyped_cst.CstNode) -> Bool {
  if node.kind is @untyped_cst.Syntax_Separator {
    state_leaf_payload(node) is Some(@untyped_cst.NodePayload::Bool(false))
  } else {
    false
  }
}

///|
fn is_any_semi(node : @untyped_cst.CstNode) -> Bool {
  node.kind is @untyped_cst.Syntax_Separator &&
  state_leaf_payload(node) is Some(@untyped_cst.NodePayload::Bool(true))
}

///|
fn is_lident(name : String) -> (@untyped_cst.CstNode) -> Bool {
  fn(node) {
    node.kind is @untyped_cst.Syntax_Identifier &&
    state_leaf_payload(node) is Some(@untyped_cst.NodePayload::String(id)) &&
    id == name
  }
}

///|
test "comments are retained and newline tokens are skipped" {
  let source = "// c\nlet x = 1"
  let result = @untyped_cst.parse_structure(source)
  let root = result.root()
  @test.assert_eq(result.diagnostics_view().length(), 0)
  @test.assert_eq(root.text(source), source[:])
  @test.assert_eq(any_node(root, is_comment), true)
  @test.assert_eq(
    first_node(root, fn(node) { node.text(source) == "\n"[:] }) is None,
    true,
  )
}

///|
test "asi semicolon is zero width and explicit semicolon spans source" {
  let asi_source = "let x = 1\nlet y = 2"
  let asi = @untyped_cst.parse_structure(asi_source).root()
  guard first_node(asi, is_asi_semi) is Some(asi_semi) else {
    fail("expected ASI semicolon")
  }
  @test.assert_eq(asi_semi.source_span.0, asi_semi.source_span.1)
  let explicit_source = "let x = 1; let y = 2"
  let explicit = @untyped_cst.parse_structure(explicit_source).root()
  guard first_node(explicit, is_any_semi) is Some(explicit_semi) else {
    fail("expected explicit semicolon")
  }
  @test.assert_eq(explicit_semi.text(explicit_source), ";"[:])
}

///|
test "default entry preserves text after emoji" {
  let source = "let s = \"😀\"\nlet x = 1"
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.root().text(source), source[:])
  guard first_node(result.root(), is_lident("x")) is Some(x) else {
    fail("expected x token")
  }
  @test.assert_eq(x.text(source), "x"[:])
}

///|
test "source-backed roots cover leading and trailing layout" {
  let source = "  let x = 1  "
  let result = @untyped_cst.parse_structure(source)
  @test.assert_eq(result.root().source_span.0, 0)
  @test.assert_eq(result.root().source_span.1, source.length())
  @test.assert_eq(result.root().text(source), source[:])
}
