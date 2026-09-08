///|
fn node_from_compat(node : @untyped_cst.CstNode) -> @node.CstNode {
  node
}

///|
fn node_to_compat(node : @node.CstNode) -> @untyped_cst.CstNode {
  node
}

///|
test "node types are available through both package paths" {
  let parsed = @untyped_cst.parse_structure("fn main {}")
  let root : @node.CstNode = parsed.root()
  let compat_root : @untyped_cst.CstNode = node_to_compat(root)
  let direct_root : @node.CstNode = node_from_compat(compat_root)
  let compat_kind : @untyped_cst.NodeKind = direct_root.kind
  let direct_kind : @node.NodeKind = compat_kind
  @test.assert_eq(direct_kind is @node.NodeKind::Impls, true)

  let compat_payload : @untyped_cst.NodePayload = @untyped_cst.NodePayload::String(
    "value",
  )
  let direct_payload : @node.NodePayload = compat_payload
  @test.assert_eq(direct_payload, @node.NodePayload::String("value"))

  let compat_leaf : @untyped_cst.NodeKind = @untyped_cst.CstNode::leaf(
      payload=compat_payload,
      loc=direct_root.loc,
      source_span=direct_root.source_span,
    ).kind
  let direct_leaf : @node.NodeKind = compat_leaf
  guard direct_leaf is @node.NodeKind::Leaf(payload) else {
    fail("expected leaf kind")
  }
  @test.assert_eq(payload, @node.NodePayload::String("value"))
}

///|
test "compatibility path retains public node-kind classifiers" {
  let parsed = @untyped_cst.parse_structure("fn main {}")
  let root = parsed.root()
  let loc = root.loc
  let source_span = root.source_span
  @test.assert_eq(
    @untyped_cst.CstNode::constant_int(value="1", loc~, source_span~).kind.is_constant(),
    true,
  )
}
