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
  @test.assert_eq(direct_kind, @node.NodeKind::Impls)

  let compat_payload : @untyped_cst.NodePayload = @untyped_cst.NodePayload::String(
    "value",
  )
  let direct_payload : @node.NodePayload = compat_payload
  @test.assert_eq(direct_payload, @node.NodePayload::String("value"))

  let compat_leaf : @untyped_cst.NodeKind = @untyped_cst.NodeKind::Leaf(
    compat_payload,
  )
  let direct_leaf : @node.NodeKind = compat_leaf
  @test.assert_eq(
    direct_leaf,
    @node.NodeKind::Leaf(@node.NodePayload::String("value")),
  )
}

///|
test "compatibility path retains public node-kind classifiers" {
  @test.assert_eq(
    @untyped_cst.NodeKind::is_impl(@untyped_cst.NodeKind::Impl_Function),
    true,
  )
  @test.assert_eq(
    @untyped_cst.NodeKind::is_expr(@untyped_cst.NodeKind::Expr_Ident),
    true,
  )
  @test.assert_eq(
    @untyped_cst.NodeKind::is_pattern(@untyped_cst.NodeKind::Pattern_Var),
    true,
  )
  @test.assert_eq(
    @untyped_cst.NodeKind::is_type(@untyped_cst.NodeKind::Type_Name),
    true,
  )
  @test.assert_eq(
    @untyped_cst.NodeKind::is_argument(
      @untyped_cst.NodeKind::Argument_Positional,
    ),
    true,
  )
  @test.assert_eq(
    @untyped_cst.NodeKind::is_constant(@untyped_cst.NodeKind::Constant_Int),
    true,
  )
}
