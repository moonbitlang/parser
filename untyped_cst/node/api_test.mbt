///|
fn test_position(offset : Int) -> @basic.Position {
  @basic.Position::{ fname: "node_test.mbt", lnum: 1, bol: 0, cnum: offset, }
}

///|
fn public_children(
  node : @node.CstNode,
) -> ReadOnlyArray[(String?, @node.CstNode)] {
  node.children
}

///|
test "public node constructors retain immutable shape and payloads" {
  let start = test_position(0)
  let end = test_position(2)
  let loc = @node.mk_loc(start, end)
  let span = @node.mk_span(0, 2)
  let children : Array[(String?, @node.CstNode)] = []
  @node.push_child(
    children,
    Some("name"),
    @node.scalar_node(
      @node.NodeKind::Name_Identifier,
      loc,
      span,
      @node.NodePayload::String("😀"),
    ),
  )
  @node.push_children(children, [
    (
      Some("enabled"),
      @node.bool_marker_node(@node.NodeKind::Flag_Async, end, 2, true),
    ),
  ])
  let root = @node.node(@node.NodeKind::Expression, loc, span, children)

  @test.assert_eq(root.loc, loc)
  @test.assert_eq(root.source_span, span)
  @test.assert_eq(public_children(root).length(), 2)
  @test.assert_eq(root.has_child_label("name"), true)
  @test.assert_eq(root.has_child_kind(@node.NodeKind::Flag_Async), true)
  @test.assert_eq(
    root.first_child_with_label("name").bind(child => child.string_payload()),
    Some("😀"),
  )
  @test.assert_eq(
    root.bool_marker_value(@node.NodeKind::Flag_Async),
    Some(true),
  )

  let copy = root.with_kind(@node.NodeKind::Expr_Block)
  @test.assert_eq(copy.kind, @node.NodeKind::Expr_Block)
  @test.assert_eq(copy.loc, root.loc)
  @test.assert_eq(copy.source_span, root.source_span)
  @test.assert_eq(copy.children.length(), root.children.length())
}

///|
test "public child traversal handles labels kinds and descendants" {
  let pos = test_position(0)
  let loc = @node.empty_loc(pos)
  let span = @node.empty_span(0)
  let constant = @node.node_with_payload(
    @node.NodeKind::Constant_Int,
    loc,
    span,
    [],
    @node.NodePayload::String("1"),
  )
  let group = @node.node(@node.NodeKind::Expr_Group, loc, span, [
    (Some("item"), constant),
  ])
  let root = @node.node(@node.NodeKind::Expression, loc, span, [
    (Some("expr"), group),
    (Some("expr"), @node.marker_node(@node.NodeKind::Expr_Hole, pos, 0)),
  ])

  @test.assert_eq(root.children_with_label("expr").length(), 2)
  @test.assert_eq(
    root
    .first_child_of_kind(fn(kind) { kind.is_expr() })
    .map(child => child.kind),
    Some(@node.NodeKind::Expr_Group),
  )
  @test.assert_eq(
    root.children_of_kind(fn(kind) { kind.is_expr() }).length(),
    2,
  )
  @test.assert_eq(
    root.has_child_or_grandchild_kind(@node.NodeKind::Constant_Int),
    true,
  )
  @test.assert_eq(constant.leaf_payload(), Some(@node.NodePayload::String("1")))
  @test.assert_eq(constant.string_payload(), Some("1"))
  @test.assert_eq(constant.int_payload(), None)
  @test.assert_eq(constant.bool_payload(), None)
}

///|
test "public token mapping and recovery constructors preserve metadata" {
  let start = test_position(0)
  let end = test_position(1)
  let loc = @node.mk_loc(start, end)
  let span = @node.mk_span(0, 1)
  let token = @node.token_node(@tokens.Token::LIDENT("x"), start, end, span)
  @test.assert_eq(token.kind, @node.NodeKind::Syntax_Identifier)
  @test.assert_eq(token.string_payload(), Some("x"))
  @test.assert_eq(
    @node.syntax_kind_from_token_kind(@tokens.TokenKind::TK_DOT_INT),
    @node.NodeKind::Syntax_Accessor,
  )
  @test.assert_eq(
    @node.token_payload(@tokens.Token::DOT_INT(3)),
    Some(@node.NodePayload::Int(3)),
  )
  @test.assert_eq(
    @node.token_payload(@tokens.Token::TRUE),
    Some(@node.NodePayload::Bool(true)),
  )
  @test.assert_eq(
    @node.token_string_payload(@tokens.Token::LIDENT("name")),
    Some("name"),
  )

  let missing = @node.missing_node(@tokens.TokenKind::TK_LIDENT, start, 0)
  @test.assert_eq(missing.kind, @node.NodeKind::Missing)
  @test.assert_eq(missing.loc, @node.empty_loc(start))
  @test.assert_eq(missing.source_span, @node.empty_span(0))

  let recovered = @node.error_node(loc, span, [
    (Some("item"), @node.token_node(@tokens.Token::QUESTION, start, end, span)),
    (
      Some("question_loc"),
      @node.node(@node.NodeKind::Syntax_Operator, loc, span, []),
    ),
  ])
  @test.assert_eq(recovered.kind, @node.NodeKind::Error)
  @test.assert_eq(
    recovered.first_child_with_label("item").map(child => child.kind),
    Some(@node.NodeKind::Syntax_Unexpected),
  )
  @test.assert_eq(
    recovered.first_child_with_label("question_loc").map(child => child.kind),
    Some(@node.NodeKind::Syntax_Operator),
  )
}
