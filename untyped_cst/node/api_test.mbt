///|
fn test_position(offset : Int) -> @basic.Position {
  @basic.Position::{ fname: "node_test.mbt", lnum: 1, bol: 0, cnum: offset, }
}

///|
test "typed constructors retain fields, optional children, and scalar payloads" {
  let start = test_position(0)
  let end = test_position(2)
  let loc = @node.mk_loc(start, end)
  let span = @node.mk_span(0, 2)
  let value = @node.CstNode::name_identifier(value="x", loc~, source_span=span)
  let open = @node.CstNode::syntax_delimiter(loc~, source_span=span)
  let close = @node.CstNode::syntax_delimiter(loc~, source_span=span)
  let index = @node.CstNode::constant_int(value="1", loc~, source_span=span)
  let get = @node.CstNode::expr_array_get(
    loc~,
    source_span=span,
    self_=value,
    open~,
    index~,
    close~,
  )
  @test.assert_eq(get.children.length(), 4)
  @test.assert_eq(
    get.first_child_with_label("self").unwrap().kind
    is @node.NodeKind::Name_Identifier,
    true,
  )
  @test.assert_eq(get.has_child_label("index_loc"), false)
  @test.assert_eq(value.string_payload(), Some("x"))
  @test.assert_eq(index.string_payload(), Some("1"))
  let ident = @node.CstNode::expr_ident(loc~, source_span=span, name=[
    value, index,
  ])
  @test.assert_eq(ident.children_with_label("name").length(), 2)
  @test.assert_eq(ident.children[0].1.string_payload(), Some("x"))
  @test.assert_eq(ident.children[1].1.string_payload(), Some("1"))
}

///|
test "syntax payload and ordered recovery entries are controlled" {
  let start = test_position(0)
  let end = test_position(1)
  let loc = @node.mk_loc(start, end)
  let span = @node.mk_span(0, 1)
  let token = @node.CstNode::syntax_identifier(
    loc~,
    source_span=span,
    payload=@node.NodePayload::String("x"),
  )
  @test.assert_eq(token.string_payload(), Some("x"))
  let recovered = @node.CstNode::error(loc~, source_span=span, entries=[
    @node.CstRecoveryChild::Item(
      @node.CstNode::syntax_operator(loc~, source_span=span),
    ),
    @node.CstRecoveryChild::QuestionLoc(
      @node.CstNode::syntax_operator(loc~, source_span=span),
    ),
  ])
  @test.assert_eq(recovered.children[0].0, Some("item"))
  @test.assert_eq(
    recovered.children[0].1.kind is @node.NodeKind::Syntax_Unexpected,
    true,
  )
  @test.assert_eq(recovered.children[1].0, Some("question_loc"))
  @test.assert_eq(
    recovered.children[1].1.kind is @node.NodeKind::Syntax_Operator,
    true,
  )
}

///|
test "predicate traversal remains usable with read-only NodeKind" {
  let loc = @node.empty_loc(test_position(0))
  let span = @node.empty_span(0)
  let child = @node.CstNode::constant_bool(value=true, loc~, source_span=span)
  let root = @node.CstNode::expression(loc~, source_span=span, entries=[
    @node.CstRootChild::Expr(child),
  ])
  @test.assert_eq(
    root.has_child_kind(fn(kind) { kind is @node.NodeKind::Constant_Bool }),
    true,
  )
  @test.assert_eq(
    root.bool_marker_value(fn(kind) { kind is @node.NodeKind::Constant_Bool }),
    Some(true),
  )
}
