///|
fn coloncolon_constructor_nodes() -> (
  @node.CstNode,
  @node.CstNode,
  Array[@node.CstNode],
  @node.CstNode,
) {
  let type_ = @node.CstNode::syntax_identifier(
    loc=@node.mk_loc(test_position(0), test_position(1)),
    source_span=(0, 1),
  )
  let op = @node.CstNode::syntax_operator(
    loc=@node.mk_loc(test_position(1), test_position(3)),
    source_span=(1, 3),
  )
  let trivia = [
    for start in [4, 13] => {
      @node.CstNode::syntax_comment(
        loc=@node.mk_loc(test_position(start), test_position(start + 7)),
        source_span=(start, start + 7),
      )
    }
  ]
  let tail = @node.CstNode::syntax_identifier(
    loc=@node.mk_loc(test_position(22), test_position(23)),
    source_span=(22, 23),
  )
  (type_, op, trivia, tail)
}

///|
fn assert_coloncolon_constructor_children(
  node : @node.CstNode,
  expected : Array[(String, @node.CstNode)],
) -> Unit raise {
  let source = "T:: // keep\n // keep\n f"
  @test.assert_eq(node.children.length(), expected.length())
  for i, (label, child) in node.children {
    @test.assert_eq(label, Some(expected[i].0))
    @test.assert_eq(child.loc, expected[i].1.loc)
    @test.assert_eq(child.source_span, expected[i].1.source_span)
    if label is Some("trivia") {
      @test.assert_eq(child.kind is @node.NodeKind::Syntax_Comment, true)
      @test.assert_eq(
        source[child.source_span.0:child.source_span.1],
        "// keep"[:],
      )
    }
  }
}

///|
test "expr_method accepts repeated trivia and defaults to no trivia" {
  let (type_, op, trivia, field) = coloncolon_constructor_nodes()
  let loc = type_.loc.merge(field.loc)
  let source_span = (0, 23)
  let with_trivia = @node.CstNode::expr_method(
    loc~,
    source_span~,
    type_~,
    op~,
    trivia~,
    field~,
  )
  @test.assert_eq(with_trivia.kind is @node.NodeKind::Expr_Method, true)
  assert_coloncolon_constructor_children(with_trivia, [
    ("type", type_),
    ("op", op),
    ("trivia", trivia[0]),
    ("trivia", trivia[1]),
    ("field", field),
  ])
  let without_trivia = @node.CstNode::expr_method(
    loc~,
    source_span~,
    type_~,
    op~,
    field~,
  )
  assert_coloncolon_constructor_children(without_trivia, [
    ("type", type_),
    ("op", op),
    ("field", field),
  ])
}

///|
test "expr_record accepts repeated trivia and defaults to no trivia" {
  let (type_, op, trivia, body) = coloncolon_constructor_nodes()
  let loc = type_.loc.merge(body.loc)
  let source_span = (0, 23)
  let with_trivia = @node.CstNode::expr_record(
    loc~,
    source_span~,
    type_~,
    op~,
    trivia~,
    body~,
  )
  @test.assert_eq(with_trivia.kind is @node.NodeKind::Expr_Record, true)
  assert_coloncolon_constructor_children(with_trivia, [
    ("type", type_),
    ("op", op),
    ("trivia", trivia[0]),
    ("trivia", trivia[1]),
    ("body", body),
  ])
  let without_trivia = @node.CstNode::expr_record(
    loc~,
    source_span~,
    type_~,
    op~,
    body~,
  )
  assert_coloncolon_constructor_children(without_trivia, [
    ("type", type_),
    ("op", op),
    ("body", body),
  ])
  assert_coloncolon_constructor_children(
    @node.CstNode::expr_record(loc~, source_span~, body~),
    [("body", body)],
  )
}

///|
test "expr_record_update accepts repeated trivia and defaults to no trivia" {
  let (type_, op, trivia, body) = coloncolon_constructor_nodes()
  let loc = type_.loc.merge(body.loc)
  let source_span = (0, 23)
  let with_trivia = @node.CstNode::expr_record_update(
    loc~,
    source_span~,
    type_~,
    op~,
    trivia~,
    body~,
  )
  @test.assert_eq(with_trivia.kind is @node.NodeKind::Expr_RecordUpdate, true)
  assert_coloncolon_constructor_children(with_trivia, [
    ("type", type_),
    ("op", op),
    ("trivia", trivia[0]),
    ("trivia", trivia[1]),
    ("body", body),
  ])
  let without_trivia = @node.CstNode::expr_record_update(
    loc~,
    source_span~,
    type_~,
    op~,
    body~,
  )
  assert_coloncolon_constructor_children(without_trivia, [
    ("type", type_),
    ("op", op),
    ("body", body),
  ])
  assert_coloncolon_constructor_children(
    @node.CstNode::expr_record_update(loc~, source_span~, body~),
    [("body", body)],
  )
}

///|
test "public missing constructor preserves explicitly supplied ranges" {
  let loc = @node.mk_loc(test_position(1), test_position(4))
  let source_span = (2, 6)
  let missing = @node.CstNode::missing(loc~, source_span~)
  @test.assert_eq(missing.kind is @node.NodeKind::Missing, true)
  @test.assert_eq(missing.loc, loc)
  @test.assert_eq(missing.source_span, source_span)
}
