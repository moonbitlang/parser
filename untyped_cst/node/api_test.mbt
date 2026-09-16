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

///|
test "legacy type application entries retain head, repeated tokens, and trivia" {
  let loc = @node.mk_loc(test_position(0), test_position(13))
  let head = @node.CstNode::type_name(
    loc=@node.mk_loc(test_position(0), test_position(1)),
    source_span=(0, 1),
    name=[],
    error=[],
  )
  let separator = @node.CstNode::syntax_separator(
    loc=@node.mk_loc(test_position(1), test_position(3)),
    source_span=(1, 3),
  )
  let trivia = @node.CstNode::syntax_comment(
    loc=@node.mk_loc(test_position(4), test_position(11)),
    source_span=(4, 11),
  )
  let tail = @node.CstNode::syntax_identifier(
    loc=@node.mk_loc(test_position(12), test_position(13)),
    source_span=(12, 13),
    payload=@node.NodePayload::String("B"),
  )
  let application = @node.CstNode::type_application(loc~, source_span=(0, 13), entries=[
    Head(head),
    Item(separator),
    Trivia(trivia),
    Item(tail),
  ])
  @test.assert_eq(application.children.length(), 4)
  @test.assert_eq(application.children[0].0, Some("head"))
  @test.assert_eq(application.children[1].0, Some("item"))
  @test.assert_eq(application.children[2].0, Some("trivia"))
  @test.assert_eq(application.children[3].0, Some("item"))
  @test.assert_eq(application.children[2].1.source_span, (4, 11))
  @test.assert_eq(application.children[2].1.loc, trivia.loc)
  @test.assert_eq(application.children[3].1.string_payload(), Some("B"))
  @test.assert_eq(application.has_child_label("func"), false)
  @test.assert_eq(application.has_child_label("arg"), false)
  @test.assert_eq(application.loc, loc)
  @test.assert_eq(application.source_span, (0, 13))
}

///|
test "trait method entries preserve repeated clauses around trivia" {
  let loc = @node.mk_loc(test_position(0), test_position(12))
  let name = @node.CstNode::name_identifier(value="f", loc~, source_span=(0, 1))
  let trivia = @node.CstNode::syntax_comment(loc~, source_span=(4, 8))
  let first_clause = @node.CstNode::clause_return_type(
    loc~,
    source_span=(2, 3),
    entries=[],
  )
  let last_clause = @node.CstNode::clause_return_type(
    loc~,
    source_span=(9, 12),
    entries=[],
  )
  let declaration = @node.CstNode::trait_method_declaration(
    loc~,
    source_span=(0, 12),
    entries=[
      Name(name),
      Clause(first_clause),
      Trivia(trivia),
      Clause(last_clause),
    ],
  )
  @test.assert_eq(declaration.children.length(), 4)
  @test.assert_eq(declaration.children[0].0, Some("name"))
  @test.assert_eq(declaration.children[1].0, Some("clause"))
  @test.assert_eq(declaration.children[2].0, Some("trivia"))
  @test.assert_eq(declaration.children[3].0, Some("clause"))
  let clauses = declaration.children_with_label("clause")
  @test.assert_eq(clauses.length(), 2)
  @test.assert_eq(clauses[0].source_span, (2, 3))
  @test.assert_eq(clauses[1].source_span, (9, 12))
  @test.assert_eq(
    declaration.children[2].1.kind is @node.NodeKind::Syntax_Comment,
    true,
  )
}
