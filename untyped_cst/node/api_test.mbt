///|
fn test_position(offset : Int) -> @basic.Position {
  @basic.Position::{ fname: "node_test.mbt", lnum: 1, bol: 0, cnum: offset, }
}

///|
test "typed constructors retain fields, optional children, and scalar payloads" {
  let start = test_position(0)
  let end = test_position(2)
  let loc = @node.mk_loc(start, end)
  let span = (0, 2)
  let value = @node.CstNode::name_identifier(value="x", loc~, source_span=span)
  let open = @node.CstNode::syntax_token(
    token_kind=TK_LBRACKET,
    loc~,
    source_span=span,
    entries=[],
  )
  let close = @node.CstNode::syntax_token(
    token_kind=TK_RBRACKET,
    loc~,
    source_span=span,
    entries=[],
  )
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
  let span = (0, 1)
  let token = @node.CstNode::syntax_token(
    token_kind=TK_LIDENT,
    loc~,
    source_span=span,
    entries=[Payload(@node.NodePayload::String("x"))],
  )
  @test.assert_eq(token.string_payload(), Some("x"))
  let recovered = @node.CstNode::error(loc~, source_span=span, entries=[
    @node.CstRecoveryChild::Item(
      @node.CstNode::syntax_token(token_kind=TK_PLUS, loc~, source_span=span, entries=[]),
    ),
    @node.CstRecoveryChild::Op(
      @node.CstNode::syntax_token(token_kind=TK_PLUS, loc~, source_span=span, entries=[]),
    ),
  ])
  @test.assert_eq(recovered.children[0].0, Some("item"))
  @test.assert_eq(
    recovered.children[0].1.kind is @node.NodeKind::Syntax_Unexpected(_),
    true,
  )
  @test.assert_eq(recovered.children[1].0, Some("op"))
  @test.assert_eq(
    recovered.children[1].1.kind is @node.NodeKind::Syntax_Unexpected(_),
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
  let separator = @node.CstNode::syntax_token(
    token_kind=TK_COLONCOLON,
    loc=@node.mk_loc(test_position(1), test_position(3)),
    source_span=(1, 3),
    entries=[],
  )
  let trivia = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(4), test_position(11)),
    source_span=(4, 11),
    entries=[],
  )
  let tail = @node.CstNode::syntax_token(
    token_kind=TK_LIDENT,
    loc=@node.mk_loc(test_position(12), test_position(13)),
    source_span=(12, 13),
    entries=[Payload(@node.NodePayload::String("B"))],
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
  let trivia = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc~,
    source_span=(4, 8),
    entries=[],
  )
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
    declaration.children[2].1.kind is @node.NodeKind::Syntax_Token(TK_COMMENT),
    true,
  )
}

///|
fn assert_constructor_entries(
  node : @node.CstNode,
  loc : @basic.Location,
  span : (Int, Int),
  expected : ArrayView[(String, @node.CstNode)],
) -> Unit raise {
  @test.assert_eq(node.loc, loc)
  @test.assert_eq(node.source_span, span)
  @test.assert_eq(node.children.length(), expected.length())
  for i, (label, child) in expected {
    let (actual_label, actual) = node.children[i]
    @test.assert_eq(actual_label, Some(label))
    assert_constructor_tree(actual, child)
  }
}

///|
fn assert_constructor_tree(
  actual : @node.CstNode,
  expected : @node.CstNode,
) -> Unit raise {
  @test.assert_eq(actual.kind == expected.kind, true)
  @test.assert_eq(actual.loc, expected.loc)
  @test.assert_eq(actual.source_span, expected.source_span)
  @test.assert_eq(actual.children.length(), expected.children.length())
  for i, (label, child) in actual.children {
    @test.assert_eq(label, expected.children[i].0)
    assert_constructor_tree(child, expected.children[i].1)
  }
}

///|
test "test declaration entries preserve trivia between keywords and TestName" {
  let loc = @node.mk_loc(test_position(0), test_position(24))
  let async_kw = @node.CstNode::syntax_token(
    token_kind=TK_ASYNC,
    loc=@node.mk_loc(test_position(0), test_position(5)),
    source_span=(0, 5),
    entries=[],
  )
  let first_comment = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(6), test_position(8)),
    source_span=(6, 8),
    entries=[],
  )
  let second_comment = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(9), test_position(11)),
    source_span=(9, 11),
    entries=[],
  )
  let test_kw = @node.CstNode::syntax_token(
    token_kind=TK_TEST,
    loc=@node.mk_loc(test_position(12), test_position(16)),
    source_span=(12, 16),
    entries=[],
  )
  let name = @node.CstNode::name_test(
    value="named",
    loc=@node.mk_loc(test_position(17), test_position(24)),
    source_span=(17, 24),
  )
  let declaration = @node.CstNode::impl_test(loc~, source_span=(0, 24), entries=[
    Kw(async_kw),
    Trivia(first_comment),
    Trivia(second_comment),
    Kw(test_kw),
    TestName(name),
  ])
  @test.assert_eq(declaration.kind is @node.NodeKind::Impl_Test, true)
  assert_constructor_entries(declaration, loc, (0, 24), [
    ("kw", async_kw),
    ("trivia", first_comment),
    ("trivia", second_comment),
    ("kw", test_kw),
    ("test_name", name),
  ])
  @test.assert_eq(
    declaration.first_child_with_label("test_name").unwrap().string_payload(),
    Some("named"),
  )
}

///|
test "import package entries preserve repeated trivia before the alias" {
  let loc = @node.mk_loc(test_position(0), test_position(15))
  let path = @node.CstNode::constant_string(
    value="pkg",
    loc=@node.mk_loc(test_position(0), test_position(5)),
    source_span=(0, 5),
  )
  let first_comment = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(6), test_position(8)),
    source_span=(6, 8),
    entries=[],
  )
  let second_comment = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(9), test_position(11)),
    source_span=(9, 11),
    entries=[],
  )
  let alias_ = @node.CstNode::name_package(
    value="p",
    loc=@node.mk_loc(test_position(13), test_position(15)),
    source_span=(13, 15),
  )
  let import_package = @node.CstNode::clause_import_package(
    loc~,
    source_span=(0, 15),
    entries=[
      Path(path),
      Trivia(first_comment),
      Trivia(second_comment),
      Alias(alias_),
    ],
  )
  @test.assert_eq(
    import_package.kind is @node.NodeKind::Clause_ImportPackage,
    true,
  )
  assert_constructor_entries(import_package, loc, (0, 15), [
    ("path", path),
    ("trivia", first_comment),
    ("trivia", second_comment),
    ("alias", alias_),
  ])
}

///|
test "extend item entries preserve trivia around the keyword" {
  let loc = @node.mk_loc(test_position(0), test_position(12))
  let target = @node.CstNode::name_identifier(
    value="f",
    loc=@node.mk_loc(test_position(0), test_position(1)),
    source_span=(0, 1),
  )
  let before = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(2), test_position(4)),
    source_span=(2, 4),
    entries=[],
  )
  let keyword = @node.CstNode::syntax_token(
    token_kind=TK_WITH,
    loc=@node.mk_loc(test_position(5), test_position(7)),
    source_span=(5, 7),
    entries=[],
  )
  let after = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(8), test_position(10)),
    source_span=(8, 10),
    entries=[],
  )
  let name = @node.CstNode::name_identifier(
    value="g",
    loc=@node.mk_loc(test_position(11), test_position(12)),
    source_span=(11, 12),
  )
  let item = @node.CstNode::clause_extend_item(loc~, source_span=(0, 12), entries=[
    Target(target),
    Trivia(before),
    Kw(keyword),
    Trivia(after),
    Name(name),
  ])
  @test.assert_eq(item.kind is @node.NodeKind::Clause_ExtendItem, true)
  assert_constructor_entries(item, loc, (0, 12), [
    ("target", target),
    ("trivia", before),
    ("kw", keyword),
    ("trivia", after),
    ("name", name),
  ])
}

///|
test "missing pattern entries preserve nested recovery and ordered errors" {
  let loc = @node.mk_loc(test_position(0), test_position(7))
  let token_loc = @node.mk_loc(test_position(0), test_position(1))
  let token = @node.CstNode::syntax_token(
    token_kind=TK_UNDERSCORE,
    loc=token_loc,
    source_span=(0, 1),
    entries=[],
  )
  let pattern = @node.CstNode::pattern_any(
    loc=token_loc,
    source_span=(0, 1),
    name=token,
  )
  let error_loc = @node.mk_loc(test_position(2), test_position(3))
  let bad = @node.CstNode::syntax_unexpected(
    token_kind=TK_NEWLINE,
    loc=error_loc,
    source_span=(2, 3),
    entries=[],
  )
  let error = @node.CstNode::error(loc=error_loc, source_span=(2, 3), entries=[
    Item(bad),
  ])
  let recovered_loc = @node.mk_loc(test_position(0), test_position(3))
  let recovered = @node.CstNode::pattern_missing_pattern(
    loc=recovered_loc,
    source_span=(0, 3),
    entries=[Pattern(pattern), Error(error)],
  )
  let trivia = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(4), test_position(6)),
    source_span=(4, 6),
    entries=[],
  )
  let marker = @node.CstNode::lex_scan_case_pattern_wildcard(
    loc=@node.empty_loc(test_position(7)),
    source_span=(7, 7),
  )
  let missing = @node.CstNode::pattern_missing_pattern(
    loc~,
    source_span=(0, 7),
    entries=[
      Error(error),
      Pattern(recovered),
      Trivia(trivia),
      Token(token),
      Error(error),
      LexScanCasePattern(marker),
    ],
  )
  @test.assert_eq(missing.kind is @node.NodeKind::Pattern_MissingPattern, true)
  assert_constructor_entries(missing, loc, (0, 7), [
    ("error", error),
    ("pattern", recovered),
    ("trivia", trivia),
    ("token", token),
    ("error", error),
    ("lex_scan_case_pattern", marker),
  ])
  let nested = missing.first_child_with_label("pattern").unwrap()
  @test.assert_eq(nested.kind is @node.NodeKind::Pattern_MissingPattern, true)
  assert_constructor_entries(nested, recovered_loc, (0, 3), [
    ("pattern", pattern),
    ("error", error),
  ])
  assert_constructor_entries(nested.children[0].1, token_loc, (0, 1), [
    ("name", token),
  ])
  assert_constructor_entries(nested.children[1].1, error_loc, (2, 3), [
    ("item", bad),
  ])
  let empty = @node.CstNode::pattern_missing_pattern(
    loc=@node.empty_loc(test_position(7)),
    source_span=(7, 7),
    entries=[],
  )
  assert_constructor_entries(empty, @node.empty_loc(test_position(7)), (7, 7), [])
}

///|
test "constraint, labelled block, and or pattern entries retain repeated trivia" {
  let loc = @node.mk_loc(test_position(0), test_position(24))
  let head = @node.CstNode::syntax_token(
    token_kind=TK_LIDENT,
    loc=@node.mk_loc(test_position(0), test_position(1)),
    source_span=(0, 1),
    entries=[Payload(@node.NodePayload::String("x"))],
  )
  let op = @node.CstNode::syntax_token(
    token_kind=TK_COLON,
    loc=@node.mk_loc(test_position(2), test_position(3)),
    source_span=(2, 3),
    entries=[],
  )
  let first = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(4), test_position(11)),
    source_span=(4, 11),
    entries=[],
  )
  let second = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(13), test_position(22)),
    source_span=(13, 22),
    entries=[],
  )
  let tail = @node.CstNode::syntax_token(
    token_kind=TK_LIDENT,
    loc=@node.mk_loc(test_position(23), test_position(24)),
    source_span=(23, 24),
    entries=[Payload(@node.NodePayload::String("y"))],
  )
  let constraint = @node.CstNode::expr_constraint(loc~, source_span=(0, 24), entries=[
    Expr(head),
    Op(op),
    Trivia(first),
    Trivia(second),
    Type_(tail),
  ])
  assert_constructor_entries(constraint, loc, (0, 24), [
    ("expr", head),
    ("op", op),
    ("trivia", first),
    ("trivia", second),
    ("type", tail),
  ])
  let block = @node.CstNode::expr_labelled_block(loc~, source_span=(0, 24), entries=[
    Label(head),
    Op(op),
    Trivia(first),
    Trivia(second),
    Body(tail),
  ])
  assert_constructor_entries(block, loc, (0, 24), [
    ("label", head),
    ("op", op),
    ("trivia", first),
    ("trivia", second),
    ("body", tail),
  ])
  let pattern = @node.CstNode::pattern_or(loc~, source_span=(0, 24), entries=[
    Lhs(head),
    Op(op),
    Trivia(first),
    Trivia(second),
    Rhs(tail),
  ])
  assert_constructor_entries(pattern, loc, (0, 24), [
    ("lhs", head),
    ("op", op),
    ("trivia", first),
    ("trivia", second),
    ("rhs", tail),
  ])
  for node in [constraint, block, pattern] {
    let comments = node.children_with_label("trivia")
    @test.assert_eq(comments.length(), 2)
    for comment in comments {
      @test.assert_eq(
        comment.kind is @node.NodeKind::Syntax_Token(TK_COMMENT),
        true,
      )
    }
  }
}

///|
test "public visibility entries retain repeated errors and their tokens" {
  let loc = @node.mk_loc(test_position(0), test_position(8))
  let kw = @node.CstNode::syntax_token(
    token_kind=TK_PUB,
    loc=@node.mk_loc(test_position(0), test_position(3)),
    source_span=(0, 3),
    entries=[],
  )
  let open = @node.CstNode::syntax_token(
    token_kind=TK_LPAREN,
    loc=@node.mk_loc(test_position(3), test_position(4)),
    source_span=(3, 4),
    entries=[],
  )
  let errors : Array[@node.CstNode] = []
  for i, value in ["1", "2"] {
    let start = 4 + i * 2
    let error_loc = @node.mk_loc(test_position(start), test_position(start + 1))
    let token = @node.CstNode::syntax_token(
      token_kind=TK_INT,
      loc=error_loc,
      source_span=(start, start + 1),
      entries=[Payload(@node.NodePayload::String(value))],
    )
    errors.push(
      @node.CstNode::error(loc=error_loc, source_span=(start, start + 1), entries=[
        Item(token),
      ]),
    )
  }
  let attr = @node.CstNode::missing(
    loc=@node.empty_loc(test_position(7)),
    source_span=(7, 7),
  )
  let close = @node.CstNode::syntax_token(
    token_kind=TK_RPAREN,
    loc=@node.mk_loc(test_position(7), test_position(8)),
    source_span=(7, 8),
    entries=[],
  )
  let visibility = @node.CstNode::visibility_pub(loc~, source_span=(0, 8), entries=[
    Kw(kw),
    Open(open),
    Error(errors[0]),
    Error(errors[1]),
    Attr(attr),
    Close(close),
  ])
  assert_constructor_entries(visibility, loc, (0, 8), [
    ("kw", kw),
    ("open", open),
    ("error", errors[0]),
    ("error", errors[1]),
    ("attr", attr),
    ("close", close),
  ])
  let actual = visibility.children_with_label("error")
  @test.assert_eq(actual.length(), 2)
  for i, error in actual {
    @test.assert_eq(error.kind is @node.NodeKind::Error, true)
    let token = error.first_child_with_label("item").unwrap()
    @test.assert_eq(token.kind is @node.NodeKind::Syntax_Unexpected(_), true)
    @test.assert_eq(
      token.string_payload(),
      Some(if i == 0 { "1" } else { "2" }),
    )
    @test.assert_eq(token.loc, errors[i].loc)
    @test.assert_eq(token.source_span, errors[i].source_span)
  }
}

///|
test "array assignment entries retain expanded and unexpanded lhs children" {
  let lhs_loc = @node.mk_loc(test_position(0), test_position(4))
  let name_loc = @node.mk_loc(test_position(0), test_position(2))
  let name = @node.CstNode::syntax_token(
    token_kind=TK_LIDENT,
    loc=name_loc,
    source_span=(0, 2),
    entries=[Payload(@node.NodePayload::String("xs"))],
  )
  let array = @node.CstNode::expr_ident(loc=name_loc, source_span=(0, 2), name=[
    name,
  ])
  let open = @node.CstNode::syntax_token(
    token_kind=TK_LBRACKET,
    loc=@node.mk_loc(test_position(2), test_position(3)),
    source_span=(2, 3),
    entries=[],
  )
  let index = @node.CstNode::missing(
    loc=@node.empty_loc(test_position(3)),
    source_span=(3, 3),
  )
  let close = @node.CstNode::syntax_token(
    token_kind=TK_RBRACKET,
    loc=@node.mk_loc(test_position(3), test_position(4)),
    source_span=(3, 4),
    entries=[],
  )
  let lhs = @node.CstNode::expr_array_get(
    loc=lhs_loc,
    source_span=(0, 4),
    self_=array,
    open~,
    index~,
    close~,
  )
  let op = @node.CstNode::syntax_token(
    token_kind=TK_EQUAL,
    loc=@node.mk_loc(test_position(5), test_position(6)),
    source_span=(5, 6),
    entries=[],
  )
  let rhs = @node.CstNode::syntax_token(
    token_kind=TK_LIDENT,
    loc=@node.mk_loc(test_position(7), test_position(12)),
    source_span=(7, 12),
    entries=[Payload(@node.NodePayload::String("value"))],
  )
  let loc = @node.mk_loc(test_position(0), test_position(12))
  let entries : ArrayView[@node.CstRecoveryChild] = [Lhs(lhs), Op(op), Rhs(rhs)]
  for
    node in [
      @node.CstNode::expr_array_set(loc~, source_span=(0, 12), entries~),
      @node.CstNode::expr_array_augmented_set(
        loc~,
        source_span=(0, 12),
        entries~,
      ),
    ] {
    assert_constructor_entries(node, loc, (0, 12), [
      ("lhs", lhs),
      ("op", op),
      ("rhs", rhs),
    ])
    @test.assert_eq(node.has_child_label("self"), false)
    let recovered = node.first_child_with_label("lhs").unwrap()
    @test.assert_eq(recovered.kind is @node.NodeKind::Expr_ArrayGet, true)
    assert_constructor_entries(recovered, lhs_loc, (0, 4), [
      ("self", array),
      ("open", open),
      ("index", index),
      ("close", close),
    ])
    assert_constructor_entries(recovered.children[0].1, name_loc, (0, 2), [
      ("name", name),
    ])
  }
  let entries : ArrayView[@node.CstRecoveryChild] = [
    Self_(array),
    Open(open),
    Index(index),
    Close(close),
    Op(op),
    Rhs(rhs),
  ]
  for
    node in [
      @node.CstNode::expr_array_set(loc~, source_span=(0, 12), entries~),
      @node.CstNode::expr_array_augmented_set(
        loc~,
        source_span=(0, 12),
        entries~,
      ),
    ] {
    assert_constructor_entries(node, loc, (0, 12), [
      ("self", array),
      ("open", open),
      ("index", index),
      ("close", close),
      ("op", op),
      ("rhs", rhs),
    ])
    @test.assert_eq(node.has_child_label("lhs"), false)
  }
}

///|
test "field assignment entry constructor preserves recovery and legacy calls" {
  // Locations count characters; spans count UTF-16 code units after a non-BMP prefix.
  let base_loc = @node.mk_loc(test_position(1), test_position(2))
  let name = @node.CstNode::name_identifier(
    value="x",
    loc=base_loc,
    source_span=(2, 3),
  )
  let self_ = @node.CstNode::expr_ident(loc=base_loc, source_span=(2, 3), name=[
    name,
  ])
  let field_loc = @node.mk_loc(test_position(2), test_position(8))
  let token = @node.CstNode::syntax_token(
    token_kind=TK_DOT_LIDENT,
    loc=field_loc,
    source_span=(3, 9),
    entries=[],
  )
  let field = @node.CstNode::accessor_label(
    value="field",
    loc=field_loc,
    source_span=(3, 9),
  )
  let lhs = @node.CstNode::expr_field(
    loc=@node.mk_loc(test_position(1), test_position(8)),
    source_span=(2, 9),
    self_~,
    token~,
    field~,
  )
  let op_loc = @node.mk_loc(test_position(9), test_position(11))
  let op = @node.CstNode::syntax_token(
    token_kind=TK_PLUS_EQUAL,
    loc=op_loc,
    source_span=(10, 12),
    entries=[],
  )
  let operator = @node.CstNode::operator_augmented(
    value="+=",
    loc=op_loc,
    source_span=(10, 12),
  )
  let rhs = @node.CstNode::expr_missing_expr(
    loc=@node.empty_loc(test_position(11)),
    source_span=(12, 12),
    error=[],
  )
  let augmented = @node.CstNode::flag_augmented(
    value=true,
    loc=rhs.loc,
    source_span=rhs.source_span,
  )
  let loc = @node.mk_loc(test_position(1), test_position(11))
  let span = (2, 12)
  for augmented in [None, Some(augmented)] {
    let legacy = @node.CstNode::expr_mutate(
      loc~,
      source_span=span,
      self_~,
      token~,
      field~,
      op~,
      operator~,
      rhs~,
      augmented?,
    )
    let entries : Array[@node.CstRecoveryChild] = [
      Self_(self_),
      Token(token),
      Field(field),
      Op(op),
      Operator(operator),
      Rhs(rhs),
    ]
    let expected = [
      ("self", self_),
      ("token", token),
      ("field", field),
      ("op", op),
      ("operator", operator),
      ("rhs", rhs),
    ]
    if augmented is Some(child) {
      entries.push(Augmented(child))
      expected.push(("augmented", child))
    }
    assert_constructor_entries(legacy, loc, span, expected)
    let ordered = @node.CstNode::expr_mutate_from_entries(
      loc~,
      source_span=span,
      entries~,
    )
    @test.assert_eq(ordered.kind is @node.NodeKind::Expr_Mutate, true)
    assert_constructor_tree(ordered, legacy)
  }
  let recovered = @node.CstNode::expr_mutate_from_entries(
    loc~,
    source_span=span,
    entries=[
      Lhs(lhs),
      Op(op),
      Operator(operator),
      Rhs(rhs),
      Augmented(augmented),
    ],
  )
  @test.assert_eq(recovered.kind is @node.NodeKind::Expr_Mutate, true)
  assert_constructor_entries(recovered, loc, span, [
    ("lhs", lhs),
    ("op", op),
    ("operator", operator),
    ("rhs", rhs),
    ("augmented", augmented),
  ])
  @test.assert_eq(recovered.has_child_label("self"), false)
}

///|
test "missing expression entries retain repeated trivia and errors" {
  let loc = @node.mk_loc(test_position(1), test_position(9))
  let span = (2, 10)
  let label = @node.CstNode::syntax_token(
    token_kind=TK_POST_LABEL,
    loc=@node.mk_loc(test_position(1), test_position(3)),
    source_span=(2, 4),
    entries=[],
  )
  let first = @node.CstNode::syntax_unexpected(
    token_kind=TK_NEWLINE,
    loc=@node.mk_loc(test_position(3), test_position(4)),
    source_span=(4, 5),
    entries=[],
  )
  let second = @node.CstNode::syntax_unexpected(
    token_kind=TK_NEWLINE,
    loc=@node.mk_loc(test_position(4), test_position(5)),
    source_span=(5, 6),
    entries=[],
  )
  let op = @node.CstNode::syntax_token(
    token_kind=TK_PLUS,
    loc=@node.mk_loc(test_position(5), test_position(6)),
    source_span=(6, 7),
    entries=[],
  )
  let error = @node.CstNode::error(loc=op.loc, source_span=op.source_span, entries=[
    Item(op),
  ])
  let expr = @node.CstNode::expr_missing_expr(
    loc=@node.mk_loc(test_position(6), test_position(9)),
    source_span=(7, 10),
    error=[],
  )
  let legacy = @node.CstNode::expr_missing_expr(
    loc~,
    source_span=span,
    label~,
    trivia=first,
    expr~,
    op~,
    error=[error, error],
  )
  assert_constructor_entries(legacy, loc, span, [
    ("label", label),
    ("trivia", first),
    ("expr", expr),
    ("op", op),
    ("error", error),
    ("error", error),
  ])
  let ordered = @node.CstNode::expr_missing_expr_from_entries(
    loc~,
    source_span=span,
    entries=[
      Label(label),
      Error(error),
      Trivia(first),
      Expr(expr),
      Trivia(second),
      Op(op),
      Error(error),
    ],
  )
  @test.assert_eq(ordered.kind is @node.NodeKind::Expr_MissingExpr, true)
  assert_constructor_entries(ordered, loc, span, [
    ("label", label),
    ("error", error),
    ("trivia", first),
    ("expr", expr),
    ("trivia", second),
    ("op", op),
    ("error", error),
  ])
  let empty = @node.CstNode::expr_missing_expr_from_entries(
    loc=@node.mk_loc(test_position(6), test_position(9)),
    source_span=(7, 10),
    entries=[],
  )
  assert_constructor_tree(empty, expr)
}

///|
test "positional constructor parameters keep optional colon order and legacy calls" {
  let loc = @node.mk_loc(test_position(1), test_position(10))
  let span = (2, 11)
  let mut_ = @node.CstNode::syntax_token(
    token_kind=TK_MUTABLE,
    loc=@node.mk_loc(test_position(1), test_position(4)),
    source_span=(2, 5),
    entries=[],
  )
  let op = @node.CstNode::syntax_token(
    token_kind=TK_COLON,
    loc=@node.mk_loc(test_position(5), test_position(6)),
    source_span=(6, 7),
    entries=[],
  )
  let type_ = @node.CstNode::type_name(
    loc=@node.mk_loc(test_position(7), test_position(10)),
    source_span=(8, 11),
    name=[],
    error=[],
  )
  let mutable = @node.CstNode::flag_mutable(
    value=true,
    loc=@node.empty_loc(test_position(10)),
    source_span=(11, 11),
  )
  let plain = @node.CstNode::parameter_constructor_positional(
    loc~,
    source_span=span,
    type_~,
  )
  assert_constructor_entries(plain, loc, span, [("type", type_)])
  let legacy = @node.CstNode::parameter_constructor_positional(
    loc~,
    source_span=span,
    mut_~,
    type_~,
    mutable~,
  )
  assert_constructor_entries(legacy, loc, span, [
    ("mut", mut_),
    ("type", type_),
    ("mutable", mutable),
  ])
  let recovered = @node.CstNode::parameter_constructor_positional(
    loc~,
    source_span=span,
    mut_~,
    op~,
    type_~,
    mutable~,
  )
  @test.assert_eq(
    recovered.kind is @node.NodeKind::Parameter_ConstructorPositional,
    true,
  )
  assert_constructor_entries(recovered, loc, span, [
    ("mut", mut_),
    ("op", op),
    ("type", type_),
    ("mutable", mutable),
  ])
  let without_mut = @node.CstNode::parameter_constructor_positional(
    loc~,
    source_span=span,
    op~,
    type_~,
  )
  assert_constructor_entries(without_mut, loc, span, [
    ("op", op),
    ("type", type_),
  ])
}
