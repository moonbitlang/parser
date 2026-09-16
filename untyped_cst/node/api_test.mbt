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
    @test.assert_eq(actual.loc, child.loc)
    @test.assert_eq(actual.source_span, child.source_span)
    @test.assert_eq(actual.string_payload(), child.string_payload())
    @test.assert_eq(actual.children.length(), child.children.length())
  }
}

///|
test "test declaration entries preserve trivia between keywords and TestName" {
  let loc = @node.mk_loc(test_position(0), test_position(24))
  let async_kw = @node.CstNode::syntax_keyword(
    loc=@node.mk_loc(test_position(0), test_position(5)),
    source_span=(0, 5),
  )
  let first_comment = @node.CstNode::syntax_comment(
    loc=@node.mk_loc(test_position(6), test_position(8)),
    source_span=(6, 8),
  )
  let second_comment = @node.CstNode::syntax_comment(
    loc=@node.mk_loc(test_position(9), test_position(11)),
    source_span=(9, 11),
  )
  let test_kw = @node.CstNode::syntax_keyword(
    loc=@node.mk_loc(test_position(12), test_position(16)),
    source_span=(12, 16),
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
  let first_comment = @node.CstNode::syntax_comment(
    loc=@node.mk_loc(test_position(6), test_position(8)),
    source_span=(6, 8),
  )
  let second_comment = @node.CstNode::syntax_comment(
    loc=@node.mk_loc(test_position(9), test_position(11)),
    source_span=(9, 11),
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
  let before = @node.CstNode::syntax_comment(
    loc=@node.mk_loc(test_position(2), test_position(4)),
    source_span=(2, 4),
  )
  let keyword = @node.CstNode::syntax_keyword(
    loc=@node.mk_loc(test_position(5), test_position(7)),
    source_span=(5, 7),
  )
  let after = @node.CstNode::syntax_comment(
    loc=@node.mk_loc(test_position(8), test_position(10)),
    source_span=(8, 10),
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
  let token = @node.CstNode::syntax_identifier(
    loc=token_loc,
    source_span=(0, 1),
    payload=@node.NodePayload::String("_"),
  )
  let pattern = @node.CstNode::pattern_any(
    loc=token_loc,
    source_span=(0, 1),
    name=token,
  )
  let error_loc = @node.mk_loc(test_position(2), test_position(3))
  let bad = @node.CstNode::syntax_unexpected(loc=error_loc, source_span=(2, 3))
  let error = @node.CstNode::error(loc=error_loc, source_span=(2, 3), entries=[
    Item(bad),
  ])
  let recovered_loc = @node.mk_loc(test_position(0), test_position(3))
  let recovered = @node.CstNode::pattern_missing_pattern(
    loc=recovered_loc,
    source_span=(0, 3),
    entries=[Pattern(pattern), Error(error)],
  )
  let trivia = @node.CstNode::syntax_comment(
    loc=@node.mk_loc(test_position(4), test_position(6)),
    source_span=(4, 6),
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
