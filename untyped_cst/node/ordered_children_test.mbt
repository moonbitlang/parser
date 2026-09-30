///|
test "ordered constructors preserve repeated labels, trivia, and recovery nodes" {
  let loc = @node.mk_loc(test_position(1), test_position(24))
  let span = (2, 26)
  let name = @node.CstNode::name_identifier(
    value="A",
    loc=@node.mk_loc(test_position(1), test_position(2)),
    source_span=(2, 3),
  )
  let op = @node.CstNode::syntax_token(
    token_kind=TK_PLUS,
    loc=@node.mk_loc(test_position(3), test_position(4)),
    source_span=(4, 5),
    entries=[],
  )
  let first = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(5), test_position(12)),
    source_span=(6, 13),
    entries=[],
  )
  let second = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc=@node.mk_loc(test_position(13), test_position(22)),
    source_span=(14, 24),
    entries=[],
  )
  let pkg = @node.CstNode::name_package(
    value="p",
    loc=@node.mk_loc(test_position(22), test_position(24)),
    source_span=(24, 26),
  )
  let missing = @node.CstNode::missing(
    loc=@node.empty_loc(test_position(24)),
    source_span=(26, 26),
  )
  let error = @node.CstNode::error(loc=op.loc, source_span=op.source_span, entries=[
    Item(op),
  ])
  // Deliberately revisit earlier source positions and repeat identical children:
  // the entry order must survive without sorting or deduplication.
  let binding = @node.CstNode::binding_regex_from_entries(
    loc~,
    source_span=span,
    entries=[
      Name(name),
      Trivia(first),
      Op(op),
      Error(error),
      Trivia(second),
      Binder(missing),
      Trivia(first),
      Op(op),
      SemanticLabel(name),
      SemanticBinder(missing),
    ],
  )
  @test.assert_eq(binding.kind is @node.NodeKind::Binding_Regex, true)
  assert_constructor_entries(binding, loc, span, [
    ("name", name),
    ("trivia", first),
    ("op", op),
    ("error", error),
    ("trivia", second),
    ("binder", missing),
    ("trivia", first),
    ("op", op),
    ("semantic_label", name),
    ("semantic_binder", missing),
  ])
  let parameter = @node.CstNode::parameter_type_decl_from_entries(
    loc~,
    source_span=span,
    entries=[
      Name(name),
      Op(op),
      Type_(name),
      Trivia(first),
      Op(op),
      Error(error),
      Trivia(second),
      Type_(missing),
      Error(error),
    ],
  )
  @test.assert_eq(parameter.kind is @node.NodeKind::Parameter_TypeDecl, true)
  assert_constructor_entries(parameter, loc, span, [
    ("name", name),
    ("op", op),
    ("type", name),
    ("trivia", first),
    ("op", op),
    ("error", error),
    ("trivia", second),
    ("type", missing),
    ("error", error),
  ])
  let method_ref = @node.CstNode::expr_method_from_entries(
    loc~,
    source_span=span,
    entries=[
      Type_(name),
      Op(op),
      Trivia(first),
      Pkg(pkg),
      Error(error),
      Trivia(second),
      Field(missing),
      Pkg(pkg),
      Error(error),
    ],
  )
  @test.assert_eq(method_ref.kind is @node.NodeKind::Expr_Method, true)
  assert_constructor_entries(method_ref, loc, span, [
    ("type", name),
    ("op", op),
    ("trivia", first),
    ("pkg", pkg),
    ("error", error),
    ("trivia", second),
    ("field", missing),
    ("pkg", pkg),
    ("error", error),
  ])
  let constructor_decl = @node.CstNode::clause_constructor_decl_from_entries(
    loc~,
    source_span=span,
    entries=[
      Attrs(name),
      Trivia(first),
      Attrs(name),
      Trivia(second),
      Vis(op),
      Error(error),
      Trivia(first),
      Name(name),
      Error(error),
      Name(missing),
    ],
  )
  @test.assert_eq(
    constructor_decl.kind is @node.NodeKind::Clause_ConstructorDecl,
    true,
  )
  assert_constructor_entries(constructor_decl, loc, span, [
    ("attrs", name),
    ("trivia", first),
    ("attrs", name),
    ("trivia", second),
    ("vis", op),
    ("error", error),
    ("trivia", first),
    ("name", name),
    ("error", error),
    ("name", missing),
  ])
  let with_pattern = @node.CstNode::pattern_with_from_entries(
    loc~,
    source_span=span,
    entries=[
      Open(op),
      Trivia(first),
      Trivia(second),
      Pattern(name),
      Op(op),
      Error(error),
      Pattern(missing),
      Trivia(first),
      Body(name),
      Close(op),
      Error(error),
    ],
  )
  @test.assert_eq(with_pattern.kind is @node.NodeKind::Pattern_With, true)
  assert_constructor_entries(with_pattern, loc, span, [
    ("open", op),
    ("trivia", first),
    ("trivia", second),
    ("pattern", name),
    ("op", op),
    ("error", error),
    ("pattern", missing),
    ("trivia", first),
    ("body", name),
    ("close", op),
    ("error", error),
  ])
  for
    empty in [
      @node.CstNode::binding_regex_from_entries(loc~, source_span=span, entries=[]),
      @node.CstNode::parameter_type_decl_from_entries(loc~, source_span=span, entries=[]),
      @node.CstNode::expr_method_from_entries(loc~, source_span=span, entries=[]),
      @node.CstNode::clause_constructor_decl_from_entries(
        loc~,
        source_span=span,
        entries=[],
      ),
      @node.CstNode::pattern_with_from_entries(loc~, source_span=span, entries=[]),
    ] {
    assert_constructor_entries(empty, loc, span, [])
  }
}

///|
test "multiline constructor preserves repeated trivia and entry order" {
  let loc = @node.mk_loc(test_position(0), test_position(19))
  let span = (0, 19)
  let token = @node.CstNode::syntax_token(
    token_kind=TK_MULTILINE_STRING,
    loc=@node.mk_loc(test_position(0), test_position(3)),
    source_span=(0, 3),
    entries=[],
  )
  let element = @node.CstNode::multiline_string_element_string(
    value="a",
    loc=token.loc,
    source_span=token.source_span,
  )
  let trivia = [
    for (start, value) in [(4, "// keep"), (12, "// next")] => {
      let loc = @node.mk_loc(test_position(start), test_position(start + 7))
      let source_span = (start, start + 7)
      @node.CstNode::syntax_token(token_kind=TK_COMMENT, loc~, source_span~, entries=[
        Comment(@node.CstNode::comment_content(value~, loc~, source_span~)),
      ])
    }
  ]
  // Revisit earlier positions and repeat a node to catch sorting or deduplication.
  let multiline = @node.CstNode::expr_multiline_string(loc~, source_span=span, entries=[
    Token(token),
    Element(element),
    Trivia(trivia[1]),
    Trivia(trivia[0]),
    Trivia(trivia[0]),
    Trivia(trivia[1]),
  ])
  @test.assert_eq(multiline.kind is @node.NodeKind::Expr_MultilineString, true)
  assert_constructor_entries(multiline, loc, span, [
    ("token", token),
    ("element", element),
    ("trivia", trivia[1]),
    ("trivia", trivia[0]),
    ("trivia", trivia[0]),
    ("trivia", trivia[1]),
  ])
}

///|
test "ordered constructors match legacy required and optional arguments" {
  let (type_, op, trivia, field) = coloncolon_constructor_nodes()
  let loc = type_.loc.merge(field.loc)
  let span = (0, 23)
  let semantic_label = @node.CstNode::name_identifier(
    value="before",
    loc=type_.loc,
    source_span=type_.source_span,
  )
  let semantic_binder = @node.CstNode::name_identifier(
    value="head",
    loc=field.loc,
    source_span=field.source_span,
  )
  let error = @node.CstNode::error(
    loc=field.loc,
    source_span=field.source_span,
    entries=[Item(field)],
  )
  for full in [false, true] {
    let binding_entries : Array[@node.CstRecoveryChild] = [Name(type_)]
    let parameter_entries : Array[@node.CstRecoveryChild] = [Name(type_)]
    let method_entries : Array[@node.CstRecoveryChild] = [Type_(type_), Op(op)]
    let constructor_entries : Array[@node.CstRecoveryChild] = []
    if full {
      binding_entries.append([
        Op(op),
        Binder(field),
        SemanticLabel(semantic_label),
        SemanticBinder(semantic_binder),
      ])
      parameter_entries.append([Op(op), Type_(field)])
      method_entries.append([
        Trivia(trivia[0]),
        Trivia(trivia[1]),
        Error(error),
        Field(field),
      ])
      constructor_entries.append([
        Attrs(trivia[0]),
        Attrs(trivia[1]),
        Error(error),
        Vis(op),
        Name(type_),
        Params(field),
        Op(op),
        Tag(field),
        ConstructorTag(field),
        SemanticName(semantic_label),
      ])
    }
    let legacy_binding = if full {
      @node.CstNode::binding_regex(
        loc~,
        source_span=span,
        name=type_,
        op~,
        binder=field,
        semantic_label~,
        semantic_binder~,
      )
    } else {
      @node.CstNode::binding_regex(loc~, source_span=span, name=type_)
    }
    let legacy_parameter = if full {
      @node.CstNode::parameter_type_decl(
        loc~,
        source_span=span,
        name=type_,
        op~,
        type_=field,
      )
    } else {
      @node.CstNode::parameter_type_decl(loc~, source_span=span, name=type_)
    }
    let legacy_method = if full {
      @node.CstNode::expr_method(
        loc~,
        source_span=span,
        type_~,
        op~,
        trivia~,
        error~,
        field~,
      )
    } else {
      @node.CstNode::expr_method(loc~, source_span=span, type_~, op~)
    }
    let legacy_constructor = if full {
      @node.CstNode::clause_constructor_decl(
        loc~,
        source_span=span,
        attrs=trivia,
        error~,
        vis=op,
        name=type_,
        params=field,
        op~,
        tag=field,
        constructor_tag=field,
        semantic_name=semantic_label,
      )
    } else {
      @node.CstNode::clause_constructor_decl(loc~, source_span=span, attrs=[])
    }
    for
      (ordered, legacy) in [
        (
          @node.CstNode::binding_regex_from_entries(
            loc~,
            source_span=span,
            entries=binding_entries[:],
          ),
          legacy_binding,
        ),
        (
          @node.CstNode::parameter_type_decl_from_entries(
            loc~,
            source_span=span,
            entries=parameter_entries[:],
          ),
          legacy_parameter,
        ),
        (
          @node.CstNode::expr_method_from_entries(
            loc~,
            source_span=span,
            entries=method_entries[:],
          ),
          legacy_method,
        ),
        (
          @node.CstNode::clause_constructor_decl_from_entries(
            loc~,
            source_span=span,
            entries=constructor_entries[:],
          ),
          legacy_constructor,
        ),
      ] {
      assert_constructor_tree(ordered, legacy)
    }
  }
  assert_constructor_tree(
    @node.CstNode::pattern_with_from_entries(loc~, source_span=span, entries=[
      Open(op),
      Pattern(type_),
      Op(op),
      Body(field),
      Close(op),
    ]),
    @node.CstNode::pattern_with(
      loc~,
      source_span=span,
      open=op,
      pattern=type_,
      op~,
      body=field,
      close=op,
    ),
  )
}

///|
test "pattern entry constructors preserve all labels and repeated children" {
  let loc = @node.mk_loc(test_position(1), test_position(8))
  let source_span = (2, 10)
  let trivia = @node.CstNode::syntax_token(
    token_kind=TK_COMMENT,
    loc~,
    source_span~,
    entries=[],
  )
  let body = @node.CstNode::missing(
    loc=@node.empty_loc(loc.end),
    source_span=(10, 10),
  )
  let marker = @node.CstNode::lex_scan_case_pattern_pattern(
    loc=body.loc,
    source_span=body.source_span,
  )
  let entries : Array[@node.CstRecoveryChild] = [
    Trivia(trivia),
    Trivia(trivia),
    Body(body),
    Error(body),
    Body(body),
    LexScanCasePattern(marker),
  ]
  for
    (node, expected_kind) in [
      (
        @node.CstNode::pattern_record_from_entries(loc~, source_span~, entries~),
        "Pattern_Record",
      ),
      (
        @node.CstNode::pattern_map_from_entries(loc~, source_span~, entries~),
        "Pattern_Map",
      ),
      (
        @node.CstNode::pattern_alias_from_entries(loc~, source_span~, entries~),
        "Pattern_Alias",
      ),
      (
        @node.CstNode::pattern_range_from_entries(loc~, source_span~, entries~),
        "Pattern_Range",
      ),
      (
        @node.CstNode::regex_pattern_sequence_from_entries(
          loc~,
          source_span~,
          entries~,
        ),
        "RegexPattern_Sequence",
      ),
      (
        @node.CstNode::regex_pattern_alternation_from_entries(
          loc~,
          source_span~,
          entries~,
        ),
        "RegexPattern_Alternation",
      ),
    ] {
    @test.assert_eq("\{Repr(node.kind)}", expected_kind)
    assert_constructor_entries(node, loc, source_span, [
      ("trivia", trivia),
      ("trivia", trivia),
      ("body", body),
      ("error", body),
      ("body", body),
      ("lex_scan_case_pattern", marker),
    ])
  }
}
