///|
fn refactor_find_node(
  source : String,
  pred : (@untyped_cst.NodeKind) -> Bool,
  expression? : Bool = false,
) -> @untyped_cst.CstNode raise {
  let parsed = if expression {
    @untyped_cst.parse_expression(source)
  } else {
    @untyped_cst.parse_structure(source)
  }
  @test.assert_eq(parsed.diagnostics_view().length(), 0)
  guard api_find_node(parsed.root(), node => pred(node.kind)) is Some(node) else {
    fail("expected CST node in \{source}")
  }
  node
}

///|
fn refactor_labels(node : @untyped_cst.CstNode) -> Array[String?] {
  let labels : Array[String?] = []
  for (label, _) in node.children {
    labels.push(label)
  }
  labels
}

///|
fn refactor_assert_comment(
  node : @untyped_cst.CstNode,
  source : String,
  prefix : String,
) -> Unit raise {
  guard node.first_child_with_label("trivia") is Some(comment) else {
    fail("expected comment in \{source}")
  }
  @test.assert_eq(
    comment.kind is @untyped_cst.NodeKind::Syntax_Token(TK_COMMENT),
    true,
  )
  @test.assert_eq(comment.text(source), "// keep"[:])
  @test.assert_eq(comment.source_span, (prefix.length(), prefix.length() + 7))
  @test.assert_eq(comment.loc.start.cnum, prefix.length())
  @test.assert_eq(comment.loc.end.cnum, prefix.length() + 7)
  @test.assert_eq(comment.loc.start.lnum, 1)
  @test.assert_eq(comment.loc.end.lnum, 1)
}

///|
test "type trivia retains comments, source order, and UTF-16 spans" {
  let source = "fn f(x : // keep 🐇\n // second\n Int) {}"
  let group = refactor_find_node(source, kind => kind is Type_Group)
  @test.assert_eq(refactor_labels(group), [
    Some("trivia"),
    Some("trivia"),
    Some("type"),
  ])
  let first = group.children[0].1
  let second = group.children[1].1
  let ty = group.children[2].1
  @test.assert_eq(
    first.kind is @untyped_cst.NodeKind::Syntax_Token(TK_COMMENT),
    true,
  )
  @test.assert_eq(
    second.kind is @untyped_cst.NodeKind::Syntax_Token(TK_COMMENT),
    true,
  )
  @test.assert_eq(first.text(source), "// keep 🐇"[:])
  @test.assert_eq(second.text(source), "// second"[:])
  @test.assert_eq(first.source_span, (9, 19))
  @test.assert_eq((first.loc.start.cnum, first.loc.end.cnum), (9, 18))
  @test.assert_eq(second.source_span, (21, 30))
  @test.assert_eq(second.loc.start.lnum, 2)
  @test.assert_eq(ty.text(source), "Int"[:])
  @test.assert_eq(ty.source_span, (32, 35))
  @test.assert_eq(ty.loc.start.lnum, 3)
  @test.assert_eq(group.source_span, (9, 35))
  assert_handrolled_cst_ast_equiv(source)
}

///|
test "if trivia stays between the body and else branches" {
  let prefix = "if true { 1 } "
  let source = prefix + "// keep\n else { 2 }"
  let node = refactor_find_node(
    source,
    kind => kind is Expr_If,
    expression=true,
  )
  @test.assert_eq(refactor_labels(node), [
    Some("kw"),
    Some("cond"),
    Some("body"),
    Some("trivia"),
    Some("else"),
    Some("else"),
  ])
  refactor_assert_comment(node, source, prefix)
  @test.assert_eq(node.children[4].1.text(source), "else"[:])
  @test.assert_eq(node.children[5].1.text(source), "{ 2 }"[:])
  assert_expr_ast_equiv(source)
}

///|
test "all parameter forms retain trailing type comments" {
  let cases : Array[(String, (@untyped_cst.NodeKind) -> Bool, Array[String?])] = [
    (
      "x",
      kind => kind is Parameter_Positional,
      [Some("pattern"), Some("op"), Some("type"), Some("trivia")],
    ),
    (
      "_",
      kind => kind is Parameter_DiscardPositional,
      [Some("pattern"), Some("op"), Some("type"), Some("trivia")],
    ),
    (
      "x~",
      kind => kind is Parameter_Labelled,
      [Some("pattern"), Some("op"), Some("type"), Some("trivia")],
    ),
    (
      "x?",
      kind => kind is Parameter_QuestionOptional,
      [
        Some("pattern"),
        Some("question"),
        Some("op"),
        Some("type"),
        Some("trivia"),
      ],
    ),
  ]
  for (name, pred, labels) in cases {
    let prefix = "fn f(\{name} : Int "
    let source = prefix + "// keep\n) {}"
    let node = refactor_find_node(source, pred)
    @test.assert_eq(refactor_labels(node), labels)
    refactor_assert_comment(node, source, prefix)
    assert_handrolled_cst_ast_equiv(source)
  }
}

///|
test "qualified constructors retain trivia before the field" {
  let source = "A:: // keep\n B"
  let node = refactor_find_node(
    source,
    kind => kind is Expr_Constr,
    expression=true,
  )
  @test.assert_eq(refactor_labels(node), [
    Some("type"),
    Some("op"),
    Some("trivia"),
    Some("field"),
    Some("semantic_name"),
    Some("constructor_extra_info"),
  ])
  refactor_assert_comment(node, source, "A:: ")
  @test.assert_eq(
    node.first_child_with_label("field").unwrap().text(source),
    "B"[:],
  )
  assert_expr_ast_equiv(source)
}

///|
test "trait methods retain trivia between the keyword and name" {
  let prefix = "trait T { fn "
  let source = prefix + "// keep\n f(Self) -> Int }"
  let node = refactor_find_node(source, kind => kind is TraitMethod_Declaration)
  @test.assert_eq(refactor_labels(node), [
    Some("kw"),
    Some("trivia"),
    Some("name"),
    Some("params"),
    Some("clause"),
    Some("semantic_name"),
    Some("async"),
  ])
  refactor_assert_comment(node, source, prefix)
  assert_handrolled_cst_ast_equiv(source)
}

///|
test "pattern trivia wrappers retain their original shape" {
  let prefix = "{ let "
  let source = prefix + "// keep\n x = 1; x }"
  let node = refactor_find_node(
    source,
    kind => kind is Pattern_Constraint,
    expression=true,
  )
  @test.assert_eq(refactor_labels(node), [Some("trivia"), Some("pattern")])
  refactor_assert_comment(node, source, prefix)
  @test.assert_eq(node.children[1].1.text(source), "x"[:])
  // Keep the pre-refactor wrapper's lowering, including its unconstrained type.
  let (expr, reports) = @untyped_cst.parse_expression(source).to_expr()
  @test.assert_eq(reports.length(), 0)
  guard expr
    is Some(@syntax.Expr::Group(expr=@syntax.Expr::Let(pattern~, ..), ..)) else {
    fail("expected grouped let expression")
  }
  guard pattern
    is @syntax.Pattern::Constraint(
      pat=@syntax.Pattern::Var(binder),
      ty=@syntax.Type::Any(loc=type_loc),
      loc~
    ) else {
    fail("expected the pre-refactor pattern wrapper")
  }
  @test.assert_eq(binder.name, "x")
  @test.assert_eq(binder.loc, node.children[1].1.loc)
  @test.assert_eq(loc, node.loc)
  @test.assert_eq(type_loc, node.loc)
}

///|
test "legacy type tails retain their head and lowering behavior" {
  // These inputs exercise an existing permissive CST path, not modern type syntax.
  for tail in ["A::B", "A::B[Int]", "A:: // keep\n B"] {
    let source = "fn f(x : \{tail}) {}"
    let parsed = @untyped_cst.parse_structure(source)
    @test.assert_eq(parsed.diagnostics_view().length(), 0)
    let node = refactor_find_node(source, kind => kind is Type_Application)
    @test.assert_eq(node.children[0].0, Some("head"))
    @test.assert_eq(node.children[0].1.text(source), "A"[:])
    @test.assert_eq(node.children[1].1.text(source), "::"[:])
    @test.assert_eq(node.text(source), tail[:])
    @test.assert_eq(node.source_span, (9, 9 + tail.length()))
    @test.assert_eq(node.has_child_label("func"), false)
    @test.assert_eq(node.has_child_label("arg"), false)
    @test.assert_eq(api_has_kind(node, kind => kind is Missing), false)
    if tail == "A::B" {
      @test.assert_eq(refactor_labels(node), [
        Some("head"),
        Some("item"),
        Some("item"),
      ])
    }
    let (impls, reports) = parsed.to_impls()
    @test.assert_eq(reports.length(), 0)
    guard impls.to_array() is [@syntax.Impl::TopFuncDef(fun_decl~, ..)] else {
      fail("expected function")
    }
    guard fun_decl.decl_params is Some(params) else {
      fail("expected parameters")
    }
    guard params.to_array() is [@syntax.Parameter::Positional(ty=Some(ty), ..)] else {
      fail("expected typed parameter")
    }
    guard ty is @syntax.Type::Name(constr_id~, tys~, loc~) else {
      fail("legacy type head must not lower to Any")
    }
    @test.assert_eq(constr_id.id is @syntax.LongIdent::Ident(name="A"), true)
    @test.assert_eq(tys.length(), 0)
    @test.assert_eq(loc, node.loc)
  }
}

///|
test "question postfix patterns retain the pattern and operator" {
  // Preserve pre-refactor lowering for this permissive CST path.
  for pattern in ["_", "y"] {
    let source = "match x { \{pattern}? => 0 }"
    let parsed = @untyped_cst.parse_expression(source)
    @test.assert_eq(parsed.diagnostics_view().length(), 0)
    let node = refactor_find_node(
      source,
      kind => kind is Pattern_Var,
      expression=true,
    )
    @test.assert_eq(refactor_labels(node), [Some("pattern"), Some("op")])
    @test.assert_eq(node.children[0].1.text(source), pattern[:])
    @test.assert_eq(node.children[0].1.source_span, (10, 11))
    @test.assert_eq(node.children[1].1.text(source), "?"[:])
    @test.assert_eq(node.children[1].1.source_span, (11, 12))
    @test.assert_eq(node.source_span, (10, 12))
    let (expr, reports) = parsed.to_expr()
    @test.assert_eq(reports.length(), 0)
    guard expr is Some(@syntax.Expr::Match(cases~, ..)) else {
      fail("expected match expression")
    }
    guard cases.to_array() is [{ pattern: @syntax.Pattern::Any(loc~), .. }] else {
      fail("question pattern must retain its previous lowering")
    }
    @test.assert_eq(loc, node.loc)
  }
}

///|
test "ordinary type applications and variable patterns still lower correctly" {
  let source = "fn f(x : Array[Int]) { x }"
  let node = refactor_find_node(source, kind => kind is Type_Application)
  @test.assert_eq(refactor_labels(node), [Some("func"), Some("arg")])
  assert_handrolled_cst_ast_equiv(source)
  assert_expr_ast_equiv("match x { y => y }")
  assert_expr_ast_equiv("match x { (y : Int) => y }")
}

///|
fn refactor_assert_source_range(
  node : @untyped_cst.CstNode,
  source : String,
  text : String,
  span : (Int, Int),
  start : (Int, Int, Int),
  end : (Int, Int, Int),
) -> Unit raise {
  @test.assert_eq(node.text(source), text[:])
  @test.assert_eq(node.source_span, span)
  @test.assert_eq(
    (node.loc.start.lnum, node.loc.start.bol, node.loc.start.cnum),
    start,
  )
  @test.assert_eq((node.loc.end.lnum, node.loc.end.bol, node.loc.end.cnum), end)
}

///|
test "async tests retain single and consecutive comments before test" {
  for multiple in [false, true] {
    let source = if multiple {
      "async // keep\n // second\n test \"named\" {}"
    } else {
      "async // keep\n test \"named\" {}"
    }
    let node = refactor_find_node(source, kind => kind is Impl_Test)
    let labels = [Some("kw"), Some("trivia")]
    if multiple {
      labels.push(Some("trivia"))
    }
    labels.append([
      Some("kw"),
      Some("name"),
      Some("test_name"),
      Some("body"),
      Some("vis"),
      Some("declare"),
      Some("async"),
    ])
    @test.assert_eq(refactor_labels(node), labels)
    let comments = node.children_with_label("trivia")
    @test.assert_eq(comments.length(), if multiple { 2 } else { 1 })
    for comment in comments {
      @test.assert_eq(
        comment.kind is @untyped_cst.NodeKind::Syntax_Token(TK_COMMENT),
        true,
      )
    }
    refactor_assert_source_range(
      comments[0],
      source,
      "// keep",
      (6, 13),
      (1, 0, 6),
      (1, 0, 13),
    )
    if multiple {
      refactor_assert_source_range(
        comments[1],
        source,
        "// second",
        (15, 24),
        (2, 14, 15),
        (2, 14, 24),
      )
    }
    let line = if multiple { 3 } else { 2 }
    let bol = if multiple { 25 } else { 14 }
    refactor_assert_source_range(
      node,
      source,
      source,
      (0, source.length()),
      (1, 0, 0),
      (line, bol, source.length()),
    )
    let test_name = node.first_child_with_label("test_name").unwrap()
    @test.assert_eq(test_name.string_payload(), Some("named"))
    refactor_assert_source_range(
      test_name,
      source,
      "\"named\"",
      (bol + 6, bol + 13),
      (line, bol, bol + 6),
      (line, bol, bol + 13),
    )
    assert_handrolled_cst_ast_equiv(source)
  }
}

///|
test "extend items retain comments on both sides of as" {
  let source = "extend T with Trait::{f // before\n as // after\n g}"
  let node = refactor_find_node(source, kind => kind is Clause_ExtendItem)
  @test.assert_eq(refactor_labels(node), [
    Some("target"),
    Some("trivia"),
    Some("kw"),
    Some("trivia"),
    Some("name"),
    Some("semantic_name"),
  ])
  let comments = node.children_with_label("trivia")
  @test.assert_eq(comments.length(), 2)
  for comment in comments {
    @test.assert_eq(
      comment.kind is @untyped_cst.NodeKind::Syntax_Token(TK_COMMENT),
      true,
    )
  }
  refactor_assert_source_range(
    comments[0],
    source,
    "// before",
    (24, 33),
    (1, 0, 24),
    (1, 0, 33),
  )
  refactor_assert_source_range(
    node.children[2].1,
    source,
    "as",
    (35, 37),
    (2, 34, 35),
    (2, 34, 37),
  )
  refactor_assert_source_range(
    comments[1],
    source,
    "// after",
    (38, 46),
    (2, 34, 38),
    (2, 34, 46),
  )
  refactor_assert_source_range(
    node.children[4].1,
    source,
    "g",
    (48, 49),
    (3, 47, 48),
    (3, 47, 49),
  )
  refactor_assert_source_range(
    node,
    source,
    "f // before\n as // after\n g",
    (22, 49),
    (1, 0, 22),
    (3, 47, 49),
  )
  assert_handrolled_cst_ast_equiv(source)
}

///|
test "import packages retain consecutive comments and UTF-16 spans" {
  let source = "import { \"pkg\" // keep 🐇\n // second\n }"
  let node = refactor_find_node(source, kind => kind is Clause_ImportPackage)
  @test.assert_eq(refactor_labels(node), [
    Some("path"),
    Some("trivia"),
    Some("trivia"),
  ])
  let comments = node.children_with_label("trivia")
  @test.assert_eq(comments.length(), 2)
  for comment in comments {
    @test.assert_eq(
      comment.kind is @untyped_cst.NodeKind::Syntax_Token(TK_COMMENT),
      true,
    )
  }
  refactor_assert_source_range(
    comments[0],
    source,
    "// keep 🐇",
    (15, 25),
    (1, 0, 15),
    (1, 0, 24),
  )
  refactor_assert_source_range(
    comments[1],
    source,
    "// second",
    (27, 36),
    (2, 25, 26),
    (2, 25, 35),
  )
  refactor_assert_source_range(
    node,
    source,
    "\"pkg\" // keep 🐇\n // second",
    (9, 36),
    (1, 0, 9),
    (2, 25, 35),
  )
  assert_handrolled_cst_ast_equiv(source)
}

///|
test "invalid record patterns retain nested patterns and commas" {
  for source in ["match x { {_,} => 0 }", "match x { {_, a} => 0 }"] {
    let parsed = @untyped_cst.parse_expression(source)
    guard api_find_node(parsed.root(), node => {
        node.kind is List_RecordPatternFieldList
      })
      is Some(list) else {
      fail("expected record pattern field list")
    }
    @test.assert_eq(
      refactor_labels(list),
      if source == "match x { {_,} => 0 }" {
        [Some("open"), Some("item"), Some("sep"), Some("close")]
      } else {
        [Some("open"), Some("item"), Some("sep"), Some("item"), Some("close")]
      },
    )
    let missing = list.children[1].1
    @test.assert_eq(
      missing.kind is @untyped_cst.NodeKind::Pattern_MissingPattern,
      true,
    )
    @test.assert_eq(refactor_labels(missing), [Some("pattern")])
    let pattern = missing.children[0].1
    @test.assert_eq(pattern.kind is @untyped_cst.NodeKind::Pattern_Any, true)
    @test.assert_eq(refactor_labels(pattern), [Some("name")])
    for node in [missing, pattern, pattern.children[0].1] {
      refactor_assert_source_range(
        node,
        source,
        "_",
        (11, 12),
        (1, 0, 11),
        (1, 0, 12),
      )
    }
    let comma = list.children[2].1
    @test.assert_eq(
      comma.kind is @untyped_cst.NodeKind::Syntax_Token(TK_COMMA),
      true,
    )
    refactor_assert_source_range(
      comma,
      source,
      ",",
      (12, 13),
      (1, 0, 12),
      (1, 0, 13),
    )
    if list.children.length() == 5 {
      let field = list.children[3].1
      @test.assert_eq(field.kind is @untyped_cst.NodeKind::Field_Pattern, true)
      refactor_assert_source_range(
        field,
        source,
        "a",
        (14, 15),
        (1, 0, 14),
        (1, 0, 15),
      )
    }
  }
}

///|
test "invalid record pattern diagnostics match handrolled locations and messages" {
  for source in ["match x { {_,} => 0 }", "match x { {_, a} => 0 }"] {
    assert_expression_acceptance("invalid record pattern field", source, false)
  }
}

///|
fn refactor_assert_ordered_comments(
  prefix : String,
  suffix : String,
  pred : (@untyped_cst.NodeKind) -> Bool,
  before : Array[String?],
  after : Array[String?],
  next_text : String,
) -> Unit raise {
  let cases : Array[Array[(String, Int)]] = [
    [("// keep", 7)],
    [("// keep", 7), ("// second", 9)],
    [("// keep 🐇", 9), ("// second 🦀", 11)],
  ]
  for comments in cases {
    let source = prefix + comments.map(c => c.0).join("\n ") + "\n " + suffix
    let node = refactor_find_node(source, pred, expression=true)
    let labels : Array[String?] = []
    labels.append(before)
    for _ in comments {
      labels.push(Some("trivia"))
    }
    labels.append(after)
    @test.assert_eq(refactor_labels(node), labels)
    let trivia = node.children_with_label("trivia")
    @test.assert_eq(trivia.length(), comments.length())
    let mut offset = prefix.length()
    let mut cnum = prefix.length()
    let mut bol = 0
    for i, (text, scalar_length) in comments {
      @test.assert_eq(
        trivia[i].kind is @untyped_cst.NodeKind::Syntax_Token(TK_COMMENT),
        true,
      )
      refactor_assert_source_range(
        trivia[i],
        source,
        text,
        (offset, offset + text.length()),
        (i + 1, bol, cnum),
        (i + 1, bol, cnum + scalar_length),
      )
      offset += text.length() + 2
      cnum += scalar_length + 2
      bol = cnum - 1
    }
    refactor_assert_source_range(
      node.children[before.length() + comments.length()].1,
      source,
      next_text,
      (offset, offset + next_text.length()),
      (comments.length() + 1, bol, cnum),
      (comments.length() + 1, bol, cnum + next_text.length()),
    )
    assert_expr_ast_equiv(source)
  }
}

///|
test "CST preservation: expression constraints retain ordered comments" {
  refactor_assert_ordered_comments(
    "(x : Int ",
    ")",
    kind => kind is Expr_Constraint,
    [Some("open"), Some("expr"), Some("op"), Some("type")],
    [Some("close")],
    ")",
  )
}

///|
test "CST preservation: labelled blocks retain ordered comments" {
  refactor_assert_ordered_comments(
    "label~: ",
    "{ 1 }",
    kind => kind is Expr_LabelledBlock,
    [Some("label"), Some("op")],
    [Some("body")],
    "{ 1 }",
  )
}

///|
test "CST preservation: or patterns retain ordered comments" {
  refactor_assert_ordered_comments(
    "match x { 1 ",
    "| 2 => 0 }",
    kind => kind is Pattern_Or,
    [Some("lhs")],
    [Some("op"), Some("rhs")],
    "|",
  )
}

///|
fn refactor_assert_same_tree(
  actual : @untyped_cst.CstNode,
  expected : @untyped_cst.CstNode,
) -> Unit raise {
  @test.assert_eq(actual.kind == expected.kind, true)
  @test.assert_eq(actual.loc, expected.loc)
  @test.assert_eq(actual.source_span, expected.source_span)
  @test.assert_eq(refactor_labels(actual), refactor_labels(expected))
  for i, (_, child) in actual.children {
    refactor_assert_same_tree(child, expected.children[i].1)
  }
}

///|
fn refactor_assert_array_assignment_recovery(op : String) -> Unit raise {
  let source = "xs[] \{op} value"
  let parsed = @untyped_cst.parse_expression(source)
  json_inspect(
    diagnostics_equiv_reports_json(reports_from_view(parsed.diagnostics_view())),
    content=[
      {
        "type": "Error",
        "loc": {
          "file": "",
          "start": { "line": 1, "column": 4 },
          "end": { "line": 1, "column": 5 },
        },
        "msg": "Unexpected token `]`, you may expect simple expression.",
      },
    ],
  )
  let node = parsed.root().first_child_with_label("expr").unwrap()
  @test.assert_eq(
    if op == "=" {
      node.kind is @untyped_cst.NodeKind::Expr_ArraySet
    } else {
      node.kind is @untyped_cst.NodeKind::Expr_ArrayAugmentedSet
    },
    true,
  )
  @test.assert_eq(refactor_labels(node), [
    Some("lhs"),
    Some("op"),
    Some("operator"),
    Some("rhs"),
    Some("augmented"),
  ])
  let lhs = node.children[0].1
  @test.assert_eq(lhs.kind is @untyped_cst.NodeKind::Expr_ArrayGet, true)
  let original_lhs = @untyped_cst.parse_expression("xs[]")
    .root()
    .first_child_with_label("expr")
    .unwrap()
  refactor_assert_same_tree(lhs, original_lhs)
  @test.assert_eq(refactor_labels(lhs), [
    Some("self"),
    Some("open"),
    Some("index"),
    Some("close"),
  ])
  refactor_assert_source_range(
    lhs,
    source,
    "xs[]",
    (0, 4),
    (1, 0, 0),
    (1, 0, 4),
  )
  let array = lhs.first_child_with_label("self").unwrap()
  @test.assert_eq(array.kind is @untyped_cst.NodeKind::Expr_Ident, true)
  let name = array.first_child_with_label("name").unwrap()
  @test.assert_eq(
    name.kind is @untyped_cst.NodeKind::Syntax_Token(TK_LIDENT),
    true,
  )
  @test.assert_eq(name.string_payload(), Some("xs"))
  refactor_assert_source_range(name, source, "xs", (0, 2), (1, 0, 0), (1, 0, 2))
  for (label, text, start) in [("open", "[", 2), ("close", "]", 3)] {
    let token = lhs.first_child_with_label(label).unwrap()
    @test.assert_eq(
      token.kind
      is @untyped_cst.NodeKind::Syntax_Token(TK_LBRACKET | TK_RBRACKET),
      true,
    )
    refactor_assert_source_range(
      token,
      source,
      text,
      (start, start + 1),
      (1, 0, start),
      (1, 0, start + 1),
    )
  }
  refactor_assert_source_range(
    node.children[1].1,
    source,
    op,
    (5, 5 + op.length()),
    (1, 0, 5),
    (1, 0, 5 + op.length()),
  )
  refactor_assert_source_range(
    node.children[3].1,
    source,
    "value",
    (6 + op.length(), source.length()),
    (1, 0, 6 + op.length()),
    (1, 0, source.length()),
  )
  @test.assert_eq(node.children[4].1.bool_payload(), Some(op != "="))
  let (expr, reports) = parsed.to_expr()
  @test.assert_eq(expr is None, true)
  @test.assert_eq(
    diagnostics_equiv_reports_json(reports),
    diagnostics_equiv_reports_json(reports_from_view(parsed.diagnostics_view())),
  )
}

///|
test "CST preservation: array assignment retains the recovery lhs" {
  refactor_assert_array_assignment_recovery("=")
}

///|
test "CST preservation: augmented array assignment retains the recovery lhs" {
  refactor_assert_array_assignment_recovery("+=")
}

///|
test "CST preservation: public visibility retains every error node" {
  let source = "pub(1 2) fn f() {}"
  let parsed = @untyped_cst.parse_structure(source)
  json_inspect(
    diagnostics_equiv_reports_json(reports_from_view(parsed.diagnostics_view())),
    content=[
      {
        "type": "Error",
        "loc": {
          "file": "",
          "start": { "line": 1, "column": 5 },
          "end": { "line": 1, "column": 6 },
        },
        "msg": "Unexpected token int, you may expect `readonly`, id (lowercase start), `extend`.",
      },
    ],
  )
  let node = api_find_node(parsed.root(), node => node.kind is Visibility_Pub).unwrap()
  @test.assert_eq(refactor_labels(node), [
    Some("kw"),
    Some("open"),
    Some("error"),
    Some("error"),
    Some("attr"),
    Some("close"),
  ])
  let errors = node.children_with_label("error")
  @test.assert_eq(errors.length(), 2)
  for i, error in errors {
    @test.assert_eq(error.kind is @untyped_cst.NodeKind::Error, true)
    @test.assert_eq(refactor_labels(error), [Some("item")])
    let token = error.children[0].1
    @test.assert_eq(
      token.kind is @untyped_cst.NodeKind::Syntax_Unexpected(_),
      true,
    )
    let text = if i == 0 { "1" } else { "2" }
    @test.assert_eq(token.string_payload(), Some(text))
    let start = 4 + i * 2
    for child in [error, token] {
      refactor_assert_source_range(
        child,
        source,
        text,
        (start, start + 1),
        (1, 0, start),
        (1, 0, start + 1),
      )
    }
  }
  refactor_assert_source_range(
    node,
    source,
    "pub(1 2)",
    (0, 8),
    (1, 0, 0),
    (1, 0, 8),
  )
}

///|
test "valid array assignments retain expanded children and AST" {
  for op in ["=", "+="] {
    let source = "xs[0] \{op} value"
    let node = refactor_find_node(
      source,
      kind => kind is (Expr_ArraySet | Expr_ArrayAugmentedSet),
      expression=true,
    )
    @test.assert_eq(refactor_labels(node), [
      Some("self"),
      Some("open"),
      Some("index"),
      Some("close"),
      Some("op"),
      Some("operator"),
      Some("rhs"),
      Some("augmented"),
    ])
    for
      (label, text, start, end) in [
        ("self", "xs", 0, 2),
        ("open", "[", 2, 3),
        ("index", "0", 3, 4),
        ("close", "]", 4, 5),
      ] {
      refactor_assert_source_range(
        node.first_child_with_label(label).unwrap(),
        source,
        text,
        (start, end),
        (1, 0, start),
        (1, 0, end),
      )
    }
    @test.assert_eq(api_has_kind(node, kind => kind is Missing), false)
    assert_expr_ast_equiv(source)
  }
}

///|
test "valid visibility and expression constraints retain their AST" {
  for visibility in ["pub", "pub(all)"] {
    let source = "\{visibility} fn f() {}"
    let node = refactor_find_node(source, kind => kind is Visibility_Pub)
    @test.assert_eq(
      refactor_labels(node),
      if visibility == "pub" {
        [Some("kw")]
      } else {
        [Some("kw"), Some("open"), Some("attr"), Some("close")]
      },
    )
    @test.assert_eq(node.text(source), visibility[:])
    @test.assert_eq(node.children_with_label("error").length(), 0)
    assert_handrolled_cst_ast_equiv(source)
  }
  assert_expr_ast_equiv("(x : Int)")
  assert_expr_ast_equiv("(x : Int) => x")
}

///|
test "CST preservation: field assignments retain the complete recovery lhs" {
  for prefix in ["", "// 🐇\n"] {
    let offset = prefix.length()
    let cnum = if prefix == "" { 0 } else { 5 }
    let line = if prefix == "" { 1 } else { 2 }
    let original = @untyped_cst.parse_expression(prefix + "xs[].field")
    let original_lhs = api_find_node(original.root(), node => {
      node.kind is Expr_Field
    }).unwrap()
    for op in ["=", "+="] {
      let source = prefix + "xs[].field \{op} value"
      let parsed = @untyped_cst.parse_expression(source)
      let reports = parsed.diagnostics_view()
      @test.assert_eq(reports.length(), 1)
      @test.assert_eq(
        reports[0].msg,
        "Unexpected token `]`, you may expect simple expression.",
      )
      @test.assert_eq(reports[0].loc.start, @basic.Position::{
        fname: "",
        lnum: line,
        bol: cnum,
        cnum: cnum + 3,
      })
      @test.assert_eq(reports[0].loc.end.cnum, cnum + 4)
      @test.assert_eq(
        diagnostics_equiv_reports_json(reports_from_view(reports)),
        diagnostics_equiv_reports_json(
          reports_from_view(original.diagnostics_view()),
        ),
      )
      let node = api_find_node(parsed.root(), node => node.kind is Expr_Mutate).unwrap()
      @test.assert_eq(refactor_labels(node), [
        Some("lhs"),
        Some("op"),
        Some("operator"),
        Some("rhs"),
        Some("augmented"),
      ])
      let lhs = node.children[0].1
      refactor_assert_same_tree(lhs, original_lhs)
      refactor_assert_source_range(
        lhs,
        source,
        "xs[].field",
        (offset, offset + 10),
        (line, cnum, cnum),
        (line, cnum, cnum + 10),
      )
      refactor_assert_source_range(
        node.children[1].1,
        source,
        op,
        (offset + 11, offset + 11 + op.length()),
        (line, cnum, cnum + 11),
        (line, cnum, cnum + 11 + op.length()),
      )
      @test.assert_eq(node.children[3].1.text(source), "value"[:])
      @test.assert_eq(node.children[4].1.bool_payload(), Some(op != "="))
      @test.assert_eq(
        node.loc,
        @node.mk_loc(lhs.loc.start, node.children[3].1.loc.end),
      )
      @test.assert_eq(node.source_span, (offset, source.length()))
      let (expr, lowered_reports) = parsed.to_expr()
      @test.assert_eq(expr is None, true)
      @test.assert_eq(
        diagnostics_equiv_reports_json(lowered_reports),
        diagnostics_equiv_reports_json(reports_from_view(reports)),
      )
    }
  }
}

///|
test "CST preservation: valid field assignments retain expanded children and AST" {
  let original_lhs = refactor_find_node(
    "xs[0].field",
    kind => kind is Expr_Field,
    expression=true,
  )
  for op in ["=", "+="] {
    let source = "xs[0].field \{op} value"
    let node = refactor_find_node(
      source,
      kind => kind is Expr_Mutate,
      expression=true,
    )
    @test.assert_eq(refactor_labels(node), [
      Some("self"),
      Some("token"),
      Some("field"),
      Some("op"),
      Some("operator"),
      Some("rhs"),
      Some("augmented"),
    ])
    for i, (_, child) in original_lhs.children {
      refactor_assert_same_tree(node.children[i].1, child)
    }
    @test.assert_eq(api_has_kind(node, kind => kind is Missing), false)
    @test.assert_eq(node.children[6].1.bool_payload(), Some(op != "="))
    assert_expr_ast_equiv(source)
  }
}

///|
test "CST preservation: missing expressions retain consecutive newline trivia" {
  for prefix in ["", "// 🐇\n"] {
    let offset = prefix.length()
    let cnum = if prefix == "" { 0 } else { 5 }
    let line = if prefix == "" { 1 } else { 2 }
    for newlines in ["\n\n", "\n\n\n"] {
      let count = newlines.length()
      let source = prefix + "x +" + newlines + "let"
      let parsed = @untyped_cst.parse_expression(source)
      let reports = parsed.diagnostics_view()
      @test.assert_eq(reports.length(), 1)
      @test.assert_eq(
        reports[0].msg,
        "Unexpected token `let`, you may expect simple expression.",
      )
      let let_start = cnum + 3 + count
      @test.assert_eq(
        reports[0].loc,
        @node.mk_loc(
          @basic.Position::{
            fname: "",
            lnum: line + count,
            bol: let_start,
            cnum: let_start,
          },
          @basic.Position::{
            fname: "",
            lnum: line + count,
            bol: let_start,
            cnum: let_start + 3,
          },
        ),
      )
      let node = api_find_node(parsed.root(), node => {
        node.kind is Expr_MissingExpr
      }).unwrap()
      let labels = [ for _ in 0..<count => Some("trivia") ]
      labels.push(Some("expr"))
      @test.assert_eq(refactor_labels(node), labels)
      for i in 0..<count {
        let trivia = node.children[i].1
        @test.assert_eq(
          trivia.kind is @untyped_cst.NodeKind::Syntax_Token(TK_NEWLINE),
          true,
        )
        let start = cnum + 3 + i
        let bol = if i == 0 { cnum } else { start }
        refactor_assert_source_range(
          trivia,
          source,
          "\n",
          (offset + 3 + i, offset + 4 + i),
          (line + i, bol, start),
          (line + i, bol, start + 1),
        )
      }
      let expr = node.children[count].1
      @test.assert_eq(
        expr.kind is @untyped_cst.NodeKind::Expr_MissingExpr,
        true,
      )
      @test.assert_eq(refactor_labels(expr), [Some("error")])
      let error = expr.children[0].1
      @test.assert_eq(error.kind is @untyped_cst.NodeKind::Error, true)
      @test.assert_eq(refactor_labels(error), [Some("item")])
      @test.assert_eq(
        error.children[0].1.kind is @untyped_cst.NodeKind::Syntax_Unexpected(_),
        true,
      )
      for child in [expr, error, error.children[0].1] {
        refactor_assert_source_range(
          child,
          source,
          "let",
          (offset + 3 + count, source.length()),
          (line + count, let_start, let_start),
          (line + count, let_start, let_start + 3),
        )
      }
      refactor_assert_source_range(
        node,
        source,
        newlines + "let",
        (offset + 3, source.length()),
        (line, cnum, cnum + 3),
        (line + count, let_start, let_start + 3),
      )
    }
  }
}

///|
test "CST preservation: constructor parameters retain colons without names" {
  for prefix in ["", "// 🐇\n"] {
    let offset = prefix.length()
    let cnum = if prefix == "" { 0 } else { 5 }
    let line = if prefix == "" { 1 } else { 2 }
    for mut_prefix in ["", "mut "] {
      for
        (tail, colon_offset, labelled) in [
          (": Int", Some(0), false),
          ("Int", None, false),
          ("name~: Int", Some(5), true),
        ] {
        let param = mut_prefix + tail
        let source = prefix + "enum E { C(\{param}) }"
        let parsed = @untyped_cst.parse_structure(source)
        // Preserve the existing permissive parsing of a colon without a name.
        @test.assert_eq(parsed.diagnostics_view().length(), 0)
        let node = api_find_node(parsed.root(), node => {
          node.kind
          is (Parameter_ConstructorPositional | Parameter_ConstructorLabelled)
        }).unwrap()
        @test.assert_eq(
          node.kind is @untyped_cst.NodeKind::Parameter_ConstructorLabelled,
          labelled,
        )
        let labels = []
        if mut_prefix != "" {
          labels.push(Some("mut"))
        }
        if labelled {
          labels.push(Some("name"))
        }
        if colon_offset is Some(_) {
          labels.push(Some("op"))
        }
        labels.append([Some("type"), Some("mutable")])
        @test.assert_eq(refactor_labels(node), labels)
        if colon_offset is Some(colon_offset) {
          let colon = node.first_child_with_label("op").unwrap()
          let start = 11 + mut_prefix.length() + colon_offset
          @test.assert_eq(
            colon.kind is @untyped_cst.NodeKind::Syntax_Token(TK_COLON),
            true,
          )
          refactor_assert_source_range(
            colon,
            source,
            ":",
            (offset + start, offset + start + 1),
            (line, cnum, cnum + start),
            (line, cnum, cnum + start + 1),
          )
        }
        let ty = node.first_child_with_label("type").unwrap()
        let end = 11 + param.length()
        refactor_assert_source_range(
          ty,
          source,
          "Int",
          (offset + end - 3, offset + end),
          (line, cnum, cnum + end - 3),
          (line, cnum, cnum + end),
        )
        refactor_assert_source_range(
          node,
          source,
          param,
          (offset + 11, offset + end),
          (line, cnum, cnum + 11),
          (line, cnum, cnum + end),
        )
        let mutable = node.first_child_with_label("mutable").unwrap()
        @test.assert_eq(mutable.bool_payload(), Some(mut_prefix != ""))
        @test.assert_eq(mutable.loc, @node.empty_loc(node.loc.end))
        @test.assert_eq(mutable.source_span, (offset + end, offset + end))
        if tail != ": Int" {
          assert_handrolled_cst_ast_equiv(source)
        }
      }
    }
  }
}
