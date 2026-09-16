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
  @test.assert_eq(comment.kind is @untyped_cst.NodeKind::Syntax_Comment, true)
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
  @test.assert_eq(first.kind is @untyped_cst.NodeKind::Syntax_Comment, true)
  @test.assert_eq(second.kind is @untyped_cst.NodeKind::Syntax_Comment, true)
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
    Some("header_loc"),
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
