///|
fn api_find_node(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.CstNode) -> Bool,
) -> @untyped_cst.CstNode? {
  if pred(node) {
    return Some(node)
  }
  for entry in node.children {
    let (_, child) = entry
    if api_find_node(child, pred) is Some(found) {
      return Some(found)
    }
  }
  None
}

///|
fn api_has_kind(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.NodeKind) -> Bool,
) -> Bool {
  api_find_node(node, fn(child) { pred(child.kind) }) is Some(_)
}

///|
fn api_has_labeled_kind(
  node : @untyped_cst.CstNode,
  label : String,
  pred : (@untyped_cst.NodeKind) -> Bool,
) -> Bool {
  for entry in node.children {
    let (child_label, child) = entry
    if child_label is Some(found) && found == label && pred(child.kind) {
      return true
    }
    if api_has_labeled_kind(child, label, pred) {
      return true
    }
  }
  false
}

///|
fn api_direct_labeled_kind(
  node : @untyped_cst.CstNode,
  label : String,
  pred : (@untyped_cst.NodeKind) -> Bool,
) -> Bool {
  for entry in node.children {
    let (child_label, child) = entry
    if child_label is Some(found) && found == label && pred(child.kind) {
      return true
    }
  }
  false
}

///|
fn api_count_direct_label(node : @untyped_cst.CstNode, label : String) -> Int {
  let mut count = 0
  for entry in node.children {
    match entry {
      (Some(found), _) if found == label => count += 1
      _ => ()
    }
  }
  count
}

///|
fn api_direct_child(
  node : @untyped_cst.CstNode,
  label : String,
) -> @untyped_cst.CstNode? {
  for entry in node.children {
    match entry {
      (Some(found), child) if found == label => return Some(child)
      _ => ()
    }
  }
  None
}

///|
fn api_payload_leaf(node : @untyped_cst.CstNode) -> @untyped_cst.CstNode? {
  for entry in node.children {
    let (label, child) = entry
    if label is Some("payload") {
      match child.kind {
        @untyped_cst.NodeKind::Leaf(_) => return Some(child)
        _ => ()
      }
    }
  }
  None
}

///|
fn api_leaf_payload(node : @untyped_cst.CstNode) -> @untyped_cst.NodePayload? {
  match api_payload_leaf(node) {
    Some(child) =>
      match child.kind {
        @untyped_cst.NodeKind::Leaf(payload) => Some(payload)
        _ => None
      }
    None => None
  }
}

///|
fn api_has_string_value(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.NodeKind) -> Bool,
  value : String,
) -> Bool {
  api_find_node(node, fn(child) {
    pred(child.kind) &&
    api_leaf_payload(child) is Some(@untyped_cst.NodePayload::String(found)) &&
    found == value
  })
  is Some(_)
}

///|
fn api_has_bool_value(
  node : @untyped_cst.CstNode,
  pred : (@untyped_cst.NodeKind) -> Bool,
  value : Bool,
) -> Bool {
  api_find_node(node, fn(child) {
    pred(child.kind) &&
    api_leaf_payload(child) is Some(@untyped_cst.NodePayload::Bool(found)) &&
    found == value
  })
  is Some(_)
}

///|
fn api_has_semantic_payload(
  node : @untyped_cst.CstNode,
  kind : String,
  payload : String,
) -> Bool {
  let kind_matches = match (kind, node.kind) {
    ("bool", @untyped_cst.NodeKind::Constant_Bool)
    | ("byte", @untyped_cst.NodeKind::Constant_Byte)
    | ("bytes", @untyped_cst.NodeKind::Constant_Bytes)
    | ("char", @untyped_cst.NodeKind::Constant_Char)
    | ("int", @untyped_cst.NodeKind::Constant_Int)
    | ("int64", @untyped_cst.NodeKind::Constant_Int64)
    | ("uint", @untyped_cst.NodeKind::Constant_UInt)
    | ("uint64", @untyped_cst.NodeKind::Constant_UInt64)
    | ("float", @untyped_cst.NodeKind::Constant_Float)
    | ("double", @untyped_cst.NodeKind::Constant_Double)
    | ("string", @untyped_cst.NodeKind::Constant_String)
    | ("regex", @untyped_cst.NodeKind::Constant_Regex)
    | ("bigint", @untyped_cst.NodeKind::Constant_BigInt) => true
    _ => false
  }
  let payload_matches = match api_leaf_payload(node) {
    Some(@untyped_cst.NodePayload::String(found)) => found == payload
    Some(@untyped_cst.NodePayload::Bool(found)) =>
      if found {
        payload == "true"
      } else {
        payload == "false"
      }
    Some(@untyped_cst.NodePayload::Int(found)) => found.to_string() == payload
    None => false
  }
  if kind_matches && payload_matches {
    return true
  }
  for entry in node.children {
    let (_, child) = entry
    if api_has_semantic_payload(child, kind, payload) {
      return true
    }
  }
  false
}

///|
test "parse entry points expose distinct roots and propagate names" {
  let structure = @untyped_cst.parse_structure(
    "let x = 1",
    name="structure.mbt",
  )
  guard structure.root().kind is @untyped_cst.NodeKind::Impls else {
    fail("expected Impls")
  }
  @test.assert_eq(structure.root().loc.start.fname, "structure.mbt")
  @test.assert_eq(structure.root().loc.end.fname, "structure.mbt")
  let expression = @untyped_cst.parse_expression("1 + 2", name="expression.mbt")
  guard expression.root().kind is @untyped_cst.NodeKind::Expression else {
    fail("expected Expression")
  }
  @test.assert_eq(expression.root().loc.start.fname, "expression.mbt")
  @test.assert_eq(expression.root().loc.end.fname, "expression.mbt")
}

///|
test "parse entry points gate metavariable syntax" {
  let expression_source = "$(value:exp)"
  @test.assert_eq(
    @untyped_cst.parse_expression(expression_source)
    .diagnostics_view()
    .is_empty(),
    false,
  )
  let expression = @untyped_cst.parse_expression(
    expression_source,
    enable_metavar=true,
  )
  @test.assert_eq(expression.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_string_value(
      expression.root(),
      fn(kind) { kind is Syntax_Identifier },
      expression_source,
    ),
    true,
  )

  let structure_source = "let $(value:pat) = 1"
  @test.assert_eq(
    @untyped_cst.parse_structure(structure_source).diagnostics_view().is_empty(),
    false,
  )
  let structure = @untyped_cst.parse_structure(
    structure_source,
    enable_metavar=true,
  )
  @test.assert_eq(structure.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_string_value(
      structure.root(),
      fn(kind) { kind is Syntax_Identifier },
      "$(value:pat)",
    ),
    true,
  )
}

///|
test "metavariable option propagates into interpolation expressions" {
  for source in ["\"\\{$x}\"", "\"\\{\"\\{$x}\"}\""] {
    @test.assert_eq(
      @untyped_cst.parse_expression(source).diagnostics_view().is_empty(),
      false,
    )
    let expression = @untyped_cst.parse_expression(source, enable_metavar=true)
    @test.assert_eq(expression.diagnostics_view().is_empty(), true)
    @test.assert_eq(
      api_has_string_value(
        expression.root(),
        fn(kind) { kind is Syntax_Identifier },
        "$x",
      ),
      true,
    )
  }
}

///|
test "constant kinds and normalized payloads are parser semantics" {
  let cases : Array[(String, String, String)] = [
    ("true", "bool", "true"),
    ("b'a'", "byte", "a"),
    ("b\"abc\"", "bytes", "abc"),
    ("'a'", "char", "a"),
    ("1", "int", "1"),
    ("+1L", "int64", "1"),
    ("-1L", "int64", "-1"),
    ("+1U", "uint", "1"),
    ("-1U", "uint", "1"),
    ("1UL", "uint64", "1"),
    ("+1.0F", "float", "1.0"),
    ("1.0", "double", "1.0"),
    ("\"abc\"", "string", "abc"),
    ("re\"abc\"", "regex", "abc"),
    ("-1N", "bigint", "1"),
  ]
  for case in cases {
    let (source, kind, payload) = case
    let result = @untyped_cst.parse_expression(source)
    @test.assert_eq(result.diagnostics_view().is_empty(), true)
    @test.assert_eq(
      api_has_semantic_payload(result.root(), kind, payload),
      true,
    )
  }
}

///|
test "public CST children expose parse-time normalized expression trees" {
  let assignment = @untyped_cst.parse_expression("array[i] = value")
  @test.assert_eq(assignment.diagnostics_view().is_empty(), true)
  guard api_find_node(assignment.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Expr_ArraySet
    })
    is Some(array_set) else {
    fail("expected array set")
  }
  @test.assert_eq(api_count_direct_label(array_set, "lhs"), 0)
  @test.assert_eq(
    api_direct_labeled_kind(array_set, "self", fn(kind) {
      kind is @untyped_cst.NodeKind::Expr_Ident
    }),
    true,
  )
  @test.assert_eq(
    api_direct_labeled_kind(array_set, "index", fn(kind) {
      kind is @untyped_cst.NodeKind::Expr_Ident
    }),
    true,
  )

  let unary_plus = @untyped_cst.parse_expression("+value")
  @test.assert_eq(unary_plus.diagnostics_view().is_empty(), true)
  guard api_find_node(unary_plus.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Expr_Apply
    })
    is Some(apply) else {
    fail("expected normalized unary-plus apply")
  }
  @test.assert_eq(
    api_direct_labeled_kind(apply, "func", fn(kind) {
      kind is @untyped_cst.NodeKind::Expr_Ident
    }),
    true,
  )
  @test.assert_eq(
    api_direct_labeled_kind(apply, "arg", fn(kind) {
      kind is @untyped_cst.NodeKind::List_ArgumentList
    }),
    true,
  )

  let pun = @untyped_cst.parse_expression("f(label~)")
  @test.assert_eq(pun.diagnostics_view().is_empty(), true)
  guard api_find_node(pun.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Argument_LabelledPun
    })
    is Some(argument) else {
    fail("expected labelled pun")
  }
  @test.assert_eq(
    api_direct_labeled_kind(argument, "expr", fn(kind) {
      kind is @untyped_cst.NodeKind::Expr_Ident
    }),
    true,
  )

  let arrow = @untyped_cst.parse_expression("(x : Int) => x")
  @test.assert_eq(arrow.diagnostics_view().is_empty(), true)
  guard api_find_node(arrow.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Parameter_Positional
    })
    is Some(parameter) else {
    fail("expected arrow parameter")
  }
  @test.assert_eq(
    api_direct_labeled_kind(parameter, "pattern", fn(kind) {
      kind is @untyped_cst.NodeKind::Pattern_Var
    }),
    true,
  )
  @test.assert_eq(
    api_direct_labeled_kind(parameter, "type", fn(kind) { kind.is_type() }),
    true,
  )
}

///|
test "operators grouping and signed map keys are parser semantics" {
  let infix = @untyped_cst.parse_expression("left + right")
  @test.assert_eq(infix.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_string_value(
      infix.root(),
      fn(kind) { kind is @untyped_cst.NodeKind::Operator_Infix },
      "+",
    ),
    true,
  )

  let augmented = @untyped_cst.parse_expression("left += right")
  @test.assert_eq(augmented.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_string_value(
      augmented.root(),
      fn(kind) { kind is @untyped_cst.NodeKind::Operator_Augmented },
      "+",
    ),
    true,
  )
  @test.assert_eq(
    api_has_bool_value(
      augmented.root(),
      fn(kind) { kind is @untyped_cst.NodeKind::Flag_Augmented },
      true,
    ),
    true,
  )

  let unary = @untyped_cst.parse_expression("-value")
  @test.assert_eq(unary.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_string_value(
      unary.root(),
      fn(kind) { kind is @untyped_cst.NodeKind::Operator_UnaryMinus },
      "-",
    ),
    true,
  )

  for case in [("()", "unit"), ("(value)", "group"), ("(left, right)", "tuple")] {
    let (source, expected) = case
    let parsed = @untyped_cst.parse_expression(source)
    @test.assert_eq(parsed.diagnostics_view().is_empty(), true)
    @test.assert_eq(
      api_has_kind(parsed.root(), fn(kind) {
        match (expected, kind) {
          ("unit", @untyped_cst.NodeKind::Expr_Unit)
          | ("group", @untyped_cst.NodeKind::Expr_Group)
          | ("tuple", @untyped_cst.NodeKind::Expr_Tuple) => true
          _ => false
        }
      }),
      true,
    )
  }

  let pattern_group = @untyped_cst.parse_expression(
    "match value { (item) => item }",
  )
  @test.assert_eq(pattern_group.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_kind(pattern_group.root(), fn(kind) {
      kind is @untyped_cst.NodeKind::Pattern_Group
    }),
    true,
  )
  let type_group = @untyped_cst.parse_structure("fn f(x : (Int)) {}")
  @test.assert_eq(type_group.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_kind(type_group.root(), fn(kind) {
      kind is @untyped_cst.NodeKind::Type_Group
    }),
    true,
  )

  let signed_map = @untyped_cst.parse_expression("{ -1: 2 }")
  @test.assert_eq(
    api_has_semantic_payload(signed_map.root(), "int", "-1"),
    true,
  )
}

///|
test "helper enum semantics are explicit CST children" {
  let constructor_cases : Array[(String, (@untyped_cst.NodeKind) -> Bool)] = [
    (
      "Some(1)",
      fn(kind) { kind is @untyped_cst.NodeKind::ConstructorExtraInfo_None },
    ),
    (
      "@pkg.Constr(1)",
      fn(kind) { kind is @untyped_cst.NodeKind::ConstructorExtraInfo_Package },
    ),
    (
      "Option::Some(1)",
      fn(kind) { kind is @untyped_cst.NodeKind::ConstructorExtraInfo_TypeName },
    ),
    (
      "@pkg.Option::@pkg.Some(1)",
      fn(kind) {
        kind
        is @untyped_cst.NodeKind::ConstructorExtraInfo_TypeNameWithConstrPackage
      },
    ),
  ]
  for case in constructor_cases {
    let (source, pred) = case
    let parsed = @untyped_cst.parse_expression(source)
    @test.assert_eq(parsed.diagnostics_view().is_empty(), true)
    if !api_has_kind(parsed.root(), pred) {
      fail("missing constructor extra info for \{source}")
    }
  }

  let foreach = @untyped_cst.parse_expression("[ for x in xs => x ]")
  @test.assert_eq(foreach.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_kind(foreach.root(), fn(kind) {
      kind is @untyped_cst.NodeKind::ListComprehensionKind_ForEach
    }),
    true,
  )
  let c_style = @untyped_cst.parse_expression(
    "[ for i = 0; i < 1; i = i + 1 => i ]",
  )
  @test.assert_eq(c_style.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_kind(c_style.root(), fn(kind) {
      kind is @untyped_cst.NodeKind::ListComprehensionKind_For
    }),
    true,
  )

  let rest_cases : Array[(String, (@untyped_cst.NodeKind) -> Bool)] = [
    (
      "match xs { [ .. ] => 0 }",
      fn(kind) { kind is @untyped_cst.NodeKind::DotDotBinder_None },
    ),
    (
      "match xs { [ .._ ] => 0 }",
      fn(kind) { kind is @untyped_cst.NodeKind::DotDotBinder_Underscore },
    ),
    (
      "match xs { [ ..rest ] => rest }",
      fn(kind) { kind is @untyped_cst.NodeKind::DotDotBinder_Binder },
    ),
    (
      "match xs { [ .. as rest ] => rest }",
      fn(kind) { kind is @untyped_cst.NodeKind::DotDotBinder_BinderAs },
    ),
  ]
  for case in rest_cases {
    let (source, pred) = case
    let parsed = @untyped_cst.parse_expression(source)
    @test.assert_eq(api_has_kind(parsed.root(), pred), true)
  }

  let multiline = @untyped_cst.parse_expression(
    "(\n$| hello \\{name}\n#| tail\n)",
  )
  @test.assert_eq(multiline.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_kind(multiline.root(), fn(kind) {
      kind is @untyped_cst.NodeKind::MultilineStringElement_Interp
    }),
    true,
  )
  @test.assert_eq(
    api_has_kind(multiline.root(), fn(kind) {
      kind is @untyped_cst.NodeKind::MultilineStringElement_String
    }),
    true,
  )

  let lex_source =
    #|lexmatch source {
    #|  re"a" => 1
    #|  name => 2
    #|  _ => 3
    #|}
  let lex = @untyped_cst.parse_expression(lex_source)
  @test.assert_eq(lex.diagnostics_view().is_empty(), true)
  for
    pred in [
      fn(kind) { kind is @untyped_cst.NodeKind::RegexPattern_Literal },
      fn(kind) { kind is @untyped_cst.NodeKind::LexScanCasePattern_Pattern },
      fn(kind) { kind is @untyped_cst.NodeKind::LexScanCasePattern_Binder },
      fn(kind) { kind is @untyped_cst.NodeKind::LexScanCasePattern_Wildcard },
    ] {
    @test.assert_eq(api_has_kind(lex.root(), pred), true)
  }

  let lexscan_source =
    #|lexscan source {
    #|  re"a" => 1
    #|  name => 2
    #|  _ => 3
    #|}
  let lexscan_result = @untyped_cst.parse_expression(lexscan_source)
  @test.assert_eq(lexscan_result.diagnostics_view().is_empty(), true)
  for
    pred in [
      fn(kind) { kind is @untyped_cst.NodeKind::LexScanCasePattern_Pattern },
      fn(kind) { kind is @untyped_cst.NodeKind::LexScanCasePattern_Binder },
      fn(kind) { kind is @untyped_cst.NodeKind::LexScanCasePattern_Wildcard },
    ] {
    @test.assert_eq(api_has_kind(lexscan_result.root(), pred), true)
  }

  let regex_literal = @untyped_cst.parse_expression("source =~ re\"a\"")
  @test.assert_eq(regex_literal.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_kind(regex_literal.root(), fn(kind) {
      kind is @untyped_cst.NodeKind::RegexPattern_Literal
    }),
    true,
  )
  let regex_reference = @untyped_cst.parse_expression("source =~ PREFIX")
  @test.assert_eq(regex_reference.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_kind(regex_reference.root(), fn(kind) {
      kind is @untyped_cst.NodeKind::RegexPattern_Reference
    }),
    true,
  )
}

///|
test "location metadata kinds and scalar helpers are explicit" {
  let async_fn = @untyped_cst.parse_structure("async fn f() { () }")
  @test.assert_eq(async_fn.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_bool_value(
      async_fn.root(),
      fn(kind) { kind is @untyped_cst.NodeKind::Flag_Async },
      true,
    ),
    true,
  )
  for label in ["decl_loc", "header_loc"] {
    @test.assert_eq(
      api_has_labeled_kind(async_fn.root(), label, fn(kind) {
        kind is @untyped_cst.NodeKind::Aggregate_Span
      }),
      true,
    )
  }
  @test.assert_eq(
    api_has_labeled_kind(async_fn.root(), "async_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Syntax_Keyword
    }),
    true,
  )

  let declared = @untyped_cst.parse_structure("declare fn f()")
  @test.assert_eq(declared.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_bool_value(
      declared.root(),
      fn(kind) { kind is @untyped_cst.NodeKind::Flag_Declare },
      true,
    ),
    true,
  )

  let object_fn = @untyped_cst.parse_structure("fn &Obj::f() {}")
  @test.assert_eq(object_fn.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_bool_value(
      object_fn.root(),
      fn(kind) { kind is @untyped_cst.NodeKind::Flag_Object },
      true,
    ),
    true,
  )
  @test.assert_eq(
    api_has_labeled_kind(object_fn.root(), "object_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Syntax_Operator
    }),
    true,
  )

  let arrow = @untyped_cst.parse_expression("value |> x => { x }")
  @test.assert_eq(arrow.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_labeled_kind(arrow.root(), "params_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Syntax_Separator
    }),
    true,
  )

  let try_expr = @untyped_cst.parse_expression(
    "try f() catch { _ => 0 } noraise { value => value }",
  )
  @test.assert_eq(try_expr.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_labeled_kind(try_expr.root(), "try_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Syntax_Keyword
    }),
    true,
  )
  for label in ["catch_loc", "else_loc"] {
    @test.assert_eq(
      api_has_labeled_kind(try_expr.root(), label, fn(kind) {
        kind is @untyped_cst.NodeKind::Aggregate_Span
      }),
      true,
    )
  }
  let suffix_try = @untyped_cst.parse_expression("(f)() catch { _ => 0 }")
  @test.assert_eq(suffix_try.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_labeled_kind(suffix_try.root(), "expr_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Aggregate_Span
    }),
    true,
  )
  let indexed = @untyped_cst.parse_expression("array[1:2]")
  @test.assert_eq(indexed.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_labeled_kind(indexed.root(), "index_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Aggregate_Span
    }),
    true,
  )
  let recovered_index = @untyped_cst.parse_expression("array[")
  @test.assert_eq(recovered_index.diagnostics_view().is_empty(), false)
  @test.assert_eq(
    api_has_labeled_kind(recovered_index.root(), "index_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Aggregate_Span
    }),
    true,
  )
  let comprehension = @untyped_cst.parse_expression(
    "[ for i = 0; i < 1; i = i + 1 => i ]",
  )
  @test.assert_eq(comprehension.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_labeled_kind(comprehension.root(), "for_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Syntax_Keyword
    }),
    true,
  )

  let optional = @untyped_cst.parse_expression("f(optional?)")
  @test.assert_eq(optional.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_labeled_kind(optional.root(), "question_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Syntax_Operator
    }),
    true,
  )
  for label in ["label_loc", "value_loc"] {
    @test.assert_eq(
      api_has_labeled_kind(optional.root(), label, fn(kind) {
        kind is @untyped_cst.NodeKind::Syntax_Identifier
      }),
      true,
    )
  }
  let separated_pun = @untyped_cst.parse_expression("f(label~, value)")
  @test.assert_eq(separated_pun.diagnostics_view().is_empty(), true)
  for label in ["label_loc", "value_loc"] {
    @test.assert_eq(
      api_has_labeled_kind(separated_pun.root(), label, fn(kind) {
        kind is @untyped_cst.NodeKind::Syntax_Label
      }),
      true,
    )
  }
  let closed_pun = @untyped_cst.parse_expression("f(label~)")
  @test.assert_eq(closed_pun.diagnostics_view().is_empty(), true)
  for label in ["label_loc", "value_loc"] {
    @test.assert_eq(
      api_has_labeled_kind(closed_pun.root(), label, fn(kind) {
        kind is @untyped_cst.NodeKind::Syntax_Label
      }),
      true,
    )
  }
  let missing_close = @untyped_cst.parse_expression("f(label~")
  @test.assert_eq(missing_close.diagnostics_view().is_empty(), false)
  for label in ["label_loc", "value_loc"] {
    @test.assert_eq(
      api_has_labeled_kind(missing_close.root(), label, fn(kind) {
        kind is @untyped_cst.NodeKind::Syntax_Label
      }),
      true,
    )
  }

  let multiline = @untyped_cst.parse_expression("(\n#| one\n#| two\n)")
  @test.assert_eq(multiline.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_labeled_kind(multiline.root(), "literal_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Aggregate_Span
    }),
    true,
  )

  let labelled_loop = @untyped_cst.parse_expression("label~: while true {}")
  @test.assert_eq(labelled_loop.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_labeled_kind(labelled_loop.root(), "label_loc", fn(kind) {
      kind is Syntax_Label
    }),
    true,
  )
  @test.assert_eq(
    api_has_labeled_kind(labelled_loop.root(), "loop_loc", fn(kind) {
      kind is @untyped_cst.NodeKind::Aggregate_Span
    }),
    true,
  )
  let eof_label = @untyped_cst.parse_expression("break label~")
  @test.assert_eq(eof_label.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_labeled_kind(eof_label.root(), "label_loc", fn(kind) {
      kind is Syntax_Label
    }),
    true,
  )
  let lex_group = @untyped_cst.parse_expression(
    "lexmatch source { (re\"a\" + (re\"b\" + re\"c\")) => 1 }",
  )
  @test.assert_eq(lex_group.diagnostics_view().is_empty(), true)
  for label in ["open", "close"] {
    @test.assert_eq(
      api_has_labeled_kind(lex_group.root(), label, fn(kind) {
        kind is @untyped_cst.NodeKind::Syntax_Delimiter
      }),
      true,
    )
  }

  let constructor_tag = @untyped_cst.parse_structure("enum E { A = 1 }")
  @test.assert_eq(constructor_tag.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_string_value(
      constructor_tag.root(),
      fn(kind) { kind is @untyped_cst.NodeKind::ConstructorTag_Value },
      "1",
    ),
    true,
  )
  let test_name = @untyped_cst.parse_structure("test \"named\" {}")
  @test.assert_eq(test_name.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_string_value(
      test_name.root(),
      fn(kind) { kind is @untyped_cst.NodeKind::Name_Test },
      "named",
    ),
    true,
  )
  let imported = @untyped_cst.parse_structure(
    "extern \"js\" fn from_code_point(p : Int) -> String = \"String\" \"fromCodePoint\"",
  )
  @test.assert_eq(imported.diagnostics_view().is_empty(), true)
  for
    case in [
      ("language", "js"),
      ("module", "String"),
      ("function", "fromCodePoint"),
    ] {
    let (kind, value) = case
    @test.assert_eq(
      api_has_string_value(
        imported.root(),
        fn(node_kind) {
          match (kind, node_kind) {
            ("language", @untyped_cst.NodeKind::Language_Value)
            | ("module", @untyped_cst.NodeKind::Stub_ModuleName)
            | ("function", @untyped_cst.NodeKind::Stub_FunctionName) => true
            _ => false
          }
        },
        value,
      ),
      true,
    )
  }
  let embedded = @untyped_cst.parse_structure(
    "extern \"js\" fn embedded() -> Unit = \"code\"",
  )
  @test.assert_eq(embedded.diagnostics_view().is_empty(), true)
  @test.assert_eq(
    api_has_string_value(
      embedded.root(),
      fn(kind) { kind is @untyped_cst.NodeKind::Stub_Code },
      "code",
    ),
    true,
  )
}

///|
test "parse entry points preserve diagnostics and AST lowering" {
  let structure = @untyped_cst.parse_structure("let x = 1")
  @test.assert_eq(structure.diagnostics_view().is_empty(), true)
  let (_, structure_reports) = structure.to_impls()
  @test.assert_eq(structure_reports.is_empty(), true)
  let expression = @untyped_cst.parse_expression("1 + 2")
  @test.assert_eq(expression.diagnostics_view().is_empty(), true)
  let (expr, expression_reports) = expression.to_expr()
  @test.assert_eq(expression_reports.is_empty(), true)
  guard expr is Some(_) else { fail("expected lowered expression") }
  @test.assert_eq(
    @untyped_cst.parse_structure("fn f(").diagnostics_view().is_empty(),
    false,
  )
  @test.assert_eq(
    @untyped_cst.parse_expression("1 +").diagnostics_view().is_empty(),
    false,
  )
}

///|
test "lexscan missing arrow has a labelled recovery anchor" {
  let source = "lexscan source { re\"a\" value }"
  let result = @untyped_cst.parse_expression(
    source,
    name="lexscan_missing_arrow.mbt",
  )
  @test.assert_eq(result.diagnostics_view().is_empty(), false)
  guard api_find_node(result.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Case_LexScanCase
    })
    is Some(case_node) else {
    fail("expected a lexscan case node")
  }
  @test.assert_eq(api_count_direct_label(case_node, "op"), 1)
  guard api_direct_child(case_node, "op") is Some(op) else {
    fail("expected a labelled missing arrow node")
  }
  guard op.kind is @untyped_cst.NodeKind::Missing else {
    fail("expected the arrow recovery node to be missing")
  }
  @test.assert_eq(op.source_span, (23, 23))
  inspect(op.loc, content="lexscan_missing_arrow.mbt-1:24-1:24")
}

///|
test "lexscan ellipsis exposes its token as the operator child" {
  let source = "lexscan source { ... }"
  let result = @untyped_cst.parse_expression(source)
  @test.assert_eq(result.diagnostics_view().is_empty(), true)
  guard api_find_node(result.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Case_LexScanCase
    })
    is Some(case_node) else {
    fail("expected a lexscan case node")
  }
  guard api_direct_child(case_node, "op") is Some(op) else {
    fail("expected a labelled ellipsis operator node")
  }
  @test.assert_eq(op.source_span, (17, 20))
  @test.assert_eq(op.text(source), "..."[:])
}

///|
test "AST conversion rejects the other parse result kind" {
  let (_, structure_reports) = @untyped_cst.parse_expression("1").to_impls()
  @test.assert_eq(structure_reports.length(), 1)
  @test.assert_eq(structure_reports[0].msg, "expected structure parse result")
  let (_, expression_reports) = @untyped_cst.parse_structure("let x = 1").to_expr()
  @test.assert_eq(expression_reports.length(), 1)
  @test.assert_eq(expression_reports[0].msg, "expected expression parse result")
}

///|
test "source spans and node text use UTF-16 slicing offsets" {
  let source = "let s = \"😀\"\nlet x = 1"
  let result = @untyped_cst.parse_structure(source)
  guard api_find_node(result.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Identifier &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::String("x"))
    })
    is Some(x) else {
    fail("expected x token")
  }
  @test.assert_eq(x.source_span.0, 17)
  @test.assert_eq(x.source_span.1, 18)
  let text : StringView = x.text(source)
  @test.assert_eq(text, "x"[:])
}

///|
test "missing nodes return an empty StringView" {
  let source = "fn f("
  let result = @untyped_cst.parse_structure(source)
  guard api_find_node(result.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Missing
    })
    is Some(missing) else {
    fail("expected missing right parenthesis")
  }
  let text : StringView = missing.text(source)
  @test.assert_eq(text, source[source.length():])
}

///|
test "CST Leaf payloads are scalar and recursive meaning stays in children" {
  let constant = @untyped_cst.parse_expression("42")
  guard api_find_node(constant.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Expr_Constant
    })
    is Some(constant_node) else {
    fail("expected constant expression")
  }
  @test.assert_eq(api_payload_leaf(constant_node) is None, true)
  guard api_find_node(constant_node, fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Literal &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::String("42"))
    })
    is Some(constant_token) else {
    fail("expected integer token")
  }
  @test.assert_eq(
    api_leaf_payload(constant_token)
    is Some(@untyped_cst.NodePayload::String("42")),
    true,
  )

  let ident = @untyped_cst.parse_expression("answer")
  guard api_find_node(ident.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Expr_Ident
    })
    is Some(ident_node) else {
    fail("expected identifier expression")
  }
  @test.assert_eq(api_payload_leaf(ident_node) is None, true)
  guard api_find_node(ident_node, fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Identifier &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::String("answer"))
    })
    is Some(ident_token) else {
    fail("expected identifier token")
  }
  @test.assert_eq(
    api_leaf_payload(ident_token)
    is Some(@untyped_cst.NodePayload::String("answer")),
    true,
  )
}

///|
test "long identifiers encode their AST variant in NodeKind" {
  let unqualified = @untyped_cst.parse_expression("answer")
  @test.assert_eq(
    api_find_node(unqualified.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::LongIdent_Ident
    })
    is Some(_),
    true,
  )
  let qualified = @untyped_cst.parse_expression("@pkg.answer")
  @test.assert_eq(qualified.diagnostics_view().length(), 0)
  @test.assert_eq(
    api_find_node(qualified.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::LongIdent_Dot
    })
    is Some(_),
    true,
  )
}

///|
test "structured declarations keep their spelling in scalar token payloads" {
  let using_result = @untyped_cst.parse_structure(
    "using @pkg {type Item as Alias}",
  )
  guard api_find_node(using_result.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Clause_UsingItem
    })
    is Some(using_item) else {
    fail("expected using item")
  }
  @test.assert_eq(api_payload_leaf(using_item) is None, true)

  let deriving_result = @untyped_cst.parse_structure("type U derive(Eq)")
  guard api_find_node(deriving_result.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Clause_DerivingDirective
    })
    is Some(deriving) else {
    fail("expected deriving directive")
  }
  @test.assert_eq(api_payload_leaf(deriving) is None, true)
  @test.assert_eq(
    api_find_node(deriving, fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Identifier &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::String("Eq"))
    })
    is Some(_),
    true,
  )

  let enum_result = @untyped_cst.parse_structure("enum E { A }")
  guard api_find_node(enum_result.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Clause_ConstructorDecl
    })
    is Some(constructor_node) else {
    fail("expected constructor declaration")
  }
  @test.assert_eq(api_payload_leaf(constructor_node) is None, true)
}

///|
test "Leaf has exactly the three scalar payload representations" {
  let payloads : Array[@untyped_cst.NodePayload] = []
  let string_result = @untyped_cst.parse_expression("text")
  guard api_find_node(string_result.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Identifier &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::String("text"))
    })
    is Some(string_node) &&
    api_leaf_payload(string_node) is Some(string_payload) else {
    fail("expected string payload")
  }
  payloads.push(string_payload)
  let int_result = @untyped_cst.parse_expression("tuple.7")
  guard api_find_node(int_result.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Accessor &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::Int(7))
    })
    is Some(int_node) &&
    api_leaf_payload(int_node) is Some(int_payload) else {
    fail("expected integer payload")
  }
  payloads.push(int_payload)
  let bool_result = @untyped_cst.parse_expression("true")
  guard api_find_node(bool_result.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Literal &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::Bool(true))
    })
    is Some(bool_node) &&
    api_leaf_payload(bool_node) is Some(bool_payload) else {
    fail("expected boolean payload")
  }
  payloads.push(bool_payload)
  let mut string_count = 0
  let mut int_count = 0
  let mut bool_count = 0
  for payload in payloads {
    match payload {
      @untyped_cst.NodePayload::String(_) => string_count += 1
      @untyped_cst.NodePayload::Int(_) => int_count += 1
      @untyped_cst.NodePayload::Bool(_) => bool_count += 1
    }
  }
  @test.assert_eq((string_count, int_count, bool_count), (1, 1, 1))
}

///|
test "lexer token payload mapping is scalar and uniform" {
  let index = @untyped_cst.parse_expression("tuple.12")
  guard api_find_node(index.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Accessor &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::Int(12))
    })
    is Some(index_token) else {
    fail("expected DOT_INT token")
  }
  @test.assert_eq(
    api_leaf_payload(index_token) is Some(@untyped_cst.NodePayload::Int(12)),
    true,
  )
  guard index_token.children.get(index_token.children.length() - 1)
    is Some((Some("payload"), index_leaf)) else {
    fail("expected trailing payload Leaf")
  }
  guard index_leaf.kind
    is @untyped_cst.NodeKind::Leaf(@untyped_cst.NodePayload::Int(12)) else {
    fail("expected integer payload Leaf")
  }
  @test.assert_eq(index_leaf.loc, index_token.loc)
  @test.assert_eq(index_leaf.source_span, index_token.source_span)
  @test.assert_eq(index_leaf.children.length(), 0)
  let mut payload_leaf_count = 0
  for entry in index_token.children {
    let (label, child) = entry
    if label is Some("payload") && child.kind is @untyped_cst.NodeKind::Leaf(_) {
      payload_leaf_count += 1
    }
  }
  @test.assert_eq(payload_leaf_count, 1)
  guard api_find_node(index.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Accessor_Index
    })
    is Some(index_accessor) else {
    fail("expected structured tuple accessor")
  }
  @test.assert_eq(
    api_leaf_payload(index_accessor) is Some(@untyped_cst.NodePayload::Int(12)),
    true,
  )

  let boolean = @untyped_cst.parse_expression("true")
  @test.assert_eq(
    api_find_node(boolean.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Literal &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::Bool(true))
    })
    is Some(_),
    true,
  )
  let explicit_semi = @untyped_cst.parse_structure("let x = 1;")
  @test.assert_eq(
    api_find_node(explicit_semi.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Separator &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::Bool(true))
    })
    is Some(_),
    true,
  )
  let fixed = @untyped_cst.parse_expression("1 + 2")
  @test.assert_eq(
    api_find_node(fixed.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Syntax_Operator &&
      api_payload_leaf(node) is None
    })
    is Some(_),
    true,
  )
}

///|
test "attribute and comment tokens expose structured metadata" {
  let attribute_source = "#test.bench(\"sample\")\nfn f() {}"
  let attribute = @untyped_cst.parse_structure(attribute_source)
  @test.assert_eq(attribute.diagnostics_view().length(), 0)
  @test.assert_eq(
    api_find_node(attribute.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Attribute_Qualifier &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::String("test"))
    })
    is Some(_),
    true,
  )
  @test.assert_eq(
    api_find_node(attribute.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Attribute_Name &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::String("bench"))
    })
    is Some(_),
    true,
  )
  @test.assert_eq(
    api_find_node(attribute.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Attribute_RawPayload
    })
    is Some(_),
    true,
  )

  let comment = @untyped_cst.parse_structure("// note\nlet x = 1")
  @test.assert_eq(comment.diagnostics_view().length(), 0)
  @test.assert_eq(
    api_find_node(comment.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Comment_Content &&
      api_leaf_payload(node)
      is Some(@untyped_cst.NodePayload::String("// note"))
    })
    is Some(_),
    true,
  )
  @test.assert_eq(
    api_find_node(comment.root(), fn(node) {
      node.kind is @untyped_cst.NodeKind::Comment_ConsumedByDocstring &&
      api_leaf_payload(node) is Some(@untyped_cst.NodePayload::Bool(false))
    })
    is Some(_),
    true,
  )
}

///|
test "source-free lowering consumes structured kinds and scalar Leaf children" {
  let result = {
    let source = "fn f(x : Int) -> Int { x + 1 }"
    @untyped_cst.parse_structure(source)
  }
  let (impls, reports) = result.to_impls()
  @test.assert_eq(reports.length(), 0)
  @test.assert_eq(impls.is_empty(), false)
}
