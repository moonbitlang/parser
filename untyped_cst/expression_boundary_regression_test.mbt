///|
fn boundary_expr(source : String) -> @syntax.Expr raise {
  assert_ast_equiv("fn boundary() { " + source + " }", label=source)
  assert_expr_ast_equiv(source)
  let parsed = @untyped_cst.parse_expression(source)
  @test.assert_eq(parsed.root().text(source), source[:])
  let (expr, reports) = parsed.to_expr()
  @test.assert_eq(reports.length(), 0)
  guard expr is Some(expr) else { fail("expected expression: \{source}") }
  expr
}

///|
fn boundary_assert_source(
  root : @untyped_cst.CstNode,
  source : String,
) -> Unit raise {
  @test.assert_eq(root.text(source), source[:])
  assert_handrolled_cst_node_source_invariants(source, "root", root, source, "")
  // Check retained syntax tokens as well as the source-backed root range.
  let lex = @lexer.tokens_from_string(source, name="", comment=true)
  for (token, start, end) in lex.tokens {
    if token is (NEWLINE | EOF | SEMI(false)) {
      continue
    }
    guard api_find_node(root, node => {
        (node.kind is Syntax_Token(_) || node.kind is Syntax_Unexpected(_)) &&
        node.loc == @node.mk_loc(start, end)
      })
      is Some(_) else {
      fail("missing source token \{Repr(token)} in \{source}")
    }
  }
}

///|
test "expression boundary: arrow record shorthand" {
  guard boundary_expr("x => { x }") is Function(func~, ..) &&
    func.body is Record(..) else {
    fail("expected Function(Record)")
  }
}

///|
test "expression boundary: arrow body includes pipe" {
  guard boundary_expr("x => { x } |> f") is Function(func~, ..) &&
    func.body is Pipe(..) else {
    fail("expected Function(Pipe)")
  }
}

///|
test "expression boundary: arrow body includes field access" {
  guard boundary_expr("x => { field: x }.field") is Function(func~, ..) &&
    func.body is Field(..) else {
    fail("expected Function(Field)")
  }
}

///|
test "expression boundary: pipe follows string template" {
  guard boundary_expr("writer <+ \"text\" |> f")
    is Pipe(lhs=TemplateWriting(..), ..) else {
    fail("expected Pipe(TemplateWriting)")
  }
}

///|
test "expression boundary: pipe follows object template" {
  guard boundary_expr("writer <+ { \"key\": value } |> f")
    is Pipe(lhs=TemplateWriting(..), ..) else {
    fail("expected Pipe(TemplateWriting)")
  }
}

///|
test "expression boundary: catch follows template" {
  guard boundary_expr("writer <+ \"text\" catch { _ => () }")
    is Try(body=TemplateWriting(..), ..) else {
    fail("expected Try(TemplateWriting)")
  }
}

///|
test "expression boundary: complete ordinary arrow bodies" {
  for params in ["x", "(x)", "()", "_", "(x, y)", "(x : Int)"] {
    for
      body in [
        "{ x }", "{ x }.x", "{ field: x }.field", "{ \"key\": x }[\"key\"]", "{ 1 } + 2 * 3",
        "{ x } |> f |> g", "{ x } <| f", "{ f() } catch { _ => () }",
      ] {
      guard boundary_expr(params + " => " + body) is Function(..) else {
        fail("expected the whole expression to remain inside the arrow")
      }
    }
  }
  guard boundary_expr("x => y => { field: x }.field |> f")
    is Function(func={ body: Function(func~, ..), .. }, ..) &&
    func.body is Pipe(lhs=Field(..), ..) else {
    fail("expected nested arrow with field and pipe in the inner body")
  }
}

///|
test "expression boundary: pipe arrows retain block semantics" {
  guard boundary_expr("value |> x => { x }")
    is Pipe(rhs=Function(func~, ..), ..) &&
    func.body is Ident(..) else {
    fail("expected a block body in a pipe arrow")
  }
  guard boundary_expr("value |> x => { x } |> f |> g")
    is Pipe(lhs=Pipe(lhs=Pipe(rhs=Function(..), ..), ..), ..) else {
    fail("expected left-associated outer pipes")
  }
  for
    source in [
      "value |> x => { x } catch { _ => () }", "value <| x => { x } catch { _ => () }",
    ] {
    guard boundary_expr(source) is Try(body~, ..) &&
      body is (Pipe(..) | RevPipe(..)) else {
      fail("expected catch outside the pipe arrow")
    }
  }
  for params in ["x", "(x)", "()", "_", "(x, y)", "(x : Int)"] {
    guard boundary_expr("value <| " + params + " => { x }")
      is RevPipe(rhs=Function(func~, ..), ..) &&
      func.body is Ident(..) else {
      fail("expected a block body in a reverse pipe arrow")
    }
  }
}

///|
test "expression boundary: all template literals leave outer suffixes" {
  let templates = [
    "\"text\"", "b\"text\"", "\"text \\{value}\"", "b\"text \\{value}\"", "\n#|text\n#|next\n",
    "\n$|text \\{value}\n$|next\n", "\n#|text\n$|next \\{value}\n", "{}", "{ \"key\": value, }",
  ]
  for op in ["<+", "<?"] {
    for template in templates {
      let source = "writer " + op + " " + template
      guard boundary_expr(source) is TemplateWriting(is_conditional~, ..) &&
        is_conditional == (op == "<?") else {
        fail("expected template writing with the correct conditional flag")
      }
      guard boundary_expr(source + " |> f |> g")
        is Pipe(lhs=Pipe(lhs=TemplateWriting(..), ..), ..) else {
        fail("expected pipes outside template")
      }
      guard boundary_expr(source + " <| f")
        is RevPipe(lhs=TemplateWriting(..), ..) else {
        fail("expected reverse pipe outside template")
      }
      // A newline after a multiline literal inserts a statement separator
      // before arithmetic operators, so group that operand explicitly.
      if template.has_prefix("\n") {
        guard boundary_expr("(" + source + ") + value")
          is Infix(lhs=Group(expr=TemplateWriting(..), ..), ..) else {
          fail("expected infix outside grouped multiline template")
        }
      } else {
        guard boundary_expr(source + " + value")
          is Infix(lhs=TemplateWriting(..), ..) else {
          fail("expected infix outside template: \{source}")
        }
      }
      guard boundary_expr(source + " catch { _ => () }")
        is Try(body=TemplateWriting(..), ..) else {
        fail("expected catch outside template")
      }
    }
  }
}

///|
test "expression boundary: template fields contain full expressions" {
  for op in ["<+", "<?"] {
    let source = "writer " +
      op +
      " { \"pipe\": value |> f, \"catch\": f() catch { _ => 0 } } |> g"
    guard boundary_expr(source)
      is Pipe(lhs=TemplateWriting(template=Map(elems~, ..), ..), ..) else {
      fail("expected object template inside outer pipe")
    }
    let fields = elems.to_array()
    @test.assert_eq(fields.length(), 2)
    @test.assert_eq(fields[0].expr is Pipe(..), true)
    @test.assert_eq(fields[1].expr is Try(..), true)
    ignore(
      boundary_expr(
        "writer " + op + " { \"fn\": x => { x } |> f, \"map\": { 1: value } }",
      ),
    )
  }
}

///|
test "expression boundary: comments and Unicode retain tokens and locations" {
  for newline in ["\n", "\r\n"] {
    for
      expr in [
        "x => { // body 🐇\nx } |> f", "value |> x => // block 🦀\n{ x } |> f",
        "writer <+ { // template 🐇\n\"你好🦀\": value } |> f", "writer <? { // key 🦀\n\"你好🐇\" // colon\n: value |> f,\n\"catch\": f() catch { _ => 0 } } catch { _ => () }",
      ] {
      let source = expr.replace_all(old="\n", new=newline)
      ignore(boundary_expr(source))
      boundary_assert_source(
        @untyped_cst.parse_expression(source).root(),
        source,
      )
      let structure = "fn boundary() { " + source + " }"
      boundary_assert_source(
        @untyped_cst.parse_structure(structure).root(),
        structure,
      )
    }
  }
}

///|
test "expression boundary: missing bodies and templates recover declarations" {
  for
    expr in [
      "value |> x =>", "value <| x =>", "writer <+", "writer <?", "writer <+ value",
      "writer <? 42", "writer <+ { key: value }", "writer <? { \"ok\": value, 1: other }",
      "writer <+ { \"key\" value }", "writer <? { \"key\": value \"next\": other }",
      "writer <+ { \"key\": }",
    ] {
    let source = "fn boundary() { " + expr + " }\nfn after() { 42 }"
    assert_all_parsers_reject(source)
    assert_structure_acceptance(expr, source, false)
    let parsed = @untyped_cst.parse_structure(source)
    boundary_assert_source(parsed.root(), source)
    @test.assert_eq(count_root_items(parsed.root()), 2)
    guard root_first_item(parsed.root()) is Some(_) &&
      api_find_node(parsed.root(), node => {
        node.kind is Impl_Function &&
        node.text(source) == "fn after() { 42 }"[:]
      })
      is Some(_) else {
      fail("expected the following declaration to survive: \{expr}")
    }
    assert_expression_acceptance(expr, expr, false)
    boundary_assert_source(@untyped_cst.parse_expression(expr).root(), expr)
  }
}

///|
test "expression boundary: pipe arrow recovery leaves outer expressions" {
  for pipe in ["|>", "<|"] {
    let source = "value " + pipe + " x => x + 1 catch { _ => () }"
    assert_expression_acceptance(source, source, false)
    let parsed = @untyped_cst.parse_expression(source)
    boundary_assert_source(parsed.root(), source)
    let expr = parsed.root().first_child_with_label("expr").unwrap()
    let body = expr.first_child_with_label("body").unwrap()
    let rhs = body.first_child_with_label("rhs").unwrap()
    @test.assert_eq(expr.kind is Expr_Try, true)
    @test.assert_eq(body.kind is (Expr_Pipe | Expr_RevPipe), true)
    @test.assert_eq(rhs.kind is Expr_Function, true)
    guard rhs.first_child_with_label("body") is Some({ kind: Expr_Infix, .. }) else {
      fail("expected recovered infix body with outer catch")
    }
  }
  let forward = "value |> x => x + 1 |> f |> g"
  assert_expression_acceptance(forward, forward, false)
  let parsed = @untyped_cst.parse_expression(forward)
  boundary_assert_source(parsed.root(), forward)
  @test.assert_eq(count_node_kind(parsed.root(), kind => kind is Expr_Pipe), 3)
  for
    source in [
      "x => return x", "x => break", "x => continue", "value |> x => { x } <| f",
      "value <| x => { x } |> f", "value <| x => { x } <| f",
    ] {
    assert_all_parsers_reject("fn boundary() { " + source + " }")
    assert_expression_acceptance(source, source, false)
  }
}

///|
test "expression boundary: every template key must be a string" {
  for op in ["<+", "<?"] {
    for key in ["name", "1", "true", "b\"key\"", "\"key \\{value}\""] {
      for prefix in ["", "\"valid\": value |> f, "] {
        let expr = "writer " +
          op +
          " { " +
          prefix +
          key +
          ": // recover 🐇\n\"你好🦀\" }"
        assert_expression_acceptance(expr, expr, false)
        boundary_assert_source(@untyped_cst.parse_expression(expr).root(), expr)
        let source = "fn boundary() { " +
          expr +
          "; let following = 42; following }\nfn after() { 42 }"
        assert_all_parsers_reject(source)
        assert_structure_acceptance(expr, source, false)
        let root = @untyped_cst.parse_structure(source).root()
        boundary_assert_source(root, source)
        @test.assert_eq(count_root_items(root), 2)
        guard api_find_node(root, node => {
            node.kind is Expr_Let &&
            node.text(source) == "let following = 42"[:]
          })
          is Some(_) else {
          fail("expected the statement after the invalid template")
        }
      }
    }
    for rhs in ["(\"text\")", "\"text\".field", "\"text\"[0]"] {
      let expr = "writer " + op + " " + rhs
      assert_expression_acceptance(expr, expr, false)
      assert_all_parsers_reject("fn boundary() { " + expr + " }")
      boundary_assert_source(@untyped_cst.parse_expression(expr).root(), expr)
    }
  }
}
