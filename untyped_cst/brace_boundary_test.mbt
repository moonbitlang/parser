///|
fn brace_boundary_sources(expr : String) -> Array[String] {
  [
    "if \{expr} { body() }",
    "while \{expr} { body() }",
    "match \{expr} { _ => body() }",
    "lexmatch \{expr} { _ => body() }",
    "lexscan \{expr} { _ => body() }",
    "for x in \{expr} { body() }",
    "for x = \{expr} { body() }",
    "for ; \{expr} { body() }",
    "for ;; x = \{expr} { body() }",
  ]
}

///|
test "brace operands and body boundaries across control flow headers" {
  let operands = [
    "{ \"key\": value }", "{}", "{ field: value }", "{ field }", "{ ..base, field: value }",
    "{ value; other }",
  ]
  for operand in operands {
    for
      expr in [
        operand,
        "lhs == \{operand}",
        "!\{operand}",
        "\{operand} ..< upper",
        "lower ..< \{operand}",
        "\{operand}()",
        "\{operand}.field",
        "\{operand}[index]",
        "lhs == \{operand}.field(index)[0]",
        "\{operand} ..< {}",
        "{} ..= \{operand}",
      ] {
      for header in brace_boundary_sources(expr) {
        let source = "fn f() { \{header} }"
        assert_ast_equiv(source, label=header)
      }
    }
  }
}

///|
test "brace boundaries are local to nested operands" {
  for
    expr in [
      "f({}, { field }, { \"k\": value })", "array[{ value; index }]", "({} == { field })",
      "{ field: { \"nested\": {} } }", "{ ..f({}), field: array[{}] }", "{ if {} {} else {}; true }",
      "lhs == // before 🐇\n { // inside\n \"🐇\": {} }", "lhs ==\n {\n \"🐇\": value,\n }",
      "{\n let x = {}\n x\n }",
    ] {
    for header in brace_boundary_sources(expr) {
      assert_ast_equiv("// 🐇\nfn f() { \{header} }", label=header)
    }
  }
  for source in ["fn f() { if x == {}\n {} }", "fn f() { while x == {}\n {} }"] {
    assert_all_parsers_reject(source)
  }
}

///|
test "local brace expressions in comprehensions guards and is patterns" {
  for
    source in [
      "fn f() { [for x in {} => x] }", "fn f() { [for x in lhs == {} if x == {} => {}] }",
      "fn f() { [for x = {}; x == {}; x = {} => x] }", "fn f() { match x { _ if x == {} => {} } }",
      "fn f() { if x is { field: {} } {} }", "fn f() { while x is { \"key\": _, .. } {} }",
      "fn f() { for ; x is { field, .. }; {} }", "fn f() { for x in {}; y = {}; y = {} {} }",
      "fn f() { for x in {}\n {} }", "fn f() { for x = ∀ y : Int, {} {} }", "fn f() { if { field; } {} }",
      "fn f() { if { field, } {} }",
    ] {
    assert_ast_equiv(source, label=source)
  }
}

///|
test "brace operands own their tokens and stop exactly before the body" {
  for
    (operand, has_kind) in [
      ("{ \"🐇\": value }", fn(kind : @node.NodeKind) { kind is Expr_Map }),
      ("{}", fn(kind : @node.NodeKind) { kind is Expr_Map }),
      ("{ field: value }", fn(kind : @node.NodeKind) { kind is Expr_Record }),
      ("{ field }", fn(kind : @node.NodeKind) { kind is Expr_Record }),
      (
        "{ ..base, field: value }",
        fn(kind : @node.NodeKind) { kind is Expr_RecordUpdate },
      ),
      ("{ value; other }", fn(kind : @node.NodeKind) { kind is Expr_Block }),
    ] {
    let source = "// 🐇\nif lhs == \{operand} { body() }"
    let parsed = @untyped_cst.parse_expression(source)
    @test.assert_eq(parsed.diagnostics_view().length(), 0)
    let control = api_find_node(parsed.root(), n => n.kind is Expr_If).unwrap()
    let cond = control.first_child_with_label("cond").unwrap()
    let rhs = cond.first_child_with_label("rhs").unwrap()
    let body = control.first_child_with_label("body").unwrap()
    @test.assert_eq(cond.kind is Expr_Infix, true)
    @test.assert_eq(has_kind(rhs.kind), true)
    @test.assert_eq(rhs.text(source), operand[:])
    @test.assert_eq(body.kind is Expr_Block, true)
    @test.assert_eq(body.text(source), "{ body() }"[:])
    let start = "// 🐇\nif lhs == ".length()
    @test.assert_eq(rhs.source_span, (start, start + operand.length()))
    @test.assert_eq(body.source_span.0, rhs.source_span.1 + 1)
    // Source spans count UTF-16 units; locations count Unicode scalars.
    @test.assert_eq(rhs.loc.start.cnum, start - 1)
    @test.assert_eq(rhs.loc.start.lnum, 2)
    @test.assert_eq(rhs.loc.start.column(), 11)
    @test.assert_eq(cond.loc.end, rhs.loc.end)
    @test.assert_eq(control.loc.end, body.loc.end)
    for (token, _, _) in @lexer.tokens_from_string(operand, comment=true).tokens {
      if token.kind() == TK_EOF || token.kind() == TK_NEWLINE {
        continue
      }
      // Each operand brace belongs to the operand, including after a non-BMP key.
      if token.kind() == TK_LBRACE || token.kind() == TK_RBRACE {
        let expected = if token.kind() == TK_LBRACE {
          rhs.source_span.0
        } else {
          rhs.source_span.1 - 1
        }
        @test.assert_eq(
          api_find_node(rhs, n => {
            n.kind.token_kind() == Some(token.kind()) &&
            n.source_span == (expected, expected + 1)
          })
          is Some(_),
          true,
        )
        @test.assert_eq(
          api_find_node(body, n => n.source_span == (expected, expected + 1))
          is None,
          true,
        )
      }
    }
    assert_handrolled_cst_node_source_invariants(
      "brace boundary",
      "root",
      parsed.root(),
      source,
      "",
    )
  }
}

///|
test "missing brace operands delimiters and bodies report and retain following declarations" {
  for
    header in [
      "if", "if x ==", "if !", "if x ..<", "if {}", "while {}", "match {}", "lexmatch {}",
      "lexscan {}", "for x in", "for x =", "for ;; x =", "for x in { value }", "for x in { value; other }",
      "for x = {}", "for ; {}", "for ;; x = {}", "if f({}", "if array[{}", "if ({ field: value }",
      "if { value; other",
    ] {
    let source = "fn f() { \{header}\n}\nfn following() { 42 }"
    let parsed = @untyped_cst.parse_structure(source)
    if parsed.diagnostics_view().is_empty() {
      fail("missing diagnostic: \{source}")
    }
    @test.assert_eq(parsed.root().text(source), source[:])
    guard api_find_node(parsed.root(), n => {
        n.kind is Impl_Function && n.text(source) == "fn following() { 42 }"[:]
      })
      is Some(_) else {
      fail("lost following declaration: \{source}")
    }
  }
}
