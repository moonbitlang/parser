///|
fn assert_no_location_metadata(node : @untyped_cst.CstNode) -> Unit raise {
  for (label, child) in node.children {
    if label is Some(label) {
      @test.assert_eq(label.has_suffix("_loc"), false)
    }
    assert_no_location_metadata(child)
  }
}

///|
test "normal and recovered CSTs contain no location copies" {
  for
    source in [
      "pub async fn f(x? : Int) { x }", "fn &Obj::f() {}", "trait T { async fn f(Self) -> Int; fn g(Self) -> Int }",
      "fn outer() { async fn inner(x) { x }; inner(1) }", "fn f() {} // trailing 🐇\n",
      "async fn f(",
    ] {
    assert_no_location_metadata(@untyped_cst.parse_structure(source).root())
  }
  for
    source in [
      "value |> _ => { 1 }", "value |> (_) => { 1 }", "async fn(x) { x }", "f(x~, y?, z=value, w?=value)",
      "f(x~, y?, z=)", "f(optional?", "try f() catch { _ => 0 } noraise { value => value }",
      "try! f()", "(f)() catch { _ => 0 }", "((f())) catch { _ => 0 }", "((f())) catch {",
      "try f() catch", "array[1:2]", "array[1:", "[ for i = 0; i < 1; i = i + 1 => i ]",
      "label~: while true { break label~ }", "label~: for x in xs { x }", "label~: value",
      "// 🐇\nlabel~:", "label~: // 🐇\n", "label~: { continue label~~ }", "(\n#| 🐇\n// between\n$| two \\{x}\n)",
    ] {
    assert_no_location_metadata(@untyped_cst.parse_expression(source).root())
  }
}

///|
test "derived locations preserve handrolled AST and diagnostics at syntax boundaries" {
  for
    source in [
      "async fn &T::f(x? : Int) {}", "pub async fn f(x : async (Int) -> Int) { x }",
      "fn outer() { async fn inner(x) { x }; inner(1) }", "trait T { async fn f(Self) -> Int; fn g(Self) -> Int }",
      "pub fn f() {} // trailing 🐇\n", "extern \"js\" fn f() = \"🐇\"",
    ] {
    assert_handrolled_cst_ast_equiv(source)
  }
  for
    source in [
      "value |> x => { x }", "try! f()", "try f() catch { _ => 0 } noraise { x => x }",
      "try f() catch { // 🐇\n _ => 0 } noraise { // 🦀\n x => x }", "((f))() catch { _ => 0 }",
      "((f())) catch { _ => 0 }", "(f)() // 🐇\n catch { _ => 0 }", "array[1:2]",
      "array[:2]", "array[1:]", "f(label~, optional?, explicit?=1)", "label~: while true { break label~ }",
      "label~: { break label~~ }", "label~: { continue label~~ }", "// 🐇\n(\n#| one\n// 🦀\n$| two \\{x}\n)",
      "f(async fn(x) { x }) catch { _ => 0 }", "array[try! f():2]",
    ] {
    let issue = handrolled_cst_expression_issue(source)
    if issue is Some("AST") {
      assert_expr_ast_equiv(source)
    }
    if issue is Some(problem) {
      fail("\{problem}: \{source}")
    }
  }
  for
    source in [
      "array[1:", "array[:", "(f)() catch", "try f() catch", "// 🐇\nlabel~:",
      "label~: // 🦀\n", "try!",
    ] {
    let issue = handrolled_cst_expression_issue(source)
    if issue is Some("AST") {
      assert_expr_ast_equiv(source)
    }
    if issue is Some(problem) {
      fail("\{problem}: \{source}")
    }
    assert_no_location_metadata(@untyped_cst.parse_expression(source).root())
  }
}

///|
test "lexer tokens retain exact kind payload and character and UTF-16 ranges" {
  let source = "// 🐇\nfn f(x) {\n let s = \"🐇\\{x}\"\n x + 1; true && false\n}"
  let lex = @lexer.tokens_from_string(source, comment=true)
  let parsed = @untyped_cst.parse_structure(source)
  @test.assert_eq(parsed.diagnostics_view().length(), 0)
  for (token, start, end) in lex.tokens {
    if token.kind() == @tokens.TokenKind::TK_NEWLINE {
      continue
    }
    guard api_find_node(parsed.root(), node => {
        node.kind.token_kind() == Some(token.kind()) &&
        node.loc.start == start &&
        node.loc.end == end
      })
      is Some(found) else {
      fail("missing token \{token.to_expect_string()} at \{start.cnum}")
    }
    @test.assert_eq(found.leaf_payload(), @node.token_payload(token))
    @test.assert_eq(found.kind is @node.NodeKind::Syntax_Token(_), true)
    if token.kind() == @tokens.TokenKind::TK_FN {
      @test.assert_eq(found.loc.start.cnum, 5)
      @test.assert_eq(found.source_span, (6, 8))
      @test.assert_eq(found.text(source), "fn"[:])
    }
  }
}
