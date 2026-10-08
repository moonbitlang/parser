///|
test "continue lowering preserves comments and complete CST source spans" {
  for newline in ["\n", "\r\n"] {
    for
      statement in [
        "continue", "continue label~", "continue label~~", "continue (1)", "continue label~ 1, (2)",
        "continue label~~ (1), (2)",
      ] {
      let prefix = "// 🐇" + newline
      let source = prefix + statement + " // trailing 🦀" + newline
      let parsed = @untyped_cst.parse_expression(source)
      @test.assert_eq(parsed.diagnostics_view().length(), 0)
      let root = parsed.root()
      @test.assert_eq(root.source_span, (0, source.length()))
      @test.assert_eq(root.text(source), source[:])
      guard api_find_node(root, node => node.kind is Expr_Continue)
        is Some(continue_node) else {
        fail("expected continue")
      }
      let span_before = continue_node.source_span
      let text_before = continue_node.text(source).to_owned()
      // Check the actual retained comment token, not just its source text.
      guard api_find_node(root, node => {
          node.kind is Syntax_Token(TK_COMMENT) &&
          node.text(source).has_prefix("// trailing")
        })
        is Some(comment) else {
        fail("expected retained trailing comment")
      }
      @test.assert_eq(comment.text(source), "// trailing 🦀"[:])
      let (expr, reports) = parsed.to_expr()
      @test.assert_eq(reports.length(), 0)
      guard expr is Some(Continue(loc~, ..)) else {
        fail("expected lowered continue")
      }
      // AST offsets count characters, whereas source spans count UTF-16 units.
      @test.assert_eq(loc.start.cnum, prefix.length() - 1)
      @test.assert_eq(loc.end.cnum, prefix.length() - 1 + statement.length())
      @test.assert_eq(loc.start.lnum, 2)
      @test.assert_eq(loc.end.lnum, 2)
      @test.assert_eq(continue_node.source_span, span_before)
      @test.assert_eq(continue_node.text(source).to_owned(), text_before)
      @test.assert_eq(root.text(source), source[:])
    }
  }
}
