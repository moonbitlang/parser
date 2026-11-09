///|
test "extend declarations accept line breaks after the soft keyword" {
  for
    prefix in [
      "", "pub ", "#deprecated\npub ", "fn previous() {}\n", "let previous = 1\n",
    ] {
    for gap in ["\n", "\r\n", " // one\n // two 🐇\n"] {
      for target in ["T", "@pkg.T", "&T"] {
        let source = prefix +
          "extend" +
          gap +
          target +
          " with Trait::{f, g as h}\nfn later() {}"
        assert_ast_equiv(source, label=source)
      }
    }
  }
}

///|
test "extend identifiers keep statement separators" {
  for
    source in [
      "let extend = 1\nlet x = extend\nlet y = 2", "fn extend(extend : Int) { extend\nnext() }",
      "fn T::extend(self : T) { self.extend()\nnext() }", "fn f() { let extend = 1\nextend\nnext() }",
      "fn f(extend : Int) { let xs = [extend\n]; xs }",
    ] {
    assert_ast_equiv(source, label=source)
  }
  assert_all_parsers_reject("extend;\nT with Trait::{f}")
}

///|
test "UUID block markers do not become documentation" {
  for newline in ["\n", "\r\n"] {
    for
      (line, content) in [
        ("///|UUID(00000000-0000-0000-0000-000000000000)", ""),
        ("///| \t UUID(anything) \t ", ""),
        ("///|UUID()", ""),
        ("/// UUID(anything)", " UUID(anything)"),
        ("///|用户正文 🐇", "用户正文 🐇"),
        ("///|UUID(x) trailing", "UUID(x) trailing"),
        ("///|UUID(x", "UUID(x"),
      ] {
      for body in ["", "/// body 🦀" + newline] {
        let source = line + newline + body + "let x = 1"
        assert_ast_equiv(source, label=source)
        let parsed = @untyped_cst.parse_structure(source)
        let (impls, reports) = parsed.to_impls()
        @test.assert_eq(reports.length(), 0)
        guard impls.to_array() is [TopLetDef(doc~, ..)] else {
          fail("expected let")
        }
        let expected = if body == "" {
          [content]
        } else {
          [content, " body 🦀"]
        }
        @test.assert_eq(doc.content.to_array(), expected)
        for parser in [@parser.Parser::Handrolled, MoonYacc] {
          let (impls, _) = @parser.parse_string(source, parser~)
          guard impls.to_array() is [TopLetDef(doc=other, ..)] else {
            fail("expected let")
          }
          @test.assert_eq(other.content.to_array(), expected)
          @test.assert_eq(other.loc, doc.loc)
        }
        let lex = @lexer.tokens_from_string(source, comment=true)
        let originals = lex.docstrings[0].to_array()
        @test.assert_eq(originals[0].1.content, line)
        @test.assert_eq(doc.loc.start, originals[0].0.start)
        @test.assert_eq(doc.loc.end, originals[originals.length() - 1].0.end)
        let comments = []
        boundary_comments(parsed.root(), comments)
        @test.assert_eq(comments[0].text(source), line[:])
        @test.assert_eq(comments[0].source_span, (0, line.length()))
      }
    }
  }
}

///|
test "trivia recovery keeps semicolons and subsequent declarations" {
  for
    broken in [
      "fn f() { let // keep\n x : = 1; x }", "struct S { field: // keep\n ; next: Int }",
      "struct S { field // keep\n ; : Int }", "fn f(x // keep\n ; : Int) {}", "fn f() { x as // keep\n ; T }",
      "impl T for S with fn[A]\nf(self : S) {}",
    ] {
    let source = broken + "\nfn later() {}"
    assert_all_parsers_reject(source)
    let parsed = @untyped_cst.parse_structure(source)
    boundary_assert_source(parsed.root(), source)
    guard api_find_node(parsed.root(), node => {
        node.kind.is_impl() && node.text(source) == "fn later() {}"[:]
      })
      is Some(_) else {
      fail("recovery lost the following declaration in " + source)
    }
  }
}
