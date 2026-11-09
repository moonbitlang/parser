///|
// Expected content was checked against moon_objinfo -dump-ast. Keep the raw
// lexer comments intact: only the chosen document block changes its location.
test "documentation block selection and slash conversion match upstream" {
  for newline in ["\n", "\r\n"] {
    for
      (lines, expected, first) in [
        (["///|", "///|"], [""], 1),
        (["/// old", "///|", "/// new 🐇"], ["", " new 🐇"], 1),
        (["/// old", "///|UUID(x)", "/// new"], ["", " new"], 1),
        (["///| first", "///| second"], [" second"], 1),
        (["/// old", "  ///|", "/// next"], [" old", "|", " next"], 0),
        (
          ["/// first", " ///|UUID(x)", "/// next"],
          [" first", "|UUID(x)", " next"],
          0,
        ),
        (["//// alpha", "///// beta"], [" alpha", " beta"], 0),
        (["////| first", "////| second"], [" first", "| second"], 0),
        (["///|UUID(x)", " ////|UUID(y)"], ["", "|UUID(y)"], 0),
      ] {
      for
        (prefix, decl) in [
          ("", "let x = 1"),
          ("struct S {" + newline, "x: Int" + newline + "}"),
          ("enum E {" + newline, "C" + newline + "}"),
        ] {
        let source = prefix + lines.join(newline) + newline + decl
        assert_ast_equiv(source, label=source)
        let parsed = @untyped_cst.parse_structure(source)
        let (impls, reports) = parsed.to_impls()
        assert_eq(reports.length(), 0)
        let doc = match impls.head().unwrap() {
          TopLetDef(doc~, ..) => doc
          TopTypeDef({ components: Record(fields~, ..), .. }) =>
            fields.head().unwrap().doc
          TopTypeDef({ components: Variant(constrs), .. }) =>
            constrs.head().unwrap().doc
          _ => fail("expected documented declaration")
        }
        assert_eq(doc.content.to_array(), expected)
        let lex = @lexer.tokens_from_string(source, comment=true)
        let originals = lex.docstrings[0].to_array()
        assert_eq(originals.length(), lines.length())
        assert_eq(doc.loc.start, originals[first].0.start)
        assert_eq(doc.loc.end, originals[originals.length() - 1].0.end)
        let comments = []
        boundary_comments(parsed.root(), comments)
        assert_eq(comments.length(), lines.length())
        for i, comment in comments {
          assert_eq(comment.text(source), lines[i].trim_start())
          assert_eq(originals[i].1.content[:], lines[i].trim_start())
        }
        // Repeated lowering must not consume or rewrite lexer-owned comments.
        assert_eq(impls_json(parsed.to_impls().0), impls_json(impls))
      }
    }
  }
}

///|
// The oracle is an unmodified moon_objinfo -dump-ast export of the input fixture.
// It is checked in so these tests do not require an upstream checkout.
test "documentation AST matches the upstream export" {
  let base = "untyped_cst/fixtures/docstring_boundaries"
  let source = @fs.read_file_to_string(base + ".input")
  let oracle = @json.parse(@fs.read_file_to_string(base + ".upstream.json"))
  guard oracle
    is {
      "has_parse_error": false,
      "has_deprecated_syntax": false,
      "impls": expected,
      ..
    } else {
    fail("expected an accepted, non-deprecated upstream fixture")
  }
  assert_ast_equiv(source)
  let (impls, _) = @untyped_cst.parse_structure(source).to_impls()
  let actual = @basic.show_loc.protect(Hidden, () => {
    Json(impls.map(impl_ => impl_.json_repr()))
  })
  assert_eq(actual, expected)
}
