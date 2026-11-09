///|
fn boundary_type(impl_ : @syntax.Impl) -> @syntax.Type raise {
  match impl_ {
    TopTypeDef({ components: Alias(ty), .. })
    | TopLetDef(ty=Some(ty), ..)
    | TopImpl(self_ty=Some(ty), ..) => ty
    TopFuncDef(fun_decl~, ..) =>
      if fun_decl.return_type is Some(ty) {
        ty
      } else {
        match fun_decl.decl_params.unwrap().head().unwrap() {
          Positional(ty=Some(ty), ..) => ty
          _ => fail("expected typed parameter")
        }
      }
    _ => fail("expected type-bearing declaration")
  }
}

///|
test "type syntax locations exclude comments and retain real parentheses" {
  for newline in ["\n", "\r\n"] {
    for header in ["", "---" + newline + "// 🦀" + newline + "---" + newline] {
      for
        ty in [
          "Array[Int]", "Int?", "(Int)?", "((Int))?", "Array[Int]?", "(Int) -> String",
          "async (Int) -> String", "((Int) -> String)?",
        ] {
        for
          (before, after) in [
            ("type Alias = ", ""),
            ("let x : ", " = value"),
            ("fn f(x : ", ") {}"),
            ("fn f() -> ", " {}"),
            ("impl Show for ", " with fn output(self) {}"),
          ] {
          let prefix = header + before + "// 🐇" + newline + "  "
          let source = prefix + ty + after
          let cst = @untyped_cst.parse_mbtx(source, name="types.mbtx")
          assert_eq(cst.diagnostics.length(), 0, msg=source)
          let (impls, reports) = cst.body.to_impls()
          assert_eq(reports.length(), 0, msg=source)
          for parser in [@parser.Parser::Handrolled, MoonYacc] {
            let (other, reports) = @parser.parse_mbtx_string(
              source,
              name="types.mbtx",
              parser~,
            )
            assert_eq(reports.length(), 0, msg=source)
            assert_eq(impls_json(other.body), impls_json(impls), msg=source)
          }
          let loc = boundary_type(impls.head().unwrap()).loc()
          let mut cnum = 0
          for _ in prefix {
            cnum += 1
          }
          assert_eq(loc.start.cnum, cnum, msg=source)
          assert_eq(loc.end.cnum, cnum + ty.length(), msg=source)
          assert_eq(loc.start.lnum, if header == "" { 2 } else { 5 })
          assert_eq(loc.start.column(), 3)
          assert_eq(loc.start.fname, "types.mbtx")
          let comments = []
          boundary_comments(cst.body.root(), comments)
          assert_eq(comments.length(), 1)
          assert_eq(comments[0].text(source), "// 🐇"[:])
          assert_eq(cst.body.root().text(source), source[:])
        }
      }
    }
  }
}

///|
test "label argument semantics survive trivia before their operators" {
  for newline in ["\n", "\r\n"] {
    for
      (head, tail, kind) in [
        ("x", "= 1", 0),
        ("x", "?= 1", 1),
        ("x?", "= 1", 1),
        ("x?", "", 2),
        ("x~", "", 3),
      ] {
      let source = "let r = f(" + head + " // 🐇" + newline + tail + ")"
      assert_ast_equiv(source, label=source)
      let (impls, _) = @untyped_cst.parse_structure(source).to_impls()
      guard impls.head() is Some(TopLetDef(expr=Apply(args~, ..), ..)) else {
        fail("expected call")
      }
      let actual = match args.head().unwrap().kind {
        Labelled(_) => 0
        LabelledOption(..) => 1
        LabelledOptionPun(..) => 2
        LabelledPun(_) => 3
        _ => fail("labelled argument became positional")
      }
      assert_eq(actual, kind)
    }
  }
}
