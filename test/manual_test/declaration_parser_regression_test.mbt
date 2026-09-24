///|
test "declare let without initializer agrees across source parsers" {
  for
    source in [
      "declare let x", "declare let value : Int", "declare pub let value : Int",
      "declare let value : Int = 1", "declare let value : Int\nlet other = 1", "declare let value : Int\nfn use() { () }",
    ] {
    expect_accepted(source, "declare_let_alignment.mbt")
  }
  let source = "declare let value : Int"
  let parsers : Array[@parser.Parser] = [Handrolled, MoonYacc]
  for parser in parsers {
    let (impls, reports) = @parser.parse_string(
      source,
      name="declare_let_alignment.mbt",
      parser~,
    )
    assert_true(reports.is_empty())
    assert_declared_let_hole(impls)
  }
  let (cst_impls, reports) = @untyped_cst.parse_structure(
    source,
    name="declare_let_alignment.mbt",
  ).to_impls()
  assert_true(reports.is_empty())
  assert_declared_let_hole(cst_impls)
}

///|
fn assert_declared_let_hole(impls : @syntax.Impls) -> Unit raise Error {
  match impls.head().unwrap() {
    TopLetDef(
      binder~,
      expr=Hole(loc~, kind=Synthesized),
      is_declare=true,
      is_constant=false,
      ..
    ) => {
      assert_eq(binder.name, "value")
      assert_eq(loc, binder.loc)
      assert_eq(loc.start.cnum, 12)
      assert_eq(loc.end.cnum, 17)
    }
    _ => fail("expected an uninitialized declare let with a binder hole")
  }
}

///|
test "let initialization errors agree across source parsers" {
  expect_rejected("let value", "let_without_equal.mbt")
  expect_rejected("const VALUE", "const_without_equal.mbt")
  expect_rejected("declare let value =", "declare_let_without_rhs.mbt")
}

///|
test "qualified extenum target spans agree across source parsers" {
  let source = "extenum @pkg.Expr[T] += { C }"
  expect_accepted(source, "qualified_extenum_alignment.mbt")
  let parsers : Array[@parser.Parser] = [Handrolled, MoonYacc]
  for parser in parsers {
    let (impls, reports) = @parser.parse_string(
      source,
      name="qualified_extenum_alignment.mbt",
      parser~,
    )
    assert_true(reports.is_empty())
    assert_qualified_extenum_spans(impls)
  }
  let (cst_impls, reports) = @untyped_cst.parse_structure(
    source,
    name="qualified_extenum_alignment.mbt",
  ).to_impls()
  assert_true(reports.is_empty())
  assert_qualified_extenum_spans(cst_impls)
}

///|
fn assert_qualified_extenum_spans(impls : @syntax.Impls) -> Unit raise Error {
  match impls.head().unwrap() {
    TopTypeDef(decl) => {
      assert_eq(decl.tycon, "Expr")
      assert_eq(decl.tycon_loc.start.cnum, 8)
      assert_eq(decl.tycon_loc.end.cnum, 17)
      match decl.components {
        ExtendEnum(target~, ..) => assert_eq(target.loc, decl.tycon_loc)
        _ => fail("expected an extenum extension")
      }
    }
    _ => fail("expected a type declaration")
  }
}
