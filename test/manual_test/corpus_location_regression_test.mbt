///|
priv struct CorpusLocationCollector {
  spans : Array[(String, Int, Int)]
}

///|
extend CorpusLocationCollector with IterVisitor::{base, visit_Impl}

///|
fn CorpusLocationCollector::add(
  self : Self,
  kind : String,
  loc : @basic.Location,
) -> Unit {
  self.spans.push((kind, loc.start.cnum, loc.end.cnum))
}

///|
impl IterVisitor for CorpusLocationCollector with fn visit_TraitDecl(self, decl) {
  self.add("trait", decl.loc)
  IterVisitor::visit_TraitDecl(self.base(), decl)
}

///|
impl IterVisitor for CorpusLocationCollector with fn visit_Pattern(self, pat) {
  if pat is Constant(loc~, ..) {
    self.add("pattern", loc)
  }
  IterVisitor::visit_Pattern(self.base(), pat)
}

///|
impl IterVisitor for CorpusLocationCollector with fn visit_Type(self, ty) {
  match ty {
    Option(loc~, ..) => self.add("option", loc)
    Object(constr) => self.add("object", constr.loc)
    _ => ()
  }
  IterVisitor::visit_Type(self.base(), ty)
}

///|
impl IterVisitor for CorpusLocationCollector with fn visit_Expr(self, expr) {
  match expr {
    Try(loc~, ..) => self.add("try", loc)
    Continue(loc~, ..) => self.add("continue", loc)
    _ => ()
  }
  IterVisitor::visit_Expr(self.base(), expr)
}

///|
impl IterVisitor for CorpusLocationCollector with fn visit_TypeVarBinder(
  self,
  binder,
) {
  self.add("binder", binder.name_loc)
  IterVisitor::visit_TypeVarBinder(self.base(), binder)
}

///|
impl IterVisitor for CorpusLocationCollector with fn visit_TypeVarConstraint(
  self,
  constraint,
) {
  self.add("constraint", constraint.loc)
}

///|
fn assert_corpus_locations(
  source : String,
  expected : Array[(String, Int, Int)],
) -> Unit raise Error {
  let name = "corpus_locations.mbt"
  let results : Array[(String, @syntax.Impls)] = []
  for
    (parser_name, parser) in [
      ("Handrolled", @parser.Parser::Handrolled),
      ("MoonYacc", MoonYacc),
    ] {
    let (impls, reports) = @parser.parse_string(source, name~, parser~)
    if !reports.is_empty() {
      fail("\{parser_name} rejected \{source}: \{Repr(reports)}")
    }
    results.push((parser_name, impls))
  }
  let cst = @untyped_cst.parse_structure(source, name~)
  @test.assert_eq(cst.diagnostics_view().length(), 0, msg=source)
  let (impls, reports) = cst.to_impls()
  @test.assert_eq(reports.length(), 0, msg=source)
  results.push(("CST", impls))
  let mut reference : Json? = None
  for (parser, impls) in results {
    let collector : CorpusLocationCollector = { spans: [], }
    impls.each(impl_ => collector.visit_Impl(impl_))
    @test.assert_eq(collector.spans, expected, msg="\{parser}: \{source}")
    let json = @basic.show_loc.protect(String, fn() {
      impls.map(impl_ => impl_.json_repr()).to_json()
    })
    if reference is Some(expected_json) {
      @test.assert_eq(json, expected_json, msg="\{parser}: \{source}")
    } else {
      reference = Some(json)
    }
  }
}

///|
test "corpus locations include trait prefixes but not trailing comments" {
  for
    declaration in [
      "trait T {}", "pub(open) trait T {}", "#deprecated\npub trait T {}", "declare trait T {}",
      "#deprecated\ndeclare pub(open) trait T {}",
    ] {
    assert_corpus_locations(declaration + " // trailing 🐇\n", [
      ("trait", 0, declaration.length()),
    ])
  }
}

///|
test "corpus locations include signs in negative patterns" {
  let prefix = "fn f(x) { match x { "
  for literal in ["-1", "- 2", "-3L", "-4.5F", "- 5.5F", "-6.5", "- 7.5"] {
    assert_corpus_locations(prefix + literal + " => 0 } }", [
      ("pattern", prefix.length(), prefix.length() + literal.length()),
    ])
  }
  assert_corpus_locations(prefix + "Number(-32601) => 0 } }", [
    ("pattern", prefix.length() + 7, prefix.length() + 13),
  ])
  for operator in ["..=", "..<"] {
    assert_corpus_locations(prefix + "-10 " + operator + " -2 => 0 } }", [
      ("pattern", prefix.length(), prefix.length() + 3),
      ("pattern", prefix.length() + 8, prefix.length() + 10),
    ])
  }
}

///|
test "corpus locations separate optional object types from their names" {
  let prefix = "fn f(x : "
  let start = prefix.length()
  for name in ["T", "@pkg.Trait"] {
    let end = start + 1 + name.length()
    assert_corpus_locations(prefix + "&" + name + ") {}", [
      ("object", start + 1, end),
    ])
    assert_corpus_locations(prefix + "&" + name + "?) {}", [
      ("option", start, end + 1),
      ("object", start + 1, end),
    ])
    assert_corpus_locations(prefix + "&" + name + "??) {}", [
      ("option", start, end + 2),
      ("option", start, end + 1),
      ("object", start + 1, end),
    ])
    assert_corpus_locations(prefix + "(&" + name + ")?) {}", [
      ("option", start, end + 3),
      ("object", start + 2, end + 1),
    ])
  }
}

///|
test "corpus locations start catch expressions at the pre-parsed body" {
  let prefix = "fn f() { "
  for
    body in [
      "()", "(x)", "((f()))", "(x, y)", "(x : Int)", "(f)()", "(xs)[0]", "(x).method()",
      "(x) |> f",
    ] {
    for suffix in [" catch { _ => 0 }", " catch { _ => 0 } noraise { x => x }"] {
      let expr = body + suffix
      assert_corpus_locations(prefix + expr + " }", [
        ("try", prefix.length(), prefix.length() + expr.length()),
      ])
    }
  }
}

///|
test "corpus locations cover complete trait constraints and retain binder names" {
  for name in ["Eq", "@json.FromJson"] {
    assert_corpus_locations("fn[T : " + name + "] f(x : T) {}", [
      ("binder", 3, 4),
      ("constraint", 7, 7 + name.length()),
    ])
  }
  assert_corpus_locations("fn[T : Eq + @json.FromJson] f(x : T) {}", [
    ("binder", 3, 4),
    ("constraint", 7, 9),
    ("constraint", 12, 26),
  ])
  assert_corpus_locations("trait T : Eq + @json.FromJson {}", [
    ("trait", 0, 32),
    ("constraint", 10, 12),
    ("constraint", 15, 29),
  ])
}

///|
test "corpus locations exclude comments after continue" {
  for newline in ["\n", "\r\n"] {
    let prefix = "fn f() {" + newline + "  "
    for
      statement in [
        "continue", "continue label~", "continue label~~", "continue 1", "continue (1)",
        "continue label~ 1, (2)", "continue label~~ (1), (2)",
      ] {
      assert_corpus_locations(
        prefix + statement + " // trailing 🐇" + newline + "}",
        [("continue", prefix.length(), prefix.length() + statement.length())],
      )
    }
  }
  assert_corpus_locations(
    "fn f() {\n  continue // first\n  continue // second\n}",
    [("continue", 11, 19), ("continue", 31, 39)],
  )
}
