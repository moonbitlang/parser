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
