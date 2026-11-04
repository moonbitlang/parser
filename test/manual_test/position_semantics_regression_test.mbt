///|
fn position_semantics_impls(
  source : String,
) -> Array[@syntax.Impls] raise Error {
  let results : Array[@syntax.Impls] = []
  let name = "position_semantics.mbt"
  for parser in [@parser.Parser::Handrolled, MoonYacc] {
    let (impls, reports) = @parser.parse_string(source, name~, parser~)
    @test.assert_eq(reports.length(), 0, msg=source)
    results.push(impls)
  }
  let cst = @untyped_cst.parse_structure(source, name~)
  @test.assert_eq(cst.diagnostics_view().length(), 0, msg=source)
  let (impls, reports) = cst.to_impls()
  @test.assert_eq(reports.length(), 0, msg=source)
  results.push(impls)
  let jsons = @basic.show_loc.protect(String, fn() {
    results.map(impls => Json(impls.map(impl_ => impl_.json_repr())))
  })
  @test.assert_eq(jsons[1], jsons[0], msg="MoonYacc: " + source)
  @test.assert_eq(jsons[2], jsons[0], msg="CST: " + source)
  results
}

///|
fn assert_position_semantics_span(
  source : String,
  loc : @basic.Location,
  start : Int32,
  text : String,
) -> Unit raise Error {
  @test.assert_eq(loc.start.cnum, start, msg=source)
  @test.assert_eq(loc.end.cnum, start + text.length(), msg=source)
  @test.assert_eq(source[loc.start.cnum:loc.end.cnum], text[:])
  // Check line/column metadata independently of offsets and parser agreement.
  for pos in [loc.start, loc.end] {
    let mut line = 1
    let mut bol = 0
    for i in 0..<pos.cnum {
      if source[i] == '\n' {
        line += 1
        bol = i + 1
      }
    }
    @test.assert_eq(pos.lnum, line)
    @test.assert_eq(pos.bol, bol)
  }
}

///|
test "declaration spans and docs start after trivia in every parser" {
  let before_doc = "/// previous\nlet previous = 0\n\n// separator\n\n"
  let prefix = before_doc + "/// current\n  "
  let declarations = [
    "fn f() {}", "pub fn f() {}", "async fn f() {}", "#deprecated\npub async fn f() {}",
    "declare fn f()", "#deprecated\ndeclare pub async fn f() -> Int", "extern \"C\" fn f() = \"m\" \"f\"",
    "pub extern \"C\" fn f() = \"code\"", "extern \"C\" fn f() =\n  #|code", "fn f() = \"m\" \"f\"",
    "fn f() = \"code\"", "fn f() =\n  #|code", "let value = 1", "pub let value : Int = 1",
    "#deprecated\npub let value = 1", "const VALUE = 1", "#deprecated\ndeclare pub let value : Int",
    "type T", "#deprecated\ndeclare pub type T[A]", "type T = Int", "pub type T[A] = Array[A] derive(Eq)",
    "suberror E", "pub suberror E derive(Eq)", "#deprecated\ndeclare pub suberror E",
    "suberror E { Bad } derive(Eq)", "struct S { value : Int } derive(Eq)", "pub struct S(Int) derive(Eq)",
    "#deprecated\ndeclare pub struct S[A] { value : A }", "enum E { A } derive(Eq)",
    "#deprecated\ndeclare pub enum E[A] { A(A) }", "using @pkg { type T }", "#deprecated\npub using @pkg { f }",
  ]
  for declaration in declarations {
    let source = prefix + declaration + "\n// trailing\n"
    for impls in position_semantics_impls(source) {
      @test.assert_eq(impls.length(), 2)
      for i, impl_ in impls.to_array() {
        let doc = match impl_ {
          TopFuncDef(fun_decl~, ..) => fun_decl.doc
          TopTypeDef(decl) => decl.doc
          TopLetDef(doc~, ..) | TopUsing(doc~, ..) => doc
          _ => fail("unexpected declaration")
        }
        let expected_doc = if i == 0 { "/// previous" } else { "/// current" }
        @test.assert_eq(doc.content.is_empty(), false)
        let doc_start = if i == 0 { 0 } else { before_doc.length() }
        assert_position_semantics_span(source, doc.loc, doc_start, expected_doc)
        if i == 1 {
          assert_position_semantics_span(
            source,
            impl_.loc(),
            prefix.length(),
            declaration,
          )
          if impl_ is TopFuncDef(fun_decl~, ..) {
            @test.assert_eq(fun_decl.loc.start, impl_.loc().start)
          }
        }
      }
    }
  }
}

///|
priv struct PositionSemanticsCollector {
  records : Array[@basic.Location]
  functions : Array[@basic.Location]
  arrows : Array[@basic.Location]
  names : Array[@basic.Location]
}

///|
extend PositionSemanticsCollector with IterVisitor::{base, visit_Impl}

///|
impl IterVisitor for PositionSemanticsCollector with fn visit_Expr(self, expr) {
  if expr is RecordUpdate(loc~, ..) {
    self.records.push(loc)
  }
  IterVisitor::visit_Expr(self.base(), expr)
}

///|
impl IterVisitor for PositionSemanticsCollector with fn visit_Func(self, func) {
  self.functions.push(func.loc)
  IterVisitor::visit_Func(self.base(), func)
}

///|
impl IterVisitor for PositionSemanticsCollector with fn visit_Type(self, ty) {
  match ty {
    Arrow(loc~, ..) => self.arrows.push(loc)
    Name(constr_id~, ..) => self.names.push(constr_id.loc)
    _ => ()
  }
  IterVisitor::visit_Type(self.base(), ty)
}

///|
fn position_semantics_collect(
  impls : @syntax.Impls,
) -> PositionSemanticsCollector {
  let collector : PositionSemanticsCollector = {
    records: [],
    functions: [],
    arrows: [],
    names: [],
  }
  impls.each(impl_ => collector.visit_Impl(impl_))
  collector
}

///|
test "record updates preserve runtime spans in surrounding expressions" {
  let prefix = "let previous = 0\n\nlet result =  "
  for
    record in [
      "{ ..r }", "{ ..r, }", "{ ..r, x: 1 }", "T::{ ..r }", "T::{ ..r, }", "T::{ ..r, x: 1 }",
    ] {
    for
      (before, after) in [
        ("", ""),
        ("", " ..< z"),
        ("z ..= ", ""),
        ("", " ..<= z"),
        ("", "()"),
        ("", ".x"),
        ("", "[0]"),
      ] {
      let expression = before + record + after
      let source = prefix + expression
      for impls in position_semantics_impls(source) {
        let collector = position_semantics_collect(impls)
        @test.assert_eq(collector.records.length(), 1)
        assert_position_semantics_span(
          source,
          collector.records[0],
          prefix.length() + before.length(),
          record,
        )
        guard impls.to_array()[1] is TopLetDef(expr~, ..) else {
          fail("expected let")
        }
        assert_position_semantics_span(
          source,
          expr.loc(),
          prefix.length(),
          expression,
        )
      }
    }
  }
}

///|
test "anonymous functions and arrow types start at their first token" {
  for async_prefix in ["", "async "] {
    let prefix = "let previous = 0\n\nlet result =  "
    let function = async_prefix + "fn(x : Int) -> Int { x }"
    for suffix in ["", "(1)"] {
      let source = prefix + function + suffix
      for impls in position_semantics_impls(source) {
        let collector = position_semantics_collect(impls)
        @test.assert_eq(collector.functions.length(), 1)
        assert_position_semantics_span(
          source,
          collector.functions[0],
          prefix.length(),
          function,
        )
        guard impls.to_array()[1] is TopLetDef(expr~, ..) else {
          fail("expected let")
        }
        assert_position_semantics_span(
          source,
          expr.loc(),
          prefix.length(),
          function + suffix,
        )
      }
    }
    for args in ["()", "(Int)", "(Int,)", "(Int, String)"] {
      let prefix = "let previous = 0\n\ntype Callback =  "
      let arrow = async_prefix + args + " -> Int"
      let source = prefix + arrow
      for impls in position_semantics_impls(source) {
        let collector = position_semantics_collect(impls)
        @test.assert_eq(collector.arrows.length(), 1)
        assert_position_semantics_span(
          source,
          collector.arrows[0],
          prefix.length(),
          arrow,
        )
      }
    }
  }
}

///|
test "qualified constructor names exclude type arguments options and groups" {
  let prefix = "type Alias = "
  for
    (ty, expected) in [
      ("@pkg.Type", [(0, "@pkg.Type")]),
      ("@pkg.Type[Int]", [(0, "@pkg.Type"), (10, "Int")]),
      ("@pkg.Type?", [(0, "@pkg.Type")]),
      ("(@pkg.Type[Int])?", [(1, "@pkg.Type"), (11, "Int")]),
      (
        "@pkg.Outer[@other.Inner[Int]?]?",
        [(0, "@pkg.Outer"), (11, "@other.Inner"), (24, "Int")],
      ),
    ] {
    let source = prefix + ty
    for impls in position_semantics_impls(source) {
      let collector = position_semantics_collect(impls)
      @test.assert_eq(collector.names.length(), expected.length())
      for i, (start, text) in expected {
        assert_position_semantics_span(
          source,
          collector.names[i],
          prefix.length() + start,
          text,
        )
      }
    }
  }
}
