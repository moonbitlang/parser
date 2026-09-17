///|
// Capture every node, payload, label, location, span, and repeated child before
// changing construction. Keep character positions separate from UTF-16 spans.
fn construction_tree_snapshot(
  t : @test.Test,
  node : @untyped_cst.CstNode,
  depth : Int,
  label : String?,
) -> Unit {
  let indent = "  ".repeat(depth)
  t.writeln(
    "\{indent}\{Repr(label)} \{Repr(node.kind)} loc=\{Repr(node.loc.start)},\{Repr(node.loc.end)} span=\{Repr(node.source_span)}",
  )
  for (label, child) in node.children {
    construction_tree_snapshot(t, child, depth + 1, label)
  }
}

///|
test "construction preserves complete expression trees" (t : @test.Test) {
  for
    source in [
      "@pkg.value", "@pkg.Type::make", "@pkg.Type::Case(x~, y?=value)", "(+1, -2, -3L, +4.5, -6.5, - -7)",
      "f(x~, y?, z=value, w?=value)", "{ x, y: value }", "{ \"🐇\": -1, \"b\": 2 }",
      "match x { Case(value~) => value; { a, b: value, .. } => value; { \"k\": value, .. } => value }",
      "match x { -1 => 0; -2L => 1; -3.5 => 2; @pkg.Type::Case(x~) => x }", "async fn(x) { x }",
      "(x, y) => x", "label~: while true { break label~ }", "label~: for i = 0; i < 2; i = i + 1 { continue label~ }",
      "label~: for x in xs { x }", "try f() catch { _ => 0 } else { x => x }", "f() catch { _ => 0 }",
      "try? f()", "// 🐇\nmatch x { // first\n // second\n { a, .. } => a }", "// 🐇\nmatch x { // first\n // second\n { \"k\": value, .. } => value }",
      "match x { (a) => a; [a, ..rest] => a }",
    ] {
    let parsed = @untyped_cst.parse_expression(source)
    t.writeln("SOURCE \{source.escape()}")
    construction_tree_snapshot(t, parsed.root(), 0, None)
    t.writeln(
      diagnostics_equiv_reports_json(
        reports_from_view(parsed.diagnostics_view()),
      ).stringify(),
    )
  }
  t.snapshot(filename="construction_expressions.txt")
}

///|
test "construction preserves complete declaration trees" (t : @test.Test) {
  for
    source in [
      "pub async fn f(x : Int) -> Int { x }", "declare async fn f(x : Int) -> Int",
      "fn Type::method(self : Type) -> Int { 1 }", "impl Trait for Type with method(self) { 1 }",
      "trait T { async fn f(Self) -> Int; fn g(Self) -> Int }", "pub struct S { x : Int; mut y : Int }",
      "enum E { C(Int); D(x~ : Int) }", "type Alias = Int", "type Abstract", "suberror E { C }",
      "let x = 1\nconst y = 2", "fn f() { fn nested(x) { x }; nested(1) }", "// 🐇\nasync test \"name\" { 1 } // trailing\n",
      "pub fn f() = \"stub\"", "import { \"pkg\" }", "pub using @pkg { type T, f }",
    ] {
    let parsed = @untyped_cst.parse_structure(source)
    t.writeln("SOURCE \{source.escape()}")
    construction_tree_snapshot(t, parsed.root(), 0, None)
    t.writeln(
      diagnostics_equiv_reports_json(
        reports_from_view(parsed.diagnostics_view()),
      ).stringify(),
    )
  }
  t.snapshot(filename="construction_declarations.txt")
}

///|
test "construction preserves recovery trees and zero width anchors" (
  t : @test.Test,
) {
  for
    source in [
      "label~: value", "// 🐇\nlabel~:", "match x { // first\n // second\n { a: , b, .. } => 0 }",
      "match x { // first\n // second\n { \"k\": , .. } => 0 }", "f(x~, y?, z=)",
      "try f() catch { _ => }", "async fn {", "// 🐇\nx +\n\nlet", "{ \"k\": }",
    ] {
    let parsed = @untyped_cst.parse_expression(source)
    t.writeln("SOURCE \{source.escape()}")
    construction_tree_snapshot(t, parsed.root(), 0, None)
    t.writeln(
      diagnostics_equiv_reports_json(
        reports_from_view(parsed.diagnostics_view()),
      ).stringify(),
    )
  }
  t.snapshot(filename="construction_recovery.txt")
}

///|
test "construction preserves lexscan roles and tuple argument recovery" (
  t : @test.Test,
) {
  for
    source in [
      "lexscan s { name => 0; _ => 1; ... }", "lexscan s { re\"a\" + re\"b\" => 0; re\"a\" | re\"b\" => 1 }",
      "lexscan s { re\"a\" as name => 0; @pkg.pattern => 1 }", "lexscan s { (re\"a\", name=value) => 0 }",
      "lexscan s { name as alias => 0; name ..< last => 1 }", "lexscan s { name | _ => 0; name : Int => 1 }",
      "// 🐇\nlexscan s { re\"a\" value }", "lexscan s { re\"a\\{x}\" => 0 }",
      "(x~, y?)", "(x~, y?) => x", "f(x~, y?)", "// 🐇\ntry! f()", "(f()) catch { _ => 0 }",
    ] {
    let parsed = @untyped_cst.parse_expression(source)
    t.writeln("SOURCE \{source.escape()}")
    construction_tree_snapshot(t, parsed.root(), 0, None)
    t.writeln(
      diagnostics_equiv_reports_json(
        reports_from_view(parsed.diagnostics_view()),
      ).stringify(),
    )
  }
  t.snapshot(filename="construction_lexscan_and_tuples.txt")
}
