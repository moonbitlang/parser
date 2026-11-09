///|
// Each pair isolates one grammar boundary. Gaps never contain semicolons.
fn trivia_boundaries() -> Array[(String, String)] {
  [
    ("fn f() { try g()", "catch { _ => () } }"),
    ("fn f() { try g() catch", "{ _ => () } }"),
    ("fn f() { try g() catch { _ => () }", "noraise { x => x } }"),
    ("fn f() { g()", "catch { _ => () } }"),
    ("fn f() { g() catch", "{ _ => () } }"),
    ("fn f() { g() catch { _ => () }", "noraise { x => x } }"),
    ("fn f() { if true {} else", "{} }"),
    ("fn f() { if true {} else", "if false {} else {} }"),
    ("fn f() { guard true", "else {} }"),
    ("fn f() { guard true else", "{} }"),
    ("fn f() { guard! true else", "{} }"),
    ("let f = fn", "(x) { x }"),
    ("let f = async", "fn(x) { x }"),
    ("let f = fn(x)", "-> Int { x }"),
    ("fn f() { let", "mut x = 1; x }"),
    ("fn f() { let mut", "x = 1; x }"),
    ("fn f() { let x: Int", "= 1; x }"),
    ("fn f() { let mut x: Int", "= 1; x }"),
    ("fn f() { proof_let", "x = 1; x }"),
    ("fn f() { proof_let x", "= 1; x }"),
    ("fn f() { proof_let x =", "1; x }"),
    ("fn f() { proof_let", "extend = 1; extend }"),
    ("fn f() { label~", ": {} }"),
    ("fn f() { label~~", ": {} }"),
    ("fn f() { label~", ": while true {} }"),
    ("fn f() { label~", ": for ;; {} }"),
    ("fn f() { continue label~", ": for ;; {} }"),
    ("fn f() { continue label~~", ": {} }"),
    ("fn f() { proof_assert ∀", "x: Int, x == x }"),
    ("fn f() { proof_assert ∀ x", ": Int, x == x }"),
    ("fn f() { proof_assert ∀ x:", "Int, x == x }"),
    ("fn f() { proof_assert ∀ x: Int", ", x == x }"),
    ("fn f() { proof_assert ∀ x: Int,", "x == x }"),
    ("fn f() { proof_assert ∀ extend: Int,", "∀ y: Int, extend == y }"),
    ("fn f() { proof_assert ∃", "x: Int, x == x }"),
    ("fn f() { proof_assert ∃ x", ": Int, x == x }"),
    ("fn f() { proof_assert ∃ x:", "Int, x == x }"),
    ("fn f() { proof_assert ∃ x: Int", ", x == x }"),
    ("fn f() { proof_assert ∃ x: Int,", "x == x }"),
    ("fn f() { proof_assert ∃ extend: Int,", "∀ y: Int, extend == y }"),
    ("let xs = [for", "i = 0; i < 2; i = i + 1 => i]"),
    ("let xs = [for i", "= 0; i < 2; i = i + 1 => i]"),
    ("let xs = [for i =", "0; i < 2; i = i + 1 => i]"),
    ("let xs = [for i = 0; i < 2; i", "= i + 1 => i]"),
    ("let xs = [for x in xs; extend", "= 0; extend = extend + 1 => x]"),
    ("let xs = [for x in xs; extend = 0; extend", "= extend + 1 => x]"),
    ("let xs = [|for", "i = 0; i < 2; i = i + 1 => i|]"),
    ("let xs = [|for i", "= 0; i < 2; i = i + 1 => i|]"),
    ("let xs = [|for i =", "0; i < 2; i = i + 1 => i|]"),
    ("let xs = [|for i = 0; i < 2; i", "= i + 1 => i|]"),
    ("let xs = [|for x in xs; extend", "= 0; extend = extend + 1 => x|]"),
    ("let xs = [|for x in xs; extend = 0; extend", "= extend + 1 => x|]"),
    ("fn f() { g(x", "= 1) }"),
    ("fn f() { g(x", "?= value) }"),
    ("fn f() { g(x?", "= value) }"),
    ("fn f() { g(x~", ", y=2) }"),
    ("fn f() { letrec", "f = x => x; f(1) }"),
    ("fn f() { letrec f", ": (Int) -> Int = x => x; f(1) }"),
    ("fn f() { letrec f", "= x => x; f(1) }"),
    ("fn f() { letrec f = x => x and", "g = x => x; g(1) }"),
    ("fn f() { while true {}", "nobreak {} }"),
    ("fn f() { while true {} nobreak", "{} }"),
    ("fn f() { for i = 0; i < 2; i = i + 1 {}", "nobreak {} }"),
    ("fn f() { for x in xs {}", "where { invariant: true } }"),
    ("fn f() { for x in xs {} nobreak", "{} }"),
    (
      "fn f() { for i = 0; i < 2; i = i + 1 {} nobreak {}", "where { invariant: true } }",
    ),
    ("fn f(x: Int", "?) {}"),
    ("fn f(x: (Int)", "?) {}"),
    ("type F = ()", "-> Int"),
    ("type F = (Int)", "-> Int"),
    ("fn f() -> Int raise E +", "nocancel {}"),
    ("fn f(x: &", "T) {}"),
    ("fn f(x:", "Array[Int]) {}"),
    ("fn f(x:", "Int?) {}"),
    ("fn f(x:", "(Int)?) {}"),
    ("fn f() { match x { T", "::C => () } }"),
    ("fn f() { match x { T::", "C => () } }"),
    ("fn f() { match x { @p", " .T::C => () } }"),
    ("fn f() { match x { T", "::_ => () } }"),
    ("fn f() { match x { T::", ".. => () } }"),
    ("fn T::", "f(self: T) {}"),
    ("let r = {x", ": 1}"),
    ("using @p { type", "T }"),
    ("using @p { f", "as g }"),
    ("using @p { f as", "g }"),
    ("impl T for S with", "fn f(self: S) {}"),
    ("impl T for S with fn f(self: S)", "-> Int { 0 }"),
    ("struct S { fn new(x: Int)", "-> S }"),
    ("let r = &", "T::f"),
    ("using", "@p { f }"),
    ("fn f() { for i = 0;", "i < 2; i = i + 1 {} }"),
    ("fn f() { for i = 0; i < 2;", "i = i + 1 {} }"),
    ("fn f() { for x in xs;", "i = 0 { x } }"),
    ("let xs = [for i = 0;", "i < 2; i = i + 1 => i]"),
    ("fn f(x", ": Int) {}"),
    ("fn f(x~", ": Int) {}"),
    ("fn f(x", "?: Int) {}"),
    ("fn f(x?", ": Int) {}"),
    ("fn f(x~", "= 1) {}"),
    ("fn[T", ": Show] f(x : T) {}"),
    ("fn[T: Show +", "Eq] f(x : T) {}"),
    ("trait T { fn f(x", ": Int) }"),
    ("trait T { fn f(x", "?: Int) }"),
    ("trait T { fn f(x?", ": Int) }"),
    ("trait T { fn f(x:", "Int) }"),
    ("struct S { x", ": Int }"),
    ("struct S { x:", "Int }"),
    ("enum E { C(mut", "Int) }"),
    ("enum E { C(x~", ": Int) }"),
    ("fn f() { x as", "T }"),
    ("fn f() { match x { _", "as y => y } }"),
    ("fn f() { match x { _", "=> () } }"),
    ("fn f() { lexmatch x { re\"a\"", "as y => y } }"),
    ("fn f() { lexmatch x { re\"a\"", "=> () } }"),
    ("fn f() { match x { _ as", "y => y } }"),
    ("fn f() { lexmatch x { re\"a\" as", "y => y } }"),
    ("impl T for S with fn", "f(self : S) {}"),
    ("impl T for S with fn", "[A] f(self : S) {}"),
    ("enumview V { C } for S with", "view(self : S) { C }"),
    ("fn f() { for i", "= 0; i < 2; i = i + 1 {} }"),
    ("fn f() { lexmatch x { re\"a\" as y", "=> y } }"),
    ("fn f() { match x { _ as y", "=> y } }"),
    ("fn f() where", "{ proof: true } {}"),
    ("fn f() where { proof", ": true } {}"),
    ("fn f() { let", "x = 1; x }"),
  ]
}

///|
test "trivia boundaries agree across all parsers" {
  for (prefix, suffix) in trivia_boundaries() {
    for newline in ["\n", "\r\n"] {
      for
        gap in [
          " ",
          newline,
          " // one" + newline,
          " // one 🐇" + newline + " // two 🦀" + newline,
        ] {
        let source = prefix + gap + suffix
        assert_ast_equiv(source, label=source)
        assert_boundary_comments(source, prefix, gap)
      }
    }
  }
}

///|
fn boundary_comments(
  node : @node.CstNode,
  comments : Array[@node.CstNode],
) -> Unit {
  for (label, child) in node.children {
    if label == Some("trivia") {
      comments.push(child)
    } else {
      boundary_comments(child, comments)
    }
  }
}

///|
fn assert_boundary_comments(
  source : String,
  prefix : String,
  gap : String,
) -> Unit raise {
  let parsed = @untyped_cst.parse_structure(source)
  boundary_assert_source(parsed.root(), source)
  let comments = []
  boundary_comments(parsed.root(), comments)
  let expected = if gap.contains("// two") {
    [("// one 🐇", 8), ("// two 🦀", 8)]
  } else if gap.contains("// one") {
    [("// one", 6)]
  } else {
    []
  }
  if comments.length() != expected.length() {
    fail("lost or duplicated comments in " + source)
  }
  let newline = if gap.contains("\r\n") { "\r\n" } else { "\n" }
  let mut offset = prefix.length() + 1
  let mut cnum = offset
  let mut bol = 0
  for i, (text, width) in expected {
    let comment = comments[i]
    refactor_assert_source_range(
      comment,
      source,
      text,
      (offset, offset + text.length()),
      (i + 1, bol, cnum),
      (i + 1, bol, cnum + width),
    )
    @test.assert_eq(comment.kind is Syntax_Token(TK_COMMENT), true)
    offset += text.length() + newline.length() + 1
    cnum += width + newline.length() + 1
    bol = cnum - 1
  }
}

///|
test "trivia cannot hide separators or missing required syntax" {
  for newline in ["\n", "\r\n"] {
    for
      broken in [
        "fn f() { try g() catch {} noraise // keep" + newline + "{} }",
        "fn f() { g() catch {} noraise // keep" + newline + "{} }",
        "let f = fn(x) // keep" + newline + "{ x }",
        "let f = fn(x) -> Int // keep" + newline + "{ x }",
        "fn f() { g(); catch {} }",
        "fn f() { try g() catch {}; noraise {} }",
        "fn f() { if true {} else; {} }",
        "fn f() { guard true else; {} }",
        "let f = fn; (x) { x }",
        "fn f() { let; mut x = 1 }",
        "fn f() { let x: Int; = 1 }",
        "let xs = [for i; = 0 => i]",
        "fn f() { proof_assert ∀ x;: Int, true }",
        "fn f() { proof_let x; = 1 }",
        "fn f() { label~;: {} }",
        "fn f() { label~~: while true {} }",
        "fn f() { label~~: for ;; {} }",
        "let r = f(x; = 1)",
        "let r = f(x?; = 1)",
        "fn f() { letrec // keep" + newline + " = x => x }",
        "fn f() { letrec f = x => x and // keep" + newline + " = x => x }",
        "using // keep" + newline + "; @p { f }",
        "using @p { f as // keep" + newline + "}",
        "using @p { type T as // keep" + newline + "}",
        "fn f() { while true {} nobreak // keep" + newline + "; }",
        "fn f() { for x in xs {} nobreak // keep" + newline + "; }",
        "fn f() { for i = 0 {} nobreak // keep" + newline + "; }",
        "fn T:: // keep" + newline + "() {}",
        "type T = () // keep" + newline,
        "type T = & // keep" + newline + ";",
        "type T = (Int) -> Int // keep" + newline + "raise E",
        "type T = (Int) -> Int raise // keep" + newline + "E",
        "fn f() { while true {}; nobreak {} }",
        "fn f() { for x in xs {}; where { invariant: true } }",
        "fn f(x: // keep" + newline + "Int -> Int) {}",
      ] {
      let source = broken + newline + "fn later() {}"
      assert_all_parsers_reject(source)
      let parsed = @untyped_cst.parse_structure(source)
      boundary_assert_source(parsed.root(), source)
      guard api_find_node(parsed.root(), node => {
          node.kind.is_impl() && node.text(source) == "fn later() {}"[:]
        })
        is Some(_) else {
        fail("recovery lost the following declaration: " + source)
      }
    }
    assert_ast_equiv("fn f() { first() // keep" + newline + "second() }")
  }
}
