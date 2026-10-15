///|
// Each pair isolates one grammar boundary. Gaps never contain semicolons.
fn trivia_boundaries() -> Array[(String, String)] {
  [
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
