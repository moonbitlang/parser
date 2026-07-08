///|
test "node getters expose immutable shape" {
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0 }
  let end = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 3 }
  let child = token_node(Token::LIDENT("x"), start, end, mk_span(0, 1))
  let root = node(NodeKind::SourceFile, mk_loc(start, end), mk_span(0, 3), [
    (Some("item"), child),
  ])
  match root.kind {
    SourceFile => ()
    _ => fail("expected SourceFile")
  }
  @test.assert_eq(root.loc, mk_loc(start, end))
  @test.assert_eq(root.source_span, mk_span(0, 3))
  @test.assert_eq(root.children.length(), 1)
  match root.children.get(0) {
    Some((Some(label), node)) => {
      @test.assert_eq(label, "item")
      match node.kind {
        Token(LIDENT(name)) => @test.assert_eq(name, "x")
        _ => fail("expected LIDENT token")
      }
    }
    _ => fail("expected labeled child")
  }
}

///|
test "node test json is stable" {
  let pos = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0 }
  let root = node(NodeKind::SourceFile, empty_loc(pos), empty_span(0), [
    (Some("missing"), missing_node(TokenKind::TK_EOF, pos, 0)),
  ])
  json_inspect(root.to_test_json(), content={
    "kind": "SourceFile",
    "loc": { "start": 0, "end": 0 },
    "source_span": { "start": 0, "end": 0 },
    "children": [
      {
        "label": "missing",
        "node": {
          "kind": "Missing",
          "expected": "EOF",
          "loc": { "start": 0, "end": 0 },
          "source_span": { "start": 0, "end": 0 },
          "children": [],
        },
      },
    ],
  })
}

///|
test "node text returns source span" {
  let source = "let x = 1"
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0 }
  let end = Position::{
    fname: "test.mbt",
    lnum: 1,
    bol: 0,
    cnum: source.length(),
  }
  let root = node(
    NodeKind::SourceFile,
    mk_loc(start, end),
    mk_span(0, source.length()),
    [],
  )
  @test.assert_eq(root.text(source), source[:])
}

///|
test "token text returns token span" {
  let source = "let x = 1"
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 4 }
  let end = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 5 }
  let token = token_node(Token::LIDENT("x"), start, end, mk_span(4, 5))
  @test.assert_eq(token.text(source), "x"[:])
}

///|
test "missing text is empty" {
  let source = "let x = 1"
  let pos = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 4 }
  let missing = missing_node(TokenKind::TK_LIDENT, pos, 4)
  @test.assert_eq(missing.text(source), ""[:])
}
