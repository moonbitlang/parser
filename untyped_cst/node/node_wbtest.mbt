///|
test "node getters expose immutable shape" {
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let end = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 3, }
  let child = token_node(LIDENT("x"), start, end, (0, 1))
  let root = node(Impls, mk_loc(start, end), (0, 3), [
    (Some("item"), child),
  ])
  match root.kind {
    Impls => ()
    _ => fail("expected Impls")
  }
  @test.assert_eq(root.loc, mk_loc(start, end))
  @test.assert_eq(root.source_span, (0, 3))
  @test.assert_eq(root.children.length(), 1)
  match root.children.get(0) {
    Some((Some(label), node)) => {
      @test.assert_eq(label, "item")
      match node.kind {
        Syntax_Identifier => {
          guard node.children.get(node.children.length() - 1)
            is Some((Some("payload"), leaf)) else {
            fail("expected trailing payload Leaf")
          }
          match leaf.kind {
            Leaf(String(name)) => @test.assert_eq(name, "x")
            _ => fail("expected identifier Leaf")
          }
          @test.assert_eq(leaf.loc, node.loc)
          @test.assert_eq(leaf.source_span, node.source_span)
          @test.assert_eq(leaf.children.length(), 0)
        }
        _ => fail("expected LIDENT token")
      }
    }
    _ => fail("expected labeled child")
  }
}

///|
test "all non-newline lexer token kinds map to dedicated syntax kinds" {
  let token_kinds = [
    TokenKind::TK_CHAR,
    TK_INT,
    TK_BYTE,
    TK_BYTES,
    TK_FLOAT,
    TK_DOUBLE,
    TK_STRING,
    TK_MULTILINE_STRING,
    TK_MULTILINE_INTERP,
    TK_INTERP,
    TK_BYTES_INTERP,
    TK_REGEX_LITERAL,
    TK_REGEX_INTERP,
    TK_ATTRIBUTE,
    TK_LIDENT,
    TK_UIDENT,
    TK_POST_LABEL,
    TK_COMMENT,
    TK_INFIX1,
    TK_INFIX2,
    TK_INFIX3,
    TK_INFIX4,
    TK_AUGMENTED_ASSIGNMENT,
    TK_PLUS_EQUAL,
    TK_EOF,
    TK_FALSE,
    TK_TRUE,
    TK_PUB,
    TK_PRIV,
    TK_READONLY,
    TK_IMPORT,
    TK_EXTERN,
    TK_BREAK,
    TK_CONTINUE,
    TK_STRUCT,
    TK_ENUM,
    TK_TRAIT,
    TK_DERIVE,
    TK_IMPL,
    TK_WITH,
    TK_RAISE,
    TK_THROW,
    TK_TRY,
    TK_CATCH,
    TK_ASYNC,
    TK_TYPEALIAS,
    TK_TRAITALIAS,
    TK_FNALIAS,
    TK_EQUAL,
    TK_EQ_TILDE,
    TK_LPAREN,
    TK_RPAREN,
    TK_COMMA,
    TK_MINUS,
    TK_QUESTION,
    TK_EXCLAMATION,
    TK_DOT_LIDENT,
    TK_DOT_UIDENT,
    TK_DOT_INT,
    TK_DOT_LPAREN,
    TK_COLONCOLON,
    TK_COLON,
    TK_SEMI,
    TK_LBRACKET,
    TK_LBRACKET_BAR,
    TK_PLUS,
    TK_RBRACKET,
    TK_BAR_RBRACKET,
    TK_UNDERSCORE,
    TK_BAR,
    TK_LBRACE,
    TK_RBRACE,
    TK_AMPERAMPER,
    TK_AMPER,
    TK_CARET,
    TK_BARBAR,
    TK_PACKAGE_NAME,
    TK_AS,
    TK_PIPE,
    TK_PIPE_LEFT,
    TK_LT_PLUS,
    TK_LT_QUESTION,
    TK_ELSE,
    TK_FN,
    TK_IF,
    TK_LET,
    TK_PROOF_ASSERT,
    TK_PROOF_LET,
    TK_CONST,
    TK_MATCH,
    TK_USING,
    TK_MUTABLE,
    TK_TYPE,
    TK_FAT_ARROW,
    TK_THIN_ARROW,
    TK_WHILE,
    TK_RETURN,
    TK_DOTDOT,
    TK_RANGE_INCLUSIVE,
    TK_RANGE_LT_INCLUSIVE,
    TK_RANGE_EXCLUSIVE,
    TK_RANGE_INCLUSIVE_REV,
    TK_RANGE_EXCLUSIVE_REV,
    TK_ELLIPSIS,
    TK_TEST,
    TK_LOOP,
    TK_GUARD,
    TK_DEFER,
    TK_FOR,
    TK_IN,
    TK_IS,
    TK_SUBERROR,
    TK_AND,
    TK_FORALL,
    TK_EXISTS,
    TK_IMPLIES,
    TK_LETREC,
    TK_ENUMVIEW,
    TK_EXTENUM,
    TK_DECLARE,
    TK_NORAISE,
    TK_NOBREAK,
    TK_WHERE,
    TK_TRY_QUESTION,
    TK_TRY_EXCLAMATION,
    TK_LEXMATCH,
    TK_LEXMATCH_QUESTION,
    TK_LEXSCAN,
    TK_EXTEND,
    TK_PACKAGE,
  ]
  @test.assert_eq(token_kinds.length(), 130)
  for i, token_kind in token_kinds {
    let syntax_kind = syntax_kind_from_token_kind(token_kind)
    @test.assert_eq(syntax_kind.is_syntax_token(), true)
    @test.assert_eq(syntax_kind == Syntax_Unexpected, false)
    for j in 0..<i {
      @test.assert_eq(token_kind == token_kinds[j], false)
    }
  }
  @test.assert_eq(syntax_kind_from_token_kind(TK_LIDENT), Syntax_Identifier)
  @test.assert_eq(syntax_kind_from_token_kind(TK_POST_LABEL), Syntax_Label)
  @test.assert_eq(syntax_kind_from_token_kind(TK_DOT_INT), Syntax_Accessor)
  @test.assert_eq(syntax_kind_from_token_kind(TK_READONLY), Syntax_Operator)
  @test.assert_eq(syntax_kind_from_token_kind(TK_EXTEND), Syntax_Operator)
  @test.assert_eq(syntax_kind_from_token_kind(TK_PACKAGE), Syntax_Keyword)
}

///|
test "recovered newline token kind falls back to unexpected syntax" {
  @test.assert_eq(syntax_kind_from_token_kind(TK_NEWLINE), Syntax_Unexpected)
}

///|
test "node test json is stable" {
  let pos = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let root = node(Impls, empty_loc(pos), empty_span(0), [
    (Some("missing"), missing_node(TK_EOF, pos, 0)),
  ])
  json_inspect(root.to_test_json(), content={
    "kind": "Impls",
    "loc": { "start": 0, "end": 0 },
    "source_span": { "start": 0, "end": 0 },
    "children": [
      {
        "label": "missing",
        "node": {
          "kind": "Missing",
          "loc": { "start": 0, "end": 0 },
          "source_span": { "start": 0, "end": 0 },
          "children": [],
        },
      },
    ],
  })
}

///|
test "leaf node test json exposes its payload" {
  let pos = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let leaf = leaf_node(String("x"), empty_loc(pos), empty_span(0))
  json_inspect(leaf.to_test_json(), content={
    "kind": "Leaf",
    "payload": "String(\"x\")",
    "loc": { "start": 0, "end": 0 },
    "source_span": { "start": 0, "end": 0 },
    "children": [],
  })
}

///|
test "implementation node test json uses concrete kind" {
  let pos = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let root = node(Impl_Function, empty_loc(pos), empty_span(0), [])
  json_inspect(root.to_test_json(), content={
    "kind": "Impl_Function",
    "loc": { "start": 0, "end": 0 },
    "source_span": { "start": 0, "end": 0 },
    "children": [],
  })
}

///|
test "node text returns source span" {
  let source = "let x = 1"
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let end = Position::{
    fname: "test.mbt",
    lnum: 1,
    bol: 0,
    cnum: source.length(),
  }
  let root = node(Impls, mk_loc(start, end), (0, source.length()), [])
  @test.assert_eq(root.text(source), source[:])
}

///|
test "token text returns token span" {
  let source = "let x = 1"
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 4, }
  let end = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 5, }
  let token = token_node(LIDENT("x"), start, end, (4, 5))
  @test.assert_eq(token.text(source), "x"[:])
}

///|
test "missing text is empty" {
  let source = "let x = 1"
  let pos = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 4, }
  let missing = missing_node(TK_LIDENT, pos, 4)
  @test.assert_eq(missing.text(source), ""[:])
}

///|
test "error recovery preserves token-shaped location metadata" {
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let end = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 1, }
  let loc = mk_loc(start, end)
  let span = (0, 1)
  let recovered = error_node(loc, span, [
    (Some("question_loc"), node(Syntax_Operator, loc, span, [])),
    (Some("item"), token_node(Token::QUESTION, start, end, span)),
  ])
  guard recovered.first_child_with_label("question_loc") is Some(metadata) else {
    fail("expected location metadata")
  }
  guard recovered.first_child_with_label("item") is Some(raw_token) else {
    fail("expected recovered raw token")
  }
  @test.assert_eq(metadata.kind, Syntax_Operator)
  @test.assert_eq(raw_token.kind, Syntax_Unexpected)
}
