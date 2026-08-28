///|
test "node getters expose immutable shape" {
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let end = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 3, }
  let child = token_node(Token::LIDENT("x"), start, end, mk_span(0, 1))
  let root = node(NodeKind::Impls, mk_loc(start, end), mk_span(0, 3), [
    (Some("item"), child),
  ])
  match root.kind {
    Impls => ()
    _ => fail("expected Impls")
  }
  @test.assert_eq(root.loc, mk_loc(start, end))
  @test.assert_eq(root.source_span, mk_span(0, 3))
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
            Leaf(NodePayload::String(name)) => @test.assert_eq(name, "x")
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
    TokenKind::TK_INT,
    TokenKind::TK_BYTE,
    TokenKind::TK_BYTES,
    TokenKind::TK_FLOAT,
    TokenKind::TK_DOUBLE,
    TokenKind::TK_STRING,
    TokenKind::TK_MULTILINE_STRING,
    TokenKind::TK_MULTILINE_INTERP,
    TokenKind::TK_INTERP,
    TokenKind::TK_BYTES_INTERP,
    TokenKind::TK_REGEX_LITERAL,
    TokenKind::TK_REGEX_INTERP,
    TokenKind::TK_ATTRIBUTE,
    TokenKind::TK_LIDENT,
    TokenKind::TK_UIDENT,
    TokenKind::TK_POST_LABEL,
    TokenKind::TK_COMMENT,
    TokenKind::TK_INFIX1,
    TokenKind::TK_INFIX2,
    TokenKind::TK_INFIX3,
    TokenKind::TK_INFIX4,
    TokenKind::TK_AUGMENTED_ASSIGNMENT,
    TokenKind::TK_PLUS_EQUAL,
    TokenKind::TK_EOF,
    TokenKind::TK_FALSE,
    TokenKind::TK_TRUE,
    TokenKind::TK_PUB,
    TokenKind::TK_PRIV,
    TokenKind::TK_READONLY,
    TokenKind::TK_IMPORT,
    TokenKind::TK_EXTERN,
    TokenKind::TK_BREAK,
    TokenKind::TK_CONTINUE,
    TokenKind::TK_STRUCT,
    TokenKind::TK_ENUM,
    TokenKind::TK_TRAIT,
    TokenKind::TK_DERIVE,
    TokenKind::TK_IMPL,
    TokenKind::TK_WITH,
    TokenKind::TK_RAISE,
    TokenKind::TK_THROW,
    TokenKind::TK_TRY,
    TokenKind::TK_CATCH,
    TokenKind::TK_ASYNC,
    TokenKind::TK_TYPEALIAS,
    TokenKind::TK_TRAITALIAS,
    TokenKind::TK_FNALIAS,
    TokenKind::TK_EQUAL,
    TokenKind::TK_EQ_TILDE,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    TokenKind::TK_COMMA,
    TokenKind::TK_MINUS,
    TokenKind::TK_QUESTION,
    TokenKind::TK_EXCLAMATION,
    TokenKind::TK_DOT_LIDENT,
    TokenKind::TK_DOT_UIDENT,
    TokenKind::TK_DOT_INT,
    TokenKind::TK_DOT_LPAREN,
    TokenKind::TK_COLONCOLON,
    TokenKind::TK_COLON,
    TokenKind::TK_SEMI,
    TokenKind::TK_LBRACKET,
    TokenKind::TK_LBRACKET_BAR,
    TokenKind::TK_PLUS,
    TokenKind::TK_RBRACKET,
    TokenKind::TK_BAR_RBRACKET,
    TokenKind::TK_UNDERSCORE,
    TokenKind::TK_BAR,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    TokenKind::TK_AMPERAMPER,
    TokenKind::TK_AMPER,
    TokenKind::TK_CARET,
    TokenKind::TK_BARBAR,
    TokenKind::TK_PACKAGE_NAME,
    TokenKind::TK_AS,
    TokenKind::TK_PIPE,
    TokenKind::TK_PIPE_LEFT,
    TokenKind::TK_LT_PLUS,
    TokenKind::TK_LT_QUESTION,
    TokenKind::TK_ELSE,
    TokenKind::TK_FN,
    TokenKind::TK_IF,
    TokenKind::TK_LET,
    TokenKind::TK_PROOF_ASSERT,
    TokenKind::TK_PROOF_LET,
    TokenKind::TK_CONST,
    TokenKind::TK_MATCH,
    TokenKind::TK_USING,
    TokenKind::TK_MUTABLE,
    TokenKind::TK_TYPE,
    TokenKind::TK_FAT_ARROW,
    TokenKind::TK_THIN_ARROW,
    TokenKind::TK_WHILE,
    TokenKind::TK_RETURN,
    TokenKind::TK_DOTDOT,
    TokenKind::TK_RANGE_INCLUSIVE,
    TokenKind::TK_RANGE_LT_INCLUSIVE,
    TokenKind::TK_RANGE_EXCLUSIVE,
    TokenKind::TK_RANGE_INCLUSIVE_REV,
    TokenKind::TK_RANGE_EXCLUSIVE_REV,
    TokenKind::TK_ELLIPSIS,
    TokenKind::TK_TEST,
    TokenKind::TK_LOOP,
    TokenKind::TK_GUARD,
    TokenKind::TK_DEFER,
    TokenKind::TK_FOR,
    TokenKind::TK_IN,
    TokenKind::TK_IS,
    TokenKind::TK_SUBERROR,
    TokenKind::TK_AND,
    TokenKind::TK_FORALL,
    TokenKind::TK_EXISTS,
    TokenKind::TK_IMPLIES,
    TokenKind::TK_LETREC,
    TokenKind::TK_ENUMVIEW,
    TokenKind::TK_EXTENUM,
    TokenKind::TK_DECLARE,
    TokenKind::TK_NORAISE,
    TokenKind::TK_NOBREAK,
    TokenKind::TK_WHERE,
    TokenKind::TK_TRY_QUESTION,
    TokenKind::TK_TRY_EXCLAMATION,
    TokenKind::TK_LEXMATCH,
    TokenKind::TK_LEXMATCH_QUESTION,
    TokenKind::TK_LEXSCAN,
    TokenKind::TK_EXTEND,
    TokenKind::TK_PACKAGE,
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
  @test.assert_eq(
    syntax_kind_from_token_kind(TokenKind::TK_LIDENT),
    Syntax_Identifier,
  )
  @test.assert_eq(
    syntax_kind_from_token_kind(TokenKind::TK_POST_LABEL),
    Syntax_Label,
  )
  @test.assert_eq(
    syntax_kind_from_token_kind(TokenKind::TK_DOT_INT),
    Syntax_Accessor,
  )
  @test.assert_eq(
    syntax_kind_from_token_kind(TokenKind::TK_READONLY),
    Syntax_Operator,
  )
  @test.assert_eq(
    syntax_kind_from_token_kind(TokenKind::TK_EXTEND),
    Syntax_Operator,
  )
  @test.assert_eq(
    syntax_kind_from_token_kind(TokenKind::TK_PACKAGE),
    Syntax_Keyword,
  )
}

///|
test "recovered newline token kind falls back to unexpected syntax" {
  @test.assert_eq(
    syntax_kind_from_token_kind(TokenKind::TK_NEWLINE),
    Syntax_Unexpected,
  )
}

///|
test "node test json is stable" {
  let pos = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let root = node(NodeKind::Impls, empty_loc(pos), empty_span(0), [
    (Some("missing"), missing_node(TokenKind::TK_EOF, pos, 0)),
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
  let leaf = leaf_node(NodePayload::String("x"), empty_loc(pos), empty_span(0))
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
  let root = node(NodeKind::Impl_Function, empty_loc(pos), empty_span(0), [])
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
  let root = node(
    NodeKind::Impls,
    mk_loc(start, end),
    mk_span(0, source.length()),
    [],
  )
  @test.assert_eq(root.text(source), source[:])
}

///|
test "token text returns token span" {
  let source = "let x = 1"
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 4, }
  let end = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 5, }
  let token = token_node(Token::LIDENT("x"), start, end, mk_span(4, 5))
  @test.assert_eq(token.text(source), "x"[:])
}

///|
test "missing text is empty" {
  let source = "let x = 1"
  let pos = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 4, }
  let missing = missing_node(TokenKind::TK_LIDENT, pos, 4)
  @test.assert_eq(missing.text(source), ""[:])
}

///|
test "error recovery preserves token-shaped location metadata" {
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let end = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 1, }
  let loc = mk_loc(start, end)
  let span = mk_span(0, 1)
  let recovered = error_node(loc, span, [
    (Some("question_loc"), token_location_node(Syntax_Operator, loc, span)),
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

///|
test "significant token search skips token-shaped location metadata" {
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let metadata_start = Position::{
    fname: "test.mbt",
    lnum: 1,
    bol: 0,
    cnum: 1,
  }
  let token_start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 2, }
  let token_end = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 3, }
  let root = node(Expr_Ident, mk_loc(start, token_end), mk_span(0, 3), [
    (
      Some("async_loc"),
      token_location_node(
        Syntax_Keyword,
        mk_loc(metadata_start, token_start),
        mk_span(1, 2),
      ),
    ),
    (
      Some("token"),
      token_node(Token::LIDENT("x"), token_start, token_end, mk_span(2, 3)),
    ),
  ])
  guard parser_first_significant_token_at_or_after(root, start) is Some(found) else {
    fail("expected significant token")
  }
  @test.assert_eq(found.kind, Syntax_Identifier)
  @test.assert_eq(found.source_span, mk_span(2, 3))
}

///|
fn collect_kind(
  root : CstNode,
  kind : NodeKind,
  result : Array[CstNode],
) -> Unit {
  if root.kind == kind {
    result.push(root)
  }
  for entry in root.children {
    let (_, child) = entry
    collect_kind(child, kind, result)
  }
}

///|
test "brace record pun is classified while parsing" {
  let source =
    #|
    #|let x = 2
    #|let record = { x; }
  let records : Array[CstNode] = []
  collect_kind(parse_structure(source).root(), Expr_Record, records)
  guard records.get(0) is Some(record) else { fail("record not found") }
  guard record.first_child_with_label("body") is Some(body) else {
    fail("record body not found")
  }
  @test.assert_eq(records.length(), 1)
  @test.assert_eq(body.has_child_kind(TrailingMark_Semi), true)
  @test.assert_eq(body.has_child_kind(Field_Definition), true)
}

///|
test "extend soft keyword keeps its accessor label" {
  let source = "fn f(x : X) -> Unit { x..extend() }"
  let dot_applies : Array[CstNode] = []
  collect_kind(parse_structure(source).root(), Expr_DotApply, dot_applies)
  guard dot_applies.get(0) is Some(dot_apply) else {
    fail("dot apply not found")
  }
  guard dot_apply.first_child_with_label("field") is Some(field) else {
    fail("dot apply field not found")
  }
  @test.assert_eq(dot_applies.length(), 1)
  @test.assert_eq(field.kind, Accessor_Label)
  @test.assert_eq(field.string_payload(), Some("extend"))
}
