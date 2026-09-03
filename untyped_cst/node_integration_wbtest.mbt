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
      @node.token_node(
        Token::LIDENT("x"),
        token_start,
        token_end,
        mk_span(2, 3),
      ),
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
