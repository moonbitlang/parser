///|
test "significant token search skips comments and newlines by exact kind" {
  let start = Position::{ fname: "test.mbt", lnum: 1, bol: 0, cnum: 0, }
  let token_start = Position::{ ..start, cnum: 2, }
  let token_end = Position::{ ..start, cnum: 3, }
  let root = CstNode::expression(
    loc=mk_loc(start, token_end),
    source_span=(0, 3),
    entries=[
      Trivia(
        CstNode::syntax_token(
          token_kind=TK_COMMENT,
          loc=mk_loc(start, token_start),
          source_span=(0, 2),
          entries=[],
        ),
      ),
      Trivia(
        CstNode::syntax_unexpected(
          token_kind=TK_NEWLINE,
          loc=empty_loc(token_start),
          source_span=(2, 2),
          entries=[],
        ),
      ),
      Expr(
        CstNode::syntax_token(
          token_kind=TK_LIDENT,
          loc=mk_loc(token_start, token_end),
          source_span=(2, 3),
          entries=[Payload(String("x"))],
        ),
      ),
    ],
  )
  guard lower_first_significant_token_at_or_after(root, start) is Some(found) else {
    fail("expected significant token")
  }
  @test.assert_eq(found.kind.token_kind(), Some(TK_LIDENT))
  @test.assert_eq(found.source_span, (2, 3))
}

///|
fn collect_kind(
  root : CstNode,
  pred : (NodeKind) -> Bool,
  result : Array[CstNode],
) -> Unit {
  if pred(root.kind) {
    result.push(root)
  }
  for entry in root.children {
    let (_, child) = entry
    collect_kind(child, pred, result)
  }
}

///|
test "brace record pun is classified while parsing" {
  let source =
    #|
    #|let x = 2
    #|let record = { x; }
  let records : Array[CstNode] = []
  collect_kind(
    parse_structure(source).root,
    fn(kind) { kind is Expr_Record },
    records,
  )
  guard records.get(0) is Some(record) else { fail("record not found") }
  guard record.first_child_with_label("body") is Some(body) else {
    fail("record body not found")
  }
  @test.assert_eq(records.length(), 1)
  @test.assert_eq(
    body.has_child_kind(fn(kind) { kind is TrailingMark_Semi }),
    true,
  )
  @test.assert_eq(
    body.has_child_kind(fn(kind) { kind is Field_Definition }),
    true,
  )
}

///|
test "extend soft keyword keeps its accessor label" {
  let source = "fn f(x : X) -> Unit { x..extend() }"
  let dot_applies : Array[CstNode] = []
  collect_kind(
    parse_structure(source).root,
    fn(kind) { kind is Expr_DotApply },
    dot_applies,
  )
  guard dot_applies.get(0) is Some(dot_apply) else {
    fail("dot apply not found")
  }
  guard dot_apply.first_child_with_label("field") is Some(field) else {
    fail("dot apply field not found")
  }
  @test.assert_eq(dot_applies.length(), 1)
  @test.assert_eq(field.kind is Accessor_Label, true)
  @test.assert_eq(field.string_payload(), Some("extend"))
}
