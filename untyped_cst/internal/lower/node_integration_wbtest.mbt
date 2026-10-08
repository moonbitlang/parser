///|
test "last significant token search skips trailing trivia by exact kind" {
  let root = CstNode::expression(loc=detached_loc(2, 31), source_span=(3, 32), entries=[
    Expr(
      CstNode::syntax_unexpected(
        token_kind=TK_LIDENT,
        loc=detached_loc(2, 3),
        source_span=(3, 4),
        entries=[Payload(String("x"))],
      ),
    ),
    Trivia(detached_token(TK_COMMENT, 4, 30)),
    Trivia(
      CstNode::syntax_unexpected(
        token_kind=TK_NEWLINE,
        loc=detached_loc(30, 31),
        source_span=(31, 32),
        entries=[],
      ),
    ),
    Trivia(detached_token(TK_EOF, 31, 31)),
  ])
  @test.assert_eq(
    lower_last_significant_token_end(root),
    Some(detached_loc(2, 3).end),
  )
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
