///|
test "token constructors preserve exact kinds and ordered repeated entries" {
  let loc = @node.mk_loc(test_position(2), test_position(3))
  let source_span = (3, 4)
  let marker = @node.CstNode::flag_async(value=true, loc~, source_span~)
  let name = @node.CstNode::name_identifier(value="x", loc~, source_span~)
  let payload1 = @node.CstNode::leaf(
    payload=String("first"),
    loc~,
    source_span~,
  )
  let payload2 = @node.CstNode::leaf(payload=Int(7), loc~, source_span~)
  let entries : Array[@node.CstTokenChild] = [
    Segment(name),
    Payload(String("first")),
    Flag(marker),
    NameValue(name),
    Operator(name),
    Constant(name),
    Accessor(name),
    Trailing(marker),
    Attribute(name),
    Comment(name),
    Segment(marker),
    Payload(Int(7)),
  ]
  let expected = [
    ("segment", name),
    ("payload", payload1),
    ("flag", marker),
    ("name_value", name),
    ("operator", name),
    ("constant", name),
    ("accessor", name),
    ("trailing", marker),
    ("attribute", name),
    ("comment", name),
    ("segment", marker),
    ("payload", payload2),
  ]
  for
    token_kind in [
      @node.TokenKind::TK_ASYNC,
      TK_FN,
      TK_AMPER,
      TK_QUESTION,
      TK_LPAREN,
      TK_RPAREN,
      TK_TRUE,
      TK_FALSE,
      TK_LIDENT,
      TK_UIDENT,
      TK_INTERP,
      TK_BYTES_INTERP,
      TK_MULTILINE_INTERP,
      TK_REGEX_INTERP,
      TK_ATTRIBUTE,
      TK_COMMENT,
      TK_NEWLINE,
      TK_EOF,
      TK_SEMI,
    ] {
    let token = @node.CstNode::syntax_token(
      token_kind~,
      loc~,
      source_span~,
      entries~,
    )
    @test.assert_eq(token.kind.token_kind(), Some(token_kind))
    @test.assert_eq(token.kind.is_syntax_token(), true)
    assert_constructor_entries(token, loc, source_span, expected)
    let error = @node.CstNode::error(loc~, source_span~, entries=[Item(token)])
    let recovered = error.children[0].1
    @test.assert_eq(
      recovered.kind is @node.NodeKind::Syntax_Unexpected(_),
      true,
    )
    @test.assert_eq(recovered.kind.token_kind(), Some(token_kind))
    assert_constructor_entries(recovered, loc, source_span, expected)
    let unexpected = @node.CstNode::syntax_unexpected(
      token_kind~,
      loc~,
      source_span~,
      entries~,
    )
    assert_constructor_tree(recovered, unexpected)
    let repeated = @node.CstNode::error(loc~, source_span~, entries=[
      Item(recovered),
    ])
    assert_constructor_tree(repeated.children[0].1, recovered)
  }
  @test.assert_eq(name.kind.token_kind(), None)
  @test.assert_eq(name.kind.is_syntax_token(), false)
}
