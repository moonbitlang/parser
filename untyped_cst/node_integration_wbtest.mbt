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
  let root = node(
    fn(loc, source_span, children) {
      let async_loc = CstNode::first_child_with_label_from(
        children, "async_loc",
      )
      let name = CstNode::children_with_label_from(children, "name")
      let token = CstNode::first_child_with_label_from(children, "token")
      let semantic_name = CstNode::first_child_with_label_from(
        children, "semantic_name",
      )
      @node.CstNode::expr_ident(
        loc~,
        source_span~,
        async_loc?,
        name~,
        token?,
        semantic_name?,
      )
    },
    mk_loc(start, token_end),
    (0, 3),
    [
      (
        Some("async_loc"),
        CstNode::syntax_keyword(
          loc=mk_loc(metadata_start, token_start),
          source_span=(1, 2),
        ),
      ),
      (
        Some("token"),
        CstNode::syntax_identifier(
          loc=mk_loc(token_start, token_end),
          source_span=(2, 3),
          payload=NodePayload::String("x"),
        ),
      ),
    ],
  )
  guard parser_first_significant_token_at_or_after(root, start) is Some(found) else {
    fail("expected significant token")
  }
  @test.assert_eq(found.kind is Syntax_Identifier, true)
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
    parse_structure(source).root(),
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
    parse_structure(source).root(),
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

///|
test "token location anchors keep kind and explicit ranges without payloads" {
  for
    source in [
      "async", "(", ":", "+", "name", "label~", ".field", "\"🐇\"", "\"a\\{x}\"",
      "#deprecated", "// comment", "", "\n",
    ] {
    let lex = @lexer.tokens_from_string(source, comment=true)
    let state = new_state(lex.tokens, source, [], false)
    while state.consume_any() is Some(token) {
      let loc = empty_loc(token.loc.end)
      let source_span = empty_span(token.source_span.1)
      let anchor = token_location_node(token, loc, source_span)
      @test.assert_eq(anchor.kind, token.kind)
      @test.assert_eq(anchor.loc, loc)
      @test.assert_eq(anchor.source_span, source_span)
      @test.assert_eq(anchor.children.length(), 0)
    }
  }
}

///|
test "recovery anchors keep token locations and zero width UTF-16 spans" {
  for tail in ["name", "\"🐇\"", "(", "async", ""] {
    let source = "// 🐇\n" + tail
    let lex = @lexer.tokens_from_string(source, comment=true)
    let state = new_state(lex.tokens, source, [], false)
    let (token, start, end) = state.peek_significant().unwrap()
    let anchor = state.current_significant_anchor()
    @test.assert_eq(anchor.kind, syntax_kind_from_token_kind(token.kind()))
    @test.assert_eq(anchor.loc, mk_loc(start, end))
    @test.assert_eq(anchor.source_span, (6, 6))
    @test.assert_eq(anchor.loc.start.cnum, 5)
    @test.assert_eq(anchor.children.length(), 0)
    @test.assert_eq(source[anchor.source_span.0:anchor.source_span.1], ""[:])
  }
}
