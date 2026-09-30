///|
/// Ordered token children. Repeated entries are retained without regrouping.
pub(all) enum CstTokenChild {
  Payload(NodePayload)
  Flag(CstNode)
  NameValue(CstNode)
  Operator(CstNode)
  Constant(CstNode)
  Accessor(CstNode)
  Trailing(CstNode)
  Attribute(CstNode)
  Comment(CstNode)
  Segment(CstNode)
}

///|
fn token_children(
  entries : ArrayView[CstTokenChild],
  loc : Location,
  source_span : (Int, Int),
) -> Array[(String?, CstNode)] {
  let children : Array[(String?, CstNode)] = []
  for entry in entries {
    match entry {
      Payload(value) => push_payload_leaf(children, value, loc, source_span)
      Flag(child) => children.push((Some("flag"), child))
      NameValue(child) => children.push((Some("name_value"), child))
      Operator(child) => children.push((Some("operator"), child))
      Constant(child) => children.push((Some("constant"), child))
      Accessor(child) => children.push((Some("accessor"), child))
      Trailing(child) => children.push((Some("trailing"), child))
      Attribute(child) => children.push((Some("attribute"), child))
      Comment(child) => children.push((Some("comment"), child))
      Segment(child) => children.push((Some("segment"), child))
    }
  }
  children
}

///|
pub fn CstNode::syntax_token(
  token_kind~ : TokenKind,
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstTokenChild],
) -> CstNode {
  node(
    Syntax_Token(token_kind),
    loc,
    source_span,
    token_children(entries, loc, source_span),
  )
}

///|
pub fn CstNode::syntax_unexpected(
  token_kind~ : TokenKind,
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstTokenChild],
) -> CstNode {
  node(
    Syntax_Unexpected(token_kind),
    loc,
    source_span,
    token_children(entries, loc, source_span),
  )
}
