///|
pub struct CstNode {
  kind : NodeKind
  loc : Location
  source_span : (Int, Int)
  children : ReadOnlyArray[(String?, CstNode)]
} derive(@debug.Debug)

///|
pub fn mk_loc(start : Position, end : Position) -> Location {
  Location::{ start, end, }
}

///|
pub fn empty_loc(pos : Position) -> Location {
  Location::{ start: pos, end: pos, }
}

///|
pub fn empty_span(offset : Int) -> (Int, Int) {
  (offset, offset)
}

///|
fn node(
  kind : NodeKind,
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  { kind, loc, source_span, children: ReadOnlyArray::from_array(children), }
}

///|
fn leaf_node(
  payload : NodePayload,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  node(Leaf(payload), loc, source_span, [])
}

///|
fn push_payload_leaf(
  children : Array[(String?, CstNode)],
  payload : NodePayload,
  loc : Location,
  source_span : (Int, Int),
) -> Unit {
  children.push((Some("payload"), leaf_node(payload, loc, source_span)))
}

///|
fn node_with_payload(
  kind : NodeKind,
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
  payload : NodePayload,
) -> CstNode {
  push_payload_leaf(children, payload, loc, source_span)
  node(kind, loc, source_span, children)
}

///|
fn token_node(
  token : Token,
  start : Position,
  end : Position,
  source_span : (Int, Int),
) -> CstNode {
  token_node_with_children(token, start, end, source_span, [])
}

///|
fn token_node_with_children(
  token : Token,
  start : Position,
  end : Position,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  let loc = mk_loc(start, end)
  let kind = Syntax_Token(token.kind())
  match token_payload(token) {
    Some(payload) =>
      node_with_payload(kind, loc, source_span, children, payload)
    None => node(kind, loc, source_span, children)
  }
}

///|
fn scalar_node(
  kind : NodeKind,
  loc : Location,
  source_span : (Int, Int),
  payload : NodePayload,
) -> CstNode {
  node_with_payload(kind, loc, source_span, [], payload)
}

///|
fn marker_node(kind : NodeKind, pos : Position, offset : Int) -> CstNode {
  node(kind, empty_loc(pos), empty_span(offset), [])
}

///|
fn bool_marker_node(
  kind : NodeKind,
  pos : Position,
  offset : Int,
  value : Bool,
) -> CstNode {
  scalar_node(kind, empty_loc(pos), empty_span(offset), Bool(value))
}

///|
fn missing_node(kind : TokenKind, pos : Position, offset : Int) -> CstNode {
  let _ = kind
  node(Missing, empty_loc(pos), empty_span(offset), [])
}

///|
fn error_node(
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  let recovered_children : Array[(String?, CstNode)] = []
  for entry in children {
    let (label, child) = entry
    recovered_children.push(
      (
        label,
        if child.kind.token_kind() is Some(kind) {
          child.with_kind(Syntax_Unexpected(kind))
        } else {
          child
        },
      ),
    )
  }
  node(Error, loc, source_span, recovered_children)
}

///|
fn push_child(
  children : Array[(String?, CstNode)],
  label : String?,
  child : CstNode,
) -> Unit {
  children.push((label, child))
}

///|
fn push_children(
  children : Array[(String?, CstNode)],
  more : Array[(String?, CstNode)],
) -> Unit {
  for child in more {
    children.push(child)
  }
}

///|
#deprecated
#doc(hidden)
pub extend CstNode with Debug::{to_repr}
