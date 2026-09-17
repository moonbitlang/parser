///|
pub fn CstNode::first_child_of_kind(
  self : CstNode,
  pred : (NodeKind) -> Bool,
) -> CstNode? {
  for i in 0..<self.children.length() {
    match self.children.get(i) {
      Some((_, child)) if pred(child.kind) => return Some(child)
      _ => ()
    }
  }
  None
}

///|
pub fn CstNode::children_of_kind(
  self : CstNode,
  pred : (NodeKind) -> Bool,
) -> Array[CstNode] {
  let result : Array[CstNode] = []
  for i in 0..<self.children.length() {
    match self.children.get(i) {
      Some((_, child)) if pred(child.kind) => result.push(child)
      _ => ()
    }
  }
  result
}

///|
pub fn CstNode::has_child_label(self : CstNode, label : String) -> Bool {
  self.first_child_with_label(label) is Some(_)
}

///|
pub fn CstNode::has_child_kind(
  self : CstNode,
  pred : (NodeKind) -> Bool,
) -> Bool {
  self.first_child_of_kind(pred) is Some(_)
}

///|
pub fn CstNode::has_child_or_grandchild_kind(
  self : CstNode,
  pred : (NodeKind) -> Bool,
) -> Bool {
  if self.has_child_kind(pred) {
    return true
  }
  for entry in self.children {
    let (_, child) = entry
    if child.has_child_kind(pred) {
      return true
    }
  }
  false
}

///|
pub fn CstNode::bool_marker_value(
  self : CstNode,
  pred : (NodeKind) -> Bool,
) -> Bool? {
  self
  .first_child_of_kind(pred)
  .map(child => child.bool_payload())
  .bind(value => value)
}

///|
fn CstNode::with_kind(self : CstNode, kind : NodeKind) -> CstNode {
  CstNode::{
    kind,
    loc: self.loc,
    source_span: self.source_span,
    children: self.children,
  }
}
