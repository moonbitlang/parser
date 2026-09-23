///|
pub(all) enum TopItemKind {
  Invalid
  Import
  Extend
  Using
  Function
  Let
  Const
  Type
  Struct
  Enum
  EnumView
  Trait
  Impl
  Test
}

///|
fn top_item_children_have_label(
  children : Array[(String?, CstNode)],
  expected : String,
) -> Bool {
  for i in 0..<children.length() {
    match children.get(i) {
      Some((Some(label), _)) if label == expected => return true
      _ => ()
    }
  }
  false
}

///|
fn top_item_child_with_label_has_kind(
  children : Array[(String?, CstNode)],
  expected_label : String,
  pred : (NodeKind) -> Bool,
) -> Bool {
  for i in 0..<children.length() {
    match children.get(i) {
      Some((Some(label), child)) if label == expected_label =>
        return pred(child.kind)
      _ => ()
    }
  }
  false
}

///|
fn top_item_children_have_child_or_grandchild_kind(
  children : Array[(String?, CstNode)],
  pred : (NodeKind) -> Bool,
) -> Bool {
  for entry in children {
    let (_, child) = entry
    if pred(child.kind) || child.has_child_kind(pred) {
      return true
    }
  }
  false
}
