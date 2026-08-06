///|
/// Scalar data stored by `Leaf` CST nodes.
///
/// Recursive syntax and non-scalar AST helper values are represented by
/// explicitly-kinded child nodes instead of payload variants.
pub enum NodePayload {
  String(String)
  Int(Int)
  Bool(Bool)
} derive(@debug.Debug, Eq)

///|
fn CstNode::first_child_with_label(self : CstNode, label : String) -> CstNode? {
  for i in 0..<self.children.length() {
    match self.children.get(i) {
      Some((Some(child_label), child)) if child_label == label =>
        return Some(child)
      _ => ()
    }
  }
  None
}

///|
fn CstNode::children_with_label(
  self : CstNode,
  label : String,
) -> Array[CstNode] {
  let result : Array[CstNode] = []
  for i in 0..<self.children.length() {
    match self.children.get(i) {
      Some((Some(child_label), child)) if child_label == label =>
        result.push(child)
      _ => ()
    }
  }
  result
}

///|
fn token_string_payload(token : Token) -> String? {
  match token {
    CHAR(value)
    | INT(value)
    | BYTE(value)
    | BYTES(value)
    | FLOAT(value)
    | DOUBLE(value)
    | STRING(value)
    | MULTILINE_STRING(value)
    | REGEX_LITERAL(value)
    | LIDENT(value)
    | UIDENT(value)
    | PACKAGE_NAME(value)
    | POST_LABEL(value)
    | POST_LABEL_TRANSPARENT(value)
    | INFIX1(value)
    | INFIX2(value)
    | INFIX3(value)
    | INFIX4(value)
    | AUGMENTED_ASSIGNMENT(value)
    | DOT_LIDENT(value)
    | DOT_UIDENT(value) => Some(value)
    _ => None
  }
}

///|
fn token_payload(token : Token) -> NodePayload? {
  if token_string_payload(token) is Some(value) {
    return Some(String(value))
  }
  match token {
    DOT_INT(index) => Some(Int(index))
    TRUE => Some(Bool(true))
    FALSE => Some(Bool(false))
    SEMI(explicit) => Some(Bool(explicit))
    _ => None
  }
}

///|
fn CstNode::leaf_payload(self : CstNode) -> NodePayload? {
  match self.first_child_with_label("payload") {
    Some(child) =>
      match child.kind {
        Leaf(payload) => Some(payload)
        _ => None
      }
    None => None
  }
}

///|
fn CstNode::string_payload(self : CstNode) -> String? {
  match self.leaf_payload() {
    Some(String(value)) => Some(value)
    _ => None
  }
}

///|
fn CstNode::int_payload(self : CstNode) -> Int? {
  match self.leaf_payload() {
    Some(Int(value)) => Some(value)
    _ => None
  }
}

///|
fn CstNode::bool_payload(self : CstNode) -> Bool? {
  match self.leaf_payload() {
    Some(Bool(value)) => Some(value)
    _ => None
  }
}

///|
