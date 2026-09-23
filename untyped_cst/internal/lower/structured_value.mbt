///|
fn lower_constant_from_node(node : CstNode) -> @syntax.Constant? {
  let direct = if node.kind.is_constant() {
    Some(node)
  } else {
    node.first_child_with_label("semantic_constant")
  }
  guard direct is Some(constant) else { return None }
  match constant.kind {
    Constant_Bool => Some(Bool(constant.bool_payload().unwrap_or(false)))
    Constant_Byte => Some(Byte(constant.string_payload().unwrap_or("")))
    Constant_Bytes => Some(Bytes(constant.string_payload().unwrap_or("")))
    Constant_Char => Some(Char(constant.string_payload().unwrap_or("")))
    Constant_Int => Some(Int(constant.string_payload().unwrap_or("")))
    Constant_Int64 => Some(Int64(constant.string_payload().unwrap_or("")))
    Constant_UInt => Some(UInt(constant.string_payload().unwrap_or("")))
    Constant_UInt64 => Some(UInt64(constant.string_payload().unwrap_or("")))
    Constant_Float => Some(Float(constant.string_payload().unwrap_or("")))
    Constant_Double => Some(Double(constant.string_payload().unwrap_or("")))
    Constant_String => Some(String(constant.string_payload().unwrap_or("")))
    Constant_Regex => Some(Regex(constant.string_payload().unwrap_or("")))
    Constant_BigInt => Some(BigInt(constant.string_payload().unwrap_or("")))
    _ => None
  }
}

///|
fn lower_attribute_node(node : CstNode) -> @attribute.Attribute? {
  guard node.first_child_with_label("attribute") is Some(attribute) else {
    return None
  }
  let name = attribute
    .first_child_with_label("name")
    .map(child => child.string_payload())
    .bind(value => value)
    .unwrap_or("")
  let qualifier = attribute
    .first_child_with_label("qualifier")
    .map(child => child.string_payload())
    .bind(value => value)
  let raw_payload = attribute
    .first_child_with_label("raw_payload")
    .map(child => child.string_payload())
    .bind(value => value)
    .unwrap_or("")
  let content = match attribute.kind {
    Attribute_Qualified => (qualifier.unwrap_or(""), Some(name), raw_payload)
    Attribute_Simple => (name, None, raw_payload)
    _ => return None
  }
  Some(@attribute.Attribute::new(loc=node.loc, content))
}

///|
fn lower_interp_segment_value(node : CstNode) -> @syntax.InterpElem? {
  match node.kind {
    InterpSegment_Literal =>
      Some(Literal(repr=node.string_payload().unwrap_or(""), loc=node.loc))
    InterpSegment_Source =>
      Some(
        Source({ source: node.string_payload().unwrap_or(""), loc: node.loc, }),
      )
    _ => None
  }
}

///|
fn collect_interp_segments(node : CstNode) -> Array[CstNode] {
  let segments : Array[CstNode] = []
  for child in node.children_with_label("segment") {
    if child.kind.is_interp_segment() {
      segments.push(child)
    }
  }
  for token in node.children_with_label("token") {
    for segment in token.children_with_label("segment") {
      if segment.kind.is_interp_segment() {
        segments.push(segment)
      }
    }
  }
  segments
}
