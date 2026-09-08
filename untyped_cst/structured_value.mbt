///|
fn CstNode::lexical_name_value(self : CstNode) -> CstNode? {
  if self.kind is (Name_Identifier | Name_Package) {
    return Some(self)
  }
  self.first_child_of_kind(fn(kind) { kind is (Name_Identifier | Name_Package) })
}

///|
fn name_component_node(source : CstNode) -> CstNode {
  let value = source.lexical_name_value().unwrap_or(source)
  scalar_node_from_kind(
    value,
    value.loc,
    value.source_span,
    String(value.string_payload().unwrap_or("")),
  )
}

///|
fn structured_long_ident_node(sources : Array[CstNode]) -> CstNode? {
  guard sources.get(0) is Some(first) else { return None }
  let last = sources.last().unwrap_or(first)
  let components : Array[(String?, CstNode)] = []
  for source in sources {
    push_child(components, Some("component"), name_component_node(source))
  }
  Some(
    node(
      if sources.length() >= 2 {
        fn(loc, source_span, children) {
          let component = CstNode::children_with_label_from(
            children, "component",
          )
          @node.CstNode::long_ident_dot(loc~, source_span~, component~)
        }
      } else {
        fn(loc, source_span, children) {
          let component = CstNode::first_child_with_label_from(
            children, "component",
          ).unwrap_or(CstNode::missing(loc~, source_span~))
          @node.CstNode::long_ident_ident(loc~, source_span~, component~)
        }
      },
      first.loc.merge(last.loc),
      mk_span(first.source_span.0, last.source_span.1),
      components,
    ),
  )
}

///|
fn structured_name_node(
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  sources : Array[CstNode],
) -> CstNode? {
  guard structured_long_ident_node(sources) is Some(long_ident) else {
    return None
  }
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("long_ident"), long_ident)
  Some(node(builder, long_ident.loc, long_ident.source_span, children))
}

///|
fn CstNode::with_structured_long_ident(
  self : CstNode,
  label : String,
  source_labels : Array[String],
) -> CstNode {
  let sources : Array[CstNode] = []
  for source_label in source_labels {
    for source in self.children_with_label(source_label) {
      if source.lexical_name_value() is Some(_) {
        sources.push(source)
      }
    }
  }
  match structured_long_ident_node(sources) {
    Some(long_ident) => {
      let children : Array[(String?, CstNode)] = []
      for entry in self.children {
        children.push(entry)
      }
      push_child(children, Some(label), long_ident)
      self.internal_rebuild(
        loc=self.loc,
        source_span=self.source_span,
        children~,
      )
    }
    None => self
  }
}

///|
fn CstNode::with_structured_name(
  self : CstNode,
  label : String,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  source_labels : Array[String],
) -> CstNode {
  let sources : Array[CstNode] = []
  for source_label in source_labels {
    for source in self.children_with_label(source_label) {
      if source.lexical_name_value() is Some(_) {
        sources.push(source)
      }
    }
  }
  match structured_name_node(builder, sources) {
    Some(name) => {
      let children : Array[(String?, CstNode)] = []
      for entry in self.children {
        children.push(entry)
      }
      push_child(children, Some(label), name)
      self.internal_rebuild(
        loc=self.loc,
        source_span=self.source_span,
        children~,
      )
    }
    None => self
  }
}

///|
fn CstNode::with_constructor_extra_info(self : CstNode) -> CstNode {
  let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if self.has_child_label(
      "type",
    ) &&
    self.has_child_label("pkg") {
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::constructor_extra_info_type_name_with_constr_package(
        loc~,
        source_span~,
      )
    }
  } else if self.has_child_label("type") {
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::constructor_extra_info_type_name(loc~, source_span~)
    }
  } else if self.has_child_label("pkg") {
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::constructor_extra_info_package(loc~, source_span~)
    }
  } else {
    let names = self.children_with_label("name")
    if names.length() == 2 &&
      names[0]
      .lexical_name_value()
      .map(fn(name) { name.kind is Name_Package })
      .unwrap_or(false) {
      fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::constructor_extra_info_package(loc~, source_span~)
      }
    } else if names.length() >= 2 {
      fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::constructor_extra_info_type_name(loc~, source_span~)
      }
    } else {
      fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::constructor_extra_info_none(loc~, source_span~)
      }
    }
  }
  with_marker_child(self, "constructor_extra_info", builder)
}

///|
fn CstNode::with_bool_marker(
  self : CstNode,
  label : String,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  value : Bool,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for entry in self.children {
    children.push(entry)
  }
  push_child(
    children,
    Some(label),
    bool_marker_node(builder, self.loc.end, self.source_span.1, value),
  )
  self.internal_rebuild(loc=self.loc, source_span=self.source_span, children~)
}

///|
fn accessor_node_from_token(token : CstNode) -> CstNode {
  match token.first_child_of_kind(NodeKind::is_accessor) {
    Some(accessor) => accessor
    None => {
      let value = token.lexical_name_value().unwrap_or(token)
      scalar_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::accessor_label(value~, loc~, source_span~)
        },
        token.loc,
        token.source_span,
        String(value.string_payload().unwrap_or("")),
      )
    }
  }
}

///|
fn CstNode::first_constant_descendant(self : CstNode) -> CstNode? {
  if self.kind.is_constant() {
    return Some(self)
  }
  for entry in self.children {
    let (_, child) = entry
    if child.kind.is_constant() {
      return Some(child)
    }
  }
  for entry in self.children {
    let (_, child) = entry
    if child.first_constant_descendant() is Some(constant) {
      return Some(constant)
    }
  }
  None
}

///|
fn CstNode::first_operator_descendant(self : CstNode) -> CstNode? {
  if self.kind.is_operator() {
    return Some(self)
  }
  for entry in self.children {
    let (_, child) = entry
    if child.first_operator_descendant() is Some(operator) {
      return Some(operator)
    }
  }
  None
}

///|
fn CstNode::with_direct_semantic_constant(self : CstNode) -> CstNode {
  if self.first_child_with_label("semantic_constant") is Some(_) {
    return self
  }
  match self.first_constant_descendant() {
    Some(constant) => {
      let children : Array[(String?, CstNode)] = []
      for entry in self.children {
        children.push(entry)
      }
      push_child(children, Some("semantic_constant"), constant)
      self.internal_rebuild(
        loc=self.loc,
        source_span=self.source_span,
        children~,
      )
    }
    None => self
  }
}

///|
fn CstNode::first_trailing_mark_descendant(self : CstNode) -> CstNode? {
  if self.kind is (TrailingMark_Comma | TrailingMark_Semi | TrailingMark_None) {
    return Some(self)
  }
  for entry in self.children {
    let (_, child) = entry
    if child.first_trailing_mark_descendant() is Some(mark) {
      return Some(mark)
    }
  }
  None
}

///|
fn list_has_item_kind(node : CstNode, pred : (NodeKind) -> Bool) -> Bool {
  for item in node.children_with_label("item") {
    if pred(item.kind) {
      return true
    }
  }
  false
}

///|
fn list_trailing_mark_kind(
  node : CstNode,
) -> (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode {
  let mut trailing : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let _ = children
    @node.CstNode::trailing_mark_none(loc~, source_span~)
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some("item"), _)) =>
        trailing = fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::trailing_mark_none(loc~, source_span~)
        }
      Some((Some("sep"), sep)) =>
        match sep.first_trailing_mark_descendant() {
          Some(mark) =>
            trailing = match mark.kind {
              TrailingMark_Comma =>
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::trailing_mark_comma(loc~, source_span~)
                }
              TrailingMark_Semi =>
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::trailing_mark_semi(loc~, source_span~)
                }
              _ =>
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::trailing_mark_none(loc~, source_span~)
                }
            }
          None => ()
        }
      Some((Some("error"), error)) =>
        match error.first_trailing_mark_descendant() {
          Some(mark) =>
            trailing = match mark.kind {
              TrailingMark_Comma =>
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::trailing_mark_comma(loc~, source_span~)
                }
              TrailingMark_Semi =>
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::trailing_mark_semi(loc~, source_span~)
                }
              _ =>
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::trailing_mark_none(loc~, source_span~)
                }
            }
          None => ()
        }
      _ => ()
    }
  }
  trailing
}

///|
fn with_marker_child(
  source : CstNode,
  label : String,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in source.children {
    children.push(child)
  }
  push_child(
    children,
    Some(label),
    marker_node(builder, source.loc.end, source.source_span.1),
  )
  source.internal_rebuild(
    loc=source.loc,
    source_span=source.source_span,
    children~,
  )
}

///|
fn lower_constant_from_node(node : CstNode) -> @syntax.Constant? {
  let direct = if node.kind.is_constant() {
    Some(node)
  } else {
    node.first_child_with_label("semantic_constant")
  }
  guard direct is Some(constant) else { return None }
  match constant.kind {
    Constant_Bool =>
      Some(@syntax.Constant::Bool(constant.bool_payload().unwrap_or(false)))
    Constant_Byte =>
      Some(@syntax.Constant::Byte(constant.string_payload().unwrap_or("")))
    Constant_Bytes =>
      Some(@syntax.Constant::Bytes(constant.string_payload().unwrap_or("")))
    Constant_Char =>
      Some(@syntax.Constant::Char(constant.string_payload().unwrap_or("")))
    Constant_Int =>
      Some(@syntax.Constant::Int(constant.string_payload().unwrap_or("")))
    Constant_Int64 =>
      Some(@syntax.Constant::Int64(constant.string_payload().unwrap_or("")))
    Constant_UInt =>
      Some(@syntax.Constant::UInt(constant.string_payload().unwrap_or("")))
    Constant_UInt64 =>
      Some(@syntax.Constant::UInt64(constant.string_payload().unwrap_or("")))
    Constant_Float =>
      Some(@syntax.Constant::Float(constant.string_payload().unwrap_or("")))
    Constant_Double =>
      Some(@syntax.Constant::Double(constant.string_payload().unwrap_or("")))
    Constant_String =>
      Some(@syntax.Constant::String(constant.string_payload().unwrap_or("")))
    Constant_Regex =>
      Some(@syntax.Constant::Regex(constant.string_payload().unwrap_or("")))
    Constant_BigInt =>
      Some(@syntax.Constant::BigInt(constant.string_payload().unwrap_or("")))
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
      Some(
        @syntax.InterpElem::Literal(
          repr=node.string_payload().unwrap_or(""),
          loc=node.loc,
        ),
      )
    InterpSegment_Source =>
      Some(
        @syntax.InterpElem::Source(@tokens.InterpSource::{
          source: node.string_payload().unwrap_or(""),
          loc: node.loc,
        }),
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
