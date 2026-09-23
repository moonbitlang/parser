///|
fn CstNode::lexical_name_value(self : CstNode) -> CstNode? {
  if self.kind is (Name_Identifier | Name_Package) {
    return Some(self)
  }
  self.first_child_of_kind(fn(kind) { kind is (Name_Identifier | Name_Package) })
}

///|
fn name_component_node(source : CstNode) -> CstNode {
  let name = source.lexical_name_value().unwrap_or(source)
  let value = name.string_payload().unwrap_or("")
  let loc = name.loc
  let source_span = name.source_span
  match name.kind {
    Name_Identifier =>
      @node.CstNode::name_identifier(value~, loc~, source_span~)
    Name_Package => @node.CstNode::name_package(value~, loc~, source_span~)
    _ => abort("expected a lexical name component")
  }
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
          let component = children_with_label_from(children, "component")
          @node.CstNode::long_ident_dot(loc~, source_span~, component~)
        }
      } else {
        fn(loc, source_span, children) {
          let component = first_child_with_label_from(children, "component").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          @node.CstNode::long_ident_ident(loc~, source_span~, component~)
        }
      },
      first.loc.merge(last.loc),
      (first.source_span.0, last.source_span.1),
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
pub fn push_structured_long_ident(
  children : Array[(String?, CstNode)],
  label : String,
  source_labels : Array[String],
) -> Unit {
  let sources : Array[CstNode] = []
  for source_label in source_labels {
    for source in children_with_label_from(children, source_label) {
      if source.lexical_name_value() is Some(_) {
        sources.push(source)
      }
    }
  }
  if structured_long_ident_node(sources) is Some(name) {
    push_child(children, Some(label), name)
  }
}

///|
pub fn push_structured_name(
  children : Array[(String?, CstNode)],
  label : String,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  source_labels : Array[String],
) -> Unit {
  let sources : Array[CstNode] = []
  for source_label in source_labels {
    for source in children_with_label_from(children, source_label) {
      if source.lexical_name_value() is Some(_) {
        sources.push(source)
      }
    }
  }
  if structured_name_node(builder, sources) is Some(name) {
    push_child(children, Some(label), name)
  }
}

///|
pub fn push_constructor_extra_info(
  children : Array[(String?, CstNode)],
  loc : Location,
  source_span : (Int, Int),
) -> Unit {
  let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if top_item_children_have_label(
      children, "type",
    ) &&
    top_item_children_have_label(children, "pkg") {
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::constructor_extra_info_type_name_with_constr_package(
        loc~,
        source_span~,
      )
    }
  } else if top_item_children_have_label(children, "type") {
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::constructor_extra_info_type_name(loc~, source_span~)
    }
  } else if top_item_children_have_label(children, "pkg") {
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::constructor_extra_info_package(loc~, source_span~)
    }
  } else {
    let names = children_with_label_from(children, "name")
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
  push_marker_child(
    children, loc, source_span, "constructor_extra_info", builder,
  )
}

///|
pub fn push_bool_marker(
  children : Array[(String?, CstNode)],
  loc : Location,
  source_span : (Int, Int),
  label : String,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  value : Bool,
) -> Unit {
  push_child(
    children,
    Some(label),
    bool_marker_node(builder, loc.end, source_span.1, value),
  )
}

///|
pub fn accessor_node_from_token(token : CstNode) -> CstNode {
  match token.first_child_of_kind(NodeKind::is_accessor) {
    Some(accessor) => accessor
    None => {
      let value = token.lexical_name_value().unwrap_or(token)
      scalar_node(
        fn(loc, source_span, children) {
          let value = match payload_from_children(children) {
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
pub fn first_operator_descendant(source : CstNode) -> CstNode? {
  if source.kind.is_operator() {
    return Some(source)
  }
  for entry in source.children {
    let (_, child) = entry
    if first_operator_descendant(child) is Some(operator) {
      return Some(operator)
    }
  }
  None
}

///|
pub fn push_direct_semantic_constant(
  children : Array[(String?, CstNode)],
) -> Unit {
  if first_child_with_label_from(children, "semantic_constant") is Some(_) {
    return
  }
  // Search direct constants before descending, as on an already constructed node.
  for (_, child) in children {
    if child.kind.is_constant() {
      push_child(children, Some("semantic_constant"), child)
      return
    }
  }
  for (_, child) in children {
    if child.first_constant_descendant() is Some(constant) {
      push_child(children, Some("semantic_constant"), constant)
      return
    }
  }
}

///|
pub fn first_trailing_mark_descendant(source : CstNode) -> CstNode? {
  if source.kind is (TrailingMark_Comma | TrailingMark_Semi | TrailingMark_None) {
    return Some(source)
  }
  for entry in source.children {
    let (_, child) = entry
    if first_trailing_mark_descendant(child) is Some(mark) {
      return Some(mark)
    }
  }
  None
}

///|
pub fn list_has_item_kind(node : CstNode, pred : (NodeKind) -> Bool) -> Bool {
  for item in node.children_with_label("item") {
    if pred(item.kind) {
      return true
    }
  }
  false
}

///|
pub fn list_trailing_mark_kind(
  children : ArrayView[(String?, CstNode)],
) -> (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode {
  let mut trailing : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let _ = children
    @node.CstNode::trailing_mark_none(loc~, source_span~)
  }
  for i in 0..<children.length() {
    match children.get(i) {
      Some((Some("item"), _)) =>
        trailing = fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::trailing_mark_none(loc~, source_span~)
        }
      Some((Some("sep"), sep)) =>
        match first_trailing_mark_descendant(sep) {
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
        match first_trailing_mark_descendant(error) {
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
pub fn push_marker_child(
  children : Array[(String?, CstNode)],
  loc : Location,
  source_span : (Int, Int),
  label : String,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
) -> Unit {
  push_child(
    children,
    Some(label),
    marker_node(builder, loc.end, source_span.1),
  )
}
