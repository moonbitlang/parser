///|
fn lower_type(node : CstNode) -> @syntax.Type {
  match node.kind {
    Type_Name => lower_name_type(node, is_object=false)
    Type_Object => lower_object_type(node)
    Type_Application => lower_application_type(node)
    Type_Option => lower_option_type(node)
    Type_Tuple => lower_tuple_type(node)
    Type_Group =>
      if node.first_child_with_label("type") is Some(ty) {
        lower_type(ty)
      } else if node.first_child_with_label("body") is Some(body) {
        if lower_type_items_from_list(body).get(0) is Some(ty) {
          ty
        } else {
          Tuple(tys=@list.empty(), loc=node.loc)
        }
      } else {
        Any(loc=node.loc)
      }
    Type_Arrow => lower_function_type(node)
    Type_Any | Type_MissingType => Any(loc=node.loc)
    _ => Any(loc=node.loc)
  }
}

///|
fn lower_name_type(node : CstNode, is_object~ : Bool) -> @syntax.Type {
  let names = node.children_with_label("name")
  let constr_loc = if names.get(0) is Some(name) { name.loc } else { node.loc }
  let constr_id = lower_constr_id_from_node(node, constr_loc)
  if is_object {
    Object(constr_id)
  } else {
    Name(constr_id~, tys=@list.empty(), loc=node.loc)
  }
}

///|
fn lower_object_type(node : CstNode) -> @syntax.Type {
  if node.first_child_with_label("type") is Some(ty) {
    match ty.kind {
      Type_Name => {
        let constr_id = lower_constr_id_from_node(ty, ty.loc)
        Object(constr_id)
      }
      _ => Any(loc=node.loc)
    }
  } else {
    Any(loc=node.loc)
  }
}

///|
fn lower_application_type(node : CstNode) -> @syntax.Type {
  let head = if node.first_child_with_label("func") is Some(func) {
    Some(func)
  } else {
    node.first_child_with_label("head")
  }
  let args = if node.first_child_with_label("arg") is Some(list) {
    lower_type_items_from_list(list)
  } else {
    []
  }
  if head is Some(head_node) {
    match lower_type(head_node) {
      Name(constr_id~, loc=_, tys=_) =>
        Name(constr_id~, tys=lower_array_to_list(args), loc=node.loc)
      other => other
    }
  } else {
    Any(loc=node.loc)
  }
}

///|
fn lower_option_type(node : CstNode) -> @syntax.Type {
  let inner_node = node.first_child_with_label("type")
  let inner = if inner_node is Some(ty) {
    lower_type(ty)
  } else {
    Any(loc=node.loc)
  }
  let question_loc = node
    .first_child_of_kind(NodeKind::is_operator)
    .map(fn(operator) { operator.loc })
    .unwrap_or(node.loc)
  let loc = match inner_node {
    Some(ty) if option_type_wraps_object(ty) =>
      Location::{ start: inner.loc().start, end: node.loc.end, }
    _ => node.loc
  }
  Option(ty=inner, loc~, question_loc~)
}

///|
fn option_type_wraps_object(node : CstNode) -> Bool {
  match node.kind {
    Type_Object => true
    Type_Option =>
      node
      .first_child_with_label("type")
      .map(option_type_wraps_object)
      .unwrap_or(false)
    _ => false
  }
}

///|
fn lower_type_items_from_list(list : CstNode) -> Array[@syntax.Type] {
  let result : Array[@syntax.Type] = []
  for item in lower_item_children(list) {
    result.push(lower_type(item))
  }
  result
}

///|
fn lower_tuple_type(node : CstNode) -> @syntax.Type {
  let items = if node.first_child_with_label("body") is Some(body) {
    lower_type_items_from_list(body)
  } else {
    []
  }
  Tuple(tys=lower_array_to_list(items), loc=node.loc)
}

///|
fn lower_type_arrow_args(param : CstNode) -> List[@syntax.Type] {
  match param.kind {
    Type_Tuple =>
      if param.first_child_with_label("body") is Some(body) {
        lower_array_to_list(lower_type_items_from_list(body))
      } else {
        @list.empty()
      }
    _ => @list.singleton(lower_type(param))
  }
}

///|
fn lower_function_type(node : CstNode) -> @syntax.Type {
  if node.first_child_with_label("type") is Some(ty) {
    match lower_type(ty) {
      Arrow(args~, res~, err~, loc=_, is_async=_, nocancel_~) =>
        Arrow(
          args~,
          res~,
          err~,
          is_async=lower_async_loc(node),
          nocancel_~,
          loc=node.loc,
        )
      other => other
    }
  } else {
    let args = if node.first_child_with_label("param") is Some(param) {
      lower_type_arrow_args(param)
    } else {
      @list.empty()
    }
    let res = if node.first_child_with_label("result") is Some(result) {
      lower_type(result)
    } else {
      Any(loc=node.loc)
    }
    Arrow(
      args~,
      res~,
      err=lower_error_type_from_clause(node.first_child_with_label("clause")),
      is_async=None,
      nocancel_=node
        .first_child_with_label("nocancel")
        .map(fn(anchor) { anchor.loc }),
      loc=node.loc,
    )
  }
}

///|
fn lower_type_name_from_type_node(node : CstNode) -> @syntax.TypeName {
  match node.kind {
    Type_Object =>
      if node.first_child_with_label("type") is Some(ty) {
        let base = lower_type_name_from_type_node(ty)
        { name: base.name, is_object: true, loc: node.loc, }
      } else {
        { name: @syntax.LongIdent::dummy(), is_object: true, loc: node.loc, }
      }
    Type_Name =>
      {
        name: lower_long_ident_from_child(node, "semantic_name", "name"),
        is_object: node
        .bool_marker_value(fn(kind) { kind is Flag_Object })
        .unwrap_or(false),
        loc: node.loc,
      }
    _ => { name: @syntax.LongIdent::dummy(), is_object: false, loc: node.loc, }
  }
}
