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
          Tuple(tys=List([]), loc=node.loc)
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
fn is_unparenthesized_arrow_type(node : CstNode) -> Bool {
  type_without_trivia(node).kind is Type_Arrow
}

///|
fn type_without_trivia(node : CstNode) -> CstNode {
  // Trivia groups have a `type` child; explicit parentheses have a `body`.
  if node.kind is Type_Group && node.first_child_with_label("type") is Some(ty) {
    type_without_trivia(ty)
  } else {
    node
  }
}

///|
// Type syntax excludes transparent trivia wrappers but retains real parentheses.
// In particular, the start of `(T)?` cannot be recovered from the lowered `T`.
fn type_syntax_loc(node : CstNode) -> Location {
  let node = type_without_trivia(node)
  let first = match node.kind {
    Type_Application =>
      match node.first_child_with_label("func") {
        Some(head) => Some(head)
        None => node.first_child_with_label("head")
      }
    Type_Option => node.first_child_with_label("type")
    Type_Arrow => node.first_child_with_label("param")
    _ => None
  }
  let start = first
    .map(ty => type_syntax_loc(ty).start)
    .unwrap_or(node.loc.start)
  let end = match node.kind {
    Type_Application =>
      match node.first_child_with_label("arg") {
        Some(args) =>
          args
          .first_child_with_label("close")
          .map(close => close.loc.end)
          .unwrap_or(args.loc.end)
        None => node.loc.end
      }
    Type_Option =>
      node
      .first_child_with_label("op")
      .map(question => question.loc.end)
      .unwrap_or(node.loc.end)
    Type_Arrow => {
      let last = match
        (
          node.first_child_with_label("nocancel"),
          node.first_child_with_label("clause"),
        ) {
        (Some(a), Some(b)) => Some(a.loc.merge(b.loc))
        (Some(a), None) | (None, Some(a)) => Some(a.loc)
        _ => None
      }
      match last {
        Some(last) => last.end
        None => {
          let result = match node.first_child_with_label("result") {
            Some(result) => Some(result)
            None => node.first_child_with_label("type")
          }
          result.map(ty => type_syntax_loc(ty).end).unwrap_or(node.loc.end)
        }
      }
    }
    _ => node.loc.end
  }
  { start, end, }
}

///|
fn lower_return_type_effects(
  node : CstNode,
  ty : @syntax.Type,
  error_clause : CstNode?,
  nocancel_anchor : CstNode?,
) -> @syntax.Type {
  guard is_unparenthesized_arrow_type(node) else { return ty }
  // Both async and trivia wrappers are transparent to effect binding.
  if node.first_child_with_label("type") is Some(inner) {
    return lower_return_type_effects(inner, ty, error_clause, nocancel_anchor)
  }
  guard ty is Arrow(args~, res~, err~, is_async~, nocancel_~, loc~) else {
    return ty
  }
  let loc = error_clause.map(clause => loc.merge(clause.loc)).unwrap_or(loc)
  let loc = nocancel_anchor.map(anchor => loc.merge(anchor.loc)).unwrap_or(loc)
  if node.first_child_with_label("result") is Some(result) &&
    is_unparenthesized_arrow_type(result) {
    Arrow(
      args~,
      res=lower_return_type_effects(result, res, error_clause, nocancel_anchor),
      err~,
      is_async~,
      nocancel_~,
      loc~,
    )
  } else {
    Arrow(
      args~,
      res~,
      err=error_clause.map(lower_error_annotation).unwrap_or(err),
      is_async~,
      nocancel_=match nocancel_anchor {
        Some(anchor) => Some(anchor.loc)
        None => nocancel_
      },
      loc~,
    )
  }
}

///|
fn lower_name_type(node : CstNode, is_object~ : Bool) -> @syntax.Type {
  let names = node.children_with_label("name")
  let constr_loc = if node.first_child_with_label("semantic_name") is Some(name) {
    name.loc
  } else if names.get(0) is Some(first) {
    names.fold(init=first.loc, fn(loc, name) { loc.merge(name.loc) })
  } else {
    node.loc
  }
  let constr_id = lower_constr_id_from_node(node, constr_loc)
  if is_object {
    Object(constr_id)
  } else {
    Name(constr_id~, tys=List([]), loc=node.loc)
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
        Name(
          constr_id~,
          tys=lower_array_to_list(args),
          loc=type_syntax_loc(node),
        )
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
  Option(ty=inner, loc=type_syntax_loc(node), question_loc~)
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
        List([])
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
          loc=type_syntax_loc(node),
        )
      other => other
    }
  } else {
    let param = node.first_child_with_label("param").map(type_without_trivia)
    let args = if param is Some(param) {
      lower_type_arrow_args(param)
    } else {
      List([])
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
      loc=type_syntax_loc(node),
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
