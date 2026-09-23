///|
fn lower_empty_doc() -> @syntax.DocString {
  @syntax.DocString::empty()
}

///|
fn lower_empty_loc() -> Location {
  let pos = Position::{ fname: "", lnum: 1, bol: 0, cnum: 0, }
  { start: pos, end: pos, }
}

///|
fn direct_semantic_scalar(node : CstNode) -> CstNode? {
  if (node.kind.is_name() && node.string_payload() is Some(_)) ||
    node.kind.is_accessor() ||
    node.kind.is_operator() ||
    node.kind.is_constant() ||
    node.kind.is_scalar_value() ||
    node.kind.is_stub() {
    return Some(node)
  }
  for label in ["name_value", "accessor", "operator", "constant"] {
    if node.first_child_with_label(label) is Some(value) {
      return Some(value)
    }
  }
  let semantic_name = if node.kind.is_name() {
    Some(node)
  } else {
    match node.first_child_with_label("semantic_name") {
      Some(name) => Some(name)
      None =>
        match node.first_child_with_label("semantic_label") {
          Some(label) => Some(label)
          None => node.first_child_with_label("semantic_binder")
        }
    }
  }
  match semantic_name {
    Some(name) =>
      match name.first_child_with_label("long_ident") {
        Some(long_ident) =>
          long_ident
          .children_with_label("component")
          .last()
          .map(fn(component) { component })
        None => None
      }
    None => None
  }
}

///|
fn lower_semantic_string(node : CstNode) -> String? {
  direct_semantic_scalar(node)
  .map(fn(scalar) { scalar.string_payload() })
  .bind(fn(value) { value })
}

///|
fn lower_label_from_node(node : CstNode) -> @syntax.Label {
  let name = lower_semantic_string(node).unwrap_or("")
  { name, loc: node.loc, }
}

///|
fn lower_binder_from_node(node : CstNode) -> @syntax.Binder {
  let semantic = direct_semantic_scalar(node).unwrap_or(node)
  let name = lower_semantic_string(semantic).unwrap_or("")
  { name, loc: semantic.loc, }
}

///|
fn lower_post_label_binder_from_node(node : CstNode) -> @syntax.Binder {
  lower_binder_from_node(node)
}

///|
fn lower_dummy_binder(loc : Location) -> @syntax.Binder {
  { name: "", loc, }
}

///|
fn recover_long_ident_from_name_nodes(
  nodes : Array[CstNode],
) -> @syntax.LongIdent {
  if nodes.length() >= 2 {
    let pkg = lower_semantic_string(nodes[0]).unwrap_or("")
    let id = lower_semantic_string(nodes[1]).unwrap_or("")
    Dot(pkg~, id~)
  } else if nodes.length() == 1 {
    Ident(name=lower_semantic_string(nodes[0]).unwrap_or(""))
  } else {
    @syntax.LongIdent::dummy()
  }
}

///|
fn direct_long_ident_node(node : CstNode) -> CstNode? {
  if node.kind is (LongIdent_Ident | LongIdent_Dot) {
    return Some(node)
  }
  if node.kind.is_name() {
    return node.first_child_with_label("long_ident")
  }
  None
}

///|
fn lower_long_ident_node(node : CstNode) -> @syntax.LongIdent {
  guard direct_long_ident_node(node) is Some(long_ident) else {
    return @syntax.LongIdent::dummy()
  }
  let components = long_ident.children_with_label("component")
  match long_ident.kind {
    LongIdent_Ident =>
      Ident(
        name=components
          .get(0)
          .map(lower_semantic_string)
          .bind(fn(value) { value })
          .unwrap_or(""),
      )
    LongIdent_Dot =>
      Dot(
        pkg=components
          .get(0)
          .map(lower_semantic_string)
          .bind(fn(value) { value })
          .unwrap_or(""),
        id=components
          .get(1)
          .map(lower_semantic_string)
          .bind(fn(value) { value })
          .unwrap_or(""),
      )
    _ => @syntax.LongIdent::dummy()
  }
}

///|
fn lower_long_ident_from_child(
  node : CstNode,
  semantic_label : String,
  recovery_label : String,
) -> @syntax.LongIdent {
  match node.first_child_with_label(semantic_label) {
    Some(name) => lower_long_ident_node(name)
    None =>
      recover_long_ident_from_name_nodes(
        node.children_with_label(recovery_label),
      )
  }
}

///|
fn lower_constr_id_from_node(
  node : CstNode,
  loc : Location,
) -> @syntax.ConstrId {
  { id: lower_long_ident_from_child(node, "semantic_name", "name"), loc, }
}

///|
fn lower_operator_var(node : CstNode) -> @syntax.Var {
  {
    name: Ident(name=lower_semantic_string(node).unwrap_or("")),
    loc: node.loc,
  }
}

///|
fn lower_hole_expr(loc : Location) -> @syntax.Expr {
  Hole(loc~, kind=Synthesized)
}

///|
fn lower_unit_expr(loc : Location) -> @syntax.Expr {
  Unit(loc~, faked=false)
}

///|
fn lower_faked_unit_expr(loc : Location) -> @syntax.Expr {
  Unit(loc~, faked=true)
}

///|
fn[A] lower_array_to_list(items : Array[A]) -> List[A] {
  List(items)
}

///|
fn lower_item_children(node : CstNode) -> Array[CstNode] {
  node.children_with_label("item")
}

///|
fn lower_error_type_from_clause(node : CstNode?) -> @syntax.ErrorType {
  if node is Some(clause) {
    match clause.kind {
      kind if kind.is_error_type() => lower_error_annotation(clause)
      _ =>
        if return_clause_error_belongs_to_type(clause) {
          NoErrorType
        } else if clause.first_child_with_label("clause") is Some(error_clause) {
          lower_error_annotation(error_clause)
        } else {
          NoErrorType
        }
    }
  } else {
    NoErrorType
  }
}

///|
fn lower_nocancel_from_clause(node : CstNode?) -> Location? {
  if node is Some(clause) {
    if return_clause_nocancel_belongs_to_type(clause) {
      None
    } else {
      clause.first_child_with_label("nocancel").map(fn(anchor) { anchor.loc })
    }
  } else {
    None
  }
}

///|
fn return_clause_error_belongs_to_type(clause : CstNode) -> Bool {
  match clause.kind {
    Clause_ReturnType =>
      match
        (
          clause.first_child_with_label("type"),
          clause.first_child_with_label("clause"),
        ) {
        (Some(ty), Some(_)) => ty.kind is Type_Arrow
        _ => false
      }
    _ => false
  }
}

///|
fn return_clause_nocancel_belongs_to_type(clause : CstNode) -> Bool {
  match clause.kind {
    Clause_ReturnType =>
      match
        (
          clause.first_child_with_label("type"),
          clause.first_child_with_label("nocancel"),
        ) {
        (Some(ty), Some(_)) => ty.kind is Type_Arrow
        _ => false
      }
    _ => false
  }
}

///|
fn lower_error_annotation(node : CstNode) -> @syntax.ErrorType {
  match node.kind {
    ErrorType_Noraise => Noraise(loc=node.loc)
    ErrorType_Default => DefaultErrorType(loc=node.loc)
    ErrorType_Maybe => MaybeError(ty=lower_fake_error_type(node.loc))
    ErrorType_Error =>
      if node.first_child_with_label("type") is Some(ty_node) {
        if ty_node.kind is Type_Name &&
          ty_node.first_child_with_label("question") is Some(_) {
          ErrorType(ty=lower_question_suffixed_error_type(ty_node))
        } else {
          ErrorType(ty=lower_error_annotation_type(ty_node))
        }
      } else {
        DefaultErrorType(loc=node.loc)
      }
    _ => NoErrorType
  }
}

///|
fn lower_error_annotation_type(node : CstNode) -> @syntax.Type {
  match lower_type(node) {
    Name(constr_id~, tys~, loc~) if node.kind is Type_Name =>
      Name(constr_id={ id: constr_id.id, loc: node.loc, }, tys~, loc~)
    ty => ty
  }
}

///|
fn lower_question_suffixed_error_type(node : CstNode) -> @syntax.Type {
  let base_name = if node.first_child_with_label("name") is Some(name_node) {
    lower_semantic_string(name_node).unwrap_or("")
  } else {
    ""
  }
  let name = base_name + "?"
  Name(
    constr_id={ id: Ident(name~), loc: node.loc, },
    tys=@list.empty(),
    loc=node.loc,
  )
}

///|
fn lower_fake_error_type(loc : Location) -> @syntax.Type {
  Name(constr_id={ id: Ident(name="Error"), loc, }, tys=@list.empty(), loc~)
}
