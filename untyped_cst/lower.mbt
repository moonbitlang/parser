///|
fn lower_empty_doc() -> @syntax.DocString {
  @syntax.DocString::empty()
}

///|
fn lower_empty_loc() -> Location {
  let pos = initial_position("")
  Location::{ start: pos, end: pos, }
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
  @syntax.Label::{ name, loc: node.loc, }
}

///|
fn lower_binder_from_node(node : CstNode) -> @syntax.Binder {
  let semantic = direct_semantic_scalar(node).unwrap_or(node)
  let name = lower_semantic_string(semantic).unwrap_or("")
  @syntax.Binder::{ name, loc: semantic.loc, }
}

///|
fn lower_post_label_binder_from_node(node : CstNode) -> @syntax.Binder {
  lower_binder_from_node(node)
}

///|
fn lower_dummy_binder(loc : Location) -> @syntax.Binder {
  @syntax.Binder::{ name: "", loc, }
}

///|
fn recover_long_ident_from_name_nodes(
  nodes : Array[CstNode],
) -> @syntax.LongIdent {
  if nodes.length() >= 2 {
    let pkg = lower_semantic_string(nodes[0]).unwrap_or("")
    let id = lower_semantic_string(nodes[1]).unwrap_or("")
    @syntax.LongIdent::Dot(pkg~, id~)
  } else if nodes.length() == 1 {
    @syntax.LongIdent::Ident(name=lower_semantic_string(nodes[0]).unwrap_or(""))
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
      @syntax.LongIdent::Ident(
        name=components
          .get(0)
          .map(lower_semantic_string)
          .bind(fn(value) { value })
          .unwrap_or(""),
      )
    LongIdent_Dot =>
      @syntax.LongIdent::Dot(
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
  @syntax.ConstrId::{
    id: lower_long_ident_from_child(node, "semantic_name", "name"),
    loc,
  }
}

///|
fn lower_operator_var(node : CstNode) -> @syntax.Var {
  @syntax.Var::{
    name: @syntax.LongIdent::Ident(
      name=lower_semantic_string(node).unwrap_or(""),
    ),
    loc: node.loc,
  }
}

///|
fn lower_hole_expr(loc : Location) -> @syntax.Expr {
  @syntax.Expr::Hole(loc~, kind=@syntax.Hole::Synthesized)
}

///|
fn lower_unit_expr(loc : Location) -> @syntax.Expr {
  @syntax.Expr::Unit(loc~, faked=false)
}

///|
fn lower_faked_unit_expr(loc : Location) -> @syntax.Expr {
  @syntax.Expr::Unit(loc~, faked=true)
}

///|
fn[A] lower_array_to_list(items : Array[A]) -> List[A] {
  @list.List(items)
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
          @syntax.ErrorType::NoErrorType
        } else if clause.first_child_with_label("clause") is Some(error_clause) {
          lower_error_annotation(error_clause)
        } else {
          @syntax.ErrorType::NoErrorType
        }
    }
  } else {
    @syntax.ErrorType::NoErrorType
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
    ErrorType_Noraise => @syntax.ErrorType::Noraise(loc=node.loc)
    ErrorType_Default => @syntax.ErrorType::DefaultErrorType(loc=node.loc)
    ErrorType_Maybe =>
      @syntax.ErrorType::MaybeError(ty=lower_fake_error_type(node.loc))
    ErrorType_Error =>
      if node.first_child_with_label("type") is Some(ty_node) {
        if ty_node.kind is Type_Name &&
          ty_node.first_child_with_label("question") is Some(_) {
          @syntax.ErrorType::ErrorType(
            ty=lower_question_suffixed_error_type(ty_node),
          )
        } else {
          @syntax.ErrorType::ErrorType(ty=lower_error_annotation_type(ty_node))
        }
      } else {
        @syntax.ErrorType::DefaultErrorType(loc=node.loc)
      }
    _ => @syntax.ErrorType::NoErrorType
  }
}

///|
fn lower_error_annotation_type(node : CstNode) -> @syntax.Type {
  match lower_type(node) {
    @syntax.Type::Name(constr_id~, tys~, loc~) if node.kind is Type_Name =>
      @syntax.Type::Name(
        constr_id=@syntax.ConstrId::{ id: constr_id.id, loc: node.loc, },
        tys~,
        loc~,
      )
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
  @syntax.Type::Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name~),
      loc: node.loc,
    },
    tys=@list.empty(),
    loc=node.loc,
  )
}

///|
fn lower_fake_error_type(loc : Location) -> @syntax.Type {
  @syntax.Type::Name(
    constr_id=@syntax.ConstrId::{
      id: @syntax.LongIdent::Ident(name="Error"),
      loc,
    },
    tys=@list.empty(),
    loc~,
  )
}

///|
fn lower_impls_from_cst(root : CstNode) -> @syntax.Impls {
  let impls : Array[@syntax.Impl] = []
  match root.kind {
    Impls =>
      for item in root.children_with_label("item") {
        impls.push(lower_top_item(item))
      }
    _ => ()
  }
  lower_array_to_list(impls)
}

///|
fn lower_top_item(node : CstNode) -> @syntax.Impl {
  match node.kind {
    Impl_Import => lower_import_decl(node)
    Impl_Extend => lower_extend_decl(node)
    Impl_Function => lower_function_decl(node)
    Impl_Let => lower_value_decl(node, is_constant=false)
    Impl_Const => lower_value_decl(node, is_constant=true)
    Impl_TypeAbstract | Impl_TypeAlias => lower_type_decl(node)
    Impl_RecordStruct | Impl_TupleStruct => lower_struct_decl(node)
    Impl_EnumVariant
    | Impl_ExtensibleEnum
    | Impl_ExtendEnum
    | Impl_Suberror
    | Impl_SuberrorEnum => lower_enum_decl(node)
    Impl_EnumView => lower_enumview_decl(node)
    Impl_Trait => lower_trait_decl(node)
    Impl_ImplMethod | Impl_ImplRelation => lower_impl_decl(node)
    Impl_Test => lower_test_decl(node)
    Impl_Using => lower_using_decl(node)
    _ =>
      @syntax.Impl::TopExpr(
        expr=lower_hole_expr(node.loc),
        is_main=false,
        is_async=None,
        loc=node.loc,
      )
  }
}

///|
fn lower_decl_loc_without_trailing_trivia(node : CstNode) -> Location {
  node
  .first_child_with_label("decl_loc")
  .map(fn(anchor) { anchor.loc })
  .unwrap_or(node.loc)
}

///|
fn lower_trait_decl_loc(node : CstNode) -> Location {
  lower_decl_loc_without_trailing_trivia(node)
}

///|
fn lower_visibility(node : CstNode) -> @syntax.Visibility {
  for vis in node.children_with_label("vis") {
    let attr = if vis.first_child_with_label("attr") is Some(attr_node) {
      lower_semantic_string(attr_node)
    } else {
      None
    }
    match vis.kind {
      Visibility_Pub => return @syntax.Visibility::Pub(attr~, loc=vis.loc)
      Visibility_Priv => return @syntax.Visibility::Priv(loc=vis.loc)
      _ => ()
    }
  }
  @syntax.Visibility::Default
}

///|
fn lower_async_loc(node : CstNode) -> Location? {
  if node.bool_marker_value(Flag_Async) is Some(false) {
    return None
  }
  node.first_child_with_label("async_loc").map(fn(anchor) { anchor.loc })
}

///|
fn lower_is_declare(node : CstNode) -> Bool {
  node.bool_marker_value(Flag_Declare).unwrap_or(false)
}

///|
fn lower_decl_name_binder(node : CstNode) -> @syntax.Binder {
  if node.first_child_with_label("name") is Some(name) {
    lower_binder_from_node(name)
  } else {
    lower_dummy_binder(node.loc)
  }
}

///|
fn lower_decl_name_string(node : CstNode) -> (String, Location) {
  let names = node.children_with_label("name")
  if names.length() > 0 {
    let name = names[names.length() - 1]
    (lower_semantic_string(name).unwrap_or(""), name.loc)
  } else {
    ("", node.loc)
  }
}

///|
fn lower_decl_type_name(node : CstNode) -> @syntax.TypeName {
  let names = node.children_with_label("name")
  let loc = if names.length() > 0 {
    names[0].loc.merge(names[names.length() - 1].loc)
  } else {
    node.loc
  }
  @syntax.TypeName::{
    name: lower_long_ident_from_child(node, "semantic_name", "name"),
    is_object: false,
    loc,
  }
}

///|
fn lower_function_decl(node : CstNode) -> @syntax.Impl {
  let name = lower_function_name_binder(node)
  let (parameters, parsed_params_loc) = lower_optional_parameters_from_list(
    node.first_child_with_label("params"),
  )
  let params_loc = if parameters is Some(_) {
    parsed_params_loc
  } else {
    empty_loc(name.loc.end)
  }
  let body_expr = node
    .first_child_with_label("body")
    .map(lower_function_body_expr)
    .unwrap_or(lower_unit_expr(node.loc))
  let return_type = lower_return_type_from_clause(
    node.first_child_with_label("clause"),
  )
  let header_loc = lower_function_header_loc(node)
  let fun_decl = @syntax.FunDecl::{
    type_name: lower_function_type_name(node),
    name,
    is_async: lower_async_loc(node),
    decl_params: parameters,
    params_loc,
    quantifiers: lower_fun_quantifiers(node),
    return_type,
    error_type: lower_error_type_from_clause(
      node.first_child_with_label("clause"),
    ),
    nocancel_: lower_nocancel_from_clause(node.first_child_with_label("clause")),
    vis: lower_visibility(node),
    attrs: lower_attrs(node),
    doc: lower_empty_doc(),
    loc: header_loc,
  }
  @syntax.Impl::TopFuncDef(
    fun_decl~,
    decl_body=lower_function_decl_body(node, body_expr),
    where_clause=node.first_child_with_label("where").map(lower_where_clause),
    loc=lower_decl_loc_without_trailing_trivia(node),
  )
}

///|
fn lower_function_header_loc(node : CstNode) -> Location {
  node
  .first_child_with_label("header_loc")
  .map(fn(anchor) { anchor.loc })
  .unwrap_or(node.loc)
}

///|
fn lower_function_name_binder(node : CstNode) -> @syntax.Binder {
  if node.first_child_with_label("method") is Some(method_name) {
    lower_binder_from_node(method_name)
  } else {
    lower_decl_name_binder(node)
  }
}

///|
fn lower_function_type_name(node : CstNode) -> @syntax.TypeName? {
  if node.first_child_with_label("method") is Some(_) {
    let owners = node.children_with_label("name")
    if owners.get(0) is Some(owner) {
      let owner_loc = if owners.last() is Some(last) {
        owner.loc.merge(last.loc)
      } else {
        owner.loc
      }
      let object_loc = lower_function_owner_object_loc(node)
      Some(@syntax.TypeName::{
        name: lower_long_ident_from_child(node, "semantic_type_name", "name"),
        is_object: object_loc is Some(_),
        loc: object_loc.map(loc => loc.merge(owner_loc)).unwrap_or(owner_loc),
      })
    } else {
      None
    }
  } else {
    None
  }
}

///|
fn lower_function_owner_object_loc(node : CstNode) -> Location? {
  if node.bool_marker_value(Flag_Object) is Some(false) {
    return None
  }
  node.first_child_with_label("object_loc").map(anchor => anchor.loc)
}

///|
fn lower_function_decl_body(
  node : CstNode,
  body_expr : @syntax.Expr,
) -> @syntax.DeclBody {
  let language = if node.first_child_with_label("language") is Some(language) {
    lower_semantic_string(language)
  } else {
    None
  }
  let stubs = node.children_with_label("stub")
  match node.first_child_of_kind(fn(kind) { kind.is_decl_body() }) {
    Some(kind_node) =>
      match kind_node.kind {
        DeclBody_None => return @syntax.DeclBody::DeclNone
        DeclBody_Import => {
          let module_name = stubs
            .get(0)
            .map(lower_semantic_string)
            .bind(value => value)
            .unwrap_or("")
          let func_name = stubs
            .get(1)
            .map(lower_semantic_string)
            .bind(value => value)
            .unwrap_or("")
          return @syntax.DeclBody::DeclStubs(
            @syntax.FuncStubs::Import(module_name~, func_name~, language~),
          )
        }
        DeclBody_EmbeddedString => {
          let code = stubs
            .get(0)
            .map(lower_semantic_string)
            .bind(value => value)
            .unwrap_or("")
          return @syntax.DeclBody::DeclStubs(
            @syntax.FuncStubs::Embedded(
              language~,
              code=@syntax.EmbeddedCode::CodeString(code),
            ),
          )
        }
        DeclBody_EmbeddedMultilineString => {
          let lines : Array[String] = []
          for stub in stubs {
            lines.push(lower_semantic_string(stub).unwrap_or(""))
          }
          return @syntax.DeclBody::DeclStubs(
            @syntax.FuncStubs::Embedded(
              language~,
              code=@syntax.EmbeddedCode::CodeMultilineString(
                lower_array_to_list(lines),
              ),
            ),
          )
        }
        DeclBody_Expr => return @syntax.DeclBody::DeclBody(expr=body_expr)
        _ => ()
      }
    None => ()
  }
  @syntax.DeclBody::DeclBody(expr=body_expr)
}

///|
fn lower_fun_quantifiers(node : CstNode) -> List[@syntax.TypeVarBinder] {
  let quantifiers : Array[@syntax.TypeVarBinder] = []
  if node.first_child_with_label("quantifier") is Some(list) {
    for item in lower_item_children(list) {
      let name_node = item.first_child_with_label("name").unwrap_or(item)
      quantifiers.push(@syntax.TypeVarBinder::{
        name: lower_semantic_string(name_node).unwrap_or(""),
        name_loc: name_node.loc,
        constraints: lower_type_var_constraints(item),
      })
    }
  } else {
    ()
  }
  lower_array_to_list(quantifiers)
}

///|
fn lower_type_var_constraints(
  node : CstNode,
) -> List[@syntax.TypeVarConstraint] {
  let constraints : Array[@syntax.TypeVarConstraint] = []
  for ty_node in node.children_with_label("type") {
    lower_type_var_constraints_from_type(ty_node, constraints)
  }
  lower_array_to_list(constraints)
}

///|
fn lower_type_var_constraints_from_type(
  node : CstNode,
  constraints : Array[@syntax.TypeVarConstraint],
) -> Unit {
  match node.kind {
    Type_Name | Type_Object =>
      constraints.push(@syntax.TypeVarConstraint::{
        trait_: lower_type_name_from_type_node(node).name,
        loc: node.loc,
      })
    Type_Tuple | Type_Group =>
      for item in node.children_with_label("type") {
        lower_type_var_constraints_from_type(item, constraints)
      }
    _ =>
      constraints.push(@syntax.TypeVarConstraint::{
        trait_: @syntax.LongIdent::dummy(),
        loc: node.loc,
      })
  }
}

///|
fn lower_value_decl(node : CstNode, is_constant~ : Bool) -> @syntax.Impl {
  let binder = if node.first_child_with_label("pattern") is Some(pat) {
    match lower_pattern(pat) {
      @syntax.Pattern::Var(binder) => binder
      _ => lower_decl_name_binder(node)
    }
  } else {
    lower_decl_name_binder(node)
  }
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_unit_expr(node.loc))
  let parsed_loc = lower_decl_loc_without_trailing_trivia(node)
  let loc = if node.first_child_with_label("expr") is Some(_) {
    Location::{ start: parsed_loc.start, end: expr.loc().end, }
  } else {
    parsed_loc
  }
  @syntax.Impl::TopLetDef(
    binder~,
    ty=node.first_child_with_label("type").map(lower_type),
    expr~,
    vis=lower_visibility(node),
    is_constant~,
    loc~,
    attrs=lower_attrs(node),
    doc=lower_empty_doc(),
    is_declare=lower_is_declare(node),
  )
}

///|
fn lower_type_decl(node : CstNode) -> @syntax.Impl {
  let (name, name_loc) = lower_decl_name_string(node)
  let components = if node.has_child_kind(TypeDesc_Abstract) ||
    node.kind is Impl_TypeAbstract {
    @syntax.TypeDesc::Abstract
  } else if node.first_child_with_label("type") is Some(ty) {
    @syntax.TypeDesc::Alias(lower_type(ty))
  } else {
    @syntax.TypeDesc::Abstract
  }
  @syntax.Impl::TopTypeDef(@syntax.TypeDecl::{
    tycon: name,
    tycon_loc: name_loc,
    params: lower_type_decl_params(node),
    components,
    attrs: lower_attrs(node),
    doc: lower_empty_doc(),
    type_vis: lower_visibility(node),
    deriving: lower_deriving(node),
    loc: lower_decl_loc_without_trailing_trivia(node),
    is_declare: lower_is_declare(node),
  })
}

///|
fn lower_simple_type_decl(
  node : CstNode,
  components : @syntax.TypeDesc,
) -> @syntax.Impl {
  let (name, name_loc) = lower_decl_name_string(node)
  @syntax.Impl::TopTypeDef(@syntax.TypeDecl::{
    tycon: name,
    tycon_loc: name_loc,
    params: lower_type_decl_params(node),
    components,
    attrs: lower_attrs(node),
    doc: lower_empty_doc(),
    type_vis: lower_visibility(node),
    deriving: lower_deriving(node),
    loc: lower_decl_loc_without_trailing_trivia(node),
    is_declare: lower_is_declare(node),
  })
}

///|
fn lower_attrs(node : CstNode) -> List[@attribute.Attribute] {
  let attrs : Array[@attribute.Attribute] = []
  for attr in node.children_with_label("attrs") {
    if lower_attribute_node(attr) is Some(value) {
      attrs.push(value)
    }
  }
  lower_array_to_list(attrs)
}

///|
fn lower_deriving(node : CstNode) -> List[@syntax.DerivingDirective] {
  let deriving : Array[@syntax.DerivingDirective] = []
  if node.first_child_with_label("deriving") is Some(list) {
    for item in lower_item_children(list) {
      let name_nodes = item.children_with_label("name")
      let name_loc = if name_nodes.length() > 0 {
        name_nodes[0].loc.merge(name_nodes[name_nodes.length() - 1].loc)
      } else {
        item.loc
      }
      let type_name = @syntax.TypeName::{
        name: lower_long_ident_from_child(item, "semantic_name", "name"),
        is_object: false,
        loc: name_loc,
      }
      deriving.push(@syntax.DerivingDirective::{
        type_name,
        args: item
        .first_child_with_label("arg")
        .map(lower_arguments)
        .unwrap_or(@list.empty()),
        loc: item.loc,
      })
    }
  } else {
    ()
  }
  lower_array_to_list(deriving)
}

///|
fn lower_struct_decl(node : CstNode) -> @syntax.Impl {
  if node.has_child_kind(TypeDesc_TupleStruct) || node.kind is Impl_TupleStruct {
    let components = node
      .first_child_with_label("body")
      .map(fn(body) {
        @syntax.TypeDesc::TupleStruct(lower_tuple_struct_types(body))
      })
      .unwrap_or(@syntax.TypeDesc::TupleStruct(@list.empty()))
    lower_simple_type_decl(node, components)
  } else if node.has_child_kind(TypeDesc_Record) ||
    node.kind is Impl_RecordStruct {
    lower_simple_type_decl(
      node,
      @syntax.TypeDesc::Record(fields=lower_struct_fields(node)),
    )
  } else if node.first_child_with_label("body") is Some(body) {
    match body.kind {
      List_TupleStructTypeList =>
        lower_simple_type_decl(
          node,
          @syntax.TypeDesc::TupleStruct(lower_tuple_struct_types(body)),
        )
      _ =>
        lower_simple_type_decl(
          node,
          @syntax.TypeDesc::Record(fields=lower_struct_fields(node)),
        )
    }
  } else {
    lower_simple_type_decl(
      node,
      @syntax.TypeDesc::Record(fields=lower_struct_fields(node)),
    )
  }
}

///|
fn lower_tuple_struct_types(body : CstNode) -> List[@syntax.Type] {
  let tys : Array[@syntax.Type] = []
  for item in lower_item_children(body) {
    tys.push(lower_type(item))
  }
  lower_array_to_list(tys)
}

///|
fn lower_enum_decl(node : CstNode) -> @syntax.Impl {
  match node.kind {
    Impl_Suberror | Impl_SuberrorEnum =>
      return lower_simple_type_decl(
        node,
        @syntax.TypeDesc::Error(lower_suberror_exception_decl(node)),
      )
    Impl_ExtendEnum =>
      return lower_simple_type_decl(
        node,
        @syntax.TypeDesc::ExtendEnum(
          target=lower_decl_type_name(node),
          constructors=lower_enum_constructors(node),
        ),
      )
    Impl_ExtensibleEnum =>
      return lower_simple_type_decl(
        node,
        @syntax.TypeDesc::ExtensibleEnum(lower_enum_constructors(node)),
      )
    Impl_EnumVariant =>
      return lower_simple_type_decl(
        node,
        @syntax.TypeDesc::Variant(lower_enum_constructors(node)),
      )
    _ => ()
  }
  lower_simple_type_decl(
    node,
    @syntax.TypeDesc::Variant(lower_enum_constructors(node)),
  )
}

///|
fn lower_enumview_decl(node : CstNode) -> @syntax.Impl {
  let (parameters, params_loc) = lower_parameters_from_list(
    node.first_child_with_label("params"),
  )
  let (view_type_name, view_type_loc) = lower_decl_name_string(node)
  @syntax.Impl::TopView(
    quantifiers=lower_fun_quantifiers(node),
    source_ty=node
      .first_child_with_label("type")
      .map(lower_type)
      .unwrap_or(@syntax.Type::Any(loc=node.loc)),
    view_type_name~,
    view_type_loc~,
    view_func_name=node
      .first_child_with_label("method")
      .map(lower_binder_from_node)
      .unwrap_or(lower_dummy_binder(node.loc)),
    parameters~,
    params_loc~,
    view_constrs=lower_enum_constructors(node),
    body=node
      .first_child_with_label("expr")
      .map(lower_function_body_expr)
      .unwrap_or(lower_unit_expr(node.loc)),
    vis=lower_visibility(node),
    loc=lower_decl_loc_without_trailing_trivia(node),
    attrs=lower_attrs(node),
    doc=lower_empty_doc(),
  )
}

///|
fn lower_suberror_exception_decl(node : CstNode) -> @syntax.ExceptionDecl {
  match node.kind {
    Impl_Suberror => @syntax.ExceptionDecl::NoPayload
    Impl_SuberrorEnum =>
      @syntax.ExceptionDecl::EnumPayload(lower_enum_constructors(node))
    _ => {
      // Coarse recovery nodes predate the point where the parser can select a
      // final suberror form. Keep their fallback deterministic.
      let constructors = lower_enum_constructors(node)
      if constructors.length() == 0 {
        @syntax.ExceptionDecl::NoPayload
      } else {
        @syntax.ExceptionDecl::EnumPayload(constructors)
      }
    }
  }
}

///|
fn lower_enum_constructors(node : CstNode) -> List[@syntax.ConstrDecl] {
  let constrs : Array[@syntax.ConstrDecl] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      match item.kind {
        Clause_ConstructorDecl => constrs.push(lower_enum_constructor(item))
        _ => ()
      }
    }
  } else {
    ()
  }
  lower_array_to_list(constrs)
}

///|
fn lower_enum_constructor(node : CstNode) -> @syntax.ConstrDecl {
  let name_node = node.first_child_with_label("name").unwrap_or(node)
  let args = if node.first_child_with_label("params") is Some(params) {
    Some(lower_constructor_params(params))
  } else {
    None
  }
  @syntax.ConstrDecl::{
    name: @syntax.ConstrName::{
      name: lower_semantic_string(name_node).unwrap_or(""),
      loc: name_node.loc,
    },
    args,
    tag: lower_constructor_tag(node),
    loc: node.loc,
    attrs: lower_attrs(node),
    doc: lower_empty_doc(),
  }
}

///|
fn lower_constructor_tag(node : CstNode) -> (String, Location)? {
  if node.first_child_with_label("constructor_tag") is Some(tag) {
    Some((tag.string_payload().unwrap_or(""), tag.loc))
  } else {
    None
  }
}

///|
fn lower_constructor_params(list : CstNode) -> List[@syntax.ConstrParam] {
  let params : Array[@syntax.ConstrParam] = []
  for item in lower_item_children(list) {
    params.push(lower_constructor_param(item))
  }
  lower_array_to_list(params)
}

///|
fn lower_constructor_param(node : CstNode) -> @syntax.ConstrParam {
  let label = if node.first_child_with_label("name") is Some(name) {
    Some(@syntax.Label::{
      name: lower_semantic_string(name).unwrap_or(""),
      loc: name.loc.trim_last_char(),
    })
  } else {
    None
  }
  @syntax.ConstrParam::{
    ty: node
    .first_child_with_label("type")
    .map(lower_type)
    .unwrap_or(@syntax.Type::Any(loc=node.loc)),
    mut_: node.bool_marker_value(Flag_Mutable).unwrap_or(false),
    label,
  }
}

///|
fn lower_struct_fields(node : CstNode) -> List[@syntax.FieldDecl] {
  let fields : Array[@syntax.FieldDecl] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      match item.kind {
        Field_Declaration => fields.push(lower_struct_field(item))
        _ => ()
      }
    }
  } else {
    ()
  }
  lower_array_to_list(fields)
}

///|
fn lower_struct_field(node : CstNode) -> @syntax.FieldDecl {
  let name_node = node.first_child_with_label("name").unwrap_or(node)
  let label = lower_semantic_string(name_node).unwrap_or("")
  @syntax.FieldDecl::{
    name: @syntax.FieldName::{ label, loc: name_node.loc, },
    ty: node
    .first_child_with_label("type")
    .map(lower_type)
    .unwrap_or(@syntax.Type::Any(loc=node.loc)),
    mut_: node.bool_marker_value(Flag_Mutable).unwrap_or(false),
    vis: lower_visibility(node),
    loc: node.loc,
    attrs: lower_attrs(node),
    doc: lower_empty_doc(),
  }
}

///|
fn lower_type_decl_params(node : CstNode) -> List[@syntax.TypeDeclBinder] {
  let params : Array[@syntax.TypeDeclBinder] = []
  if node.first_child_with_label("params") is Some(list) {
    for item in lower_item_children(list) {
      let name_node = item.first_child_with_label("name").unwrap_or(item)
      let name = match lower_semantic_string(name_node) {
        Some("_") => None
        value => value
      }
      params.push(@syntax.TypeDeclBinder::{ name, loc: name_node.loc, })
    }
  } else {
    ()
  }
  lower_array_to_list(params)
}

///|
fn lower_trait_decl(node : CstNode) -> @syntax.Impl {
  @syntax.Impl::TopTrait(@syntax.TraitDecl::{
    name: lower_decl_name_binder(node),
    supers: lower_type_var_constraints(node),
    methods: lower_trait_methods(node),
    vis: lower_visibility(node),
    loc: lower_trait_decl_loc(node),
    attrs: lower_attrs(node),
    doc: lower_empty_doc(),
    is_declare: lower_is_declare(node),
  })
}

///|
fn lower_trait_methods(node : CstNode) -> List[@syntax.TraitMethodDecl] {
  let methods : Array[@syntax.TraitMethodDecl] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      match item.kind {
        TraitMethod_Declaration => methods.push(lower_trait_method(item))
        _ => ()
      }
    }
  } else {
    ()
  }
  lower_array_to_list(methods)
}

///|
fn lower_trait_method(node : CstNode) -> @syntax.TraitMethodDecl {
  let (params, _) = lower_trait_method_parameters(
    node.first_child_with_label("params"),
  )
  @syntax.TraitMethodDecl::{
    name: lower_decl_name_binder(node),
    is_async: lower_async_loc(node),
    quantifiers: lower_fun_quantifiers(node),
    params,
    return_type: lower_return_type_from_clause(
      node.first_child_with_label("clause"),
    ),
    error_type: lower_error_type_from_clause(
      node.first_child_with_label("clause"),
    ),
    nocancel_: lower_nocancel_from_clause(node.first_child_with_label("clause")),
    has_default: node.first_child_with_label("stub").map(fn(node) { node.loc }),
    attrs: lower_attrs(node),
    loc: node.loc,
  }
}

///|
fn lower_trait_method_parameters(
  list : CstNode?,
) -> (List[@syntax.Parameter], Location) {
  if list is Some(list) {
    let params : Array[@syntax.Parameter] = []
    for item in lower_item_children(list) {
      params.push(lower_trait_method_parameter(item))
    }
    (lower_array_to_list(params), list.loc)
  } else {
    (@list.empty(), lower_empty_loc())
  }
}

///|
fn lower_trait_method_parameter(node : CstNode) -> @syntax.Parameter {
  let ty = node.first_child_with_label("type").map(lower_type)
  if node.first_child_with_label("pattern") is Some(pat_node) {
    let binder = lower_post_label_binder_from_node(pat_node)
    match node.kind {
      Parameter_Labelled => @syntax.Parameter::Labelled(binder~, ty~)
      Parameter_QuestionOptional =>
        @syntax.Parameter::QuestionOptional(binder~, ty~)
      _ => @syntax.Parameter::Positional(binder~, ty~)
    }
  } else {
    @syntax.Parameter::DiscardPositional(
      ty~,
      loc=ty.map(type_ => type_.loc()).unwrap_or(node.loc),
    )
  }
}

///|
fn lower_impl_decl(node : CstNode) -> @syntax.Impl {
  match node.kind {
    Impl_ImplMethod =>
      return if node.first_child_with_label("method") is Some(method_node) {
        lower_impl_method_decl(node, method_node)
      } else {
        lower_impl_method_decl(node, node)
      }
    Impl_ImplRelation => ()
    _ =>
      if node.first_child_with_label("method") is Some(method_node) {
        return lower_impl_method_decl(node, method_node)
      }
  }
  let trait_name = lower_decl_type_name(node)
  @syntax.Impl::TopImplRelation(
    self_ty=node
      .first_child_with_label("type")
      .map(lower_type)
      .unwrap_or(@syntax.Type::Any(loc=node.loc)),
    trait_=trait_name,
    quantifiers=lower_fun_quantifiers(node),
    vis=lower_visibility(node),
    attrs=lower_attrs(node),
    loc=lower_decl_loc_without_trailing_trivia(node),
    doc=lower_empty_doc(),
    is_declare=lower_is_declare(node),
  )
}

///|
fn lower_impl_method_decl(
  node : CstNode,
  method_node : CstNode,
) -> @syntax.Impl {
  let trait_name = lower_decl_type_name(node)
  let (params, _) = lower_parameters_from_list(
    method_node.first_child_with_label("params"),
  )
  @syntax.Impl::TopImpl(
    self_ty=node.first_child_with_label("type").map(lower_type),
    trait_=trait_name,
    method_name=lower_decl_name_binder(method_node),
    quantifiers=lower_fun_quantifiers(node),
    method_quantifiers=lower_fun_quantifiers(method_node),
    params~,
    ret_ty=lower_return_type_from_clause(
      method_node.first_child_with_label("clause"),
    ),
    err_ty=lower_error_type_from_clause(
      method_node.first_child_with_label("clause"),
    ),
    nocancel_=lower_nocancel_from_clause(
      method_node.first_child_with_label("clause"),
    ),
    body=lower_function_decl_body(
      method_node,
      method_node
      .first_child_with_label("body")
      .map(lower_function_body_expr)
      .unwrap_or(lower_unit_expr(method_node.loc)),
    ),
    vis=lower_visibility(node),
    loc=lower_decl_loc_without_trailing_trivia(node),
    attrs=lower_attrs(node),
    doc=lower_empty_doc(),
  )
}

///|
fn lower_test_decl(node : CstNode) -> @syntax.Impl {
  let expr = node
    .first_child_with_label("body")
    .map(lower_function_body_expr)
    .unwrap_or(lower_unit_expr(node.loc))
  let name : @syntax.TestName = if node.first_child_with_label("test_name")
    is Some(name_node) {
    Some((name_node.string_payload().unwrap_or(""), name_node.loc))
  } else {
    None
  }
  let (params, _) = lower_optional_parameters_from_list(
    node.first_child_with_label("params"),
  )
  @syntax.Impl::TopTest(
    expr~,
    name~,
    params~,
    is_async=lower_async_loc(node),
    loc=lower_decl_loc_without_trailing_trivia(node),
    attrs=lower_attrs(node),
    doc=lower_empty_doc(),
  )
}

///|
fn lower_using_decl(node : CstNode) -> @syntax.Impl {
  let pkg = node
    .first_child_with_label("name")
    .map(lower_label_from_node)
    .unwrap_or(@syntax.Label::{ name: "", loc: node.loc, })
  @syntax.Impl::TopUsing(
    pkg~,
    names=lower_using_names(node),
    vis=lower_visibility(node),
    attrs=lower_attrs(node),
    loc=lower_decl_loc_without_trailing_trivia(node),
    doc=lower_empty_doc(),
  )
}

///|
fn lower_string_literal_node(node : CstNode) -> String {
  match node.string_payload() {
    Some(value) => value
    None =>
      match lower_constant_from_node(node) {
        Some(String(value)) => value
        _ => ""
      }
  }
}

///|
fn lower_import_decl(node : CstNode) -> @syntax.Impl {
  let packages : Array[@syntax.ImportPackage] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      let path_node = item.first_child_with_label("path").unwrap_or(item)
      let alias_node = item.first_child_with_label("alias")
      let alias_ = alias_node.map(lower_semantic_string).bind(value => value)
      packages.push({
        path: lower_string_literal_node(path_node),
        alias_,
        loc: alias_node
        .map(alias_ => path_node.loc.merge(alias_.loc))
        .unwrap_or(path_node.loc),
      })
    }
  }
  let kind = node.first_child_with_label("kind").map(lower_string_literal_node)
  TopImport(
    kind~,
    packages=lower_array_to_list(packages),
    loc=lower_decl_loc_without_trailing_trivia(node),
    doc=lower_empty_doc(),
  )
}

///|
fn lower_extend_decl(node : CstNode) -> @syntax.Impl {
  let type_name = node
    .first_child_with_label("type_name")
    .map(lower_type_name_from_type_node)
    .unwrap_or({
      name: @syntax.LongIdent::dummy(),
      is_object: false,
      loc: node.loc,
    })
  let trait_ = node
    .first_child_with_label("trait")
    .map(lower_type_name_from_type_node)
    .unwrap_or({
      name: @syntax.LongIdent::dummy(),
      is_object: false,
      loc: node.loc,
    })
  let names : Array[@syntax.AliasTarget] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      let target_node = item.first_child_with_label("target").unwrap_or(item)
      let binder_node = item
        .first_child_with_label("name")
        .unwrap_or(target_node)
      names.push({
        binder: lower_binder_from_node(binder_node),
        target: if item.first_child_with_label("name") is Some(_) {
          Some(lower_label_from_node(target_node))
        } else {
          None
        },
      })
    }
  }
  TopExtend(
    type_name~,
    trait_~,
    names=lower_array_to_list(names),
    vis=lower_visibility(node),
    attrs=lower_attrs(node),
    loc=lower_decl_loc_without_trailing_trivia(node),
    doc=lower_empty_doc(),
  )
}

///|
fn lower_using_names(
  node : CstNode,
) -> List[(@syntax.AliasTarget, @syntax.UsingKind)] {
  let names : Array[(@syntax.AliasTarget, @syntax.UsingKind)] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      let target_node = item.first_child_with_label("target").unwrap_or(item)
      let binder_node = item
        .first_child_with_label("name")
        .unwrap_or(target_node)
      let target = if item.first_child_with_label("name") is Some(_) {
        Some(lower_label_from_node(target_node))
      } else {
        None
      }
      let using_kind = match
        item.first_child_of_kind(fn(kind) {
          kind is (UsingKind_Value | UsingKind_Type | UsingKind_Trait)
        }) {
        Some(kind_node) =>
          match kind_node.kind {
            UsingKind_Type => @syntax.UsingKind::Type
            UsingKind_Trait => @syntax.UsingKind::Trait
            _ => @syntax.UsingKind::Value
          }
        None => @syntax.UsingKind::Value
      }
      names.push(
        (
          @syntax.AliasTarget::{
            binder: lower_binder_from_node(binder_node),
            target,
          },
          using_kind,
        ),
      )
    }
  } else {
    ()
  }
  lower_array_to_list(names)
}

///|
fn lower_expr(node : CstNode) -> @syntax.Expr {
  match node.kind {
    Expression => {
      let items = node.children_with_label("expr")
      if items.is_empty() {
        lower_hole_expr(node.loc)
      } else {
        lower_statement_sequence(items, 0, node.loc)
      }
    }
    Expr_Ident =>
      @syntax.Expr::Ident(
        id=@syntax.Var::{
          name: lower_long_ident_from_child(node, "semantic_name", "name"),
          loc: node.loc,
        },
        loc=node.loc,
      )
    Expr_Constr => lower_constructor_expr(node)
    Expr_Constant | Expr_Interp | Expr_BytesInterp => lower_literal_expr(node)
    Expr_MultilineString => lower_multiline_string_expr(node)
    Expr_Hole =>
      @syntax.Expr::Hole(
        loc=node.loc,
        kind=if node.has_child_kind(HoleKind_Todo) {
          @syntax.Hole::Todo
        } else {
          @syntax.Hole::Incomplete
        },
      )
    Expr_MissingExpr => lower_hole_expr(node.loc)
    Expr_Unit | Expr_Group | Expr_Tuple => lower_tuple_expr(node)
    Expr_Constraint => lower_constraint_expr(node)
    Expr_Array | Expr_ArraySpread => lower_array_expr_node(node)
    Expr_ListComprehension => lower_list_comprehension_expr(node)
    Expr_Record | Expr_RecordUpdate => lower_record_expr_node(node)
    Expr_Map => lower_map_expr_node(node)
    Expr_Block => lower_block_expr_node(node)
    Expr_Sequence => lower_sequence_cst_node(node)
    Expr_Infix | Expr_Pipe | Expr_RevPipe | Expr_Implies =>
      lower_infix_expr(node)
    Expr_Unary => lower_prefix_expr(node)
    Expr_Apply => lower_apply_expr(node)
    Expr_DotApply => lower_dot_apply_expr(node)
    Expr_Method => lower_method_expr(node)
    Expr_Field => lower_field_expr(node)
    Expr_ArrayGet => lower_array_get_expr(node)
    Expr_ArrayGetSlice => lower_array_get_slice_expr(node)
    Expr_If => lower_if_expr(node)
    Expr_Match => lower_match_expr(node)
    Expr_Try => lower_try_expr(node)
    Expr_TryOperator => lower_try_operator_expr(node)
    Expr_LexMatch => lower_lexmatch_expr(node)
    Expr_LexScan => lower_lexscan_expr(node)
    Expr_For => lower_for_expr(node)
    Expr_ForEach => lower_foreach_expr(node)
    Expr_While => lower_while_expr(node)
    Expr_Function => lower_function_expr_node(node)
    Expr_LetFn =>
      lower_letfn_expr_with_body(node, lower_faked_unit_expr(node.loc))
    Expr_LetAnd =>
      lower_letrec_expr_with_body(node, lower_faked_unit_expr(node.loc))
    Expr_Let | Expr_LetMut => lower_let_expr_without_body(node)
    Expr_Assign | Expr_ArraySet | Expr_ArrayAugmentedSet | Expr_Mutate =>
      lower_assign_expr(node)
    Expr_Guard => lower_guard_expr_with_body(node, lower_unit_expr(node.loc))
    Expr_Defer => lower_defer_expr_with_body(node, lower_unit_expr(node.loc))
    Expr_ErrDefer => lower_defer_expr_with_body(node, lower_unit_expr(node.loc))
    Expr_ProofAssert => lower_proof_assert_expr(node)
    Expr_ProofLet => lower_proof_let_expr(node)
    Expr_Return => lower_return_expr(node)
    Expr_Raise => lower_raise_expr(node)
    Expr_Break => lower_break_expr(node)
    Expr_Continue => lower_continue_expr(node)
    Expr_LabelledBlock => lower_labelled_block_expr(node)
    Expr_As => lower_as_expr(node)
    Expr_Is => lower_is_expr(node)
    Expr_RegexMatch => lower_regex_match_expr(node)
    Expr_TemplateWriting => lower_template_writing_expr(node)
    Expr_Quantifier => lower_quantifier_expr(node)
    _ => lower_hole_expr(node.loc)
  }
}

///|
fn lower_quantifier_expr(node : CstNode) -> @syntax.Expr {
  let kind = if node.has_child_kind(QuantifierKind_Exists) {
    @syntax.PredicateQuantifierKind::Exists
  } else {
    @syntax.PredicateQuantifierKind::Forall
  }
  @syntax.Expr::Quantifier(
    kind~,
    binder=node
      .first_child_with_label("name")
      .map(lower_binder_from_node)
      .unwrap_or(lower_dummy_binder(node.loc)),
    binder_ty=node
      .first_child_with_label("type")
      .map(lower_type)
      .unwrap_or(@syntax.Type::Any(loc=node.loc)),
    body=node
      .first_child_with_label("body")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_literal_expr(node : CstNode) -> @syntax.Expr {
  match node.kind {
    Expr_Interp =>
      @syntax.Expr::Interp(elems=lower_interp_elems(node), loc=node.loc)
    Expr_BytesInterp =>
      @syntax.Expr::BytesInterp(elems=lower_interp_elems(node), loc=node.loc)
    _ =>
      if lower_constant_from_node(node) is Some(c) {
        let loc = node
          .first_child_with_label("semantic_constant")
          .map(constant => constant.loc)
          .unwrap_or(node.loc)
        @syntax.Expr::Constant(c~, loc~)
      } else {
        lower_hole_expr(node.loc)
      }
  }
}

///|
fn lower_template_writing_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::TemplateWriting(
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    template=node
      .first_child_with_label("template")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    is_conditional=node.bool_marker_value(Flag_Conditional).unwrap_or(false),
    loc=node.loc,
  )
}

///|
fn lower_interp_elems(node : CstNode) -> List[@syntax.InterpElem] {
  let elems : Array[@syntax.InterpElem] = []
  for segment in collect_interp_segments(node) {
    if lower_interp_segment_value(segment) is Some(elem) {
      elems.push(elem)
    }
  }
  lower_array_to_list(elems)
}

///|
fn lower_child_token_name(node : CstNode, label : String) -> String {
  if node.first_child_with_label(label) is Some(child) {
    lower_semantic_string(child).unwrap_or("")
  } else {
    ""
  }
}

///|
fn lower_interp_segment(node : CstNode) -> @syntax.InterpElem {
  lower_interp_segment_value(node).unwrap_or(
    @syntax.InterpElem::Source(@tokens.InterpSource::{
      source: "",
      loc: node.loc,
    }),
  )
}

///|
fn lower_multiline_string_expr(node : CstNode) -> @syntax.Expr {
  let elems : Array[@syntax.MultilineStringElem] = []
  for element in node.children_with_label("element") {
    match element.kind {
      MultilineStringElement_String =>
        elems.push(
          @syntax.MultilineStringElem::String(
            element.string_payload().unwrap_or(""),
          ),
        )
      MultilineStringElement_Interp => {
        let interp_elems : Array[@syntax.InterpElem] = []
        for segment in element.children_with_label("segment") {
          if lower_interp_segment_value(segment) is Some(elem) {
            interp_elems.push(elem)
          }
        }
        elems.push(
          @syntax.MultilineStringElem::Interp(lower_array_to_list(interp_elems)),
        )
      }
      _ => ()
    }
  }
  @syntax.Expr::MultilineString(
    elems=lower_array_to_list(elems),
    loc=lower_multiline_string_loc(node),
  )
}

///|
fn lower_multiline_string_loc(node : CstNode) -> Location {
  node
  .first_child_with_label("literal_loc")
  .map(fn(anchor) { anchor.loc })
  .unwrap_or(node.loc)
}

///|
fn lower_constructor_expr(node : CstNode) -> @syntax.Expr {
  let constr = if node.first_child_with_label("field") is Some(field) {
    @syntax.Constructor::{
      name: @syntax.ConstrName::{
        name: lower_semantic_string(field).unwrap_or(""),
        loc: field.loc,
      },
      extra_info: lower_constructor_extra_info(node),
      loc: node.loc,
    }
  } else {
    lower_constructor_from_names(
      node.children_with_label("name"),
      node
      .first_child_of_kind(NodeKind::is_constructor_extra_info)
      .map(fn(marker) { marker.kind })
      .unwrap_or(ConstructorExtraInfo_None),
      node.loc,
    )
  }
  @syntax.Expr::Constr(constr~, loc=node.loc)
}

///|
fn lower_constructor_extra_info(node : CstNode) -> @syntax.ConstructorExtraInfo {
  let marker = node.first_child_of_kind(NodeKind::is_constructor_extra_info)
  match marker.map(fn(marker) { marker.kind }) {
    Some(ConstructorExtraInfo_TypeNameWithConstrPackage) =>
      if node.first_child_with_label("type") is Some(type_node) &&
        node.first_child_with_label("pkg") is Some(pkg_node) {
        @syntax.ConstructorExtraInfo::TypeNameWithConstrPackage(
          type_name=lower_type_name_from_expr_node(type_node),
          pkg=lower_semantic_string(pkg_node).unwrap_or(""),
        )
      } else {
        @syntax.ConstructorExtraInfo::NoExtraInfo
      }
    Some(ConstructorExtraInfo_TypeName) =>
      if node.first_child_with_label("type") is Some(type_node) {
        @syntax.ConstructorExtraInfo::TypeName(
          lower_type_name_from_expr_node(type_node),
        )
      } else {
        @syntax.ConstructorExtraInfo::NoExtraInfo
      }
    Some(ConstructorExtraInfo_Package) =>
      if node.first_child_with_label("pkg") is Some(pkg_node) {
        @syntax.ConstructorExtraInfo::Package(
          lower_semantic_string(pkg_node).unwrap_or(""),
        )
      } else {
        @syntax.ConstructorExtraInfo::NoExtraInfo
      }
    _ => @syntax.ConstructorExtraInfo::NoExtraInfo
  }
}

///|
fn lower_argument_kind(item : CstNode) -> @syntax.ArgumentKind {
  match item.kind {
    Argument_Labelled =>
      @syntax.ArgumentKind::Labelled(
        item
        .first_child_with_label("name")
        .map(lower_label_from_node)
        .unwrap_or(@syntax.Label::{ name: "", loc: item.loc, }),
      )
    Argument_LabelledPun =>
      @syntax.ArgumentKind::LabelledPun(
        item
        .first_child_with_label("name")
        .map(name => @syntax.Label::{
          name: lower_semantic_string(name).unwrap_or(""),
          loc: item
          .first_child_with_label("label_loc")
          .map(fn(anchor) { anchor.loc })
          .unwrap_or(item.loc),
        })
        .unwrap_or(@syntax.Label::{ name: "", loc: item.loc, }),
      )
    Argument_LabelledOption =>
      @syntax.ArgumentKind::LabelledOption(
        label=item
          .first_child_with_label("name")
          .map(lower_label_from_node)
          .unwrap_or(@syntax.Label::{ name: "", loc: item.loc, }),
        question_loc=item
          .first_child_with_label("question_loc")
          .map(fn(anchor) { anchor.loc })
          .unwrap_or(item.loc),
      )
    Argument_LabelledOptionPun =>
      @syntax.ArgumentKind::LabelledOptionPun(
        label=item
          .first_child_with_label("name")
          .map(lower_label_from_node)
          .unwrap_or(@syntax.Label::{ name: "", loc: item.loc, }),
        question_loc=item
          .first_child_with_label("question_loc")
          .map(fn(anchor) { anchor.loc })
          .unwrap_or(item.loc),
      )
    _ => @syntax.ArgumentKind::Positional
  }
}

///|
fn lower_arguments(list : CstNode) -> List[@syntax.Argument] {
  let args : Array[@syntax.Argument] = []
  for i in 0..<list.children.length() {
    guard list.children.get(i) is Some((Some("item"), item)) else { continue }
    let value = if item.kind.is_argument() {
      item
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(item.loc))
    } else {
      lower_expr(item)
    }
    args.push(@syntax.Argument::{ value, kind: lower_argument_kind(item), })
  }
  lower_array_to_list(args)
}

///|
fn lower_tuple_expr(node : CstNode) -> @syntax.Expr {
  let exprs : Array[@syntax.Expr] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      exprs.push(lower_expr(item))
    }
  } else {
    ()
  }
  match node.kind {
    Expr_Unit =>
      @syntax.Expr::Unit(
        loc=node.loc,
        faked=node.bool_marker_value(Flag_Faked).unwrap_or(false),
      )
    Expr_Group =>
      @syntax.Expr::Group(
        expr=exprs.get(0).unwrap_or(lower_hole_expr(node.loc)),
        group=@syntax.Group::Paren,
        loc=node.loc,
      )
    Expr_Tuple =>
      @syntax.Expr::Tuple(exprs=lower_array_to_list(exprs), loc=node.loc)
    _ => lower_hole_expr(node.loc)
  }
}

///|
fn lower_constraint_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::Constraint(
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    ty=node
      .first_child_with_label("type")
      .map(lower_type)
      .unwrap_or(@syntax.Type::Any(loc=node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_array_expr_node(node : CstNode) -> @syntax.Expr {
  let elems : Array[@syntax.SpreadableElem] = []
  let is_iter = node.has_child_kind(ArrayKind_Iter)
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      elems.push(lower_array_expr_elem(item))
    }
  } else {
    ()
  }
  @syntax.make_array_expr(loc=node.loc, is_iter~, lower_array_to_list(elems))
}

///|
fn lower_array_expr_elem(item : CstNode) -> @syntax.SpreadableElem {
  match item.kind {
    ArrayElement_Spread =>
      @syntax.SpreadableElem::Spread(
        expr=item
          .first_child_with_label("expr")
          .map(lower_expr)
          .unwrap_or(lower_hole_expr(item.loc)),
        loc=item.loc,
      )
    ArrayElement_Regular =>
      @syntax.SpreadableElem::Regular(
        item
        .first_child_with_label("expr")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(item.loc)),
      )
    _ => @syntax.SpreadableElem::Regular(lower_expr(item))
  }
}

///|
fn lower_list_comprehension_expr(node : CstNode) -> @syntax.Expr {
  let is_iter = node.has_child_kind(ArrayKind_Iter)
  let kind = if node.has_child_kind(ListComprehensionKind_ForEach) {
    @syntax.ListComprehensionKind::Foreach(
      binders=lower_foreach_binders(node),
      expr=node
        .first_child_with_label("expr")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc)),
      init=lower_for_binding_list(node, "init"),
      continue_block=lower_for_binding_list(node, "continue"),
    )
  } else {
    @syntax.ListComprehensionKind::For(
      binders=lower_for_binding_list(node, "init"),
      condition=node.first_child_with_label("cond").map(lower_expr),
      continue_block=lower_for_binding_list(node, "continue"),
      for_loc=node
        .first_child_with_label("for_loc")
        .map(anchor => anchor.loc)
        .unwrap_or(node.loc),
    )
  }
  @syntax.Expr::ListComprehension(
    kind~,
    guard_=node.first_child_with_label("guard").map(lower_expr),
    body=node
      .first_child_with_label("body")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    is_iter~,
    loc=node.loc,
  )
}

///|
fn lower_record_expr_node(node : CstNode) -> @syntax.Expr {
  let fields : Array[@syntax.FieldDef] = []
  let mut record : @syntax.Expr? = None
  let mut trailing = @syntax.TrailingMark::None
  if node.first_child_with_label("body") is Some(body) {
    trailing = lower_record_trailing_mark(body)
    for item in lower_item_children(body) {
      if item.kind is Field_RecordSpread {
        record = item.first_child_with_label("expr").map(lower_expr)
      } else {
        fields.push(lower_record_field_def(item))
      }
    }
  } else {
    ()
  }
  let type_name = node
    .first_child_with_label("type")
    .map(lower_type_name_from_expr_node)
  if node.kind is Expr_RecordUpdate {
    @syntax.Expr::RecordUpdate(
      type_name~,
      record=record.unwrap_or(lower_hole_expr(node.loc)),
      fields=lower_array_to_list(fields),
      loc=node.loc,
    )
  } else {
    @syntax.Expr::Record(
      type_name~,
      fields=lower_array_to_list(fields),
      trailing~,
      loc=node.loc,
    )
  }
}

///|
fn lower_record_field_def(node : CstNode) -> @syntax.FieldDef {
  let label_node = node.first_child_with_label("name")
  let label = label_node
    .map(lower_label_from_node)
    .unwrap_or(@syntax.Label::{ name: "", loc: node.loc, })
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  @syntax.FieldDef::{
    label,
    expr,
    is_pun: node.bool_marker_value(Flag_Pun).unwrap_or(false),
    loc: node.loc,
  }
}

///|
fn lower_where_clause(node : CstNode) -> @syntax.WhereClause {
  let fields : Array[@syntax.FieldDef] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      fields.push(lower_record_field_def(item))
    }
  } else {
    ()
  }
  @syntax.WhereClause::{ fields: lower_array_to_list(fields), loc: node.loc, }
}

///|
fn lower_record_trailing_mark(node : CstNode) -> @syntax.TrailingMark {
  match
    node.first_child_of_kind(fn(kind) {
      kind is (TrailingMark_Comma | TrailingMark_Semi | TrailingMark_None)
    }) {
    Some(mark) =>
      return match mark.kind {
        TrailingMark_Comma => @syntax.TrailingMark::Comma
        TrailingMark_Semi => @syntax.TrailingMark::Semi
        _ => @syntax.TrailingMark::None
      }
    None => ()
  }
  @syntax.TrailingMark::None
}

///|
fn lower_map_expr_node(node : CstNode) -> @syntax.Expr {
  let elems : Array[@syntax.MapExprElem] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      elems.push(lower_map_expr_elem(item))
    }
  } else {
    ()
  }
  @syntax.Expr::Map(elems=lower_array_to_list(elems), loc=node.loc)
}

///|
fn lower_map_expr_elem(node : CstNode) -> @syntax.MapExprElem {
  let constant_node = node.first_child_with_label("semantic_constant")
  let (key, key_loc) = if constant_node is Some(constant_node) {
    let key = lower_constant_from_node(constant_node)
    let key_loc = if node.first_child_of_kind(fn(kind) {
        kind is Operator_UnaryMinus
      })
      is Some(operator) {
      operator.loc.merge(constant_node.loc)
    } else {
      constant_node.loc
    }
    if key is Some(key) {
      (key, key_loc)
    } else {
      (@syntax.Constant::String(""), key_loc)
    }
  } else {
    (@syntax.Constant::String(""), node.loc)
  }
  @syntax.MapExprElem::{
    key,
    expr: node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc)),
    key_loc,
    loc: node.loc,
  }
}

///|
fn lower_infix_expr(node : CstNode) -> @syntax.Expr {
  let lhs = node
    .first_child_with_label("lhs")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let rhs = node
    .first_child_with_label("rhs")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  match node.kind {
    Expr_Pipe => @syntax.Expr::Pipe(lhs~, rhs~, loc=node.loc)
    Expr_RevPipe => @syntax.Expr::RevPipe(lhs~, rhs~, loc=node.loc)
    Expr_Implies => @syntax.Expr::Implies(lhs~, rhs~, loc=node.loc)
    _ => {
      let op_node = node
        .first_child_of_kind(NodeKind::is_operator)
        .unwrap_or(node)
      let op_var = @syntax.Var::{
        name: @syntax.LongIdent::Ident(
          name=op_node.string_payload().unwrap_or(""),
        ),
        loc: op_node.loc,
      }
      @syntax.Expr::Infix(op=op_var, lhs~, rhs~, loc=node.loc)
    }
  }
}

///|
fn lower_prefix_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let operator = node.first_child_of_kind(NodeKind::is_operator).unwrap_or(node)
  let op_name = operator.string_payload().unwrap_or("")
  let op = @syntax.Var::{
    name: @syntax.LongIdent::Ident(name=op_name),
    loc: node.loc,
  }
  @syntax.Expr::Unary(op~, expr~, loc=node.loc)
}

///|
fn lower_while_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::While(
    loop_cond=node
      .first_child_with_label("cond")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    loop_body=node
      .first_child_with_label("body")
      .map(lower_function_body_expr)
      .unwrap_or(lower_unit_expr(node.loc)),
    while_else=node.first_child_with_label("else").map(lower_function_body_expr),
    label=lower_loop_label(node),
    loc=node.loc,
  )
}

///|
fn lower_assign_expr(node : CstNode) -> @syntax.Expr {
  if node.first_child_with_label("lhs") is Some(_) {
    return lower_hole_expr(node.loc)
  }
  let augmented = node.bool_marker_value(Flag_Augmented).unwrap_or(false)
  match node.kind {
    Expr_Assign => {
      guard node.first_child_with_label("semantic_name") is Some(name) else {
        return lower_hole_expr(node.loc)
      }
      let id = @syntax.Var::{
        name: lower_long_ident_from_child(node, "semantic_name", "name"),
        loc: name.loc,
      }
      let expr = node
        .first_child_with_label("rhs")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc))
      let augmented_by = if augmented {
        node.first_child_of_kind(NodeKind::is_operator).map(lower_operator_var)
      } else {
        None
      }
      @syntax.Expr::Assign(var_=id, expr~, augmented_by~, loc=node.loc)
    }
    Expr_ArraySet | Expr_ArrayAugmentedSet => {
      guard node.first_child_with_label("self") is Some(array_node) &&
        node.first_child_with_label("index") is Some(index_node) else {
        return lower_hole_expr(node.loc)
      }
      let array = lower_expr(array_node)
      let index = lower_expr(index_node)
      let value = node
        .first_child_with_label("rhs")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc))
      if node.kind is Expr_ArrayAugmentedSet || augmented {
        let op = node
          .first_child_of_kind(NodeKind::is_operator)
          .map(lower_operator_var)
          .unwrap_or(@syntax.Var::{
            name: @syntax.LongIdent::Ident(name=""),
            loc: node.loc,
          })
        @syntax.Expr::ArrayAugmentedSet(
          op~,
          array~,
          index~,
          value~,
          loc=node.loc,
        )
      } else {
        @syntax.Expr::ArraySet(array~, index~, value~, loc=node.loc)
      }
    }
    Expr_Mutate => {
      guard node.first_child_with_label("self") is Some(record_node) &&
        node.first_child_with_label("field") is Some(_) else {
        return lower_hole_expr(node.loc)
      }
      let record = lower_expr(record_node)
      let accessor = lower_accessor_child(node)
      let field = node
        .first_child_with_label("rhs")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc))
      let augmented_by = if augmented {
        node.first_child_of_kind(NodeKind::is_operator).map(lower_operator_var)
      } else {
        None
      }
      @syntax.Expr::Mutate(
        record~,
        accessor~,
        field~,
        augmented_by~,
        loc=node.loc,
      )
    }
    _ => lower_hole_expr(node.loc)
  }
}

///|
fn lower_for_binding(node : CstNode) -> (@syntax.Binder, @syntax.Expr) {
  (
    node
    .first_child_with_label("name")
    .map(lower_binder_from_node)
    .unwrap_or(lower_dummy_binder(node.loc)),
    node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_unit_expr(node.loc)),
  )
}

///|
fn lower_for_binding_list(
  node : CstNode,
  label : String,
) -> List[(@syntax.Binder, @syntax.Expr)] {
  let bindings : Array[(@syntax.Binder, @syntax.Expr)] = []
  if node.first_child_with_label(label) is Some(list) {
    for item in lower_item_children(list) {
      bindings.push(lower_for_binding(item))
    }
  } else {
    ()
  }
  lower_array_to_list(bindings)
}

///|
fn lower_for_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::For(
    binders=lower_for_binding_list(node, "init"),
    condition=node.first_child_with_label("cond").map(lower_expr),
    continue_block=lower_for_binding_list(node, "continue"),
    body=node
      .first_child_with_label("body")
      .map(lower_function_body_expr)
      .unwrap_or(lower_unit_expr(node.loc)),
    for_else=node.first_child_with_label("else").map(lower_function_body_expr),
    where_clause=node.first_child_with_label("where").map(lower_where_clause),
    label=lower_loop_label(node),
    loc=node.loc,
  )
}

///|
fn lower_foreach_binders(node : CstNode) -> List[@syntax.Binder?] {
  let binders : Array[@syntax.Binder?] = []
  if node.first_child_with_label("pattern") is Some(pattern) {
    match lower_pattern(pattern) {
      @syntax.Pattern::Var(binder) => binders.push(Some(binder))
      @syntax.Pattern::Tuple(pats~, ..) =>
        for pat in pats {
          match pat {
            @syntax.Pattern::Var(binder) => binders.push(Some(binder))
            @syntax.Pattern::Any(..) => binders.push(None)
            _ => binders.push(Some(lower_dummy_binder(pattern.loc)))
          }
        }
      @syntax.Pattern::Any(..) => binders.push(None)
      _ => binders.push(Some(lower_dummy_binder(pattern.loc)))
    }
  } else {
    ()
  }
  lower_array_to_list(binders)
}

///|
fn lower_foreach_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::ForEach(
    binders=lower_foreach_binders(node),
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    init=lower_for_binding_list(node, "init"),
    continue_block=lower_for_binding_list(node, "continue"),
    body=node
      .first_child_with_label("body")
      .map(lower_function_body_expr)
      .unwrap_or(lower_unit_expr(node.loc)),
    else_block=node.first_child_with_label("else").map(lower_function_body_expr),
    where_clause=node.first_child_with_label("where").map(lower_where_clause),
    label=lower_loop_label(node),
    loc=node.loc,
  )
}

///|
fn lower_loop_label(node : CstNode) -> @syntax.Label? {
  guard node.first_child_with_label("label") is Some(label) else { return None }
  let lowered = lower_label_from_node(label)
  Some(@syntax.Label::{
    name: lowered.name,
    loc: node
    .first_child_with_label("label_loc")
    .map(fn(anchor) { anchor.loc })
    .unwrap_or(lowered.loc),
  })
}

///|
fn lower_apply_expr(node : CstNode) -> @syntax.Expr {
  let func = node
    .first_child_with_label("func")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let args = node
    .first_child_with_label("arg")
    .map(lower_arguments)
    .unwrap_or(@list.empty())
  @syntax.Expr::Apply(func~, args~, loc=node.loc)
}

///|
fn lower_dot_apply_expr(node : CstNode) -> @syntax.Expr {
  let self_expr = node
    .first_child_with_label("self")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let method_label = node
    .first_child_with_label("field")
    .map(lower_label_from_node)
    .unwrap_or(@syntax.Label::{ name: "", loc: node.loc, })
  let args = node
    .first_child_with_label("arg")
    .map(lower_arguments)
    .unwrap_or(@list.empty())
  let return_self = node.bool_marker_value(Flag_ReturnSelf).unwrap_or(false)
  @syntax.Expr::DotApply(
    self=self_expr,
    method_name=method_label,
    args~,
    return_self~,
    loc=node.loc,
  )
}

///|
fn lower_type_name_from_expr_node(node : CstNode) -> @syntax.TypeName {
  match node.kind {
    kind if kind.is_type() => lower_type_name_from_type_node(node)
    Expr_Constr | Expr_Ident =>
      @syntax.TypeName::{
        name: lower_long_ident_from_child(node, "semantic_name", "name"),
        is_object: false,
        loc: node.loc,
      }
    _ =>
      @syntax.TypeName::{
        name: @syntax.LongIdent::dummy(),
        is_object: false,
        loc: node.loc,
      }
  }
}

///|
fn lower_method_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::Method(
    type_name=node
      .first_child_with_label("type")
      .map(lower_type_name_from_expr_node)
      .unwrap_or(@syntax.TypeName::{
        name: @syntax.LongIdent::dummy(),
        is_object: false,
        loc: node.loc,
      }),
    method_name=node
      .first_child_with_label("field")
      .map(lower_label_from_node)
      .unwrap_or(@syntax.Label::{ name: "", loc: node.loc, }),
    loc=node.loc,
  )
}

///|
fn lower_accessor_child(node : CstNode) -> @syntax.Accessor {
  if node.first_child_with_label("field") is Some(field) {
    match field.kind {
      Accessor_Index =>
        @syntax.Accessor::Index(
          tuple_index=field.int_payload().unwrap_or(0),
          loc=field.loc,
        )
      Accessor_Newtype => @syntax.Accessor::Newtype(loc=field.loc)
      _ => @syntax.Accessor::Label(lower_label_from_node(field))
    }
  } else {
    @syntax.Accessor::Label(@syntax.Label::{ name: "", loc: node.loc, })
  }
}

///|
fn lower_field_expr(node : CstNode) -> @syntax.Expr {
  let record = node
    .first_child_with_label("self")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let accessor = lower_accessor_child(node)
  @syntax.Expr::Field(record~, accessor~, loc=node.loc)
}

///|
fn lower_array_get_expr(node : CstNode) -> @syntax.Expr {
  let array = node
    .first_child_with_label("self")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let index = node
    .first_child_with_label("index")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  @syntax.Expr::ArrayGet(array~, index~, loc=node.loc)
}

///|
fn lower_array_get_slice_expr(node : CstNode) -> @syntax.Expr {
  let array = node
    .first_child_with_label("self")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let indexes = node.children_with_label("index")
  let start_index = if node.first_child_with_label("start") is Some(start) {
    Some(lower_expr(start))
  } else {
    indexes.get(0).map(lower_expr)
  }
  let end_index = if node.first_child_with_label("end") is Some(end) {
    Some(lower_expr(end))
  } else {
    indexes.get(1).map(lower_expr)
  }
  @syntax.Expr::ArrayGetSlice(
    array~,
    start_index~,
    end_index~,
    index_loc=node
      .first_child_with_label("index_loc")
      .map(fn(anchor) { anchor.loc })
      .unwrap_or(node.loc),
    loc=node.loc,
  )
}

///|
fn lower_if_expr(node : CstNode) -> @syntax.Expr {
  let cond = node
    .first_child_with_label("cond")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let bodies = node.children_with_label("body")
  let ifso = bodies
    .get(0)
    .map(lower_function_body_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let ifnot = node
    .children_with_label("else")
    .last()
    .map(lower_function_body_expr)
  @syntax.Expr::If(cond~, ifso~, ifnot~, loc=node.loc)
}

///|
fn lower_match_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let cases : Array[@syntax.Case] = []
  for case_node in node.children_with_label("case") {
    cases.push(lower_case(case_node))
  }
  @syntax.Expr::Match(
    expr~,
    cases=lower_array_to_list(cases),
    match_loc=node.loc,
    loc=node.loc,
  )
}

///|
fn lower_case(node : CstNode) -> @syntax.Case {
  @syntax.Case::{
    pattern: node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(@syntax.Pattern::Any(loc=node.loc)),
    guard_: node.first_child_with_label("cond").map(lower_expr),
    body: node
    .first_child_with_label("body")
    .map(lower_expr)
    .unwrap_or(lower_unit_expr(node.loc)),
  }
}

///|
fn lower_try_expr(node : CstNode) -> @syntax.Expr {
  let cases : Array[@syntax.Case] = []
  for case_list in node.children_with_label("case") {
    for case_node in case_list.children_with_label("case") {
      cases.push(lower_case(case_node))
    }
  }
  let else_cases : Array[@syntax.Case] = []
  for case_list in node.children_with_label("else") {
    for case_node in case_list.children_with_label("case") {
      else_cases.push(lower_case(case_node))
    }
  }
  let try_else = if else_cases.length() == 0 {
    None
  } else {
    Some(lower_array_to_list(else_cases))
  }
  let try_loc = node
    .first_child_with_label("try_loc")
    .map(fn(anchor) { anchor.loc })
    .unwrap_or(node.loc)
  let has_try = node.bool_marker_value(Flag_HasTry).unwrap_or(false)
  let catch_loc = node
    .first_child_with_label("catch_loc")
    .map(fn(anchor) { anchor.loc })
    .unwrap_or(node.loc)
  let else_loc = node
    .first_child_with_label("else_loc")
    .map(fn(anchor) { anchor.loc })
    .unwrap_or(node.loc)
  let loc = node
    .first_child_with_label("expr_loc")
    .map(fn(anchor) { anchor.loc })
    .unwrap_or(node.loc)
  @syntax.Expr::Try(
    body=node
      .first_child_with_label("body")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    catch_=lower_array_to_list(cases),
    try_else~,
    has_try~,
    try_loc~,
    catch_loc~,
    else_loc~,
    loc~,
  )
}

///|
fn lower_try_operator_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::TryOperator(
    body=node
      .first_child_with_label("body")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    kind=Exclamation,
    try_loc=node
      .first_child_with_label("try_loc")
      .map(fn(anchor) { anchor.loc })
      .unwrap_or(node.loc),
    loc=node.loc,
  )
}

///|
fn lower_lex_case_nodes(node : CstNode) -> Array[CstNode] {
  let cases : Array[CstNode] = []
  for block in node.children_with_label("case") {
    for case_node in block.children_with_label("case") {
      cases.push(case_node)
    }
  }
  cases
}

///|
fn lower_lexmatch_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let cases : Array[@syntax.LexScanCase] = []
  for case_node in lower_lex_case_nodes(node) {
    cases.push(lower_lex_scan_case(case_node))
  }
  @syntax.Expr::LexScan(
    streaming=false,
    strategy=node.first_child_with_label("name").map(lower_label_from_node),
    expr~,
    match_loc=node.loc,
    cases=lower_array_to_list(cases),
    loc=node.loc,
  )
}

///|
fn lower_lexscan_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let cases : Array[@syntax.LexScanCase] = []
  for case_node in lower_lex_case_nodes(node) {
    cases.push(lower_lex_scan_case(case_node))
  }
  @syntax.Expr::LexScan(
    streaming=true,
    strategy=node.first_child_with_label("name").map(lower_label_from_node),
    expr~,
    match_loc=node.loc,
    cases=lower_array_to_list(cases),
    loc=node.loc,
  )
}

///|
fn lower_lex_case(node : CstNode) -> @syntax.LexCase {
  let pat_node = node.first_child_with_label("pattern")
  let pat_loc = pat_node.map(fn(node) { node.loc }).unwrap_or(node.loc)
  let body = if node.first_child_with_label("body") is Some(body_node) {
    lower_expr(body_node)
  } else if pat_node
    .map(fn(pat) { pat.has_child_kind(LexTopPattern_Wildcard) })
    .unwrap_or(false) {
    lower_todo_hole_expr(node.loc)
  } else {
    lower_unit_expr(node.loc)
  }
  @syntax.LexCase::{
    pat: pat_node.map(lower_lex_top_patterns).unwrap_or(@list.empty()),
    pat_loc,
    guard_: node.first_child_with_label("cond").map(lower_expr),
    body,
  }
}

///|
fn lower_lex_scan_case(node : CstNode) -> @syntax.LexScanCase {
  let pattern_node = node.first_child_with_label("pattern")
  @syntax.LexScanCase::{
    pat: pattern_node
    .map(lower_lex_scan_case_pattern)
    .unwrap_or(@syntax.LexScanCasePattern::Wildcard(loc=node.loc)),
    bindings: lower_regex_match_bindings(pattern_node),
    body: node
    .first_child_with_label("body")
    .map(lower_expr)
    .unwrap_or(lower_unit_expr(node.loc)),
  }
}

///|
fn lower_pattern_tuple_items(node : CstNode) -> Array[CstNode] {
  if node.first_child_with_label("body") is Some(body) {
    lower_item_children(body)
  } else {
    []
  }
}

///|
fn lower_pattern_tuple_has_separator(node : CstNode) -> Bool {
  if node.first_child_with_label("body") is Some(body) {
    for i in 0..<body.children.length() {
      match body.children.get(i) {
        Some((Some("sep"), _)) => return true
        _ => ()
      }
    }
    false
  } else {
    false
  }
}

///|
fn lower_lex_top_patterns(node : CstNode) -> List[@syntax.LexTopPattern] {
  let pats : Array[@syntax.LexTopPattern] = []
  match node.kind {
    Pattern_Tuple if !node.has_child_kind(Pattern_Group) =>
      lower_lex_top_pattern_groups(node).each(fn(pat) { pats.push(pat) })
    _ => pats.push(lower_lex_top_pattern(node))
  }
  lower_array_to_list(pats)
}

///|
fn lower_lex_top_pattern_groups(node : CstNode) -> Array[@syntax.LexTopPattern] {
  let pats : Array[@syntax.LexTopPattern] = []
  if node.first_child_with_label("body") is Some(body) {
    let mut group : Array[CstNode] = []
    for i in 0..<body.children.length() {
      match body.children.get(i) {
        Some((Some("item"), item)) => group.push(item)
        Some((Some("sep"), _)) => {
          pats.push(lower_lex_top_pattern_group(group, node.loc))
          group = []
        }
        _ => ()
      }
    }
    if group.length() > 0 {
      pats.push(lower_lex_top_pattern_group(group, node.loc))
    }
  } else {
    ()
  }
  pats
}

///|
fn lower_lex_top_pattern_group(
  group : Array[CstNode],
  loc : Location,
) -> @syntax.LexTopPattern {
  if group.length() == 0 {
    @syntax.LexTopPattern::Wildcard(loc~)
  } else if group.length() == 1 {
    lower_lex_top_pattern(group[0])
  } else {
    let pats : Array[@syntax.LexPattern] = []
    for item in group {
      pats.push(lower_lex_pattern(item))
    }
    @syntax.LexTopPattern::Pattern(
      @syntax.LexPattern::Sequence(pats=lower_array_to_list(pats), loc~),
    )
  }
}

///|
fn lower_lex_top_pattern(node : CstNode) -> @syntax.LexTopPattern {
  match node.first_child_of_kind(NodeKind::is_lex_top_pattern) {
    Some(marker) =>
      match marker.kind {
        LexTopPattern_Wildcard => @syntax.LexTopPattern::Wildcard(loc=node.loc)
        LexTopPattern_Binder =>
          @syntax.LexTopPattern::Binder(
            node
            .first_child_with_label("semantic_name")
            .map(lower_binder_from_node)
            .unwrap_or(lower_dummy_binder(node.loc)),
          )
        LexTopPattern_Pattern =>
          @syntax.LexTopPattern::Pattern(lower_lex_pattern(node))
        _ => @syntax.LexTopPattern::Wildcard(loc=node.loc)
      }
    None => @syntax.LexTopPattern::Wildcard(loc=node.loc)
  }
}

///|
fn lower_todo_hole_expr(loc : Location) -> @syntax.Expr {
  @syntax.Expr::Hole(loc~, kind=@syntax.Hole::Todo)
}

///|
fn lower_lex_pattern(node : CstNode) -> @syntax.LexPattern {
  match node.kind {
    Pattern_Constant | LexPattern_Literal => lower_lex_literal_pattern(node)
    LexPattern_RegexInterp =>
      @syntax.LexPattern::RegexInterp(
        elems=lower_interp_elems(node),
        loc=node.loc,
      )
    Pattern_Alias | LexPattern_Alias => {
      let pat = node
        .first_child_with_label("pattern")
        .map(lower_lex_pattern)
        .unwrap_or(lower_lex_empty_regex(node.loc))
      let binder = node
        .first_child_with_label("name")
        .map(lower_binder_from_node)
        .unwrap_or(lower_dummy_binder(node.loc))
      @syntax.LexPattern::Alias(pat~, binder~, loc=node.loc)
    }
    Pattern_Tuple | LexPattern_Sequence => {
      let items = lower_pattern_tuple_items(node)
      if items.length() == 0 {
        lower_lex_empty_regex(node.loc)
      } else if items.length() == 1 {
        lower_lex_pattern(items[0])
      } else {
        let pats : Array[@syntax.LexPattern] = []
        for item in items {
          pats.push(lower_lex_pattern(item))
        }
        @syntax.LexPattern::Sequence(
          pats=lower_array_to_list(pats),
          loc=node
            .first_child_with_label("handrolled_loc")
            .map(fn(node) { node.loc })
            .unwrap_or(node.loc),
        )
      }
    }
    Pattern_Constr | LexPattern_ConstantRef =>
      @syntax.LexPattern::ConstantRef(
        lid=lower_long_ident_from_child(node, "semantic_name", "name"),
        loc=node.loc,
      )
    Pattern_Var =>
      @syntax.LexPattern::ConstantRef(
        lid=@syntax.LongIdent::Ident(
          name=if node.first_child_with_label("name") is Some(name_node) {
            lower_semantic_string(name_node).unwrap_or("")
          } else {
            ""
          },
        ),
        loc=node.loc,
      )
    _ => lower_lex_empty_regex(node.loc)
  }
}

///|
fn lower_lex_empty_regex(loc : Location) -> @syntax.LexPattern {
  @syntax.LexPattern::Regex(lit="", offset=1, loc~)
}

///|
fn lower_lex_literal_pattern(node : CstNode) -> @syntax.LexPattern {
  let literal = node.first_child_of_kind(NodeKind::is_lex_literal)
  let constant = node.first_child_with_label("semantic_constant")
  match (literal, constant) {
    (Some(literal), _) if literal.kind is LexLiteral_Regex =>
      @syntax.LexPattern::Regex(
        lit=literal.string_payload().unwrap_or(""),
        offset=3,
        loc=node.loc,
      )
    (Some(literal), _) if literal.kind is LexLiteral_String =>
      @syntax.LexPattern::Regex(
        lit=literal.string_payload().unwrap_or(""),
        offset=1,
        loc=node.loc,
      )
    (_, Some(constant)) if constant.kind is Constant_Regex =>
      @syntax.LexPattern::Regex(
        lit=constant.string_payload().unwrap_or(""),
        offset=3,
        loc=node.loc,
      )
    (_, Some(constant)) if constant.kind is Constant_String =>
      @syntax.LexPattern::Regex(
        lit=constant.string_payload().unwrap_or(""),
        offset=1,
        loc=node.loc,
      )
    _ if node.first_child_with_label("semantic_name") is Some(name) =>
      @syntax.LexPattern::ConstantRef(
        lid=lower_long_ident_node(name),
        loc=node.loc,
      )
    _ => lower_lex_empty_regex(node.loc)
  }
}

///|
fn lower_regex_pattern(node : CstNode) -> @syntax.RegexPattern {
  match node.kind {
    RegexPattern_Placeholder =>
      node
      .first_child_with_label("pattern")
      .map(lower_regex_pattern)
      .unwrap_or(lower_empty_regex_pattern(node.loc))
    RegexPattern_Literal | Pattern_Constant => lower_regex_literal_pattern(node)
    RegexPattern_Alias | Pattern_Alias => {
      let pat = node
        .first_child_with_label("pattern")
        .map(lower_regex_pattern)
        .unwrap_or(lower_empty_regex_pattern(node.loc))
      let binder = node
        .first_child_with_label("name")
        .map(lower_binder_from_node)
        .unwrap_or(lower_dummy_binder(node.loc))
      @syntax.RegexPattern::Alias(pat~, binder~, loc=node.loc)
    }
    RegexPattern_Sequence | Pattern_Tuple =>
      match
        (node.first_child_with_label("lhs"), node.first_child_with_label("rhs")) {
        (Some(lhs), Some(rhs)) =>
          @syntax.RegexPattern::Sequence(
            pat1=lower_regex_pattern(lhs),
            pat2=lower_regex_pattern(rhs),
            loc=node.loc,
          )
        _ => {
          let items = lower_pattern_tuple_items(node)
          if items.length() == 0 {
            lower_empty_regex_pattern(node.loc)
          } else {
            let mut pat = lower_regex_pattern(items[0])
            for i in 1..<items.length() {
              pat = @syntax.RegexPattern::Sequence(
                pat1=pat,
                pat2=lower_regex_pattern(items[i]),
                loc=node.loc,
              )
            }
            pat
          }
        }
      }
    RegexPattern_Alternation => {
      let lhs = node
        .first_child_with_label("lhs")
        .map(lower_regex_pattern)
        .unwrap_or(lower_empty_regex_pattern(node.loc))
      let rhs = node
        .first_child_with_label("rhs")
        .map(lower_regex_pattern)
        .unwrap_or(lower_empty_regex_pattern(node.loc))
      @syntax.RegexPattern::Alternation(pat1=lhs, pat2=rhs, loc=node.loc)
    }
    RegexPattern_Reference | Pattern_Constr | Pattern_Var =>
      @syntax.RegexPattern::Reference(
        lid=lower_long_ident_from_child(node, "semantic_name", "name"),
        loc=node.loc,
      )
    _ => lower_empty_regex_pattern(node.loc)
  }
}

///|
fn lower_regex_match_bindings(
  pattern_node : CstNode?,
) -> List[(@syntax.Label, @syntax.Binder?)] {
  let bindings : Array[(@syntax.Label, @syntax.Binder?)] = []
  if pattern_node is Some(node) {
    if node.first_child_with_label("bindings") is Some(list) {
      for item in lower_item_children(list) {
        bindings.push(lower_regex_match_binding(item))
      }
    } else {
      ()
    }
  } else {
    ()
  }
  lower_array_to_list(bindings)
}

///|
fn lower_regex_match_binding(
  node : CstNode,
) -> (@syntax.Label, @syntax.Binder?) {
  let label = node
    .first_child_with_label("name")
    .map(lower_label_from_node)
    .unwrap_or(@syntax.Label::{ name: "", loc: node.loc, })
  let binder = node.first_child_with_label("binder").map(lower_binder_from_node)
  (label, binder)
}

///|
fn lower_empty_regex_pattern(loc : Location) -> @syntax.RegexPattern {
  @syntax.RegexPattern::Literal(lit="", recovered_=false, loc~)
}

///|
fn lower_lex_scan_case_pattern(node : CstNode) -> @syntax.LexScanCasePattern {
  match node.first_child_of_kind(NodeKind::is_lex_scan_case_pattern) {
    Some(marker) =>
      match marker.kind {
        LexScanCasePattern_Wildcard =>
          @syntax.LexScanCasePattern::Wildcard(loc=node.loc)
        LexScanCasePattern_Binder =>
          @syntax.LexScanCasePattern::Binder(
            node
            .first_child_with_label("semantic_name")
            .map(lower_binder_from_node)
            .unwrap_or(lower_dummy_binder(node.loc)),
          )
        LexScanCasePattern_Pattern =>
          @syntax.LexScanCasePattern::Pattern(lower_regex_pattern(node))
        _ => @syntax.LexScanCasePattern::Wildcard(loc=node.loc)
      }
    None => @syntax.LexScanCasePattern::Wildcard(loc=node.loc)
  }
}

///|
fn lower_regex_match_expr(node : CstNode) -> @syntax.Expr {
  let pattern_node = node.first_child_with_label("pattern")
  @syntax.Expr::RegexMatch(
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    pat=pattern_node
      .map(lower_regex_pattern)
      .unwrap_or(lower_empty_regex_pattern(node.loc)),
    bindings=lower_regex_match_bindings(pattern_node),
    loc=node.loc,
  )
}

///|
fn lower_regex_literal_pattern(node : CstNode) -> @syntax.RegexPattern {
  let literal = node.first_child_of_kind(NodeKind::is_lex_literal)
  let constant = node.first_child_with_label("semantic_constant")
  match (literal, constant) {
    (Some(literal), _) if literal.kind is LexLiteral_String =>
      @syntax.RegexPattern::Literal(
        lit=literal.string_payload().unwrap_or(""),
        recovered_=true,
        loc=node.loc,
      )
    (Some(literal), _) =>
      @syntax.RegexPattern::Literal(
        lit=literal.string_payload().unwrap_or(""),
        recovered_=false,
        loc=node.loc,
      )
    (_, Some(constant)) if constant.kind is Constant_String =>
      @syntax.RegexPattern::Literal(
        lit=constant.string_payload().unwrap_or(""),
        recovered_=true,
        loc=node.loc,
      )
    (_, Some(constant)) if constant.kind is Constant_Regex =>
      @syntax.RegexPattern::Literal(
        lit=constant.string_payload().unwrap_or(""),
        recovered_=false,
        loc=node.loc,
      )
    _ if node.first_child_with_label("semantic_name") is Some(name) =>
      @syntax.RegexPattern::Reference(
        lid=lower_long_ident_node(name),
        loc=node.loc,
      )
    _ => lower_empty_regex_pattern(node.loc)
  }
}

///|
fn lower_parameter(node : CstNode) -> @syntax.Parameter {
  let pat = node.first_child_with_label("pattern")
  let ty = node.first_child_with_label("type").map(lower_type)
  let pat_loc = pat.map(fn(pat) { pat.loc }).unwrap_or(node.loc)
  let binder = if pat is Some(pat_node) {
    pat_node
    .first_child_with_label("name")
    .map(lower_post_label_binder_from_node)
    .unwrap_or(lower_dummy_binder(pat_node.loc))
  } else {
    lower_dummy_binder(node.loc)
  }
  match node.kind {
    Parameter_DiscardPositional =>
      @syntax.Parameter::DiscardPositional(ty~, loc=pat_loc)
    Parameter_Labelled => @syntax.Parameter::Labelled(binder~, ty~)
    Parameter_Optional =>
      @syntax.Parameter::Optional(
        binder~,
        default=node
          .first_child_with_label("default")
          .map(lower_expr)
          .unwrap_or(lower_unit_expr(node.loc)),
        ty~,
      )
    Parameter_QuestionOptional =>
      @syntax.Parameter::QuestionOptional(binder~, ty~)
    Parameter_Positional => @syntax.Parameter::Positional(binder~, ty~)
    _ => @syntax.Parameter::DiscardPositional(ty~, loc=pat_loc)
  }
}

///|
fn lower_parameters_from_list(
  list : CstNode?,
) -> (List[@syntax.Parameter], Location) {
  lower_parameters_from_list_with_discard_loc(list, None)
}

///|
fn lower_parameters_from_list_with_discard_loc(
  list : CstNode?,
  first_discard_loc : Location?,
) -> (List[@syntax.Parameter], Location) {
  if list is Some(list) {
    let params : Array[@syntax.Parameter] = []
    let mut item_index = 0
    for item in lower_item_children(list) {
      let parameter = lower_parameter(item)
      let parameter = match (item_index, first_discard_loc, parameter) {
        (0, Some(loc), @syntax.Parameter::DiscardPositional(ty~, loc=_)) =>
          @syntax.Parameter::DiscardPositional(ty~, loc~)
        (_, _, parameter) => parameter
      }
      params.push(parameter)
      item_index += 1
    }
    (lower_array_to_list(params), list.loc)
  } else {
    (@list.empty(), lower_empty_loc())
  }
}

///|
fn lower_optional_parameters_from_list(
  list : CstNode?,
) -> (List[@syntax.Parameter]?, Location) {
  if list is Some(_) {
    let (params, loc) = lower_parameters_from_list(list)
    (Some(params), loc)
  } else {
    (None, lower_empty_loc())
  }
}

///|
fn lower_return_type_from_clause(node : CstNode?) -> @syntax.Type? {
  if node is Some(clause) {
    if clause.first_child_with_label("type") is Some(ty_node) {
      let ty = lower_type(ty_node)
      let outer_error = clause.first_child_with_label("clause")
      let outer_nocancel = clause.first_child_with_label("nocancel")
      match ty {
        @syntax.Type::Arrow(args~, res~, err~, is_async~, nocancel_~, loc~) if ty_node.kind
          is Type_Arrow &&
          (outer_error is Some(_) || outer_nocancel is Some(_)) => {
          let err = match outer_error {
            Some(error_clause) => lower_error_annotation(error_clause)
            None => err
          }
          let nocancel_ = match outer_nocancel {
            Some(anchor) => Some(anchor.loc)
            None => nocancel_
          }
          let loc = match (outer_error, outer_nocancel) {
            (Some(error_clause), Some(anchor)) =>
              loc.merge(error_clause.loc).merge(anchor.loc)
            (Some(error_clause), None) => loc.merge(error_clause.loc)
            (None, Some(anchor)) => loc.merge(anchor.loc)
            (None, None) => loc
          }
          Some(
            @syntax.Type::Arrow(args~, res~, err~, is_async~, nocancel_~, loc~),
          )
        }
        _ => Some(ty)
      }
    } else {
      None
    }
  } else {
    None
  }
}

///|
fn lower_function_expr_node(node : CstNode) -> @syntax.Expr {
  let params_node = node.first_child_with_label("params")
  let (parameters, params_loc) = lower_parameters_from_list_with_discard_loc(
    params_node,
    lower_pipe_arrow_discard_loc(node, params_node),
  )
  let body = if node.first_child_with_label("body") is Some(body_node) {
    if arrow_lambda_block_keeps_group(node, body_node) {
      lower_expr(body_node)
    } else if node.first_child_with_label("pipe") is Some(_) ||
      function_body_block_can_unwrap(body_node) {
      lower_function_body_expr(body_node)
    } else {
      lower_expr(body_node)
    }
  } else {
    lower_unit_expr(node.loc)
  }
  let return_type = lower_return_type_from_clause(
    node.first_child_with_label("type"),
  )
  let kind = if node.has_child_kind(FnKind_Arrow) {
    @syntax.FnKind::Arrow
  } else {
    @syntax.FnKind::Lambda
  }
  let func = @syntax.Func::{
    parameters,
    params_loc,
    body,
    return_type,
    error_type: lower_error_type_from_clause(
      node.first_child_with_label("type"),
    ),
    kind,
    is_async: lower_async_loc(node),
    nocancel_: lower_nocancel_from_clause(node.first_child_with_label("type")),
    loc: node.loc,
  }
  @syntax.Expr::Function(func~, loc=node.loc)
}

///|
fn lower_pipe_arrow_discard_loc(
  node : CstNode,
  _params_node : CstNode?,
) -> Location? {
  node.first_child_with_label("params_loc").map(fn(anchor) { anchor.loc })
}

///|
fn arrow_lambda_block_keeps_group(node : CstNode, body_node : CstNode) -> Bool {
  guard body_node.kind is Expr_Block else { return false }
  if node.first_child_with_label("pipe") is Some(_) {
    return false
  }
  node.has_child_kind(FnKind_Arrow)
}

///|
fn function_body_block_can_unwrap(node : CstNode) -> Bool {
  match node.kind {
    Expr_Block => true
    _ => false
  }
}

///|
fn lower_let_expr_without_body(node : CstNode) -> @syntax.Expr {
  lower_let_expr_with_body(node, lower_unit_expr(node.loc))
}

///|
fn lower_let_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  let base_pat = node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(@syntax.Pattern::Any(loc=node.loc))
  let pat = match node.kind {
    Expr_Let =>
      if node.first_child_with_label("type") is Some(type_node) {
        let ty = lower_type(type_node)
        let loc = base_pat.loc().merge(ty.loc())
        @syntax.Pattern::Constraint(pat=base_pat, ty~, loc~)
      } else {
        base_pat
      }
    _ => base_pat
  }
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_unit_expr(node.loc))
  match node.kind {
    Expr_LetMut => {
      let binder = match pat {
        @syntax.Pattern::Var(binder) => binder
        _ => lower_dummy_binder(pat.loc())
      }
      @syntax.Expr::LetMut(
        binder~,
        ty=node.first_child_with_label("type").map(lower_type),
        expr~,
        body~,
        loc=node.loc.merge(body.loc()),
      )
    }
    _ =>
      @syntax.Expr::Let(
        pattern=pat,
        expr~,
        body~,
        loc=node.loc.merge(body.loc()),
      )
  }
}

///|
fn lower_func_from_decl_like_node(node : CstNode) -> @syntax.Func {
  let (parameters, params_loc) = lower_parameters_from_list(
    node.first_child_with_label("params"),
  )
  let body = node
    .first_child_with_label("body")
    .map(lower_function_body_expr)
    .unwrap_or(lower_unit_expr(node.loc))
  let loc = node
    .first_child_with_label("header_loc")
    .map(fn(anchor) { anchor.loc })
    .unwrap_or(node.loc)
  @syntax.Func::{
    parameters,
    params_loc,
    body,
    return_type: lower_return_type_from_clause(
      node.first_child_with_label("type"),
    ),
    error_type: lower_error_type_from_clause(
      node.first_child_with_label("type"),
    ),
    kind: @syntax.FnKind::Lambda,
    is_async: lower_async_loc(node),
    nocancel_: lower_nocancel_from_clause(node.first_child_with_label("type")),
    loc,
  }
}

///|
fn lower_func_from_letrec_binding(node : CstNode) -> @syntax.Func {
  match node.first_child_with_label("expr").map(lower_expr) {
    Some(@syntax.Expr::Function(func~, ..)) => func
    Some(expr) =>
      @syntax.Func::{
        parameters: @list.empty(),
        params_loc: lower_empty_loc(),
        body: expr,
        return_type: None,
        error_type: @syntax.ErrorType::NoErrorType,
        kind: @syntax.FnKind::Lambda,
        is_async: None,
        nocancel_: None,
        loc: node.loc,
      }
    None => lower_func_from_decl_like_node(node)
  }
}

///|
fn lower_letfn_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  @syntax.Expr::LetFn(
    name=node
      .first_child_with_label("name")
      .map(lower_binder_from_node)
      .unwrap_or(lower_dummy_binder(node.loc)),
    func=lower_func_from_decl_like_node(node),
    body~,
    loc=node.loc.merge(body.loc()),
  )
}

///|
fn lower_letrec_bindings(
  node : CstNode,
) -> List[(@syntax.Binder, @syntax.Type?, @syntax.Func)] {
  let bindings : Array[(@syntax.Binder, @syntax.Type?, @syntax.Func)] = []
  for binding in node.children_with_label("binding") {
    let binding_type = binding
      .first_child_with_label("binding_type")
      .map(lower_type)
    bindings.push(
      (
        binding
        .first_child_with_label("name")
        .map(lower_binder_from_node)
        .unwrap_or(lower_dummy_binder(binding.loc)),
        binding_type,
        lower_func_from_letrec_binding(binding),
      ),
    )
  }
  lower_array_to_list(bindings)
}

///|
fn lower_letrec_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  @syntax.Expr::LetAnd(
    bindings=lower_letrec_bindings(node),
    body~,
    loc=node.loc.merge(body.loc()),
  )
}

///|
fn lower_guard_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  @syntax.Expr::Guard(
    cond=node
      .first_child_with_label("cond")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    otherwise=node.first_child_with_label("else").map(lower_function_body_expr),
    body~,
    exclamation=node.bool_marker_value(Flag_Exclamation).unwrap_or(false),
    guard_loc=node
      .first_child_with_label("kw")
      .map(anchor => anchor.loc)
      .unwrap_or(node.loc),
    loc=node.loc.merge(body.loc()),
  )
}

///|
fn lower_defer_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let loc = node.loc.merge(body.loc())
  if node.kind is Expr_ErrDefer {
    @syntax.Expr::ErrDefer(expr~, body~, loc~)
  } else {
    @syntax.Expr::Defer(expr~, body~, loc~)
  }
}

///|
fn lower_proof_assert_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::ProofAssert(
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_proof_let_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::ProofLet(
    binder=node
      .first_child_with_label("name")
      .map(lower_binder_from_node)
      .unwrap_or(lower_dummy_binder(node.loc)),
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_statement_sequence(
  items : Array[CstNode],
  index : Int,
  loc : Location,
) -> @syntax.Expr {
  if index >= items.length() {
    return lower_faked_unit_expr(loc)
  }
  let first = items[index]
  if index == items.length() - 1 {
    return lower_statement_tail_expr(first)
  }
  match first.kind {
    Expr_Let | Expr_LetMut => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_let_expr_with_body(first, body)
    }
    Expr_LetFn => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_letfn_expr_with_body(first, body)
    }
    Expr_LetAnd => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_letrec_expr_with_body(first, body)
    }
    Expr_Guard => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_guard_expr_with_body(first, body)
    }
    Expr_Defer | Expr_ErrDefer => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_defer_expr_with_body(first, body)
    }
    _ => {
      let prefix : Array[@syntax.Expr] = []
      let mut i = index
      while i < items.length() - 1 {
        match items[i].kind {
          Expr_Let | Expr_LetMut => {
            let body = lower_statement_sequence(items, i, loc)
            return if prefix.length() == 0 {
              body
            } else {
              lower_sequence_expr(prefix, body)
            }
          }
          Expr_LetFn => {
            let body = lower_statement_sequence(items, i + 1, loc)
            let letfn = lower_letfn_expr_with_body(items[i], body)
            return if prefix.length() == 0 {
              letfn
            } else {
              lower_sequence_expr(prefix, letfn)
            }
          }
          Expr_LetAnd => {
            let body = lower_statement_sequence(items, i + 1, loc)
            let letrec_expr = lower_letrec_expr_with_body(items[i], body)
            return if prefix.length() == 0 {
              letrec_expr
            } else {
              lower_sequence_expr(prefix, letrec_expr)
            }
          }
          Expr_Guard => {
            let body = lower_statement_sequence(items, i + 1, loc)
            let guard_expr = lower_guard_expr_with_body(items[i], body)
            return if prefix.length() == 0 {
              guard_expr
            } else {
              lower_sequence_expr(prefix, guard_expr)
            }
          }
          Expr_Defer | Expr_ErrDefer => {
            let body = lower_statement_sequence(items, i + 1, loc)
            let defer_expr = lower_defer_expr_with_body(items[i], body)
            return if prefix.length() == 0 {
              defer_expr
            } else {
              lower_sequence_expr(prefix, defer_expr)
            }
          }
          _ => {
            prefix.push(lower_expr(items[i]))
            i += 1
          }
        }
      }
      lower_sequence_expr(
        prefix,
        lower_statement_tail_expr(items[items.length() - 1]),
      )
    }
  }
}

///|
fn lower_statement_items(node : CstNode) -> Array[CstNode] {
  if node.kind is Expr_Sequence {
    node.first_child_with_label("body").map(lower_item_children).unwrap_or([])
  } else {
    lower_item_children(node)
  }
}

///|
fn lower_sequence_cst_node(node : CstNode) -> @syntax.Expr {
  lower_statement_sequence(lower_statement_items(node), 0, node.loc)
}

///|
fn lower_sequence_expr(
  exprs : Array[@syntax.Expr],
  last_expr : @syntax.Expr,
) -> @syntax.Expr {
  let loc = if exprs.get(0) is Some(first) {
    first.loc().merge(last_expr.loc())
  } else {
    last_expr.loc()
  }
  @syntax.Expr::Sequence(exprs=lower_array_to_list(exprs), last_expr~, loc~)
}

///|
fn lower_statement_tail_expr(item : CstNode) -> @syntax.Expr {
  match item.kind {
    Expr_Let | Expr_LetMut =>
      lower_let_expr_with_body(item, lower_faked_unit_expr(item.loc))
    Expr_Guard =>
      lower_guard_expr_with_body(item, lower_faked_unit_expr(item.loc))
    Expr_Defer | Expr_ErrDefer =>
      lower_defer_expr_with_body(item, lower_faked_unit_expr(item.loc))
    _ => lower_expr(item)
  }
}

///|
fn lower_block_expr_node(node : CstNode) -> @syntax.Expr {
  let expr = if node.first_child_with_label("body") is Some(body) {
    lower_statement_sequence(lower_statement_items(body), 0, node.loc)
  } else {
    lower_unit_expr(node.loc)
  }
  @syntax.Expr::Group(expr~, group=@syntax.Group::Brace, loc=node.loc)
}

///|
fn lower_function_body_expr(node : CstNode) -> @syntax.Expr {
  match node.kind {
    Expr_Block =>
      if node.first_child_with_label("body") is Some(body) {
        lower_statement_sequence(lower_statement_items(body), 0, node.loc)
      } else {
        lower_unit_expr(node.loc)
      }
    _ => lower_expr(node)
  }
}

///|
fn lower_return_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::Return(
    return_value=node.first_child_with_label("expr").map(lower_expr),
    loc=node.loc,
  )
}

///|
fn lower_raise_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::Raise(
    err_value=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_break_expr(node : CstNode) -> @syntax.Expr {
  @syntax.Expr::Break(
    arg=node.first_child_with_label("expr").map(lower_expr),
    label=lower_control_label(node),
    loc=node.loc,
  )
}

///|
fn lower_continue_expr(node : CstNode) -> @syntax.Expr {
  let args : Array[@syntax.Expr] = []
  if node.first_child_with_label("arg") is Some(list) {
    for item in lower_item_children(list) {
      args.push(lower_expr(item))
    }
  } else if node.first_child_with_label("expr") is Some(expr) {
    args.push(lower_expr(expr))
  } else {
    ()
  }
  let label = lower_control_label(node)
  @syntax.Expr::Continue(args=lower_array_to_list(args), label~, loc=node.loc)
}

///|
fn lower_labelled_block_expr(node : CstNode) -> @syntax.Expr {
  LabelledBlock(
    label=node
      .first_child_with_label("label")
      .map(lower_label_from_node)
      .unwrap_or({ name: "", loc: node.loc, }),
    body=node
      .first_child_with_label("body")
      .map(lower_function_body_expr)
      .unwrap_or(lower_unit_expr(node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_control_label(node : CstNode) -> @syntax.Label? {
  if node.first_child_with_label("label") is Some(label_node) {
    let label = lower_label_from_node(label_node)
    Some(@syntax.Label::{
      name: label.name,
      loc: node
      .first_child_with_label("label_loc")
      .map(fn(node) { node.loc })
      .unwrap_or(label.loc),
    })
  } else {
    None
  }
}

///|
fn lower_as_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let trait_ = node
    .first_child_with_label("type")
    .map(lower_type_name_from_type_node)
    .unwrap_or(@syntax.TypeName::{
      name: @syntax.LongIdent::dummy(),
      is_object: false,
      loc: node.loc,
    })
  @syntax.Expr::As(expr~, trait_~, loc=node.loc)
}

///|
fn lower_is_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let pat = node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(@syntax.Pattern::Any(loc=node.loc))
  @syntax.Expr::Is(expr~, pat~, loc=node.loc)
}

///|
fn lower_pattern(node : CstNode) -> @syntax.Pattern {
  match node.kind {
    Pattern_Var =>
      if node.first_child_with_label("name") is Some(name) {
        @syntax.Pattern::Var(lower_binder_from_node(name))
      } else {
        @syntax.Pattern::Any(loc=node.loc)
      }
    Pattern_Any | Pattern_MissingPattern => @syntax.Pattern::Any(loc=node.loc)
    Pattern_Constant =>
      if lower_constant_from_node(node) is Some(c) {
        let loc = if node.has_child_kind(Operator_UnaryMinus) {
          node
          .first_child_with_label("semantic_constant")
          .map(constant => constant.loc)
          .unwrap_or(node.loc)
        } else {
          node.loc
        }
        @syntax.Pattern::Constant(c~, loc~)
      } else {
        @syntax.Pattern::Any(loc=node.loc)
      }
    Pattern_Tuple => lower_tuple_pattern(node)
    Pattern_Array =>
      @syntax.Pattern::Array(pats=lower_array_patterns(node), loc=node.loc)
    Pattern_Record =>
      @syntax.Pattern::Record(
        fields=lower_record_pattern_fields(node),
        is_closed=lower_record_pattern_is_closed(node),
        loc=node.loc,
      )
    Pattern_Map =>
      @syntax.Pattern::Map(
        elems=lower_map_pattern_elems(node),
        is_closed=lower_map_pattern_is_closed(node),
        loc=node.loc,
      )
    Pattern_Constr => lower_constructor_pattern(node)
    Pattern_ConstrAny =>
      @syntax.Pattern::ConstrAny(
        type_name=node
          .first_child_with_label("type")
          .map(lower_type_name_from_type_node)
          .unwrap_or(@syntax.TypeName::{
            name: @syntax.LongIdent::dummy(),
            is_object: false,
            loc: node.loc,
          }),
        loc=node.loc,
      )
    Pattern_SpecialConstr => lower_special_constructor_pattern(node)
    Pattern_Or => {
      let lhs = node
        .first_child_with_label("lhs")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc))
      let rhs = node
        .first_child_with_label("rhs")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc))
      @syntax.Pattern::Or(pat1=lhs, pat2=rhs, loc=node.loc)
    }
    Pattern_Range => {
      let lhs = node
        .first_child_with_label("lhs")
        .map(lower_pattern)
        .unwrap_or(@syntax.Pattern::Any(loc=node.loc))
      let rhs = node
        .first_child_with_label("rhs")
        .map(lower_pattern)
        .unwrap_or(@syntax.Pattern::Any(loc=node.loc))
      @syntax.Pattern::Range(
        lhs~,
        rhs~,
        kind=lower_pattern_range_kind(node),
        loc=node.loc,
      )
    }
    Pattern_Alias => {
      let pat = node
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(@syntax.Pattern::Any(loc=node.loc))
      let binder = node
        .first_child_with_label("name")
        .map(lower_binder_from_node)
        .unwrap_or(lower_dummy_binder(node.loc))
      @syntax.Pattern::Alias(pat~, alias_=binder, loc=node.loc)
    }
    Pattern_Constraint => {
      let pat = node
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(@syntax.Pattern::Any(loc=node.loc))
      let ty = node
        .first_child_with_label("type")
        .map(lower_type)
        .unwrap_or(@syntax.Type::Any(loc=node.loc))
      @syntax.Pattern::Constraint(pat~, ty~, loc=node.loc)
    }
    Pattern_With => {
      let pat = node
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc))
      let bindings : Array[(@syntax.Binder, @syntax.Expr)] = []
      if node.first_child_with_label("body") is Some(body) {
        for binding in lower_item_children(body) {
          bindings.push(
            (
              binding
              .first_child_with_label("name")
              .map(lower_binder_from_node)
              .unwrap_or(lower_dummy_binder(binding.loc)),
              binding
              .first_child_with_label("expr")
              .map(lower_expr)
              .unwrap_or(lower_hole_expr(binding.loc)),
            ),
          )
        }
      }
      With(pat~, bindings=lower_array_to_list(bindings), loc=node.loc)
    }
    _ => @syntax.Pattern::Any(loc=node.loc)
  }
}

///|
fn lower_pattern_range_kind(node : CstNode) -> @syntax.RangeKind {
  match
    node.first_child_of_kind(fn(kind) {
      kind is (RangeKind_Inclusive | RangeKind_Exclusive)
    }) {
    Some(kind_node) =>
      return match kind_node.kind {
        RangeKind_Exclusive => @syntax.RangeKind::Exclusive
        RangeKind_Inclusive => @syntax.RangeKind::Inclusive
        _ => @syntax.RangeKind::InclusiveMissingEqual
      }
    None => ()
  }
  @syntax.RangeKind::InclusiveMissingEqual
}

///|
fn lower_array_patterns(node : CstNode) -> @syntax.ArrayPatterns {
  let prefix : Array[@syntax.ArrayPattern] = []
  let suffix : Array[@syntax.ArrayPattern] = []
  let mut rest : @syntax.DotDotBinder? = None
  if node.first_child_with_label("body") is Some(patterns) {
    let body = patterns.first_child_with_label("body").unwrap_or(patterns)
    for item in lower_item_children(body) {
      match item.kind {
        Pattern_Rest if rest is None => rest = Some(lower_dot_dot_binder(item))
        _ =>
          if rest is Some(_) {
            suffix.push(lower_array_pattern_item(item))
          } else {
            prefix.push(lower_array_pattern_item(item))
          }
      }
    }
  } else {
    ()
  }
  if node.first_child_with_label("body").map(patterns => patterns.kind)
    is Some(ArrayPatterns_Open) &&
    rest is Some(dot_dot_binder) {
    @syntax.ArrayPatterns::Open(
      lower_array_to_list(prefix),
      lower_array_to_list(suffix),
      dot_dot_binder,
    )
  } else {
    @syntax.ArrayPatterns::Closed(lower_array_to_list(prefix))
  }
}

///|
fn lower_array_pattern_item(node : CstNode) -> @syntax.ArrayPattern {
  match node.kind {
    ArrayPattern_Pattern =>
      @syntax.ArrayPattern::Pattern(
        node
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(@syntax.Pattern::Any(loc=node.loc)),
      )
    ArrayPattern_StringSpread =>
      if node.first_child_with_label("str") is Some(str_node) {
        @syntax.ArrayPattern::StringSpread(
          str=lower_semantic_string(str_node).unwrap_or(""),
          loc=str_node.loc,
        )
      } else {
        @syntax.ArrayPattern::StringSpread(str="", loc=node.loc)
      }
    ArrayPattern_BytesSpread =>
      if node.first_child_with_label("bytes") is Some(bytes_node) {
        @syntax.ArrayPattern::BytesSpread(
          bytes=lower_semantic_string(bytes_node).unwrap_or(""),
          loc=bytes_node.loc,
        )
      } else {
        @syntax.ArrayPattern::BytesSpread(bytes="", loc=node.loc)
      }
    ArrayPattern_ConstSpread =>
      if node.first_child_with_label("const") is Some(name_node) {
        let pkg = if node.first_child_with_label("pkg") is Some(pkg_node) {
          lower_semantic_string(pkg_node)
        } else {
          None
        }
        @syntax.ArrayPattern::ConstSpread(
          binder=lower_binder_from_node(name_node),
          pkg~,
          loc=node.loc,
        )
      } else {
        @syntax.ArrayPattern::Pattern(@syntax.Pattern::Any(loc=node.loc))
      }
    Pattern_Rest =>
      @syntax.ArrayPattern::Pattern(@syntax.Pattern::Any(loc=node.loc))
    _ => @syntax.ArrayPattern::Pattern(lower_pattern(node))
  }
}

///|
fn lower_dot_dot_binder(node : CstNode) -> @syntax.DotDotBinder {
  match node.first_child_of_kind(NodeKind::is_dot_dot_binder) {
    Some(marker) =>
      match marker.kind {
        DotDotBinder_Underscore => @syntax.DotDotBinder::Underscore
        DotDotBinder_Binder =>
          node
          .first_child_with_label("name")
          .map(lower_binder_from_node)
          .map(value => @syntax.DotDotBinder::Binder(value))
          .unwrap_or(@syntax.DotDotBinder::NoBinder)
        DotDotBinder_BinderAs =>
          node
          .first_child_with_label("name")
          .map(lower_binder_from_node)
          .map(value => @syntax.DotDotBinder::BinderAs(value))
          .unwrap_or(@syntax.DotDotBinder::NoBinder)
        _ => @syntax.DotDotBinder::NoBinder
      }
    None => @syntax.DotDotBinder::NoBinder
  }
}

///|
fn lower_record_pattern_fields(node : CstNode) -> List[@syntax.FieldPat] {
  let fields : Array[@syntax.FieldPat] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      if !(item.kind is Pattern_Rest) {
        fields.push(lower_record_pattern_field(item))
      }
    }
  } else {
    ()
  }
  lower_array_to_list(fields)
}

///|
fn lower_record_pattern_is_closed(node : CstNode) -> Bool {
  if node.has_child_kind(Openness_Closed) {
    return true
  }
  if node.has_child_kind(Openness_Open) {
    return false
  }
  true
}

///|
fn lower_record_pattern_field(node : CstNode) -> @syntax.FieldPat {
  let label_node = node.first_child_with_label("name")
  let label = label_node
    .map(lower_label_from_node)
    .unwrap_or(@syntax.Label::{ name: "", loc: node.loc, })
  let pattern = node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(@syntax.Pattern::Any(loc=node.loc))
  @syntax.FieldPat::{
    label,
    pattern,
    is_pun: node.bool_marker_value(Flag_Pun).unwrap_or(false),
    loc: node.loc,
  }
}

///|
fn lower_map_pattern_is_closed(node : CstNode) -> Bool {
  if node.has_child_kind(Openness_Closed) {
    return true
  }
  if node.has_child_kind(Openness_Open) {
    return false
  }
  true
}

///|
fn lower_map_pattern_elems(node : CstNode) -> List[@syntax.MapPatElem] {
  let elems : Array[@syntax.MapPatElem] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      if !(item.kind is Pattern_Rest) {
        elems.push(lower_map_pattern_elem(item))
      }
    }
  } else {
    ()
  }
  lower_array_to_list(elems)
}

///|
fn lower_map_pattern_elem(node : CstNode) -> @syntax.MapPatElem {
  let (key, key_loc) = lower_map_pattern_key(node)
  @syntax.MapPatElem::{
    key,
    pat: node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(@syntax.Pattern::Any(loc=node.loc)),
    match_absent: node.bool_marker_value(Flag_MatchAbsent).unwrap_or(false),
    key_loc,
    loc: node.loc,
  }
}

///|
fn lower_map_pattern_key(node : CstNode) -> (@syntax.Constant, Location) {
  if node.first_child_with_label("semantic_constant") is Some(constant_node) {
    let key = lower_constant_from_node(constant_node).unwrap_or(
      @syntax.Constant::String(""),
    )
    let key_loc = match
      node.first_child_of_kind(fn(kind) { kind is Operator_UnaryMinus }) {
      Some(operator) => operator.loc.merge(constant_node.loc)
      None => constant_node.loc
    }
    (key, key_loc)
  } else {
    (@syntax.Constant::String(""), node.loc)
  }
}

///|
fn lower_pattern_items(node : CstNode) -> Array[@syntax.Pattern] {
  let result : Array[@syntax.Pattern] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      result.push(lower_pattern(item))
    }
  } else {
    ()
  }
  result
}

///|
fn lower_tuple_pattern(node : CstNode) -> @syntax.Pattern {
  let items = lower_pattern_items(node)
  if node.has_child_kind(Pattern_Group) {
    match items.get(0).unwrap_or(@syntax.Pattern::Any(loc=node.loc)) {
      @syntax.Pattern::Constraint(pat~, ty~, loc=_) =>
        @syntax.Pattern::Constraint(pat~, ty~, loc=node.loc)
      item => item
    }
  } else {
    @syntax.Pattern::Tuple(pats=lower_array_to_list(items), loc=node.loc)
  }
}

///|
fn lower_constructor_from_names(
  names : Array[CstNode],
  extra_info_kind : NodeKind,
  loc : Location,
) -> @syntax.Constructor {
  let extra_info = match extra_info_kind {
    ConstructorExtraInfo_Package if names.length() >= 2 =>
      @syntax.ConstructorExtraInfo::Package(
        lower_semantic_string(names[0]).unwrap_or(""),
      )
    ConstructorExtraInfo_TypeName if names.length() >= 2 => {
      let type_nodes : Array[CstNode] = []
      for i in 0..<(names.length() - 1) {
        type_nodes.push(names[i])
      }
      let type_loc = type_nodes[0].loc.merge(
        type_nodes[type_nodes.length() - 1].loc,
      )
      @syntax.ConstructorExtraInfo::TypeName(@syntax.TypeName::{
        name: recover_long_ident_from_name_nodes(type_nodes),
        is_object: false,
        loc: type_loc,
      })
    }
    _ => @syntax.ConstructorExtraInfo::NoExtraInfo
  }
  let loc = if names.length() > 0 {
    names[0].loc.merge(names[names.length() - 1].loc)
  } else {
    loc
  }
  let (name, name_loc) = if names.length() > 0 {
    let name_node = names[names.length() - 1]
    (lower_semantic_string(name_node).unwrap_or(""), name_node.loc)
  } else {
    ("", loc)
  }
  @syntax.Constructor::{
    name: @syntax.ConstrName::{ name, loc: name_loc, },
    extra_info,
    loc,
  }
}

///|
fn lower_constructor_pattern_args(
  node : CstNode,
) -> List[@syntax.ConstrPatArg]? {
  if node.first_child_with_label("params") is Some(list) {
    let args : Array[@syntax.ConstrPatArg] = []
    for item in lower_item_children(list) {
      if !(item.kind is Pattern_Rest) {
        args.push(lower_constructor_pattern_arg(item))
      }
    }
    Some(lower_array_to_list(args))
  } else {
    None
  }
}

///|
fn constructor_pattern_is_open(node : CstNode) -> Bool {
  if node.has_child_kind(Openness_Open) {
    return true
  }
  if node.has_child_kind(Openness_Closed) {
    return false
  }
  false
}

///|
fn lower_constructor_pattern_arg(item : CstNode) -> @syntax.ConstrPatArg {
  match item.kind {
    Argument_LabelledPun =>
      @syntax.ConstrPatArg::{
        pat: item
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(@syntax.Pattern::Any(loc=item.loc)),
        kind: lower_argument_kind(item),
      }
    Argument_Labelled =>
      @syntax.ConstrPatArg::{
        pat: item
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(@syntax.Pattern::Any(loc=item.loc)),
        kind: @syntax.ArgumentKind::Labelled(
          item
          .first_child_with_label("name")
          .map(lower_label_from_node)
          .unwrap_or(@syntax.Label::{ name: "", loc: item.loc, }),
        ),
      }
    _ =>
      @syntax.ConstrPatArg::{
        pat: lower_pattern(item),
        kind: @syntax.ArgumentKind::Positional,
      }
  }
}

///|
fn lower_constructor_pattern(node : CstNode) -> @syntax.Pattern {
  let constr = if node.first_child_with_label("field") is Some(field) {
    let loc = node
      .first_child_with_label("type")
      .map(type_name => type_name.loc.merge(field.loc))
      .unwrap_or(field.loc)
    @syntax.Constructor::{
      name: {
        name: lower_semantic_string(field).unwrap_or(""),
        loc: field.loc,
      },
      extra_info: lower_constructor_extra_info(node),
      loc,
    }
  } else {
    lower_constructor_from_names(
      node.children_with_label("name"),
      node
      .first_child_of_kind(NodeKind::is_constructor_extra_info)
      .map(fn(marker) { marker.kind })
      .unwrap_or(ConstructorExtraInfo_None),
      node.loc,
    )
  }
  @syntax.Pattern::Constr(
    constr~,
    args=lower_constructor_pattern_args(node),
    is_open=constructor_pattern_is_open(node),
    loc=node.loc,
  )
}

///|
fn lower_special_constructor_pattern(node : CstNode) -> @syntax.Pattern {
  let binder = node
    .first_child_with_label("name")
    .map(lower_binder_from_node)
    .unwrap_or(lower_dummy_binder(node.loc))
  let args : Array[@syntax.ConstrPatArg] = []
  if node.first_child_with_label("params") is Some(list) {
    for item in lower_item_children(list) {
      args.push(@syntax.ConstrPatArg::{
        pat: lower_pattern(item),
        kind: @syntax.ArgumentKind::Positional,
      })
    }
  } else {
    ()
  }
  @syntax.Pattern::SpecialConstr(
    binder~,
    args=lower_array_to_list(args),
    loc=node.loc,
  )
}

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
          @syntax.Type::Tuple(tys=@list.empty(), loc=node.loc)
        }
      } else {
        @syntax.Type::Any(loc=node.loc)
      }
    Type_Arrow => lower_function_type(node)
    Type_Any | Type_MissingType => @syntax.Type::Any(loc=node.loc)
    _ => @syntax.Type::Any(loc=node.loc)
  }
}

///|
fn lower_name_type(node : CstNode, is_object~ : Bool) -> @syntax.Type {
  let names = node.children_with_label("name")
  let constr_loc = if names.get(0) is Some(name) { name.loc } else { node.loc }
  let constr_id = lower_constr_id_from_node(node, constr_loc)
  if is_object {
    @syntax.Type::Object(constr_id)
  } else {
    @syntax.Type::Name(constr_id~, tys=@list.empty(), loc=node.loc)
  }
}

///|
fn lower_object_type(node : CstNode) -> @syntax.Type {
  if node.first_child_with_label("type") is Some(ty) {
    match ty.kind {
      Type_Name => {
        let constr_id = lower_constr_id_from_node(ty, ty.loc)
        @syntax.Type::Object(constr_id)
      }
      _ => @syntax.Type::Any(loc=node.loc)
    }
  } else {
    @syntax.Type::Any(loc=node.loc)
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
      @syntax.Type::Name(constr_id~, loc=_, tys=_) =>
        @syntax.Type::Name(
          constr_id~,
          tys=lower_array_to_list(args),
          loc=node.loc,
        )
      other => other
    }
  } else {
    @syntax.Type::Any(loc=node.loc)
  }
}

///|
fn lower_option_type(node : CstNode) -> @syntax.Type {
  let inner_node = node.first_child_with_label("type")
  let inner = if inner_node is Some(ty) {
    lower_type(ty)
  } else {
    @syntax.Type::Any(loc=node.loc)
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
  @syntax.Type::Option(ty=inner, loc~, question_loc~)
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
  @syntax.Type::Tuple(tys=lower_array_to_list(items), loc=node.loc)
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
      @syntax.Type::Arrow(args~, res~, err~, loc=_, is_async=_, nocancel_~) =>
        @syntax.Type::Arrow(
          args~,
          res~,
          err~,
          is_async=node
            .first_child_with_label("async_loc")
            .map(fn(anchor) { anchor.loc }),
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
      @syntax.Type::Any(loc=node.loc)
    }
    @syntax.Type::Arrow(
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
        @syntax.TypeName::{ name: base.name, is_object: true, loc: node.loc, }
      } else {
        @syntax.TypeName::{
          name: @syntax.LongIdent::dummy(),
          is_object: true,
          loc: node.loc,
        }
      }
    Type_Name =>
      @syntax.TypeName::{
        name: lower_long_ident_from_child(node, "semantic_name", "name"),
        is_object: node.bool_marker_value(Flag_Object).unwrap_or(false),
        loc: node.loc,
      }
    _ =>
      @syntax.TypeName::{
        name: @syntax.LongIdent::dummy(),
        is_object: false,
        loc: node.loc,
      }
  }
}
