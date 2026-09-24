///|
pub fn lower_impls_from_cst(root : CstNode) -> @syntax.Impls {
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
      TopExpr(
        expr=lower_hole_expr(node.loc),
        is_main=false,
        is_async=None,
        loc=node.loc,
      )
  }
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
      Visibility_Pub => return Pub(attr~, loc=vis.loc)
      Visibility_Priv => return Priv(loc=vis.loc)
      _ => ()
    }
  }
  Default
}

///|
fn lower_is_declare(node : CstNode) -> Bool {
  node.bool_marker_value(fn(kind) { kind is Flag_Declare }).unwrap_or(false)
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
  {
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
  TopFuncDef(
    fun_decl~,
    decl_body=lower_function_decl_body(node, body_expr),
    where_clause=node.first_child_with_label("where").map(lower_where_clause),
    loc=lower_decl_loc_without_trailing_trivia(node),
  )
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
      Some({
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
        DeclBody_None => return DeclNone
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
          return DeclStubs(Import(module_name~, func_name~, language~))
        }
        DeclBody_EmbeddedString => {
          let code = stubs
            .get(0)
            .map(lower_semantic_string)
            .bind(value => value)
            .unwrap_or("")
          return DeclStubs(Embedded(language~, code=CodeString(code)))
        }
        DeclBody_EmbeddedMultilineString => {
          let lines : Array[String] = []
          for stub in stubs {
            lines.push(lower_semantic_string(stub).unwrap_or(""))
          }
          return DeclStubs(
            Embedded(
              language~,
              code=CodeMultilineString(lower_array_to_list(lines)),
            ),
          )
        }
        DeclBody_Expr => return DeclBody(expr=body_expr)
        _ => ()
      }
    None => ()
  }
  DeclBody(expr=body_expr)
}

///|
fn lower_fun_quantifiers(node : CstNode) -> List[@syntax.TypeVarBinder] {
  let quantifiers : Array[@syntax.TypeVarBinder] = []
  if node.first_child_with_label("quantifier") is Some(list) {
    for item in lower_item_children(list) {
      let name_node = item.first_child_with_label("name").unwrap_or(item)
      quantifiers.push({
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
      constraints.push({
        trait_: lower_type_name_from_type_node(node).name,
        loc: node.loc,
      })
    Type_Tuple | Type_Group =>
      for item in node.children_with_label("type") {
        lower_type_var_constraints_from_type(item, constraints)
      }
    _ =>
      constraints.push({ trait_: @syntax.LongIdent::dummy(), loc: node.loc, })
  }
}

///|
fn lower_value_decl(node : CstNode, is_constant~ : Bool) -> @syntax.Impl {
  let binder = if node.first_child_with_label("pattern") is Some(pat) {
    match lower_pattern(pat) {
      Var(binder) => binder
      _ => lower_decl_name_binder(node)
    }
  } else {
    lower_decl_name_binder(node)
  }
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(
      if lower_is_declare(node) && !is_constant {
        @syntax.Expr::Hole(loc=binder.loc, kind=Synthesized)
      } else {
        lower_unit_expr(node.loc)
      },
    )
  let parsed_loc = lower_decl_loc_without_trailing_trivia(node)
  let loc = if node.first_child_with_label("expr") is Some(_) {
    Location::{ start: parsed_loc.start, end: expr.loc().end, }
  } else {
    parsed_loc
  }
  TopLetDef(
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
  let components = if node.has_child_kind(fn(kind) { kind is TypeDesc_Abstract }) ||
    node.kind is Impl_TypeAbstract {
    @syntax.TypeDesc::Abstract
  } else if node.first_child_with_label("type") is Some(ty) {
    Alias(lower_type(ty))
  } else {
    Abstract
  }
  TopTypeDef({
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
  let tycon_loc = if node.kind is Impl_ExtendEnum {
    lower_decl_type_name(node).loc
  } else {
    name_loc
  }
  TopTypeDef({
    tycon: name,
    tycon_loc,
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
      deriving.push({
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
  if node.has_child_kind(fn(kind) { kind is TypeDesc_TupleStruct }) ||
    node.kind is Impl_TupleStruct {
    let components = node
      .first_child_with_label("body")
      .map(fn(body) {
        @syntax.TypeDesc::TupleStruct(lower_tuple_struct_types(body))
      })
      .unwrap_or(TupleStruct(@list.empty()))
    lower_simple_type_decl(node, components)
  } else if node.has_child_kind(fn(kind) { kind is TypeDesc_Record }) ||
    node.kind is Impl_RecordStruct {
    lower_simple_type_decl(node, Record(fields=lower_struct_fields(node)))
  } else if node.first_child_with_label("body") is Some(body) {
    match body.kind {
      List_TupleStructTypeList =>
        lower_simple_type_decl(
          node,
          TupleStruct(lower_tuple_struct_types(body)),
        )
      _ =>
        lower_simple_type_decl(node, Record(fields=lower_struct_fields(node)))
    }
  } else {
    lower_simple_type_decl(node, Record(fields=lower_struct_fields(node)))
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
        Error(lower_suberror_exception_decl(node)),
      )
    Impl_ExtendEnum =>
      return lower_simple_type_decl(
        node,
        ExtendEnum(
          target=lower_decl_type_name(node),
          constructors=lower_enum_constructors(node),
        ),
      )
    Impl_ExtensibleEnum =>
      return lower_simple_type_decl(
        node,
        ExtensibleEnum(lower_enum_constructors(node)),
      )
    Impl_EnumVariant =>
      return lower_simple_type_decl(
        node,
        Variant(lower_enum_constructors(node)),
      )
    _ => ()
  }
  lower_simple_type_decl(node, Variant(lower_enum_constructors(node)))
}

///|
fn lower_enumview_decl(node : CstNode) -> @syntax.Impl {
  let (parameters, params_loc) = lower_parameters_from_list(
    node.first_child_with_label("params"),
  )
  let (view_type_name, view_type_loc) = lower_decl_name_string(node)
  TopView(
    quantifiers=lower_fun_quantifiers(node),
    source_ty=node
      .first_child_with_label("type")
      .map(lower_type)
      .unwrap_or(Any(loc=node.loc)),
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
    Impl_Suberror => NoPayload
    Impl_SuberrorEnum => EnumPayload(lower_enum_constructors(node))
    _ => {
      // Coarse recovery nodes predate the point where the parser can select a
      // final suberror form. Keep their fallback deterministic.
      let constructors = lower_enum_constructors(node)
      if constructors.length() == 0 {
        NoPayload
      } else {
        EnumPayload(constructors)
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
  {
    name: {
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
  {
    ty: node
    .first_child_with_label("type")
    .map(lower_type)
    .unwrap_or(@syntax.Type::Any(loc=node.loc)),
    mut_: node
    .bool_marker_value(fn(kind) { kind is Flag_Mutable })
    .unwrap_or(false),
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
  {
    name: { label, loc: name_node.loc, },
    ty: node
    .first_child_with_label("type")
    .map(lower_type)
    .unwrap_or(@syntax.Type::Any(loc=node.loc)),
    mut_: node
    .bool_marker_value(fn(kind) { kind is Flag_Mutable })
    .unwrap_or(false),
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
      params.push({ name, loc: name_node.loc, })
    }
  } else {
    ()
  }
  lower_array_to_list(params)
}

///|
fn lower_trait_decl(node : CstNode) -> @syntax.Impl {
  TopTrait({
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
  {
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
      Parameter_Labelled => Labelled(binder~, ty~)
      Parameter_QuestionOptional => QuestionOptional(binder~, ty~)
      _ => Positional(binder~, ty~)
    }
  } else {
    DiscardPositional(ty~, loc=ty.map(type_ => type_.loc()).unwrap_or(node.loc))
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
  TopImplRelation(
    self_ty=node
      .first_child_with_label("type")
      .map(lower_type)
      .unwrap_or(Any(loc=node.loc)),
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
  TopImpl(
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
  TopTest(
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
    .unwrap_or({ name: "", loc: node.loc, })
  TopUsing(
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
      let star_node = item.first_child_with_label("op")
      let alias_ = if star_node is Some(_) {
        Some("*" + alias_.unwrap_or(""))
      } else {
        alias_
      }
      packages.push({
        path: lower_string_literal_node(path_node),
        alias_,
        loc: if star_node is Some(star) {
          path_node.loc.merge(star.loc)
        } else if alias_node is Some(alias_node) {
          path_node.loc.merge(alias_node.loc)
        } else {
          path_node.loc
        },
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
            UsingKind_Trait => Trait
            _ => Value
          }
        None => Value
      }
      names.push(
        ({ binder: lower_binder_from_node(binder_node), target, }, using_kind),
      )
    }
  } else {
    ()
  }
  lower_array_to_list(names)
}
