///|
fn lower_empty_doc() -> @syntax.DocString {
  @syntax.DocString::empty()
}

///|
fn lower_empty_loc() -> Location {
  let pos = initial_position("")
  Location::{ start: pos, end: pos }
}

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
fn CstNode::first_token(self : CstNode) -> Token? {
  match self.kind {
    Token(tok) => Some(tok)
    _ => {
      for i in 0..<self.children.length() {
        match self.children.get(i) {
          Some((_, child)) =>
            if child.first_token() is Some(tok) {
              return Some(tok)
            } else {
              ()
            }
          None => ()
        }
      }
      None
    }
  }
}

///|
fn CstNode::first_token_child_with_label(
  self : CstNode,
  label : String,
) -> CstNode? {
  for child in self.children_with_label(label) {
    match child.kind {
      Token(_) => return Some(child)
      _ => ()
    }
  }
  None
}

///|
fn lower_token_name(tok : Token) -> String? {
  match tok {
    LIDENT(name)
    | UIDENT(name)
    | PACKAGE_NAME(name)
    | DOT_LIDENT(name)
    | DOT_UIDENT(name)
    | POST_LABEL(name)
    | INFIX1(name)
    | INFIX2(name)
    | INFIX3(name)
    | INFIX4(name)
    | AUGMENTED_ASSIGNMENT(name)
    | INT(name)
    | DOUBLE(name)
    | FLOAT(name)
    | STRING(name)
    | BYTES(name)
    | REGEX_LITERAL(name)
    | MULTILINE_STRING(name)
    | BYTE(name)
    | CHAR(name) => Some(name)
    TRUE => Some("true")
    FALSE => Some("false")
    PLUS => Some("+")
    PLUS_EQUAL => Some("+")
    MINUS => Some("-")
    EXCLAMATION => Some("!")
    AMPER => Some("&")
    AMPERAMPER => Some("&&")
    BAR => Some("|")
    BARBAR => Some("||")
    CARET => Some("^")
    PIPE => Some("|>")
    PIPE_LEFT => Some("<|")
    IMPLIES => Some("→")
    DOTDOT => Some("..")
    RANGE_EXCLUSIVE => Some("..<")
    RANGE_INCLUSIVE => Some("..=")
    RANGE_LT_INCLUSIVE => Some("..<=")
    RANGE_INCLUSIVE_REV => Some(">=..")
    RANGE_EXCLUSIVE_REV => Some(">..")
    READONLY => Some("readonly")
    EXTEND => Some("extend")
    _ => None
  }
}

///|
fn lower_token_child_name(node : CstNode) -> String? {
  match node.kind {
    Token(tok) => lower_token_name(tok)
    _ =>
      if node.first_token() is Some(tok) {
        lower_token_name(tok)
      } else {
        None
      }
  }
}

///|
fn lower_label_from_node(node : CstNode) -> @syntax.Label {
  let name = lower_token_child_name(node).unwrap_or("")
  @syntax.Label::{ name, loc: node.loc }
}

///|
fn lower_binder_from_node(node : CstNode) -> @syntax.Binder {
  let name = lower_token_child_name(node).unwrap_or("")
  @syntax.Binder::{ name, loc: node.loc }
}

///|
fn lower_post_label_binder_from_node(node : CstNode) -> @syntax.Binder {
  let binder = lower_binder_from_node(node)
  match node.first_token() {
    Some(POST_LABEL(_)) =>
      @syntax.Binder::{ name: binder.name, loc: binder.loc.trim_last_char() }
    _ => binder
  }
}

///|
fn lower_dummy_binder(loc : Location) -> @syntax.Binder {
  @syntax.Binder::{ name: "", loc }
}

///|
fn lower_long_ident_from_name_nodes(
  nodes : Array[CstNode],
) -> @syntax.LongIdent {
  if nodes.length() >= 2 {
    let pkg = lower_token_child_name(nodes[0]).unwrap_or("")
    let id = lower_token_child_name(nodes[1]).unwrap_or("")
    @syntax.LongIdent::Dot(pkg~, id~)
  } else if nodes.length() == 1 {
    @syntax.LongIdent::Ident(
      name=lower_token_child_name(nodes[0]).unwrap_or(""),
    )
  } else {
    @syntax.LongIdent::dummy()
  }
}

///|
fn lower_constr_id_from_name_nodes(
  nodes : Array[CstNode],
  loc : Location,
) -> @syntax.ConstrId {
  @syntax.ConstrId::{ id: lower_long_ident_from_name_nodes(nodes), loc }
}

///|
fn lower_var_from_token_node(node : CstNode) -> @syntax.Var {
  @syntax.Var::{
    name: @syntax.LongIdent::Ident(
      name=lower_token_child_name(node).unwrap_or(""),
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
fn lower_constant_from_token(tok : Token) -> @syntax.Constant? {
  match tok {
    TRUE => Some(@syntax.Constant::Bool(true))
    FALSE => Some(@syntax.Constant::Bool(false))
    INT(value) => Some(@syntax.make_int(value))
    BYTE(value) => Some(@syntax.Constant::Byte(value))
    CHAR(value) => Some(@syntax.Constant::Char(value))
    DOUBLE(value) => Some(@syntax.Constant::Double(value))
    FLOAT(value) => Some(@syntax.make_float(value))
    STRING(value) => Some(@syntax.Constant::String(value))
    BYTES(value) => Some(@syntax.Constant::Bytes(value))
    REGEX_LITERAL(value) => Some(@syntax.Constant::Regex(value))
    _ => None
  }
}

///|
fn lower_error_type_from_clause(node : CstNode?) -> @syntax.ErrorType {
  if node is Some(clause) {
    match clause.kind {
      Clause_ErrorAnnotation => lower_error_annotation(clause)
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
fn return_clause_error_belongs_to_type(clause : CstNode) -> Bool {
  match clause.kind {
    Clause_ReturnType =>
      match
        (
          clause.first_child_with_label("type"),
          clause.first_child_with_label("clause"),
        ) {
        (Some(ty), Some(_)) => ty.kind is Type_Function
        _ => false
      }
    _ => false
  }
}

///|
fn lower_error_annotation(node : CstNode) -> @syntax.ErrorType {
  match node.first_child_with_label("op").map(CstNode::first_token) {
    Some(Some(NORAISE)) => @syntax.ErrorType::Noraise(loc=node.loc)
    Some(Some(RAISE)) =>
      if node.first_child_with_label("type") is Some(ty_node) {
        match ty_node.first_token() {
          Some(QUESTION) =>
            @syntax.ErrorType::MaybeError(ty=lower_fake_error_type(node.loc))
          _ =>
            if ty_node.kind is Type_Name &&
              ty_node.first_child_with_label("question") is Some(_) {
              @syntax.ErrorType::ErrorType(
                ty=lower_question_suffixed_error_type(ty_node),
              )
            } else {
              @syntax.ErrorType::ErrorType(
                ty=lower_error_annotation_type(ty_node),
              )
            }
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
        constr_id=@syntax.ConstrId::{ id: constr_id.id, loc: node.loc },
        tys~,
        loc~,
      )
    ty => ty
  }
}

///|
fn lower_question_suffixed_error_type(node : CstNode) -> @syntax.Type {
  let base_name = if node.first_child_with_label("name") is Some(name_node) {
    lower_token_child_name(name_node).unwrap_or("")
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
    SourceFile =>
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
    Decl_Function => lower_function_decl(node)
    Decl_Let => lower_value_decl(node, is_constant=false)
    Decl_Const => lower_value_decl(node, is_constant=true)
    Decl_Type => lower_type_decl(node)
    Decl_Struct => lower_struct_decl(node)
    Decl_Enum => lower_enum_decl(node)
    Decl_EnumView => lower_enumview_decl(node)
    Decl_Trait => lower_trait_decl(node)
    Decl_Impl => lower_impl_decl(node)
    Decl_Test => lower_test_decl(node)
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
  let mut end = node.loc.start
  let mut found = false
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some("trivia"), _)) => ()
      Some((_, child)) => {
        end = child.loc.end
        found = true
      }
      None => ()
    }
  }
  if found {
    Location::{ start: node.loc.start, end }
  } else {
    node.loc
  }
}

///|
fn lower_trait_decl_loc(node : CstNode) -> Location {
  let semantic_loc = lower_decl_loc_without_trailing_trivia(node)
  for kw in node.children_with_label("kw") {
    if kw.first_token() is Some(TRAIT) {
      return Location::{ start: kw.loc.start, end: semantic_loc.end }
    }
  }
  semantic_loc
}

///|
fn lower_visibility(node : CstNode) -> @syntax.Visibility {
  for vis in node.children_with_label("vis") {
    let attr = if vis.first_child_with_label("attr") is Some(attr_node) {
      lower_token_child_name(attr_node)
    } else {
      None
    }
    let kw = vis.first_child_with_label("kw").unwrap_or(vis)
    match kw.first_token() {
      Some(PUB) => return @syntax.Visibility::Pub(attr~, loc=vis.loc)
      Some(PRIV) => return @syntax.Visibility::Priv(loc=vis.loc)
      _ => ()
    }
  }
  @syntax.Visibility::Default
}

///|
fn lower_async_loc(node : CstNode) -> Location? {
  for kw in node.children_with_label("kw") {
    match kw.first_token() {
      Some(ASYNC) => return Some(kw.loc)
      _ => ()
    }
  }
  None
}

///|
fn lower_is_declare(node : CstNode) -> Bool {
  for kw in node.children_with_label("kw") {
    match kw.first_token() {
      Some(DECLARE) => return true
      _ => ()
    }
  }
  false
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
    (lower_token_child_name(name).unwrap_or(""), name.loc)
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
    name: lower_long_ident_from_name_nodes(names),
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
  let start = node.loc.start
  let mut end = start
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some("body" | "where" | "stub" | "trivia"), _)) => ()
      Some((Some("op"), child)) if child.first_token() is Some(EQUAL) => ()
      Some((_, child)) => end = child.loc.end
      None => ()
    }
  }
  Location::{ start, end }
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
        name: lower_long_ident_from_name_nodes(owners),
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
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) =>
        if child.first_token() is Some(AMPER) {
          return Some(child.loc)
        }
      None => ()
    }
  }
  None
}

///|
fn lower_function_decl_body(
  node : CstNode,
  body_expr : @syntax.Expr,
) -> @syntax.DeclBody {
  if lower_is_declare(node) {
    return @syntax.DeclBody::DeclNone
  }
  let language = if node.first_child_with_label("language") is Some(language) {
    lower_token_child_name(language)
  } else {
    None
  }
  let stubs = node.children_with_label("stub")
  if stubs.get(0) is Some(stub) {
    match stub.first_token() {
      Some(STRING(code)) =>
        if stubs.get(1) is Some(func_stub) {
          match func_stub.first_token() {
            Some(STRING(func_name)) =>
              @syntax.DeclBody::DeclStubs(
                @syntax.FuncStubs::Import(
                  module_name=code,
                  func_name~,
                  language~,
                ),
              )
            _ =>
              @syntax.DeclBody::DeclStubs(
                @syntax.FuncStubs::Embedded(
                  language~,
                  code=@syntax.EmbeddedCode::CodeString(code),
                ),
              )
          }
        } else {
          @syntax.DeclBody::DeclStubs(
            @syntax.FuncStubs::Embedded(
              language~,
              code=@syntax.EmbeddedCode::CodeString(code),
            ),
          )
        }
      Some(MULTILINE_STRING(_)) => {
        let lines : Array[String] = []
        for stub in stubs {
          match stub.first_token() {
            Some(MULTILINE_STRING(code)) => lines.push(code)
            _ => ()
          }
        }
        @syntax.DeclBody::DeclStubs(
          @syntax.FuncStubs::Embedded(
            language~,
            code=@syntax.EmbeddedCode::CodeMultilineString(
              lower_array_to_list(lines),
            ),
          ),
        )
      }
      _ => @syntax.DeclBody::DeclBody(expr=body_expr)
    }
  } else {
    @syntax.DeclBody::DeclBody(expr=body_expr)
  }
}

///|
fn lower_fun_quantifiers(node : CstNode) -> List[@syntax.TypeVarBinder] {
  let quantifiers : Array[@syntax.TypeVarBinder] = []
  if node.first_child_with_label("quantifier") is Some(list) {
    for item in lower_item_children(list) {
      let name_node = item.first_child_with_label("name").unwrap_or(item)
      quantifiers.push(@syntax.TypeVarBinder::{
        name: lower_token_child_name(name_node).unwrap_or(""),
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
    Location::{ start: parsed_loc.start, end: expr.loc().end }
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
  let components = if node.first_child_with_label("type") is Some(ty) {
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
    match attr.first_token() {
      Some(ATTRIBUTE(content)) =>
        attrs.push(@attribute.Attribute::new(loc=attr.loc, content))
      _ => ()
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
      deriving.push(@syntax.DerivingDirective::{
        type_name: @syntax.TypeName::{
          name: lower_long_ident_from_name_nodes(name_nodes),
          is_object: false,
          loc: name_loc,
        },
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
  if node.first_child_with_label("body") is Some(body) {
    match body.kind {
      List_TypeArgumentList =>
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
  let mut is_extenum = false
  let mut is_extend = false
  for kw in node.children_with_label("kw") {
    match kw.first_token() {
      Some(SUBERROR) =>
        return lower_simple_type_decl(
          node,
          @syntax.TypeDesc::Error(lower_suberror_exception_decl(node)),
        )
      Some(EXTENUM) => is_extenum = true
      _ => ()
    }
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) =>
        if child.first_token() is Some(PLUS_EQUAL) {
          is_extend = true
        }
      None => ()
    }
  }
  if is_extenum && is_extend {
    return lower_simple_type_decl(
      node,
      @syntax.TypeDesc::ExtendEnum(
        target=lower_decl_type_name(node),
        constructors=lower_enum_constructors(node),
      ),
    )
  }
  if is_extenum {
    return lower_simple_type_decl(
      node,
      @syntax.TypeDesc::ExtensibleEnum(lower_enum_constructors(node)),
    )
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
  let constructors = lower_enum_constructors(node)
  if constructors.length() == 0 {
    @syntax.ExceptionDecl::NoPayload
  } else {
    @syntax.ExceptionDecl::EnumPayload(constructors)
  }
}

///|
fn lower_enum_constructors(node : CstNode) -> List[@syntax.ConstrDecl] {
  let constrs : Array[@syntax.ConstrDecl] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      match item.kind {
        Name_Constructor => constrs.push(lower_enum_constructor(item))
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
      name: lower_token_child_name(name_node).unwrap_or(""),
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
  if node.first_child_with_label("tag") is Some(tag_node) {
    match tag_node.first_token() {
      Some(INT(value)) => Some((value, tag_node.loc))
      _ => None
    }
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
      name: lower_token_child_name(name).unwrap_or(""),
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
    mut_: node.first_child_with_label("mut") is Some(_),
    label,
  }
}

///|
fn lower_struct_fields(node : CstNode) -> List[@syntax.FieldDecl] {
  let fields : Array[@syntax.FieldDecl] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      match item.kind {
        Clause_Parameter => fields.push(lower_struct_field(item))
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
  let label = lower_token_child_name(name_node).unwrap_or("")
  @syntax.FieldDecl::{
    name: @syntax.FieldName::{ label, loc: name_node.loc },
    ty: node
    .first_child_with_label("type")
    .map(lower_type)
    .unwrap_or(@syntax.Type::Any(loc=node.loc)),
    mut_: node.first_child_with_label("mut") is Some(_),
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
      let name = match name_node.first_token() {
        Some(UNDERSCORE) => None
        Some(tok) => lower_token_name(tok)
        None => None
      }
      params.push(@syntax.TypeDeclBinder::{ name, loc: name_node.loc })
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
        Decl_Function => methods.push(lower_trait_method(item))
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
    match pat_node.first_token() {
      Some(POST_LABEL(_)) => @syntax.Parameter::Labelled(binder~, ty~)
      _ =>
        if lower_trait_parameter_is_question_optional(node) {
          @syntax.Parameter::QuestionOptional(binder~, ty~)
        } else {
          @syntax.Parameter::Positional(binder~, ty~)
        }
    }
  } else {
    @syntax.Parameter::DiscardPositional(
      ty~,
      loc=ty.map(type_ => type_.loc()).unwrap_or(node.loc),
    )
  }
}

///|
fn lower_trait_parameter_is_question_optional(node : CstNode) -> Bool {
  if node.first_token_child_with_label("op") is Some(op) {
    match op.first_token() {
      Some(QUESTION) => true
      _ => false
    }
  } else {
    false
  }
}

///|
fn lower_impl_decl(node : CstNode) -> @syntax.Impl {
  if node.first_child_with_label("method") is Some(method_node) {
    return lower_impl_method_decl(node, method_node)
  } else {
    ()
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
  let name : @syntax.TestName = if node.first_child_with_label("name")
    is Some(name_node) {
    match name_node.first_token() {
      Some(STRING(value)) => Some((value, name_node.loc))
      _ => None
    }
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
    .unwrap_or(@syntax.Label::{ name: "", loc: node.loc })
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
        item.first_child_with_label("kind").map(CstNode::first_token) {
        Some(Some(TYPE)) => @syntax.UsingKind::Type
        Some(Some(TRAIT)) => @syntax.UsingKind::Trait
        _ => @syntax.UsingKind::Value
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
    ExpressionFragment =>
      node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc))
    Expr_Identifier =>
      @syntax.Expr::Ident(
        id=@syntax.Var::{
          name: lower_long_ident_from_name_nodes(
            node.children_with_label("name"),
          ),
          loc: node.loc,
        },
        loc=node.loc,
      )
    Expr_Constructor => lower_constructor_expr(node)
    Expr_Literal => lower_literal_expr(node)
    Expr_MultilineString => lower_multiline_string_expr(node)
    Expr_Hole =>
      match node.first_token() {
        Some(ELLIPSIS) =>
          @syntax.Expr::Hole(loc=node.loc, kind=@syntax.Hole::Todo)
        _ => @syntax.Expr::Hole(loc=node.loc, kind=@syntax.Hole::Incomplete)
      }
    Expr_MissingExpr => lower_hole_expr(node.loc)
    Expr_Tuple => lower_tuple_expr(node)
    Expr_Constraint => lower_constraint_expr(node)
    Expr_Array => lower_array_expr_node(node)
    Expr_ListComprehension => lower_list_comprehension_expr(node)
    Expr_Record => lower_record_expr_node(node)
    Expr_Map => lower_map_expr_node(node)
    Expr_Block => lower_block_expr_node(node)
    Expr_Infix => lower_infix_expr(node)
    Expr_Range => lower_infix_expr(node)
    Expr_Prefix => lower_prefix_expr(node)
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
    Expr_LetRec =>
      lower_letrec_expr_with_body(node, lower_faked_unit_expr(node.loc))
    Expr_Let | Expr_LetMut => lower_let_expr_without_body(node)
    Expr_Assign | Expr_AugmentedAssign => lower_assign_expr(node)
    Expr_Guard => lower_guard_expr_with_body(node, lower_unit_expr(node.loc))
    Expr_Defer => lower_defer_expr_with_body(node, lower_unit_expr(node.loc))
    Stmt_ProofAssert => lower_proof_assert_expr(node)
    Stmt_ProofLet => lower_proof_let_expr(node)
    Expr_Return => lower_return_expr(node)
    Expr_Raise => lower_raise_expr(node)
    Expr_Break => lower_break_expr(node)
    Expr_Continue => lower_continue_expr(node)
    Expr_Group =>
      node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc))
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
  let kind = match node.first_child_with_label("kw").map(CstNode::first_token) {
    Some(Some(EXISTS)) => @syntax.PredicateQuantifierKind::Exists
    _ => @syntax.PredicateQuantifierKind::Forall
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
  match node.first_token() {
    Some(INTERP(_)) =>
      @syntax.Expr::Interp(elems=lower_interp_elems(node), loc=node.loc)
    Some(BYTES_INTERP(_)) =>
      @syntax.Expr::BytesInterp(elems=lower_interp_elems(node), loc=node.loc)
    Some(tok) =>
      if lower_constant_from_token(tok) is Some(c) {
        @syntax.Expr::Constant(c~, loc=node.loc)
      } else {
        lower_hole_expr(node.loc)
      }
    None => lower_hole_expr(node.loc)
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
    is_conditional=node.first_child_with_label("op").map(CstNode::first_token)
      is Some(Some(LT_QUESTION)),
    loc=node.loc,
  )
}

///|
fn lower_interp_elems(node : CstNode) -> List[@syntax.InterpElem] {
  let elems : Array[@syntax.InterpElem] = []
  for segment in node.children_with_label("segment") {
    elems.push(lower_interp_segment(segment))
  }
  lower_array_to_list(elems)
}

///|
fn lower_child_token_name(node : CstNode, label : String) -> String {
  if node.first_child_with_label(label) is Some(child) {
    lower_token_child_name(child).unwrap_or("")
  } else {
    ""
  }
}

///|
fn lower_interp_segment(node : CstNode) -> @syntax.InterpElem {
  match node.kind {
    InterpSegment_InterpLiteralSegment =>
      @syntax.InterpElem::Literal(
        repr=lower_child_token_name(node, "repr"),
        loc=node.loc,
      )
    InterpSegment_InterpSourceSegment =>
      @syntax.InterpElem::Source(@tokens.InterpSource::{
        source: lower_child_token_name(node, "source"),
        loc: node.loc,
      })
    _ =>
      @syntax.InterpElem::Source(@tokens.InterpSource::{
        source: "",
        loc: node.loc,
      })
  }
}

///|
fn lower_multiline_string_expr(node : CstNode) -> @syntax.Expr {
  let elems : Array[@syntax.MultilineStringElem] = []
  let mut interp_elems : Array[@syntax.InterpElem] = []
  let mut collecting_interp = false
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some("token"), token_node)) =>
        match token_node.first_token() {
          Some(MULTILINE_STRING(value)) => {
            if collecting_interp {
              elems.push(
                @syntax.MultilineStringElem::Interp(
                  lower_array_to_list(interp_elems),
                ),
              )
              interp_elems = []
              collecting_interp = false
            }
            elems.push(@syntax.MultilineStringElem::String(value))
          }
          Some(MULTILINE_INTERP(_)) => {
            if collecting_interp {
              elems.push(
                @syntax.MultilineStringElem::Interp(
                  lower_array_to_list(interp_elems),
                ),
              )
              interp_elems = []
            }
            collecting_interp = true
          }
          _ => ()
        }
      Some((Some("segment"), segment)) =>
        if collecting_interp {
          interp_elems.push(lower_interp_segment(segment))
        }
      _ => ()
    }
  }
  if collecting_interp {
    elems.push(
      @syntax.MultilineStringElem::Interp(lower_array_to_list(interp_elems)),
    )
  }
  @syntax.Expr::MultilineString(
    elems=lower_array_to_list(elems),
    loc=lower_multiline_string_loc(node),
  )
}

///|
fn lower_multiline_string_loc(node : CstNode) -> Location {
  let tokens = node.children_with_label("token")
  match (tokens.get(0), tokens.last()) {
    (Some(first), Some(last)) => first.loc.merge(last.loc)
    _ => node.loc
  }
}

///|
fn lower_constructor_expr(node : CstNode) -> @syntax.Expr {
  let constr = if node.first_child_with_label("field") is Some(field) {
    @syntax.Constructor::{
      name: @syntax.ConstrName::{
        name: lower_token_child_name(field).unwrap_or(""),
        loc: field.loc,
      },
      extra_info: lower_constructor_expr_extra_info(node),
      loc: node.loc,
    }
  } else {
    lower_constructor_from_names(node.children_with_label("name"), node.loc)
  }
  @syntax.Expr::Constr(constr~, loc=node.loc)
}

///|
fn lower_constructor_expr_extra_info(
  node : CstNode,
) -> @syntax.ConstructorExtraInfo {
  if node.first_child_with_label("type") is Some(type_node) {
    let type_name = lower_type_name_from_expr_node(type_node)
    if node.first_child_with_label("pkg") is Some(pkg_node) {
      @syntax.ConstructorExtraInfo::TypeNameWithConstrPackage(
        type_name~,
        pkg=lower_token_child_name(pkg_node).unwrap_or(""),
      )
    } else {
      @syntax.ConstructorExtraInfo::TypeName(type_name)
    }
  } else if node.first_child_with_label("pkg") is Some(pkg_node) {
    @syntax.ConstructorExtraInfo::Package(
      lower_token_child_name(pkg_node).unwrap_or(""),
    )
  } else {
    @syntax.ConstructorExtraInfo::NoExtraInfo
  }
}

///|
fn lower_argument_kind(
  item : CstNode,
  labelled_pun_loc : Location,
) -> @syntax.ArgumentKind {
  match item.kind {
    Argument_Labelled =>
      @syntax.ArgumentKind::Labelled(
        item
        .first_child_with_label("name")
        .map(lower_label_from_node)
        .unwrap_or(@syntax.Label::{ name: "", loc: item.loc }),
      )
    Argument_LabelledPun =>
      @syntax.ArgumentKind::LabelledPun(
        item
        .first_child_with_label("name")
        .map(name => @syntax.Label::{
          name: lower_token_child_name(name).unwrap_or(""),
          loc: labelled_pun_loc,
        })
        .unwrap_or(@syntax.Label::{ name: "", loc: item.loc }),
      )
    Argument_LabelledOption =>
      @syntax.ArgumentKind::LabelledOption(
        label=item
          .first_child_with_label("name")
          .map(lower_label_from_node)
          .unwrap_or(@syntax.Label::{ name: "", loc: item.loc }),
        question_loc=item
          .first_child_with_label("op")
          .map(fn(node) { node.loc })
          .unwrap_or(item.loc),
      )
    Argument_LabelledOptionPun =>
      @syntax.ArgumentKind::LabelledOptionPun(
        label=item
          .first_child_with_label("name")
          .map(lower_label_from_node)
          .unwrap_or(@syntax.Label::{ name: "", loc: item.loc }),
        question_loc=item
          .first_child_with_label("op")
          .map(fn(node) { node.loc })
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
    let following_loc = lower_argument_following_loc(list, i, item.loc)
    let value = match item.kind {
      Argument_LabelledPun =>
        item
        .first_child_with_label("name")
        .map(name => {
          lower_argument_pun_value(name, following_loc.trim_last_char())
        })
        .unwrap_or(lower_hole_expr(item.loc))
      Argument_LabelledOptionPun =>
        item
        .first_child_with_label("name")
        .map(name => lower_argument_pun_value(name, name.loc))
        .unwrap_or(lower_hole_expr(item.loc))
      kind if kind.is_argument() =>
        item
        .first_child_with_label("expr")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(item.loc))
      _ => lower_expr(item)
    }
    args.push(@syntax.Argument::{
      value,
      kind: lower_argument_kind(item, following_loc),
    })
  }
  lower_array_to_list(args)
}

///|
fn lower_argument_following_loc(
  list : CstNode,
  item_index : Int,
  fallback : Location,
) -> Location {
  for i in (item_index + 1)..<list.children.length() {
    match list.children.get(i) {
      Some((Some("sep" | "close"), child)) => return child.loc
      _ => ()
    }
  }
  fallback
}

///|
fn lower_argument_pun_value(node : CstNode, loc : Location) -> @syntax.Expr {
  @syntax.Expr::Ident(
    id=@syntax.Var::{
      name: @syntax.LongIdent::Ident(
        name=lower_token_child_name(node).unwrap_or(""),
      ),
      loc,
    },
    loc~,
  )
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
  if exprs.length() == 0 {
    @syntax.Expr::Unit(loc=node.loc, faked=false)
  } else if exprs.length() == 1 {
    @syntax.Expr::Group(expr=exprs[0], group=@syntax.Group::Paren, loc=node.loc)
  } else {
    @syntax.Expr::Tuple(exprs=lower_array_to_list(exprs), loc=node.loc)
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
  let mut is_iter = false
  if node.first_child_with_label("body") is Some(body) {
    is_iter = match
      body.first_child_with_label("open").map(CstNode::first_token) {
      Some(Some(LBRACKET_BAR)) => true
      _ => false
    }
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
  match item.first_child_with_label("op").map(CstNode::first_token) {
    Some(Some(DOTDOT)) =>
      @syntax.SpreadableElem::Spread(
        expr=item
          .first_child_with_label("expr")
          .map(lower_expr)
          .unwrap_or(lower_hole_expr(item.loc)),
        loc=item.loc,
      )
    _ => @syntax.SpreadableElem::Regular(lower_expr(item))
  }
}

///|
fn lower_list_comprehension_expr(node : CstNode) -> @syntax.Expr {
  let is_iter = match
    node.first_child_with_label("open").map(CstNode::first_token) {
    Some(Some(LBRACKET_BAR)) => true
    _ => false
  }
  let kind = if node.first_child_with_label("pattern") is Some(_) {
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
        .first_child_with_label("kw")
        .map(fn(node) { node.loc })
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
      if item.first_child_with_label("op") is Some(op) {
        match op.first_token() {
          Some(DOTDOT) =>
            record = item.first_child_with_label("expr").map(lower_expr)
          _ => fields.push(lower_record_field_def(item))
        }
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
  if record is Some(record) {
    @syntax.Expr::RecordUpdate(
      type_name~,
      record~,
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
    .unwrap_or(@syntax.Label::{ name: "", loc: node.loc })
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(
      @syntax.Expr::Ident(
        id=@syntax.Var::{
          name: @syntax.LongIdent::Ident(name=label.name),
          loc: label.loc,
        },
        loc=label.loc,
      ),
    )
  @syntax.FieldDef::{
    label,
    expr,
    is_pun: node.first_child_with_label("expr") is None,
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
  @syntax.WhereClause::{ fields: lower_array_to_list(fields), loc: node.loc }
}

///|
fn lower_record_trailing_mark(node : CstNode) -> @syntax.TrailingMark {
  let mut trailing = @syntax.TrailingMark::None
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some("item"), _)) => trailing = @syntax.TrailingMark::None
      Some((Some("sep"), sep)) =>
        match sep.first_token() {
          Some(COMMA) => trailing = @syntax.TrailingMark::Comma
          Some(SEMI(_)) => trailing = @syntax.TrailingMark::Semi
          _ => ()
        }
      _ => ()
    }
  }
  trailing
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
  let key_node = node.first_child_with_label("key")
  let (key, key_loc) = if key_node is Some(key_node) {
    if key_node.first_token() is Some(tok) {
      let key = if node.first_child_with_label("op").map(CstNode::first_token)
        is Some(Some(MINUS)) {
        match tok {
          INT(value) => Some(@syntax.make_int("-\{value}"))
          DOUBLE(value) => Some(@syntax.Constant::Double("-\{value}"))
          FLOAT(value) => Some(@syntax.make_float("-\{value}"))
          _ => lower_constant_from_token(tok)
        }
      } else {
        lower_constant_from_token(tok)
      }
      let key_loc = match node.first_child_with_label("op") {
        Some(op) if op.first_token() is Some(MINUS) =>
          op.loc.merge(key_node.loc)
        _ => key_node.loc
      }
      if key is Some(key) {
        (key, key_loc)
      } else {
        (@syntax.Constant::String(""), key_loc)
      }
    } else {
      (@syntax.Constant::String(""), key_node.loc)
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
  let op_node = node.first_child_with_label("op")
  let op_token = if op_node is Some(n) { n.first_token() } else { None }
  match op_token {
    Some(PIPE) => @syntax.Expr::Pipe(lhs~, rhs~, loc=node.loc)
    Some(PIPE_LEFT) => @syntax.Expr::RevPipe(lhs~, rhs~, loc=node.loc)
    Some(IMPLIES) => @syntax.Expr::Implies(lhs~, rhs~, loc=node.loc)
    Some(tok) => {
      let op_var = @syntax.Var::{
        name: @syntax.LongIdent::Ident(name=lower_token_name(tok).unwrap_or("")),
        loc: op_node.unwrap().loc,
      }
      @syntax.Expr::Infix(op=op_var, lhs~, rhs~, loc=node.loc)
    }
    None =>
      @syntax.Expr::Infix(
        op=@syntax.Var::{
          name: @syntax.LongIdent::Ident(name=""),
          loc: node.loc,
        },
        lhs~,
        rhs~,
        loc=node.loc,
      )
  }
}

///|
fn lower_prefix_expr(node : CstNode) -> @syntax.Expr {
  let op_token = if node.first_child_with_label("op") is Some(op_node) {
    op_node.first_token()
  } else {
    None
  }
  let expr_token = if node.first_child_with_label("expr") is Some(expr_node) {
    expr_node.first_token()
  } else {
    None
  }
  match (op_token, expr_token) {
    (Some(PLUS), Some(INT(_))) | (Some(PLUS), Some(DOUBLE(_))) =>
      return node
        .first_child_with_label("expr")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc))
    (Some(MINUS), Some(INT(value))) if !value.has_suffix("N") =>
      return @syntax.Expr::Constant(
        c=@syntax.make_int("-\{value}"),
        loc=node.loc,
      )
    _ => ()
  }
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let op_node = node.first_child_with_label("op").unwrap_or(node)
  let op_name = if op_node.first_token() is Some(tok) {
    lower_token_name(tok).unwrap_or("")
  } else {
    ""
  }
  if op_token is Some(PLUS) {
    return @syntax.make_uplus(loc=node.loc, "+", expr)
  }
  if op_token is Some(MINUS) {
    return @syntax.make_uminus(loc=node.loc, op_name, expr)
  }
  if op_token is Some(EXCLAMATION) {
    return @syntax.make_unot(loc=node.loc, expr)
  }
  let op = @syntax.Var::{
    name: @syntax.LongIdent::Ident(name=op_name),
    loc: op_node.loc,
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
    label=node.first_child_with_label("label").map(lower_label_from_node),
    loc=lower_loop_loc(node),
  )
}

///|
fn lower_assign_expr(node : CstNode) -> @syntax.Expr {
  let lhs = node.first_child_with_label("lhs")
  match lhs.map(lower_expr) {
    Some(@syntax.Expr::Ident(id~, ..)) => {
      let expr = node
        .first_child_with_label("rhs")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc))
      let augmented_by = match node.kind {
        Expr_AugmentedAssign =>
          node.first_child_with_label("op").map(lower_var_from_token_node)
        _ => None
      }
      @syntax.Expr::Assign(var_=id, expr~, augmented_by~, loc=node.loc)
    }
    Some(@syntax.Expr::ArrayGet(array~, index~, ..)) => {
      let value = node
        .first_child_with_label("rhs")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc))
      match node.kind {
        Expr_AugmentedAssign => {
          let op = node
            .first_child_with_label("op")
            .map(lower_var_from_token_node)
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
        }
        _ => @syntax.Expr::ArraySet(array~, index~, value~, loc=node.loc)
      }
    }
    Some(@syntax.Expr::Field(record~, accessor~, ..)) => {
      let field = node
        .first_child_with_label("rhs")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc))
      let augmented_by = match node.kind {
        Expr_AugmentedAssign =>
          node.first_child_with_label("op").map(lower_var_from_token_node)
        _ => None
      }
      @syntax.Expr::Mutate(
        record~,
        accessor~,
        field~,
        augmented_by~,
        loc=node.loc,
      )
    }
    Some(_) => lower_hole_expr(node.loc)
    None => lower_hole_expr(node.loc)
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
    label=node.first_child_with_label("label").map(lower_label_from_node),
    loc=lower_loop_loc(node),
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
    label=node.first_child_with_label("label").map(lower_label_from_node),
    loc=lower_loop_loc(node),
  )
}

///|
fn lower_loop_loc(node : CstNode) -> Location {
  for kw in node.children_with_label("kw") {
    match kw.first_token() {
      Some(FOR) | Some(WHILE) =>
        return Location::{ start: kw.loc.start, end: node.loc.end }
      _ => ()
    }
  }
  node.loc
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
    .unwrap_or(@syntax.Label::{ name: "", loc: node.loc })
  let args = node
    .first_child_with_label("arg")
    .map(lower_arguments)
    .unwrap_or(@list.empty())
  let return_self = node.first_child_with_label("op").map(CstNode::first_token)
    is Some(Some(DOTDOT))
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
    Expr_Constructor | Expr_Identifier =>
      @syntax.TypeName::{
        name: lower_long_ident_from_name_nodes(node.children_with_label("name")),
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
      .unwrap_or(@syntax.Label::{ name: "", loc: node.loc }),
    loc=node.loc,
  )
}

///|
fn lower_field_expr(node : CstNode) -> @syntax.Expr {
  let record = node
    .first_child_with_label("self")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let accessor = if node.first_child_with_label("field") is Some(field) {
    match field.first_token() {
      Some(DOT_INT(tuple_index)) =>
        @syntax.Accessor::Index(tuple_index~, loc=field.loc)
      _ => @syntax.Accessor::Label(lower_label_from_node(field))
    }
  } else {
    @syntax.Accessor::Label(@syntax.Label::{ name: "", loc: node.loc })
  }
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
    index_loc=node.loc,
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
    .children_with_label("kw")
    .get(0)
    .map(fn(node) { node.loc })
    .unwrap_or(node.loc)
  let has_try = if node.children_with_label("kw").get(0) is Some(kw) {
    match kw.first_token() {
      Some(TRY) => true
      _ => false
    }
  } else {
    false
  }
  let catch_kw_loc = if has_try {
    node
    .children_with_label("kw")
    .get(1)
    .map(fn(node) { node.loc })
    .unwrap_or(node.loc)
  } else {
    node
    .children_with_label("kw")
    .get(0)
    .map(fn(node) { node.loc })
    .unwrap_or(node.loc)
  }
  let catch_loc = if has_try {
    if node.children_with_label("case").get(0) is Some(case_list) {
      case_list
      .first_child_with_label("open")
      .map(open => catch_kw_loc.merge(open.loc))
      .unwrap_or(catch_kw_loc)
    } else {
      catch_kw_loc
    }
  } else {
    catch_kw_loc
  }
  let else_loc = match
    (
      node.first_child_with_label("else_kw"),
      node.children_with_label("else").get(0),
    ) {
    (Some(else_kw), Some(case_list)) =>
      case_list
      .first_child_with_label("open")
      .map(open => else_kw.loc.merge(open.loc))
      .unwrap_or(else_kw.loc)
    (Some(else_kw), None) => else_kw.loc
    _ => node.loc
  }
  let loc = if has_try {
    node.loc
  } else {
    let suffix_loc = if node.first_child_with_label("body") is Some(body) {
      lower_handrolled_suffix_loc(body, catch_kw_loc)
    } else {
      None
    }
    if suffix_loc is Some(suffix_loc) {
      Location::{ start: suffix_loc.start, end: node.loc.end }
    } else {
      node.loc
    }
  }
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
fn lower_handrolled_suffix_loc(
  node : CstNode,
  catch_kw_loc : Location,
) -> Location? {
  if lower_leading_parenthesized_end(node) is Some(paren_end) {
    Some(
      lower_first_significant_token_at_or_after(node, paren_end).unwrap_or(
        catch_kw_loc,
      ),
    )
  } else {
    None
  }
}

///|
fn lower_leading_parenthesized_end(node : CstNode) -> Position? {
  match node.kind {
    Expr_Tuple | Expr_Constraint => return Some(node.loc.end)
    _ => ()
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some("trivia"), _)) => continue
      Some((_, child)) => return lower_leading_parenthesized_end(child)
      None => ()
    }
  }
  None
}

///|
fn lower_first_significant_token_at_or_after(
  node : CstNode,
  pos : Position,
) -> Location? {
  match node.kind {
    Token(COMMENT(_) | NEWLINE) => ()
    Token(_) if node.loc.start.cnum >= pos.cnum => return Some(node.loc)
    _ => ()
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((_, child)) =>
        if lower_first_significant_token_at_or_after(child, pos) is Some(loc) {
          return Some(loc)
        } else {
          ()
        }
      None => ()
    }
  }
  None
}

///|
fn lower_try_operator_expr(node : CstNode) -> @syntax.Expr {
  let kind = match node.first_child_with_label("kw").map(CstNode::first_token) {
    Some(Some(TRY_EXCLAMATION)) => @syntax.TryOperatorKind::Exclamation
    _ => @syntax.TryOperatorKind::Question
  }
  @syntax.Expr::TryOperator(
    body=node
      .first_child_with_label("body")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    kind~,
    try_loc=node
      .first_child_with_label("kw")
      .map(fn(node) { node.loc })
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
  let cases : Array[@syntax.LexCase] = []
  for case_node in lower_lex_case_nodes(node) {
    cases.push(lower_lex_case(case_node))
  }
  @syntax.Expr::LexMatch(
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
  } else if pat_node.map(lower_node_is_ellipsis).unwrap_or(false) {
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
  @syntax.LexScanCase::{
    pat: node
    .first_child_with_label("pattern")
    .map(lower_lex_scan_case_pattern)
    .unwrap_or(@syntax.LexScanCasePattern::Wildcard(loc=node.loc)),
    guard_: node.first_child_with_label("cond").map(lower_expr),
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
    Pattern_Tuple if lower_pattern_tuple_has_separator(node) =>
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
  let items : Array[CstNode] = []
  for item in group {
    if !lower_lex_sequence_item_is_separator(item) {
      items.push(item)
    }
  }
  if items.length() == 0 {
    @syntax.LexTopPattern::Wildcard(loc~)
  } else if items.length() == 1 {
    lower_lex_top_pattern(items[0])
  } else {
    let pats : Array[@syntax.LexPattern] = []
    for item in items {
      pats.push(lower_lex_pattern(item))
    }
    @syntax.LexTopPattern::Pattern(
      @syntax.LexPattern::Sequence(pats=lower_array_to_list(pats), loc~),
    )
  }
}

///|
fn lower_lex_top_pattern(node : CstNode) -> @syntax.LexTopPattern {
  match node.kind {
    _ if lower_node_is_ellipsis(node) =>
      @syntax.LexTopPattern::Wildcard(loc=node.loc)
    Pattern_Tuple if !lower_pattern_tuple_has_separator(node) => {
      let items = lower_pattern_tuple_items(node)
      if items.length() == 1 {
        lower_lex_top_pattern(items[0])
      } else {
        @syntax.LexTopPattern::Pattern(lower_lex_pattern(node))
      }
    }
    Pattern_Underscore | Pattern_MissingPattern =>
      @syntax.LexTopPattern::Wildcard(loc=node.loc)
    Pattern_Binder =>
      @syntax.LexTopPattern::Binder(
        node
        .first_child_with_label("name")
        .map(lower_binder_from_node)
        .unwrap_or(lower_dummy_binder(node.loc)),
      )
    _ => @syntax.LexTopPattern::Pattern(lower_lex_pattern(node))
  }
}

///|
fn lower_node_is_ellipsis(node : CstNode) -> Bool {
  match node.first_token() {
    Some(ELLIPSIS) => true
    _ => false
  }
}

///|
fn lower_todo_hole_expr(loc : Location) -> @syntax.Expr {
  @syntax.Expr::Hole(loc~, kind=@syntax.Hole::Todo)
}

///|
fn lower_lex_pattern(node : CstNode) -> @syntax.LexPattern {
  match node.kind {
    Pattern_Literal | LexPattern_Literal => lower_lex_literal_pattern(node)
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
          if !lower_lex_sequence_item_is_separator(item) {
            pats.push(lower_lex_pattern(item))
          }
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
    Pattern_Constructor | LexPattern_ConstantRef =>
      @syntax.LexPattern::ConstantRef(
        lid=lower_long_ident_from_name_nodes(node.children_with_label("name")),
        loc=node.loc,
      )
    Pattern_Binder =>
      @syntax.LexPattern::ConstantRef(
        lid=@syntax.LongIdent::Ident(
          name=if node.first_child_with_label("name") is Some(name_node) {
            lower_token_child_name(name_node).unwrap_or("")
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
fn lower_lex_sequence_item_is_separator(node : CstNode) -> Bool {
  match node.first_token() {
    Some(SEMI(_)) => true
    _ => false
  }
}

///|
fn lower_lex_empty_regex(loc : Location) -> @syntax.LexPattern {
  @syntax.LexPattern::Regex(lit="", offset=1, loc~)
}

///|
fn lower_lex_literal_pattern(node : CstNode) -> @syntax.LexPattern {
  match node.first_token() {
    Some(REGEX_LITERAL(lit)) =>
      @syntax.LexPattern::Regex(lit~, offset=3, loc=node.loc)
    Some(STRING(lit)) => @syntax.LexPattern::Regex(lit~, offset=1, loc=node.loc)
    Some(tok) =>
      @syntax.LexPattern::ConstantRef(
        lid=@syntax.LongIdent::Ident(name=lower_token_name(tok).unwrap_or("")),
        loc=node.loc,
      )
    None => lower_lex_empty_regex(node.loc)
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
    RegexPattern_Literal | Pattern_Literal => lower_regex_literal_pattern(node)
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
    RegexPattern_Reference | Pattern_Constructor | Pattern_Binder =>
      @syntax.RegexPattern::Reference(
        lid=lower_long_ident_from_name_nodes(node.children_with_label("name")),
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
    .unwrap_or(@syntax.Label::{ name: "", loc: node.loc })
  let binder = node.first_child_with_label("binder").map(lower_binder_from_node)
  (label, binder)
}

///|
fn lower_empty_regex_pattern(loc : Location) -> @syntax.RegexPattern {
  @syntax.RegexPattern::Literal(lit="", loc~)
}

///|
fn lower_lex_scan_case_pattern(node : CstNode) -> @syntax.LexScanCasePattern {
  match node.kind {
    _ if lower_node_is_ellipsis(node) =>
      @syntax.LexScanCasePattern::Wildcard(loc=node.loc)
    Pattern_Underscore | Pattern_MissingPattern =>
      @syntax.LexScanCasePattern::Wildcard(loc=node.loc)
    Pattern_Binder =>
      @syntax.LexScanCasePattern::Binder(
        node
        .first_child_with_label("name")
        .map(lower_binder_from_node)
        .unwrap_or(lower_dummy_binder(node.loc)),
      )
    _ => @syntax.LexScanCasePattern::Pattern(lower_regex_pattern(node))
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
  match node.first_token() {
    Some(STRING(lit)) | Some(REGEX_LITERAL(lit)) =>
      @syntax.RegexPattern::Literal(lit~, loc=node.loc)
    Some(tok) =>
      @syntax.RegexPattern::Reference(
        lid=@syntax.LongIdent::Ident(name=lower_token_name(tok).unwrap_or("")),
        loc=node.loc,
      )
    None => lower_empty_regex_pattern(node.loc)
  }
}

///|
fn lower_parameter(node : CstNode) -> @syntax.Parameter {
  let pat = node.first_child_with_label("pattern")
  let ty = node.first_child_with_label("type").map(lower_type)
  if pat is Some(pat_node) {
    match pat_node.kind {
      Pattern_Underscore =>
        @syntax.Parameter::DiscardPositional(ty~, loc=pat_node.loc)
      Expr_Hole => @syntax.Parameter::DiscardPositional(ty~, loc=pat_node.loc)
      _ => {
        let binder = if pat_node.first_child_with_label("name") is Some(name) {
          lower_post_label_binder_from_node(name)
        } else {
          match pat_node.first_token() {
            Some(POST_LABEL(_)) => lower_post_label_binder_from_node(pat_node)
            _ => lower_dummy_binder(pat_node.loc)
          }
        }
        match pat_node.first_token() {
          Some(POST_LABEL(_)) =>
            if node.first_child_with_label("default") is Some(default) {
              @syntax.Parameter::Optional(
                binder~,
                default=lower_expr(default),
                ty~,
              )
            } else {
              @syntax.Parameter::Labelled(binder~, ty~)
            }
          _ if node.first_child_with_label("default") is Some(_) =>
            @syntax.Parameter::Optional(
              binder~,
              default=node
                .first_child_with_label("default")
                .map(lower_expr)
                .unwrap_or(lower_unit_expr(node.loc)),
              ty~,
            )
          _ if node.first_child_with_label("question") is Some(_) =>
            @syntax.Parameter::QuestionOptional(binder~, ty~)
          _ => @syntax.Parameter::Positional(binder~, ty~)
        }
      }
    }
  } else {
    @syntax.Parameter::DiscardPositional(ty~, loc=node.loc)
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
      match (ty, clause.first_child_with_label("clause")) {
        (
          @syntax.Type::Arrow(args~, res~, err=_, is_async~, loc~),
          Some(error_clause),
        ) if ty_node.kind is Type_Function =>
          Some(
            @syntax.Type::Arrow(
              args~,
              res~,
              err=lower_error_annotation(error_clause),
              is_async~,
              loc=loc.merge(error_clause.loc),
            ),
          )
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
  let kind = if node.first_child_with_label("op") is Some(op) {
    match op.first_token() {
      Some(FAT_ARROW) => @syntax.FnKind::Arrow
      _ => @syntax.FnKind::Lambda
    }
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
    loc: node.loc,
  }
  @syntax.Expr::Function(func~, loc=node.loc)
}

///|
fn lower_pipe_arrow_discard_loc(
  node : CstNode,
  params_node : CstNode?,
) -> Location? {
  guard node.first_child_with_label("pipe") is Some(_) else { return None }
  guard params_node is Some(params) else { return None }
  let items = lower_item_children(params)
  guard items.length() == 1 else { return None }
  let item_loc = items[0].loc
  guard params.loc.start == item_loc.start && params.loc.end == item_loc.end else {
    return None
  }
  node.first_child_with_label("op").map(fn(node) { node.loc })
}

///|
fn arrow_lambda_block_keeps_group(node : CstNode, body_node : CstNode) -> Bool {
  guard body_node.kind is Expr_Block else { return false }
  if node.first_child_with_label("pipe") is Some(_) {
    return false
  }
  match node.first_child_with_label("op").map(CstNode::first_token) {
    Some(Some(FAT_ARROW)) => true
    _ => false
  }
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
  let loc = if node.children_with_label("kw").get(0) is Some(kw) {
    Location::{ start: kw.loc.start, end: node.loc.end }
  } else {
    node.loc
  }
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
    loc=node.loc.merge(body.loc()),
  )
}

///|
fn lower_defer_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  @syntax.Expr::Defer(
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    body~,
    loc=node.loc.merge(body.loc()),
  )
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
    Expr_LetRec => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_letrec_expr_with_body(first, body)
    }
    Expr_Guard => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_guard_expr_with_body(first, body)
    }
    Expr_Defer => {
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
          Expr_LetRec => {
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
          Expr_Defer => {
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
    Expr_Defer =>
      lower_defer_expr_with_body(item, lower_faked_unit_expr(item.loc))
    _ => lower_expr(item)
  }
}

///|
fn lower_block_expr_node(node : CstNode) -> @syntax.Expr {
  if lower_block_as_record_pun(node) is Some(record) {
    return record
  } else {
    ()
  }
  let expr = if node.first_child_with_label("body") is Some(body) {
    lower_statement_sequence(lower_item_children(body), 0, node.loc)
  } else {
    lower_unit_expr(node.loc)
  }
  @syntax.Expr::Group(expr~, group=@syntax.Group::Brace, loc=node.loc)
}

///|
fn lower_block_as_record_pun(node : CstNode) -> @syntax.Expr? {
  if node.first_child_with_label("body") is Some(body) {
    let items = lower_item_children(body)
    match items.length() {
      1 =>
        match items[0].kind {
          Expr_Identifier =>
            if items[0].children_with_label("name").get(0) is Some(name_node) {
              let label = lower_label_from_node(name_node)
              Some(
                @syntax.Expr::Record(
                  type_name=None,
                  fields=@list.singleton(@syntax.FieldDef::{
                    label,
                    expr: lower_expr(items[0]),
                    is_pun: true,
                    loc: items[0].loc,
                  }),
                  trailing=lower_record_trailing_mark(body),
                  loc=node.loc,
                ),
              )
            } else {
              None
            }
          _ => None
        }
      _ => None
    }
  } else {
    None
  }
}

///|
fn lower_function_body_expr(node : CstNode) -> @syntax.Expr {
  match node.kind {
    Expr_Block =>
      if node.first_child_with_label("body") is Some(body) {
        lower_statement_sequence(lower_item_children(body), 0, node.loc)
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
    Pattern_Binder =>
      if node.first_child_with_label("name") is Some(name) {
        @syntax.Pattern::Var(lower_binder_from_node(name))
      } else {
        @syntax.Pattern::Any(loc=node.loc)
      }
    Pattern_Underscore | Pattern_MissingPattern =>
      @syntax.Pattern::Any(loc=node.loc)
    Pattern_Literal =>
      match node.first_token() {
        Some(MINUS) =>
          if node.first_child_with_label("token") is Some(number) {
            match number.first_token() {
              Some(INT(value)) =>
                @syntax.Pattern::Constant(
                  c=@syntax.make_int("-\{value}"),
                  loc=number.loc,
                )
              Some(DOUBLE(value)) =>
                @syntax.Pattern::Constant(
                  c=@syntax.Constant::Double("-\{value}"),
                  loc=number.loc,
                )
              Some(FLOAT(value)) =>
                @syntax.Pattern::Constant(
                  c=@syntax.make_float("-\{value}"),
                  loc=number.loc,
                )
              _ => @syntax.Pattern::Any(loc=node.loc)
            }
          } else {
            @syntax.Pattern::Any(loc=node.loc)
          }
        Some(tok) =>
          if lower_constant_from_token(tok) is Some(c) {
            @syntax.Pattern::Constant(c~, loc=node.loc)
          } else {
            @syntax.Pattern::Any(loc=node.loc)
          }
        None => @syntax.Pattern::Any(loc=node.loc)
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
    Pattern_Constructor => lower_constructor_pattern(node)
    Pattern_SpecialConstructor => lower_special_constructor_pattern(node)
    Pattern_Or => {
      let lhs = node
        .first_child_with_label("lhs")
        .map(lower_pattern)
        .unwrap_or(@syntax.Pattern::Any(loc=node.loc))
      let rhs = node
        .first_child_with_label("rhs")
        .map(lower_pattern)
        .unwrap_or(@syntax.Pattern::Any(loc=node.loc))
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
    _ => @syntax.Pattern::Any(loc=node.loc)
  }
}

///|
fn lower_pattern_range_kind(node : CstNode) -> @syntax.RangeKind {
  let op_token = if node.first_child_with_label("op") is Some(op) {
    op.first_token()
  } else {
    None
  }
  match op_token {
    Some(RANGE_EXCLUSIVE) => @syntax.RangeKind::Exclusive
    Some(RANGE_INCLUSIVE) => @syntax.RangeKind::Inclusive
    _ => @syntax.RangeKind::InclusiveMissingEqual
  }
}

///|
fn lower_array_patterns(node : CstNode) -> @syntax.ArrayPatterns {
  let prefix : Array[@syntax.ArrayPattern] = []
  let suffix : Array[@syntax.ArrayPattern] = []
  let mut rest : @syntax.DotDotBinder? = None
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      match item.kind {
        Pattern_Rest if rest is None && !array_rest_pattern_is_spread(item) =>
          rest = Some(lower_dot_dot_binder(item))
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
  if rest is Some(dot_dot_binder) {
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
fn array_rest_pattern_is_spread(node : CstNode) -> Bool {
  node.first_child_with_label("str") is Some(_) ||
  node.first_child_with_label("bytes") is Some(_) ||
  node.first_child_with_label("const") is Some(_)
}

///|
fn lower_array_pattern_item(node : CstNode) -> @syntax.ArrayPattern {
  match node.kind {
    Pattern_Rest =>
      if node.first_child_with_label("str") is Some(str_node) {
        @syntax.ArrayPattern::StringSpread(
          str=lower_token_child_name(str_node).unwrap_or(""),
          loc=str_node.loc,
        )
      } else if node.first_child_with_label("bytes") is Some(bytes_node) {
        @syntax.ArrayPattern::BytesSpread(
          bytes=lower_token_child_name(bytes_node).unwrap_or(""),
          loc=bytes_node.loc,
        )
      } else if node.first_child_with_label("const") is Some(name_node) {
        let pkg = if node.first_child_with_label("pkg") is Some(pkg_node) {
          lower_token_child_name(pkg_node)
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
    _ => @syntax.ArrayPattern::Pattern(lower_pattern(node))
  }
}

///|
fn lower_dot_dot_binder(node : CstNode) -> @syntax.DotDotBinder {
  if node.first_child_with_label("name") is Some(name_node) {
    match name_node.first_token() {
      Some(UNDERSCORE) => @syntax.DotDotBinder::Underscore
      Some(LIDENT(_)) | Some(UIDENT(_)) =>
        if node.first_child_with_label("as") is Some(_) {
          @syntax.DotDotBinder::BinderAs(lower_binder_from_node(name_node))
        } else {
          @syntax.DotDotBinder::Binder(lower_binder_from_node(name_node))
        }
      _ => @syntax.DotDotBinder::NoBinder
    }
  } else {
    @syntax.DotDotBinder::NoBinder
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
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      if item.kind is Pattern_Rest {
        return false
      }
    }
  } else {
    ()
  }
  true
}

///|
fn lower_record_pattern_field(node : CstNode) -> @syntax.FieldPat {
  let label_node = node.first_child_with_label("name")
  let label = label_node
    .map(lower_label_from_node)
    .unwrap_or(@syntax.Label::{ name: "", loc: node.loc })
  let pattern = node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(
      @syntax.Pattern::Var(@syntax.Binder::{ name: label.name, loc: label.loc }),
    )
  @syntax.FieldPat::{
    label,
    pattern,
    is_pun: node.first_child_with_label("pattern") is None,
    loc: node.loc,
  }
}

///|
fn lower_map_pattern_is_closed(node : CstNode) -> Bool {
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      if item.kind is Pattern_Rest {
        return false
      }
    }
  } else {
    ()
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
    match_absent: node.first_child_with_label("question") is Some(_),
    key_loc,
    loc: node.loc,
  }
}

///|
fn lower_map_pattern_key(node : CstNode) -> (@syntax.Constant, Location) {
  if node.first_child_with_label("key") is Some(key_node) {
    let key = match
      (node.first_child_with_label("op"), key_node.first_token()) {
      (Some(op), Some(INT(value))) if op.first_token() is Some(MINUS) =>
        @syntax.make_int("-\{value}")
      (Some(op), Some(DOUBLE(value))) if op.first_token() is Some(MINUS) =>
        @syntax.Constant::Double("-\{value}")
      (Some(op), Some(FLOAT(value))) if op.first_token() is Some(MINUS) =>
        @syntax.make_float("-\{value}")
      (_, Some(tok)) =>
        lower_constant_from_token(tok).unwrap_or(@syntax.Constant::String(""))
      _ => @syntax.Constant::String("")
    }
    let key_loc = match node.first_child_with_label("op") {
      Some(op) if op.first_token() is Some(MINUS) => op.loc.merge(key_node.loc)
      _ => key_node.loc
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
  // TODO: Reject the trailing-comma form `(pat,)` during CST parsing. Until
  // then, normalize it here to match the handrolled parser's AST.
  if items.length() == 1 {
    match items[0] {
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
  loc : Location,
) -> @syntax.Constructor {
  let name_node = if names.length() > 0 {
    names[names.length() - 1]
  } else {
    node(Token(UIDENT("")), loc, mk_span(0, 0), [])
  }
  let extra_info = if names.length() == 2 &&
    names[0].first_token() is Some(PACKAGE_NAME(_)) {
    @syntax.ConstructorExtraInfo::Package(
      lower_token_child_name(names[0]).unwrap_or(""),
    )
  } else if names.length() >= 2 {
    let type_nodes : Array[CstNode] = []
    for i in 0..<(names.length() - 1) {
      type_nodes.push(names[i])
    }
    let type_loc = type_nodes[0].loc.merge(
      type_nodes[type_nodes.length() - 1].loc,
    )
    @syntax.ConstructorExtraInfo::TypeName(@syntax.TypeName::{
      name: lower_long_ident_from_name_nodes(type_nodes),
      is_object: false,
      loc: type_loc,
    })
  } else {
    @syntax.ConstructorExtraInfo::NoExtraInfo
  }
  let loc = if names.length() > 0 {
    names[0].loc.merge(names[names.length() - 1].loc)
  } else {
    loc
  }
  @syntax.Constructor::{
    name: @syntax.ConstrName::{
      name: lower_token_child_name(name_node).unwrap_or(""),
      loc: name_node.loc,
    },
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
  if node.first_child_with_label("params") is Some(list) {
    for item in lower_item_children(list) {
      if item.kind is Pattern_Rest {
        return true
      }
    }
  } else {
    ()
  }
  false
}

///|
fn lower_constructor_pattern_arg(item : CstNode) -> @syntax.ConstrPatArg {
  match item.kind {
    Argument_LabelledPun => {
      let label_node = item.first_child_with_label("name").unwrap_or(item)
      let binder = lower_post_label_binder_from_node(label_node)
      @syntax.ConstrPatArg::{
        pat: @syntax.Pattern::Var(binder),
        kind: @syntax.ArgumentKind::LabelledPun(
          lower_label_from_node(label_node),
        ),
      }
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
          .unwrap_or(@syntax.Label::{ name: "", loc: item.loc }),
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
  @syntax.Pattern::Constr(
    constr=lower_constructor_from_names(
      node.children_with_label("name"),
      node.loc,
    ),
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
    Type_Function => lower_function_type(node)
    Type_Hole | Type_MissingType => @syntax.Type::Any(loc=node.loc)
    _ => @syntax.Type::Any(loc=node.loc)
  }
}

///|
fn lower_name_type(node : CstNode, is_object~ : Bool) -> @syntax.Type {
  let names = node.children_with_label("name")
  let constr_loc = if names.get(0) is Some(name) { name.loc } else { node.loc }
  let constr_id = lower_constr_id_from_name_nodes(names, constr_loc)
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
        let names = ty.children_with_label("name")
        @syntax.Type::Object(lower_constr_id_from_name_nodes(names, ty.loc))
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
  let question_loc = if node.first_child_with_label("op") is Some(op) {
    op.loc
  } else {
    node.loc
  }
  let loc = match inner_node {
    Some(ty) if option_type_wraps_object(ty) =>
      Location::{ start: inner.loc().start, end: node.loc.end }
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
  // TODO: Reject the trailing-comma form `(Type,)` during CST parsing. Until
  // then, normalize it here to match the handrolled parser's AST.
  if items.length() == 1 {
    items[0]
  } else {
    @syntax.Type::Tuple(tys=lower_array_to_list(items), loc=node.loc)
  }
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
      @syntax.Type::Arrow(args~, res~, err~, loc=_, is_async=_) =>
        @syntax.Type::Arrow(
          args~,
          res~,
          err~,
          is_async=Some(
            node
            .first_child_with_label("kw")
            .map(fn(node) { node.loc })
            .unwrap_or(node.loc),
          ),
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
        @syntax.TypeName::{ name: base.name, is_object: true, loc: node.loc }
      } else {
        @syntax.TypeName::{
          name: @syntax.LongIdent::dummy(),
          is_object: true,
          loc: node.loc,
        }
      }
    Type_Name =>
      @syntax.TypeName::{
        name: lower_long_ident_from_name_nodes(node.children_with_label("name")),
        is_object: node.first_child_with_label("op") is Some(_),
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
