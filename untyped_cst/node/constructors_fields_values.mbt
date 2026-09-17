///|
pub fn CstNode::parameter_type_var(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Parameter_TypeVar,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
/// Constructs a positional constructor parameter in mut, op, type, mutable order.
/// The optional op retains a consumed colon when the parameter name is missing.
pub fn CstNode::parameter_constructor_positional(
  loc~ : Location,
  source_span~ : (Int, Int),
  mut_? : CstNode,
  op? : CstNode,
  type_~ : CstNode,
  mutable? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match mut_ {
    Some(child) => children.push((Some("mut"), child))
    None => ()
  }
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  children.push((Some("type"), type_))
  match mutable {
    Some(child) => children.push((Some("mutable"), child))
    None => ()
  }
  node(NodeKind::Parameter_ConstructorPositional, loc, source_span, children)
}

///|
pub fn CstNode::parameter_constructor_labelled(
  loc~ : Location,
  source_span~ : (Int, Int),
  mut_? : CstNode,
  name~ : CstNode,
  op~ : CstNode,
  type_~ : CstNode,
  mutable? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match mut_ {
    Some(child) => children.push((Some("mut"), child))
    None => ()
  }
  children.push((Some("name"), name))
  children.push((Some("op"), op))
  children.push((Some("type"), type_))
  match mutable {
    Some(child) => children.push((Some("mutable"), child))
    None => ()
  }
  node(NodeKind::Parameter_ConstructorLabelled, loc, source_span, children)
}

///|
pub fn CstNode::field_declaration(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Field_Declaration,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::field_definition(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Field_Definition,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::field_pattern(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Field_Pattern,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::field_map_expression(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Field_MapExpression,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::field_map_pattern(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Field_MapPattern,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::field_where(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  op~ : CstNode,
  expr~ : CstNode,
  semantic_name? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  children.push((Some("op"), op))
  children.push((Some("expr"), expr))
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  node(NodeKind::Field_Where, loc, source_span, children)
}

///|
pub fn CstNode::field_record_spread(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
  expr~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  children.push((Some("expr"), expr))
  node(NodeKind::Field_RecordSpread, loc, source_span, children)
}

///|
pub fn CstNode::binding_let_rec(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Binding_LetRec,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::binding_for(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  error~ : ArrayView[CstNode],
  op? : CstNode,
  expr? : CstNode,
  semantic_name? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  for child in error {
    children.push((Some("error"), child))
  }
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  match expr {
    Some(child) => children.push((Some("expr"), child))
    None => ()
  }
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  node(NodeKind::Binding_For, loc, source_span, children)
}

///|
pub fn CstNode::binding_for_each(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  op~ : CstNode,
  expr~ : CstNode,
  semantic_name? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  children.push((Some("op"), op))
  children.push((Some("expr"), expr))
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  node(NodeKind::Binding_ForEach, loc, source_span, children)
}

///|
pub fn CstNode::binding_regex(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  op? : CstNode,
  binder? : CstNode,
  semantic_label? : CstNode,
  semantic_binder? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  match binder {
    Some(child) => children.push((Some("binder"), child))
    None => ()
  }
  match semantic_label {
    Some(child) => children.push((Some("semantic_label"), child))
    None => ()
  }
  match semantic_binder {
    Some(child) => children.push((Some("semantic_binder"), child))
    None => ()
  }
  node(NodeKind::Binding_Regex, loc, source_span, children)
}

///|
pub fn CstNode::trait_method_declaration(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::TraitMethod_Declaration,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::name_identifier(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Name_Identifier,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::name_package(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Name_Package,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::long_ident_ident(
  loc~ : Location,
  source_span~ : (Int, Int),
  component~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("component"), component))
  node(NodeKind::LongIdent_Ident, loc, source_span, children)
}

///|
pub fn CstNode::long_ident_dot(
  loc~ : Location,
  source_span~ : (Int, Int),
  component~ : ArrayView[CstNode],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in component {
    children.push((Some("component"), child))
  }
  node(NodeKind::LongIdent_Dot, loc, source_span, children)
}

///|
pub fn CstNode::name_binder(
  loc~ : Location,
  source_span~ : (Int, Int),
  long_ident~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("long_ident"), long_ident))
  node(NodeKind::Name_Binder, loc, source_span, children)
}

///|
pub fn CstNode::name_label(
  loc~ : Location,
  source_span~ : (Int, Int),
  long_ident? : CstNode,
  payload? : CstNode,
  syntax? : CstNode,
  name_value? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match long_ident {
    Some(child) => children.push((Some("long_ident"), child))
    None => ()
  }
  match payload {
    Some(child) => children.push((Some("payload"), child))
    None => ()
  }
  match syntax {
    Some(child) => children.push((Some("syntax"), child))
    None => ()
  }
  match name_value {
    Some(child) => children.push((Some("name_value"), child))
    None => ()
  }
  node(NodeKind::Name_Label, loc, source_span, children)
}

///|
pub fn CstNode::name_type(
  loc~ : Location,
  source_span~ : (Int, Int),
  long_ident~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("long_ident"), long_ident))
  node(NodeKind::Name_Type, loc, source_span, children)
}

///|
pub fn CstNode::name_constr(
  loc~ : Location,
  source_span~ : (Int, Int),
  long_ident~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("long_ident"), long_ident))
  node(NodeKind::Name_Constr, loc, source_span, children)
}

///|
pub fn CstNode::name_constr_id(
  loc~ : Location,
  source_span~ : (Int, Int),
  long_ident~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("long_ident"), long_ident))
  node(NodeKind::Name_ConstrId, loc, source_span, children)
}

///|
pub fn CstNode::name_var(
  loc~ : Location,
  source_span~ : (Int, Int),
  long_ident~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("long_ident"), long_ident))
  node(NodeKind::Name_Var, loc, source_span, children)
}

///|
pub fn CstNode::name_constructor(
  loc~ : Location,
  source_span~ : (Int, Int),
  long_ident~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("long_ident"), long_ident))
  node(NodeKind::Name_Constructor, loc, source_span, children)
}

///|
pub fn CstNode::name_field(
  loc~ : Location,
  source_span~ : (Int, Int),
  long_ident~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("long_ident"), long_ident))
  node(NodeKind::Name_Field, loc, source_span, children)
}

///|
pub fn CstNode::name_alias_target(
  loc~ : Location,
  source_span~ : (Int, Int),
  long_ident~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("long_ident"), long_ident))
  node(NodeKind::Name_AliasTarget, loc, source_span, children)
}

///|
pub fn CstNode::accessor_label(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Accessor_Label,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::accessor_index(
  value~ : Int,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Accessor_Index,
    loc,
    source_span,
    [],
    NodePayload::Int(value),
  )
}

///|
pub fn CstNode::accessor_newtype(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Accessor_Newtype, loc, source_span, children)
}

///|
pub fn CstNode::name_test(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Name_Test,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_bool(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_Bool,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::constant_byte(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_Byte,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_bytes(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_Bytes,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_char(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_Char,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_int(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_Int,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_int64(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_Int64,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_uint(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_UInt,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_uint64(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_UInt64,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_float(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_Float,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_double(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_Double,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_string(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_String,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_regex(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_Regex,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::constant_big_int(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Constant_BigInt,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::operator_infix(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Operator_Infix,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::operator_augmented(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Operator_Augmented,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::operator_unary_plus(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Operator_UnaryPlus,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::operator_unary_minus(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Operator_UnaryMinus,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::operator_unary_not(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Operator_UnaryNot,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::list_comprehension_kind_for(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::ListComprehensionKind_For, loc, source_span, children)
}

///|
pub fn CstNode::list_comprehension_kind_for_each(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::ListComprehensionKind_ForEach, loc, source_span, children)
}

///|
pub fn CstNode::dot_dot_binder_none(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::DotDotBinder_None, loc, source_span, children)
}

///|
pub fn CstNode::dot_dot_binder_underscore(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::DotDotBinder_Underscore, loc, source_span, children)
}

///|
pub fn CstNode::dot_dot_binder_binder(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::DotDotBinder_Binder, loc, source_span, children)
}

///|
pub fn CstNode::dot_dot_binder_binder_as(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::DotDotBinder_BinderAs, loc, source_span, children)
}

///|
pub fn CstNode::constructor_extra_info_none(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::ConstructorExtraInfo_None, loc, source_span, children)
}

///|
pub fn CstNode::constructor_extra_info_package(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::ConstructorExtraInfo_Package, loc, source_span, children)
}

///|
pub fn CstNode::constructor_extra_info_type_name(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::ConstructorExtraInfo_TypeName, loc, source_span, children)
}

///|
pub fn CstNode::constructor_extra_info_type_name_with_constr_package(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(
    NodeKind::ConstructorExtraInfo_TypeNameWithConstrPackage,
    loc,
    source_span,
    children,
  )
}

///|
pub fn CstNode::multiline_string_element_string(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::MultilineStringElement_String,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::multiline_string_element_interp(
  loc~ : Location,
  source_span~ : (Int, Int),
  segment~ : ArrayView[CstNode],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in segment {
    children.push((Some("segment"), child))
  }
  node(NodeKind::MultilineStringElement_Interp, loc, source_span, children)
}

///|
pub fn CstNode::lex_literal_string(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::LexLiteral_String,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::lex_literal_regex(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::LexLiteral_Regex,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::lex_scan_case_pattern_pattern(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::LexScanCasePattern_Pattern, loc, source_span, children)
}

///|
pub fn CstNode::lex_scan_case_pattern_binder(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::LexScanCasePattern_Binder, loc, source_span, children)
}

///|
pub fn CstNode::lex_scan_case_pattern_wildcard(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::LexScanCasePattern_Wildcard, loc, source_span, children)
}

///|
pub fn CstNode::constructor_tag_value(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::ConstructorTag_Value,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::language_value(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Language_Value,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::stub_module_name(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Stub_ModuleName,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::stub_function_name(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Stub_FunctionName,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::stub_code(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Stub_Code,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::aggregate_span(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Aggregate_Span, loc, source_span, children)
}

///|
pub fn CstNode::decl_body_expr(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::DeclBody_Expr, loc, source_span, children)
}

///|
pub fn CstNode::decl_body_import(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::DeclBody_Import, loc, source_span, children)
}

///|
pub fn CstNode::decl_body_embedded_string(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::DeclBody_EmbeddedString, loc, source_span, children)
}

///|
pub fn CstNode::decl_body_embedded_multiline_string(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::DeclBody_EmbeddedMultilineString, loc, source_span, children)
}

///|
pub fn CstNode::decl_body_none(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::DeclBody_None, loc, source_span, children)
}

///|
pub fn CstNode::visibility_default(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Visibility_Default, loc, source_span, children)
}

///|
pub fn CstNode::visibility_pub(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Visibility_Pub,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::visibility_priv(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  node(NodeKind::Visibility_Priv, loc, source_span, children)
}

///|
pub fn CstNode::range_kind_inclusive(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::RangeKind_Inclusive, loc, source_span, children)
}

///|
pub fn CstNode::range_kind_exclusive(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::RangeKind_Exclusive, loc, source_span, children)
}

///|
pub fn CstNode::hole_kind_incomplete(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::HoleKind_Incomplete, loc, source_span, children)
}

///|
pub fn CstNode::hole_kind_todo(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::HoleKind_Todo, loc, source_span, children)
}
