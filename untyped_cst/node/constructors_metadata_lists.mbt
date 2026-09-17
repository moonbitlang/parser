///|
pub fn CstNode::fn_kind_lambda(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::FnKind_Lambda, loc, source_span, children)
}

///|
pub fn CstNode::fn_kind_arrow(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::FnKind_Arrow, loc, source_span, children)
}

///|
pub fn CstNode::group_brace(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Group_Brace, loc, source_span, children)
}

///|
pub fn CstNode::group_paren(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Group_Paren, loc, source_span, children)
}

///|
pub fn CstNode::trailing_mark_comma(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TrailingMark_Comma, loc, source_span, children)
}

///|
pub fn CstNode::trailing_mark_semi(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TrailingMark_Semi, loc, source_span, children)
}

///|
pub fn CstNode::trailing_mark_none(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TrailingMark_None, loc, source_span, children)
}

///|
pub fn CstNode::try_operator_kind_exclamation(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TryOperatorKind_Exclamation, loc, source_span, children)
}

///|
pub fn CstNode::quantifier_kind_forall(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::QuantifierKind_Forall, loc, source_span, children)
}

///|
pub fn CstNode::quantifier_kind_exists(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::QuantifierKind_Exists, loc, source_span, children)
}

///|
pub fn CstNode::using_kind_value(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::UsingKind_Value, loc, source_span, children)
}

///|
pub fn CstNode::using_kind_type(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::UsingKind_Type, loc, source_span, children)
}

///|
pub fn CstNode::using_kind_trait(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::UsingKind_Trait, loc, source_span, children)
}

///|
pub fn CstNode::openness_open(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Openness_Open, loc, source_span, children)
}

///|
pub fn CstNode::openness_closed(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Openness_Closed, loc, source_span, children)
}

///|
pub fn CstNode::array_kind_array(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::ArrayKind_Array, loc, source_span, children)
}

///|
pub fn CstNode::array_kind_iter(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::ArrayKind_Iter, loc, source_span, children)
}

///|
pub fn CstNode::flag_async(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_Async,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_declare(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_Declare,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_mutable(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_Mutable,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_pun(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_Pun,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_return_self(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_ReturnSelf,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_conditional(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_Conditional,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_has_try(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_HasTry,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_match_absent(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_MatchAbsent,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_faked(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_Faked,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_augmented(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_Augmented,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_object(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_Object,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::flag_exclamation(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Flag_Exclamation,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::attribute_simple(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  raw_payload~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  children.push((Some("raw_payload"), raw_payload))
  node(NodeKind::Attribute_Simple, loc, source_span, children)
}

///|
pub fn CstNode::attribute_qualified(
  loc~ : Location,
  source_span~ : (Int, Int),
  qualifier~ : CstNode,
  name~ : CstNode,
  raw_payload~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("qualifier"), qualifier))
  children.push((Some("name"), name))
  children.push((Some("raw_payload"), raw_payload))
  node(NodeKind::Attribute_Qualified, loc, source_span, children)
}

///|
pub fn CstNode::attribute_name(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Attribute_Name,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::attribute_qualifier(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Attribute_Qualifier,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::attribute_raw_payload(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Attribute_RawPayload,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::comment_inline_trailing(
  loc~ : Location,
  source_span~ : (Int, Int),
  content~ : CstNode,
  consumed_by_docstring~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("content"), content))
  children.push((Some("consumed_by_docstring"), consumed_by_docstring))
  node(NodeKind::Comment_InlineTrailing, loc, source_span, children)
}

///|
pub fn CstNode::comment_ownline(
  loc~ : Location,
  source_span~ : (Int, Int),
  content~ : CstNode,
  leading_blank_line~ : CstNode,
  trailing_blank_line~ : CstNode,
  consumed_by_docstring~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("content"), content))
  children.push((Some("leading_blank_line"), leading_blank_line))
  children.push((Some("trailing_blank_line"), trailing_blank_line))
  children.push((Some("consumed_by_docstring"), consumed_by_docstring))
  node(NodeKind::Comment_Ownline, loc, source_span, children)
}

///|
pub fn CstNode::comment_content(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Comment_Content,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::comment_leading_blank_line(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Comment_LeadingBlankLine,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::comment_trailing_blank_line(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Comment_TrailingBlankLine,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::comment_consumed_by_docstring(
  value~ : Bool,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::Comment_ConsumedByDocstring,
    loc,
    source_span,
    [],
    NodePayload::Bool(value),
  )
}

///|
pub fn CstNode::interp_segment_literal(
  value~ : String,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node_with_payload(
    NodeKind::InterpSegment_Literal,
    loc,
    source_span,
    [],
    NodePayload::String(value),
  )
}

///|
pub fn CstNode::interp_segment_source(
  loc~ : Location,
  source_span~ : (Int, Int),
  expr~ : CstNode,
  payload~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("expr"), expr))
  children.push((Some("payload"), payload))
  node(NodeKind::InterpSegment_Source, loc, source_span, children)
}

///|
pub fn CstNode::clause_return_type(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Clause_ReturnType,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::clause_where(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  children.push((Some("body"), body))
  node(NodeKind::Clause_Where, loc, source_span, children)
}

///|
/// Use clause_constructor_decl_from_entries to retain interleaved trivia and
/// recovery children in their original order.
pub fn CstNode::clause_constructor_decl(
  loc~ : Location,
  source_span~ : (Int, Int),
  attrs~ : ArrayView[CstNode],
  error? : CstNode,
  vis? : CstNode,
  name? : CstNode,
  params? : CstNode,
  op? : CstNode,
  tag? : CstNode,
  constructor_tag? : CstNode,
  semantic_name? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in attrs {
    children.push((Some("attrs"), child))
  }
  match error {
    Some(child) => children.push((Some("error"), child))
    None => ()
  }
  match vis {
    Some(child) => children.push((Some("vis"), child))
    None => ()
  }
  match name {
    Some(child) => children.push((Some("name"), child))
    None => ()
  }
  match params {
    Some(child) => children.push((Some("params"), child))
    None => ()
  }
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  match tag {
    Some(child) => children.push((Some("tag"), child))
    None => ()
  }
  match constructor_tag {
    Some(child) => children.push((Some("constructor_tag"), child))
    None => ()
  }
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  node(NodeKind::Clause_ConstructorDecl, loc, source_span, children)
}

///|
/// Constructs a constructor declaration from ordered children, preserving every
/// entry and its location, including repeated labels, trivia, and recovery nodes.
pub fn CstNode::clause_constructor_decl_from_entries(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Clause_ConstructorDecl,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::clause_using_item(
  loc~ : Location,
  source_span~ : (Int, Int),
  kind? : CstNode,
  target~ : CstNode,
  kw? : CstNode,
  name? : CstNode,
  semantic_name? : CstNode,
  using_kind? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match kind {
    Some(child) => children.push((Some("kind"), child))
    None => ()
  }
  children.push((Some("target"), target))
  match kw {
    Some(child) => children.push((Some("kw"), child))
    None => ()
  }
  match name {
    Some(child) => children.push((Some("name"), child))
    None => ()
  }
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  match using_kind {
    Some(child) => children.push((Some("using_kind"), child))
    None => ()
  }
  node(NodeKind::Clause_UsingItem, loc, source_span, children)
}

///|
pub fn CstNode::clause_import_package(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Clause_ImportPackage,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::clause_extend_item(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Clause_ExtendItem,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::clause_with_binding(
  loc~ : Location,
  source_span~ : (Int, Int),
  error~ : ArrayView[CstNode],
  name? : CstNode,
  op? : CstNode,
  expr? : CstNode,
  semantic_name? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in error {
    children.push((Some("error"), child))
  }
  match name {
    Some(child) => children.push((Some("name"), child))
    None => ()
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
  node(NodeKind::Clause_WithBinding, loc, source_span, children)
}

///|
pub fn CstNode::clause_deriving_directive(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : ArrayView[CstNode],
  arg? : CstNode,
  semantic_name? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in name {
    children.push((Some("name"), child))
  }
  match arg {
    Some(child) => children.push((Some("arg"), child))
    None => ()
  }
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  node(NodeKind::Clause_DerivingDirective, loc, source_span, children)
}

///|
pub fn CstNode::list_argument_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ArgumentList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_parameter_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ParameterList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_trait_method_parameter_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_TraitMethodParameterList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_type_decl_parameter_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_TypeDeclParameterList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_type_var_binder_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_TypeVarBinderList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_constructor_parameter_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ConstructorParameterList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_tuple_struct_type_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_TupleStructTypeList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_struct_field_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_StructFieldList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_trait_method_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_TraitMethodList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_record_field_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_RecordFieldList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_map_field_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_MapFieldList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_block_statement_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_BlockStatementList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_continue_argument_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ContinueArgumentList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_tuple_element_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_TupleElementList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_tuple_type_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_TupleTypeList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_array_element_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ArrayElementList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_type_argument_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_TypeArgumentList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_tuple_pattern_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_TuplePatternList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_constructor_pattern_argument_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ConstructorPatternArgumentList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_array_pattern_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ArrayPatternList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_record_pattern_field_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_RecordPatternFieldList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_map_pattern_field_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_MapPatternFieldList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_where_field_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_WhereFieldList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_object_field_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ObjectFieldList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_constructor_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ConstructorList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_using_item_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_UsingItemList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_import_package_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ImportPackageList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_extend_item_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ExtendItemList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_deriving_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_DerivingList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_match_case_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_MatchCaseList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_lex_scan_case_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_LexScanCaseList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_regex_binding_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_RegexBindingList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_for_binding_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ForBindingList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_for_each_binding_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_ForEachBindingList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}

///|
pub fn CstNode::list_with_binding_list(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstListChild],
) -> CstNode {
  node(
    NodeKind::List_WithBindingList,
    loc,
    source_span,
    entries_from_cstlistchild(entries),
  )
}
