///|
pub fn CstNode::impls(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRootChild],
) -> CstNode {
  node(NodeKind::Impls, loc, source_span, entries_from_cstrootchild(entries))
}

///|
pub fn CstNode::expression(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRootChild],
) -> CstNode {
  node(
    NodeKind::Expression,
    loc,
    source_span,
    entries_from_cstrootchild(entries),
  )
}

///|
pub fn CstNode::missing(loc~ : Location, source_span~ : (Int, Int)) -> CstNode {
  node(NodeKind::Missing, loc, source_span, [])
}

///|
pub fn CstNode::error(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  error_node(loc, source_span, entries_from_cstrecoverychild(entries))
}

///|
pub fn CstNode::syntax_keyword(
  loc~ : Location,
  source_span~ : (Int, Int),
  flag? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match flag {
    Some(child) => children.push((Some("flag"), child))
    None => ()
  }
  node(NodeKind::Syntax_Keyword, loc, source_span, children)
}

///|
pub fn CstNode::syntax_delimiter(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Syntax_Delimiter, loc, source_span, children)
}

///|
pub fn CstNode::syntax_separator(
  loc~ : Location,
  source_span~ : (Int, Int),
  trailing? : CstNode,
  payload? : NodePayload,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match trailing {
    Some(child) => children.push((Some("trailing"), child))
    None => ()
  }
  match payload {
    Some(value) => push_payload_leaf(children, value, loc, source_span)
    None => ()
  }
  node(NodeKind::Syntax_Separator, loc, source_span, children)
}

///|
pub fn CstNode::syntax_operator(
  loc~ : Location,
  source_span~ : (Int, Int),
  flag? : CstNode,
  name_value? : CstNode,
  operator? : CstNode,
  payload? : NodePayload,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match flag {
    Some(child) => children.push((Some("flag"), child))
    None => ()
  }
  match name_value {
    Some(child) => children.push((Some("name_value"), child))
    None => ()
  }
  match operator {
    Some(child) => children.push((Some("operator"), child))
    None => ()
  }
  match payload {
    Some(value) => push_payload_leaf(children, value, loc, source_span)
    None => ()
  }
  node(NodeKind::Syntax_Operator, loc, source_span, children)
}

///|
pub fn CstNode::syntax_identifier(
  loc~ : Location,
  source_span~ : (Int, Int),
  name_value? : CstNode,
  payload? : NodePayload,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match name_value {
    Some(child) => children.push((Some("name_value"), child))
    None => ()
  }
  match payload {
    Some(value) => push_payload_leaf(children, value, loc, source_span)
    None => ()
  }
  node(NodeKind::Syntax_Identifier, loc, source_span, children)
}

///|
pub fn CstNode::syntax_label(
  loc~ : Location,
  source_span~ : (Int, Int),
  name_value? : CstNode,
  payload? : NodePayload,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match name_value {
    Some(child) => children.push((Some("name_value"), child))
    None => ()
  }
  match payload {
    Some(value) => push_payload_leaf(children, value, loc, source_span)
    None => ()
  }
  node(NodeKind::Syntax_Label, loc, source_span, children)
}

///|
pub fn CstNode::syntax_accessor(
  loc~ : Location,
  source_span~ : (Int, Int),
  name_value? : CstNode,
  accessor? : CstNode,
  payload? : NodePayload,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match name_value {
    Some(child) => children.push((Some("name_value"), child))
    None => ()
  }
  match accessor {
    Some(child) => children.push((Some("accessor"), child))
    None => ()
  }
  match payload {
    Some(value) => push_payload_leaf(children, value, loc, source_span)
    None => ()
  }
  node(NodeKind::Syntax_Accessor, loc, source_span, children)
}

///|
pub fn CstNode::syntax_literal(
  loc~ : Location,
  source_span~ : (Int, Int),
  constant? : CstNode,
  payload? : NodePayload,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match constant {
    Some(child) => children.push((Some("constant"), child))
    None => ()
  }
  match payload {
    Some(value) => push_payload_leaf(children, value, loc, source_span)
    None => ()
  }
  node(NodeKind::Syntax_Literal, loc, source_span, children)
}

///|
pub fn CstNode::syntax_interpolation(
  loc~ : Location,
  source_span~ : (Int, Int),
  segment~ : ArrayView[CstNode],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in segment {
    children.push((Some("segment"), child))
  }
  node(NodeKind::Syntax_Interpolation, loc, source_span, children)
}

///|
pub fn CstNode::syntax_attribute(
  loc~ : Location,
  source_span~ : (Int, Int),
  attribute? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match attribute {
    Some(child) => children.push((Some("attribute"), child))
    None => ()
  }
  node(NodeKind::Syntax_Attribute, loc, source_span, children)
}

///|
pub fn CstNode::syntax_comment(
  loc~ : Location,
  source_span~ : (Int, Int),
  comment? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match comment {
    Some(child) => children.push((Some("comment"), child))
    None => ()
  }
  node(NodeKind::Syntax_Comment, loc, source_span, children)
}

///|
pub fn CstNode::syntax_eof(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Syntax_Eof, loc, source_span, children)
}

///|
pub fn CstNode::syntax_unexpected(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Syntax_Unexpected, loc, source_span, children)
}

///|
pub fn CstNode::impl_import(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_Import,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_extend(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_Extend,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_using(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_Using,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_function(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_Function,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_let(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_Let,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_const(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_Const,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_type_abstract(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_TypeAbstract,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_type_alias(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_TypeAlias,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_record_struct(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_RecordStruct,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_tuple_struct(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_TupleStruct,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_enum_variant(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_EnumVariant,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_extensible_enum(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_ExtensibleEnum,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_extend_enum(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_ExtendEnum,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_suberror(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_Suberror,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_suberror_enum(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_SuberrorEnum,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_enum_view(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_EnumView,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_trait(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_Trait,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_impl_method(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_ImplMethod,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_impl_relation(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_ImplRelation,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::impl_test(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstDeclarationChild],
) -> CstNode {
  node(
    NodeKind::Impl_Test,
    loc,
    source_span,
    entries_from_cstdeclarationchild(entries),
  )
}

///|
pub fn CstNode::expr_ident(
  loc~ : Location,
  source_span~ : (Int, Int),
  async_loc? : CstNode,
  name~ : ArrayView[CstNode],
  token? : CstNode,
  semantic_name? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match async_loc {
    Some(child) => children.push((Some("async_loc"), child))
    None => ()
  }
  for child in name {
    children.push((Some("name"), child))
  }
  match token {
    Some(child) => children.push((Some("token"), child))
    None => ()
  }
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  node(NodeKind::Expr_Ident, loc, source_span, children)
}

///|
pub fn CstNode::expr_constant(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_Constant,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_interp(
  loc~ : Location,
  source_span~ : (Int, Int),
  token~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("token"), token))
  node(NodeKind::Expr_Interp, loc, source_span, children)
}

///|
pub fn CstNode::expr_bytes_interp(
  loc~ : Location,
  source_span~ : (Int, Int),
  token~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("token"), token))
  node(NodeKind::Expr_BytesInterp, loc, source_span, children)
}

///|
pub fn CstNode::expr_block(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
  group? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  match group {
    Some(child) => children.push((Some("group"), child))
    None => ()
  }
  node(NodeKind::Expr_Block, loc, source_span, children)
}

///|
pub fn CstNode::expr_apply(
  loc~ : Location,
  source_span~ : (Int, Int),
  op? : CstNode,
  operator? : CstNode,
  func~ : CstNode,
  arg~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  match operator {
    Some(child) => children.push((Some("operator"), child))
    None => ()
  }
  children.push((Some("func"), func))
  children.push((Some("arg"), arg))
  node(NodeKind::Expr_Apply, loc, source_span, children)
}

///|
pub fn CstNode::expr_dot_apply(
  loc~ : Location,
  source_span~ : (Int, Int),
  self_~ : CstNode,
  op? : CstNode,
  error~ : ArrayView[CstNode],
  token? : CstNode,
  field? : CstNode,
  arg? : CstNode,
  return_self? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("self"), self_))
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  for child in error {
    children.push((Some("error"), child))
  }
  match token {
    Some(child) => children.push((Some("token"), child))
    None => ()
  }
  match field {
    Some(child) => children.push((Some("field"), child))
    None => ()
  }
  match arg {
    Some(child) => children.push((Some("arg"), child))
    None => ()
  }
  match return_self {
    Some(child) => children.push((Some("return_self"), child))
    None => ()
  }
  node(NodeKind::Expr_DotApply, loc, source_span, children)
}

///|
pub fn CstNode::expr_field(
  loc~ : Location,
  source_span~ : (Int, Int),
  self_~ : CstNode,
  token~ : CstNode,
  field~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("self"), self_))
  children.push((Some("token"), token))
  children.push((Some("field"), field))
  node(NodeKind::Expr_Field, loc, source_span, children)
}

///|
pub fn CstNode::expr_array_get(
  loc~ : Location,
  source_span~ : (Int, Int),
  self_~ : CstNode,
  open~ : CstNode,
  index~ : CstNode,
  close~ : CstNode,
  index_loc? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("self"), self_))
  children.push((Some("open"), open))
  children.push((Some("index"), index))
  children.push((Some("close"), close))
  match index_loc {
    Some(child) => children.push((Some("index_loc"), child))
    None => ()
  }
  node(NodeKind::Expr_ArrayGet, loc, source_span, children)
}

///|
pub fn CstNode::expr_array_get_slice(
  loc~ : Location,
  source_span~ : (Int, Int),
  self_~ : CstNode,
  open~ : CstNode,
  start? : CstNode,
  op~ : CstNode,
  end? : CstNode,
  close~ : CstNode,
  index_loc? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("self"), self_))
  children.push((Some("open"), open))
  match start {
    Some(child) => children.push((Some("start"), child))
    None => ()
  }
  children.push((Some("op"), op))
  match end {
    Some(child) => children.push((Some("end"), child))
    None => ()
  }
  children.push((Some("close"), close))
  match index_loc {
    Some(child) => children.push((Some("index_loc"), child))
    None => ()
  }
  node(NodeKind::Expr_ArrayGetSlice, loc, source_span, children)
}

///|
pub fn CstNode::expr_array(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
  array_kind~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  children.push((Some("array_kind"), array_kind))
  node(NodeKind::Expr_Array, loc, source_span, children)
}

///|
pub fn CstNode::expr_array_spread(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
  array_kind~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  children.push((Some("array_kind"), array_kind))
  node(NodeKind::Expr_ArraySpread, loc, source_span, children)
}

///|
pub fn CstNode::expr_array_set(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_ArraySet,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_array_augmented_set(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_ArrayAugmentedSet,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_list_comprehension(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_ListComprehension,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_unit(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
  faked~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  children.push((Some("faked"), faked))
  node(NodeKind::Expr_Unit, loc, source_span, children)
}

///|
pub fn CstNode::expr_group(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
  group~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  children.push((Some("group"), group))
  node(NodeKind::Expr_Group, loc, source_span, children)
}

///|
pub fn CstNode::expr_tuple(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  node(NodeKind::Expr_Tuple, loc, source_span, children)
}

///|
pub fn CstNode::expr_sequence(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  node(NodeKind::Expr_Sequence, loc, source_span, children)
}

///|
pub fn CstNode::expr_constraint(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_Constraint,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_record(
  loc~ : Location,
  source_span~ : (Int, Int),
  type_? : CstNode,
  op? : CstNode,
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match type_ {
    Some(child) => children.push((Some("type"), child))
    None => ()
  }
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  children.push((Some("body"), body))
  node(NodeKind::Expr_Record, loc, source_span, children)
}

///|
pub fn CstNode::expr_record_update(
  loc~ : Location,
  source_span~ : (Int, Int),
  type_? : CstNode,
  op? : CstNode,
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match type_ {
    Some(child) => children.push((Some("type"), child))
    None => ()
  }
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  children.push((Some("body"), body))
  node(NodeKind::Expr_RecordUpdate, loc, source_span, children)
}

///|
pub fn CstNode::expr_map(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  node(NodeKind::Expr_Map, loc, source_span, children)
}

///|
pub fn CstNode::expr_infix(
  loc~ : Location,
  source_span~ : (Int, Int),
  lhs~ : CstNode,
  op~ : CstNode,
  operator~ : CstNode,
  rhs~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("lhs"), lhs))
  children.push((Some("op"), op))
  children.push((Some("operator"), operator))
  children.push((Some("rhs"), rhs))
  node(NodeKind::Expr_Infix, loc, source_span, children)
}

///|
pub fn CstNode::expr_pipe(
  loc~ : Location,
  source_span~ : (Int, Int),
  lhs~ : CstNode,
  op~ : CstNode,
  rhs~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("lhs"), lhs))
  children.push((Some("op"), op))
  children.push((Some("rhs"), rhs))
  node(NodeKind::Expr_Pipe, loc, source_span, children)
}

///|
pub fn CstNode::expr_rev_pipe(
  loc~ : Location,
  source_span~ : (Int, Int),
  lhs~ : CstNode,
  op~ : CstNode,
  rhs~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("lhs"), lhs))
  children.push((Some("op"), op))
  children.push((Some("rhs"), rhs))
  node(NodeKind::Expr_RevPipe, loc, source_span, children)
}

///|
pub fn CstNode::expr_implies(
  loc~ : Location,
  source_span~ : (Int, Int),
  lhs~ : CstNode,
  op~ : CstNode,
  operator~ : CstNode,
  rhs~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("lhs"), lhs))
  children.push((Some("op"), op))
  children.push((Some("operator"), operator))
  children.push((Some("rhs"), rhs))
  node(NodeKind::Expr_Implies, loc, source_span, children)
}

///|
pub fn CstNode::expr_unary(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
  operator~ : CstNode,
  expr~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  children.push((Some("operator"), operator))
  children.push((Some("expr"), expr))
  node(NodeKind::Expr_Unary, loc, source_span, children)
}

///|
pub fn CstNode::expr_assign(
  loc~ : Location,
  source_span~ : (Int, Int),
  lhs? : CstNode,
  name~ : ArrayView[CstNode],
  semantic_name? : CstNode,
  op~ : CstNode,
  operator~ : CstNode,
  rhs~ : CstNode,
  augmented? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match lhs {
    Some(child) => children.push((Some("lhs"), child))
    None => ()
  }
  for child in name {
    children.push((Some("name"), child))
  }
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  children.push((Some("op"), op))
  children.push((Some("operator"), operator))
  children.push((Some("rhs"), rhs))
  match augmented {
    Some(child) => children.push((Some("augmented"), child))
    None => ()
  }
  node(NodeKind::Expr_Assign, loc, source_span, children)
}

///|
pub fn CstNode::expr_mutate(
  loc~ : Location,
  source_span~ : (Int, Int),
  self_~ : CstNode,
  token~ : CstNode,
  field~ : CstNode,
  op~ : CstNode,
  operator~ : CstNode,
  rhs~ : CstNode,
  augmented? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("self"), self_))
  children.push((Some("token"), token))
  children.push((Some("field"), field))
  children.push((Some("op"), op))
  children.push((Some("operator"), operator))
  children.push((Some("rhs"), rhs))
  match augmented {
    Some(child) => children.push((Some("augmented"), child))
    None => ()
  }
  node(NodeKind::Expr_Mutate, loc, source_span, children)
}

///|
pub fn CstNode::expr_if(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_If,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_match(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_Match,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_try(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_Try,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_try_operator(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  body~ : CstNode,
  try_operator_kind? : CstNode,
  try_loc? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  children.push((Some("body"), body))
  match try_operator_kind {
    Some(child) => children.push((Some("try_operator_kind"), child))
    None => ()
  }
  match try_loc {
    Some(child) => children.push((Some("try_loc"), child))
    None => ()
  }
  node(NodeKind::Expr_TryOperator, loc, source_span, children)
}

///|
pub fn CstNode::expr_for(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_For,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_for_each(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_ForEach,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_while(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_While,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_function(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : ArrayView[CstNode],
  error~ : ArrayView[CstNode],
  params? : CstNode,
  type_? : CstNode,
  op? : CstNode,
  pipe? : CstNode,
  body? : CstNode,
  async_? : CstNode,
  fn_kind? : CstNode,
  async_loc? : CstNode,
  params_loc? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in kw {
    children.push((Some("kw"), child))
  }
  for child in error {
    children.push((Some("error"), child))
  }
  match params {
    Some(child) => children.push((Some("params"), child))
    None => ()
  }
  match type_ {
    Some(child) => children.push((Some("type"), child))
    None => ()
  }
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  match pipe {
    Some(child) => children.push((Some("pipe"), child))
    None => ()
  }
  match body {
    Some(child) => children.push((Some("body"), child))
    None => ()
  }
  match async_ {
    Some(child) => children.push((Some("async"), child))
    None => ()
  }
  match fn_kind {
    Some(child) => children.push((Some("fn_kind"), child))
    None => ()
  }
  match async_loc {
    Some(child) => children.push((Some("async_loc"), child))
    None => ()
  }
  match params_loc {
    Some(child) => children.push((Some("params_loc"), child))
    None => ()
  }
  node(NodeKind::Expr_Function, loc, source_span, children)
}

///|
pub fn CstNode::expr_method(
  loc~ : Location,
  source_span~ : (Int, Int),
  type_~ : CstNode,
  op~ : CstNode,
  error? : CstNode,
  field? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("type"), type_))
  children.push((Some("op"), op))
  match error {
    Some(child) => children.push((Some("error"), child))
    None => ()
  }
  match field {
    Some(child) => children.push((Some("field"), child))
    None => ()
  }
  node(NodeKind::Expr_Method, loc, source_span, children)
}

///|
pub fn CstNode::expr_as(
  loc~ : Location,
  source_span~ : (Int, Int),
  expr~ : CstNode,
  op~ : CstNode,
  type_~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("expr"), expr))
  children.push((Some("op"), op))
  children.push((Some("type"), type_))
  node(NodeKind::Expr_As, loc, source_span, children)
}

///|
pub fn CstNode::expr_is(
  loc~ : Location,
  source_span~ : (Int, Int),
  expr~ : CstNode,
  op~ : CstNode,
  pattern~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("expr"), expr))
  children.push((Some("op"), op))
  children.push((Some("pattern"), pattern))
  node(NodeKind::Expr_Is, loc, source_span, children)
}

///|
pub fn CstNode::expr_lex_match(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_LexMatch,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_lex_scan(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_LexScan,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_regex_match(
  loc~ : Location,
  source_span~ : (Int, Int),
  expr~ : CstNode,
  op~ : CstNode,
  pattern~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("expr"), expr))
  children.push((Some("op"), op))
  children.push((Some("pattern"), pattern))
  node(NodeKind::Expr_RegexMatch, loc, source_span, children)
}

///|
pub fn CstNode::expr_template_writing(
  loc~ : Location,
  source_span~ : (Int, Int),
  expr~ : CstNode,
  op~ : CstNode,
  template~ : CstNode,
  conditional? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("expr"), expr))
  children.push((Some("op"), op))
  children.push((Some("template"), template))
  match conditional {
    Some(child) => children.push((Some("conditional"), child))
    None => ()
  }
  node(NodeKind::Expr_TemplateWriting, loc, source_span, children)
}

///|
pub fn CstNode::expr_let(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_Let,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_let_mut(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_LetMut,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::leaf(
  payload~ : NodePayload,
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  node(NodeKind::Leaf(payload), loc, source_span, [])
}
