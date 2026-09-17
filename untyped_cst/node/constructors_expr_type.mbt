///|
pub fn CstNode::expr_let_and(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_LetAnd,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_let_fn(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_LetFn,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_return(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  expr? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  match expr {
    Some(child) => children.push((Some("expr"), child))
    None => ()
  }
  node(NodeKind::Expr_Return, loc, source_span, children)
}

///|
pub fn CstNode::expr_raise(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  expr~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  children.push((Some("expr"), expr))
  node(NodeKind::Expr_Raise, loc, source_span, children)
}

///|
pub fn CstNode::expr_break(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  label_loc? : CstNode,
  label? : CstNode,
  expr? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  match label_loc {
    Some(child) => children.push((Some("label_loc"), child))
    None => ()
  }
  match label {
    Some(child) => children.push((Some("label"), child))
    None => ()
  }
  match expr {
    Some(child) => children.push((Some("expr"), child))
    None => ()
  }
  node(NodeKind::Expr_Break, loc, source_span, children)
}

///|
pub fn CstNode::expr_continue(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  label_loc? : CstNode,
  label? : CstNode,
  arg? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  match label_loc {
    Some(child) => children.push((Some("label_loc"), child))
    None => ()
  }
  match label {
    Some(child) => children.push((Some("label"), child))
    None => ()
  }
  match arg {
    Some(child) => children.push((Some("arg"), child))
    None => ()
  }
  node(NodeKind::Expr_Continue, loc, source_span, children)
}

///|
pub fn CstNode::expr_labelled_block(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_LabelledBlock,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_defer(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  expr~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  children.push((Some("expr"), expr))
  node(NodeKind::Expr_Defer, loc, source_span, children)
}

///|
pub fn CstNode::expr_err_defer(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  expr~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  children.push((Some("expr"), expr))
  node(NodeKind::Expr_ErrDefer, loc, source_span, children)
}

///|
pub fn CstNode::expr_guard(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_Guard,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_quantifier(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  name~ : CstNode,
  op~ : CstNode,
  type_~ : CstNode,
  sep~ : CstNode,
  body~ : CstNode,
  quantifier_kind? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  children.push((Some("name"), name))
  children.push((Some("op"), op))
  children.push((Some("type"), type_))
  children.push((Some("sep"), sep))
  children.push((Some("body"), body))
  match quantifier_kind {
    Some(child) => children.push((Some("quantifier_kind"), child))
    None => ()
  }
  node(NodeKind::Expr_Quantifier, loc, source_span, children)
}

///|
pub fn CstNode::expr_hole(
  loc~ : Location,
  source_span~ : (Int, Int),
  token~ : CstNode,
  hole_kind? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("token"), token))
  match hole_kind {
    Some(child) => children.push((Some("hole_kind"), child))
    None => ()
  }
  node(NodeKind::Expr_Hole, loc, source_span, children)
}

///|
pub fn CstNode::expr_constr(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Expr_Constr,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::expr_multiline_string(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstMultilineChild],
) -> CstNode {
  node(
    NodeKind::Expr_MultilineString,
    loc,
    source_span,
    entries_from_cstmultilinechild(entries),
  )
}

///|
pub fn CstNode::expr_missing_expr(
  loc~ : Location,
  source_span~ : (Int, Int),
  label? : CstNode,
  trivia? : CstNode,
  label_loc? : CstNode,
  expr? : CstNode,
  op? : CstNode,
  loop_loc? : CstNode,
  error~ : ArrayView[CstNode],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match label {
    Some(child) => children.push((Some("label"), child))
    None => ()
  }
  match trivia {
    Some(child) => children.push((Some("trivia"), child))
    None => ()
  }
  match label_loc {
    Some(child) => children.push((Some("label_loc"), child))
    None => ()
  }
  match expr {
    Some(child) => children.push((Some("expr"), child))
    None => ()
  }
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  match loop_loc {
    Some(child) => children.push((Some("loop_loc"), child))
    None => ()
  }
  for child in error {
    children.push((Some("error"), child))
  }
  node(NodeKind::Expr_MissingExpr, loc, source_span, children)
}

///|
pub fn CstNode::expr_proof_assert(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  expr~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  children.push((Some("expr"), expr))
  node(NodeKind::Expr_ProofAssert, loc, source_span, children)
}

///|
pub fn CstNode::expr_proof_let(
  loc~ : Location,
  source_span~ : (Int, Int),
  kw~ : CstNode,
  name~ : CstNode,
  op~ : CstNode,
  expr~ : CstNode,
  semantic_name? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("kw"), kw))
  children.push((Some("name"), name))
  children.push((Some("op"), op))
  children.push((Some("expr"), expr))
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  node(NodeKind::Expr_ProofLet, loc, source_span, children)
}

///|
pub fn CstNode::pattern_var(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Pattern_Var,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::pattern_any(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  lex_scan_case_pattern? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  match lex_scan_case_pattern {
    Some(child) => children.push((Some("lex_scan_case_pattern"), child))
    None => ()
  }
  node(NodeKind::Pattern_Any, loc, source_span, children)
}

///|
pub fn CstNode::pattern_constant(
  loc~ : Location,
  source_span~ : (Int, Int),
  op? : CstNode,
  operator? : CstNode,
  error? : CstNode,
  token? : CstNode,
  constant? : CstNode,
  semantic_constant? : CstNode,
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
  match error {
    Some(child) => children.push((Some("error"), child))
    None => ()
  }
  match token {
    Some(child) => children.push((Some("token"), child))
    None => ()
  }
  match constant {
    Some(child) => children.push((Some("constant"), child))
    None => ()
  }
  match semantic_constant {
    Some(child) => children.push((Some("semantic_constant"), child))
    None => ()
  }
  node(NodeKind::Pattern_Constant, loc, source_span, children)
}

///|
pub fn CstNode::pattern_tuple(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
  pattern_group? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  match pattern_group {
    Some(child) => children.push((Some("pattern_group"), child))
    None => ()
  }
  node(NodeKind::Pattern_Tuple, loc, source_span, children)
}

///|
pub fn CstNode::pattern_array(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  node(NodeKind::Pattern_Array, loc, source_span, children)
}

///|
pub fn CstNode::pattern_rest(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
  as_? : CstNode,
  bytes? : CstNode,
  pkg? : CstNode,
  str? : CstNode,
  name? : CstNode,
  const_? : CstNode,
  dot_dot_binder? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  match as_ {
    Some(child) => children.push((Some("as"), child))
    None => ()
  }
  match bytes {
    Some(child) => children.push((Some("bytes"), child))
    None => ()
  }
  match pkg {
    Some(child) => children.push((Some("pkg"), child))
    None => ()
  }
  match str {
    Some(child) => children.push((Some("str"), child))
    None => ()
  }
  match name {
    Some(child) => children.push((Some("name"), child))
    None => ()
  }
  match const_ {
    Some(child) => children.push((Some("const"), child))
    None => ()
  }
  match dot_dot_binder {
    Some(child) => children.push((Some("dot_dot_binder"), child))
    None => ()
  }
  node(NodeKind::Pattern_Rest, loc, source_span, children)
}

///|
pub fn CstNode::pattern_record(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
  openness? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  match openness {
    Some(child) => children.push((Some("openness"), child))
    None => ()
  }
  node(NodeKind::Pattern_Record, loc, source_span, children)
}

///|
pub fn CstNode::pattern_map(
  loc~ : Location,
  source_span~ : (Int, Int),
  trivia? : CstNode,
  body~ : CstNode,
  openness? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match trivia {
    Some(child) => children.push((Some("trivia"), child))
    None => ()
  }
  children.push((Some("body"), body))
  match openness {
    Some(child) => children.push((Some("openness"), child))
    None => ()
  }
  node(NodeKind::Pattern_Map, loc, source_span, children)
}

///|
pub fn CstNode::pattern_constr(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Pattern_Constr,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::pattern_constr_any(
  loc~ : Location,
  source_span~ : (Int, Int),
  type_~ : CstNode,
  op~ : CstNode,
  wildcard~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("type"), type_))
  children.push((Some("op"), op))
  children.push((Some("wildcard"), wildcard))
  node(NodeKind::Pattern_ConstrAny, loc, source_span, children)
}

///|
pub fn CstNode::pattern_special_constr(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  params~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  children.push((Some("params"), params))
  node(NodeKind::Pattern_SpecialConstr, loc, source_span, children)
}

///|
pub fn CstNode::pattern_or(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Pattern_Or,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::pattern_alias(
  loc~ : Location,
  source_span~ : (Int, Int),
  pattern~ : CstNode,
  op~ : CstNode,
  name~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("pattern"), pattern))
  children.push((Some("op"), op))
  children.push((Some("name"), name))
  node(NodeKind::Pattern_Alias, loc, source_span, children)
}

///|
pub fn CstNode::pattern_range(
  loc~ : Location,
  source_span~ : (Int, Int),
  lhs~ : CstNode,
  op~ : CstNode,
  rhs~ : CstNode,
  range_kind? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("lhs"), lhs))
  children.push((Some("op"), op))
  children.push((Some("rhs"), rhs))
  match range_kind {
    Some(child) => children.push((Some("range_kind"), child))
    None => ()
  }
  node(NodeKind::Pattern_Range, loc, source_span, children)
}

///|
pub fn CstNode::pattern_constraint(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Pattern_Constraint,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::pattern_with(
  loc~ : Location,
  source_span~ : (Int, Int),
  open~ : CstNode,
  pattern~ : CstNode,
  op~ : CstNode,
  body~ : CstNode,
  close~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("open"), open))
  children.push((Some("pattern"), pattern))
  children.push((Some("op"), op))
  children.push((Some("body"), body))
  children.push((Some("close"), close))
  node(NodeKind::Pattern_With, loc, source_span, children)
}

///|
pub fn CstNode::pattern_missing_pattern(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Pattern_MissingPattern,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::pattern_group(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::Pattern_Group, loc, source_span, children)
}

///|
pub fn CstNode::array_pattern_pattern(
  loc~ : Location,
  source_span~ : (Int, Int),
  pattern~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("pattern"), pattern))
  node(NodeKind::ArrayPattern_Pattern, loc, source_span, children)
}

///|
pub fn CstNode::array_pattern_string_spread(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
  str~ : CstNode,
  dot_dot_binder~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  children.push((Some("str"), str))
  children.push((Some("dot_dot_binder"), dot_dot_binder))
  node(NodeKind::ArrayPattern_StringSpread, loc, source_span, children)
}

///|
pub fn CstNode::array_pattern_bytes_spread(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
  bytes~ : CstNode,
  dot_dot_binder~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  children.push((Some("bytes"), bytes))
  children.push((Some("dot_dot_binder"), dot_dot_binder))
  node(NodeKind::ArrayPattern_BytesSpread, loc, source_span, children)
}

///|
pub fn CstNode::array_pattern_const_spread(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
  pkg? : CstNode,
  const_~ : CstNode,
  dot_dot_binder~ : CstNode,
  semantic_name? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  match pkg {
    Some(child) => children.push((Some("pkg"), child))
    None => ()
  }
  children.push((Some("const"), const_))
  children.push((Some("dot_dot_binder"), dot_dot_binder))
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  node(NodeKind::ArrayPattern_ConstSpread, loc, source_span, children)
}

///|
pub fn CstNode::array_patterns_closed(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  node(NodeKind::ArrayPatterns_Closed, loc, source_span, children)
}

///|
pub fn CstNode::array_patterns_open(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  node(NodeKind::ArrayPatterns_Open, loc, source_span, children)
}

///|
pub fn CstNode::type_name(
  loc~ : Location,
  source_span~ : (Int, Int),
  op? : CstNode,
  name~ : ArrayView[CstNode],
  error~ : ArrayView[CstNode],
  question? : CstNode,
  semantic_name? : CstNode,
  object? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  for child in name {
    children.push((Some("name"), child))
  }
  for child in error {
    children.push((Some("error"), child))
  }
  match question {
    Some(child) => children.push((Some("question"), child))
    None => ()
  }
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  match object {
    Some(child) => children.push((Some("object"), child))
    None => ()
  }
  node(NodeKind::Type_Name, loc, source_span, children)
}

///|
pub fn CstNode::type_tuple(
  loc~ : Location,
  source_span~ : (Int, Int),
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("body"), body))
  node(NodeKind::Type_Tuple, loc, source_span, children)
}

///|
pub fn CstNode::type_arrow(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Type_Arrow,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::type_application(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Type_Application,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::type_option(
  loc~ : Location,
  source_span~ : (Int, Int),
  type_~ : CstNode,
  op~ : CstNode,
  operator~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("type"), type_))
  children.push((Some("op"), op))
  children.push((Some("operator"), operator))
  node(NodeKind::Type_Option, loc, source_span, children)
}

///|
pub fn CstNode::type_object(
  loc~ : Location,
  source_span~ : (Int, Int),
  body? : CstNode,
  op? : CstNode,
  type_? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match body {
    Some(child) => children.push((Some("body"), child))
    None => ()
  }
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  match type_ {
    Some(child) => children.push((Some("type"), child))
    None => ()
  }
  node(NodeKind::Type_Object, loc, source_span, children)
}

///|
pub fn CstNode::type_any(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  node(NodeKind::Type_Any, loc, source_span, children)
}

///|
pub fn CstNode::type_group(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Type_Group,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::type_missing_type(
  loc~ : Location,
  source_span~ : (Int, Int),
  error~ : ArrayView[CstNode],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in error {
    children.push((Some("error"), child))
  }
  node(NodeKind::Type_MissingType, loc, source_span, children)
}

///|
pub fn CstNode::error_type_error(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
  type_~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  children.push((Some("type"), type_))
  node(NodeKind::ErrorType_Error, loc, source_span, children)
}

///|
pub fn CstNode::error_type_default(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  node(NodeKind::ErrorType_Default, loc, source_span, children)
}

///|
pub fn CstNode::error_type_noraise(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  node(NodeKind::ErrorType_Noraise, loc, source_span, children)
}

///|
pub fn CstNode::error_type_maybe(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
  type_~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  children.push((Some("type"), type_))
  node(NodeKind::ErrorType_Maybe, loc, source_span, children)
}

///|
pub fn CstNode::type_desc_abstract(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TypeDesc_Abstract, loc, source_span, children)
}

///|
pub fn CstNode::type_desc_extensible_enum(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TypeDesc_ExtensibleEnum, loc, source_span, children)
}

///|
pub fn CstNode::type_desc_error_no_payload(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TypeDesc_ErrorNoPayload, loc, source_span, children)
}

///|
pub fn CstNode::type_desc_error_enum_payload(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TypeDesc_ErrorEnumPayload, loc, source_span, children)
}

///|
pub fn CstNode::type_desc_variant(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TypeDesc_Variant, loc, source_span, children)
}

///|
pub fn CstNode::type_desc_extend_enum(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TypeDesc_ExtendEnum, loc, source_span, children)
}

///|
pub fn CstNode::type_desc_record(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TypeDesc_Record, loc, source_span, children)
}

///|
pub fn CstNode::type_desc_tuple_struct(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TypeDesc_TupleStruct, loc, source_span, children)
}

///|
pub fn CstNode::type_desc_alias(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::TypeDesc_Alias, loc, source_span, children)
}

///|
pub fn CstNode::lex_pattern_regex_interp(
  loc~ : Location,
  source_span~ : (Int, Int),
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  node(NodeKind::LexPattern_RegexInterp, loc, source_span, children)
}

///|
pub fn CstNode::regex_pattern_placeholder(
  loc~ : Location,
  source_span~ : (Int, Int),
  open~ : CstNode,
  pattern~ : CstNode,
  bindings? : CstNode,
  error~ : ArrayView[CstNode],
  close~ : CstNode,
  lex_scan_case_pattern? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("open"), open))
  children.push((Some("pattern"), pattern))
  match bindings {
    Some(child) => children.push((Some("bindings"), child))
    None => ()
  }
  for child in error {
    children.push((Some("error"), child))
  }
  children.push((Some("close"), close))
  match lex_scan_case_pattern {
    Some(child) => children.push((Some("lex_scan_case_pattern"), child))
    None => ()
  }
  node(NodeKind::RegexPattern_Placeholder, loc, source_span, children)
}

///|
pub fn CstNode::regex_pattern_literal(
  loc~ : Location,
  source_span~ : (Int, Int),
  error? : CstNode,
  token? : CstNode,
  lex_literal? : CstNode,
  lex_scan_case_pattern? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match error {
    Some(child) => children.push((Some("error"), child))
    None => ()
  }
  match token {
    Some(child) => children.push((Some("token"), child))
    None => ()
  }
  match lex_literal {
    Some(child) => children.push((Some("lex_literal"), child))
    None => ()
  }
  match lex_scan_case_pattern {
    Some(child) => children.push((Some("lex_scan_case_pattern"), child))
    None => ()
  }
  node(NodeKind::RegexPattern_Literal, loc, source_span, children)
}

///|
pub fn CstNode::regex_pattern_reference(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : ArrayView[CstNode],
  semantic_name? : CstNode,
  lex_scan_case_pattern? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in name {
    children.push((Some("name"), child))
  }
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  match lex_scan_case_pattern {
    Some(child) => children.push((Some("lex_scan_case_pattern"), child))
    None => ()
  }
  node(NodeKind::RegexPattern_Reference, loc, source_span, children)
}

///|
pub fn CstNode::regex_pattern_sequence(
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
  node(NodeKind::RegexPattern_Sequence, loc, source_span, children)
}

///|
pub fn CstNode::regex_pattern_alternation(
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
  node(NodeKind::RegexPattern_Alternation, loc, source_span, children)
}

///|
pub fn CstNode::regex_pattern_alias(
  loc~ : Location,
  source_span~ : (Int, Int),
  pattern~ : CstNode,
  kw~ : CstNode,
  name~ : CstNode,
  lex_scan_case_pattern? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("pattern"), pattern))
  children.push((Some("kw"), kw))
  children.push((Some("name"), name))
  match lex_scan_case_pattern {
    Some(child) => children.push((Some("lex_scan_case_pattern"), child))
    None => ()
  }
  node(NodeKind::RegexPattern_Alias, loc, source_span, children)
}

///|
pub fn CstNode::case_match_case(
  loc~ : Location,
  source_span~ : (Int, Int),
  pattern~ : CstNode,
  kw? : CstNode,
  cond? : CstNode,
  op? : CstNode,
  error~ : ArrayView[CstNode],
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("pattern"), pattern))
  match kw {
    Some(child) => children.push((Some("kw"), child))
    None => ()
  }
  match cond {
    Some(child) => children.push((Some("cond"), child))
    None => ()
  }
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  for child in error {
    children.push((Some("error"), child))
  }
  children.push((Some("body"), body))
  node(NodeKind::Case_MatchCase, loc, source_span, children)
}

///|
pub fn CstNode::case_lex_scan_case(
  loc~ : Location,
  source_span~ : (Int, Int),
  pattern~ : CstNode,
  op~ : CstNode,
  body~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("pattern"), pattern))
  children.push((Some("op"), op))
  children.push((Some("body"), body))
  node(NodeKind::Case_LexScanCase, loc, source_span, children)
}

///|
pub fn CstNode::argument_positional(
  loc~ : Location,
  source_span~ : (Int, Int),
  expr~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("expr"), expr))
  node(NodeKind::Argument_Positional, loc, source_span, children)
}

///|
pub fn CstNode::argument_labelled(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  op~ : CstNode,
  expr? : CstNode,
  pattern? : CstNode,
  semantic_name? : CstNode,
  pun? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  children.push((Some("op"), op))
  match expr {
    Some(child) => children.push((Some("expr"), child))
    None => ()
  }
  match pattern {
    Some(child) => children.push((Some("pattern"), child))
    None => ()
  }
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  match pun {
    Some(child) => children.push((Some("pun"), child))
    None => ()
  }
  node(NodeKind::Argument_Labelled, loc, source_span, children)
}

///|
pub fn CstNode::argument_labelled_pun(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  semantic_name? : CstNode,
  pun? : CstNode,
  label_loc? : CstNode,
  value_loc? : CstNode,
  expr? : CstNode,
  pattern? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  match pun {
    Some(child) => children.push((Some("pun"), child))
    None => ()
  }
  match label_loc {
    Some(child) => children.push((Some("label_loc"), child))
    None => ()
  }
  match value_loc {
    Some(child) => children.push((Some("value_loc"), child))
    None => ()
  }
  match expr {
    Some(child) => children.push((Some("expr"), child))
    None => ()
  }
  match pattern {
    Some(child) => children.push((Some("pattern"), child))
    None => ()
  }
  node(NodeKind::Argument_LabelledPun, loc, source_span, children)
}

///|
pub fn CstNode::argument_labelled_option(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  op~ : ArrayView[CstNode],
  expr~ : CstNode,
  semantic_name? : CstNode,
  pun? : CstNode,
  question_loc? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  for child in op {
    children.push((Some("op"), child))
  }
  children.push((Some("expr"), expr))
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  match pun {
    Some(child) => children.push((Some("pun"), child))
    None => ()
  }
  match question_loc {
    Some(child) => children.push((Some("question_loc"), child))
    None => ()
  }
  node(NodeKind::Argument_LabelledOption, loc, source_span, children)
}

///|
pub fn CstNode::argument_labelled_option_pun(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  op~ : CstNode,
  semantic_name? : CstNode,
  pun? : CstNode,
  question_loc? : CstNode,
  label_loc? : CstNode,
  value_loc? : CstNode,
  expr? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  children.push((Some("op"), op))
  match semantic_name {
    Some(child) => children.push((Some("semantic_name"), child))
    None => ()
  }
  match pun {
    Some(child) => children.push((Some("pun"), child))
    None => ()
  }
  match question_loc {
    Some(child) => children.push((Some("question_loc"), child))
    None => ()
  }
  match label_loc {
    Some(child) => children.push((Some("label_loc"), child))
    None => ()
  }
  match value_loc {
    Some(child) => children.push((Some("value_loc"), child))
    None => ()
  }
  match expr {
    Some(child) => children.push((Some("expr"), child))
    None => ()
  }
  node(NodeKind::Argument_LabelledOptionPun, loc, source_span, children)
}

///|
pub fn CstNode::array_element_regular(
  loc~ : Location,
  source_span~ : (Int, Int),
  expr~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("expr"), expr))
  node(NodeKind::ArrayElement_Regular, loc, source_span, children)
}

///|
pub fn CstNode::array_element_spread(
  loc~ : Location,
  source_span~ : (Int, Int),
  op~ : CstNode,
  expr~ : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("op"), op))
  children.push((Some("expr"), expr))
  node(NodeKind::ArrayElement_Spread, loc, source_span, children)
}

///|
pub fn CstNode::parameter_discard_positional(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Parameter_DiscardPositional,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::parameter_positional(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Parameter_Positional,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::parameter_labelled(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Parameter_Labelled,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::parameter_optional(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Parameter_Optional,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::parameter_question_optional(
  loc~ : Location,
  source_span~ : (Int, Int),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    NodeKind::Parameter_QuestionOptional,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
pub fn CstNode::parameter_type_decl(
  loc~ : Location,
  source_span~ : (Int, Int),
  name~ : CstNode,
  op? : CstNode,
  type_? : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  children.push((Some("name"), name))
  match op {
    Some(child) => children.push((Some("op"), child))
    None => ()
  }
  match type_ {
    Some(child) => children.push((Some("type"), child))
    None => ()
  }
  node(NodeKind::Parameter_TypeDecl, loc, source_span, children)
}
