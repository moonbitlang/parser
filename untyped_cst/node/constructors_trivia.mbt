///|
// Preserve syntax order and comments at this grammar boundary.
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn CstNode::expr_as_from_entries(
  loc~ : Location,
  source_span~ : (Int32, Int32),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(Expr_As, loc, source_span, entries_from_cstrecoverychild(entries))
}

///|
// Preserve syntax order and comments at this grammar boundary.
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn CstNode::regex_pattern_alias_from_entries(
  loc~ : Location,
  source_span~ : (Int32, Int32),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    RegexPattern_Alias,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
// Preserve syntax order and comments at this grammar boundary.
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn CstNode::parameter_constructor_positional_from_entries(
  loc~ : Location,
  source_span~ : (Int32, Int32),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    Parameter_ConstructorPositional,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
// Preserve syntax order and comments at this grammar boundary.
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn CstNode::parameter_constructor_labelled_from_entries(
  loc~ : Location,
  source_span~ : (Int32, Int32),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    Parameter_ConstructorLabelled,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}

///|
// Preserve syntax order and comments at this grammar boundary.
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn CstNode::binding_for_from_entries(
  loc~ : Location,
  source_span~ : (Int32, Int32),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(Binding_For, loc, source_span, entries_from_cstrecoverychild(entries))
}

///|
// Preserve syntax order and comments at this grammar boundary.
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn CstNode::clause_where_from_entries(
  loc~ : Location,
  source_span~ : (Int32, Int32),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(Clause_Where, loc, source_span, entries_from_cstrecoverychild(entries))
}

///|
// Preserve syntax order and comments at this grammar boundary.
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn CstNode::field_where_from_entries(
  loc~ : Location,
  source_span~ : (Int32, Int32),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(Field_Where, loc, source_span, entries_from_cstrecoverychild(entries))
}

///|
// Preserve syntax order and comments at this grammar boundary.
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn CstNode::case_match_case_from_entries(
  loc~ : Location,
  source_span~ : (Int32, Int32),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(Case_MatchCase, loc, source_span, entries_from_cstrecoverychild(entries))
}

///|
// Preserve syntax order and comments at this grammar boundary.
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn CstNode::case_lex_scan_case_from_entries(
  loc~ : Location,
  source_span~ : (Int32, Int32),
  entries~ : ArrayView[CstRecoveryChild],
) -> CstNode {
  node(
    Case_LexScanCase,
    loc,
    source_span,
    entries_from_cstrecoverychild(entries),
  )
}
