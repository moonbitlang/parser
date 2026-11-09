///|
// Typed constructors omit trivia fields. Retain the parser's direct comments
// without changing their public signatures or duplicating entry-based trivia.
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn CstNode::preserve_trivia(
  self : CstNode,
  entries : ArrayView[(String?, CstNode)],
) -> CstNode {
  let missing : Array[(String?, CstNode)] = []
  let mut seen = 0
  // Direct comments in both sequences are in source order. Scan once even for
  // large declaration/statement lists whose constructors already retain them.
  for entry in entries {
    if entry.0 != Some("trivia") {
      continue
    }
    while seen < self.children.length() &&
          (
            self.children[seen].0 != Some("trivia") ||
            self.children[seen].1.source_span.0 < entry.1.source_span.0
          ) {
      seen += 1
    }
    if seen == self.children.length() ||
      self.children[seen].1.source_span != entry.1.source_span {
      missing.push(entry)
    }
  }
  if missing.is_empty() {
    return self
  }
  let children : Array[(String?, CstNode)] = []
  let mut i = 0
  for entry in self.children {
    while i < missing.length() &&
          missing[i].1.source_span.0 < entry.1.source_span.0 {
      children.push(missing[i])
      i += 1
    }
    children.push(entry)
  }
  while i < missing.length() {
    children.push(missing[i])
    i += 1
  }
  node(self.kind, self.loc, self.source_span, children)
}

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
