///|
// The lexscan grammar selects its case role only after parsing the whole
// pattern. Reconstruct only the root forms reachable along this syntax path.
fn finish_lexscan_case_pattern(source : CstNode) -> CstNode {
  let loc = source.loc
  let source_span = source.source_span
  let marker_loc = empty_loc(loc.end)
  let marker_span = empty_span(source_span.1)
  let marker = match source.kind {
    Pattern_Var =>
      @node.CstNode::lex_scan_case_pattern_binder(
        loc=marker_loc,
        source_span=marker_span,
      )
    Pattern_Any | Pattern_MissingPattern =>
      @node.CstNode::lex_scan_case_pattern_wildcard(
        loc=marker_loc,
        source_span=marker_span,
      )
    _ =>
      @node.CstNode::lex_scan_case_pattern_pattern(
        loc=marker_loc,
        source_span=marker_span,
      )
  }
  let children = [ for entry in source.children => entry ]
  push_child(children, Some("lex_scan_case_pattern"), marker)
  match source.kind {
    Pattern_Var => {
      let entries = recovery_children(children)
      @node.CstNode::pattern_var(loc~, source_span~, entries~)
    }
    Pattern_Any => {
      let name = CstNode::first_child_with_label_from(children, "name").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let lex_scan_case_pattern = CstNode::first_child_with_label_from(
        children, "lex_scan_case_pattern",
      )
      @node.CstNode::pattern_any(
        loc~,
        source_span~,
        name~,
        lex_scan_case_pattern?,
      )
    }
    Pattern_MissingPattern => {
      let entries = recovery_children(children)
      @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
    }
    Pattern_Or => {
      let entries = recovery_children(children)
      @node.CstNode::pattern_or(loc~, source_span~, entries~)
    }
    Pattern_Alias => {
      let entries = recovery_children(children)
      @node.CstNode::pattern_alias_from_entries(loc~, source_span~, entries~)
    }
    Pattern_Range => {
      let entries = recovery_children(children)
      @node.CstNode::pattern_range_from_entries(loc~, source_span~, entries~)
    }
    Pattern_Constraint => {
      let entries = recovery_children(children)
      @node.CstNode::pattern_constraint(loc~, source_span~, entries~)
    }
    Pattern_With => {
      let entries = recovery_children(children)
      @node.CstNode::pattern_with_from_entries(loc~, source_span~, entries~)
    }
    RegexPattern_Placeholder => {
      let open = CstNode::first_child_with_label_from(children, "open").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let pattern = CstNode::first_child_with_label_from(children, "pattern").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let bindings = CstNode::first_child_with_label_from(children, "bindings")
      let error = CstNode::children_with_label_from(children, "error")
      let close = CstNode::first_child_with_label_from(children, "close").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let lex_scan_case_pattern = CstNode::first_child_with_label_from(
        children, "lex_scan_case_pattern",
      )
      @node.CstNode::regex_pattern_placeholder(
        loc~,
        source_span~,
        open~,
        pattern~,
        bindings?,
        error~,
        close~,
        lex_scan_case_pattern?,
      )
    }
    RegexPattern_Literal => {
      let error = CstNode::first_child_with_label_from(children, "error")
      let token = CstNode::first_child_with_label_from(children, "token")
      let lex_literal = CstNode::first_child_with_label_from(
        children, "lex_literal",
      )
      let lex_scan_case_pattern = CstNode::first_child_with_label_from(
        children, "lex_scan_case_pattern",
      )
      @node.CstNode::regex_pattern_literal(
        loc~,
        source_span~,
        error?,
        token?,
        lex_literal?,
        lex_scan_case_pattern?,
      )
    }
    RegexPattern_Reference => {
      let name = CstNode::children_with_label_from(children, "name")
      let semantic_name = CstNode::first_child_with_label_from(
        children, "semantic_name",
      )
      let lex_scan_case_pattern = CstNode::first_child_with_label_from(
        children, "lex_scan_case_pattern",
      )
      @node.CstNode::regex_pattern_reference(
        loc~,
        source_span~,
        name~,
        semantic_name?,
        lex_scan_case_pattern?,
      )
    }
    RegexPattern_Sequence => {
      let entries = recovery_children(children)
      @node.CstNode::regex_pattern_sequence_from_entries(
        loc~,
        source_span~,
        entries~,
      )
    }
    RegexPattern_Alternation => {
      let entries = recovery_children(children)
      @node.CstNode::regex_pattern_alternation_from_entries(
        loc~,
        source_span~,
        entries~,
      )
    }
    RegexPattern_Alias => {
      let pattern = CstNode::first_child_with_label_from(children, "pattern").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let kw = CstNode::first_child_with_label_from(children, "kw").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let name = CstNode::first_child_with_label_from(children, "name").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let lex_scan_case_pattern = CstNode::first_child_with_label_from(
        children, "lex_scan_case_pattern",
      )
      @node.CstNode::regex_pattern_alias(
        loc~,
        source_span~,
        pattern~,
        kw~,
        name~,
        lex_scan_case_pattern?,
      )
    }
    _ => abort("unexpected lexscan case pattern")
  }
}
