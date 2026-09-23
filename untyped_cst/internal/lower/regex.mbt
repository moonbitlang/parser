///|
fn lower_pattern_tuple_items(node : CstNode) -> Array[CstNode] {
  if node.first_child_with_label("body") is Some(body) {
    lower_item_children(body)
  } else {
    []
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
      Alias(pat~, binder~, loc=node.loc)
    }
    RegexPattern_Sequence | Pattern_Tuple =>
      match
        (node.first_child_with_label("lhs"), node.first_child_with_label("rhs")) {
        (Some(lhs), Some(rhs)) =>
          Sequence(
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
              pat = Sequence(
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
      Alternation(pat1=lhs, pat2=rhs, loc=node.loc)
    }
    RegexPattern_Reference | Pattern_Constr | Pattern_Var =>
      Reference(
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
    .unwrap_or({ name: "", loc: node.loc, })
  let binder = node.first_child_with_label("binder").map(lower_binder_from_node)
  (label, binder)
}

///|
fn lower_empty_regex_pattern(loc : Location) -> @syntax.RegexPattern {
  Literal(lit="", recovered_=false, loc~)
}

///|
fn lower_lex_scan_case_pattern(node : CstNode) -> @syntax.LexScanCasePattern {
  match node.first_child_of_kind(NodeKind::is_lex_scan_case_pattern) {
    Some(marker) =>
      match marker.kind {
        LexScanCasePattern_Wildcard => Wildcard(loc=node.loc)
        LexScanCasePattern_Binder =>
          Binder(
            node
            .first_child_with_label("semantic_name")
            .map(lower_binder_from_node)
            .unwrap_or(lower_dummy_binder(node.loc)),
          )
        LexScanCasePattern_Pattern => Pattern(lower_regex_pattern(node))
        _ => Wildcard(loc=node.loc)
      }
    None => Wildcard(loc=node.loc)
  }
}

///|
fn lower_regex_match_expr(node : CstNode) -> @syntax.Expr {
  let pattern_node = node.first_child_with_label("pattern")
  RegexMatch(
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
      Literal(
        lit=literal.string_payload().unwrap_or(""),
        recovered_=true,
        loc=node.loc,
      )
    (Some(literal), _) =>
      Literal(
        lit=literal.string_payload().unwrap_or(""),
        recovered_=false,
        loc=node.loc,
      )
    (_, Some(constant)) if constant.kind is Constant_String =>
      Literal(
        lit=constant.string_payload().unwrap_or(""),
        recovered_=true,
        loc=node.loc,
      )
    (_, Some(constant)) if constant.kind is Constant_Regex =>
      Literal(
        lit=constant.string_payload().unwrap_or(""),
        recovered_=false,
        loc=node.loc,
      )
    _ if node.first_child_with_label("semantic_name") is Some(name) =>
      Reference(lid=lower_long_ident_node(name), loc=node.loc)
    _ => lower_empty_regex_pattern(node.loc)
  }
}
