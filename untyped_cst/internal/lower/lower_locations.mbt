///|
// Location queries stay within the named syntax field. Looking through an
// expression or type subtree could pick a keyword from a nested declaration.
fn lower_token_loc(
  node : CstNode,
  label : String,
  kind : TokenKind,
) -> Location? {
  for (field, token) in node.children {
    if field == Some(label) && token.kind.token_kind() == Some(kind) {
      return Some(token.loc)
    }
  }
  None
}

///|
fn lower_async_loc(node : CstNode) -> Location? {
  if node.bool_marker_value(fn(kind) { kind is Flag_Async }) is Some(false) {
    return None
  }
  lower_token_loc(node, "kw", TK_ASYNC)
}

///|
fn lower_function_owner_object_loc(node : CstNode) -> Location? {
  if node.bool_marker_value(fn(kind) { kind is Flag_Object }) is Some(false) {
    return None
  }
  lower_token_loc(node, "op", TK_AMPER)
}

///|
fn lower_decl_loc_without_trailing_trivia(node : CstNode) -> Location {
  let mut end = node.loc.start
  let mut found = false
  for (label, child) in node.children {
    if label is Some("trivia") ||
      child.kind.is_syntax_flag() ||
      child.kind.is_type_desc() ||
      child.kind.is_decl_body() ||
      child.kind.is_name() ||
      child.kind.is_accessor() {
      continue
    }
    end = child.loc.end
    found = true
  }
  if found {
    Location::{ start: node.loc.start, end, }
  } else {
    node.loc
  }
}

///|
fn lower_trait_decl_loc(node : CstNode) -> Location {
  let loc = lower_decl_loc_without_trailing_trivia(node)
  match node.first_child_with_label("kw") {
    Some(keyword) => Location::{ start: keyword.loc.start, end: loc.end, }
    None => loc
  }
}

///|
fn lower_function_header_loc(node : CstNode) -> Location {
  let mut end = node.loc.start
  for (label, child) in node.children {
    if label is Some("body" | "where" | "stub" | "stub_token" | "trivia") ||
      (label is Some("op") && child.kind.token_kind() == Some(TK_EQUAL)) ||
      child.kind.is_syntax_flag() ||
      child.kind.is_type_desc() ||
      child.kind.is_decl_body() ||
      child.kind.is_name() ||
      child.kind.is_accessor() {
      continue
    }
    end = child.loc.end
  }
  Location::{ start: node.loc.start, end, }
}

///|
fn lower_local_function_loc(node : CstNode) -> Location {
  match node.first_child_with_label("kw") {
    Some(keyword) => Location::{ start: keyword.loc.start, end: node.loc.end, }
    None => node.loc
  }
}

///|
fn lower_index_loc(node : CstNode) -> Location {
  match
    (node.first_child_with_label("open"), node.first_child_with_label("close")) {
    (Some(open), Some(close)) => open.loc.merge(close.loc)
    _ => node.loc
  }
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
fn lower_pipe_arrow_discard_loc(
  node : CstNode,
  params_node : CstNode?,
) -> Location? {
  if node.first_child_with_label("pipe") is Some(_) &&
    params_node is Some(params) &&
    params.children_with_label("item") is [item] &&
    params.loc.start == item.loc.start &&
    params.loc.end == item.loc.end {
    lower_token_loc(node, "op", TK_FAT_ARROW)
  } else {
    None
  }
}

///|
fn lower_try_loc(node : CstNode) -> Location {
  for keyword in node.children_with_label("kw") {
    if keyword.kind.token_kind() is Some(TK_TRY | TK_TRY_EXCLAMATION | TK_CATCH) {
      return keyword.loc
    }
  }
  node.loc
}

///|
fn lower_catch_loc(node : CstNode) -> Location {
  guard lower_token_loc(node, "kw", TK_CATCH) is Some(loc) else {
    return node.loc
  }
  if node.bool_marker_value(fn(kind) { kind is Flag_HasTry }) is Some(true) &&
    node.first_child_with_label("case") is Some(cases) &&
    cases.first_child_with_label("open") is Some(open) {
    loc.merge(open.loc)
  } else {
    loc
  }
}

///|
fn lower_else_loc(node : CstNode) -> Location {
  guard lower_token_loc(node, "else_kw", TK_NORAISE) is Some(loc) else {
    return node.loc
  }
  if node.first_child_with_label("else") is Some(cases) &&
    cases.first_child_with_label("open") is Some(open) {
    loc.merge(open.loc)
  } else {
    loc
  }
}

///|
fn lower_leading_parenthesized_end(node : CstNode) -> Position? {
  match node.kind {
    Expr_Unit | Expr_Group | Expr_Tuple | Expr_Constraint =>
      return Some(node.loc.end)
    _ => ()
  }
  for (label, child) in node.children {
    if label is Some("trivia") {
      continue
    }
    return lower_leading_parenthesized_end(child)
  }
  None
}

///|
fn lower_first_significant_token_at_or_after(
  node : CstNode,
  pos : Position,
) -> CstNode? {
  match node.kind.token_kind() {
    Some(TK_COMMENT | TK_NEWLINE) => return None
    Some(_) if node.loc.start.cnum >= pos.cnum => return Some(node)
    _ => ()
  }
  for (_, child) in node.children {
    if lower_first_significant_token_at_or_after(child, pos) is Some(found) {
      return Some(found)
    }
  }
  None
}

///|
fn lower_try_expr_loc(node : CstNode) -> Location {
  if node.bool_marker_value(fn(kind) { kind is Flag_HasTry }) is Some(true) {
    return node.loc
  }
  if node.first_child_with_label("body") is Some(body) &&
    lower_leading_parenthesized_end(body) is Some(paren_end) &&
    lower_token_loc(node, "kw", TK_CATCH) is Some(catch_loc) {
    let start = lower_first_significant_token_at_or_after(body, paren_end)
      .map(token => token.loc.start)
      .unwrap_or(catch_loc.start)
    Location::{ start, end: node.loc.end, }
  } else {
    node.loc
  }
}
