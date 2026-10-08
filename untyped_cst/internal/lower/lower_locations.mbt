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
  lower_decl_loc_without_trailing_trivia(node)
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
fn lower_delimited_inner_loc(node : CstNode) -> Location {
  let fallback = node
    .first_child_with_label("open")
    .map(open => open.loc.end)
    .unwrap_or(node.loc.start)
  let mut start = fallback
  let mut end = fallback
  let mut saw_item = false
  for (label, child) in node.children {
    match label {
      Some("item") => {
        if !saw_item {
          start = child.loc.start
          saw_item = true
        }
        end = child.loc.end
      }
      Some("sep") if saw_item => end = child.loc.end
      _ => ()
    }
  }
  { start, end, }
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
fn lower_last_significant_token_end(node : CstNode) -> Position? {
  match node.kind.token_kind() {
    Some(TK_COMMENT | TK_NEWLINE | TK_EOF) => return None
    Some(_) => return Some(node.loc.end)
    None => ()
  }
  for i = node.children.length() - 1; i >= 0; i = i - 1 {
    let (_, child) = node.children[i]
    if lower_last_significant_token_end(child) is Some(found) {
      return Some(found)
    }
  }
  None
}
