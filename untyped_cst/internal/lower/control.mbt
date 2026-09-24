///|
fn lower_while_expr(node : CstNode) -> @syntax.Expr {
  While(
    loop_cond=node
      .first_child_with_label("cond")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    loop_body=node
      .first_child_with_label("body")
      .map(lower_function_body_expr)
      .unwrap_or(lower_unit_expr(node.loc)),
    while_else=node.first_child_with_label("else").map(lower_function_body_expr),
    label=lower_loop_label(node),
    loc=node.loc,
  )
}

///|
fn lower_assign_expr(node : CstNode) -> @syntax.Expr {
  if node.first_child_with_label("lhs") is Some(_) {
    return lower_hole_expr(node.loc)
  }
  let augmented = node
    .bool_marker_value(fn(kind) { kind is Flag_Augmented })
    .unwrap_or(false)
  match node.kind {
    Expr_Assign => {
      guard node.first_child_with_label("semantic_name") is Some(name) else {
        return lower_hole_expr(node.loc)
      }
      let id = @syntax.Var::{
        name: lower_long_ident_from_child(node, "semantic_name", "name"),
        loc: name.loc,
      }
      let expr = node
        .first_child_with_label("rhs")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc))
      let augmented_by = if augmented {
        node.first_child_of_kind(NodeKind::is_operator).map(lower_operator_var)
      } else {
        None
      }
      Assign(var_=id, expr~, augmented_by~, loc=node.loc)
    }
    Expr_ArraySet | Expr_ArrayAugmentedSet => {
      guard node.first_child_with_label("self") is Some(array_node) &&
        node.first_child_with_label("index") is Some(index_node) else {
        return lower_hole_expr(node.loc)
      }
      let array = lower_expr(array_node)
      let index = lower_expr(index_node)
      let value = node
        .first_child_with_label("rhs")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc))
      if node.kind is Expr_ArrayAugmentedSet || augmented {
        let op = node
          .first_child_of_kind(NodeKind::is_operator)
          .map(lower_operator_var)
          .unwrap_or({ name: Ident(name=""), loc: node.loc, })
        ArrayAugmentedSet(op~, array~, index~, value~, loc=node.loc)
      } else {
        ArraySet(array~, index~, value~, loc=node.loc)
      }
    }
    Expr_Mutate => {
      guard node.first_child_with_label("self") is Some(record_node) &&
        node.first_child_with_label("field") is Some(_) else {
        return lower_hole_expr(node.loc)
      }
      let record = lower_expr(record_node)
      let accessor = lower_accessor_child(node)
      let field = node
        .first_child_with_label("rhs")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc))
      let augmented_by = if augmented {
        node.first_child_of_kind(NodeKind::is_operator).map(lower_operator_var)
      } else {
        None
      }
      Mutate(record~, accessor~, field~, augmented_by~, loc=node.loc)
    }
    _ => lower_hole_expr(node.loc)
  }
}

///|
fn lower_for_binding(node : CstNode) -> (@syntax.Binder, @syntax.Expr) {
  (
    node
    .first_child_with_label("name")
    .map(lower_binder_from_node)
    .unwrap_or(lower_dummy_binder(node.loc)),
    node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_unit_expr(node.loc)),
  )
}

///|
fn lower_for_binding_list(
  node : CstNode,
  label : String,
) -> List[(@syntax.Binder, @syntax.Expr)] {
  let bindings : Array[(@syntax.Binder, @syntax.Expr)] = []
  if node.first_child_with_label(label) is Some(list) {
    for item in lower_item_children(list) {
      bindings.push(lower_for_binding(item))
    }
  } else {
    ()
  }
  lower_array_to_list(bindings)
}

///|
fn lower_for_expr(node : CstNode) -> @syntax.Expr {
  For(
    binders=lower_for_binding_list(node, "init"),
    condition=node.first_child_with_label("cond").map(lower_expr),
    continue_block=lower_for_binding_list(node, "continue"),
    body=node
      .first_child_with_label("body")
      .map(lower_function_body_expr)
      .unwrap_or(lower_unit_expr(node.loc)),
    for_else=node.first_child_with_label("else").map(lower_function_body_expr),
    where_clause=node.first_child_with_label("where").map(lower_where_clause),
    label=lower_loop_label(node),
    loc=node.loc,
  )
}

///|
fn lower_foreach_patterns(node : CstNode) -> List[@syntax.Pattern] {
  guard node.first_child_with_label("pattern") is Some(pattern) else {
    return @list.empty()
  }
  if pattern.kind is List_TuplePatternList {
    let patterns : Array[@syntax.Pattern] = []
    for item in lower_item_children(pattern) {
      patterns.push(lower_pattern(item))
    }
    lower_array_to_list(patterns)
  } else {
    @list.singleton(lower_pattern(pattern))
  }
}

///|
fn lower_foreach_expr(node : CstNode) -> @syntax.Expr {
  ForEach(
    patterns=lower_foreach_patterns(node),
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    init=lower_for_binding_list(node, "init"),
    continue_block=lower_for_binding_list(node, "continue"),
    body=node
      .first_child_with_label("body")
      .map(lower_function_body_expr)
      .unwrap_or(lower_unit_expr(node.loc)),
    else_block=node.first_child_with_label("else").map(lower_function_body_expr),
    where_clause=node.first_child_with_label("where").map(lower_where_clause),
    label=lower_loop_label(node),
    loc=node.loc,
  )
}

///|
fn lower_loop_label(node : CstNode) -> @syntax.Label? {
  guard node.first_child_with_label("label") is Some(label) else { return None }
  let lowered = lower_label_from_node(label)
  Some({ name: lowered.name, loc: label.loc, })
}

///|
fn lower_apply_expr(node : CstNode) -> @syntax.Expr {
  let func = node
    .first_child_with_label("func")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let args = node
    .first_child_with_label("arg")
    .map(lower_arguments)
    .unwrap_or(@list.empty())
  Apply(func~, args~, loc=node.loc)
}

///|
fn lower_dot_apply_expr(node : CstNode) -> @syntax.Expr {
  let self_expr = node
    .first_child_with_label("self")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let method_label = node
    .first_child_with_label("field")
    .map(lower_label_from_node)
    .unwrap_or({ name: "", loc: node.loc, })
  let args = node
    .first_child_with_label("arg")
    .map(lower_arguments)
    .unwrap_or(@list.empty())
  let return_self = node
    .bool_marker_value(fn(kind) { kind is Flag_ReturnSelf })
    .unwrap_or(false)
  @syntax.Expr::DotApply(
    self=self_expr,
    method_name=method_label,
    args~,
    return_self~,
    loc=node.loc,
  )
}

///|
fn lower_type_name_from_expr_node(node : CstNode) -> @syntax.TypeName {
  match node.kind {
    kind if kind.is_type() => lower_type_name_from_type_node(node)
    Expr_Constr | Expr_Ident =>
      {
        name: lower_long_ident_from_child(node, "semantic_name", "name"),
        is_object: false,
        loc: node.loc,
      }
    _ => { name: @syntax.LongIdent::dummy(), is_object: false, loc: node.loc, }
  }
}

///|
fn lower_method_expr(node : CstNode) -> @syntax.Expr {
  Method(
    type_name=node
      .first_child_with_label("type")
      .map(lower_type_name_from_expr_node)
      .unwrap_or({
        name: @syntax.LongIdent::dummy(),
        is_object: false,
        loc: node.loc,
      }),
    method_name=node
      .first_child_with_label("field")
      .map(lower_label_from_node)
      .unwrap_or({ name: "", loc: node.loc, }),
    loc=node.loc,
  )
}

///|
fn lower_accessor_child(node : CstNode) -> @syntax.Accessor {
  if node.first_child_with_label("field") is Some(field) {
    match field.kind {
      Accessor_Index =>
        Index(tuple_index=field.int_payload().unwrap_or(0), loc=field.loc)
      Accessor_Newtype => Newtype(loc=field.loc)
      _ => Label(lower_label_from_node(field))
    }
  } else {
    Label({ name: "", loc: node.loc, })
  }
}

///|
fn lower_field_expr(node : CstNode) -> @syntax.Expr {
  let record = node
    .first_child_with_label("self")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let accessor = lower_accessor_child(node)
  Field(record~, accessor~, loc=node.loc)
}

///|
fn lower_array_get_expr(node : CstNode) -> @syntax.Expr {
  let array = node
    .first_child_with_label("self")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let index = node
    .first_child_with_label("index")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  ArrayGet(array~, index~, loc=node.loc)
}

///|
fn lower_array_get_slice_expr(node : CstNode) -> @syntax.Expr {
  let array = node
    .first_child_with_label("self")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let indexes = node.children_with_label("index")
  let start_index = if node.first_child_with_label("start") is Some(start) {
    Some(lower_expr(start))
  } else {
    indexes.get(0).map(lower_expr)
  }
  let end_index = if node.first_child_with_label("end") is Some(end) {
    Some(lower_expr(end))
  } else {
    indexes.get(1).map(lower_expr)
  }
  ArrayGetSlice(
    array~,
    start_index~,
    end_index~,
    index_loc=lower_index_loc(node),
    loc=node.loc,
  )
}

///|
fn lower_if_expr(node : CstNode) -> @syntax.Expr {
  let cond = node
    .first_child_with_label("cond")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let bodies = node.children_with_label("body")
  let ifso = bodies
    .get(0)
    .map(lower_function_body_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let ifnot = node
    .children_with_label("else")
    .last()
    .map(lower_function_body_expr)
  If(cond~, ifso~, ifnot~, loc=node.loc)
}

///|
fn lower_match_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let cases : Array[@syntax.Case] = []
  for case_node in node.children_with_label("case") {
    cases.push(lower_case(case_node))
  }
  Match(
    expr~,
    cases=lower_array_to_list(cases),
    match_loc=node.loc,
    loc=node.loc,
  )
}

///|
fn lower_case(node : CstNode) -> @syntax.Case {
  {
    pattern: node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(Any(loc=node.loc)),
    guard_: node.first_child_with_label("cond").map(lower_expr),
    body: node
    .first_child_with_label("body")
    .map(lower_expr)
    .unwrap_or(lower_unit_expr(node.loc)),
  }
}

///|
fn lower_try_expr(node : CstNode) -> @syntax.Expr {
  let cases : Array[@syntax.Case] = []
  for case_list in node.children_with_label("case") {
    for case_node in case_list.children_with_label("case") {
      cases.push(lower_case(case_node))
    }
  }
  let else_cases : Array[@syntax.Case] = []
  for case_list in node.children_with_label("else") {
    for case_node in case_list.children_with_label("case") {
      else_cases.push(lower_case(case_node))
    }
  }
  let try_else = if else_cases.length() == 0 {
    None
  } else {
    Some(lower_array_to_list(else_cases))
  }
  let try_loc = lower_try_loc(node)
  let has_try = node
    .bool_marker_value(fn(kind) { kind is Flag_HasTry })
    .unwrap_or(false)
  let catch_loc = lower_catch_loc(node)
  let else_loc = lower_else_loc(node)
  let loc = lower_try_expr_loc(node)
  Try(
    body=node
      .first_child_with_label("body")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    catch_=lower_array_to_list(cases),
    try_else~,
    has_try~,
    try_loc~,
    catch_loc~,
    else_loc~,
    loc~,
  )
}

///|
fn lower_try_operator_expr(node : CstNode) -> @syntax.Expr {
  TryOperator(
    body=node
      .first_child_with_label("body")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    kind=Exclamation,
    try_loc=lower_try_loc(node),
    loc=node.loc,
  )
}

///|
fn lower_lex_case_nodes(node : CstNode) -> Array[CstNode] {
  let cases : Array[CstNode] = []
  for block in node.children_with_label("case") {
    for case_node in block.children_with_label("case") {
      cases.push(case_node)
    }
  }
  cases
}

///|
fn lower_lexmatch_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let cases : Array[@syntax.LexScanCase] = []
  for case_node in lower_lex_case_nodes(node) {
    cases.push(lower_lex_scan_case(case_node))
  }
  LexScan(
    streaming=false,
    strategy=node.first_child_with_label("name").map(lower_label_from_node),
    expr~,
    match_loc=node.loc,
    cases=lower_array_to_list(cases),
    loc=node.loc,
  )
}

///|
fn lower_lexscan_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let cases : Array[@syntax.LexScanCase] = []
  for case_node in lower_lex_case_nodes(node) {
    cases.push(lower_lex_scan_case(case_node))
  }
  LexScan(
    streaming=true,
    strategy=node.first_child_with_label("name").map(lower_label_from_node),
    expr~,
    match_loc=node.loc,
    cases=lower_array_to_list(cases),
    loc=node.loc,
  )
}

///|
fn lower_lex_scan_case(node : CstNode) -> @syntax.LexScanCase {
  let pattern_node = node.first_child_with_label("pattern")
  {
    pat: pattern_node
    .map(lower_lex_scan_case_pattern)
    .unwrap_or(Wildcard(loc=node.loc)),
    bindings: lower_regex_match_bindings(pattern_node),
    body: node
    .first_child_with_label("body")
    .map(lower_expr)
    .unwrap_or(lower_unit_expr(node.loc)),
    arrow_loc: node
    .first_child_with_label("op")
    .map(fn(op) { op.loc })
    .unwrap_or(node.loc),
  }
}
