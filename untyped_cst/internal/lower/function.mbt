///|
fn lower_parameter(node : CstNode) -> @syntax.Parameter {
  let pat = node.first_child_with_label("pattern")
  let ty = node.first_child_with_label("type").map(lower_type)
  let pat_loc = pat.map(fn(pat) { pat.loc }).unwrap_or(node.loc)
  let binder = if pat is Some(pat_node) {
    pat_node
    .first_child_with_label("name")
    .map(lower_post_label_binder_from_node)
    .unwrap_or(lower_dummy_binder(pat_node.loc))
  } else {
    lower_dummy_binder(node.loc)
  }
  match node.kind {
    Parameter_DiscardPositional => DiscardPositional(ty~, loc=pat_loc)
    Parameter_Labelled => Labelled(binder~, ty~)
    Parameter_Optional =>
      Optional(
        binder~,
        default=node
          .first_child_with_label("default")
          .map(lower_expr)
          .unwrap_or(lower_unit_expr(node.loc)),
        ty~,
      )
    Parameter_QuestionOptional => QuestionOptional(binder~, ty~)
    Parameter_Positional => Positional(binder~, ty~)
    _ => DiscardPositional(ty~, loc=pat_loc)
  }
}

///|
fn lower_parameters_from_list(
  list : CstNode?,
) -> (List[@syntax.Parameter], Location) {
  lower_parameters_from_list_with_discard_loc(list, None)
}

///|
fn lower_parameters_from_list_with_discard_loc(
  list : CstNode?,
  first_discard_loc : Location?,
) -> (List[@syntax.Parameter], Location) {
  if list is Some(list) {
    let params : Array[@syntax.Parameter] = []
    let mut item_index = 0
    for item in lower_item_children(list) {
      let parameter = lower_parameter(item)
      let parameter = match (item_index, first_discard_loc, parameter) {
        (0, Some(loc), DiscardPositional(ty~, loc=_)) =>
          @syntax.Parameter::DiscardPositional(ty~, loc~)
        (_, _, parameter) => parameter
      }
      params.push(parameter)
      item_index += 1
    }
    (lower_array_to_list(params), list.loc)
  } else {
    (@list.empty(), lower_empty_loc())
  }
}

///|
fn lower_optional_parameters_from_list(
  list : CstNode?,
) -> (List[@syntax.Parameter]?, Location) {
  if list is Some(_) {
    let (params, loc) = lower_parameters_from_list(list)
    (Some(params), loc)
  } else {
    (None, lower_empty_loc())
  }
}

///|
fn lower_return_type_from_clause(node : CstNode?) -> @syntax.Type? {
  if node is Some(clause) {
    if clause.first_child_with_label("type") is Some(ty_node) {
      let ty = lower_type(ty_node)
      let outer_error = clause.first_child_with_label("clause")
      let outer_nocancel = clause.first_child_with_label("nocancel")
      match ty {
        Arrow(args~, res~, err~, is_async~, nocancel_~, loc~) if ty_node.kind
          is Type_Arrow &&
          (outer_error is Some(_) || outer_nocancel is Some(_)) => {
          let err = match outer_error {
            Some(error_clause) => lower_error_annotation(error_clause)
            None => err
          }
          let nocancel_ = match outer_nocancel {
            Some(anchor) => Some(anchor.loc)
            None => nocancel_
          }
          let loc = match (outer_error, outer_nocancel) {
            (Some(error_clause), Some(anchor)) =>
              loc.merge(error_clause.loc).merge(anchor.loc)
            (Some(error_clause), None) => loc.merge(error_clause.loc)
            (None, Some(anchor)) => loc.merge(anchor.loc)
            (None, None) => loc
          }
          Some(Arrow(args~, res~, err~, is_async~, nocancel_~, loc~))
        }
        _ => Some(ty)
      }
    } else {
      None
    }
  } else {
    None
  }
}

///|
fn lower_function_expr_node(node : CstNode) -> @syntax.Expr {
  let params_node = node.first_child_with_label("params")
  let (parameters, params_loc) = lower_parameters_from_list_with_discard_loc(
    params_node,
    lower_pipe_arrow_discard_loc(node, params_node),
  )
  let body = if node.first_child_with_label("body") is Some(body_node) {
    if arrow_lambda_block_keeps_group(node, body_node) {
      lower_expr(body_node)
    } else if node.first_child_with_label("pipe") is Some(_) ||
      function_body_block_can_unwrap(body_node) {
      lower_function_body_expr(body_node)
    } else {
      lower_expr(body_node)
    }
  } else {
    lower_unit_expr(node.loc)
  }
  let return_type = lower_return_type_from_clause(
    node.first_child_with_label("type"),
  )
  let kind = if node.has_child_kind(fn(kind) { kind is FnKind_Arrow }) {
    @syntax.FnKind::Arrow
  } else {
    Lambda
  }
  let func = @syntax.Func::{
    parameters,
    params_loc,
    body,
    return_type,
    error_type: lower_error_type_from_clause(
      node.first_child_with_label("type"),
    ),
    kind,
    is_async: lower_async_loc(node),
    nocancel_: lower_nocancel_from_clause(node.first_child_with_label("type")),
    loc: node.loc,
  }
  Function(func~, loc=node.loc)
}

///|
fn arrow_lambda_block_keeps_group(node : CstNode, body_node : CstNode) -> Bool {
  guard body_node.kind is Expr_Block else { return false }
  if node.first_child_with_label("pipe") is Some(_) {
    return false
  }
  node.has_child_kind(fn(kind) { kind is FnKind_Arrow })
}

///|
fn function_body_block_can_unwrap(node : CstNode) -> Bool {
  match node.kind {
    Expr_Block => true
    _ => false
  }
}

///|
fn lower_let_expr_without_body(node : CstNode) -> @syntax.Expr {
  lower_let_expr_with_body(node, lower_unit_expr(node.loc))
}

///|
fn lower_let_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  let base_pat = node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(Any(loc=node.loc))
  let pat = match node.kind {
    Expr_Let =>
      if node.first_child_with_label("type") is Some(type_node) {
        let ty = lower_type(type_node)
        let loc = base_pat.loc().merge(ty.loc())
        @syntax.Pattern::Constraint(pat=base_pat, ty~, loc~)
      } else {
        base_pat
      }
    _ => base_pat
  }
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_unit_expr(node.loc))
  match node.kind {
    Expr_LetMut => {
      let binder = match pat {
        Var(binder) => binder
        _ => lower_dummy_binder(pat.loc())
      }
      LetMut(
        binder~,
        ty=node.first_child_with_label("type").map(lower_type),
        expr~,
        body~,
        loc=node.loc.merge(body.loc()),
      )
    }
    _ => Let(pattern=pat, expr~, body~, loc=node.loc.merge(body.loc()))
  }
}

///|
fn lower_func_from_decl_like_node(node : CstNode) -> @syntax.Func {
  let (parameters, params_loc) = lower_parameters_from_list(
    node.first_child_with_label("params"),
  )
  let body = node
    .first_child_with_label("body")
    .map(lower_function_body_expr)
    .unwrap_or(lower_unit_expr(node.loc))
  let loc = lower_local_function_loc(node)
  {
    parameters,
    params_loc,
    body,
    return_type: lower_return_type_from_clause(
      node.first_child_with_label("type"),
    ),
    error_type: lower_error_type_from_clause(
      node.first_child_with_label("type"),
    ),
    kind: Lambda,
    is_async: lower_async_loc(node),
    nocancel_: lower_nocancel_from_clause(node.first_child_with_label("type")),
    loc,
  }
}

///|
fn lower_func_from_letrec_binding(node : CstNode) -> @syntax.Func {
  match node.first_child_with_label("expr").map(lower_expr) {
    Some(Function(func~, ..)) => func
    Some(expr) =>
      {
        parameters: @list.empty(),
        params_loc: lower_empty_loc(),
        body: expr,
        return_type: None,
        error_type: NoErrorType,
        kind: Lambda,
        is_async: None,
        nocancel_: None,
        loc: node.loc,
      }
    None => lower_func_from_decl_like_node(node)
  }
}

///|
fn lower_letfn_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  LetFn(
    name=node
      .first_child_with_label("name")
      .map(lower_binder_from_node)
      .unwrap_or(lower_dummy_binder(node.loc)),
    func=lower_func_from_decl_like_node(node),
    body~,
    loc=node.loc.merge(body.loc()),
  )
}

///|
fn lower_letrec_bindings(
  node : CstNode,
) -> List[(@syntax.Binder, @syntax.Type?, @syntax.Func)] {
  let bindings : Array[(@syntax.Binder, @syntax.Type?, @syntax.Func)] = []
  for binding in node.children_with_label("binding") {
    let binding_type = binding
      .first_child_with_label("binding_type")
      .map(lower_type)
    bindings.push(
      (
        binding
        .first_child_with_label("name")
        .map(lower_binder_from_node)
        .unwrap_or(lower_dummy_binder(binding.loc)),
        binding_type,
        lower_func_from_letrec_binding(binding),
      ),
    )
  }
  lower_array_to_list(bindings)
}

///|
fn lower_letrec_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  LetAnd(
    bindings=lower_letrec_bindings(node),
    body~,
    loc=node.loc.merge(body.loc()),
  )
}

///|
fn lower_guard_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  Guard(
    cond=node
      .first_child_with_label("cond")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    otherwise=node.first_child_with_label("else").map(lower_function_body_expr),
    body~,
    exclamation=node
      .bool_marker_value(fn(kind) { kind is Flag_Exclamation })
      .unwrap_or(false),
    guard_loc=node
      .first_child_with_label("kw")
      .map(anchor => anchor.loc)
      .unwrap_or(node.loc),
    loc=node.loc.merge(body.loc()),
  )
}

///|
fn lower_defer_expr_with_body(
  node : CstNode,
  body : @syntax.Expr,
) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let loc = node.loc.merge(body.loc())
  if node.kind is Expr_ErrDefer {
    ErrDefer(expr~, body~, loc~)
  } else {
    Defer(expr~, body~, loc~)
  }
}

///|
fn lower_proof_assert_expr(node : CstNode) -> @syntax.Expr {
  ProofAssert(
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_proof_let_expr(node : CstNode) -> @syntax.Expr {
  ProofLet(
    binder=node
      .first_child_with_label("name")
      .map(lower_binder_from_node)
      .unwrap_or(lower_dummy_binder(node.loc)),
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    loc=node.loc,
  )
}
