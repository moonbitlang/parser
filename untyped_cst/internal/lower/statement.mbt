///|
fn lower_statement_sequence(
  items : Array[CstNode],
  index : Int,
  loc : Location,
) -> @syntax.Expr {
  if index >= items.length() {
    return lower_faked_unit_expr(loc)
  }
  let first = items[index]
  if index == items.length() - 1 {
    return lower_statement_tail_expr(first)
  }
  match first.kind {
    Expr_Let | Expr_LetMut => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_let_expr_with_body(first, body)
    }
    Expr_LetFn => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_letfn_expr_with_body(first, body)
    }
    Expr_LetAnd => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_letrec_expr_with_body(first, body)
    }
    Expr_Guard => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_guard_expr_with_body(first, body)
    }
    Expr_Defer | Expr_ErrDefer => {
      let body = lower_statement_sequence(items, index + 1, loc)
      lower_defer_expr_with_body(first, body)
    }
    _ => {
      let prefix : Array[@syntax.Expr] = []
      let mut i = index
      while i < items.length() - 1 {
        match items[i].kind {
          Expr_Let | Expr_LetMut => {
            let body = lower_statement_sequence(items, i, loc)
            return if prefix.length() == 0 {
              body
            } else {
              lower_sequence_expr(prefix, body)
            }
          }
          Expr_LetFn => {
            let body = lower_statement_sequence(items, i + 1, loc)
            let letfn = lower_letfn_expr_with_body(items[i], body)
            return if prefix.length() == 0 {
              letfn
            } else {
              lower_sequence_expr(prefix, letfn)
            }
          }
          Expr_LetAnd => {
            let body = lower_statement_sequence(items, i + 1, loc)
            let letrec_expr = lower_letrec_expr_with_body(items[i], body)
            return if prefix.length() == 0 {
              letrec_expr
            } else {
              lower_sequence_expr(prefix, letrec_expr)
            }
          }
          Expr_Guard => {
            let body = lower_statement_sequence(items, i + 1, loc)
            let guard_expr = lower_guard_expr_with_body(items[i], body)
            return if prefix.length() == 0 {
              guard_expr
            } else {
              lower_sequence_expr(prefix, guard_expr)
            }
          }
          Expr_Defer | Expr_ErrDefer => {
            let body = lower_statement_sequence(items, i + 1, loc)
            let defer_expr = lower_defer_expr_with_body(items[i], body)
            return if prefix.length() == 0 {
              defer_expr
            } else {
              lower_sequence_expr(prefix, defer_expr)
            }
          }
          _ => {
            prefix.push(lower_expr(items[i]))
            i += 1
          }
        }
      }
      lower_sequence_expr(
        prefix,
        lower_statement_tail_expr(items[items.length() - 1]),
      )
    }
  }
}

///|
fn lower_statement_items(node : CstNode) -> Array[CstNode] {
  if node.kind is Expr_Sequence {
    node.first_child_with_label("body").map(lower_item_children).unwrap_or([])
  } else {
    lower_item_children(node)
  }
}

///|
fn lower_sequence_cst_node(node : CstNode) -> @syntax.Expr {
  lower_statement_sequence(lower_statement_items(node), 0, node.loc)
}

///|
fn lower_sequence_expr(
  exprs : Array[@syntax.Expr],
  last_expr : @syntax.Expr,
) -> @syntax.Expr {
  let loc = if exprs.get(0) is Some(first) {
    first.loc().merge(last_expr.loc())
  } else {
    last_expr.loc()
  }
  Sequence(exprs=lower_array_to_list(exprs), last_expr~, loc~)
}

///|
fn lower_statement_tail_expr(item : CstNode) -> @syntax.Expr {
  match item.kind {
    Expr_Let | Expr_LetMut =>
      lower_let_expr_with_body(item, lower_faked_unit_expr(item.loc))
    Expr_Guard =>
      lower_guard_expr_with_body(item, lower_faked_unit_expr(item.loc))
    Expr_Defer | Expr_ErrDefer =>
      lower_defer_expr_with_body(item, lower_faked_unit_expr(item.loc))
    _ => lower_expr(item)
  }
}

///|
fn lower_block_expr_node(node : CstNode) -> @syntax.Expr {
  let expr = if node.first_child_with_label("body") is Some(body) {
    lower_statement_sequence(lower_statement_items(body), 0, node.loc)
  } else {
    lower_unit_expr(node.loc)
  }
  Group(expr~, group=Brace, loc=node.loc)
}

///|
fn lower_function_body_expr(node : CstNode) -> @syntax.Expr {
  match node.kind {
    Expr_Block =>
      if node.first_child_with_label("body") is Some(body) {
        lower_statement_sequence(lower_statement_items(body), 0, node.loc)
      } else {
        lower_unit_expr(node.loc)
      }
    _ => lower_expr(node)
  }
}

///|
fn lower_return_expr(node : CstNode) -> @syntax.Expr {
  Return(
    return_value=node.first_child_with_label("expr").map(lower_expr),
    loc=node.loc,
  )
}

///|
fn lower_raise_expr(node : CstNode) -> @syntax.Expr {
  Raise(
    err_value=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_break_expr(node : CstNode) -> @syntax.Expr {
  Break(
    arg=node.first_child_with_label("expr").map(lower_expr),
    label=lower_control_label(node),
    loc=node.loc,
  )
}

///|
fn lower_continue_expr(node : CstNode) -> @syntax.Expr {
  let args : Array[@syntax.Expr] = []
  if node.first_child_with_label("arg") is Some(list) {
    for item in lower_item_children(list) {
      args.push(lower_expr(item))
    }
  } else if node.first_child_with_label("expr") is Some(expr) {
    args.push(lower_expr(expr))
  } else {
    ()
  }
  let label = lower_control_label(node)
  Continue(args=lower_array_to_list(args), label~, loc=node.loc)
}

///|
fn lower_labelled_block_expr(node : CstNode) -> @syntax.Expr {
  LabelledBlock(
    label=node
      .first_child_with_label("label")
      .map(lower_label_from_node)
      .unwrap_or({ name: "", loc: node.loc, }),
    body=node
      .first_child_with_label("body")
      .map(lower_function_body_expr)
      .unwrap_or(lower_unit_expr(node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_control_label(node : CstNode) -> @syntax.Label? {
  if node.first_child_with_label("label") is Some(label_node) {
    let label = lower_label_from_node(label_node)
    Some({ name: label.name, loc: label_node.loc, })
  } else {
    None
  }
}

///|
fn lower_as_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let trait_ = node
    .first_child_with_label("type")
    .map(lower_type_name_from_type_node)
    .unwrap_or({
      name: @syntax.LongIdent::dummy(),
      is_object: false,
      loc: node.loc,
    })
  As(expr~, trait_~, loc=node.loc)
}

///|
fn lower_is_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let pat = node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(Any(loc=node.loc))
  Is(expr~, pat~, loc=node.loc)
}
