///|
pub fn lower_expr(node : CstNode) -> @syntax.Expr {
  match node.kind {
    Expression => {
      let items = node.children_with_label("expr")
      if items.is_empty() {
        lower_hole_expr(node.loc)
      } else {
        lower_statement_sequence(items, 0, node.loc)
      }
    }
    Expr_Ident =>
      Ident(
        id={
          name: lower_long_ident_from_child(node, "semantic_name", "name"),
          loc: node.loc,
        },
        loc=node.loc,
      )
    Expr_Constr => lower_constructor_expr(node)
    Expr_Constant | Expr_Interp | Expr_BytesInterp => lower_literal_expr(node)
    Expr_MultilineString => lower_multiline_string_expr(node)
    Expr_Hole =>
      Hole(
        loc=node.loc,
        kind=if node.has_child_kind(fn(kind) { kind is HoleKind_Todo }) {
          @syntax.Hole::Todo
        } else {
          @syntax.Hole::Incomplete
        },
      )
    Expr_MissingExpr => lower_hole_expr(node.loc)
    Expr_Unit | Expr_Group | Expr_Tuple => lower_tuple_expr(node)
    Expr_Constraint => lower_constraint_expr(node)
    Expr_Array | Expr_ArraySpread => lower_array_expr_node(node)
    Expr_ListComprehension => lower_list_comprehension_expr(node)
    Expr_Record | Expr_RecordUpdate => lower_record_expr_node(node)
    Expr_Map => lower_map_expr_node(node)
    Expr_Block => lower_block_expr_node(node)
    Expr_Sequence => lower_sequence_cst_node(node)
    Expr_Infix | Expr_Pipe | Expr_RevPipe | Expr_Implies =>
      lower_infix_expr(node)
    Expr_Unary => lower_prefix_expr(node)
    Expr_Apply => lower_apply_expr(node)
    Expr_DotApply => lower_dot_apply_expr(node)
    Expr_Method => lower_method_expr(node)
    Expr_Field => lower_field_expr(node)
    Expr_ArrayGet => lower_array_get_expr(node)
    Expr_ArrayGetSlice => lower_array_get_slice_expr(node)
    Expr_If => lower_if_expr(node)
    Expr_Match => lower_match_expr(node)
    Expr_Try => lower_try_expr(node)
    Expr_TryOperator => lower_try_operator_expr(node)
    Expr_LexMatch => lower_lexmatch_expr(node)
    Expr_LexScan => lower_lexscan_expr(node)
    Expr_For => lower_for_expr(node)
    Expr_ForEach => lower_foreach_expr(node)
    Expr_While => lower_while_expr(node)
    Expr_Function => lower_function_expr_node(node)
    Expr_LetFn =>
      lower_letfn_expr_with_body(node, lower_faked_unit_expr(node.loc))
    Expr_LetAnd =>
      lower_letrec_expr_with_body(node, lower_faked_unit_expr(node.loc))
    Expr_Let | Expr_LetMut => lower_let_expr_without_body(node)
    Expr_Assign | Expr_ArraySet | Expr_ArrayAugmentedSet | Expr_Mutate =>
      lower_assign_expr(node)
    Expr_Guard => lower_guard_expr_with_body(node, lower_unit_expr(node.loc))
    Expr_Defer => lower_defer_expr_with_body(node, lower_unit_expr(node.loc))
    Expr_ErrDefer => lower_defer_expr_with_body(node, lower_unit_expr(node.loc))
    Expr_ProofAssert => lower_proof_assert_expr(node)
    Expr_ProofLet => lower_proof_let_expr(node)
    Expr_Return => lower_return_expr(node)
    Expr_Raise => lower_raise_expr(node)
    Expr_Break => lower_break_expr(node)
    Expr_Continue => lower_continue_expr(node)
    Expr_LabelledBlock => lower_labelled_block_expr(node)
    Expr_As => lower_as_expr(node)
    Expr_Is => lower_is_expr(node)
    Expr_RegexMatch => lower_regex_match_expr(node)
    Expr_TemplateWriting => lower_template_writing_expr(node)
    Expr_Quantifier => lower_quantifier_expr(node)
    _ => lower_hole_expr(node.loc)
  }
}

///|
fn lower_quantifier_expr(node : CstNode) -> @syntax.Expr {
  let kind = if node.has_child_kind(fn(kind) { kind is QuantifierKind_Exists }) {
    @syntax.PredicateQuantifierKind::Exists
  } else {
    Forall
  }
  Quantifier(
    kind~,
    binder=node
      .first_child_with_label("name")
      .map(lower_binder_from_node)
      .unwrap_or(lower_dummy_binder(node.loc)),
    binder_ty=node
      .first_child_with_label("type")
      .map(lower_type)
      .unwrap_or(Any(loc=node.loc)),
    body=node
      .first_child_with_label("body")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_literal_expr(node : CstNode) -> @syntax.Expr {
  match node.kind {
    Expr_Interp => Interp(elems=lower_interp_elems(node), loc=node.loc)
    Expr_BytesInterp =>
      BytesInterp(elems=lower_interp_elems(node), loc=node.loc)
    _ =>
      if lower_constant_from_node(node) is Some(c) {
        let loc = node
          .first_child_with_label("semantic_constant")
          .map(constant => constant.loc)
          .unwrap_or(node.loc)
        Constant(c~, loc~)
      } else {
        lower_hole_expr(node.loc)
      }
  }
}

///|
fn lower_template_writing_expr(node : CstNode) -> @syntax.Expr {
  TemplateWriting(
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    template=node
      .first_child_with_label("template")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    is_conditional=node
      .bool_marker_value(fn(kind) { kind is Flag_Conditional })
      .unwrap_or(false),
    loc=node.loc,
  )
}

///|
fn lower_interp_elems(node : CstNode) -> List[@syntax.InterpElem] {
  let elems : Array[@syntax.InterpElem] = []
  for segment in collect_interp_segments(node) {
    if lower_interp_segment_value(segment) is Some(elem) {
      elems.push(elem)
    }
  }
  lower_array_to_list(elems)
}

///|
fn lower_child_token_name(node : CstNode, label : String) -> String {
  if node.first_child_with_label(label) is Some(child) {
    lower_semantic_string(child).unwrap_or("")
  } else {
    ""
  }
}

///|
fn lower_interp_segment(node : CstNode) -> @syntax.InterpElem {
  lower_interp_segment_value(node).unwrap_or(
    Source({ source: "", loc: node.loc, }),
  )
}

///|
fn lower_multiline_string_expr(node : CstNode) -> @syntax.Expr {
  let elems : Array[@syntax.MultilineStringElem] = []
  for element in node.children_with_label("element") {
    match element.kind {
      MultilineStringElement_String =>
        elems.push(String(element.string_payload().unwrap_or("")))
      MultilineStringElement_Interp => {
        let interp_elems : Array[@syntax.InterpElem] = []
        for segment in element.children_with_label("segment") {
          if lower_interp_segment_value(segment) is Some(elem) {
            interp_elems.push(elem)
          }
        }
        elems.push(Interp(lower_array_to_list(interp_elems)))
      }
      _ => ()
    }
  }
  MultilineString(
    elems=lower_array_to_list(elems),
    loc=lower_multiline_string_loc(node),
  )
}

///|
fn lower_constructor_expr(node : CstNode) -> @syntax.Expr {
  let constr = if node.first_child_with_label("field") is Some(field) {
    @syntax.Constructor::{
      name: {
        name: lower_semantic_string(field).unwrap_or(""),
        loc: field.loc,
      },
      extra_info: lower_constructor_extra_info(node),
      loc: node.loc,
    }
  } else {
    lower_constructor_from_names(
      node.children_with_label("name"),
      node
      .first_child_of_kind(NodeKind::is_constructor_extra_info)
      .map(fn(marker) { marker.kind }),
      node.loc,
    )
  }
  Constr(constr~, loc=node.loc)
}

///|
fn lower_constructor_extra_info(node : CstNode) -> @syntax.ConstructorExtraInfo {
  let marker = node.first_child_of_kind(NodeKind::is_constructor_extra_info)
  match marker.map(fn(marker) { marker.kind }) {
    Some(ConstructorExtraInfo_TypeNameWithConstrPackage) =>
      if node.first_child_with_label("type") is Some(type_node) &&
        node.first_child_with_label("pkg") is Some(pkg_node) {
        TypeNameWithConstrPackage(
          type_name=lower_type_name_from_expr_node(type_node),
          pkg=lower_semantic_string(pkg_node).unwrap_or(""),
        )
      } else {
        NoExtraInfo
      }
    Some(ConstructorExtraInfo_TypeName) =>
      if node.first_child_with_label("type") is Some(type_node) {
        TypeName(lower_type_name_from_expr_node(type_node))
      } else {
        NoExtraInfo
      }
    Some(ConstructorExtraInfo_Package) =>
      if node.first_child_with_label("pkg") is Some(pkg_node) {
        Package(lower_semantic_string(pkg_node).unwrap_or(""))
      } else {
        NoExtraInfo
      }
    _ => NoExtraInfo
  }
}

///|
fn lower_argument_kind(item : CstNode) -> @syntax.ArgumentKind {
  match item.kind {
    Argument_Labelled =>
      Labelled(
        item
        .first_child_with_label("name")
        .map(lower_label_from_node)
        .unwrap_or({ name: "", loc: item.loc, }),
      )
    Argument_LabelledPun =>
      LabelledPun(
        item
        .first_child_with_label("name")
        .map(name => @syntax.Label::{
          name: lower_semantic_string(name).unwrap_or(""),
          loc: name.loc,
        })
        .unwrap_or({ name: "", loc: item.loc, }),
      )
    Argument_LabelledOption =>
      LabelledOption(
        label=item
          .first_child_with_label("name")
          .map(lower_label_from_node)
          .unwrap_or({ name: "", loc: item.loc, }),
        question_loc=lower_token_loc(item, "op", TK_QUESTION).unwrap_or(
          item.loc,
        ),
      )
    Argument_LabelledOptionPun =>
      LabelledOptionPun(
        label=item
          .first_child_with_label("name")
          .map(lower_label_from_node)
          .unwrap_or({ name: "", loc: item.loc, }),
        question_loc=lower_token_loc(item, "op", TK_QUESTION).unwrap_or(
          item.loc,
        ),
      )
    _ => Positional
  }
}

///|
fn lower_arguments(list : CstNode) -> List[@syntax.Argument] {
  let args : Array[@syntax.Argument] = []
  for i in 0..<list.children.length() {
    guard list.children.get(i) is Some((Some("item"), item)) else { continue }
    let value = if item.kind.is_argument() {
      item
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(item.loc))
    } else {
      lower_expr(item)
    }
    args.push({ value, kind: lower_argument_kind(item), })
  }
  lower_array_to_list(args)
}

///|
fn lower_tuple_expr(node : CstNode) -> @syntax.Expr {
  let exprs : Array[@syntax.Expr] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      exprs.push(lower_expr(item))
    }
  } else {
    ()
  }
  match node.kind {
    Expr_Unit =>
      Unit(
        loc=node.loc,
        faked=node
          .bool_marker_value(fn(kind) { kind is Flag_Faked })
          .unwrap_or(false),
      )
    Expr_Group =>
      Group(
        expr=exprs.get(0).unwrap_or(lower_hole_expr(node.loc)),
        group=Paren,
        loc=node.loc,
      )
    Expr_Tuple => Tuple(exprs=lower_array_to_list(exprs), loc=node.loc)
    _ => lower_hole_expr(node.loc)
  }
}

///|
fn lower_constraint_expr(node : CstNode) -> @syntax.Expr {
  Constraint(
    expr=node
      .first_child_with_label("expr")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    ty=node
      .first_child_with_label("type")
      .map(lower_type)
      .unwrap_or(Any(loc=node.loc)),
    loc=node.loc,
  )
}

///|
fn lower_array_expr_node(node : CstNode) -> @syntax.Expr {
  let elems : Array[@syntax.SpreadableElem] = []
  let is_iter = node.has_child_kind(fn(kind) { kind is ArrayKind_Iter })
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      elems.push(lower_array_expr_elem(item))
    }
  } else {
    ()
  }
  @syntax.make_array_expr(loc=node.loc, is_iter~, lower_array_to_list(elems))
}

///|
fn lower_array_expr_elem(item : CstNode) -> @syntax.SpreadableElem {
  match item.kind {
    ArrayElement_Spread =>
      Spread(
        expr=item
          .first_child_with_label("expr")
          .map(lower_expr)
          .unwrap_or(lower_hole_expr(item.loc)),
        loc=item.loc,
      )
    ArrayElement_Regular =>
      Regular(
        item
        .first_child_with_label("expr")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(item.loc)),
      )
    _ => Regular(lower_expr(item))
  }
}

///|
fn lower_list_comprehension_expr(node : CstNode) -> @syntax.Expr {
  let is_iter = node.has_child_kind(fn(kind) { kind is ArrayKind_Iter })
  let kind = if node.has_child_kind(fn(kind) {
      kind is ListComprehensionKind_ForEach
    }) {
    @syntax.ListComprehensionKind::Foreach(
      patterns=lower_foreach_patterns(node),
      expr=node
        .first_child_with_label("expr")
        .map(lower_expr)
        .unwrap_or(lower_hole_expr(node.loc)),
      init=lower_for_binding_list(node, "init"),
      continue_block=lower_for_binding_list(node, "continue"),
    )
  } else {
    For(
      binders=lower_for_binding_list(node, "init"),
      condition=node.first_child_with_label("cond").map(lower_expr),
      continue_block=lower_for_binding_list(node, "continue"),
      for_loc=lower_token_loc(node, "kw", TK_FOR).unwrap_or(node.loc),
    )
  }
  ListComprehension(
    kind~,
    guard_=node.first_child_with_label("guard").map(lower_expr),
    body=node
      .first_child_with_label("body")
      .map(lower_expr)
      .unwrap_or(lower_hole_expr(node.loc)),
    is_iter~,
    loc=node.loc,
  )
}

///|
fn lower_record_expr_node(node : CstNode) -> @syntax.Expr {
  let fields : Array[@syntax.FieldDef] = []
  let mut record : @syntax.Expr? = None
  let mut trailing = @syntax.TrailingMark::None
  if node.first_child_with_label("body") is Some(body) {
    trailing = lower_record_trailing_mark(body)
    for item in lower_item_children(body) {
      if item.kind is Field_RecordSpread {
        record = item.first_child_with_label("expr").map(lower_expr)
      } else {
        fields.push(lower_record_field_def(item))
      }
    }
  } else {
    ()
  }
  let type_name = node
    .first_child_with_label("type")
    .map(lower_type_name_from_expr_node)
  if node.kind is Expr_RecordUpdate {
    RecordUpdate(
      type_name~,
      record=record.unwrap_or(lower_hole_expr(node.loc)),
      fields=lower_array_to_list(fields),
      loc=node.loc,
    )
  } else {
    Record(
      type_name~,
      fields=lower_array_to_list(fields),
      trailing~,
      loc=node.loc,
    )
  }
}

///|
fn lower_record_field_def(node : CstNode) -> @syntax.FieldDef {
  let label_node = node.first_child_with_label("name")
  let label = label_node
    .map(lower_label_from_node)
    .unwrap_or({ name: "", loc: node.loc, })
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  {
    label,
    expr,
    is_pun: node
    .bool_marker_value(fn(kind) { kind is Flag_Pun })
    .unwrap_or(false),
    loc: node.loc,
  }
}

///|
fn lower_where_clause(node : CstNode) -> @syntax.WhereClause {
  let fields : Array[@syntax.FieldDef] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      fields.push(lower_record_field_def(item))
    }
  } else {
    ()
  }
  { fields: lower_array_to_list(fields), loc: node.loc, }
}

///|
fn lower_record_trailing_mark(node : CstNode) -> @syntax.TrailingMark {
  match
    node.first_child_of_kind(fn(kind) {
      kind is (TrailingMark_Comma | TrailingMark_Semi | TrailingMark_None)
    }) {
    Some(mark) =>
      return match mark.kind {
        TrailingMark_Comma => Comma
        TrailingMark_Semi => Semi
        _ => None
      }
    None => ()
  }
  None
}

///|
fn lower_map_expr_node(node : CstNode) -> @syntax.Expr {
  let elems : Array[@syntax.MapExprElem] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      elems.push(lower_map_expr_elem(item))
    }
  } else {
    ()
  }
  Map(elems=lower_array_to_list(elems), loc=node.loc)
}

///|
fn lower_map_expr_elem(node : CstNode) -> @syntax.MapExprElem {
  let constant_node = node.first_child_with_label("semantic_constant")
  let (key, key_loc) = if constant_node is Some(constant_node) {
    let key = lower_constant_from_node(constant_node)
    let key_loc = if node.first_child_of_kind(fn(kind) {
        kind is Operator_UnaryMinus
      })
      is Some(operator) {
      operator.loc.merge(constant_node.loc)
    } else {
      constant_node.loc
    }
    if key is Some(key) {
      (key, key_loc)
    } else {
      (String(""), key_loc)
    }
  } else {
    (String(""), node.loc)
  }
  {
    key,
    expr: node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc)),
    key_loc,
    loc: node.loc,
  }
}

///|
fn lower_infix_expr(node : CstNode) -> @syntax.Expr {
  let lhs = node
    .first_child_with_label("lhs")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let rhs = node
    .first_child_with_label("rhs")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  match node.kind {
    Expr_Pipe => Pipe(lhs~, rhs~, loc=node.loc)
    Expr_RevPipe => RevPipe(lhs~, rhs~, loc=node.loc)
    Expr_Implies => Implies(lhs~, rhs~, loc=node.loc)
    _ => {
      let op_node = node
        .first_child_of_kind(NodeKind::is_operator)
        .unwrap_or(node)
      let op_var = @syntax.Var::{
        name: Ident(name=op_node.string_payload().unwrap_or("")),
        loc: op_node.loc,
      }
      Infix(op=op_var, lhs~, rhs~, loc=node.loc)
    }
  }
}

///|
fn lower_prefix_expr(node : CstNode) -> @syntax.Expr {
  let expr = node
    .first_child_with_label("expr")
    .map(lower_expr)
    .unwrap_or(lower_hole_expr(node.loc))
  let operator = node.first_child_of_kind(NodeKind::is_operator).unwrap_or(node)
  let op_name = operator.string_payload().unwrap_or("")
  let op = @syntax.Var::{ name: Ident(name=op_name), loc: node.loc, }
  Unary(op~, expr~, loc=node.loc)
}
