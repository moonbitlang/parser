///|
fn lower_pattern(node : CstNode) -> @syntax.Pattern {
  match node.kind {
    Pattern_Var =>
      if node.first_child_with_label("name") is Some(name) {
        Var(lower_binder_from_node(name))
      } else {
        Any(loc=node.loc)
      }
    Pattern_Any | Pattern_MissingPattern => Any(loc=node.loc)
    Pattern_Constant =>
      if lower_constant_from_node(node) is Some(c) {
        let loc = if node.has_child_kind(fn(kind) {
            kind is Operator_UnaryMinus
          }) {
          node
          .first_child_with_label("semantic_constant")
          .map(constant => constant.loc)
          .unwrap_or(node.loc)
        } else {
          node.loc
        }
        Constant(c~, loc~)
      } else {
        Any(loc=node.loc)
      }
    Pattern_Tuple => lower_tuple_pattern(node)
    Pattern_Array => Array(pats=lower_array_patterns(node), loc=node.loc)
    Pattern_Record =>
      Record(
        fields=lower_record_pattern_fields(node),
        is_closed=lower_record_pattern_is_closed(node),
        loc=node.loc,
      )
    Pattern_Map =>
      Map(
        elems=lower_map_pattern_elems(node),
        is_closed=lower_map_pattern_is_closed(node),
        loc=node.loc,
      )
    Pattern_Constr => lower_constructor_pattern(node)
    Pattern_ConstrAny =>
      ConstrAny(
        type_name=node
          .first_child_with_label("type")
          .map(lower_type_name_from_type_node)
          .unwrap_or({
            name: @syntax.LongIdent::dummy(),
            is_object: false,
            loc: node.loc,
          }),
        loc=node.loc,
      )
    Pattern_SpecialConstr => lower_special_constructor_pattern(node)
    Pattern_Or => {
      let lhs = node
        .first_child_with_label("lhs")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc))
      let rhs = node
        .first_child_with_label("rhs")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc))
      Or(pat1=lhs, pat2=rhs, loc=node.loc)
    }
    Pattern_Range => {
      let lhs = node
        .first_child_with_label("lhs")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc))
      let rhs = node
        .first_child_with_label("rhs")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc))
      Range(lhs~, rhs~, kind=lower_pattern_range_kind(node), loc=node.loc)
    }
    Pattern_Alias => {
      let pat = node
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc))
      let binder = node
        .first_child_with_label("name")
        .map(lower_binder_from_node)
        .unwrap_or(lower_dummy_binder(node.loc))
      Alias(pat~, alias_=binder, loc=node.loc)
    }
    Pattern_Constraint => {
      let pat = node
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc))
      let ty = node
        .first_child_with_label("type")
        .map(lower_type)
        .unwrap_or(Any(loc=node.loc))
      Constraint(pat~, ty~, loc=node.loc)
    }
    Pattern_With => {
      let pat = node
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc))
      let bindings : Array[(@syntax.Binder, @syntax.Expr)] = []
      if node.first_child_with_label("body") is Some(body) {
        for binding in lower_item_children(body) {
          bindings.push(
            (
              binding
              .first_child_with_label("name")
              .map(lower_binder_from_node)
              .unwrap_or(lower_dummy_binder(binding.loc)),
              binding
              .first_child_with_label("expr")
              .map(lower_expr)
              .unwrap_or(lower_hole_expr(binding.loc)),
            ),
          )
        }
      }
      With(pat~, bindings=lower_array_to_list(bindings), loc=node.loc)
    }
    _ => Any(loc=node.loc)
  }
}

///|
fn lower_pattern_range_kind(node : CstNode) -> @syntax.RangeKind {
  match
    node.first_child_of_kind(fn(kind) {
      kind is (RangeKind_Inclusive | RangeKind_Exclusive)
    }) {
    Some(kind_node) =>
      return match kind_node.kind {
        RangeKind_Exclusive => Exclusive
        RangeKind_Inclusive => Inclusive
        _ => InclusiveMissingEqual
      }
    None => ()
  }
  InclusiveMissingEqual
}

///|
fn lower_array_patterns(node : CstNode) -> @syntax.ArrayPatterns {
  let prefix : Array[@syntax.ArrayPattern] = []
  let suffix : Array[@syntax.ArrayPattern] = []
  let mut rest : @syntax.DotDotBinder? = None
  if node.first_child_with_label("body") is Some(patterns) {
    let body = patterns.first_child_with_label("body").unwrap_or(patterns)
    for item in lower_item_children(body) {
      match item.kind {
        Pattern_Rest if rest is None => rest = Some(lower_dot_dot_binder(item))
        _ =>
          if rest is Some(_) {
            suffix.push(lower_array_pattern_item(item))
          } else {
            prefix.push(lower_array_pattern_item(item))
          }
      }
    }
  } else {
    ()
  }
  if node.first_child_with_label("body").map(patterns => patterns.kind)
    is Some(ArrayPatterns_Open) &&
    rest is Some(dot_dot_binder) {
    Open(
      lower_array_to_list(prefix),
      lower_array_to_list(suffix),
      dot_dot_binder,
    )
  } else {
    Closed(lower_array_to_list(prefix))
  }
}

///|
fn lower_array_pattern_item(node : CstNode) -> @syntax.ArrayPattern {
  match node.kind {
    ArrayPattern_Pattern =>
      Pattern(
        node
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(Any(loc=node.loc)),
      )
    ArrayPattern_StringSpread =>
      if node.first_child_with_label("str") is Some(str_node) {
        StringSpread(
          str=lower_semantic_string(str_node).unwrap_or(""),
          loc=str_node.loc,
        )
      } else {
        StringSpread(str="", loc=node.loc)
      }
    ArrayPattern_BytesSpread =>
      if node.first_child_with_label("bytes") is Some(bytes_node) {
        BytesSpread(
          bytes=lower_semantic_string(bytes_node).unwrap_or(""),
          loc=bytes_node.loc,
        )
      } else {
        BytesSpread(bytes="", loc=node.loc)
      }
    ArrayPattern_ConstSpread =>
      if node.first_child_with_label("const") is Some(name_node) {
        let pkg = if node.first_child_with_label("pkg") is Some(pkg_node) {
          lower_semantic_string(pkg_node)
        } else {
          None
        }
        ConstSpread(
          binder=lower_binder_from_node(name_node),
          pkg~,
          loc=node.loc,
        )
      } else {
        Pattern(Any(loc=node.loc))
      }
    Pattern_Rest => Pattern(Any(loc=node.loc))
    _ => Pattern(lower_pattern(node))
  }
}

///|
fn lower_dot_dot_binder(node : CstNode) -> @syntax.DotDotBinder {
  match node.first_child_of_kind(NodeKind::is_dot_dot_binder) {
    Some(marker) =>
      match marker.kind {
        DotDotBinder_Underscore => Underscore
        DotDotBinder_Binder =>
          node
          .first_child_with_label("name")
          .map(lower_binder_from_node)
          .map(value => @syntax.DotDotBinder::Binder(value))
          .unwrap_or(NoBinder)
        DotDotBinder_BinderAs =>
          node
          .first_child_with_label("name")
          .map(lower_binder_from_node)
          .map(value => @syntax.DotDotBinder::BinderAs(value))
          .unwrap_or(NoBinder)
        _ => NoBinder
      }
    None => NoBinder
  }
}

///|
fn lower_record_pattern_fields(node : CstNode) -> List[@syntax.FieldPat] {
  let fields : Array[@syntax.FieldPat] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      if !(item.kind is Pattern_Rest) {
        fields.push(lower_record_pattern_field(item))
      }
    }
  } else {
    ()
  }
  lower_array_to_list(fields)
}

///|
fn lower_record_pattern_is_closed(node : CstNode) -> Bool {
  if node.has_child_kind(fn(kind) { kind is Openness_Closed }) {
    return true
  }
  if node.has_child_kind(fn(kind) { kind is Openness_Open }) {
    return false
  }
  true
}

///|
fn lower_record_pattern_field(node : CstNode) -> @syntax.FieldPat {
  let label_node = node.first_child_with_label("name")
  let label = label_node
    .map(lower_label_from_node)
    .unwrap_or({ name: "", loc: node.loc, })
  let pattern = node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(Any(loc=node.loc))
  {
    label,
    pattern,
    is_pun: node
    .bool_marker_value(fn(kind) { kind is Flag_Pun })
    .unwrap_or(false),
    loc: node.loc,
  }
}

///|
fn lower_map_pattern_is_closed(node : CstNode) -> Bool {
  if node.has_child_kind(fn(kind) { kind is Openness_Closed }) {
    return true
  }
  if node.has_child_kind(fn(kind) { kind is Openness_Open }) {
    return false
  }
  true
}

///|
fn lower_map_pattern_elems(node : CstNode) -> List[@syntax.MapPatElem] {
  let elems : Array[@syntax.MapPatElem] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      if !(item.kind is Pattern_Rest) {
        elems.push(lower_map_pattern_elem(item))
      }
    }
  } else {
    ()
  }
  lower_array_to_list(elems)
}

///|
fn lower_map_pattern_elem(node : CstNode) -> @syntax.MapPatElem {
  let (key, key_loc) = lower_map_pattern_key(node)
  {
    key,
    pat: node
    .first_child_with_label("pattern")
    .map(lower_pattern)
    .unwrap_or(@syntax.Pattern::Any(loc=node.loc)),
    match_absent: node
    .bool_marker_value(fn(kind) { kind is Flag_MatchAbsent })
    .unwrap_or(false),
    key_loc,
    loc: node.loc,
  }
}

///|
fn lower_map_pattern_key(node : CstNode) -> (@syntax.Constant, Location) {
  if node.first_child_with_label("semantic_constant") is Some(constant_node) {
    let key = lower_constant_from_node(constant_node).unwrap_or(String(""))
    let key_loc = match
      node.first_child_of_kind(fn(kind) { kind is Operator_UnaryMinus }) {
      Some(operator) => operator.loc.merge(constant_node.loc)
      None => constant_node.loc
    }
    (key, key_loc)
  } else {
    (String(""), node.loc)
  }
}

///|
fn lower_pattern_items(node : CstNode) -> Array[@syntax.Pattern] {
  let result : Array[@syntax.Pattern] = []
  if node.first_child_with_label("body") is Some(body) {
    for item in lower_item_children(body) {
      result.push(lower_pattern(item))
    }
  } else {
    ()
  }
  result
}

///|
fn lower_tuple_pattern(node : CstNode) -> @syntax.Pattern {
  let items = lower_pattern_items(node)
  if node.has_child_kind(fn(kind) { kind is Pattern_Group }) {
    match items.get(0).unwrap_or(@syntax.Pattern::Any(loc=node.loc)) {
      @syntax.Pattern::Constraint(pat~, ty~, loc=_) =>
        @syntax.Pattern::Constraint(pat~, ty~, loc=node.loc)
      item => item
    }
  } else {
    Tuple(pats=lower_array_to_list(items), loc=node.loc)
  }
}

///|
fn lower_constructor_from_names(
  names : Array[CstNode],
  extra_info_kind : NodeKind?,
  loc : Location,
) -> @syntax.Constructor {
  let extra_info = match extra_info_kind {
    Some(ConstructorExtraInfo_Package) if names.length() >= 2 =>
      @syntax.ConstructorExtraInfo::Package(
        lower_semantic_string(names[0]).unwrap_or(""),
      )
    Some(ConstructorExtraInfo_TypeName) if names.length() >= 2 => {
      let type_nodes : Array[CstNode] = []
      for i in 0..<(names.length() - 1) {
        type_nodes.push(names[i])
      }
      let type_loc = type_nodes[0].loc.merge(
        type_nodes[type_nodes.length() - 1].loc,
      )
      TypeName({
        name: recover_long_ident_from_name_nodes(type_nodes),
        is_object: false,
        loc: type_loc,
      })
    }
    _ => NoExtraInfo
  }
  let loc = if names.length() > 0 {
    names[0].loc.merge(names[names.length() - 1].loc)
  } else {
    loc
  }
  let (name, name_loc) = if names.length() > 0 {
    let name_node = names[names.length() - 1]
    (lower_semantic_string(name_node).unwrap_or(""), name_node.loc)
  } else {
    ("", loc)
  }
  { name: { name, loc: name_loc, }, extra_info, loc, }
}

///|
fn lower_constructor_pattern_args(
  node : CstNode,
) -> List[@syntax.ConstrPatArg]? {
  if node.first_child_with_label("params") is Some(list) {
    let args : Array[@syntax.ConstrPatArg] = []
    for item in lower_item_children(list) {
      if !(item.kind is Pattern_Rest) {
        args.push(lower_constructor_pattern_arg(item))
      }
    }
    Some(lower_array_to_list(args))
  } else {
    None
  }
}

///|
fn constructor_pattern_is_open(node : CstNode) -> Bool {
  if node.has_child_kind(fn(kind) { kind is Openness_Open }) {
    return true
  }
  if node.has_child_kind(fn(kind) { kind is Openness_Closed }) {
    return false
  }
  false
}

///|
fn lower_constructor_pattern_arg(item : CstNode) -> @syntax.ConstrPatArg {
  match item.kind {
    Argument_LabelledPun =>
      {
        pat: item
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(Any(loc=item.loc)),
        kind: lower_argument_kind(item),
      }
    Argument_Labelled =>
      {
        pat: item
        .first_child_with_label("pattern")
        .map(lower_pattern)
        .unwrap_or(Any(loc=item.loc)),
        kind: Labelled(
          item
          .first_child_with_label("name")
          .map(lower_label_from_node)
          .unwrap_or({ name: "", loc: item.loc, }),
        ),
      }
    _ => { pat: lower_pattern(item), kind: Positional, }
  }
}

///|
fn lower_constructor_pattern(node : CstNode) -> @syntax.Pattern {
  let constr = if node.first_child_with_label("field") is Some(field) {
    let loc = node
      .first_child_with_label("type")
      .map(type_name => type_name.loc.merge(field.loc))
      .unwrap_or(field.loc)
    @syntax.Constructor::{
      name: {
        name: lower_semantic_string(field).unwrap_or(""),
        loc: field.loc,
      },
      extra_info: lower_constructor_extra_info(node),
      loc,
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
  Constr(
    constr~,
    args=lower_constructor_pattern_args(node),
    is_open=constructor_pattern_is_open(node),
    loc=node.loc,
  )
}

///|
fn lower_special_constructor_pattern(node : CstNode) -> @syntax.Pattern {
  let binder = node
    .first_child_with_label("name")
    .map(lower_binder_from_node)
    .unwrap_or(lower_dummy_binder(node.loc))
  let args : Array[@syntax.ConstrPatArg] = []
  if node.first_child_with_label("params") is Some(list) {
    for item in lower_item_children(list) {
      args.push({ pat: lower_pattern(item), kind: Positional, })
    }
  } else {
    ()
  }
  SpecialConstr(binder~, args=lower_array_to_list(args), loc=node.loc)
}
