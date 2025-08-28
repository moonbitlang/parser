///|
pub(open) trait ImplMapVisitor {
  base(Self) ->  MapVisitorBase[Self] = _
  visit_impl(Self, Impl) -> Impl = _
  visit_top_expr(
    Self,
    expr~ : Expr,
    is_main~ : Bool,
    local_types~ : @list.List[LocalTypeDecl],
    loc~ : Location,
  ) -> Impl = _
  visit_top_test(
    Self,
    expr~ : Expr,
    name~ : TestName,
    params~ : Parameters?,
    local_types~ : @list.List[LocalTypeDecl],
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> Impl = _
  visit_top_type_def(Self, TypeDecl) -> Impl = _
  visit_top_func_def(
    Self,
    fun_decl~ : FunDecl,
    decl_body~ : DeclBody,
    loc~ : Location,
  ) -> Impl = _
  visit_top_func_alias(
    Self,
    pkg~ : Label?,
    type_name~ : Label?,
    targets~ : @list.List[AliasTarget],
    vis~ : Visibility,
    attrs~ : @list.List[Attribute],
    is_list~ : Bool,
    doc~ : DocString,
    loc~ : Location,
  ) -> Impl = _
  visit_top_let_def(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    expr~ : Expr,
    vis~ : Visibility,
    is_constant~ : Bool,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> Impl = _
  visit_top_trait(Self, TraitDecl) -> Impl = _
  visit_top_batch_type_alias(
    Self,
    pkg~ : Label?,
    targets~ : @list.List[AliasTarget],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    is_list~ : Bool,
    doc~ : DocString,
  ) -> Impl = _
  visit_top_batch_trait_alias(
    Self,
    pkg~ : Label?,
    targets~ : @list.List[AliasTarget],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    is_list~ : Bool,
    doc~ : DocString,
  ) -> Impl = _
  visit_top_impl(
    Self,
    self_ty~ : Type?,
    trait_~ : TypeName,
    method_name~ : Binder,
    has_error~ : Location?,
    quantifiers~ : @list.List[TypeVarBinder],
    params~ : Parameters,
    ret_ty~ : Type?,
    err_ty~ : ErrorType,
    body~ : DeclBody,
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> Impl = _
  visit_top_view(
    Self,
    quantifiers~ : @list.List[TypeVarBinder],
    source_ty~ : Type,
    view_type_name~ : String,
    view_type_loc~ : Location,
    view_constrs~ : @list.List[ConstrDecl],
    view_func_name~ : Binder,
    parameters~ : Parameters,
    params_loc~ : Location,
    body~ : Expr,
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> Impl = _
  visit_top_impl_relation(
    Self,
    self_ty~ : Type,
    trait_~ : TypeName,
    quantifiers~ : @list.List[TypeVarBinder],
    vis~ : Visibility,
    attrs~ : @list.List[Attribute],
    loc~ : Location,
    doc~ : DocString,
  ) -> Impl = _
  visit_expr(Self, Expr) -> Expr = _
  visit_apply(
    Self,
    func~ : Expr,
    args~ : @list.List[Argument],
    attr~ : ApplyAttr,
    loc~ : Location,
  ) -> Expr = _
  visit_infix(Self, op~ : Var, lhs~ : Expr, rhs~ : Expr, loc~ : Location) -> Expr = _
  visit_unary(Self, op~ : Var, expr~ : Expr, loc~ : Location) -> Expr = _
  visit_array(Self, exprs~ : @list.List[Expr], loc~ : Location) -> Expr = _
  visit_array_spread(Self, elems~ : @list.List[SpreadableElem], loc~ : Location) -> Expr = _
  visit_array_get(Self, array~ : Expr, index~ : Expr, loc~ : Location) -> Expr = _
  visit_array_get_slice(
    Self,
    array~ : Expr,
    start_index~ : Expr?,
    end_index~ : Expr?,
    index_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  visit_array_set(
    Self,
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_array_augmented_set(
    Self,
    op~ : Var,
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_constant(Self, c~ : Constant, loc~ : Location) -> Expr = _
  visit_multiline_string(
    Self,
    elems~ : @list.List[MultilineStringElem],
    loc~ : Location,
  ) -> Expr = _
  visit_interp(Self, elems~ : @list.List[InterpElem], loc~ : Location) -> Expr = _
  visit_constraint(Self, expr~ : Expr, ty~ : Type, loc~ : Location) -> Expr = _
  visit_constr(Self, constr~ : Constructor, loc~ : Location) -> Expr = _
  visit_while(
    Self,
    loop_cond~ : Expr,
    loop_body~ : Expr,
    while_else~ : Expr?,
    label~ : Label?,
    loc~ : Location,
  ) -> Expr = _
  visit_function(Self, func~ : Func, loc~ : Location) -> Expr = _
  visit_ident(Self, id~ : Var, loc~ : Location) -> Expr = _
  visit_if(Self, cond~ : Expr, ifso~ : Expr, ifnot~ : Expr?, loc~ : Location) -> Expr = _
  visit_guard(
    Self,
    cond~ : Expr,
    otherwise~ : Expr?,
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_is(Self, expr~ : Expr, pat~ : Pattern, loc~ : Location) -> Expr = _
  visit_defer(Self, expr~ : Expr, body~ : Expr, loc~ : Location) -> Expr = _
  visit_let_fn(
    Self,
    name~ : Binder,
    func~ : Func,
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_let_rec(
    Self,
    bindings~ : @list.List[(Binder, Func)],
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_let_and(
    Self,
    bindings~ : @list.List[(Binder, Type?, Func)],
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_let(
    Self,
    pattern~ : Pattern,
    expr~ : Expr,
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_sequence(
    Self,
    exprs~ : @list.List[Expr],
    last_expr~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_tuple(Self, exprs~ : @list.List[Expr], loc~ : Location) -> Expr = _
  visit_record(
    Self,
    type_name~ : TypeName?,
    fields~ : @list.List[FieldDef],
    trailing~ : TrailingMark,
    loc~ : Location,
  ) -> Expr = _
  visit_record_update(
    Self,
    type_name~ : TypeName?,
    record~ : Expr,
    fields~ : @list.List[FieldDef],
    loc~ : Location,
  ) -> Expr = _
  visit_field(Self, record~ : Expr, accessor~ : Accessor, loc~ : Location) -> Expr = _
  visit_method(
    Self,
    type_name~ : TypeName,
    method_name~ : Label,
    loc~ : Location,
  ) -> Expr = _
  visit_dot_apply(
    Self,
    self~ : Expr,
    method_name~ : Label,
    args~ : @list.List[Argument],
    return_self~ : Bool,
    attr~ : ApplyAttr,
    loc~ : Location,
  ) -> Expr = _
  visit_as(Self, expr~ : Expr, trait_~ : TypeName, loc~ : Location) -> Expr = _
  visit_mutate(
    Self,
    record~ : Expr,
    accessor~ : Accessor,
    field~ : Expr,
    augmented_by~ : Var?,
    loc~ : Location,
  ) -> Expr = _
  visit_match(
    Self,
    expr~ : Expr,
    cases~ : @list.List[Case],
    match_loc~ : Location,
    using_~ : Label?,
    loc~ : Location,
  ) -> Expr = _
  visit_let_mut(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    expr~ : Expr,
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_pipe(Self, lhs~ : Expr, rhs~ : Expr, loc~ : Location) -> Expr = _
  visit_assign(
    Self,
    var_~ : Var,
    expr~ : Expr,
    augmented_by~ : Var?,
    loc~ : Location,
  ) -> Expr = _
  visit_hole(Self, loc~ : Location, kind~ : Hole) -> Expr = _
  visit_return(Self, return_value~ : Expr?, loc~ : Location) -> Expr = _
  visit_raise(Self, err_value~ : Expr, loc~ : Location) -> Expr = _
  visit_unit(Self, loc~ : Location, faked~ : Bool) -> Expr = _
  visit_break(Self, arg~ : Expr?, label~ : Label?, loc~ : Location) -> Expr = _
  visit_continue(
    Self,
    args~ : @list.List[Expr],
    label~ : Label?,
    loc~ : Location,
  ) -> Expr = _
  visit_loop(
    Self,
    args~ : @list.List[Expr],
    body~ : @list.List[MultiArgCase],
    label~ : Label?,
    loop_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  visit_for(
    Self,
    binders~ : @list.List[(Binder, Expr)],
    condition~ : Expr?,
    continue_block~ : @list.List[(Binder, Expr)],
    body~ : Expr,
    for_else~ : Expr?,
    label~ : Label?,
    loc~ : Location,
  ) -> Expr = _
  visit_for_each(
    Self,
    binders~ : @list.List[Binder?],
    expr~ : Expr,
    body~ : Expr,
    else_block~ : Expr?,
    label~ : Label?,
    loc~ : Location,
  ) -> Expr = _
  visit_try(
    Self,
    body~ : Expr,
    catch_~ : @list.List[Case],
    catch_all~ : Bool,
    try_else~ : @list.List[Case]?,
    has_try~ : Bool,
    try_loc~ : Location,
    catch_loc~ : Location,
    else_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  visit_try_operator(
    Self,
    body~ : Expr,
    kind~ : TryOperatorKind,
    try_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  visit_map(Self, elems~ : @list.List[MapExprElem], loc~ : Location) -> Expr = _
  visit_group(Self, expr~ : Expr, group~ : Group, loc~ : Location) -> Expr = _
  visit_static_assert(
    Self,
    asserts~ : @list.List[StaticAssertion],
    body~ : Expr,
  ) -> Expr = _
}

impl ImplMapVisitor with base(self) -> MapVisitorBase[Self] {
  MapVisitorBase::MapVisitorBase(self)
} 

///|
impl ImplMapVisitor with visit_impl(env, impl_) {
  MapVisitorBase::visit_impl(env, impl_)
}

///|
impl ImplMapVisitor with visit_top_expr(
  env,
  expr~,
  is_main~,
  local_types~,
  loc~,
) {
  MapVisitorBase::visit_top_expr(env, expr~, is_main~, local_types~, loc~)
}

///|
impl ImplMapVisitor with visit_top_test(
  env,
  expr~,
  name~,
  params~,
  local_types~,
  loc~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_top_test(
    env,
    expr~,
    name~,
    params~,
    local_types~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
impl ImplMapVisitor with visit_top_type_def(env, type_decl) {
  MapVisitorBase::visit_top_type_def(env, type_decl)
}

///|
impl ImplMapVisitor with visit_top_func_def(env, fun_decl~, decl_body~, loc~) {
  MapVisitorBase::visit_top_func_def(env, fun_decl~, decl_body~, loc~)
}

///|
impl ImplMapVisitor with visit_top_func_alias(
  env,
  pkg~,
  type_name~,
  targets~,
  vis~,
  attrs~,
  is_list~,
  doc~,
  loc~,
) {
  MapVisitorBase::visit_top_func_alias(
    env,
    pkg~,
    type_name~,
    targets~,
    vis~,
    attrs~,
    is_list~,
    doc~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_top_let_def(
  env,
  binder~,
  ty~,
  expr~,
  vis~,
  is_constant~,
  loc~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_top_let_def(
    env,
    binder~,
    ty~,
    expr~,
    vis~,
    is_constant~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
impl ImplMapVisitor with visit_top_trait(env, trait_decl) {
  MapVisitorBase::visit_top_trait(env, trait_decl)
}

///|
impl ImplMapVisitor with visit_top_batch_type_alias(
  env,
  pkg~,
  targets~,
  vis~,
  loc~,
  attrs~,
  is_list~,
  doc~,
) {
  MapVisitorBase::visit_top_batch_type_alias(
    env,
    pkg~,
    targets~,
    vis~,
    loc~,
    attrs~,
    is_list~,
    doc~,
  )
}

///|
impl ImplMapVisitor with visit_top_batch_trait_alias(
  env,
  pkg~,
  targets~,
  vis~,
  loc~,
  attrs~,
  is_list~,
  doc~,
) {
  MapVisitorBase::visit_top_batch_trait_alias(
    env,
    pkg~,
    targets~,
    vis~,
    loc~,
    attrs~,
    is_list~,
    doc~,
  )
}

///|
impl ImplMapVisitor with visit_top_impl(
  env,
  self_ty~,
  trait_~,
  method_name~,
  has_error~,
  quantifiers~,
  params~,
  ret_ty~,
  err_ty~,
  body~,
  vis~,
  loc~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_top_impl(
    env,
    self_ty~,
    trait_~,
    method_name~,
    has_error~,
    quantifiers~,
    params~,
    ret_ty~,
    err_ty~,
    body~,
    vis~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
impl ImplMapVisitor with visit_top_view(
  env,
  quantifiers~,
  source_ty~,
  view_type_name~,
  view_type_loc~,
  view_constrs~,
  view_func_name~,
  parameters~,
  params_loc~,
  body~,
  vis~,
  loc~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_top_view(
    env,
    quantifiers~,
    source_ty~,
    view_type_name~,
    view_type_loc~,
    view_constrs~,
    view_func_name~,
    parameters~,
    params_loc~,
    body~,
    vis~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
impl ImplMapVisitor with visit_top_impl_relation(
  env,
  self_ty~,
  trait_~,
  quantifiers~,
  vis~,
  attrs~,
  loc~,
  doc~,
) {
  MapVisitorBase::visit_top_impl_relation(
    env,
    self_ty~,
    trait_~,
    quantifiers~,
    vis~,
    attrs~,
    loc~,
    doc~,
  )
}

///|
impl ImplMapVisitor with visit_expr(env, expr) {
  MapVisitorBase::visit_expr(env, expr)
}

///|
impl ImplMapVisitor with visit_apply(env, func~, args~, attr~, loc~) {
  MapVisitorBase::visit_apply(env, func~, args~, attr~, loc~)
}

///|
impl ImplMapVisitor with visit_infix(env, op~, lhs~, rhs~, loc~) {
  MapVisitorBase::visit_infix(env, op~, lhs~, rhs~, loc~)
}

///|
impl ImplMapVisitor with visit_unary(env, op~, expr~, loc~) {
  MapVisitorBase::visit_unary(env, op~, expr~, loc~)
}

///|
impl ImplMapVisitor with visit_array(env, exprs~, loc~) {
  MapVisitorBase::visit_array(env, exprs~, loc~)
}

///|
impl ImplMapVisitor with visit_array_spread(env, elems~, loc~) {
  MapVisitorBase::visit_array_spread(env, elems~, loc~)
}

///|
impl ImplMapVisitor with visit_array_get(env, array~, index~, loc~) {
  MapVisitorBase::visit_array_get(env, array~, index~, loc~)
}

///|
impl ImplMapVisitor with visit_array_get_slice(
  env,
  array~,
  start_index~,
  end_index~,
  index_loc~,
  loc~,
) {
  MapVisitorBase::visit_array_get_slice(
    env,
    array~,
    start_index~,
    end_index~,
    index_loc~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_array_set(env, array~, index~, value~, loc~) {
  MapVisitorBase::visit_array_set(env, array~, index~, value~, loc~)
}

///|
impl ImplMapVisitor with visit_array_augmented_set(
  env,
  op~,
  array~,
  index~,
  value~,
  loc~,
) {
  MapVisitorBase::visit_array_augmented_set(
    env,
    op~,
    array~,
    index~,
    value~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_constant(env, c~, loc~) {
  MapVisitorBase::visit_constant(env, c~, loc~)
}

///|
impl ImplMapVisitor with visit_multiline_string(env, elems~, loc~) {
  MapVisitorBase::visit_multiline_string(env, elems~, loc~)
}

///|
impl ImplMapVisitor with visit_interp(env, elems~, loc~) {
  MapVisitorBase::visit_interp(env, elems~, loc~)
}

///|
impl ImplMapVisitor with visit_constraint(env, expr~, ty~, loc~) {
  MapVisitorBase::visit_constraint(env, expr~, ty~, loc~)
}

///|
impl ImplMapVisitor with visit_constr(env, constr~, loc~) {
  MapVisitorBase::visit_constr(env, constr~, loc~)
}

///|
impl ImplMapVisitor with visit_while(
  env,
  loop_cond~,
  loop_body~,
  while_else~,
  label~,
  loc~,
) {
  MapVisitorBase::visit_while(
    env,
    loop_cond~,
    loop_body~,
    while_else~,
    label~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_function(env, func~, loc~) {
  MapVisitorBase::visit_function(env, func~, loc~)
}

///|
impl ImplMapVisitor with visit_ident(env, id~, loc~) {
  MapVisitorBase::visit_ident(env, id~, loc~)
}

///|
impl ImplMapVisitor with visit_if(env, cond~, ifso~, ifnot~, loc~) {
  MapVisitorBase::visit_if(env, cond~, ifso~, ifnot~, loc~)
}

///|
impl ImplMapVisitor with visit_guard(env, cond~, otherwise~, body~, loc~) {
  MapVisitorBase::visit_guard(env, cond~, otherwise~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_is(env, expr~, pat~, loc~) {
  MapVisitorBase::visit_is(env, expr~, pat~, loc~)
}

///|
impl ImplMapVisitor with visit_defer(env, expr~, body~, loc~) {
  MapVisitorBase::visit_defer(env, expr~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_let_fn(env, name~, func~, body~, loc~) {
  MapVisitorBase::visit_let_fn(env, name~, func~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_let_rec(env, bindings~, body~, loc~) {
  MapVisitorBase::visit_let_rec(env, bindings~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_let_and(env, bindings~, body~, loc~) {
  MapVisitorBase::visit_let_and(env, bindings~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_let(env, pattern~, expr~, body~, loc~) {
  MapVisitorBase::visit_let(env, pattern~, expr~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_sequence(env, exprs~, last_expr~, loc~) {
  MapVisitorBase::visit_sequence(env, exprs~, last_expr~, loc~)
}

///|
impl ImplMapVisitor with visit_tuple(env, exprs~, loc~) {
  MapVisitorBase::visit_tuple(env, exprs~, loc~)
}

///|
impl ImplMapVisitor with visit_record(env, type_name~, fields~, trailing~, loc~) {
  MapVisitorBase::visit_record(env, type_name~, fields~, trailing~, loc~)
}

///|
impl ImplMapVisitor with visit_record_update(
  env,
  type_name~,
  record~,
  fields~,
  loc~,
) {
  MapVisitorBase::visit_record_update(env, type_name~, record~, fields~, loc~)
}

///|
impl ImplMapVisitor with visit_field(env, record~, accessor~, loc~) {
  MapVisitorBase::visit_field(env, record~, accessor~, loc~)
}

///|
impl ImplMapVisitor with visit_method(env, type_name~, method_name~, loc~) {
  MapVisitorBase::visit_method(env, type_name~, method_name~, loc~)
}

///|
impl ImplMapVisitor with visit_dot_apply(
  env,
  self~,
  method_name~,
  args~,
  return_self~,
  attr~,
  loc~,
) {
  MapVisitorBase::visit_dot_apply(
    env,
    self~,
    method_name~,
    args~,
    return_self~,
    attr~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_as(env, expr~, trait_~, loc~) {
  MapVisitorBase::visit_as(env, expr~, trait_~, loc~)
}

///|
impl ImplMapVisitor with visit_mutate(
  env,
  record~,
  accessor~,
  field~,
  augmented_by~,
  loc~,
) {
  MapVisitorBase::visit_mutate(
    env,
    record~,
    accessor~,
    field~,
    augmented_by~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_match(
  env,
  expr~,
  cases~,
  match_loc~,
  using_~,
  loc~,
) {
  MapVisitorBase::visit_match(env, expr~, cases~, match_loc~, using_~, loc~)
}

///|
impl ImplMapVisitor with visit_let_mut(env, binder~, ty~, expr~, body~, loc~) {
  MapVisitorBase::visit_let_mut(env, binder~, ty~, expr~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_pipe(env, lhs~, rhs~, loc~) {
  MapVisitorBase::visit_pipe(env, lhs~, rhs~, loc~)
}

///|
impl ImplMapVisitor with visit_assign(env, var_~, expr~, augmented_by~, loc~) {
  MapVisitorBase::visit_assign(env, var_~, expr~, augmented_by~, loc~)
}

///|
impl ImplMapVisitor with visit_hole(env, loc~, kind~) {
  MapVisitorBase::visit_hole(env, loc~, kind~)
}

///|
impl ImplMapVisitor with visit_return(env, return_value~, loc~) {
  MapVisitorBase::visit_return(env, return_value~, loc~)
}

///|
impl ImplMapVisitor with visit_raise(env, err_value~, loc~) {
  MapVisitorBase::visit_raise(env, err_value~, loc~)
}

///|
impl ImplMapVisitor with visit_unit(env, loc~, faked~) {
  MapVisitorBase::visit_unit(env, loc~, faked~)
}

///|
impl ImplMapVisitor with visit_break(env, arg~, label~, loc~) {
  MapVisitorBase::visit_break(env, arg~, label~, loc~)
}

///|
impl ImplMapVisitor with visit_continue(env, args~, label~, loc~) {
  MapVisitorBase::visit_continue(env, args~, label~, loc~)
}

///|
impl ImplMapVisitor with visit_loop(env, args~, body~, label~, loop_loc~, loc~) {
  MapVisitorBase::visit_loop(env, args~, body~, label~, loop_loc~, loc~)
}

///|
impl ImplMapVisitor with visit_for(
  env,
  binders~,
  condition~,
  continue_block~,
  body~,
  for_else~,
  label~,
  loc~,
) {
  MapVisitorBase::visit_for(
    env,
    binders~,
    condition~,
    continue_block~,
    body~,
    for_else~,
    label~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_for_each(
  env,
  binders~,
  expr~,
  body~,
  else_block~,
  label~,
  loc~,
) {
  MapVisitorBase::visit_for_each(
    env,
    binders~,
    expr~,
    body~,
    else_block~,
    label~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_try(
  env,
  body~,
  catch_~,
  catch_all~,
  try_else~,
  has_try~,
  try_loc~,
  catch_loc~,
  else_loc~,
  loc~,
) {
  MapVisitorBase::visit_try(
    env,
    body~,
    catch_~,
    catch_all~,
    try_else~,
    has_try~,
    try_loc~,
    catch_loc~,
    else_loc~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_try_operator(env, body~, kind~, try_loc~, loc~) {
  MapVisitorBase::visit_try_operator(env, body~, kind~, try_loc~, loc~)
}

///|
impl ImplMapVisitor with visit_map(env, elems~, loc~) {
  MapVisitorBase::visit_map(env, elems~, loc~)
}

///|
impl ImplMapVisitor with visit_group(env, expr~, group~, loc~) {
  MapVisitorBase::visit_group(env, expr~, group~, loc~)
}

///|
impl ImplMapVisitor with visit_static_assert(env, asserts~, body~) {
  MapVisitorBase::visit_static_assert(env, asserts~, body~)
}

///|
pub(all) struct MapVisitorBase[T](T)

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_impl(
  env,
  impl_,
) {
  match impl_ {
    TopView(
      quantifiers~,
      source_ty~,
      view_type_name~,
      view_type_loc~,
      view_constrs~,
      parameters~,
      view_func_name~,
      params_loc~,
      body~,
      vis~,
      attrs~,
      loc~,
      doc~
    ) =>
      env
      .inner()
      .visit_top_view(
        quantifiers~,
        source_ty~,
        view_type_name~,
        view_type_loc~,
        view_constrs~,
        view_func_name~,
        parameters~,
        params_loc~,
        body~,
        vis~,
        loc~,
        attrs~,
        doc~,
      )
    TopImpl(
      self_ty~,
      trait_~,
      method_name~,
      has_error~,
      quantifiers~,
      params~,
      ret_ty~,
      err_ty~,
      body~,
      vis~,
      loc~,
      attrs~,
      doc~
    ) =>
      env
      .inner()
      .visit_top_impl(
        self_ty~,
        trait_~,
        method_name~,
        has_error~,
        quantifiers~,
        params~,
        ret_ty~,
        err_ty~,
        body~,
        vis~,
        loc~,
        attrs~,
        doc~,
      )
    TopBatchTraitAlias(pkg~, targets~, vis~, loc~, attrs~, is_list~, doc~) =>
      env
      .inner()
      .visit_top_batch_trait_alias(
        pkg~,
        targets~,
        vis~,
        loc~,
        attrs~,
        is_list~,
        doc~,
      )
    TopBatchTypeAlias(pkg~, targets~, vis~, loc~, attrs~, is_list~, doc~) =>
      env
      .inner()
      .visit_top_batch_type_alias(
        pkg~,
        targets~,
        vis~,
        loc~,
        attrs~,
        is_list~,
        doc~,
      )
    TopTrait(trait_decl) => env.inner().visit_top_trait(trait_decl)
    TopLetDef(binder~, ty~, expr~, vis~, is_constant~, loc~, attrs~, doc~) =>
      env
      .inner()
      .visit_top_let_def(
        binder~,
        ty~,
        expr~,
        vis~,
        is_constant~,
        loc~,
        attrs~,
        doc~,
      )
    TopFuncAlias(pkg~, type_name~, targets~, vis~, attrs~, is_list~, doc~, loc~) =>
      env
      .inner()
      .visit_top_func_alias(
        pkg~,
        type_name~,
        targets~,
        vis~,
        attrs~,
        is_list~,
        doc~,
        loc~,
      )
    TopFuncDef(fun_decl~, decl_body~, loc~) =>
      env.inner().visit_top_func_def(fun_decl~, decl_body~, loc~)
    TopTypeDef(type_decl) => env.inner().visit_top_type_def(type_decl)
    TopTest(expr~, name~, params~, local_types~, loc~, attrs~, doc~) =>
      env
      .inner()
      .visit_top_test(expr~, name~, params~, local_types~, loc~, attrs~, doc~)
    TopExpr(expr~, is_main~, local_types~, loc~) =>
      env.inner().visit_top_expr(expr~, is_main~, local_types~, loc~)
    TopImplRelation(self_ty~, trait_~, quantifiers~, vis~, attrs~, loc~, doc~) =>
      env
      .inner()
      .visit_top_impl_relation(
        self_ty~,
        trait_~,
        quantifiers~,
        vis~,
        attrs~,
        loc~,
        doc~,
      )
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_expr(
  env,
  expr~,
  is_main~,
  local_types~,
  loc~,
) {
  env
  .inner()
  .visit_top_expr(
    expr=env.inner().visit_expr(expr),
    is_main~,
    local_types~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_test(
  env,
  expr~,
  name~,
  params~,
  local_types~,
  loc~,
  attrs~,
  doc~,
) {
  env
  .inner()
  .visit_top_test(
    expr=env.inner().visit_expr(expr),
    name~,
    params~,
    local_types~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_type_def(
  env,
  type_decl,
) {
  ignore(env)
  TopTypeDef(type_decl)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_func_def(
  env,
  fun_decl~,
  decl_body~,
  loc~,
) {
  TopFuncDef(
    fun_decl~,
    decl_body=match decl_body {
      DeclBody(local_types~, expr~) =>
        DeclBody(local_types~, expr=env.inner().visit_expr(expr))
      DeclStubs(_) => decl_body
    },
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_func_alias(
  env,
  pkg~,
  type_name~,
  targets~,
  vis~,
  attrs~,
  is_list~,
  doc~,
  loc~,
) {
  ignore(env)
  TopFuncAlias(pkg~, type_name~, targets~, vis~, attrs~, is_list~, doc~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_let_def(
  env,
  binder~,
  ty~,
  expr~,
  vis~,
  is_constant~,
  loc~,
  attrs~,
  doc~,
) {
  TopLetDef(
    binder~,
    ty~,
    expr=env.inner().visit_expr(expr),
    vis~,
    is_constant~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_trait(
  env,
  trait_decl,
) {
  ignore(env)
  TopTrait(trait_decl)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_batch_type_alias(
  env,
  pkg~,
  targets~,
  vis~,
  loc~,
  attrs~,
  is_list~,
  doc~,
) {
  ignore(env)
  TopBatchTypeAlias(pkg~, targets~, vis~, loc~, attrs~, is_list~, doc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_batch_trait_alias(
  env,
  pkg~,
  targets~,
  vis~,
  loc~,
  attrs~,
  is_list~,
  doc~,
) {
  ignore(env)
  TopBatchTraitAlias(pkg~, targets~, vis~, loc~, attrs~, is_list~, doc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_impl(
  env,
  self_ty~,
  trait_~,
  method_name~,
  has_error~,
  quantifiers~,
  params~,
  ret_ty~,
  err_ty~,
  body~,
  vis~,
  loc~,
  attrs~,
  doc~,
) {
  TopImpl(
    self_ty~,
    trait_~,
    method_name~,
    has_error~,
    quantifiers~,
    params~,
    ret_ty~,
    err_ty~,
    body=match body {
      DeclBody(local_types~, expr~) =>
        DeclBody(local_types~, expr=env.inner().visit_expr(expr))
      DeclStubs(_) => body
    },
    vis~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_view(
  env,
  quantifiers~,
  source_ty~,
  view_type_name~,
  view_type_loc~,
  view_constrs~,
  view_func_name~,
  parameters~,
  params_loc~,
  body~,
  vis~,
  loc~,
  attrs~,
  doc~,
) {
  TopView(
    quantifiers~,
    source_ty~,
    view_type_name~,
    view_type_loc~,
    view_constrs~,
    view_func_name~,
    parameters~,
    params_loc~,
    body=env.inner().visit_expr(body),
    vis~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_top_impl_relation(
  env,
  self_ty~,
  trait_~,
  quantifiers~,
  vis~,
  attrs~,
  loc~,
  doc~,
) {
  ignore(env)
  TopImplRelation(self_ty~, trait_~, quantifiers~, vis~, attrs~, loc~, doc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_apply(
  env,
  func~,
  args~,
  attr~,
  loc~,
) {
  Expr::Apply(
    func=env.inner().visit_expr(func),
    args=args.map(arg => Argument::{
      ..arg,
      value: env.inner().visit_expr(arg.value),
    }),
    attr~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_infix(
  env,
  op~,
  lhs~,
  rhs~,
  loc~,
) {
  Expr::Infix(
    op~,
    lhs=env.inner().visit_expr(lhs),
    rhs=env.inner().visit_expr(rhs),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_unary(
  env,
  op~,
  expr~,
  loc~,
) {
  Expr::Unary(op~, expr=env.inner().visit_expr(expr), loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_array(
  env,
  exprs~,
  loc~,
) {
  Expr::Array(exprs=exprs.map(expr => env.inner().visit_expr(expr)), loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_array_spread(
  env,
  elems~,
  loc~,
) {
  Expr::ArraySpread(
    elems=elems.map(fn(elem) {
      match elem {
        Regular(expr) => Regular(env.inner().visit_expr(expr))
        Spread(expr~, loc~) => Spread(expr=env.inner().visit_expr(expr), loc~)
      }
    }),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_array_get(
  env,
  array~,
  index~,
  loc~,
) {
  Expr::ArrayGet(
    array=env.inner().visit_expr(array),
    index=env.inner().visit_expr(index),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_array_get_slice(
  env,
  array~,
  start_index~,
  end_index~,
  index_loc~,
  loc~,
) {
  Expr::ArrayGetSlice(
    array=env.inner().visit_expr(array),
    start_index=start_index.map(e => env.inner().visit_expr(e)),
    end_index=end_index.map(e => env.inner().visit_expr(e)),
    index_loc~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_array_set(
  env,
  array~,
  index~,
  value~,
  loc~,
) {
  Expr::ArraySet(
    array=env.inner().visit_expr(array),
    index=env.inner().visit_expr(index),
    value=env.inner().visit_expr(value),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_array_augmented_set(
  env,
  op~,
  array~,
  index~,
  value~,
  loc~,
) {
  Expr::ArrayAugmentedSet(
    op~,
    array=env.inner().visit_expr(array),
    index=env.inner().visit_expr(index),
    value=env.inner().visit_expr(value),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_constant(
  env,
  c~,
  loc~,
) {
  ignore(env)
  Expr::Constant(c~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_multiline_string(
  env,
  elems~,
  loc~,
) {
  ignore(env)
  Expr::MultilineString(elems~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_interp(
  env,
  elems~,
  loc~,
) {
  ignore(env)
  Expr::Interp(elems~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_constraint(
  env,
  expr~,
  ty~,
  loc~,
) {
  ignore(env)
  Expr::Constraint(expr=env.inner().visit_expr(expr), ty~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_constr(
  env,
  constr~,
  loc~,
) {
  ignore(env)
  Expr::Constr(constr~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_while(
  env,
  loop_cond~,
  loop_body~,
  while_else~,
  label~,
  loc~,
) {
  Expr::While(
    loop_cond=env.inner().visit_expr(loop_cond),
    loop_body=env.inner().visit_expr(loop_body),
    while_else=while_else.map(else_block => env.inner().visit_expr(else_block)),
    label~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_function(
  env,
  func~,
  loc~,
) {
  let func = match func {
    Lambda(
      parameters~,
      params_loc~,
      body~,
      return_type~,
      error_type~,
      kind~,
      has_error~,
      is_async~,
      loc~
    ) =>
      Func::Lambda(
        parameters~,
        params_loc~,
        body=env.inner().visit_expr(body),
        return_type~,
        error_type~,
        kind~,
        has_error~,
        is_async~,
        loc~,
      )
    Func::Match(_) => func
  }
  Expr::Function(func~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ident(
  env,
  id~,
  loc~,
) {
  ignore(env)
  Expr::Ident(id~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_if(
  env,
  cond~,
  ifso~,
  ifnot~,
  loc~,
) {
  Expr::If(
    cond=env.inner().visit_expr(cond),
    ifso=env.inner().visit_expr(ifso),
    ifnot=ifnot.map(expr => env.inner().visit_expr(expr)),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_guard(
  env,
  cond~,
  otherwise~,
  body~,
  loc~,
) {
  Expr::Guard(
    cond=env.inner().visit_expr(cond),
    otherwise=otherwise.map(expr => env.inner().visit_expr(expr)),
    body=env.inner().visit_expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_is(
  env,
  expr~,
  pat~,
  loc~,
) {
  Expr::Is(expr=env.inner().visit_expr(expr), pat~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_defer(
  env,
  expr~,
  body~,
  loc~,
) {
  Expr::Defer(
    expr=env.inner().visit_expr(expr),
    body=env.inner().visit_expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_let_fn(
  env,
  name~,
  func~,
  body~,
  loc~,
) {
  Expr::LetFn(
    name~,
    func=match func {
      Lambda(
        parameters~,
        params_loc~,
        body~,
        return_type~,
        error_type~,
        kind~,
        has_error~,
        is_async~,
        loc~
      ) =>
        Func::Lambda(
          parameters~,
          params_loc~,
          body=env.inner().visit_expr(body),
          return_type~,
          error_type~,
          kind~,
          has_error~,
          is_async~,
          loc~,
        )
      Func::Match(_) => func
    },
    body=env.inner().visit_expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_let_rec(
  env,
  bindings~,
  body~,
  loc~,
) {
  Expr::LetRec(
    bindings=bindings.map(binding => match binding {
      (name, func) =>
        (
          name,
          match func {
            Lambda(
              parameters~,
              params_loc~,
              body~,
              return_type~,
              error_type~,
              kind~,
              has_error~,
              is_async~,
              loc~
            ) =>
              Func::Lambda(
                parameters~,
                params_loc~,
                body=env.inner().visit_expr(body),
                return_type~,
                error_type~,
                kind~,
                has_error~,
                is_async~,
                loc~,
              )
            Func::Match(_) => func
          },
        )
    }),
    body=env.inner().visit_expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_let_and(
  env,
  bindings~,
  body~,
  loc~,
) {
  Expr::LetAnd(
    bindings=bindings.map(binding => match binding {
      (binder, ty, func) =>
        (
          binder,
          ty,
          match func {
            Lambda(
              parameters~,
              params_loc~,
              body~,
              return_type~,
              error_type~,
              kind~,
              has_error~,
              is_async~,
              loc~
            ) =>
              Func::Lambda(
                parameters~,
                params_loc~,
                body=env.inner().visit_expr(body),
                return_type~,
                error_type~,
                kind~,
                has_error~,
                is_async~,
                loc~,
              )
            Func::Match(_) => func
          },
        )
    }),
    body=env.inner().visit_expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_let(
  env,
  pattern~,
  expr~,
  body~,
  loc~,
) {
  Expr::Let(
    pattern~,
    expr=env.inner().visit_expr(expr),
    body=env.inner().visit_expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_sequence(
  env,
  exprs~,
  last_expr~,
  loc~,
) {
  Expr::Sequence(
    exprs=exprs.map(expr => env.inner().visit_expr(expr)),
    last_expr=env.inner().visit_expr(last_expr),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_tuple(
  env,
  exprs~,
  loc~,
) {
  Expr::Tuple(exprs=exprs.map(expr => env.inner().visit_expr(expr)), loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_record(
  env,
  type_name~,
  fields~,
  trailing~,
  loc~,
) {
  Expr::Record(
    type_name~,
    fields=fields.map(field => {
      ..field,
      expr: env.inner().visit_expr(field.expr),
    }),
    trailing~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_record_update(
  env,
  type_name~,
  record~,
  fields~,
  loc~,
) {
  Expr::RecordUpdate(
    type_name~,
    record=env.inner().visit_expr(record),
    fields=fields.map(field => {
      ..field,
      expr: env.inner().visit_expr(field.expr),
    }),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_field(
  env,
  record~,
  accessor~,
  loc~,
) {
  Expr::Field(record=env.inner().visit_expr(record), accessor~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_method(
  env,
  type_name~,
  method_name~,
  loc~,
) {
  ignore(env)
  Expr::Method(type_name~, method_name~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_dot_apply(
  env,
  self~,
  method_name~,
  args~,
  return_self~,
  attr~,
  loc~,
) {
  Expr::DotApply(
    self=env.inner().visit_expr(self),
    method_name~,
    args=args.map(arg => Argument::{
      ..arg,
      value: env.inner().visit_expr(arg.value),
    }),
    return_self~,
    attr~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_as(
  env,
  expr~,
  trait_~,
  loc~,
) {
  Expr::As(expr=env.inner().visit_expr(expr), trait_~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_mutate(
  env,
  record~,
  accessor~,
  field~,
  augmented_by~,
  loc~,
) {
  Expr::Mutate(
    record=env.inner().visit_expr(record),
    accessor~,
    field~,
    augmented_by~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_match(
  env,
  expr~,
  cases~,
  match_loc~,
  using_~,
  loc~,
) {
  Expr::Match(
    expr=env.inner().visit_expr(expr),
    cases=cases.map(case => {
      ..case,
      body: env.inner().visit_expr(case.body),
      guard_: case.guard_.map(guard_ => env.inner().visit_expr(guard_)),
    }),
    match_loc~,
    using_~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_let_mut(
  env,
  binder~,
  ty~,
  expr~,
  body~,
  loc~,
) {
  Expr::LetMut(
    binder~,
    ty~,
    expr=env.inner().visit_expr(expr),
    body=env.inner().visit_expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_pipe(
  env,
  lhs~,
  rhs~,
  loc~,
) {
  Expr::Pipe(
    lhs=env.inner().visit_expr(lhs),
    rhs=env.inner().visit_expr(rhs),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_assign(
  env,
  var_~,
  expr~,
  augmented_by~,
  loc~,
) {
  Expr::Assign(var_~, expr=env.inner().visit_expr(expr), augmented_by~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_hole(
  env,
  loc~,
  kind~,
) {
  ignore(env)
  Expr::Hole(loc~, kind~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_return(
  env,
  return_value~,
  loc~,
) {
  Expr::Return(
    return_value=return_value.map(value => env.inner().visit_expr(value)),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_raise(
  env,
  err_value~,
  loc~,
) {
  Expr::Raise(err_value=env.inner().visit_expr(err_value), loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_unit(
  env,
  loc~,
  faked~,
) {
  ignore(env)
  Expr::Unit(loc~, faked~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_break(
  env,
  arg~,
  label~,
  loc~,
) {
  Expr::Break(arg=arg.map(value => env.inner().visit_expr(value)), label~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_continue(
  env,
  args~,
  label~,
  loc~,
) {
  Expr::Continue(
    args=args.map(arg => env.inner().visit_expr(arg)),
    label~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_loop(
  env,
  args~,
  body~,
  label~,
  loop_loc~,
  loc~,
) {
  Expr::Loop(
    args=args.map(arg => env.inner().visit_expr(arg)),
    body=body.map(multi_arg_case => {
      ..multi_arg_case,
      body: env.inner().visit_expr(multi_arg_case.body),
      guard_: multi_arg_case.guard_.map(guard_ => env.inner().visit_expr(guard_)),
    }),
    label~,
    loop_loc~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_for(
  env,
  binders~,
  condition~,
  continue_block~,
  body~,
  for_else~,
  label~,
  loc~,
) {
  Expr::For(
    binders=binders.map(p => (p.0, env.inner().visit_expr(p.1))),
    condition=condition.map(cond => env.inner().visit_expr(cond)),
    continue_block=continue_block.map(p => (p.0, env.inner().visit_expr(p.1))),
    body=env.inner().visit_expr(body),
    for_else=for_else.map(else_block => env.inner().visit_expr(else_block)),
    label~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_for_each(
  env,
  binders~,
  expr~,
  body~,
  else_block~,
  label~,
  loc~,
) {
  Expr::ForEach(
    binders~,
    expr=env.inner().visit_expr(expr),
    body=env.inner().visit_expr(body),
    else_block=else_block.map(else_block => env.inner().visit_expr(else_block)),
    label~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_try(
  env,
  body~,
  catch_~,
  catch_all~,
  try_else~,
  has_try~,
  try_loc~,
  catch_loc~,
  else_loc~,
  loc~,
) {
  Expr::Try(
    body=env.inner().visit_expr(body),
    catch_=catch_.map(catch_case => {
      ..catch_case,
      body: env.inner().visit_expr(catch_case.body),
      guard_: catch_case.guard_.map(guard_ => env.inner().visit_expr(guard_)),
    }),
    catch_all~,
    try_else=try_else.map(else_block => else_block.map(case => {
      ..case,
      body: env.inner().visit_expr(case.body),
      guard_: case.guard_.map(guard_ => env.inner().visit_expr(guard_)),
    })),
    has_try~,
    try_loc~,
    catch_loc~,
    else_loc~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_try_operator(
  env,
  body~,
  kind~,
  try_loc~,
  loc~,
) {
  Expr::TryOperator(body=env.inner().visit_expr(body), kind~, try_loc~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_map(
  env,
  elems~,
  loc~,
) {
  Expr::Map(
    elems=elems.map(elem => { ..elem, expr: env.inner().visit_expr(elem.expr) }),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_group(
  env,
  expr~,
  group~,
  loc~,
) {
  Expr::Group(expr=env.inner().visit_expr(expr), group~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_static_assert(
  env,
  asserts~,
  body~,
) {
  Expr::StaticAssert(asserts~, body=env.inner().visit_expr(body))
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_expr(
  env,
  expr,
) {
  match expr {
    Apply(func~, args~, attr~, loc~) =>
      env.inner().visit_apply(func~, args~, attr~, loc~)
    Infix(op~, lhs~, rhs~, loc~) =>
      env.inner().visit_infix(op~, lhs~, rhs~, loc~)
    Unary(op~, expr~, loc~) => env.inner().visit_unary(op~, expr~, loc~)
    Array(exprs~, loc~) => env.inner().visit_array(exprs~, loc~)
    ArraySpread(elems~, loc~) => env.inner().visit_array_spread(elems~, loc~)
    ArrayGet(array~, index~, loc~) =>
      env.inner().visit_array_get(array~, index~, loc~)
    ArrayGetSlice(array~, start_index~, end_index~, index_loc~, loc~) =>
      env
      .inner()
      .visit_array_get_slice(array~, start_index~, end_index~, index_loc~, loc~)
    ArraySet(array~, index~, value~, loc~) =>
      env.inner().visit_array_set(array~, index~, value~, loc~)
    ArrayAugmentedSet(op~, array~, index~, value~, loc~) =>
      env.inner().visit_array_augmented_set(op~, array~, index~, value~, loc~)
    Constant(c~, loc~) => env.inner().visit_constant(c~, loc~)
    MultilineString(elems~, loc~) =>
      env.inner().visit_multiline_string(elems~, loc~)
    Interp(elems~, loc~) => env.inner().visit_interp(elems~, loc~)
    Constraint(expr~, ty~, loc~) =>
      env.inner().visit_constraint(expr~, ty~, loc~)
    Constr(constr~, loc~) => env.inner().visit_constr(constr~, loc~)
    While(loop_cond~, loop_body~, while_else~, label~, loc~) =>
      env.inner().visit_while(loop_cond~, loop_body~, while_else~, label~, loc~)
    Function(func~, loc~) => env.inner().visit_function(func~, loc~)
    Ident(id~, loc~) => env.inner().visit_ident(id~, loc~)
    If(cond~, ifso~, ifnot~, loc~) =>
      env.inner().visit_if(cond~, ifso~, ifnot~, loc~)
    Guard(cond~, otherwise~, body~, loc~) =>
      env.inner().visit_guard(cond~, otherwise~, body~, loc~)
    Is(expr~, pat~, loc~) => env.inner().visit_is(expr~, pat~, loc~)
    Defer(expr~, body~, loc~) => env.inner().visit_defer(expr~, body~, loc~)
    LetFn(name~, func~, body~, loc~) =>
      env.inner().visit_let_fn(name~, func~, body~, loc~)
    LetRec(bindings~, body~, loc~) =>
      env.inner().visit_let_rec(bindings~, body~, loc~)
    LetAnd(bindings~, body~, loc~) =>
      env.inner().visit_let_and(bindings~, body~, loc~)
    Let(pattern~, expr~, body~, loc~) =>
      env.inner().visit_let(pattern~, expr~, body~, loc~)
    Sequence(exprs~, last_expr~, loc~) =>
      env.inner().visit_sequence(exprs~, last_expr~, loc~)
    Tuple(exprs~, loc~) => env.inner().visit_tuple(exprs~, loc~)
    Record(type_name~, fields~, trailing~, loc~) =>
      env.inner().visit_record(type_name~, fields~, trailing~, loc~)
    RecordUpdate(type_name~, record~, fields~, loc~) =>
      env.inner().visit_record_update(type_name~, record~, fields~, loc~)
    Field(record~, accessor~, loc~) =>
      env.inner().visit_field(record~, accessor~, loc~)
    Method(type_name~, method_name~, loc~) =>
      env.inner().visit_method(type_name~, method_name~, loc~)
    DotApply(self~, method_name~, args~, return_self~, attr~, loc~) =>
      env
      .inner()
      .visit_dot_apply(self~, method_name~, args~, return_self~, attr~, loc~)
    As(expr~, trait_~, loc~) => env.inner().visit_as(expr~, trait_~, loc~)
    Mutate(record~, accessor~, field~, augmented_by~, loc~) =>
      env.inner().visit_mutate(record~, accessor~, field~, augmented_by~, loc~)
    Match(expr~, cases~, match_loc~, using_~, loc~) =>
      env.inner().visit_match(expr~, cases~, match_loc~, using_~, loc~)
    LetMut(binder~, ty~, expr~, body~, loc~) =>
      env.inner().visit_let_mut(binder~, ty~, expr~, body~, loc~)
    Pipe(lhs~, rhs~, loc~) => env.inner().visit_pipe(lhs~, rhs~, loc~)
    Assign(var_~, expr~, augmented_by~, loc~) =>
      env.inner().visit_assign(var_~, expr~, augmented_by~, loc~)
    Hole(loc~, kind~) => env.inner().visit_hole(loc~, kind~)
    Return(return_value~, loc~) => env.inner().visit_return(return_value~, loc~)
    Raise(err_value~, loc~) => env.inner().visit_raise(err_value~, loc~)
    Unit(loc~, faked~) => env.inner().visit_unit(loc~, faked~)
    Break(arg~, label~, loc~) => env.inner().visit_break(arg~, label~, loc~)
    Continue(args~, label~, loc~) =>
      env.inner().visit_continue(args~, label~, loc~)
    Loop(args~, body~, label~, loop_loc~, loc~) =>
      env.inner().visit_loop(args~, body~, label~, loop_loc~, loc~)
    For(binders~, condition~, continue_block~, body~, for_else~, label~, loc~) =>
      env
      .inner()
      .visit_for(
        binders~,
        condition~,
        continue_block~,
        body~,
        for_else~,
        label~,
        loc~,
      )
    ForEach(binders~, expr~, body~, else_block~, label~, loc~) =>
      env
      .inner()
      .visit_for_each(binders~, expr~, body~, else_block~, label~, loc~)
    Try(
      body~,
      catch_~,
      catch_all~,
      try_else~,
      has_try~,
      try_loc~,
      catch_loc~,
      else_loc~,
      loc~
    ) =>
      env
      .inner()
      .visit_try(
        body~,
        catch_~,
        catch_all~,
        try_else~,
        has_try~,
        try_loc~,
        catch_loc~,
        else_loc~,
        loc~,
      )
    TryOperator(body~, kind~, try_loc~, loc~) =>
      env.inner().visit_try_operator(body~, kind~, try_loc~, loc~)
    Map(elems~, loc~) => env.inner().visit_map(elems~, loc~)
    Group(expr~, group~, loc~) => env.inner().visit_group(expr~, group~, loc~)
    StaticAssert(asserts~, body~) =>
      env.inner().visit_static_assert(asserts~, body~)
  }
}
