///|
pub(open) trait ImplMapVisitor {
  base(Self) -> MapVisitorBase[Self] = _
  visit_Impl(Self, Impl) -> Impl = _
  visit_Impl_TopExpr(
    Self,
    expr~ : Expr,
    is_main~ : Bool,
    local_types~ : @list.List[LocalTypeDecl],
    loc~ : Location,
  ) -> Impl = _
  visit_Impl_TopTest(
    Self,
    expr~ : Expr,
    name~ : TestName,
    params~ : Parameters?,
    local_types~ : @list.List[LocalTypeDecl],
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> Impl = _
  visit_Impl_TopTypeDef(Self, TypeDecl) -> Impl = _
  visit_Impl_TopFuncDef(
    Self,
    fun_decl~ : FunDecl,
    decl_body~ : DeclBody,
    loc~ : Location,
  ) -> Impl = _
  visit_Impl_TopFuncAlias(
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
  visit_Impl_TopLetDef(
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
  visit_Impl_TopTrait(Self, TraitDecl) -> Impl = _
  visit_Impl_TopBatchTypeAlias(
    Self,
    pkg~ : Label?,
    targets~ : @list.List[AliasTarget],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    is_list~ : Bool,
    doc~ : DocString,
  ) -> Impl = _
  visit_Impl_TopBatchTraitAlias(
    Self,
    pkg~ : Label?,
    targets~ : @list.List[AliasTarget],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    is_list~ : Bool,
    doc~ : DocString,
  ) -> Impl = _
  visit_Impl_TopImpl(
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
  visit_Impl_TopView(
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
  visit_Impl_TopImplRelation(
    Self,
    self_ty~ : Type,
    trait_~ : TypeName,
    quantifiers~ : @list.List[TypeVarBinder],
    vis~ : Visibility,
    attrs~ : @list.List[Attribute],
    loc~ : Location,
    doc~ : DocString,
  ) -> Impl = _
  visit_Expr(Self, Expr) -> Expr = _
  visit_Expr_Apply(
    Self,
    func~ : Expr,
    args~ : @list.List[Argument],
    attr~ : ApplyAttr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Infix(Self, op~ : Var, lhs~ : Expr, rhs~ : Expr, loc~ : Location) -> Expr = _
  visit_Expr_Unary(Self, op~ : Var, expr~ : Expr, loc~ : Location) -> Expr = _
  visit_Expr_Array(Self, exprs~ : @list.List[Expr], loc~ : Location) -> Expr = _
  visit_Expr_ArraySpread(
    Self,
    elems~ : @list.List[SpreadableElem],
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_ArrayGet(Self, array~ : Expr, index~ : Expr, loc~ : Location) -> Expr = _
  visit_Expr_ArrayGetSlice(
    Self,
    array~ : Expr,
    start_index~ : Expr?,
    end_index~ : Expr?,
    index_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_ArraySet(
    Self,
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_ArrayAugmentedSet(
    Self,
    op~ : Var,
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Constant(Self, c~ : Constant, loc~ : Location) -> Expr = _
  visit_Expr_MultilineString(
    Self,
    elems~ : @list.List[MultilineStringElem],
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Interp(Self, elems~ : @list.List[InterpElem], loc~ : Location) -> Expr = _
  visit_Expr_Constraint(Self, expr~ : Expr, ty~ : Type, loc~ : Location) -> Expr = _
  visit_Expr_Constr(Self, constr~ : Constructor, loc~ : Location) -> Expr = _
  visit_Expr_While(
    Self,
    loop_cond~ : Expr,
    loop_body~ : Expr,
    while_else~ : Expr?,
    label~ : Label?,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Function(Self, func~ : Func, loc~ : Location) -> Expr = _
  visit_Expr_Ident(Self, id~ : Var, loc~ : Location) -> Expr = _
  visit_Expr_If(
    Self,
    cond~ : Expr,
    ifso~ : Expr,
    ifnot~ : Expr?,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Guard(
    Self,
    cond~ : Expr,
    otherwise~ : Expr?,
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Is(Self, expr~ : Expr, pat~ : Pattern, loc~ : Location) -> Expr = _
  visit_Expr_Defer(Self, expr~ : Expr, body~ : Expr, loc~ : Location) -> Expr = _
  visit_Expr_LetFn(
    Self,
    name~ : Binder,
    func~ : Func,
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_LetRec(
    Self,
    bindings~ : @list.List[(Binder, Func)],
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_LetAnd(
    Self,
    bindings~ : @list.List[(Binder, Type?, Func)],
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Let(
    Self,
    pattern~ : Pattern,
    expr~ : Expr,
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Sequence(
    Self,
    exprs~ : @list.List[Expr],
    last_expr~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Tuple(Self, exprs~ : @list.List[Expr], loc~ : Location) -> Expr = _
  visit_Expr_Record(
    Self,
    type_name~ : TypeName?,
    fields~ : @list.List[FieldDef],
    trailing~ : TrailingMark,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_RecordUpdate(
    Self,
    type_name~ : TypeName?,
    record~ : Expr,
    fields~ : @list.List[FieldDef],
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Field(Self, record~ : Expr, accessor~ : Accessor, loc~ : Location) -> Expr = _
  visit_Expr_Method(
    Self,
    type_name~ : TypeName,
    method_name~ : Label,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_DotApply(
    Self,
    self~ : Expr,
    method_name~ : Label,
    args~ : @list.List[Argument],
    return_self~ : Bool,
    attr~ : ApplyAttr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_As(Self, expr~ : Expr, trait_~ : TypeName, loc~ : Location) -> Expr = _
  visit_Expr_Mutate(
    Self,
    record~ : Expr,
    accessor~ : Accessor,
    field~ : Expr,
    augmented_by~ : Var?,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Match(
    Self,
    expr~ : Expr,
    cases~ : @list.List[Case],
    match_loc~ : Location,
    using_~ : Label?,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_LetMut(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    expr~ : Expr,
    body~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Pipe(Self, lhs~ : Expr, rhs~ : Expr, loc~ : Location) -> Expr = _
  visit_Expr_Assign(
    Self,
    var_~ : Var,
    expr~ : Expr,
    augmented_by~ : Var?,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Hole(Self, loc~ : Location, kind~ : Hole) -> Expr = _
  visit_Expr_Return(Self, return_value~ : Expr?, loc~ : Location) -> Expr = _
  visit_Expr_Raise(Self, err_value~ : Expr, loc~ : Location) -> Expr = _
  visit_Expr_Unit(Self, loc~ : Location, faked~ : Bool) -> Expr = _
  visit_Expr_Break(Self, arg~ : Expr?, label~ : Label?, loc~ : Location) -> Expr = _
  visit_Expr_Continue(
    Self,
    args~ : @list.List[Expr],
    label~ : Label?,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Loop(
    Self,
    args~ : @list.List[Expr],
    body~ : @list.List[MultiArgCase],
    label~ : Label?,
    loop_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_For(
    Self,
    binders~ : @list.List[(Binder, Expr)],
    condition~ : Expr?,
    continue_block~ : @list.List[(Binder, Expr)],
    body~ : Expr,
    for_else~ : Expr?,
    label~ : Label?,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_ForEach(
    Self,
    binders~ : @list.List[Binder?],
    expr~ : Expr,
    body~ : Expr,
    else_block~ : Expr?,
    label~ : Label?,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Try(
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
  visit_Expr_TryOperator(
    Self,
    body~ : Expr,
    kind~ : TryOperatorKind,
    try_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  visit_Expr_Map(Self, elems~ : @list.List[MapExprElem], loc~ : Location) -> Expr = _
  visit_Expr_Group(Self, expr~ : Expr, group~ : Group, loc~ : Location) -> Expr = _
  visit_Expr_StaticAssert(
    Self,
    asserts~ : @list.List[StaticAssertion],
    body~ : Expr,
  ) -> Expr = _
}

///|
impl ImplMapVisitor with base(self) -> MapVisitorBase[Self] {
  MapVisitorBase::MapVisitorBase(self)
}

///|
impl ImplMapVisitor with visit_Impl(env, impl_) {
  MapVisitorBase::visit_Impl(env, impl_)
}

///|
impl ImplMapVisitor with visit_Impl_TopExpr(
  env,
  expr~,
  is_main~,
  local_types~,
  loc~,
) {
  MapVisitorBase::visit_Impl_TopExpr(env, expr~, is_main~, local_types~, loc~)
}

///|
impl ImplMapVisitor with visit_Impl_TopTest(
  env,
  expr~,
  name~,
  params~,
  local_types~,
  loc~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_Impl_TopTest(
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
impl ImplMapVisitor with visit_Impl_TopTypeDef(env, type_decl) {
  MapVisitorBase::visit_Impl_TopTypeDef(env, type_decl)
}

///|
impl ImplMapVisitor with visit_Impl_TopFuncDef(env, fun_decl~, decl_body~, loc~) {
  MapVisitorBase::visit_Impl_TopFuncDef(env, fun_decl~, decl_body~, loc~)
}

///|
impl ImplMapVisitor with visit_Impl_TopFuncAlias(
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
  MapVisitorBase::visit_Impl_TopFuncAlias(
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
impl ImplMapVisitor with visit_Impl_TopLetDef(
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
  MapVisitorBase::visit_Impl_TopLetDef(
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
impl ImplMapVisitor with visit_Impl_TopTrait(env, trait_decl) {
  MapVisitorBase::visit_Impl_TopTrait(env, trait_decl)
}

///|
impl ImplMapVisitor with visit_Impl_TopBatchTypeAlias(
  env,
  pkg~,
  targets~,
  vis~,
  loc~,
  attrs~,
  is_list~,
  doc~,
) {
  MapVisitorBase::visit_Impl_TopBatchTypeAlias(
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
impl ImplMapVisitor with visit_Impl_TopBatchTraitAlias(
  env,
  pkg~,
  targets~,
  vis~,
  loc~,
  attrs~,
  is_list~,
  doc~,
) {
  MapVisitorBase::visit_Impl_TopBatchTraitAlias(
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
impl ImplMapVisitor with visit_Impl_TopImpl(
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
  MapVisitorBase::visit_Impl_TopImpl(
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
impl ImplMapVisitor with visit_Impl_TopView(
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
  MapVisitorBase::visit_Impl_TopView(
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
impl ImplMapVisitor with visit_Impl_TopImplRelation(
  env,
  self_ty~,
  trait_~,
  quantifiers~,
  vis~,
  attrs~,
  loc~,
  doc~,
) {
  MapVisitorBase::visit_Impl_TopImplRelation(
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
impl ImplMapVisitor with visit_Expr(env, expr) {
  MapVisitorBase::visit_Expr(env, expr)
}

///|
impl ImplMapVisitor with visit_Expr_Apply(env, func~, args~, attr~, loc~) {
  MapVisitorBase::visit_Expr_Apply(env, func~, args~, attr~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Infix(env, op~, lhs~, rhs~, loc~) {
  MapVisitorBase::visit_Expr_Infix(env, op~, lhs~, rhs~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Unary(env, op~, expr~, loc~) {
  MapVisitorBase::visit_Expr_Unary(env, op~, expr~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Array(env, exprs~, loc~) {
  MapVisitorBase::visit_Expr_Array(env, exprs~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_ArraySpread(env, elems~, loc~) {
  MapVisitorBase::visit_Expr_ArraySpread(env, elems~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_ArrayGet(env, array~, index~, loc~) {
  MapVisitorBase::visit_Expr_ArrayGet(env, array~, index~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_ArrayGetSlice(
  env,
  array~,
  start_index~,
  end_index~,
  index_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_ArrayGetSlice(
    env,
    array~,
    start_index~,
    end_index~,
    index_loc~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_Expr_ArraySet(env, array~, index~, value~, loc~) {
  MapVisitorBase::visit_Expr_ArraySet(env, array~, index~, value~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_ArrayAugmentedSet(
  env,
  op~,
  array~,
  index~,
  value~,
  loc~,
) {
  MapVisitorBase::visit_Expr_ArrayAugmentedSet(
    env,
    op~,
    array~,
    index~,
    value~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_Expr_Constant(env, c~, loc~) {
  MapVisitorBase::visit_Expr_Constant(env, c~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_MultilineString(env, elems~, loc~) {
  MapVisitorBase::visit_Expr_MultilineString(env, elems~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Interp(env, elems~, loc~) {
  MapVisitorBase::visit_Expr_Interp(env, elems~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Constraint(env, expr~, ty~, loc~) {
  MapVisitorBase::visit_Expr_Constraint(env, expr~, ty~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Constr(env, constr~, loc~) {
  MapVisitorBase::visit_Expr_Constr(env, constr~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_While(
  env,
  loop_cond~,
  loop_body~,
  while_else~,
  label~,
  loc~,
) {
  MapVisitorBase::visit_Expr_While(
    env,
    loop_cond~,
    loop_body~,
    while_else~,
    label~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_Expr_Function(env, func~, loc~) {
  MapVisitorBase::visit_Expr_Function(env, func~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Ident(env, id~, loc~) {
  MapVisitorBase::visit_Expr_Ident(env, id~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_If(env, cond~, ifso~, ifnot~, loc~) {
  MapVisitorBase::visit_Expr_If(env, cond~, ifso~, ifnot~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Guard(env, cond~, otherwise~, body~, loc~) {
  MapVisitorBase::visit_Expr_Guard(env, cond~, otherwise~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Is(env, expr~, pat~, loc~) {
  MapVisitorBase::visit_Expr_Is(env, expr~, pat~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Defer(env, expr~, body~, loc~) {
  MapVisitorBase::visit_Expr_Defer(env, expr~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_LetFn(env, name~, func~, body~, loc~) {
  MapVisitorBase::visit_Expr_LetFn(env, name~, func~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_LetRec(env, bindings~, body~, loc~) {
  MapVisitorBase::visit_Expr_LetRec(env, bindings~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_LetAnd(env, bindings~, body~, loc~) {
  MapVisitorBase::visit_Expr_LetAnd(env, bindings~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Let(env, pattern~, expr~, body~, loc~) {
  MapVisitorBase::visit_Expr_Let(env, pattern~, expr~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Sequence(env, exprs~, last_expr~, loc~) {
  MapVisitorBase::visit_Expr_Sequence(env, exprs~, last_expr~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Tuple(env, exprs~, loc~) {
  MapVisitorBase::visit_Expr_Tuple(env, exprs~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Record(
  env,
  type_name~,
  fields~,
  trailing~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Record(env, type_name~, fields~, trailing~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_RecordUpdate(
  env,
  type_name~,
  record~,
  fields~,
  loc~,
) {
  MapVisitorBase::visit_Expr_RecordUpdate(
    env,
    type_name~,
    record~,
    fields~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_Expr_Field(env, record~, accessor~, loc~) {
  MapVisitorBase::visit_Expr_Field(env, record~, accessor~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Method(env, type_name~, method_name~, loc~) {
  MapVisitorBase::visit_Expr_Method(env, type_name~, method_name~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_DotApply(
  env,
  self~,
  method_name~,
  args~,
  return_self~,
  attr~,
  loc~,
) {
  MapVisitorBase::visit_Expr_DotApply(
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
impl ImplMapVisitor with visit_Expr_As(env, expr~, trait_~, loc~) {
  MapVisitorBase::visit_Expr_As(env, expr~, trait_~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Mutate(
  env,
  record~,
  accessor~,
  field~,
  augmented_by~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Mutate(
    env,
    record~,
    accessor~,
    field~,
    augmented_by~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_Expr_Match(
  env,
  expr~,
  cases~,
  match_loc~,
  using_~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Match(
    env,
    expr~,
    cases~,
    match_loc~,
    using_~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_Expr_LetMut(
  env,
  binder~,
  ty~,
  expr~,
  body~,
  loc~,
) {
  MapVisitorBase::visit_Expr_LetMut(env, binder~, ty~, expr~, body~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Pipe(env, lhs~, rhs~, loc~) {
  MapVisitorBase::visit_Expr_Pipe(env, lhs~, rhs~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Assign(
  env,
  var_~,
  expr~,
  augmented_by~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Assign(env, var_~, expr~, augmented_by~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Hole(env, loc~, kind~) {
  MapVisitorBase::visit_Expr_Hole(env, loc~, kind~)
}

///|
impl ImplMapVisitor with visit_Expr_Return(env, return_value~, loc~) {
  MapVisitorBase::visit_Expr_Return(env, return_value~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Raise(env, err_value~, loc~) {
  MapVisitorBase::visit_Expr_Raise(env, err_value~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Unit(env, loc~, faked~) {
  MapVisitorBase::visit_Expr_Unit(env, loc~, faked~)
}

///|
impl ImplMapVisitor with visit_Expr_Break(env, arg~, label~, loc~) {
  MapVisitorBase::visit_Expr_Break(env, arg~, label~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Continue(env, args~, label~, loc~) {
  MapVisitorBase::visit_Expr_Continue(env, args~, label~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Loop(
  env,
  args~,
  body~,
  label~,
  loop_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Loop(env, args~, body~, label~, loop_loc~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_For(
  env,
  binders~,
  condition~,
  continue_block~,
  body~,
  for_else~,
  label~,
  loc~,
) {
  MapVisitorBase::visit_Expr_For(
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
impl ImplMapVisitor with visit_Expr_ForEach(
  env,
  binders~,
  expr~,
  body~,
  else_block~,
  label~,
  loc~,
) {
  MapVisitorBase::visit_Expr_ForEach(
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
impl ImplMapVisitor with visit_Expr_Try(
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
  MapVisitorBase::visit_Expr_Try(
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
impl ImplMapVisitor with visit_Expr_TryOperator(
  env,
  body~,
  kind~,
  try_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_TryOperator(env, body~, kind~, try_loc~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Map(env, elems~, loc~) {
  MapVisitorBase::visit_Expr_Map(env, elems~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_Group(env, expr~, group~, loc~) {
  MapVisitorBase::visit_Expr_Group(env, expr~, group~, loc~)
}

///|
impl ImplMapVisitor with visit_Expr_StaticAssert(env, asserts~, body~) {
  MapVisitorBase::visit_Expr_StaticAssert(env, asserts~, body~)
}

///|
pub(all) struct MapVisitorBase[T](T)

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl(
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
      .visit_Impl_TopView(
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
      .visit_Impl_TopImpl(
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
      .visit_Impl_TopBatchTraitAlias(
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
      .visit_Impl_TopBatchTypeAlias(
        pkg~,
        targets~,
        vis~,
        loc~,
        attrs~,
        is_list~,
        doc~,
      )
    TopTrait(trait_decl) => env.inner().visit_Impl_TopTrait(trait_decl)
    TopLetDef(binder~, ty~, expr~, vis~, is_constant~, loc~, attrs~, doc~) =>
      env
      .inner()
      .visit_Impl_TopLetDef(
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
      .visit_Impl_TopFuncAlias(
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
      env.inner().visit_Impl_TopFuncDef(fun_decl~, decl_body~, loc~)
    TopTypeDef(type_decl) => env.inner().visit_Impl_TopTypeDef(type_decl)
    TopTest(expr~, name~, params~, local_types~, loc~, attrs~, doc~) =>
      env
      .inner()
      .visit_Impl_TopTest(
        expr~,
        name~,
        params~,
        local_types~,
        loc~,
        attrs~,
        doc~,
      )
    TopExpr(expr~, is_main~, local_types~, loc~) =>
      env.inner().visit_Impl_TopExpr(expr~, is_main~, local_types~, loc~)
    TopImplRelation(self_ty~, trait_~, quantifiers~, vis~, attrs~, loc~, doc~) =>
      env
      .inner()
      .visit_Impl_TopImplRelation(
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
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopExpr(
  env,
  expr~,
  is_main~,
  local_types~,
  loc~,
) {
  env
  .inner()
  .visit_Impl_TopExpr(
    expr=env.inner().visit_Expr(expr),
    is_main~,
    local_types~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopTest(
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
  .visit_Impl_TopTest(
    expr=env.inner().visit_Expr(expr),
    name~,
    params~,
    local_types~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopTypeDef(
  env,
  type_decl,
) {
  ignore(env)
  TopTypeDef(type_decl)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopFuncDef(
  env,
  fun_decl~,
  decl_body~,
  loc~,
) {
  TopFuncDef(
    fun_decl~,
    decl_body=match decl_body {
      DeclBody(local_types~, expr~) =>
        DeclBody(local_types~, expr=env.inner().visit_Expr(expr))
      DeclStubs(_) => decl_body
    },
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopFuncAlias(
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
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopLetDef(
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
    expr=env.inner().visit_Expr(expr),
    vis~,
    is_constant~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopTrait(
  env,
  trait_decl,
) {
  ignore(env)
  TopTrait(trait_decl)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopBatchTypeAlias(
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
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopBatchTraitAlias(
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
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopImpl(
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
        DeclBody(local_types~, expr=env.inner().visit_Expr(expr))
      DeclStubs(_) => body
    },
    vis~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopView(
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
    body=env.inner().visit_Expr(body),
    vis~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopImplRelation(
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
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Apply(
  env,
  func~,
  args~,
  attr~,
  loc~,
) {
  Expr::Apply(
    func=env.inner().visit_Expr(func),
    args=args.map(arg => Argument::{
      ..arg,
      value: env.inner().visit_Expr(arg.value),
    }),
    attr~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Infix(
  env,
  op~,
  lhs~,
  rhs~,
  loc~,
) {
  Expr::Infix(
    op~,
    lhs=env.inner().visit_Expr(lhs),
    rhs=env.inner().visit_Expr(rhs),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Unary(
  env,
  op~,
  expr~,
  loc~,
) {
  Expr::Unary(op~, expr=env.inner().visit_Expr(expr), loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Array(
  env,
  exprs~,
  loc~,
) {
  Expr::Array(exprs=exprs.map(expr => env.inner().visit_Expr(expr)), loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_ArraySpread(
  env,
  elems~,
  loc~,
) {
  Expr::ArraySpread(
    elems=elems.map(fn(elem) {
      match elem {
        Regular(expr) => Regular(env.inner().visit_Expr(expr))
        Spread(expr~, loc~) => Spread(expr=env.inner().visit_Expr(expr), loc~)
      }
    }),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_ArrayGet(
  env,
  array~,
  index~,
  loc~,
) {
  Expr::ArrayGet(
    array=env.inner().visit_Expr(array),
    index=env.inner().visit_Expr(index),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_ArrayGetSlice(
  env,
  array~,
  start_index~,
  end_index~,
  index_loc~,
  loc~,
) {
  Expr::ArrayGetSlice(
    array=env.inner().visit_Expr(array),
    start_index=start_index.map(e => env.inner().visit_Expr(e)),
    end_index=end_index.map(e => env.inner().visit_Expr(e)),
    index_loc~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_ArraySet(
  env,
  array~,
  index~,
  value~,
  loc~,
) {
  Expr::ArraySet(
    array=env.inner().visit_Expr(array),
    index=env.inner().visit_Expr(index),
    value=env.inner().visit_Expr(value),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_ArrayAugmentedSet(
  env,
  op~,
  array~,
  index~,
  value~,
  loc~,
) {
  Expr::ArrayAugmentedSet(
    op~,
    array=env.inner().visit_Expr(array),
    index=env.inner().visit_Expr(index),
    value=env.inner().visit_Expr(value),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Constant(
  env,
  c~,
  loc~,
) {
  ignore(env)
  Expr::Constant(c~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_MultilineString(
  env,
  elems~,
  loc~,
) {
  ignore(env)
  Expr::MultilineString(elems~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Interp(
  env,
  elems~,
  loc~,
) {
  ignore(env)
  Expr::Interp(elems~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Constraint(
  env,
  expr~,
  ty~,
  loc~,
) {
  ignore(env)
  Expr::Constraint(expr=env.inner().visit_Expr(expr), ty~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Constr(
  env,
  constr~,
  loc~,
) {
  ignore(env)
  Expr::Constr(constr~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_While(
  env,
  loop_cond~,
  loop_body~,
  while_else~,
  label~,
  loc~,
) {
  Expr::While(
    loop_cond=env.inner().visit_Expr(loop_cond),
    loop_body=env.inner().visit_Expr(loop_body),
    while_else=while_else.map(else_block => env.inner().visit_Expr(else_block)),
    label~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Function(
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
        body=env.inner().visit_Expr(body),
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
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Ident(
  env,
  id~,
  loc~,
) {
  ignore(env)
  Expr::Ident(id~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_If(
  env,
  cond~,
  ifso~,
  ifnot~,
  loc~,
) {
  Expr::If(
    cond=env.inner().visit_Expr(cond),
    ifso=env.inner().visit_Expr(ifso),
    ifnot=ifnot.map(expr => env.inner().visit_Expr(expr)),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Guard(
  env,
  cond~,
  otherwise~,
  body~,
  loc~,
) {
  Expr::Guard(
    cond=env.inner().visit_Expr(cond),
    otherwise=otherwise.map(expr => env.inner().visit_Expr(expr)),
    body=env.inner().visit_Expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Is(
  env,
  expr~,
  pat~,
  loc~,
) {
  Expr::Is(expr=env.inner().visit_Expr(expr), pat~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Defer(
  env,
  expr~,
  body~,
  loc~,
) {
  Expr::Defer(
    expr=env.inner().visit_Expr(expr),
    body=env.inner().visit_Expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_LetFn(
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
          body=env.inner().visit_Expr(body),
          return_type~,
          error_type~,
          kind~,
          has_error~,
          is_async~,
          loc~,
        )
      Func::Match(_) => func
    },
    body=env.inner().visit_Expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_LetRec(
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
                body=env.inner().visit_Expr(body),
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
    body=env.inner().visit_Expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_LetAnd(
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
                body=env.inner().visit_Expr(body),
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
    body=env.inner().visit_Expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Let(
  env,
  pattern~,
  expr~,
  body~,
  loc~,
) {
  Expr::Let(
    pattern~,
    expr=env.inner().visit_Expr(expr),
    body=env.inner().visit_Expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Sequence(
  env,
  exprs~,
  last_expr~,
  loc~,
) {
  Expr::Sequence(
    exprs=exprs.map(expr => env.inner().visit_Expr(expr)),
    last_expr=env.inner().visit_Expr(last_expr),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Tuple(
  env,
  exprs~,
  loc~,
) {
  Expr::Tuple(exprs=exprs.map(expr => env.inner().visit_Expr(expr)), loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Record(
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
      expr: env.inner().visit_Expr(field.expr),
    }),
    trailing~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_RecordUpdate(
  env,
  type_name~,
  record~,
  fields~,
  loc~,
) {
  Expr::RecordUpdate(
    type_name~,
    record=env.inner().visit_Expr(record),
    fields=fields.map(field => {
      ..field,
      expr: env.inner().visit_Expr(field.expr),
    }),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Field(
  env,
  record~,
  accessor~,
  loc~,
) {
  Expr::Field(record=env.inner().visit_Expr(record), accessor~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Method(
  env,
  type_name~,
  method_name~,
  loc~,
) {
  ignore(env)
  Expr::Method(type_name~, method_name~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_DotApply(
  env,
  self~,
  method_name~,
  args~,
  return_self~,
  attr~,
  loc~,
) {
  Expr::DotApply(
    self=env.inner().visit_Expr(self),
    method_name~,
    args=args.map(arg => Argument::{
      ..arg,
      value: env.inner().visit_Expr(arg.value),
    }),
    return_self~,
    attr~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_As(
  env,
  expr~,
  trait_~,
  loc~,
) {
  Expr::As(expr=env.inner().visit_Expr(expr), trait_~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Mutate(
  env,
  record~,
  accessor~,
  field~,
  augmented_by~,
  loc~,
) {
  Expr::Mutate(
    record=env.inner().visit_Expr(record),
    accessor~,
    field~,
    augmented_by~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Match(
  env,
  expr~,
  cases~,
  match_loc~,
  using_~,
  loc~,
) {
  Expr::Match(
    expr=env.inner().visit_Expr(expr),
    cases=cases.map(case => {
      ..case,
      body: env.inner().visit_Expr(case.body),
      guard_: case.guard_.map(guard_ => env.inner().visit_Expr(guard_)),
    }),
    match_loc~,
    using_~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_LetMut(
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
    expr=env.inner().visit_Expr(expr),
    body=env.inner().visit_Expr(body),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Pipe(
  env,
  lhs~,
  rhs~,
  loc~,
) {
  Expr::Pipe(
    lhs=env.inner().visit_Expr(lhs),
    rhs=env.inner().visit_Expr(rhs),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Assign(
  env,
  var_~,
  expr~,
  augmented_by~,
  loc~,
) {
  Expr::Assign(var_~, expr=env.inner().visit_Expr(expr), augmented_by~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Hole(
  env,
  loc~,
  kind~,
) {
  ignore(env)
  Expr::Hole(loc~, kind~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Return(
  env,
  return_value~,
  loc~,
) {
  Expr::Return(
    return_value=return_value.map(value => env.inner().visit_Expr(value)),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Raise(
  env,
  err_value~,
  loc~,
) {
  Expr::Raise(err_value=env.inner().visit_Expr(err_value), loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Unit(
  env,
  loc~,
  faked~,
) {
  ignore(env)
  Expr::Unit(loc~, faked~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Break(
  env,
  arg~,
  label~,
  loc~,
) {
  Expr::Break(arg=arg.map(value => env.inner().visit_Expr(value)), label~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Continue(
  env,
  args~,
  label~,
  loc~,
) {
  Expr::Continue(
    args=args.map(arg => env.inner().visit_Expr(arg)),
    label~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Loop(
  env,
  args~,
  body~,
  label~,
  loop_loc~,
  loc~,
) {
  Expr::Loop(
    args=args.map(arg => env.inner().visit_Expr(arg)),
    body=body.map(multi_arg_case => {
      ..multi_arg_case,
      body: env.inner().visit_Expr(multi_arg_case.body),
      guard_: multi_arg_case.guard_.map(guard_ => env.inner().visit_Expr(guard_)),
    }),
    label~,
    loop_loc~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_For(
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
    binders=binders.map(p => (p.0, env.inner().visit_Expr(p.1))),
    condition=condition.map(cond => env.inner().visit_Expr(cond)),
    continue_block=continue_block.map(p => (p.0, env.inner().visit_Expr(p.1))),
    body=env.inner().visit_Expr(body),
    for_else=for_else.map(else_block => env.inner().visit_Expr(else_block)),
    label~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_ForEach(
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
    expr=env.inner().visit_Expr(expr),
    body=env.inner().visit_Expr(body),
    else_block=else_block.map(else_block => env.inner().visit_Expr(else_block)),
    label~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Try(
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
    body=env.inner().visit_Expr(body),
    catch_=catch_.map(catch_case => {
      ..catch_case,
      body: env.inner().visit_Expr(catch_case.body),
      guard_: catch_case.guard_.map(guard_ => env.inner().visit_Expr(guard_)),
    }),
    catch_all~,
    try_else=try_else.map(else_block => else_block.map(case => {
      ..case,
      body: env.inner().visit_Expr(case.body),
      guard_: case.guard_.map(guard_ => env.inner().visit_Expr(guard_)),
    })),
    has_try~,
    try_loc~,
    catch_loc~,
    else_loc~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_TryOperator(
  env,
  body~,
  kind~,
  try_loc~,
  loc~,
) {
  Expr::TryOperator(body=env.inner().visit_Expr(body), kind~, try_loc~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Map(
  env,
  elems~,
  loc~,
) {
  Expr::Map(
    elems=elems.map(elem => { ..elem, expr: env.inner().visit_Expr(elem.expr) }),
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Group(
  env,
  expr~,
  group~,
  loc~,
) {
  Expr::Group(expr=env.inner().visit_Expr(expr), group~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_StaticAssert(
  env,
  asserts~,
  body~,
) {
  Expr::StaticAssert(asserts~, body=env.inner().visit_Expr(body))
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr(
  env,
  expr,
) {
  match expr {
    Apply(func~, args~, attr~, loc~) =>
      env.inner().visit_Expr_Apply(func~, args~, attr~, loc~)
    Infix(op~, lhs~, rhs~, loc~) =>
      env.inner().visit_Expr_Infix(op~, lhs~, rhs~, loc~)
    Unary(op~, expr~, loc~) => env.inner().visit_Expr_Unary(op~, expr~, loc~)
    Array(exprs~, loc~) => env.inner().visit_Expr_Array(exprs~, loc~)
    ArraySpread(elems~, loc~) =>
      env.inner().visit_Expr_ArraySpread(elems~, loc~)
    ArrayGet(array~, index~, loc~) =>
      env.inner().visit_Expr_ArrayGet(array~, index~, loc~)
    ArrayGetSlice(array~, start_index~, end_index~, index_loc~, loc~) =>
      env
      .inner()
      .visit_Expr_ArrayGetSlice(
        array~,
        start_index~,
        end_index~,
        index_loc~,
        loc~,
      )
    ArraySet(array~, index~, value~, loc~) =>
      env.inner().visit_Expr_ArraySet(array~, index~, value~, loc~)
    ArrayAugmentedSet(op~, array~, index~, value~, loc~) =>
      env
      .inner()
      .visit_Expr_ArrayAugmentedSet(op~, array~, index~, value~, loc~)
    Constant(c~, loc~) => env.inner().visit_Expr_Constant(c~, loc~)
    MultilineString(elems~, loc~) =>
      env.inner().visit_Expr_MultilineString(elems~, loc~)
    Interp(elems~, loc~) => env.inner().visit_Expr_Interp(elems~, loc~)
    Constraint(expr~, ty~, loc~) =>
      env.inner().visit_Expr_Constraint(expr~, ty~, loc~)
    Constr(constr~, loc~) => env.inner().visit_Expr_Constr(constr~, loc~)
    While(loop_cond~, loop_body~, while_else~, label~, loc~) =>
      env
      .inner()
      .visit_Expr_While(loop_cond~, loop_body~, while_else~, label~, loc~)
    Function(func~, loc~) => env.inner().visit_Expr_Function(func~, loc~)
    Ident(id~, loc~) => env.inner().visit_Expr_Ident(id~, loc~)
    If(cond~, ifso~, ifnot~, loc~) =>
      env.inner().visit_Expr_If(cond~, ifso~, ifnot~, loc~)
    Guard(cond~, otherwise~, body~, loc~) =>
      env.inner().visit_Expr_Guard(cond~, otherwise~, body~, loc~)
    Is(expr~, pat~, loc~) => env.inner().visit_Expr_Is(expr~, pat~, loc~)
    Defer(expr~, body~, loc~) =>
      env.inner().visit_Expr_Defer(expr~, body~, loc~)
    LetFn(name~, func~, body~, loc~) =>
      env.inner().visit_Expr_LetFn(name~, func~, body~, loc~)
    LetRec(bindings~, body~, loc~) =>
      env.inner().visit_Expr_LetRec(bindings~, body~, loc~)
    LetAnd(bindings~, body~, loc~) =>
      env.inner().visit_Expr_LetAnd(bindings~, body~, loc~)
    Let(pattern~, expr~, body~, loc~) =>
      env.inner().visit_Expr_Let(pattern~, expr~, body~, loc~)
    Sequence(exprs~, last_expr~, loc~) =>
      env.inner().visit_Expr_Sequence(exprs~, last_expr~, loc~)
    Tuple(exprs~, loc~) => env.inner().visit_Expr_Tuple(exprs~, loc~)
    Record(type_name~, fields~, trailing~, loc~) =>
      env.inner().visit_Expr_Record(type_name~, fields~, trailing~, loc~)
    RecordUpdate(type_name~, record~, fields~, loc~) =>
      env.inner().visit_Expr_RecordUpdate(type_name~, record~, fields~, loc~)
    Field(record~, accessor~, loc~) =>
      env.inner().visit_Expr_Field(record~, accessor~, loc~)
    Method(type_name~, method_name~, loc~) =>
      env.inner().visit_Expr_Method(type_name~, method_name~, loc~)
    DotApply(self~, method_name~, args~, return_self~, attr~, loc~) =>
      env
      .inner()
      .visit_Expr_DotApply(
        self~,
        method_name~,
        args~,
        return_self~,
        attr~,
        loc~,
      )
    As(expr~, trait_~, loc~) => env.inner().visit_Expr_As(expr~, trait_~, loc~)
    Mutate(record~, accessor~, field~, augmented_by~, loc~) =>
      env
      .inner()
      .visit_Expr_Mutate(record~, accessor~, field~, augmented_by~, loc~)
    Match(expr~, cases~, match_loc~, using_~, loc~) =>
      env.inner().visit_Expr_Match(expr~, cases~, match_loc~, using_~, loc~)
    LetMut(binder~, ty~, expr~, body~, loc~) =>
      env.inner().visit_Expr_LetMut(binder~, ty~, expr~, body~, loc~)
    Pipe(lhs~, rhs~, loc~) => env.inner().visit_Expr_Pipe(lhs~, rhs~, loc~)
    Assign(var_~, expr~, augmented_by~, loc~) =>
      env.inner().visit_Expr_Assign(var_~, expr~, augmented_by~, loc~)
    Hole(loc~, kind~) => env.inner().visit_Expr_Hole(loc~, kind~)
    Return(return_value~, loc~) =>
      env.inner().visit_Expr_Return(return_value~, loc~)
    Raise(err_value~, loc~) => env.inner().visit_Expr_Raise(err_value~, loc~)
    Unit(loc~, faked~) => env.inner().visit_Expr_Unit(loc~, faked~)
    Break(arg~, label~, loc~) =>
      env.inner().visit_Expr_Break(arg~, label~, loc~)
    Continue(args~, label~, loc~) =>
      env.inner().visit_Expr_Continue(args~, label~, loc~)
    Loop(args~, body~, label~, loop_loc~, loc~) =>
      env.inner().visit_Expr_Loop(args~, body~, label~, loop_loc~, loc~)
    For(binders~, condition~, continue_block~, body~, for_else~, label~, loc~) =>
      env
      .inner()
      .visit_Expr_For(
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
      .visit_Expr_ForEach(binders~, expr~, body~, else_block~, label~, loc~)
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
      .visit_Expr_Try(
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
      env.inner().visit_Expr_TryOperator(body~, kind~, try_loc~, loc~)
    Map(elems~, loc~) => env.inner().visit_Expr_Map(elems~, loc~)
    Group(expr~, group~, loc~) =>
      env.inner().visit_Expr_Group(expr~, group~, loc~)
    StaticAssert(asserts~, body~) =>
      env.inner().visit_Expr_StaticAssert(asserts~, body~)
  }
}
