///|
pub(open) trait ImplMapVisitor {
  base(Self) -> MapVisitorBase[Self] = _

  // Visibility
  visit_Visibility(Self, Visibility) -> Visibility = _
  visit_Visibility_Pub(Self, attr~ : String?, loc~ : Location) -> Visibility = _
  visit_Visibility_Priv(Self, loc~ : Location) -> Visibility = _

  // Constant
  visit_Constant(Self, Constant) -> Constant = _
  visit_Constant_Bool(Self, Bool) -> Constant = _
  visit_Constant_Byte(Self, ByteLiteral) -> Constant = _
  visit_Constant_Bytes(Self, BytesLiteral) -> Constant = _
  visit_Constant_Char(Self, CharLiteral) -> Constant = _
  visit_Constant_Int(Self, String) -> Constant = _
  visit_Constant_Int64(Self, String) -> Constant = _
  visit_Constant_UInt(Self, String) -> Constant = _
  visit_Constant_UInt64(Self, String) -> Constant = _
  visit_Constant_Float(Self, String) -> Constant = _
  visit_Constant_Double(Self, String) -> Constant = _
  visit_Constant_String(Self, StringLiteral) -> Constant = _
  visit_Constant_BigInt(Self, String) -> Constant = _

  // Label
  visit_Label(Self, name~ : String, loc~ : Location) -> Label = _

  // ConstrName
  visit_ConstrName(Self, name~ : String, loc~ : Location) -> ConstrName = _

  // LongIdent
  visit_LongIdent(Self, LongIdent) -> LongIdent = _
  visit_LongIdent_Ident(Self, name~ : String) -> LongIdent = _
  visit_LongIdent_Dot(Self, pkg~ : String, id~ : String) -> LongIdent = _

  // TypeName
  visit_TypeName(Self, name~ : LongIdent, is_object~ : Bool, loc~ : Location) -> TypeName = _

  // ConstrId
  visit_ConstrId(Self, id~ : LongIdent, loc~ : Location) -> ConstrId = _

  // Type
  visit_Type(Self, Type) -> Type = _
  visit_Type_Any(Self, loc~ : Location) -> Type = _
  visit_Type_Arrow(
    Self,
    args~ : @list.List[Type],
    res~ : Type,
    err~ : ErrorType,
    is_async~ : Bool,
    loc~ : Location,
  ) -> Type = _
  visit_Type_Tuple(Self, tys~ : @list.List[Type], loc~ : Location) -> Type = _
  visit_Type_Name(
    Self,
    constr_id~ : ConstrId,
    tys~ : @list.List[Type],
    loc~ : Location,
  ) -> Type = _
  visit_Type_Option(Self, ty~ : Type, loc~ : Location, question_loc~ : Location) -> Type = _
  visit_Type_Object(Self, ConstrId) -> Type = _

  // ErrorType
  visit_ErrorType(Self, ErrorType) -> ErrorType = _
  visit_ErrorType_ErrorType(Self, ty~ : Type) -> ErrorType = _
  visit_ErrorType_DefaultErrorType(Self, loc~ : Location) -> ErrorType = _
  visit_ErrorType_MaybeError(Self, ty~ : Type) -> ErrorType = _

  // ConstrParam
  visit_ConstrParam(Self, ty~ : Type, mut_~ : Bool, label~ : Label?) -> ConstrParam = _

  // ConstrDecl
  visit_ConstrDecl(
    Self,
    name~ : ConstrName,
    args~ : @list.List[ConstrParam]?,
    tag~ : (String, Location)?,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> ConstrDecl = _

  // ExceptionDecl
  visit_ExceptionDecl(Self, ExceptionDecl) -> ExceptionDecl = _
  visit_ExceptionDecl_SinglePayload(Self, Type) -> ExceptionDecl = _
  visit_ExceptionDecl_EnumPayload(Self, @list.List[ConstrDecl]) -> ExceptionDecl = _

  // FieldName
  visit_FieldName(Self, label~ : String, loc~ : Location) -> FieldName = _

  // FieldDecl
  visit_FieldDecl(
    Self,
    name~ : FieldName,
    ty~ : Type,
    mut_~ : Bool,
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> FieldDecl = _

  // TypeDesc
  visit_TypeDesc(Self, TypeDesc) -> TypeDesc = _
  visit_TypeDesc_Newtype(Self, Type) -> TypeDesc = _
  visit_TypeDesc_Error(Self, ExceptionDecl) -> TypeDesc = _
  visit_TypeDesc_Variant(Self, @list.List[ConstrDecl]) -> TypeDesc = _
  visit_TypeDesc_Record(Self, @list.List[FieldDecl]) -> TypeDesc = _
  visit_TypeDesc_TupleStruct(Self, @list.List[Type]) -> TypeDesc = _
  visit_TypeDesc_Alias(Self, Type) -> TypeDesc = _

  // Hole
  visit_Hole(Self, Hole) -> Hole = _

  // ArgumentKind
  visit_ArgumentKind(Self, ArgumentKind) -> ArgumentKind = _
  visit_ArgumentKind_Labelled(Self, Label) -> ArgumentKind = _
  visit_ArgumentKind_LabelledPun(Self, Label) -> ArgumentKind = _
  visit_ArgumentKind_LabelledOption(
    Self,
    label~ : Label,
    question_loc~ : Location,
  ) -> ArgumentKind = _
  visit_ArgumentKind_LabelledOptionPun(
    Self,
    label~ : Label,
    question_loc~ : Location,
  ) -> ArgumentKind = _

  // FnKind
  visit_FnKind(Self, FnKind) -> FnKind = _

  // Group
  visit_Group(Self, Group) -> Group = _

  // TrailingMark
  visit_TrailingMark(Self, TrailingMark) -> TrailingMark = _

  // ApplyAttr
  visit_ApplyAttr(Self, ApplyAttr) -> ApplyAttr = _

  // TypeVarConstraint
  visit_TypeVarConstraint(Self, trait_~ : LongIdent, loc~ : Location) -> TypeVarConstraint = _

  // TypeVarBinder
  visit_TypeVarBinder(
    Self,
    name~ : String,
    constraints~ : @list.List[TypeVarConstraint],
    loc~ : Location,
  ) -> TypeVarBinder = _

  // TypeDeclBinder
  visit_TypeDeclBinder(Self, name~ : String?, loc~ : Location) -> TypeDeclBinder = _

  // Binder
  visit_Binder(Self, name~ : String, loc~ : Location) -> Binder = _

  // Var
  visit_Var(Self, name~ : LongIdent, loc~ : Location) -> Var = _

  // ConstructorExtraInfo
  visit_ConstructorExtraInfo(Self, ConstructorExtraInfo) -> ConstructorExtraInfo = _
  visit_ConstructorExtraInfo_TypeName(Self, TypeName) -> ConstructorExtraInfo = _
  visit_ConstructorExtraInfo_Package(Self, String) -> ConstructorExtraInfo = _

  // Constructor
  visit_Constructor(
    Self,
    name~ : ConstrName,
    extra_info~ : ConstructorExtraInfo,
    loc~ : Location,
  ) -> Constructor = _

  // Accessor
  visit_Accessor(Self, Accessor) -> Accessor = _
  visit_Accessor_Label(Self, Label) -> Accessor = _
  visit_Accessor_Index(Self, tuple_index~ : Int, loc~ : Location) -> Accessor = _
  visit_Accessor_Newtype(Self, loc~ : Location) -> Accessor = _

  // AliasTarget
  visit_AliasTarget(Self, binder~ : Binder, target~ : Label?) -> AliasTarget = _

  // Argument
  visit_Argument(Self, value~ : Expr, kind~ : ArgumentKind) -> Argument = _

  // Parameter
  visit_Parameter(Self, Parameter) -> Parameter = _
  visit_Parameter_DiscardPositional(Self, ty~ : Type?, loc~ : Location) -> Parameter = _
  visit_Parameter_Positional(Self, binder~ : Binder, ty~ : Type?) -> Parameter = _
  visit_Parameter_Labelled(Self, binder~ : Binder, ty~ : Type?) -> Parameter = _
  visit_Parameter_Optional(Self, binder~ : Binder, default~ : Expr, ty~ : Type?) -> Parameter = _
  visit_Parameter_QuestionOptional(Self, binder~ : Binder, ty~ : Type?) -> Parameter = _

  // Case
  visit_Case(Self, pattern~ : Pattern, guard_~ : Expr?, body~ : Expr) -> Case = _

  // MultiArgCase
  visit_MultiArgCase(
    Self,
    patterns~ : @list.List[Pattern],
    guard_~ : Expr?,
    body~ : Expr,
  ) -> MultiArgCase = _

  // SpreadableElem
  visit_SpreadableElem(Self, SpreadableElem) -> SpreadableElem = _
  visit_SpreadableElem_Regular(Self, Expr) -> SpreadableElem = _
  visit_SpreadableElem_Spread(Self, expr~ : Expr, loc~ : Location) -> SpreadableElem = _

  // MapExprElem
  visit_MapExprElem(
    Self,
    key~ : Constant,
    expr~ : Expr,
    key_loc~ : Location,
    loc~ : Location,
  ) -> MapExprElem = _

  // StaticAssertion
  visit_StaticAssertion(
    Self,
    ty~ : Type,
    trait_~ : LongIdent,
    loc~ : Location,
    msg~ : String,
  ) -> StaticAssertion = _

  // Func
  visit_Func(Self, Func) -> Func = _
  visit_Func_Lambda(
    Self,
    parameters~ : Parameters,
    params_loc~ : Location,
    body~ : Expr,
    return_type~ : Type?,
    error_type~ : ErrorType,
    kind~ : FnKind,
    has_error~ : Location?,
    is_async~ : Bool,
    loc~ : Location,
  ) -> Func = _
  visit_Func_Match(
    Self,
    cases~ : @list.List[MultiArgCase],
    has_error~ : Location?,
    is_async~ : Bool,
    fn_loc~ : Location,
    loc~ : Location,
  ) -> Func = _

  // FieldDef
  visit_FieldDef(
    Self,
    label~ : Label,
    expr~ : Expr,
    is_pun~ : Bool,
    loc~ : Location,
  ) -> FieldDef = _

  // InterpElem
  visit_InterpElem(Self, InterpElem) -> InterpElem = _
  visit_InterpElem_Literal(Self, repr~ : StringLiteral, loc~ : Location) -> InterpElem = _
  visit_InterpElem_Expr(Self, expr~ : Expr, loc~ : Location) -> InterpElem = _
  visit_InterpElem_Source(Self, @tokens.InterpSource) -> InterpElem = _

  // MultilineStringElem
  visit_MultilineStringElem(Self, MultilineStringElem) -> MultilineStringElem = _
  visit_MultilineStringElem_String(Self, String) -> MultilineStringElem = _
  visit_MultilineStringElem_Interp(Self, @list.List[InterpElem]) -> MultilineStringElem = _

  // TryOperatorKind
  visit_TryOperatorKind(Self, TryOperatorKind) -> TryOperatorKind = _

  // DotDotBinder
  visit_DotDotBinder(Self, DotDotBinder) -> DotDotBinder = _
  visit_DotDotBinder_BinderAs(Self, Binder) -> DotDotBinder = _
  visit_DotDotBinder_Binder(Self, Binder) -> DotDotBinder = _

  // ArrayPattern
  visit_ArrayPattern(Self, ArrayPattern) -> ArrayPattern = _
  visit_ArrayPattern_Pattern(Self, Pattern) -> ArrayPattern = _
  visit_ArrayPattern_StringSpread(Self, StringLiteral) -> ArrayPattern = _
  visit_ArrayPattern_BytesSpread(Self, BytesLiteral) -> ArrayPattern = _
  visit_ArrayPattern_ConstSpread(
    Self,
    binder~ : Binder,
    pkg~ : String?,
    loc~ : Location,
  ) -> ArrayPattern = _

  // ArrayPatterns
  visit_ArrayPatterns(Self, ArrayPatterns) -> ArrayPatterns = _
  visit_ArrayPatterns_Closed(Self, @list.List[ArrayPattern]) -> ArrayPatterns = _
  visit_ArrayPatterns_Open(
    Self,
    @list.List[ArrayPattern],
    @list.List[ArrayPattern],
    DotDotBinder,
  ) -> ArrayPatterns = _

  // FieldPat
  visit_FieldPat(
    Self,
    label~ : Label,
    pattern~ : Pattern,
    is_pun~ : Bool,
    loc~ : Location,
  ) -> FieldPat = _

  // ConstrPatArg
  visit_ConstrPatArg(Self, pat~ : Pattern, kind~ : ArgumentKind) -> ConstrPatArg = _

  // MapPatElem
  visit_MapPatElem(
    Self,
    key~ : Constant,
    pat~ : Pattern,
    match_absent~ : Bool,
    key_loc~ : Location,
    loc~ : Location,
  ) -> MapPatElem = _

  // Pattern
  visit_Pattern(Self, Pattern) -> Pattern = _
  visit_Pattern_Alias(Self, pat~ : Pattern, alias_~ : Binder, loc~ : Location) -> Pattern = _
  visit_Pattern_Any(Self, loc~ : Location) -> Pattern = _
  visit_Pattern_Array(Self, pats~ : ArrayPatterns, loc~ : Location) -> Pattern = _
  visit_Pattern_Constant(Self, c~ : Constant, loc~ : Location) -> Pattern = _
  visit_Pattern_Constraint(Self, pat~ : Pattern, ty~ : Type, loc~ : Location) -> Pattern = _
  visit_Pattern_Constr(
    Self,
    constr~ : Constructor,
    args~ : @list.List[ConstrPatArg]?,
    is_open~ : Bool,
    loc~ : Location,
  ) -> Pattern = _
  visit_Pattern_Or(Self, pat1~ : Pattern, pat2~ : Pattern, loc~ : Location) -> Pattern = _
  visit_Pattern_Tuple(Self, pats~ : @list.List[Pattern], loc~ : Location) -> Pattern = _
  visit_Pattern_Var(Self, Binder) -> Pattern = _
  visit_Pattern_Record(
    Self,
    fields~ : @list.List[FieldPat],
    is_closed~ : Bool,
    loc~ : Location,
  ) -> Pattern = _
  visit_Pattern_Map(
    Self,
    elems~ : @list.List[MapPatElem],
    is_closed~ : Bool,
    loc~ : Location,
  ) -> Pattern = _
  visit_Pattern_Range(
    Self,
    lhs~ : Pattern,
    rhs~ : Pattern,
    inclusive~ : Bool,
    loc~ : Location,
  ) -> Pattern = _

  // LocalTypeDecl
  visit_LocalTypeDecl(
    Self,
    tycon~ : String,
    tycon_loc~ : Location,
    components~ : TypeDesc,
    deriving~ : @list.List[DerivingDirective],
  ) -> LocalTypeDecl = _

  // DerivingDirective
  visit_DerivingDirective(
    Self,
    type_name~ : TypeName,
    args~ : @list.List[Argument],
    loc~ : Location,
  ) -> DerivingDirective = _

  // TypeDecl
  visit_TypeDecl(
    Self,
    tycon~ : String,
    tycon_loc~ : Location,
    params~ : @list.List[TypeDeclBinder],
    components~ : TypeDesc,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
    type_vis~ : Visibility,
    deriving~ : @list.List[DerivingDirective],
    loc~ : Location,
  ) -> TypeDecl = _

  // FuncStubs
  visit_FuncStubs(Self, FuncStubs) -> FuncStubs = _
  visit_FuncStubs_Import(
    Self,
    module_name~ : StringLiteral,
    func_name~ : StringLiteral,
  ) -> FuncStubs = _
  visit_FuncStubs_Embedded(
    Self,
    language~ : StringLiteral?,
    code~ : EmbeddedCode,
  ) -> FuncStubs = _

  // EmbeddedCode
  visit_EmbeddedCode(Self, EmbeddedCode) -> EmbeddedCode = _
  visit_EmbeddedCode_CodeString(Self, StringLiteral) -> EmbeddedCode = _
  visit_EmbeddedCode_CodeMultilineString(Self, @list.List[String]) -> EmbeddedCode = _

  // DeclBody
  visit_DeclBody(Self, DeclBody) -> DeclBody = _
  visit_DeclBody_DeclBody(
    Self,
    local_types~ : @list.List[LocalTypeDecl],
    expr~ : Expr,
  ) -> DeclBody = _
  visit_DeclBody_DeclStubs(Self, FuncStubs) -> DeclBody = _

  // FunDecl
  visit_FunDecl(
    Self,
    type_name~ : TypeName?,
    name~ : Binder,
    has_error~ : Location?,
    is_async~ : Bool,
    decl_params~ : Parameters?,
    params_loc~ : Location,
    quantifiers~ : @list.List[TypeVarBinder],
    return_type~ : Type?,
    error_type~ : ErrorType,
    vis~ : Visibility,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> FunDecl = _

  // TraitMethodDecl
  visit_TraitMethodDecl(
    Self,
    name~ : Binder,
    has_error~ : Location?,
    is_async~ : Bool,
    quantifiers~ : @list.List[TypeVarBinder],
    params~ : Parameters,
    return_type~ : Type?,
    error_type~ : ErrorType,
    has_default~ : Location?,
    attrs~ : @list.List[Attribute],
    loc~ : Location,
  ) -> TraitMethodDecl = _

  // TraitDecl
  visit_TraitDecl(
    Self,
    name~ : Binder,
    supers~ : @list.List[TypeVarConstraint],
    methods~ : @list.List[TraitMethodDecl],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> TraitDecl = _

  // Impl
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

  // Expr
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
impl ImplMapVisitor with visit_Visibility(env, visibility) {
  MapVisitorBase::visit_Visibility(env, visibility)
}

///|
impl ImplMapVisitor with visit_Visibility_Pub(env, attr~, loc~) {
  MapVisitorBase::visit_Visibility_Pub(env, attr~, loc~)
}

///|
impl ImplMapVisitor with visit_Visibility_Priv(env, loc~) {
  MapVisitorBase::visit_Visibility_Priv(env, loc~)
}

///|
impl ImplMapVisitor with visit_Constant(env, constant) {
  MapVisitorBase::visit_Constant(env, constant)
}

///|
impl ImplMapVisitor with visit_Constant_Bool(env, bool_val) {
  MapVisitorBase::visit_Constant_Bool(env, bool_val)
}

///|
impl ImplMapVisitor with visit_Constant_Byte(env, byte_literal) {
  MapVisitorBase::visit_Constant_Byte(env, byte_literal)
}

///|
impl ImplMapVisitor with visit_Constant_Bytes(env, bytes_literal) {
  MapVisitorBase::visit_Constant_Bytes(env, bytes_literal)
}

///|
impl ImplMapVisitor with visit_Constant_Char(env, char_literal) {
  MapVisitorBase::visit_Constant_Char(env, char_literal)
}

///|
impl ImplMapVisitor with visit_Constant_Int(env, int_val) {
  MapVisitorBase::visit_Constant_Int(env, int_val)
}

///|
impl ImplMapVisitor with visit_Constant_Int64(env, int64_val) {
  MapVisitorBase::visit_Constant_Int64(env, int64_val)
}

///|
impl ImplMapVisitor with visit_Constant_UInt(env, uint_val) {
  MapVisitorBase::visit_Constant_UInt(env, uint_val)
}

///|
impl ImplMapVisitor with visit_Constant_UInt64(env, uint64_val) {
  MapVisitorBase::visit_Constant_UInt64(env, uint64_val)
}

///|
impl ImplMapVisitor with visit_Constant_Float(env, float_val) {
  MapVisitorBase::visit_Constant_Float(env, float_val)
}

///|
impl ImplMapVisitor with visit_Constant_Double(env, double_val) {
  MapVisitorBase::visit_Constant_Double(env, double_val)
}

///|
impl ImplMapVisitor with visit_Constant_String(env, string_literal) {
  MapVisitorBase::visit_Constant_String(env, string_literal)
}

///|
impl ImplMapVisitor with visit_Constant_BigInt(env, bigint_val) {
  MapVisitorBase::visit_Constant_BigInt(env, bigint_val)
}

///|
impl ImplMapVisitor with visit_Label(env, name~, loc~) {
  MapVisitorBase::visit_Label(env, name~, loc~)
}

///|
impl ImplMapVisitor with visit_ConstrName(env, name~, loc~) {
  MapVisitorBase::visit_ConstrName(env, name~, loc~)
}

///|
impl ImplMapVisitor with visit_LongIdent(env, long_ident) {
  MapVisitorBase::visit_LongIdent(env, long_ident)
}

///|
impl ImplMapVisitor with visit_LongIdent_Ident(env, name~) {
  MapVisitorBase::visit_LongIdent_Ident(env, name~)
}

///|
impl ImplMapVisitor with visit_LongIdent_Dot(env, pkg~, id~) {
  MapVisitorBase::visit_LongIdent_Dot(env, pkg~, id~)
}

///|
impl ImplMapVisitor with visit_TypeName(env, name~, is_object~, loc~) {
  MapVisitorBase::visit_TypeName(env, name~, is_object~, loc~)
}

///|
impl ImplMapVisitor with visit_ConstrId(env, id~, loc~) {
  MapVisitorBase::visit_ConstrId(env, id~, loc~)
}

///|
impl ImplMapVisitor with visit_Type(env, type_) {
  MapVisitorBase::visit_Type(env, type_)
}

///|
impl ImplMapVisitor with visit_Type_Any(env, loc~) {
  MapVisitorBase::visit_Type_Any(env, loc~)
}

///|
impl ImplMapVisitor with visit_Type_Arrow(
  env,
  args~,
  res~,
  err~,
  is_async~,
  loc~,
) {
  MapVisitorBase::visit_Type_Arrow(env, args~, res~, err~, is_async~, loc~)
}

///|
impl ImplMapVisitor with visit_Type_Tuple(env, tys~, loc~) {
  MapVisitorBase::visit_Type_Tuple(env, tys~, loc~)
}

///|
impl ImplMapVisitor with visit_Type_Name(env, constr_id~, tys~, loc~) {
  MapVisitorBase::visit_Type_Name(env, constr_id~, tys~, loc~)
}

///|
impl ImplMapVisitor with visit_Type_Option(env, ty~, loc~, question_loc~) {
  MapVisitorBase::visit_Type_Option(env, ty~, loc~, question_loc~)
}

///|
impl ImplMapVisitor with visit_Type_Object(env, constr_id) {
  MapVisitorBase::visit_Type_Object(env, constr_id)
}

///|
impl ImplMapVisitor with visit_ErrorType(env, error_type) {
  MapVisitorBase::visit_ErrorType(env, error_type)
}

///|
impl ImplMapVisitor with visit_ErrorType_ErrorType(env, ty~) {
  MapVisitorBase::visit_ErrorType_ErrorType(env, ty~)
}

///|
impl ImplMapVisitor with visit_ErrorType_DefaultErrorType(env, loc~) {
  MapVisitorBase::visit_ErrorType_DefaultErrorType(env, loc~)
}

///|
impl ImplMapVisitor with visit_ErrorType_MaybeError(env, ty~) {
  MapVisitorBase::visit_ErrorType_MaybeError(env, ty~)
}

///|
impl ImplMapVisitor with visit_ConstrParam(env, ty~, mut_~, label~) {
  MapVisitorBase::visit_ConstrParam(env, ty~, mut_~, label~)
}

///|
impl ImplMapVisitor with visit_ConstrDecl(
  env,
  name~,
  args~,
  tag~,
  loc~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_ConstrDecl(env, name~, args~, tag~, loc~, attrs~, doc~)
}

///|
impl ImplMapVisitor with visit_ExceptionDecl(env, exception_decl) {
  MapVisitorBase::visit_ExceptionDecl(env, exception_decl)
}

///|
impl ImplMapVisitor with visit_ExceptionDecl_SinglePayload(env, type_) {
  MapVisitorBase::visit_ExceptionDecl_SinglePayload(env, type_)
}

///|
impl ImplMapVisitor with visit_ExceptionDecl_EnumPayload(env, constr_decls) {
  MapVisitorBase::visit_ExceptionDecl_EnumPayload(env, constr_decls)
}

///|
impl ImplMapVisitor with visit_FieldName(env, label~, loc~) {
  MapVisitorBase::visit_FieldName(env, label~, loc~)
}

///|
impl ImplMapVisitor with visit_FieldDecl(
  env,
  name~,
  ty~,
  mut_~,
  vis~,
  loc~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_FieldDecl(
    env,
    name~,
    ty~,
    mut_~,
    vis~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
impl ImplMapVisitor with visit_TypeDesc(env, type_desc) {
  MapVisitorBase::visit_TypeDesc(env, type_desc)
}

///|
impl ImplMapVisitor with visit_TypeDesc_Newtype(env, type_) {
  MapVisitorBase::visit_TypeDesc_Newtype(env, type_)
}

///|
impl ImplMapVisitor with visit_TypeDesc_Error(env, exception_decl) {
  MapVisitorBase::visit_TypeDesc_Error(env, exception_decl)
}

///|
impl ImplMapVisitor with visit_TypeDesc_Variant(env, constr_decls) {
  MapVisitorBase::visit_TypeDesc_Variant(env, constr_decls)
}

///|
impl ImplMapVisitor with visit_TypeDesc_Record(env, field_decls) {
  MapVisitorBase::visit_TypeDesc_Record(env, field_decls)
}

///|
impl ImplMapVisitor with visit_TypeDesc_TupleStruct(env, types) {
  MapVisitorBase::visit_TypeDesc_TupleStruct(env, types)
}

///|
impl ImplMapVisitor with visit_TypeDesc_Alias(env, type_) {
  MapVisitorBase::visit_TypeDesc_Alias(env, type_)
}

///|
impl ImplMapVisitor with visit_Hole(env, hole) {
  MapVisitorBase::visit_Hole(env, hole)
}

///|
impl ImplMapVisitor with visit_ArgumentKind(env, argument_kind) {
  MapVisitorBase::visit_ArgumentKind(env, argument_kind)
}

///|
impl ImplMapVisitor with visit_ArgumentKind_Labelled(env, label) {
  MapVisitorBase::visit_ArgumentKind_Labelled(env, label)
}

///|
impl ImplMapVisitor with visit_ArgumentKind_LabelledPun(env, label) {
  MapVisitorBase::visit_ArgumentKind_LabelledPun(env, label)
}

///|
impl ImplMapVisitor with visit_ArgumentKind_LabelledOption(
  env,
  label~,
  question_loc~,
) {
  MapVisitorBase::visit_ArgumentKind_LabelledOption(env, label~, question_loc~)
}

///|
impl ImplMapVisitor with visit_ArgumentKind_LabelledOptionPun(
  env,
  label~,
  question_loc~,
) {
  MapVisitorBase::visit_ArgumentKind_LabelledOptionPun(
    env,
    label~,
    question_loc~,
  )
}

///|
impl ImplMapVisitor with visit_FnKind(env, fn_kind) {
  MapVisitorBase::visit_FnKind(env, fn_kind)
}

///|
impl ImplMapVisitor with visit_Group(env, group) {
  MapVisitorBase::visit_Group(env, group)
}

///|
impl ImplMapVisitor with visit_TrailingMark(env, trailing_mark) {
  MapVisitorBase::visit_TrailingMark(env, trailing_mark)
}

///|
impl ImplMapVisitor with visit_ApplyAttr(env, apply_attr) {
  MapVisitorBase::visit_ApplyAttr(env, apply_attr)
}

///|
impl ImplMapVisitor with visit_TypeVarConstraint(env, trait_~, loc~) {
  MapVisitorBase::visit_TypeVarConstraint(env, trait_~, loc~)
}

///|
impl ImplMapVisitor with visit_TypeVarBinder(env, name~, constraints~, loc~) {
  MapVisitorBase::visit_TypeVarBinder(env, name~, constraints~, loc~)
}

///|
impl ImplMapVisitor with visit_TypeDeclBinder(env, name~, loc~) {
  MapVisitorBase::visit_TypeDeclBinder(env, name~, loc~)
}

///|
impl ImplMapVisitor with visit_Binder(env, name~, loc~) {
  MapVisitorBase::visit_Binder(env, name~, loc~)
}

///|
impl ImplMapVisitor with visit_Var(env, name~, loc~) {
  MapVisitorBase::visit_Var(env, name~, loc~)
}

///|
impl ImplMapVisitor with visit_ConstructorExtraInfo(env, constructor_extra_info) {
  MapVisitorBase::visit_ConstructorExtraInfo(env, constructor_extra_info)
}

///|
impl ImplMapVisitor with visit_ConstructorExtraInfo_TypeName(env, type_name) {
  MapVisitorBase::visit_ConstructorExtraInfo_TypeName(env, type_name)
}

///|
impl ImplMapVisitor with visit_ConstructorExtraInfo_Package(env, package_) {
  MapVisitorBase::visit_ConstructorExtraInfo_Package(env, package_)
}

///|
impl ImplMapVisitor with visit_Constructor(env, name~, extra_info~, loc~) {
  MapVisitorBase::visit_Constructor(env, name~, extra_info~, loc~)
}

///|
impl ImplMapVisitor with visit_Accessor(env, accessor) {
  MapVisitorBase::visit_Accessor(env, accessor)
}

///|
impl ImplMapVisitor with visit_Accessor_Label(env, label) {
  MapVisitorBase::visit_Accessor_Label(env, label)
}

///|
impl ImplMapVisitor with visit_Accessor_Index(env, tuple_index~, loc~) {
  MapVisitorBase::visit_Accessor_Index(env, tuple_index~, loc~)
}

///|
impl ImplMapVisitor with visit_Accessor_Newtype(env, loc~) {
  MapVisitorBase::visit_Accessor_Newtype(env, loc~)
}

///|
impl ImplMapVisitor with visit_AliasTarget(env, binder~, target~) {
  MapVisitorBase::visit_AliasTarget(env, binder~, target~)
}

///|
impl ImplMapVisitor with visit_Argument(env, value~, kind~) {
  MapVisitorBase::visit_Argument(env, value~, kind~)
}

///|
impl ImplMapVisitor with visit_Parameter(env, parameter) {
  MapVisitorBase::visit_Parameter(env, parameter)
}

///|
impl ImplMapVisitor with visit_Parameter_DiscardPositional(env, ty~, loc~) {
  MapVisitorBase::visit_Parameter_DiscardPositional(env, ty~, loc~)
}

///|
impl ImplMapVisitor with visit_Parameter_Positional(env, binder~, ty~) {
  MapVisitorBase::visit_Parameter_Positional(env, binder~, ty~)
}

///|
impl ImplMapVisitor with visit_Parameter_Labelled(env, binder~, ty~) {
  MapVisitorBase::visit_Parameter_Labelled(env, binder~, ty~)
}

///|
impl ImplMapVisitor with visit_Parameter_Optional(env, binder~, default~, ty~) {
  MapVisitorBase::visit_Parameter_Optional(env, binder~, default~, ty~)
}

///|
impl ImplMapVisitor with visit_Parameter_QuestionOptional(env, binder~, ty~) {
  MapVisitorBase::visit_Parameter_QuestionOptional(env, binder~, ty~)
}

///|
impl ImplMapVisitor with visit_Case(env, pattern~, guard_~, body~) {
  MapVisitorBase::visit_Case(env, pattern~, guard_~, body~)
}

///|
impl ImplMapVisitor with visit_MultiArgCase(env, patterns~, guard_~, body~) {
  MapVisitorBase::visit_MultiArgCase(env, patterns~, guard_~, body~)
}

///|
impl ImplMapVisitor with visit_SpreadableElem(env, spreadable_elem) {
  MapVisitorBase::visit_SpreadableElem(env, spreadable_elem)
}

///|
impl ImplMapVisitor with visit_SpreadableElem_Regular(env, expr) {
  MapVisitorBase::visit_SpreadableElem_Regular(env, expr)
}

///|
impl ImplMapVisitor with visit_SpreadableElem_Spread(env, expr~, loc~) {
  MapVisitorBase::visit_SpreadableElem_Spread(env, expr~, loc~)
}

///|
impl ImplMapVisitor with visit_MapExprElem(env, key~, expr~, key_loc~, loc~) {
  MapVisitorBase::visit_MapExprElem(env, key~, expr~, key_loc~, loc~)
}

///|
impl ImplMapVisitor with visit_StaticAssertion(env, ty~, trait_~, loc~, msg~) {
  MapVisitorBase::visit_StaticAssertion(env, ty~, trait_~, loc~, msg~)
}

///|
impl ImplMapVisitor with visit_Func(env, func) {
  MapVisitorBase::visit_Func(env, func)
}

///|
impl ImplMapVisitor with visit_Func_Lambda(
  env,
  parameters~,
  params_loc~,
  body~,
  return_type~,
  error_type~,
  kind~,
  has_error~,
  is_async~,
  loc~,
) {
  MapVisitorBase::visit_Func_Lambda(
    env,
    parameters~,
    params_loc~,
    body~,
    return_type~,
    error_type~,
    kind~,
    has_error~,
    is_async~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_Func_Match(
  env,
  cases~,
  has_error~,
  is_async~,
  fn_loc~,
  loc~,
) {
  MapVisitorBase::visit_Func_Match(
    env,
    cases~,
    has_error~,
    is_async~,
    fn_loc~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_FieldDef(env, label~, expr~, is_pun~, loc~) {
  MapVisitorBase::visit_FieldDef(env, label~, expr~, is_pun~, loc~)
}

///|
impl ImplMapVisitor with visit_InterpElem(env, interp_elem) {
  MapVisitorBase::visit_InterpElem(env, interp_elem)
}

///|
impl ImplMapVisitor with visit_InterpElem_Literal(env, repr~, loc~) {
  MapVisitorBase::visit_InterpElem_Literal(env, repr~, loc~)
}

///|
impl ImplMapVisitor with visit_InterpElem_Expr(env, expr~, loc~) {
  MapVisitorBase::visit_InterpElem_Expr(env, expr~, loc~)
}

///|
impl ImplMapVisitor with visit_InterpElem_Source(env, interp_source) {
  MapVisitorBase::visit_InterpElem_Source(env, interp_source)
}

///|
impl ImplMapVisitor with visit_MultilineStringElem(env, multiline_string_elem) {
  MapVisitorBase::visit_MultilineStringElem(env, multiline_string_elem)
}

///|
impl ImplMapVisitor with visit_MultilineStringElem_String(env, string_val) {
  MapVisitorBase::visit_MultilineStringElem_String(env, string_val)
}

///|
impl ImplMapVisitor with visit_MultilineStringElem_Interp(env, interp_elems) {
  MapVisitorBase::visit_MultilineStringElem_Interp(env, interp_elems)
}

///|
impl ImplMapVisitor with visit_TryOperatorKind(env, try_operator_kind) {
  MapVisitorBase::visit_TryOperatorKind(env, try_operator_kind)
}

///|
impl ImplMapVisitor with visit_DotDotBinder(env, dot_dot_binder) {
  MapVisitorBase::visit_DotDotBinder(env, dot_dot_binder)
}

///|
impl ImplMapVisitor with visit_DotDotBinder_BinderAs(env, binder) {
  MapVisitorBase::visit_DotDotBinder_BinderAs(env, binder)
}

///|
impl ImplMapVisitor with visit_DotDotBinder_Binder(env, binder) {
  MapVisitorBase::visit_DotDotBinder_Binder(env, binder)
}

///|
impl ImplMapVisitor with visit_ArrayPattern(env, array_pattern) {
  MapVisitorBase::visit_ArrayPattern(env, array_pattern)
}

///|
impl ImplMapVisitor with visit_ArrayPattern_Pattern(env, pattern) {
  MapVisitorBase::visit_ArrayPattern_Pattern(env, pattern)
}

///|
impl ImplMapVisitor with visit_ArrayPattern_StringSpread(env, string_literal) {
  MapVisitorBase::visit_ArrayPattern_StringSpread(env, string_literal)
}

///|
impl ImplMapVisitor with visit_ArrayPattern_BytesSpread(env, bytes_literal) {
  MapVisitorBase::visit_ArrayPattern_BytesSpread(env, bytes_literal)
}

///|
impl ImplMapVisitor with visit_ArrayPattern_ConstSpread(
  env,
  binder~,
  pkg~,
  loc~,
) {
  MapVisitorBase::visit_ArrayPattern_ConstSpread(env, binder~, pkg~, loc~)
}

///|
impl ImplMapVisitor with visit_ArrayPatterns(env, array_patterns) {
  MapVisitorBase::visit_ArrayPatterns(env, array_patterns)
}

///|
impl ImplMapVisitor with visit_ArrayPatterns_Closed(env, array_patterns) {
  MapVisitorBase::visit_ArrayPatterns_Closed(env, array_patterns)
}

///|
impl ImplMapVisitor with visit_ArrayPatterns_Open(
  env,
  prefix_patterns,
  suffix_patterns,
  dot_dot_binder,
) {
  MapVisitorBase::visit_ArrayPatterns_Open(
    env, prefix_patterns, suffix_patterns, dot_dot_binder,
  )
}

///|
impl ImplMapVisitor with visit_FieldPat(env, label~, pattern~, is_pun~, loc~) {
  MapVisitorBase::visit_FieldPat(env, label~, pattern~, is_pun~, loc~)
}

///|
impl ImplMapVisitor with visit_ConstrPatArg(env, pat~, kind~) {
  MapVisitorBase::visit_ConstrPatArg(env, pat~, kind~)
}

///|
impl ImplMapVisitor with visit_MapPatElem(
  env,
  key~,
  pat~,
  match_absent~,
  key_loc~,
  loc~,
) {
  MapVisitorBase::visit_MapPatElem(
    env,
    key~,
    pat~,
    match_absent~,
    key_loc~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_Pattern(env, pattern) {
  MapVisitorBase::visit_Pattern(env, pattern)
}

///|
impl ImplMapVisitor with visit_Pattern_Alias(env, pat~, alias_~, loc~) {
  MapVisitorBase::visit_Pattern_Alias(env, pat~, alias_~, loc~)
}

///|
impl ImplMapVisitor with visit_Pattern_Any(env, loc~) {
  MapVisitorBase::visit_Pattern_Any(env, loc~)
}

///|
impl ImplMapVisitor with visit_Pattern_Array(env, pats~, loc~) {
  MapVisitorBase::visit_Pattern_Array(env, pats~, loc~)
}

///|
impl ImplMapVisitor with visit_Pattern_Constant(env, c~, loc~) {
  MapVisitorBase::visit_Pattern_Constant(env, c~, loc~)
}

///|
impl ImplMapVisitor with visit_Pattern_Constraint(env, pat~, ty~, loc~) {
  MapVisitorBase::visit_Pattern_Constraint(env, pat~, ty~, loc~)
}

///|
impl ImplMapVisitor with visit_Pattern_Constr(
  env,
  constr~,
  args~,
  is_open~,
  loc~,
) {
  MapVisitorBase::visit_Pattern_Constr(env, constr~, args~, is_open~, loc~)
}

///|
impl ImplMapVisitor with visit_Pattern_Or(env, pat1~, pat2~, loc~) {
  MapVisitorBase::visit_Pattern_Or(env, pat1~, pat2~, loc~)
}

///|
impl ImplMapVisitor with visit_Pattern_Tuple(env, pats~, loc~) {
  MapVisitorBase::visit_Pattern_Tuple(env, pats~, loc~)
}

///|
impl ImplMapVisitor with visit_Pattern_Var(env, binder) {
  MapVisitorBase::visit_Pattern_Var(env, binder)
}

///|
impl ImplMapVisitor with visit_Pattern_Record(env, fields~, is_closed~, loc~) {
  MapVisitorBase::visit_Pattern_Record(env, fields~, is_closed~, loc~)
}

///|
impl ImplMapVisitor with visit_Pattern_Map(env, elems~, is_closed~, loc~) {
  MapVisitorBase::visit_Pattern_Map(env, elems~, is_closed~, loc~)
}

///|
impl ImplMapVisitor with visit_Pattern_Range(env, lhs~, rhs~, inclusive~, loc~) {
  MapVisitorBase::visit_Pattern_Range(env, lhs~, rhs~, inclusive~, loc~)
}

///|
impl ImplMapVisitor with visit_LocalTypeDecl(
  env,
  tycon~,
  tycon_loc~,
  components~,
  deriving~,
) {
  MapVisitorBase::visit_LocalTypeDecl(
    env,
    tycon~,
    tycon_loc~,
    components~,
    deriving~,
  )
}

///|
impl ImplMapVisitor with visit_DerivingDirective(env, type_name~, args~, loc~) {
  MapVisitorBase::visit_DerivingDirective(env, type_name~, args~, loc~)
}

///|
impl ImplMapVisitor with visit_TypeDecl(
  env,
  tycon~,
  tycon_loc~,
  params~,
  components~,
  attrs~,
  doc~,
  type_vis~,
  deriving~,
  loc~,
) {
  MapVisitorBase::visit_TypeDecl(
    env,
    tycon~,
    tycon_loc~,
    params~,
    components~,
    attrs~,
    doc~,
    type_vis~,
    deriving~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_FuncStubs(env, func_stubs) {
  MapVisitorBase::visit_FuncStubs(env, func_stubs)
}

///|
impl ImplMapVisitor with visit_FuncStubs_Import(env, module_name~, func_name~) {
  MapVisitorBase::visit_FuncStubs_Import(env, module_name~, func_name~)
}

///|
impl ImplMapVisitor with visit_FuncStubs_Embedded(env, language~, code~) {
  MapVisitorBase::visit_FuncStubs_Embedded(env, language~, code~)
}

///|
impl ImplMapVisitor with visit_EmbeddedCode(env, embedded_code) {
  MapVisitorBase::visit_EmbeddedCode(env, embedded_code)
}

///|
impl ImplMapVisitor with visit_EmbeddedCode_CodeString(env, string_literal) {
  MapVisitorBase::visit_EmbeddedCode_CodeString(env, string_literal)
}

///|
impl ImplMapVisitor with visit_EmbeddedCode_CodeMultilineString(env, strings) {
  MapVisitorBase::visit_EmbeddedCode_CodeMultilineString(env, strings)
}

///|
impl ImplMapVisitor with visit_DeclBody(env, decl_body) {
  MapVisitorBase::visit_DeclBody(env, decl_body)
}

///|
impl ImplMapVisitor with visit_DeclBody_DeclBody(env, local_types~, expr~) {
  MapVisitorBase::visit_DeclBody_DeclBody(env, local_types~, expr~)
}

///|
impl ImplMapVisitor with visit_DeclBody_DeclStubs(env, func_stubs) {
  MapVisitorBase::visit_DeclBody_DeclStubs(env, func_stubs)
}

///|
impl ImplMapVisitor with visit_FunDecl(
  env,
  type_name~,
  name~,
  has_error~,
  is_async~,
  decl_params~,
  params_loc~,
  quantifiers~,
  return_type~,
  error_type~,
  vis~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_FunDecl(
    env,
    type_name~,
    name~,
    has_error~,
    is_async~,
    decl_params~,
    params_loc~,
    quantifiers~,
    return_type~,
    error_type~,
    vis~,
    attrs~,
    doc~,
  )
}

///|
impl ImplMapVisitor with visit_TraitMethodDecl(
  env,
  name~,
  has_error~,
  is_async~,
  quantifiers~,
  params~,
  return_type~,
  error_type~,
  has_default~,
  attrs~,
  loc~,
) {
  MapVisitorBase::visit_TraitMethodDecl(
    env,
    name~,
    has_error~,
    is_async~,
    quantifiers~,
    params~,
    return_type~,
    error_type~,
    has_default~,
    attrs~,
    loc~,
  )
}

///|
impl ImplMapVisitor with visit_TraitDecl(
  env,
  name~,
  supers~,
  methods~,
  vis~,
  loc~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_TraitDecl(
    env,
    name~,
    supers~,
    methods~,
    vis~,
    loc~,
    attrs~,
    doc~,
  )
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
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Visibility(
  env,
  visibility,
) {
  match visibility {
    Default => Default
    Pub(attr~, loc~) => env.inner().visit_Visibility_Pub(attr~, loc~)
    Priv(loc~) => env.inner().visit_Visibility_Priv(loc~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Visibility_Pub(
  _,
  attr~,
  loc~,
) {
  Pub(attr~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Visibility_Priv(
  _,
  loc~,
) {
  Priv(loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant(
  env,
  constant,
) {
  match constant {
    Bool(val) => env.inner().visit_Constant_Bool(val)
    Byte(val) => env.inner().visit_Constant_Byte(val)
    Bytes(val) => env.inner().visit_Constant_Bytes(val)
    Char(val) => env.inner().visit_Constant_Char(val)
    Int(val) => env.inner().visit_Constant_Int(val)
    Int64(val) => env.inner().visit_Constant_Int64(val)
    UInt(val) => env.inner().visit_Constant_UInt(val)
    UInt64(val) => env.inner().visit_Constant_UInt64(val)
    Float(val) => env.inner().visit_Constant_Float(val)
    Double(val) => env.inner().visit_Constant_Double(val)
    String(val) => env.inner().visit_Constant_String(val)
    BigInt(val) => env.inner().visit_Constant_BigInt(val)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_Bool(
  _,
  bool_val,
) {
  Bool(bool_val)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_Byte(
  _,
  byte_literal,
) {
  Byte(byte_literal)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_Bytes(
  _,
  bytes_literal,
) {
  Bytes(bytes_literal)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_Char(
  _,
  char_literal,
) {
  Char(char_literal)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_Int(
  _,
  int_val,
) {
  Int(int_val)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_Int64(
  _,
  int64_val,
) {
  Int64(int64_val)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_UInt(
  _,
  uint_val,
) {
  UInt(uint_val)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_UInt64(
  _,
  uint64_val,
) {
  UInt64(uint64_val)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_Float(
  _,
  float_val,
) {
  Float(float_val)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_Double(
  _,
  double_val,
) {
  Double(double_val)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_String(
  _,
  string_literal,
) {
  String(string_literal)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constant_BigInt(
  _,
  bigint_val,
) {
  BigInt(bigint_val)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Label(
  _,
  name~,
  loc~,
) {
  Label::{ name, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ConstrName(
  _,
  name~,
  loc~,
) {
  ConstrName::{ name, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_LongIdent(
  env,
  long_ident,
) {
  match long_ident {
    Ident(name~) => env.inner().visit_LongIdent_Ident(name~)
    Dot(pkg~, id~) => env.inner().visit_LongIdent_Dot(pkg~, id~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_LongIdent_Ident(
  _,
  name~,
) {
  Ident(name~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_LongIdent_Dot(
  _,
  pkg~,
  id~,
) {
  Dot(pkg~, id~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeName(
  env,
  name~,
  is_object~,
  loc~,
) {
  let name = env.inner().visit_LongIdent(name)
  TypeName::{ name, is_object, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ConstrId(
  env,
  id~,
  loc~,
) {
  let id = env.inner().visit_LongIdent(id)
  ConstrId::{ id, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Type(
  env,
  type_,
) {
  match type_ {
    Any(loc~) => env.inner().visit_Type_Any(loc~)
    Arrow(args~, res~, err~, is_async~, loc~) =>
      env.inner().visit_Type_Arrow(args~, res~, err~, is_async~, loc~)
    Tuple(tys~, loc~) => env.inner().visit_Type_Tuple(tys~, loc~)
    Name(constr_id~, tys~, loc~) =>
      env.inner().visit_Type_Name(constr_id~, tys~, loc~)
    Option(ty~, loc~, question_loc~) =>
      env.inner().visit_Type_Option(ty~, loc~, question_loc~)
    Object(constr_id) => env.inner().visit_Type_Object(constr_id)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Type_Any(
  _,
  loc~,
) {
  Any(loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Type_Arrow(
  env,
  args~,
  res~,
  err~,
  is_async~,
  loc~,
) {
  let args = args.map(env.inner().visit_Type(_))
  let res = env.inner().visit_Type(res)
  let err = env.inner().visit_ErrorType(err)
  Arrow(args~, res~, err~, is_async~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Type_Tuple(
  env,
  tys~,
  loc~,
) {
  let tys = tys.map(env.inner().visit_Type(_))
  Tuple(tys~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Type_Name(
  env,
  constr_id~,
  tys~,
  loc~,
) {
  let constr_id = env.inner().visit_ConstrId(id=constr_id.id, loc=constr_id.loc)
  let tys = tys.map(env.inner().visit_Type(_))
  Name(constr_id~, tys~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Type_Option(
  env,
  ty~,
  loc~,
  question_loc~,
) {
  let ty = env.inner().visit_Type(ty)
  Option(ty~, loc~, question_loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Type_Object(
  env,
  constr_id,
) {
  let constr_id = env.inner().visit_ConstrId(id=constr_id.id, loc=constr_id.loc)
  Object(constr_id)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ErrorType(
  env,
  error_type,
) {
  match error_type {
    ErrorType(ty~) => env.inner().visit_ErrorType_ErrorType(ty~)
    DefaultErrorType(loc~) => env.inner().visit_ErrorType_DefaultErrorType(loc~)
    NoErrorType => NoErrorType
    Noraise(loc~) => Noraise(loc~)
    MaybeError(ty~) => env.inner().visit_ErrorType_MaybeError(ty~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ErrorType_ErrorType(
  env,
  ty~,
) {
  let ty = env.inner().visit_Type(ty)
  ErrorType(ty~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ErrorType_DefaultErrorType(
  _,
  loc~,
) {
  DefaultErrorType(loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ErrorType_MaybeError(
  env,
  ty~,
) {
  let ty = env.inner().visit_Type(ty)
  MaybeError(ty~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ConstrParam(
  env,
  ty~,
  mut_~,
  label~,
) {
  let ty = env.inner().visit_Type(ty)
  let label = label.map(l => env.inner().visit_Label(name=l.name, loc=l.loc))
  ConstrParam::{ ty, mut_, label }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ConstrDecl(
  env,
  name~,
  args~,
  tag~,
  loc~,
  attrs~,
  doc~,
) {
  let name = env.inner().visit_ConstrName(name=name.name, loc=name.loc)
  let args = args.map(args => args.map(arg => env
    .inner()
    .visit_ConstrParam(ty=arg.ty, mut_=arg.mut_, label=arg.label)))
  ConstrDecl::{ name, args, tag, loc, attrs, doc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ExceptionDecl(
  env,
  exception_decl,
) {
  match exception_decl {
    NoPayload => NoPayload
    SinglePayload(type_) => env.inner().visit_ExceptionDecl_SinglePayload(type_)
    EnumPayload(constr_decls) =>
      env.inner().visit_ExceptionDecl_EnumPayload(constr_decls)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ExceptionDecl_SinglePayload(
  env,
  type_,
) {
  let type_ = env.inner().visit_Type(type_)
  SinglePayload(type_)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ExceptionDecl_EnumPayload(
  env,
  constr_decls,
) {
  let constr_decls = constr_decls.map(decl => env
    .inner()
    .visit_ConstrDecl(
      name=decl.name,
      args=decl.args,
      tag=decl.tag,
      loc=decl.loc,
      attrs=decl.attrs,
      doc=decl.doc,
    ))
  EnumPayload(constr_decls)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_FieldName(
  _,
  label~,
  loc~,
) {
  FieldName::{ label, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_FieldDecl(
  env,
  name~,
  ty~,
  mut_~,
  vis~,
  loc~,
  attrs~,
  doc~,
) {
  let name = env.inner().visit_FieldName(label=name.label, loc=name.loc)
  let ty = env.inner().visit_Type(ty)
  let vis = env.inner().visit_Visibility(vis)
  FieldDecl::{ name, ty, mut_, vis, loc, attrs, doc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeDesc(
  env,
  type_desc,
) {
  match type_desc {
    Abstract => Abstract
    Extern => Extern
    Newtype(type_) => env.inner().visit_TypeDesc_Newtype(type_)
    Error(exception_decl) => env.inner().visit_TypeDesc_Error(exception_decl)
    Variant(constr_decls) => env.inner().visit_TypeDesc_Variant(constr_decls)
    Record(field_decls) => env.inner().visit_TypeDesc_Record(field_decls)
    TupleStruct(types) => env.inner().visit_TypeDesc_TupleStruct(types)
    Alias(type_) => env.inner().visit_TypeDesc_Alias(type_)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeDesc_Newtype(
  env,
  type_,
) {
  let type_ = env.inner().visit_Type(type_)
  Newtype(type_)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeDesc_Error(
  env,
  exception_decl,
) {
  let exception_decl = env.inner().visit_ExceptionDecl(exception_decl)
  Error(exception_decl)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeDesc_Variant(
  env,
  constr_decls,
) {
  let constr_decls = constr_decls.map(decl => env
    .inner()
    .visit_ConstrDecl(
      name=decl.name,
      args=decl.args,
      tag=decl.tag,
      loc=decl.loc,
      attrs=decl.attrs,
      doc=decl.doc,
    ))
  Variant(constr_decls)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeDesc_Record(
  env,
  field_decls,
) {
  let field_decls = field_decls.map(decl => env
    .inner()
    .visit_FieldDecl(
      name=decl.name,
      ty=decl.ty,
      mut_=decl.mut_,
      vis=decl.vis,
      loc=decl.loc,
      attrs=decl.attrs,
      doc=decl.doc,
    ))
  Record(field_decls)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeDesc_TupleStruct(
  env,
  types,
) {
  let types = types.map(env.inner().visit_Type(_))
  TupleStruct(types)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeDesc_Alias(
  env,
  type_,
) {
  let type_ = env.inner().visit_Type(type_)
  Alias(type_)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Hole(
  _,
  hole,
) {
  return hole
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArgumentKind(
  env,
  argument_kind,
) {
  match argument_kind {
    Positional => Positional
    Labelled(label) => env.inner().visit_ArgumentKind_Labelled(label)
    LabelledPun(label) => env.inner().visit_ArgumentKind_LabelledPun(label)
    LabelledOption(label~, question_loc~) =>
      env.inner().visit_ArgumentKind_LabelledOption(label~, question_loc~)
    LabelledOptionPun(label~, question_loc~) =>
      env.inner().visit_ArgumentKind_LabelledOptionPun(label~, question_loc~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArgumentKind_Labelled(
  _,
  label,
) {
  Labelled(label)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArgumentKind_LabelledPun(
  _,
  label,
) {
  LabelledPun(label)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArgumentKind_LabelledOption(
  _,
  label~,
  question_loc~,
) {
  LabelledOption(label~, question_loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArgumentKind_LabelledOptionPun(
  _,
  label~,
  question_loc~,
) {
  LabelledOptionPun(label~, question_loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_FnKind(
  _,
  fn_kind,
) {
  fn_kind
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Group(
  _,
  group,
) {
  group
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TrailingMark(
  _,
  trailing_mark,
) {
  trailing_mark
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ApplyAttr(
  _,
  apply_attr,
) {
  apply_attr
}

// Continue with remaining MapVisitorBase implementations that are missing

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeVarConstraint(
  env,
  trait_~,
  loc~,
) {
  let trait_ = env.inner().visit_LongIdent(trait_)
  TypeVarConstraint::{ trait_, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeVarBinder(
  env,
  name~,
  constraints~,
  loc~,
) {
  let constraints = constraints.map(constraint => env
    .inner()
    .visit_TypeVarConstraint(trait_=constraint.trait_, loc=constraint.loc))
  TypeVarBinder::{ name, constraints, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeDeclBinder(
  _,
  name~,
  loc~,
) {
  TypeDeclBinder::{ name, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Binder(
  _,
  name~,
  loc~,
) {
  Binder::{ name, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Var(
  _,
  name~,
  loc~,
) {
  Var::{ name, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ConstructorExtraInfo(
  env,
  constructor_extra_info,
) {
  match constructor_extra_info {
    TypeName(type_name) =>
      env.inner().visit_ConstructorExtraInfo_TypeName(type_name)
    Package(package_) =>
      env.inner().visit_ConstructorExtraInfo_Package(package_)
    NoExtraInfo => NoExtraInfo
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ConstructorExtraInfo_TypeName(
  env,
  type_name,
) {
  let type_name = env
    .inner()
    .visit_TypeName(
      name=type_name.name,
      is_object=type_name.is_object,
      loc=type_name.loc,
    )
  TypeName(type_name)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ConstructorExtraInfo_Package(
  _,
  package_,
) {
  Package(package_)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Constructor(
  env,
  name~,
  extra_info~,
  loc~,
) {
  let name = env.inner().visit_ConstrName(name=name.name, loc=name.loc)
  let extra_info = env.inner().visit_ConstructorExtraInfo(extra_info)
  Constructor::{ name, extra_info, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Accessor(
  env,
  accessor,
) {
  match accessor {
    Label(label) => env.inner().visit_Accessor_Label(label)
    Index(tuple_index~, loc~) =>
      env.inner().visit_Accessor_Index(tuple_index~, loc~)
    Newtype(loc~) => env.inner().visit_Accessor_Newtype(loc~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Accessor_Label(
  env,
  label,
) {
  let label = env.visit_Label(name=label.name, loc=label.loc)
  Label(label)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Accessor_Index(
  _,
  tuple_index~,
  loc~,
) {
  Index(tuple_index~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Accessor_Newtype(
  _,
  loc~,
) {
  Newtype(loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_AliasTarget(
  env,
  binder~,
  target~,
) {
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  AliasTarget::{ binder, target }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Argument(
  env,
  value~,
  kind~,
) {
  let value = env.inner().visit_Expr(value)
  let kind = env.inner().visit_ArgumentKind(kind)
  Argument::{ value, kind }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Parameter(
  env,
  parameter,
) {
  match parameter {
    DiscardPositional(ty~, loc~) =>
      env.inner().visit_Parameter_DiscardPositional(ty~, loc~)
    Positional(binder~, ty~) =>
      env.inner().visit_Parameter_Positional(binder~, ty~)
    Labelled(binder~, ty~) => env.inner().visit_Parameter_Labelled(binder~, ty~)
    Optional(binder~, default~, ty~) =>
      env.inner().visit_Parameter_Optional(binder~, default~, ty~)
    QuestionOptional(binder~, ty~) =>
      env.inner().visit_Parameter_QuestionOptional(binder~, ty~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Parameter_DiscardPositional(
  env,
  ty~,
  loc~,
) {
  let ty = ty.map(env.inner().visit_Type(_))
  DiscardPositional(ty~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Parameter_Positional(
  env,
  binder~,
  ty~,
) {
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  let ty = ty.map(env.inner().visit_Type(_))
  Positional(binder~, ty~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Parameter_Labelled(
  env,
  binder~,
  ty~,
) {
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  Labelled(binder~, ty~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Parameter_Optional(
  env,
  binder~,
  default~,
  ty~,
) {
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  let default = env.inner().visit_Expr(default)
  let ty = ty.map(env.inner().visit_Type(_))
  Optional(binder~, default~, ty~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Parameter_QuestionOptional(
  env,
  binder~,
  ty~,
) {
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  let ty = ty.map(env.inner().visit_Type(_))
  QuestionOptional(binder~, ty~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Case(
  env,
  pattern~,
  guard_~,
  body~,
) {
  let pattern = env.inner().visit_Pattern(pattern)
  let guard_ = guard_.map(env.inner().visit_Expr(_))
  let body = env.inner().visit_Expr(body)
  Case::{ pattern, guard_, body }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_MultiArgCase(
  env,
  patterns~,
  guard_~,
  body~,
) {
  let patterns = patterns.map(env.inner().visit_Pattern(_))
  let guard_ = guard_.map(env.inner().visit_Expr(_))
  let body = env.inner().visit_Expr(body)
  MultiArgCase::{ patterns, guard_, body }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_SpreadableElem(
  env,
  spreadable_elem,
) {
  match spreadable_elem {
    Regular(expr) => env.inner().visit_SpreadableElem_Regular(expr)
    Spread(expr~, loc~) => env.inner().visit_SpreadableElem_Spread(expr~, loc~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_SpreadableElem_Regular(
  env,
  expr,
) {
  let expr = env.inner().visit_Expr(expr)
  Regular(expr)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_SpreadableElem_Spread(
  env,
  expr~,
  loc~,
) {
  let expr = env.inner().visit_Expr(expr)
  Spread(expr~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_MapExprElem(
  env,
  key~,
  expr~,
  key_loc~,
  loc~,
) {
  let key = env.inner().visit_Constant(key)
  let expr = env.inner().visit_Expr(expr)
  MapExprElem::{ key, expr, key_loc, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_StaticAssertion(
  env,
  ty~,
  trait_~,
  loc~,
  msg~,
) {
  let ty = env.inner().visit_Type(ty)
  let trait_ = env.inner().visit_LongIdent(trait_)
  StaticAssertion::{ ty, trait_, loc, msg }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Func(
  env,
  func,
) {
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
      env
      .inner()
      .visit_Func_Lambda(
        parameters~,
        params_loc~,
        body~,
        return_type~,
        error_type~,
        kind~,
        has_error~,
        is_async~,
        loc~,
      )
    Match(cases~, has_error~, is_async~, fn_loc~, loc~) =>
      env.inner().visit_Func_Match(cases~, has_error~, is_async~, fn_loc~, loc~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Func_Lambda(
  env,
  parameters~,
  params_loc~,
  body~,
  return_type~,
  error_type~,
  kind~,
  has_error~,
  is_async~,
  loc~,
) {
  let parameters = parameters.map(env.inner().visit_Parameter(_))
  let body = env.inner().visit_Expr(body)
  let return_type = return_type.map(env.inner().visit_Type(_))
  let error_type = env.inner().visit_ErrorType(error_type)
  let kind = env.inner().visit_FnKind(kind)
  Lambda(
    parameters~,
    params_loc~,
    body~,
    return_type~,
    error_type~,
    kind~,
    has_error~,
    is_async~,
    loc~,
  )
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Func_Match(
  env,
  cases~,
  has_error~,
  is_async~,
  fn_loc~,
  loc~,
) {
  let cases = cases.map(case => env
    .inner()
    .visit_MultiArgCase(
      patterns=case.patterns,
      guard_=case.guard_,
      body=case.body,
    ))
  Match(cases~, has_error~, is_async~, fn_loc~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_FieldDef(
  env,
  label~,
  expr~,
  is_pun~,
  loc~,
) {
  let label = env.inner().visit_Label(name=label.name, loc=label.loc)
  let expr = env.inner().visit_Expr(expr)
  FieldDef::{ label, expr, is_pun, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_InterpElem(
  env,
  interp_elem,
) {
  match interp_elem {
    Literal(repr~, loc~) => env.inner().visit_InterpElem_Literal(repr~, loc~)
    Expr(expr~, loc~) => env.inner().visit_InterpElem_Expr(expr~, loc~)
    Source(interp_source) => env.inner().visit_InterpElem_Source(interp_source)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_InterpElem_Literal(
  _,
  repr~,
  loc~,
) {
  Literal(repr~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_InterpElem_Expr(
  env,
  expr~,
  loc~,
) {
  let expr = env.inner().visit_Expr(expr)
  Expr(expr~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_InterpElem_Source(
  _,
  interp_source,
) {
  Source(interp_source)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_MultilineStringElem(
  env,
  multiline_string_elem,
) {
  match multiline_string_elem {
    String(string_val) =>
      env.inner().visit_MultilineStringElem_String(string_val)
    Interp(interp_elems) =>
      env.inner().visit_MultilineStringElem_Interp(interp_elems)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_MultilineStringElem_String(
  _,
  string_val,
) {
  String(string_val)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_MultilineStringElem_Interp(
  _,
  interp_elems,
) {
  Interp(interp_elems)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TryOperatorKind(
  _,
  try_operator_kind,
) {
  try_operator_kind
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_DotDotBinder(
  env,
  dot_dot_binder,
) {
  match dot_dot_binder {
    Underscore => Underscore
    NoBinder => NoBinder
    BinderAs(binder) => env.inner().visit_DotDotBinder_BinderAs(binder)
    Binder(binder) => env.inner().visit_DotDotBinder_Binder(binder)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_DotDotBinder_BinderAs(
  env,
  binder,
) {
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  BinderAs(binder)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_DotDotBinder_Binder(
  env,
  binder,
) {
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  Binder(binder)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArrayPattern(
  env,
  array_pattern,
) {
  match array_pattern {
    Pattern(pattern) => env.inner().visit_ArrayPattern_Pattern(pattern)
    StringSpread(string_literal) =>
      env.inner().visit_ArrayPattern_StringSpread(string_literal)
    BytesSpread(bytes_literal) =>
      env.inner().visit_ArrayPattern_BytesSpread(bytes_literal)
    ConstSpread(binder~, pkg~, loc~) =>
      env.inner().visit_ArrayPattern_ConstSpread(binder~, pkg~, loc~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArrayPattern_Pattern(
  env,
  pattern,
) {
  let pattern = env.inner().visit_Pattern(pattern)
  Pattern(pattern)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArrayPattern_StringSpread(
  _,
  string_literal,
) {
  StringSpread(string_literal)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArrayPattern_BytesSpread(
  _,
  bytes_literal,
) {
  BytesSpread(bytes_literal)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArrayPattern_ConstSpread(
  env,
  binder~,
  pkg~,
  loc~,
) {
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  ConstSpread(binder~, pkg~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArrayPatterns(
  env,
  array_patterns,
) {
  match array_patterns {
    Closed(patterns) => env.inner().visit_ArrayPatterns_Closed(patterns)
    Open(prefix_patterns, suffix_patterns, dot_dot_binder) =>
      env
      .inner()
      .visit_ArrayPatterns_Open(
        prefix_patterns, suffix_patterns, dot_dot_binder,
      )
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArrayPatterns_Closed(
  env,
  array_patterns,
) {
  let array_patterns = array_patterns.map(env.inner().visit_ArrayPattern(_))
  Closed(array_patterns)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ArrayPatterns_Open(
  env,
  prefix_patterns,
  suffix_patterns,
  dot_dot_binder,
) {
  let prefix_patterns = prefix_patterns.map(env.inner().visit_ArrayPattern(_))
  let suffix_patterns = suffix_patterns.map(env.inner().visit_ArrayPattern(_))
  let dot_dot_binder = env.inner().visit_DotDotBinder(dot_dot_binder)
  Open(prefix_patterns, suffix_patterns, dot_dot_binder)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_FieldPat(
  env,
  label~,
  pattern~,
  is_pun~,
  loc~,
) {
  let label = env.inner().visit_Label(name=label.name, loc=label.loc)
  let pattern = env.inner().visit_Pattern(pattern)
  FieldPat::{ label, pattern, is_pun, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_ConstrPatArg(
  env,
  pat~,
  kind~,
) {
  let pat = env.inner().visit_Pattern(pat)
  let kind = env.inner().visit_ArgumentKind(kind)
  ConstrPatArg::{ pat, kind }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_MapPatElem(
  env,
  key~,
  pat~,
  match_absent~,
  key_loc~,
  loc~,
) {
  let key = env.inner().visit_Constant(key)
  let pat = env.inner().visit_Pattern(pat)
  MapPatElem::{ key, pat, match_absent, key_loc, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern(
  env,
  pattern,
) {
  match pattern {
    Alias(pat~, alias_~, loc~) =>
      env.inner().visit_Pattern_Alias(pat~, alias_~, loc~)
    Any(loc~) => env.inner().visit_Pattern_Any(loc~)
    Array(pats~, loc~) => env.inner().visit_Pattern_Array(pats~, loc~)
    Constant(c~, loc~) => env.inner().visit_Pattern_Constant(c~, loc~)
    Constraint(pat~, ty~, loc~) =>
      env.inner().visit_Pattern_Constraint(pat~, ty~, loc~)
    Constr(constr~, args~, is_open~, loc~) =>
      env.inner().visit_Pattern_Constr(constr~, args~, is_open~, loc~)
    Or(pat1~, pat2~, loc~) => env.inner().visit_Pattern_Or(pat1~, pat2~, loc~)
    Tuple(pats~, loc~) => env.inner().visit_Pattern_Tuple(pats~, loc~)
    Var(binder) => env.inner().visit_Pattern_Var(binder)
    Record(fields~, is_closed~, loc~) =>
      env.inner().visit_Pattern_Record(fields~, is_closed~, loc~)
    Map(elems~, is_closed~, loc~) =>
      env.inner().visit_Pattern_Map(elems~, is_closed~, loc~)
    Range(lhs~, rhs~, inclusive~, loc~) =>
      env.inner().visit_Pattern_Range(lhs~, rhs~, inclusive~, loc~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Alias(
  env,
  pat~,
  alias_~,
  loc~,
) {
  let pat = env.inner().visit_Pattern(pat)
  let alias_ = env.inner().visit_Binder(name=alias_.name, loc=alias_.loc)
  Alias(pat~, alias_~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Any(
  _,
  loc~,
) {
  Any(loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Array(
  env,
  pats~,
  loc~,
) {
  let pats = env.inner().visit_ArrayPatterns(pats)
  Array(pats~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Constant(
  env,
  c~,
  loc~,
) {
  let c = env.inner().visit_Constant(c)
  Constant(c~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Constraint(
  env,
  pat~,
  ty~,
  loc~,
) {
  let pat = env.inner().visit_Pattern(pat)
  let ty = env.inner().visit_Type(ty)
  Constraint(pat~, ty~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Constr(
  env,
  constr~,
  args~,
  is_open~,
  loc~,
) {
  let constr = env
    .inner()
    .visit_Constructor(
      name=constr.name,
      loc=constr.loc,
      extra_info=constr.extra_info,
    )
  let args = args.map(args => args.map(arg => env
    .inner()
    .visit_ConstrPatArg(pat=arg.pat, kind=arg.kind)))
  Constr(constr~, args~, is_open~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Or(
  env,
  pat1~,
  pat2~,
  loc~,
) {
  let pat1 = env.inner().visit_Pattern(pat1)
  let pat2 = env.inner().visit_Pattern(pat2)
  Or(pat1~, pat2~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Tuple(
  env,
  pats~,
  loc~,
) {
  let pats = pats.map(env.inner().visit_Pattern(_))
  Tuple(pats~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Var(
  env,
  binder,
) {
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  Var(binder)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Record(
  env,
  fields~,
  is_closed~,
  loc~,
) {
  let fields = fields.map(field => env
    .inner()
    .visit_FieldPat(
      label=field.label,
      pattern=field.pattern,
      is_pun=field.is_pun,
      loc=field.loc,
    ))
  Record(fields~, is_closed~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Map(
  env,
  elems~,
  is_closed~,
  loc~,
) {
  let elems = elems.map(elem => env
    .inner()
    .visit_MapPatElem(
      key=elem.key,
      pat=elem.pat,
      match_absent=elem.match_absent,
      key_loc=elem.key_loc,
      loc=elem.loc,
    ))
  Map(elems~, is_closed~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Pattern_Range(
  env,
  lhs~,
  rhs~,
  inclusive~,
  loc~,
) {
  let lhs = env.inner().visit_Pattern(lhs)
  let rhs = env.inner().visit_Pattern(rhs)
  Range(lhs~, rhs~, inclusive~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_LocalTypeDecl(
  env,
  tycon~,
  tycon_loc~,
  components~,
  deriving~,
) {
  let components = env.inner().visit_TypeDesc(components)
  let deriving = deriving.map(deriving => env
    .inner()
    .visit_DerivingDirective(
      type_name=deriving.type_name,
      args=deriving.args,
      loc=deriving.loc,
    ))
  LocalTypeDecl::{ tycon, tycon_loc, components, deriving }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_DerivingDirective(
  env,
  type_name~,
  args~,
  loc~,
) {
  let type_name = env
    .inner()
    .visit_TypeName(
      name=type_name.name,
      is_object=type_name.is_object,
      loc=type_name.loc,
    )
  let args = args.map(arg => env
    .inner()
    .visit_Argument(value=arg.value, kind=arg.kind))
  DerivingDirective::{ type_name, args, loc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TypeDecl(
  env,
  tycon~,
  tycon_loc~,
  params~,
  components~,
  attrs~,
  doc~,
  type_vis~,
  deriving~,
  loc~,
) {
  let params = params.map(param => env
    .inner()
    .visit_TypeDeclBinder(name=param.name, loc=param.loc))
  let components = env.inner().visit_TypeDesc(components)
  let deriving = deriving.map(deriving => env
    .inner()
    .visit_DerivingDirective(
      type_name=deriving.type_name,
      args=deriving.args,
      loc=deriving.loc,
    ))
  let type_vis = env.inner().visit_Visibility(type_vis)
  TypeDecl::{
    tycon,
    tycon_loc,
    params,
    components,
    attrs,
    doc,
    type_vis,
    deriving,
    loc,
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_FuncStubs(
  env,
  func_stubs,
) {
  match func_stubs {
    Import(module_name~, func_name~) =>
      env.inner().visit_FuncStubs_Import(module_name~, func_name~)
    Embedded(language~, code~) =>
      env.inner().visit_FuncStubs_Embedded(language~, code~)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_FuncStubs_Import(
  _,
  module_name~,
  func_name~,
) {
  Import(module_name~, func_name~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_FuncStubs_Embedded(
  env,
  language~,
  code~,
) {
  let code = env.inner().visit_EmbeddedCode(code)
  Embedded(language~, code~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_EmbeddedCode(
  env,
  embedded_code,
) {
  match embedded_code {
    CodeString(string_literal) =>
      env.inner().visit_EmbeddedCode_CodeString(string_literal)
    CodeMultilineString(strings) =>
      env.inner().visit_EmbeddedCode_CodeMultilineString(strings)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_EmbeddedCode_CodeString(
  _,
  string_literal,
) {
  CodeString(string_literal)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_EmbeddedCode_CodeMultilineString(
  _,
  strings,
) {
  CodeMultilineString(strings)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_DeclBody(
  env,
  decl_body,
) {
  match decl_body {
    DeclBody(local_types~, expr~) =>
      env.inner().visit_DeclBody_DeclBody(local_types~, expr~)
    DeclStubs(func_stubs) => env.inner().visit_DeclBody_DeclStubs(func_stubs)
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_DeclBody_DeclBody(
  env,
  local_types~,
  expr~,
) {
  let local_types = local_types.map(local_type => env
    .inner()
    .visit_LocalTypeDecl(
      tycon=local_type.tycon,
      tycon_loc=local_type.tycon_loc,
      components=local_type.components,
      deriving=local_type.deriving,
    ))
  let expr = env.inner().visit_Expr(expr)
  DeclBody(local_types~, expr~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_DeclBody_DeclStubs(
  env,
  func_stubs,
) {
  let func_stubs = env.inner().visit_FuncStubs(func_stubs)
  DeclStubs(func_stubs)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_FunDecl(
  env,
  type_name~,
  name~,
  has_error~,
  is_async~,
  decl_params~,
  params_loc~,
  quantifiers~,
  return_type~,
  error_type~,
  vis~,
  attrs~,
  doc~,
) {
  let type_name = type_name.map(type_name => env
    .inner()
    .visit_TypeName(
      name=type_name.name,
      is_object=type_name.is_object,
      loc=type_name.loc,
    ))
  let name = env.inner().visit_Binder(name=name.name, loc=name.loc)
  let decl_params = decl_params.map(decl_params => decl_params.map(param => env
    .inner()
    .visit_Parameter(param)))
  let quantifiers = quantifiers.map(quantifier => env
    .inner()
    .visit_TypeVarBinder(
      name=quantifier.name,
      constraints=quantifier.constraints,
      loc=quantifier.loc,
    ))
  let return_type = return_type.map(return_type => env
    .inner()
    .visit_Type(return_type))
  let error_type = env.inner().visit_ErrorType(error_type)
  let vis = env.inner().visit_Visibility(vis)
  FunDecl::{
    type_name,
    name,
    has_error,
    is_async,
    decl_params,
    params_loc,
    quantifiers,
    return_type,
    error_type,
    vis,
    attrs,
    doc,
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TraitMethodDecl(
  env,
  name~,
  has_error~,
  is_async~,
  quantifiers~,
  params~,
  return_type~,
  error_type~,
  has_default~,
  attrs~,
  loc~,
) {
  let name = env.inner().visit_Binder(name=name.name, loc=name.loc)
  let quantifiers = quantifiers.map(quantifier => env
    .inner()
    .visit_TypeVarBinder(
      name=quantifier.name,
      constraints=quantifier.constraints,
      loc=quantifier.loc,
    ))
  let params = params.map(param => env.inner().visit_Parameter(param))
  let return_type = return_type.map(return_type => env
    .inner()
    .visit_Type(return_type))
  let error_type = env.inner().visit_ErrorType(error_type)
  TraitMethodDecl::{
    name,
    has_error,
    is_async,
    quantifiers,
    params,
    return_type,
    error_type,
    has_default,
    attrs,
    loc,
  }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_TraitDecl(
  env,
  name~,
  supers~,
  methods~,
  vis~,
  loc~,
  attrs~,
  doc~,
) {
  let name = env.inner().visit_Binder(name=name.name, loc=name.loc)
  let supers = supers.map(super_ => env
    .inner()
    .visit_TypeVarConstraint(trait_=super_.trait_, loc=super_.loc))
  let methods = methods.map(method_ => env
    .inner()
    .visit_TraitMethodDecl(
      name=method_.name,
      has_error=method_.has_error,
      is_async=method_.is_async,
      quantifiers=method_.quantifiers,
      params=method_.params,
      return_type=method_.return_type,
      error_type=method_.error_type,
      has_default=method_.has_default,
      attrs=method_.attrs,
      loc=method_.loc,
    ))
  let vis = env.inner().visit_Visibility(vis)
  TraitDecl::{ name, supers, methods, vis, loc, attrs, doc }
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopExpr(
  env,
  expr~,
  is_main~,
  local_types~,
  loc~,
) {
  let expr = env.inner().visit_Expr(expr)
  let local_types = local_types.map(local_type => env
    .inner()
    .visit_LocalTypeDecl(
      tycon=local_type.tycon,
      tycon_loc=local_type.tycon_loc,
      components=local_type.components,
      deriving=local_type.deriving,
    ))
  TopExpr(expr~, is_main~, local_types~, loc~)
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
  let expr = env.inner().visit_Expr(expr)
  let params = params.map(params => params.map(param => env
    .inner()
    .visit_Parameter(param)))
  let local_types = local_types.map(local_type => env
    .inner()
    .visit_LocalTypeDecl(
      tycon=local_type.tycon,
      tycon_loc=local_type.tycon_loc,
      components=local_type.components,
      deriving=local_type.deriving,
    ))
  TopTest(expr~, name~, params~, local_types~, loc~, attrs~, doc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopTypeDef(
  env,
  type_decl,
) {
  let type_decl = env
    .inner()
    .visit_TypeDecl(
      tycon=type_decl.tycon,
      tycon_loc=type_decl.tycon_loc,
      params=type_decl.params,
      components=type_decl.components,
      attrs=type_decl.attrs,
      doc=type_decl.doc,
      type_vis=type_decl.type_vis,
      deriving=type_decl.deriving,
      loc=type_decl.loc,
    )
  TopTypeDef(type_decl)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopFuncDef(
  env,
  fun_decl~,
  decl_body~,
  loc~,
) {
  let fun_decl = env
    .inner()
    .visit_FunDecl(
      type_name=fun_decl.type_name,
      name=fun_decl.name,
      has_error=fun_decl.has_error,
      is_async=fun_decl.is_async,
      decl_params=fun_decl.decl_params,
      params_loc=fun_decl.params_loc,
      quantifiers=fun_decl.quantifiers,
      return_type=fun_decl.return_type,
      error_type=fun_decl.error_type,
      vis=fun_decl.vis,
      attrs=fun_decl.attrs,
      doc=fun_decl.doc,
    )
  let decl_body = env.inner().visit_DeclBody(decl_body)
  TopFuncDef(fun_decl~, decl_body~, loc~)
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
  let pkg = pkg.map(pkg => env.inner().visit_Label(name=pkg.name, loc=pkg.loc))
  let type_name = type_name.map(type_name => env
    .inner()
    .visit_Label(name=type_name.name, loc=type_name.loc))
  let targets = targets.map(target => env
    .inner()
    .visit_AliasTarget(binder=target.binder, target=target.target))
  let vis = env.inner().visit_Visibility(vis)
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
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  let ty = ty.map(env.inner().visit_Type(_))
  let expr = env.inner().visit_Expr(expr)
  let vis = env.inner().visit_Visibility(vis)
  TopLetDef(binder~, ty~, expr~, vis~, is_constant~, loc~, attrs~, doc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Impl_TopTrait(
  env,
  trait_decl,
) {
  let trait_decl = env
    .inner()
    .visit_TraitDecl(
      name=trait_decl.name,
      supers=trait_decl.supers,
      methods=trait_decl.methods,
      vis=trait_decl.vis,
      loc=trait_decl.loc,
      attrs=trait_decl.attrs,
      doc=trait_decl.doc,
    )
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
  let pkg = pkg.map(pkg => env.inner().visit_Label(name=pkg.name, loc=pkg.loc))
  let targets = targets.map(target => env
    .inner()
    .visit_AliasTarget(binder=target.binder, target=target.target))
  let vis = env.inner().visit_Visibility(vis)
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
  let pkg = pkg.map(pkg => env.inner().visit_Label(name=pkg.name, loc=pkg.loc))
  let targets = targets.map(target => env
    .inner()
    .visit_AliasTarget(binder=target.binder, target=target.target))
  let vis = env.inner().visit_Visibility(vis)
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
  let self_ty = self_ty.map(env.inner().visit_Type(_))
  let trait_ = env
    .inner()
    .visit_TypeName(
      name=trait_.name,
      is_object=trait_.is_object,
      loc=trait_.loc,
    )
  let method_name = env
    .inner()
    .visit_Binder(name=method_name.name, loc=method_name.loc)
  let quantifiers = quantifiers.map(quantifier => env
    .inner()
    .visit_TypeVarBinder(
      name=quantifier.name,
      constraints=quantifier.constraints,
      loc=quantifier.loc,
    ))
  let params = params.map(param => env.inner().visit_Parameter(param))
  let ret_ty = ret_ty.map(env.inner().visit_Type(_))
  let err_ty = env.inner().visit_ErrorType(err_ty)
  let body = env.inner().visit_DeclBody(body)
  let vis = env.inner().visit_Visibility(vis)
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
  let quantifiers = quantifiers.map(quantifier => env
    .inner()
    .visit_TypeVarBinder(
      name=quantifier.name,
      constraints=quantifier.constraints,
      loc=quantifier.loc,
    ))
  let source_ty = env.inner().visit_Type(source_ty)
  let view_constrs = view_constrs.map(constr => env
    .inner()
    .visit_ConstrDecl(
      name=constr.name,
      args=constr.args,
      tag=constr.tag,
      attrs=constr.attrs,
      loc=constr.loc,
      doc=constr.doc,
    ))
  let view_func_name = env
    .inner()
    .visit_Binder(name=view_func_name.name, loc=view_func_name.loc)
  let parameters = parameters.map(param => env.inner().visit_Parameter(param))
  let body = env.inner().visit_Expr(body)
  let vis = env.inner().visit_Visibility(vis)
  TopView(
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
  let self_ty = env.inner().visit_Type(self_ty)
  let trait_ = env
    .inner()
    .visit_TypeName(
      name=trait_.name,
      is_object=trait_.is_object,
      loc=trait_.loc,
    )
  let quantifiers = quantifiers.map(quantifier => env
    .inner()
    .visit_TypeVarBinder(
      name=quantifier.name,
      constraints=quantifier.constraints,
      loc=quantifier.loc,
    ))
  let vis = env.inner().visit_Visibility(vis)
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
  let func = env.inner().visit_Expr(func)
  let args = args.map(arg => env
    .inner()
    .visit_Argument(value=arg.value, kind=arg.kind))
  Expr::Apply(func~, args~, attr~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Infix(
  env,
  op~,
  lhs~,
  rhs~,
  loc~,
) {
  let op = env.inner().visit_Var(name=op.name, loc=op.loc)
  let lhs = env.inner().visit_Expr(lhs)
  let rhs = env.inner().visit_Expr(rhs)
  Expr::Infix(op~, lhs~, rhs~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Unary(
  env,
  op~,
  expr~,
  loc~,
) {
  let op = env.inner().visit_Var(name=op.name, loc=op.loc)
  let expr = env.inner().visit_Expr(expr)
  Expr::Unary(op~, expr~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Array(
  env,
  exprs~,
  loc~,
) {
  let exprs = exprs.map(expr => env.inner().visit_Expr(expr))
  Expr::Array(exprs~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_ArraySpread(
  env,
  elems~,
  loc~,
) {
  let elems = elems.map(fn(elem) {
    match elem {
      Regular(expr) => Regular(env.inner().visit_Expr(expr))
      Spread(expr~, loc~) => Spread(expr=env.inner().visit_Expr(expr), loc~)
    }
  })
  Expr::ArraySpread(elems~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_ArrayGet(
  env,
  array~,
  index~,
  loc~,
) {
  let array = env.inner().visit_Expr(array)
  let index = env.inner().visit_Expr(index)
  Expr::ArrayGet(array~, index~, loc~)
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
  let array = env.inner().visit_Expr(array)
  let start_index = start_index.map(e => env.inner().visit_Expr(e))
  let end_index = end_index.map(e => env.inner().visit_Expr(e))
  Expr::ArrayGetSlice(array~, start_index~, end_index~, index_loc~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_ArraySet(
  env,
  array~,
  index~,
  value~,
  loc~,
) {
  let index = env.inner().visit_Expr(index)
  let value = env.inner().visit_Expr(value)
  let array = env.inner().visit_Expr(array)
  Expr::ArraySet(array~, index~, value~, loc~)
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
  let op = env.inner().visit_Var(name=op.name, loc=op.loc)
  let array = env.inner().visit_Expr(array)
  let index = env.inner().visit_Expr(index)
  let value = env.inner().visit_Expr(value)
  Expr::ArrayAugmentedSet(op~, array~, index~, value~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Constant(
  env,
  c~,
  loc~,
) {
  let c = env.inner().visit_Constant(c)
  Expr::Constant(c~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_MultilineString(
  env,
  elems~,
  loc~,
) {
  let elems = elems.map(elem => env.inner().visit_MultilineStringElem(elem))
  Expr::MultilineString(elems~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Interp(
  env,
  elems~,
  loc~,
) {
  let elems = elems.map(elem => env.inner().visit_InterpElem(elem))
  Expr::Interp(elems~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Constraint(
  env,
  expr~,
  ty~,
  loc~,
) {
  let ty = env.inner().visit_Type(ty)
  let expr = env.inner().visit_Expr(expr)
  Expr::Constraint(expr~, ty~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Constr(
  env,
  constr~,
  loc~,
) {
  let constr = env
    .inner()
    .visit_Constructor(
      name=constr.name,
      loc=constr.loc,
      extra_info=constr.extra_info,
    )
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
  let loop_cond = env.inner().visit_Expr(loop_cond)
  let loop_body = env.inner().visit_Expr(loop_body)
  let while_else = while_else.map(else_block => env
    .inner()
    .visit_Expr(else_block))
  let label = label.map(label => env
    .inner()
    .visit_Label(name=label.name, loc=label.loc))
  Expr::While(loop_cond~, loop_body~, while_else~, label~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Function(
  env,
  func~,
  loc~,
) {
  let func = env.inner().visit_Func(func)
  Expr::Function(func~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Ident(
  env,
  id~,
  loc~,
) {
  let id = env.inner().visit_Var(name=id.name, loc=id.loc)
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
  let cond = env.inner().visit_Expr(cond)
  let ifso = env.inner().visit_Expr(ifso)
  let ifnot = ifnot.map(expr => env.inner().visit_Expr(expr))
  Expr::If(cond~, ifso~, ifnot~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Guard(
  env,
  cond~,
  otherwise~,
  body~,
  loc~,
) {
  let cond = env.inner().visit_Expr(cond)
  let body = env.inner().visit_Expr(body)
  let otherwise = otherwise.map(expr => env.inner().visit_Expr(expr))
  Expr::Guard(cond~, otherwise~, body~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Is(
  env,
  expr~,
  pat~,
  loc~,
) {
  let expr = env.inner().visit_Expr(expr)
  let pat = env.inner().visit_Pattern(pat)
  Expr::Is(expr~, pat~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Defer(
  env,
  expr~,
  body~,
  loc~,
) {
  let expr = env.inner().visit_Expr(expr)
  let body = env.inner().visit_Expr(body)
  Expr::Defer(expr~, body~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_LetFn(
  env,
  name~,
  func~,
  body~,
  loc~,
) {
  let name = env.inner().visit_Binder(name=name.name, loc=name.loc)
  let func = env.inner().visit_Func(func)
  let body = env.inner().visit_Expr(body)
  Expr::LetFn(name~, func~, body~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_LetRec(
  env,
  bindings~,
  body~,
  loc~,
) {
  let bindings = bindings.map(binding => match binding {
    (name, func) =>
      (
        env.inner().visit_Binder(name=name.name, loc=name.loc),
        env.inner().visit_Func(func),
      )
  })
  let body = env.inner().visit_Expr(body)
  Expr::LetRec(bindings~, body~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_LetAnd(
  env,
  bindings~,
  body~,
  loc~,
) {
  let bindings = bindings.map(binding => match binding {
    (binder, ty, func) =>
      (
        env.inner().visit_Binder(name=binder.name, loc=binder.loc),
        ty.map(env.inner().visit_Type(_)),
        env.inner().visit_Func(func),
      )
  })
  let body = env.inner().visit_Expr(body)
  Expr::LetAnd(bindings~, body~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Let(
  env,
  pattern~,
  expr~,
  body~,
  loc~,
) {
  let pattern = env.inner().visit_Pattern(pattern)
  let expr = env.inner().visit_Expr(expr)
  let body = env.inner().visit_Expr(body)
  Expr::Let(pattern~, expr~, body~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Sequence(
  env,
  exprs~,
  last_expr~,
  loc~,
) {
  let exprs = exprs.map(expr => env.inner().visit_Expr(expr))
  let last_expr = env.inner().visit_Expr(last_expr)
  Expr::Sequence(exprs~, last_expr~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Tuple(
  env,
  exprs~,
  loc~,
) {
  let exprs = exprs.map(expr => env.inner().visit_Expr(expr))
  Expr::Tuple(exprs~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Record(
  env,
  type_name~,
  fields~,
  trailing~,
  loc~,
) {
  let type_name = type_name.map(type_name => env
    .inner()
    .visit_TypeName(
      name=type_name.name,
      is_object=type_name.is_object,
      loc=type_name.loc,
    ))
  let fields = fields.map(field => env
    .inner()
    .visit_FieldDef(
      label=field.label,
      expr=field.expr,
      loc=field.loc,
      is_pun=field.is_pun,
    ))
  let trailing = env.inner().visit_TrailingMark(trailing)
  Expr::Record(type_name~, fields~, trailing~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_RecordUpdate(
  env,
  type_name~,
  record~,
  fields~,
  loc~,
) {
  let type_name = type_name.map(type_name => env
    .inner()
    .visit_TypeName(
      name=type_name.name,
      is_object=type_name.is_object,
      loc=type_name.loc,
    ))
  let fields = fields.map(field => env
    .inner()
    .visit_FieldDef(
      label=field.label,
      expr=field.expr,
      loc=field.loc,
      is_pun=field.is_pun,
    ))
  let record = env.inner().visit_Expr(record)
  Expr::RecordUpdate(type_name~, record~, fields~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Field(
  env,
  record~,
  accessor~,
  loc~,
) {
  let record = env.inner().visit_Expr(record)
  let accessor = env.inner().visit_Accessor(accessor)
  Expr::Field(record~, accessor~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Method(
  env,
  type_name~,
  method_name~,
  loc~,
) {
  let type_name = env
    .inner()
    .visit_TypeName(
      name=type_name.name,
      is_object=type_name.is_object,
      loc=type_name.loc,
    )
  let method_name = env
    .inner()
    .visit_Label(name=method_name.name, loc=method_name.loc)
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
  let self = env.inner().visit_Expr(self)
  let method_name = env
    .inner()
    .visit_Label(name=method_name.name, loc=method_name.loc)
  let args = args.map(arg => env
    .inner()
    .visit_Argument(value=arg.value, kind=arg.kind))
  Expr::DotApply(self~, method_name~, args~, return_self~, attr~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_As(
  env,
  expr~,
  trait_~,
  loc~,
) {
  let trait_ = env
    .inner()
    .visit_TypeName(
      name=trait_.name,
      is_object=trait_.is_object,
      loc=trait_.loc,
    )
  let expr = env.inner().visit_Expr(expr)
  Expr::As(expr~, trait_~, loc~)
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
  let record = env.inner().visit_Expr(record)
  let accessor = env.inner().visit_Accessor(accessor)
  let field = env.inner().visit_Expr(field)
  let augmented_by = augmented_by.map(var_ => env
    .inner()
    .visit_Var(name=var_.name, loc=var_.loc))
  Expr::Mutate(record~, accessor~, field~, augmented_by~, loc~)
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
  let expr = env.inner().visit_Expr(expr)
  let cases = cases.map(case => env
    .inner()
    .visit_Case(
      pattern=case.pattern,
      guard_=case.guard_.map(guard_ => env.inner().visit_Expr(guard_)),
      body=env.inner().visit_Expr(case.body),
    ))
  let using_ = using_.map(using_ => env
    .inner()
    .visit_Label(name=using_.name, loc=using_.loc))
  Expr::Match(expr~, cases~, match_loc~, using_~, loc~)
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
  let binder = env.inner().visit_Binder(name=binder.name, loc=binder.loc)
  let ty = ty.map(env.inner().visit_Type(_))
  let expr = env.inner().visit_Expr(expr)
  let body = env.inner().visit_Expr(body)
  Expr::LetMut(binder~, ty~, expr~, body~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Pipe(
  env,
  lhs~,
  rhs~,
  loc~,
) {
  let lhs = env.inner().visit_Expr(lhs)
  let rhs = env.inner().visit_Expr(rhs)
  Expr::Pipe(lhs~, rhs~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Assign(
  env,
  var_~,
  expr~,
  augmented_by~,
  loc~,
) {
  let var_ = env.inner().visit_Var(name=var_.name, loc=var_.loc)
  let augmented_by = augmented_by.map(var_ => env
    .inner()
    .visit_Var(name=var_.name, loc=var_.loc))
  let expr = env.inner().visit_Expr(expr)
  Expr::Assign(var_~, expr~, augmented_by~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Hole(
  env,
  loc~,
  kind~,
) {
  let kind = env.inner().visit_Hole(kind)
  Expr::Hole(loc~, kind~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Return(
  env,
  return_value~,
  loc~,
) {
  let return_value = return_value.map(value => env.inner().visit_Expr(value))
  Expr::Return(return_value~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Raise(
  env,
  err_value~,
  loc~,
) {
  let err_value = env.inner().visit_Expr(err_value)
  Expr::Raise(err_value~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Unit(
  _,
  loc~,
  faked~,
) {
  Expr::Unit(loc~, faked~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Break(
  env,
  arg~,
  label~,
  loc~,
) {
  let label = label.map(label => env
    .inner()
    .visit_Label(name=label.name, loc=label.loc))
  let arg = arg.map(arg => env.inner().visit_Expr(arg))
  Expr::Break(arg~, label~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Continue(
  env,
  args~,
  label~,
  loc~,
) {
  let args = args.map(arg => env.inner().visit_Expr(arg))
  let label = label.map(label => env
    .inner()
    .visit_Label(name=label.name, loc=label.loc))
  Expr::Continue(args~, label~, loc~)
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
  let args = args.map(arg => env.inner().visit_Expr(arg))
  let body = body.map(multi_arg_case => env
    .inner()
    .visit_MultiArgCase(
      patterns=multi_arg_case.patterns,
      guard_=multi_arg_case.guard_.map(guard_ => env.inner().visit_Expr(guard_)),
      body=env.inner().visit_Expr(multi_arg_case.body),
    ))
  let label = label.map(label => env
    .inner()
    .visit_Label(name=label.name, loc=label.loc))
  Expr::Loop(args~, body~, label~, loop_loc~, loc~)
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
  let binders = binders.map(p => (
    env.inner().visit_Binder(name=p.0.name, loc=p.0.loc),
    env.inner().visit_Expr(p.1),
  ))
  let condition = condition.map(cond => env.inner().visit_Expr(cond))
  let continue_block = continue_block.map(p => (
    p.0,
    env.inner().visit_Expr(p.1),
  ))
  let body = env.inner().visit_Expr(body)
  let for_else = for_else.map(else_block => env.inner().visit_Expr(else_block))
  let label = label.map(label => env
    .inner()
    .visit_Label(name=label.name, loc=label.loc))
  Expr::For(
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
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_ForEach(
  env,
  binders~,
  expr~,
  body~,
  else_block~,
  label~,
  loc~,
) {
  let binders = binders.map(binder => binder.map(binder => env
    .inner()
    .visit_Binder(name=binder.name, loc=binder.loc)))
  let expr = env.inner().visit_Expr(expr)
  let body = env.inner().visit_Expr(body)
  let else_block = else_block.map(else_block => env
    .inner()
    .visit_Expr(else_block))
  let label = label.map(label => env
    .inner()
    .visit_Label(name=label.name, loc=label.loc))
  Expr::ForEach(binders~, expr~, body~, else_block~, label~, loc~)
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
  let body = env.inner().visit_Expr(body)
  let catch_ = catch_.map(case => env
    .inner()
    .visit_Case(
      pattern=case.pattern,
      guard_=case.guard_.map(guard_ => env.inner().visit_Expr(guard_)),
      body=env.inner().visit_Expr(case.body),
    ))
  let try_else = try_else.map(try_else => try_else.map(case => env
    .inner()
    .visit_Case(
      pattern=case.pattern,
      guard_=case.guard_.map(guard_ => env.inner().visit_Expr(guard_)),
      body=env.inner().visit_Expr(case.body),
    )))
  Expr::Try(
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
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_TryOperator(
  env,
  body~,
  kind~,
  try_loc~,
  loc~,
) {
  let body = env.inner().visit_Expr(body)
  let kind = env.inner().visit_TryOperatorKind(kind)
  Expr::TryOperator(body~, kind~, try_loc~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Map(
  env,
  elems~,
  loc~,
) {
  let elems = elems.map(elem => env
    .inner()
    .visit_MapExprElem(
      key=elem.key,
      expr=elem.expr,
      loc=elem.loc,
      key_loc=elem.key_loc,
    ))
  Expr::Map(elems~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_Group(
  env,
  expr~,
  group~,
  loc~,
) {
  let expr = env.inner().visit_Expr(expr)
  let group = env.inner().visit_Group(group)
  Expr::Group(expr~, group~, loc~)
}

///|
pub impl[T : ImplMapVisitor] ImplMapVisitor for MapVisitorBase[T] with visit_Expr_StaticAssert(
  env,
  asserts~,
  body~,
) {
  let asserts = asserts.map(assert_ => env
    .inner()
    .visit_StaticAssertion(
      ty=assert_.ty,
      loc=assert_.loc,
      trait_=assert_.trait_,
      msg=assert_.msg,
    ))
  let body = env.inner().visit_Expr(body)
  Expr::StaticAssert(asserts~, body~)
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
