///|
pub(open) trait MapVisitor {
  fn base(Self) -> MapVisitorBase[Self] = _

  // Visibility
  fn visit_Visibility(Self, Visibility) -> Visibility = _
  fn visit_Visibility_Pub(Self, attr~ : String?, loc~ : Location) -> Visibility = _
  fn visit_Visibility_Priv(Self, loc~ : Location) -> Visibility = _

  // Constant
  fn visit_Constant(Self, Constant) -> Constant = _

  // Label
  fn visit_Label(Self, Label) -> Label = _

  // ConstrName
  fn visit_ConstrName(Self, ConstrName) -> ConstrName = _

  // LongIdent
  fn visit_LongIdent(Self, LongIdent) -> LongIdent = _
  fn visit_LongIdent_Ident(Self, name~ : String) -> LongIdent = _
  fn visit_LongIdent_Dot(
    Self,
    pkg~ : String,
    id~ : String,
    pkg_loc~ : Location,
    dot_id_loc~ : Location,
  ) -> LongIdent = _

  // TypeName
  fn visit_TypeName(Self, TypeName) -> TypeName = _

  // ConstrId
  fn visit_ConstrId(Self, ConstrId) -> ConstrId = _

  // Type
  fn visit_Type(Self, Type) -> Type = _
  fn visit_Type_Any(Self, loc~ : Location) -> Type = _
  fn visit_Type_Arrow(
    Self,
    args~ : @list.List[Type],
    res~ : Type,
    err~ : ErrorType,
    is_async~ : Location?,
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    arrow_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Type = _
  fn visit_Type_Tuple(
    Self,
    tys~ : @list.List[Type],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Type = _
  fn visit_Type_Name(
    Self,
    constr_id~ : ConstrId,
    tys~ : @list.List[Type],
    type_args_lbracket_loc~ : Location?,
    type_args_rbracket_loc~ : Location?,
    type_arg_comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Type = _
  fn visit_Type_Option(
    Self,
    ty~ : Type,
    loc~ : Location,
    question_loc~ : Location,
  ) -> Type = _
  fn visit_Type_Object(
    Self,
    constr_id~ : ConstrId,
    amp_loc~ : Location,
    loc~ : Location,
  ) -> Type = _

  // ErrorType
  fn visit_ErrorType(Self, ErrorType) -> ErrorType = _
  fn visit_ErrorType_ErrorType(
    Self,
    ty~ : Type,
    raise_loc~ : Location,
    loc~ : Location,
  ) -> ErrorType = _
  fn visit_ErrorType_DefaultErrorType(Self, loc~ : Location) -> ErrorType = _
  fn visit_ErrorType_Noraise(Self, loc~ : Location) -> ErrorType = _
  fn visit_ErrorType_MaybeError(
    Self,
    ty~ : Type,
    raise_loc~ : Location,
    question_loc~ : Location,
    loc~ : Location,
  ) -> ErrorType = _

  // ConstrParam
  fn visit_ConstrParam(Self, ConstrParam) -> ConstrParam = _

  // ConstrDecl
  fn visit_ConstrDecl(Self, ConstrDecl) -> ConstrDecl = _

  // ExceptionDecl
  fn visit_ExceptionDecl(Self, ExceptionDecl) -> ExceptionDecl = _
  fn visit_ExceptionDecl_EnumPayload(
    Self,
    constrs~ : @list.List[ConstrDecl],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
  ) -> ExceptionDecl = _

  // FieldName
  fn visit_FieldName(Self, FieldName) -> FieldName = _

  // FieldDecl
  fn visit_FieldDecl(Self, FieldDecl) -> FieldDecl = _

  // TypeDesc
  fn visit_TypeDesc(Self, TypeDesc) -> TypeDesc = _
  fn visit_TypeDesc_Error(Self, ExceptionDecl) -> TypeDesc = _
  fn visit_TypeDesc_Variant(
    Self,
    constrs~ : @list.List[ConstrDecl],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
  ) -> TypeDesc = _
  fn visit_TypeDesc_Record(
    Self,
    fields~ : @list.List[FieldDecl],
    constr_decl~ : FunDecl?,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
  ) -> TypeDesc = _
  fn visit_TypeDesc_TupleStruct(
    Self,
    types~ : @list.List[Type],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
  ) -> TypeDesc = _
  fn visit_TypeDesc_Alias(Self, type_~ : Type, equal_loc~ : Location) -> TypeDesc = _

  // Hole
  fn visit_Hole(Self, Hole) -> Hole = _

  // ArgumentKind
  fn visit_ArgumentKind(Self, ArgumentKind) -> ArgumentKind = _
  fn visit_ArgumentKind_Labelled(Self, label~ : Label, equal_loc~ : Location) -> ArgumentKind = _
  fn visit_ArgumentKind_LabelledPun(Self, Label) -> ArgumentKind = _
  fn visit_ArgumentKind_LabelledOption(
    Self,
    label~ : Label,
    question_loc~ : Location,
    equal_loc~ : Location,
  ) -> ArgumentKind = _
  fn visit_ArgumentKind_LabelledOptionPun(
    Self,
    label~ : Label,
    question_loc~ : Location,
  ) -> ArgumentKind = _

  // FnKind
  fn visit_FnKind(Self, FnKind) -> FnKind = _

  // Group
  fn visit_Group(Self, Group) -> Group = _

  // TrailingMark
  fn visit_TrailingMark(Self, TrailingMark) -> TrailingMark = _

  // TypeVarConstraint
  fn visit_TypeVarConstraint(Self, TypeVarConstraint) -> TypeVarConstraint = _

  // TypeVarBinder
  fn visit_TypeVarBinder(Self, TypeVarBinder) -> TypeVarBinder = _

  // TypeDeclBinder
  fn visit_TypeDeclBinder(Self, TypeDeclBinder) -> TypeDeclBinder = _

  // Binder
  fn visit_Binder(Self, Binder) -> Binder = _

  // Var
  fn visit_Var(Self, Var) -> Var = _

  // ConstructorExtraInfo
  fn visit_ConstructorExtraInfo(Self, ConstructorExtraInfo) -> ConstructorExtraInfo = _
  fn visit_ConstructorExtraInfo_TypeName(
    Self,
    type_name~ : TypeName,
    coloncolon_loc~ : Location,
  ) -> ConstructorExtraInfo = _
  fn visit_ConstructorExtraInfo_Package(
    Self,
    pkg~ : String,
    pkg_loc~ : Location,
    dot_constr_loc~ : Location,
  ) -> ConstructorExtraInfo = _

  // Constructor
  fn visit_Constructor(Self, Constructor) -> Constructor = _

  // Accessor
  fn visit_Accessor(Self, Accessor) -> Accessor = _
  fn visit_Accessor_Label(Self, Label) -> Accessor = _
  fn visit_Accessor_Index(Self, tuple_index~ : Int, loc~ : Location) -> Accessor = _
  fn visit_Accessor_Newtype(Self, loc~ : Location) -> Accessor = _

  // AliasTarget
  fn visit_AliasTarget(Self, AliasTarget) -> AliasTarget = _

  // Argument
  fn visit_Argument(Self, Argument) -> Argument = _

  // Parameter
  fn visit_Parameter(Self, Parameter) -> Parameter = _
  fn visit_Parameter_DiscardPositional(
    Self,
    ty~ : Type?,
    colon_loc~ : Location?,
    loc~ : Location,
  ) -> Parameter = _
  fn visit_Parameter_Positional(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    colon_loc~ : Location?,
  ) -> Parameter = _
  fn visit_Parameter_Labelled(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    post_label_loc~ : Location?,
    colon_loc~ : Location?,
  ) -> Parameter = _
  fn visit_Parameter_Optional(
    Self,
    binder~ : Binder,
    default~ : Expr,
    ty~ : Type?,
    post_label_loc~ : Location?,
    question_loc~ : Location?,
    colon_loc~ : Location?,
    equal_loc~ : Location,
  ) -> Parameter = _
  fn visit_Parameter_QuestionOptional(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    question_loc~ : Location,
    colon_loc~ : Location?,
  ) -> Parameter = _

  // Case
  fn visit_Case(Self, Case) -> Case = _

  // LexCase
  fn visit_LexCase(Self, LexCase) -> LexCase = _

  // LexTopPattern
  fn visit_LexTopPattern(Self, LexTopPattern) -> LexTopPattern = _
  fn visit_LexTopPattern_Pattern(Self, LexPattern) -> LexTopPattern = _
  fn visit_LexTopPattern_Binder(Self, Binder) -> LexTopPattern = _
  fn visit_LexTopPattern_Wildcard(Self, loc~ : Location) -> LexTopPattern = _

  // LexPattern
  fn visit_LexPattern(Self, LexPattern) -> LexPattern = _
  fn visit_LexPattern_Regex(Self, lit~ : String, offset~ : Int, loc~ : Location) -> LexPattern = _
  fn visit_LexPattern_RegexInterp(
    Self,
    elems~ : @list.List[InterpElem],
    loc~ : Location,
  ) -> LexPattern = _
  fn visit_LexPattern_ConstantRef(Self, lid~ : LongIdent, loc~ : Location) -> LexPattern = _
  fn visit_LexPattern_Alias(
    Self,
    pat~ : LexPattern,
    binder~ : Binder,
    as_loc~ : Location,
    loc~ : Location,
  ) -> LexPattern = _
  fn visit_LexPattern_Sequence(
    Self,
    pats~ : @list.List[LexPattern],
    semi_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> LexPattern = _

  // SpreadableElem
  fn visit_SpreadableElem(Self, SpreadableElem) -> SpreadableElem = _
  fn visit_SpreadableElem_Regular(Self, Expr) -> SpreadableElem = _
  fn visit_SpreadableElem_Spread(
    Self,
    expr~ : Expr,
    dotdot_loc~ : Location,
    loc~ : Location,
  ) -> SpreadableElem = _

  // MapExprElem
  fn visit_MapExprElem(Self, MapExprElem) -> MapExprElem = _

  // StaticAssertion
  fn visit_StaticAssertion(Self, StaticAssertion) -> StaticAssertion = _

  // Func
  fn visit_Func(Self, Func) -> Func = _

  // FieldDef
  fn visit_FieldDef(Self, FieldDef) -> FieldDef = _

  // InterpElem
  fn visit_InterpElem(Self, InterpElem) -> InterpElem = _
  fn visit_InterpElem_Literal(Self, repr~ : StringLiteral, loc~ : Location) -> InterpElem = _
  fn visit_InterpElem_Expr(Self, expr~ : Expr, loc~ : Location) -> InterpElem = _
  fn visit_InterpElem_Source(Self, @tokens.InterpSource) -> InterpElem = _

  // MultilineStringElem
  fn visit_MultilineStringElem(Self, MultilineStringElem) -> MultilineStringElem = _
  fn visit_MultilineStringElem_String(Self, String) -> MultilineStringElem = _
  fn visit_MultilineStringElem_Interp(Self, @list.List[InterpElem]) -> MultilineStringElem = _

  // TryOperatorKind
  fn visit_TryOperatorKind(Self, TryOperatorKind) -> TryOperatorKind = _

  // DotDotBinder
  fn visit_DotDotBinder(Self, DotDotBinder) -> DotDotBinder = _
  fn visit_DotDotBinder_Underscore(
    Self,
    dotdot_loc~ : Location,
    underscore_loc~ : Location,
  ) -> DotDotBinder = _
  fn visit_DotDotBinder_NoBinder(Self, dotdot_loc~ : Location) -> DotDotBinder = _
  fn visit_DotDotBinder_BinderAs(
    Self,
    binder~ : Binder,
    dotdot_loc~ : Location,
    as_loc~ : Location,
  ) -> DotDotBinder = _
  fn visit_DotDotBinder_Binder(Self, binder~ : Binder, dotdot_loc~ : Location) -> DotDotBinder = _

  // ArrayPattern
  fn visit_ArrayPattern(Self, ArrayPattern) -> ArrayPattern = _
  fn visit_ArrayPattern_Pattern(Self, Pattern) -> ArrayPattern = _
  fn visit_ArrayPattern_StringSpread(
    Self,
    str~ : StringLiteral,
    dotdot_loc~ : Location,
    loc~ : Location,
  ) -> ArrayPattern = _
  fn visit_ArrayPattern_BytesSpread(
    Self,
    bytes~ : BytesLiteral,
    dotdot_loc~ : Location,
    loc~ : Location,
  ) -> ArrayPattern = _
  fn visit_ArrayPattern_ConstSpread(
    Self,
    binder~ : Binder,
    pkg~ : String?,
    dotdot_loc~ : Location,
    pkg_loc~ : Location?,
    dot_uident_loc~ : Location?,
    loc~ : Location,
  ) -> ArrayPattern = _

  // ArrayPatterns
  fn visit_ArrayPatterns(Self, ArrayPatterns) -> ArrayPatterns = _
  fn visit_ArrayPatterns_Closed(
    Self,
    @list.List[ArrayPattern],
    comma_locs~ : @list.List[Location],
  ) -> ArrayPatterns = _
  fn visit_ArrayPatterns_Open(
    Self,
    @list.List[ArrayPattern],
    @list.List[ArrayPattern],
    DotDotBinder,
    left_comma_locs~ : @list.List[Location],
    dotdot_comma_loc~ : Location?,
    right_comma_locs~ : @list.List[Location],
  ) -> ArrayPatterns = _

  // WhereClause
  fn visit_WhereClause(Self, WhereClause) -> WhereClause = _

  // FieldPat
  fn visit_FieldPat(Self, FieldPat) -> FieldPat = _

  // ConstrPatArg
  fn visit_ConstrPatArg(Self, ConstrPatArg) -> ConstrPatArg = _

  // MapPatElem
  fn visit_MapPatElem(Self, MapPatElem) -> MapPatElem = _

  // RangeKind
  fn visit_RangeKind(Self, RangeKind) -> RangeKind = _

  // Pattern
  fn visit_Pattern(Self, Pattern) -> Pattern = _
  fn visit_Pattern_Alias(
    Self,
    pat~ : Pattern,
    alias_~ : Binder,
    as_loc~ : Location,
    loc~ : Location,
  ) -> Pattern = _
  fn visit_Pattern_Any(Self, loc~ : Location) -> Pattern = _
  fn visit_Pattern_Array(
    Self,
    pats~ : ArrayPatterns,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    loc~ : Location,
  ) -> Pattern = _
  fn visit_Pattern_Constant(Self, c~ : Constant, loc~ : Location) -> Pattern = _
  fn visit_Pattern_Constraint(
    Self,
    pat~ : Pattern,
    ty~ : Type,
    lparen_loc~ : Location?,
    colon_loc~ : Location,
    rparen_loc~ : Location?,
    loc~ : Location,
  ) -> Pattern = _
  fn visit_Pattern_Constr(
    Self,
    constr~ : Constructor,
    args~ : @list.List[ConstrPatArg]?,
    is_open~ : Bool,
    args_lparen_loc~ : Location?,
    args_rparen_loc~ : Location?,
    arg_comma_locs~ : @list.List[Location],
    open_dotdot_loc~ : Location?,
    loc~ : Location,
  ) -> Pattern = _
  fn visit_Pattern_Or(
    Self,
    pat1~ : Pattern,
    pat2~ : Pattern,
    bar_loc~ : Location,
    loc~ : Location,
  ) -> Pattern = _
  fn visit_Pattern_Tuple(
    Self,
    pats~ : @list.List[Pattern],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Pattern = _
  fn visit_Pattern_Var(Self, Binder) -> Pattern = _
  fn visit_Pattern_Record(
    Self,
    fields~ : @list.List[FieldPat],
    is_closed~ : Bool,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    dotdot_loc~ : Location?,
    loc~ : Location,
  ) -> Pattern = _
  fn visit_Pattern_Map(
    Self,
    elems~ : @list.List[MapPatElem],
    is_closed~ : Bool,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    dotdot_loc~ : Location?,
    loc~ : Location,
  ) -> Pattern = _
  fn visit_Pattern_Range(
    Self,
    lhs~ : Pattern,
    rhs~ : Pattern,
    kind~ : RangeKind,
    range_op_loc~ : Location,
    loc~ : Location,
  ) -> Pattern = _
  fn visit_Pattern_SpecialConstr(
    Self,
    binder~ : Binder,
    args~ : @list.List[ConstrPatArg],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Pattern = _

  // DerivingDirective
  fn visit_DerivingDirective(Self, DerivingDirective) -> DerivingDirective = _

  // TypeDecl
  fn visit_TypeDecl(Self, TypeDecl) -> TypeDecl = _

  // FuncStubs
  fn visit_FuncStubs(Self, FuncStubs) -> FuncStubs = _
  fn visit_FuncStubs_Import(
    Self,
    module_name~ : StringLiteral,
    func_name~ : StringLiteral,
    language~ : StringLiteral?,
    module_name_loc~ : Location,
    func_name_loc~ : Location,
    language_loc~ : Location?,
  ) -> FuncStubs = _
  fn visit_FuncStubs_Embedded(
    Self,
    language~ : StringLiteral?,
    code~ : EmbeddedCode,
    language_loc~ : Location?,
    code_loc~ : Location,
  ) -> FuncStubs = _

  // EmbeddedCode
  fn visit_EmbeddedCode(Self, EmbeddedCode) -> EmbeddedCode = _
  fn visit_EmbeddedCode_CodeString(Self, StringLiteral, loc~ : Location) -> EmbeddedCode = _
  fn visit_EmbeddedCode_CodeMultilineString(
    Self,
    @list.List[String],
    string_locs~ : @list.List[Location],
  ) -> EmbeddedCode = _

  // DeclBody
  fn visit_DeclBody(Self, DeclBody) -> DeclBody = _
  fn visit_DeclBody_DeclBody(
    Self,
    expr~ : Expr,
    lbrace_loc~ : Location?,
    rbrace_loc~ : Location?,
  ) -> DeclBody = _
  fn visit_DeclBody_DeclStubs(Self, FuncStubs, equal_loc~ : Location) -> DeclBody = _

  // FunDecl
  fn visit_FunDecl(Self, FunDecl) -> FunDecl = _

  // TraitMethodDecl
  fn visit_TraitMethodDecl(Self, TraitMethodDecl) -> TraitMethodDecl = _

  // TraitDecl
  fn visit_TraitDecl(Self, TraitDecl) -> TraitDecl = _

  // UsingKind
  fn visit_UsingKind(Self, UsingKind) -> UsingKind = _

  // Impl
  fn visit_Impl(Self, Impl) -> Impl = _
  fn visit_Impl_TopExpr(
    Self,
    expr~ : Expr,
    is_main~ : Bool,
    is_async~ : Location?,
    loc~ : Location,
  ) -> Impl = _
  fn visit_Impl_TopTest(
    Self,
    expr~ : Expr,
    name~ : TestName,
    params~ : Parameters?,
    is_async~ : Location?,
    test_loc~ : Location,
    params_lparen_loc~ : Location?,
    params_rparen_loc~ : Location?,
    param_comma_locs~ : @list.List[Location],
    body_lbrace_loc~ : Location,
    body_rbrace_loc~ : Location,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> Impl = _
  fn visit_Impl_TopTypeDef(Self, TypeDecl) -> Impl = _
  fn visit_Impl_TopFuncDef(
    Self,
    fun_decl~ : FunDecl,
    decl_body~ : DeclBody,
    where_clause~ : WhereClause?,
    equal_loc~ : Location?,
    loc~ : Location,
  ) -> Impl = _
  fn visit_Impl_TopLetDef(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    expr~ : Expr,
    vis~ : Visibility,
    is_constant~ : Bool,
    declare_loc~ : Location?,
    let_or_const_loc~ : Location,
    annot_colon_loc~ : Location?,
    equal_loc~ : Location,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
    is_declare~ : Bool,
  ) -> Impl = _
  fn visit_Impl_TopTrait(Self, TraitDecl) -> Impl = _
  fn visit_Impl_TopImpl(
    Self,
    self_ty~ : Type?,
    trait_~ : TypeName,
    method_name~ : Binder,
    quantifiers~ : @list.List[TypeVarBinder],
    params~ : Parameters,
    ret_ty~ : Type?,
    err_ty~ : ErrorType,
    body~ : DeclBody,
    vis~ : Visibility,
    impl_loc~ : Location,
    type_params_lbracket_loc~ : Location?,
    type_params_rbracket_loc~ : Location?,
    type_param_comma_locs~ : @list.List[Location],
    for_loc~ : Location?,
    with_loc~ : Location,
    fn_loc~ : Location?,
    params_lparen_loc~ : Location,
    params_rparen_loc~ : Location,
    param_comma_locs~ : @list.List[Location],
    return_arrow_loc~ : Location?,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> Impl = _
  fn visit_Impl_TopView(
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
    enumview_loc~ : Location,
    type_params_lbracket_loc~ : Location?,
    type_params_rbracket_loc~ : Location?,
    type_param_comma_locs~ : @list.List[Location],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    constructor_semi_locs~ : @list.List[Location],
    for_loc~ : Location,
    with_loc~ : Location,
    params_lparen_loc~ : Location,
    params_rparen_loc~ : Location,
    param_comma_locs~ : @list.List[Location],
    body_lbrace_loc~ : Location,
    body_rbrace_loc~ : Location,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    doc~ : DocString,
  ) -> Impl = _
  fn visit_Impl_TopImplRelation(
    Self,
    self_ty~ : Type,
    trait_~ : TypeName,
    quantifiers~ : @list.List[TypeVarBinder],
    vis~ : Visibility,
    attrs~ : @list.List[Attribute],
    declare_loc~ : Location?,
    impl_loc~ : Location,
    type_params_lbracket_loc~ : Location?,
    type_params_rbracket_loc~ : Location?,
    type_param_comma_locs~ : @list.List[Location],
    for_loc~ : Location,
    loc~ : Location,
    doc~ : DocString,
    is_declare~ : Bool,
  ) -> Impl = _
  fn visit_Impl_TopUsing(
    Self,
    pkg~ : Label,
    names~ : @list.List[(AliasTarget, UsingKind)],
    vis~ : Visibility,
    attrs~ : @list.List[Attribute],
    using_loc~ : Location,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
    doc~ : DocString,
  ) -> Impl = _

  // Expr
  fn visit_Expr(Self, Expr) -> Expr = _
  fn visit_Expr_Apply(
    Self,
    func~ : Expr,
    args~ : @list.List[Argument],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Infix(
    Self,
    op~ : Var,
    lhs~ : Expr,
    rhs~ : Expr,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Unary(Self, op~ : Var, expr~ : Expr, loc~ : Location) -> Expr = _
  fn visit_Expr_Array(
    Self,
    exprs~ : @list.List[Expr],
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_ArraySpread(
    Self,
    elems~ : @list.List[SpreadableElem],
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_ArrayGet(
    Self,
    array~ : Expr,
    index~ : Expr,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_ArrayGetSlice(
    Self,
    array~ : Expr,
    start_index~ : Expr?,
    end_index~ : Expr?,
    index_loc~ : Location,
    lbracket_loc~ : Location,
    colon_loc~ : Location,
    rbracket_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_ArraySet(
    Self,
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    equal_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_ArrayAugmentedSet(
    Self,
    op~ : Var,
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    augmented_assign_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Constant(Self, c~ : Constant, loc~ : Location) -> Expr = _
  fn visit_Expr_MultilineString(
    Self,
    elems~ : @list.List[MultilineStringElem],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Interp(Self, elems~ : @list.List[InterpElem], loc~ : Location) -> Expr = _
  fn visit_Expr_Constraint(
    Self,
    expr~ : Expr,
    ty~ : Type,
    lparen_loc~ : Location?,
    colon_loc~ : Location,
    rparen_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Constr(Self, constr~ : Constructor, loc~ : Location) -> Expr = _
  fn visit_Expr_While(
    Self,
    loop_cond~ : Expr,
    loop_body~ : Expr,
    while_else~ : Expr?,
    label~ : Label?,
    label_colon_loc~ : Location?,
    while_loc~ : Location,
    body_lbrace_loc~ : Location,
    body_rbrace_loc~ : Location,
    nobreak_loc~ : Location?,
    nobreak_lbrace_loc~ : Location?,
    nobreak_rbrace_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Function(Self, func~ : Func, loc~ : Location) -> Expr = _
  fn visit_Expr_Ident(Self, id~ : Var, loc~ : Location) -> Expr = _
  fn visit_Expr_If(
    Self,
    cond~ : Expr,
    ifso~ : Expr,
    ifnot~ : Expr?,
    if_loc~ : Location,
    then_lbrace_loc~ : Location,
    then_rbrace_loc~ : Location,
    else_loc~ : Location?,
    else_lbrace_loc~ : Location?,
    else_rbrace_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Guard(
    Self,
    cond~ : Expr,
    otherwise~ : Expr?,
    body~ : Expr,
    guard_loc~ : Location,
    else_loc~ : Location?,
    else_lbrace_loc~ : Location?,
    else_rbrace_loc~ : Location?,
    semi_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Is(
    Self,
    expr~ : Expr,
    pat~ : Pattern,
    is_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_IsLexMatch(
    Self,
    expr~ : Expr,
    strategy~ : Label?,
    pat~ : @list.List[LexTopPattern],
    pat_loc~ : Location,
    lexmatch_question_loc~ : Location,
    with_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Defer(
    Self,
    expr~ : Expr,
    body~ : Expr,
    defer_loc~ : Location,
    semi_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_LetFn(
    Self,
    name~ : Binder,
    func~ : Func,
    body~ : Expr,
    semi_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_LetAnd(
    Self,
    bindings~ : @list.List[(Binder, Type?, Func)],
    body~ : Expr,
    letrec_loc~ : Location,
    and_locs~ : @list.List[Location],
    equal_locs~ : @list.List[Location],
    annot_colon_locs~ : @list.List[Location],
    semi_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Let(
    Self,
    pattern~ : Pattern,
    expr~ : Expr,
    body~ : Expr,
    let_loc~ : Location,
    annot_colon_loc~ : Location?,
    equal_loc~ : Location,
    semi_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Sequence(
    Self,
    exprs~ : @list.List[Expr],
    last_expr~ : Expr,
    semi_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Tuple(
    Self,
    exprs~ : @list.List[Expr],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Record(
    Self,
    type_name~ : TypeName?,
    fields~ : @list.List[FieldDef],
    trailing~ : TrailingMark,
    coloncolon_loc~ : Location?,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    semi_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_RecordUpdate(
    Self,
    type_name~ : TypeName?,
    record~ : Expr,
    fields~ : @list.List[FieldDef],
    coloncolon_loc~ : Location?,
    lbrace_loc~ : Location,
    dotdot_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Field(
    Self,
    record~ : Expr,
    accessor~ : Accessor,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Method(
    Self,
    type_name~ : TypeName,
    method_name~ : Label,
    coloncolon_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_DotApply(
    Self,
    self~ : Expr,
    method_name~ : Label,
    args~ : @list.List[Argument],
    return_self~ : Bool,
    dot_loc~ : Location,
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_As(
    Self,
    expr~ : Expr,
    trait_~ : TypeName,
    as_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Mutate(
    Self,
    record~ : Expr,
    accessor~ : Accessor,
    field~ : Expr,
    augmented_by~ : Var?,
    equal_loc~ : Location?,
    augmented_assign_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Match(
    Self,
    expr~ : Expr,
    cases~ : @list.List[Case],
    match_loc~ : Location,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_LexMatch(
    Self,
    strategy~ : Label?,
    expr~ : Expr,
    match_loc~ : Location,
    with_loc~ : Location?,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
    cases~ : @list.List[LexCase],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_LetMut(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    expr~ : Expr,
    body~ : Expr,
    let_loc~ : Location,
    mut_loc~ : Location,
    annot_colon_loc~ : Location?,
    equal_loc~ : Location,
    semi_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Pipe(
    Self,
    lhs~ : Expr,
    rhs~ : Expr,
    pipe_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Assign(
    Self,
    var_~ : Var,
    expr~ : Expr,
    augmented_by~ : Var?,
    equal_loc~ : Location?,
    augmented_assign_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Hole(Self, loc~ : Location, kind~ : Hole) -> Expr = _
  fn visit_Expr_Return(
    Self,
    return_value~ : Expr?,
    return_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Raise(
    Self,
    err_value~ : Expr,
    raise_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Unit(
    Self,
    loc~ : Location,
    faked~ : Bool,
    lparen_loc~ : Location,
    rparen_loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Break(
    Self,
    arg~ : Expr?,
    label~ : Label?,
    break_loc~ : Location,
    post_label_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Continue(
    Self,
    args~ : @list.List[Expr],
    label~ : Label?,
    continue_loc~ : Location,
    post_label_loc~ : Location?,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_For(
    Self,
    binders~ : @list.List[(Binder, Expr)],
    condition~ : Expr?,
    continue_block~ : @list.List[(Binder, Expr)],
    body~ : Expr,
    for_else~ : Expr?,
    label~ : Label?,
    label_colon_loc~ : Location?,
    for_loc~ : Location,
    binder_comma_locs~ : @list.List[Location],
    binder_equal_locs~ : @list.List[Location],
    semi_locs~ : @list.List[Location],
    continue_comma_locs~ : @list.List[Location],
    continue_equal_locs~ : @list.List[Location],
    body_lbrace_loc~ : Location,
    body_rbrace_loc~ : Location,
    nobreak_loc~ : Location?,
    nobreak_lbrace_loc~ : Location?,
    nobreak_rbrace_loc~ : Location?,
    loc~ : Location,
    where_clause~ : WhereClause?,
  ) -> Expr = _
  fn visit_Expr_ForEach(
    Self,
    binders~ : @list.List[Binder?],
    expr~ : Expr,
    init~ : @list.List[(Binder, Expr)],
    continue_block~ : @list.List[(Binder, Expr)],
    body~ : Expr,
    else_block~ : Expr?,
    where_clause~ : WhereClause?,
    label~ : Label?,
    label_colon_loc~ : Location?,
    for_loc~ : Location,
    binder_comma_locs~ : @list.List[Location],
    in_loc~ : Location,
    semi_locs~ : @list.List[Location],
    init_comma_locs~ : @list.List[Location],
    init_equal_locs~ : @list.List[Location],
    continue_comma_locs~ : @list.List[Location],
    continue_equal_locs~ : @list.List[Location],
    body_lbrace_loc~ : Location,
    body_rbrace_loc~ : Location,
    nobreak_loc~ : Location?,
    nobreak_lbrace_loc~ : Location?,
    nobreak_rbrace_loc~ : Location?,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Try(
    Self,
    body~ : Expr,
    catch_~ : @list.List[Case],
    try_else~ : @list.List[Case]?,
    has_try~ : Bool,
    try_loc~ : Location,
    catch_loc~ : Location,
    else_loc~ : Location,
    catch_lbrace_loc~ : Location,
    catch_rbrace_loc~ : Location,
    catch_semi_locs~ : @list.List[Location],
    noraise_lbrace_loc~ : Location?,
    noraise_rbrace_loc~ : Location?,
    noraise_semi_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_TryOperator(
    Self,
    body~ : Expr,
    kind~ : TryOperatorKind,
    try_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Map(
    Self,
    elems~ : @list.List[MapExprElem],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_Group(
    Self,
    expr~ : Expr,
    group~ : Group,
    left_delim_loc~ : Location,
    right_delim_loc~ : Location,
    loc~ : Location,
  ) -> Expr = _
  fn visit_Expr_StaticAssert(
    Self,
    asserts~ : @list.List[StaticAssertion],
    body~ : Expr,
  ) -> Expr = _
}

///|
impl MapVisitor with fn base(self) -> MapVisitorBase[Self] {
  MapVisitorBase::MapVisitorBase(self)
}

///|
impl MapVisitor with fn visit_Impl(env, impl_) {
  MapVisitorBase::visit_Impl(env, impl_)
}

///|
impl MapVisitor with fn visit_Impl_TopExpr(
  env,
  expr~,
  is_main~,
  is_async~,
  loc~,
) {
  MapVisitorBase::visit_Impl_TopExpr(env, expr~, is_main~, is_async~, loc~)
}

///|
impl MapVisitor with fn visit_Impl_TopTest(
  env,
  expr~,
  name~,
  params~,
  is_async~,
  test_loc~,
  params_lparen_loc~,
  params_rparen_loc~,
  param_comma_locs~,
  body_lbrace_loc~,
  body_rbrace_loc~,
  loc~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_Impl_TopTest(
    env,
    expr~,
    name~,
    params~,
    is_async~,
    test_loc~,
    params_lparen_loc~,
    params_rparen_loc~,
    param_comma_locs~,
    body_lbrace_loc~,
    body_rbrace_loc~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
impl MapVisitor with fn visit_Impl_TopTypeDef(env, type_decl) {
  MapVisitorBase::visit_Impl_TopTypeDef(env, type_decl)
}

///|
impl MapVisitor with fn visit_Impl_TopFuncDef(
  env,
  fun_decl~,
  decl_body~,
  where_clause~,
  equal_loc~,
  loc~,
) {
  MapVisitorBase::visit_Impl_TopFuncDef(
    env,
    fun_decl~,
    decl_body~,
    where_clause~,
    equal_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Impl_TopLetDef(
  env,
  binder~,
  ty~,
  expr~,
  vis~,
  is_constant~,
  declare_loc~,
  let_or_const_loc~,
  annot_colon_loc~,
  equal_loc~,
  loc~,
  attrs~,
  doc~,
  is_declare~,
) {
  MapVisitorBase::visit_Impl_TopLetDef(
    env,
    binder~,
    ty~,
    expr~,
    vis~,
    is_constant~,
    declare_loc~,
    let_or_const_loc~,
    annot_colon_loc~,
    equal_loc~,
    loc~,
    attrs~,
    doc~,
    is_declare~,
  )
}

///|
impl MapVisitor with fn visit_Impl_TopTrait(env, trait_decl) {
  MapVisitorBase::visit_Impl_TopTrait(env, trait_decl)
}

///|
impl MapVisitor with fn visit_Impl_TopImpl(
  env,
  self_ty~,
  trait_~,
  method_name~,
  quantifiers~,
  params~,
  ret_ty~,
  err_ty~,
  body~,
  vis~,
  impl_loc~,
  type_params_lbracket_loc~,
  type_params_rbracket_loc~,
  type_param_comma_locs~,
  for_loc~,
  with_loc~,
  fn_loc~,
  params_lparen_loc~,
  params_rparen_loc~,
  param_comma_locs~,
  return_arrow_loc~,
  loc~,
  attrs~,
  doc~,
) {
  MapVisitorBase::visit_Impl_TopImpl(
    env,
    self_ty~,
    trait_~,
    method_name~,
    quantifiers~,
    params~,
    ret_ty~,
    err_ty~,
    body~,
    vis~,
    impl_loc~,
    type_params_lbracket_loc~,
    type_params_rbracket_loc~,
    type_param_comma_locs~,
    for_loc~,
    with_loc~,
    fn_loc~,
    params_lparen_loc~,
    params_rparen_loc~,
    param_comma_locs~,
    return_arrow_loc~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
impl MapVisitor with fn visit_Impl_TopView(
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
  enumview_loc~,
  type_params_lbracket_loc~,
  type_params_rbracket_loc~,
  type_param_comma_locs~,
  lbrace_loc~,
  rbrace_loc~,
  constructor_semi_locs~,
  for_loc~,
  with_loc~,
  params_lparen_loc~,
  params_rparen_loc~,
  param_comma_locs~,
  body_lbrace_loc~,
  body_rbrace_loc~,
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
    enumview_loc~,
    type_params_lbracket_loc~,
    type_params_rbracket_loc~,
    type_param_comma_locs~,
    lbrace_loc~,
    rbrace_loc~,
    constructor_semi_locs~,
    for_loc~,
    with_loc~,
    params_lparen_loc~,
    params_rparen_loc~,
    param_comma_locs~,
    body_lbrace_loc~,
    body_rbrace_loc~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
impl MapVisitor with fn visit_Impl_TopImplRelation(
  env,
  self_ty~,
  trait_~,
  quantifiers~,
  vis~,
  attrs~,
  declare_loc~,
  impl_loc~,
  type_params_lbracket_loc~,
  type_params_rbracket_loc~,
  type_param_comma_locs~,
  for_loc~,
  loc~,
  doc~,
  is_declare~,
) {
  MapVisitorBase::visit_Impl_TopImplRelation(
    env,
    self_ty~,
    trait_~,
    quantifiers~,
    vis~,
    attrs~,
    declare_loc~,
    impl_loc~,
    type_params_lbracket_loc~,
    type_params_rbracket_loc~,
    type_param_comma_locs~,
    for_loc~,
    loc~,
    doc~,
    is_declare~,
  )
}

///|
impl MapVisitor with fn visit_Impl_TopUsing(
  env,
  pkg~,
  names~,
  vis~,
  attrs~,
  using_loc~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  loc~,
  doc~,
) {
  MapVisitorBase::visit_Impl_TopUsing(
    env,
    pkg~,
    names~,
    vis~,
    attrs~,
    using_loc~,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs~,
    loc~,
    doc~,
  )
}

///|
impl MapVisitor with fn visit_UsingKind(env, using_kind) {
  MapVisitorBase::visit_UsingKind(env, using_kind)
}

///|
impl MapVisitor with fn visit_Expr(env, expr) {
  MapVisitorBase::visit_Expr(env, expr)
}

///|
impl MapVisitor with fn visit_Expr_Apply(
  env,
  func~,
  args~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Apply(
    env,
    func~,
    args~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Infix(env, op~, lhs~, rhs~, loc~) {
  MapVisitorBase::visit_Expr_Infix(env, op~, lhs~, rhs~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Unary(env, op~, expr~, loc~) {
  MapVisitorBase::visit_Expr_Unary(env, op~, expr~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Array(
  env,
  exprs~,
  lbracket_loc~,
  rbracket_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Array(
    env,
    exprs~,
    lbracket_loc~,
    rbracket_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_ArraySpread(
  env,
  elems~,
  lbracket_loc~,
  rbracket_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_ArraySpread(
    env,
    elems~,
    lbracket_loc~,
    rbracket_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_ArrayGet(
  env,
  array~,
  index~,
  lbracket_loc~,
  rbracket_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_ArrayGet(
    env,
    array~,
    index~,
    lbracket_loc~,
    rbracket_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_ArrayGetSlice(
  env,
  array~,
  start_index~,
  end_index~,
  index_loc~,
  lbracket_loc~,
  colon_loc~,
  rbracket_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_ArrayGetSlice(
    env,
    array~,
    start_index~,
    end_index~,
    index_loc~,
    lbracket_loc~,
    colon_loc~,
    rbracket_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_ArraySet(
  env,
  array~,
  index~,
  value~,
  lbracket_loc~,
  rbracket_loc~,
  equal_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_ArraySet(
    env,
    array~,
    index~,
    value~,
    lbracket_loc~,
    rbracket_loc~,
    equal_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_ArrayAugmentedSet(
  env,
  op~,
  array~,
  index~,
  value~,
  lbracket_loc~,
  rbracket_loc~,
  augmented_assign_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_ArrayAugmentedSet(
    env,
    op~,
    array~,
    index~,
    value~,
    lbracket_loc~,
    rbracket_loc~,
    augmented_assign_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Constant(env, c~, loc~) {
  MapVisitorBase::visit_Expr_Constant(env, c~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_MultilineString(env, elems~, loc~) {
  MapVisitorBase::visit_Expr_MultilineString(env, elems~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Interp(env, elems~, loc~) {
  MapVisitorBase::visit_Expr_Interp(env, elems~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Constraint(
  env,
  expr~,
  ty~,
  lparen_loc~,
  colon_loc~,
  rparen_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Constraint(
    env,
    expr~,
    ty~,
    lparen_loc~,
    colon_loc~,
    rparen_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Constr(env, constr~, loc~) {
  MapVisitorBase::visit_Expr_Constr(env, constr~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_While(
  env,
  loop_cond~,
  loop_body~,
  while_else~,
  label~,
  label_colon_loc~,
  while_loc~,
  body_lbrace_loc~,
  body_rbrace_loc~,
  nobreak_loc~,
  nobreak_lbrace_loc~,
  nobreak_rbrace_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_While(
    env,
    loop_cond~,
    loop_body~,
    while_else~,
    label~,
    label_colon_loc~,
    while_loc~,
    body_lbrace_loc~,
    body_rbrace_loc~,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Function(env, func~, loc~) {
  MapVisitorBase::visit_Expr_Function(env, func~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Ident(env, id~, loc~) {
  MapVisitorBase::visit_Expr_Ident(env, id~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_If(
  env,
  cond~,
  ifso~,
  ifnot~,
  if_loc~,
  then_lbrace_loc~,
  then_rbrace_loc~,
  else_loc~,
  else_lbrace_loc~,
  else_rbrace_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_If(
    env,
    cond~,
    ifso~,
    ifnot~,
    if_loc~,
    then_lbrace_loc~,
    then_rbrace_loc~,
    else_loc~,
    else_lbrace_loc~,
    else_rbrace_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Guard(
  env,
  cond~,
  otherwise~,
  body~,
  guard_loc~,
  else_loc~,
  else_lbrace_loc~,
  else_rbrace_loc~,
  semi_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Guard(
    env,
    cond~,
    otherwise~,
    body~,
    guard_loc~,
    else_loc~,
    else_lbrace_loc~,
    else_rbrace_loc~,
    semi_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Is(env, expr~, pat~, is_loc~, loc~) {
  MapVisitorBase::visit_Expr_Is(env, expr~, pat~, is_loc~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Defer(
  env,
  expr~,
  body~,
  defer_loc~,
  semi_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Defer(
    env,
    expr~,
    body~,
    defer_loc~,
    semi_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_LetFn(
  env,
  name~,
  func~,
  body~,
  semi_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_LetFn(env, name~, func~, body~, semi_loc~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_LetAnd(
  env,
  bindings~,
  body~,
  letrec_loc~,
  and_locs~,
  equal_locs~,
  annot_colon_locs~,
  semi_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_LetAnd(
    env,
    bindings~,
    body~,
    letrec_loc~,
    and_locs~,
    equal_locs~,
    annot_colon_locs~,
    semi_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Let(
  env,
  pattern~,
  expr~,
  body~,
  let_loc~,
  annot_colon_loc~,
  equal_loc~,
  semi_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Let(
    env,
    pattern~,
    expr~,
    body~,
    let_loc~,
    annot_colon_loc~,
    equal_loc~,
    semi_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Sequence(
  env,
  exprs~,
  last_expr~,
  semi_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Sequence(env, exprs~, last_expr~, semi_locs~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Tuple(
  env,
  exprs~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Tuple(
    env,
    exprs~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Record(
  env,
  type_name~,
  fields~,
  trailing~,
  coloncolon_loc~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  semi_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Record(
    env,
    type_name~,
    fields~,
    trailing~,
    coloncolon_loc~,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs~,
    semi_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_RecordUpdate(
  env,
  type_name~,
  record~,
  fields~,
  coloncolon_loc~,
  lbrace_loc~,
  dotdot_loc~,
  rbrace_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_RecordUpdate(
    env,
    type_name~,
    record~,
    fields~,
    coloncolon_loc~,
    lbrace_loc~,
    dotdot_loc~,
    rbrace_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Field(env, record~, accessor~, loc~) {
  MapVisitorBase::visit_Expr_Field(env, record~, accessor~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Method(
  env,
  type_name~,
  method_name~,
  coloncolon_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Method(
    env,
    type_name~,
    method_name~,
    coloncolon_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_DotApply(
  env,
  self~,
  method_name~,
  args~,
  return_self~,
  dot_loc~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_DotApply(
    env,
    self~,
    method_name~,
    args~,
    return_self~,
    dot_loc~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_As(env, expr~, trait_~, as_loc~, loc~) {
  MapVisitorBase::visit_Expr_As(env, expr~, trait_~, as_loc~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Mutate(
  env,
  record~,
  accessor~,
  field~,
  augmented_by~,
  equal_loc~,
  augmented_assign_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Mutate(
    env,
    record~,
    accessor~,
    field~,
    augmented_by~,
    equal_loc~,
    augmented_assign_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Match(
  env,
  expr~,
  cases~,
  match_loc~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Match(
    env,
    expr~,
    cases~,
    match_loc~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_LetMut(
  env,
  binder~,
  ty~,
  expr~,
  body~,
  let_loc~,
  mut_loc~,
  annot_colon_loc~,
  equal_loc~,
  semi_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_LetMut(
    env,
    binder~,
    ty~,
    expr~,
    body~,
    let_loc~,
    mut_loc~,
    annot_colon_loc~,
    equal_loc~,
    semi_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Pipe(env, lhs~, rhs~, pipe_loc~, loc~) {
  MapVisitorBase::visit_Expr_Pipe(env, lhs~, rhs~, pipe_loc~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Assign(
  env,
  var_~,
  expr~,
  augmented_by~,
  equal_loc~,
  augmented_assign_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Assign(
    env,
    var_~,
    expr~,
    augmented_by~,
    equal_loc~,
    augmented_assign_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Hole(env, loc~, kind~) {
  MapVisitorBase::visit_Expr_Hole(env, loc~, kind~)
}

///|
impl MapVisitor with fn visit_Expr_Return(env, return_value~, return_loc~, loc~) {
  MapVisitorBase::visit_Expr_Return(env, return_value~, return_loc~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Raise(env, err_value~, raise_loc~, loc~) {
  MapVisitorBase::visit_Expr_Raise(env, err_value~, raise_loc~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Unit(
  env,
  loc~,
  faked~,
  lparen_loc~,
  rparen_loc~,
) {
  MapVisitorBase::visit_Expr_Unit(env, loc~, faked~, lparen_loc~, rparen_loc~)
}

///|
impl MapVisitor with fn visit_Expr_Break(
  env,
  arg~,
  label~,
  break_loc~,
  post_label_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Break(
    env,
    arg~,
    label~,
    break_loc~,
    post_label_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Continue(
  env,
  args~,
  label~,
  continue_loc~,
  post_label_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Continue(
    env,
    args~,
    label~,
    continue_loc~,
    post_label_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_For(
  env,
  binders~,
  condition~,
  continue_block~,
  body~,
  for_else~,
  label~,
  label_colon_loc~,
  for_loc~,
  binder_comma_locs~,
  binder_equal_locs~,
  semi_locs~,
  continue_comma_locs~,
  continue_equal_locs~,
  body_lbrace_loc~,
  body_rbrace_loc~,
  nobreak_loc~,
  nobreak_lbrace_loc~,
  nobreak_rbrace_loc~,
  loc~,
  where_clause~,
) {
  MapVisitorBase::visit_Expr_For(
    env,
    binders~,
    condition~,
    continue_block~,
    body~,
    for_else~,
    label~,
    label_colon_loc~,
    for_loc~,
    binder_comma_locs~,
    binder_equal_locs~,
    semi_locs~,
    continue_comma_locs~,
    continue_equal_locs~,
    body_lbrace_loc~,
    body_rbrace_loc~,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
    loc~,
    where_clause~,
  )
}

///|
impl MapVisitor with fn visit_Expr_ForEach(
  env,
  binders~,
  expr~,
  init~,
  continue_block~,
  body~,
  else_block~,
  where_clause~,
  label~,
  label_colon_loc~,
  for_loc~,
  binder_comma_locs~,
  in_loc~,
  semi_locs~,
  init_comma_locs~,
  init_equal_locs~,
  continue_comma_locs~,
  continue_equal_locs~,
  body_lbrace_loc~,
  body_rbrace_loc~,
  nobreak_loc~,
  nobreak_lbrace_loc~,
  nobreak_rbrace_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_ForEach(
    env,
    binders~,
    expr~,
    init~,
    continue_block~,
    body~,
    else_block~,
    where_clause~,
    label~,
    label_colon_loc~,
    for_loc~,
    binder_comma_locs~,
    in_loc~,
    semi_locs~,
    init_comma_locs~,
    init_equal_locs~,
    continue_comma_locs~,
    continue_equal_locs~,
    body_lbrace_loc~,
    body_rbrace_loc~,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Try(
  env,
  body~,
  catch_~,
  try_else~,
  has_try~,
  try_loc~,
  catch_loc~,
  else_loc~,
  catch_lbrace_loc~,
  catch_rbrace_loc~,
  catch_semi_locs~,
  noraise_lbrace_loc~,
  noraise_rbrace_loc~,
  noraise_semi_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Try(
    env,
    body~,
    catch_~,
    try_else~,
    has_try~,
    try_loc~,
    catch_loc~,
    else_loc~,
    catch_lbrace_loc~,
    catch_rbrace_loc~,
    catch_semi_locs~,
    noraise_lbrace_loc~,
    noraise_rbrace_loc~,
    noraise_semi_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_TryOperator(
  env,
  body~,
  kind~,
  try_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_TryOperator(env, body~, kind~, try_loc~, loc~)
}

///|
impl MapVisitor with fn visit_Expr_Map(
  env,
  elems~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Map(
    env,
    elems~,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_Group(
  env,
  expr~,
  group~,
  left_delim_loc~,
  right_delim_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_Group(
    env,
    expr~,
    group~,
    left_delim_loc~,
    right_delim_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_StaticAssert(env, asserts~, body~) {
  MapVisitorBase::visit_Expr_StaticAssert(env, asserts~, body~)
}

///|
impl MapVisitor with fn visit_Visibility(env, visibility) {
  MapVisitorBase::visit_Visibility(env, visibility)
}

///|
impl MapVisitor with fn visit_Visibility_Pub(env, attr~, loc~) {
  MapVisitorBase::visit_Visibility_Pub(env, attr~, loc~)
}

///|
impl MapVisitor with fn visit_Visibility_Priv(env, loc~) {
  MapVisitorBase::visit_Visibility_Priv(env, loc~)
}

///|
impl MapVisitor with fn visit_Constant(env, constant) {
  MapVisitorBase::visit_Constant(env, constant)
}

///|
impl MapVisitor with fn visit_Label(env, label) {
  MapVisitorBase::visit_Label(env, label)
}

///|
impl MapVisitor with fn visit_ConstrName(env, constr_name) {
  MapVisitorBase::visit_ConstrName(env, constr_name)
}

///|
impl MapVisitor with fn visit_LongIdent(env, long_ident) {
  MapVisitorBase::visit_LongIdent(env, long_ident)
}

///|
impl MapVisitor with fn visit_LongIdent_Ident(env, name~) {
  MapVisitorBase::visit_LongIdent_Ident(env, name~)
}

///|
impl MapVisitor with fn visit_LongIdent_Dot(
  env,
  pkg~,
  id~,
  pkg_loc~,
  dot_id_loc~,
) {
  MapVisitorBase::visit_LongIdent_Dot(env, pkg~, id~, pkg_loc~, dot_id_loc~)
}

///|
impl MapVisitor with fn visit_TypeName(env, type_name) {
  MapVisitorBase::visit_TypeName(env, type_name)
}

///|
impl MapVisitor with fn visit_ConstrId(env, constr_id) {
  MapVisitorBase::visit_ConstrId(env, constr_id)
}

///|
impl MapVisitor with fn visit_Type(env, type_) {
  MapVisitorBase::visit_Type(env, type_)
}

///|
impl MapVisitor with fn visit_Type_Any(env, loc~) {
  MapVisitorBase::visit_Type_Any(env, loc~)
}

///|
impl MapVisitor with fn visit_Type_Arrow(
  env,
  args~,
  res~,
  err~,
  is_async~,
  lparen_loc~,
  rparen_loc~,
  arrow_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Type_Arrow(
    env,
    args~,
    res~,
    err~,
    is_async~,
    lparen_loc~,
    rparen_loc~,
    arrow_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Type_Tuple(
  env,
  tys~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Type_Tuple(
    env,
    tys~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Type_Name(
  env,
  constr_id~,
  tys~,
  type_args_lbracket_loc~,
  type_args_rbracket_loc~,
  type_arg_comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Type_Name(
    env,
    constr_id~,
    tys~,
    type_args_lbracket_loc~,
    type_args_rbracket_loc~,
    type_arg_comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Type_Option(env, ty~, loc~, question_loc~) {
  MapVisitorBase::visit_Type_Option(env, ty~, loc~, question_loc~)
}

///|
impl MapVisitor with fn visit_Type_Object(env, constr_id~, amp_loc~, loc~) {
  MapVisitorBase::visit_Type_Object(env, constr_id~, amp_loc~, loc~)
}

///|
impl MapVisitor with fn visit_ErrorType(env, error_type) {
  MapVisitorBase::visit_ErrorType(env, error_type)
}

///|
impl MapVisitor with fn visit_ErrorType_ErrorType(env, ty~, raise_loc~, loc~) {
  MapVisitorBase::visit_ErrorType_ErrorType(env, ty~, raise_loc~, loc~)
}

///|
impl MapVisitor with fn visit_ErrorType_DefaultErrorType(env, loc~) {
  MapVisitorBase::visit_ErrorType_DefaultErrorType(env, loc~)
}

///|
impl MapVisitor with fn visit_ErrorType_Noraise(env, loc~) {
  MapVisitorBase::visit_ErrorType_Noraise(env, loc~)
}

///|
impl MapVisitor with fn visit_ErrorType_MaybeError(
  env,
  ty~,
  raise_loc~,
  question_loc~,
  loc~,
) {
  MapVisitorBase::visit_ErrorType_MaybeError(
    env,
    ty~,
    raise_loc~,
    question_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_ConstrParam(env, constr_param) {
  MapVisitorBase::visit_ConstrParam(env, constr_param)
}

///|
impl MapVisitor with fn visit_ConstrDecl(env, constr_decl) {
  MapVisitorBase::visit_ConstrDecl(env, constr_decl)
}

///|
impl MapVisitor with fn visit_ExceptionDecl(env, exception_decl) {
  MapVisitorBase::visit_ExceptionDecl(env, exception_decl)
}

///|
impl MapVisitor with fn visit_ExceptionDecl_EnumPayload(
  env,
  constrs~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  MapVisitorBase::visit_ExceptionDecl_EnumPayload(
    env,
    constrs~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
  )
}

///|
impl MapVisitor with fn visit_FieldName(env, field_name) {
  MapVisitorBase::visit_FieldName(env, field_name)
}

///|
impl MapVisitor with fn visit_FieldDecl(env, field_decl) {
  MapVisitorBase::visit_FieldDecl(env, field_decl)
}

///|
impl MapVisitor with fn visit_TypeDesc(env, type_desc) {
  MapVisitorBase::visit_TypeDesc(env, type_desc)
}

///|
impl MapVisitor with fn visit_TypeDesc_Error(env, exception_decl) {
  MapVisitorBase::visit_TypeDesc_Error(env, exception_decl)
}

///|
impl MapVisitor with fn visit_TypeDesc_Variant(
  env,
  constrs~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  MapVisitorBase::visit_TypeDesc_Variant(
    env,
    constrs~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
  )
}

///|
impl MapVisitor with fn visit_TypeDesc_Record(
  env,
  fields~,
  constr_decl~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  MapVisitorBase::visit_TypeDesc_Record(
    env,
    fields~,
    constr_decl~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
  )
}

///|
impl MapVisitor with fn visit_TypeDesc_TupleStruct(
  env,
  types~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
) {
  MapVisitorBase::visit_TypeDesc_TupleStruct(
    env,
    types~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
  )
}

///|
impl MapVisitor with fn visit_TypeDesc_Alias(env, type_~, equal_loc~) {
  MapVisitorBase::visit_TypeDesc_Alias(env, type_~, equal_loc~)
}

///|
impl MapVisitor with fn visit_Hole(env, hole) {
  MapVisitorBase::visit_Hole(env, hole)
}

///|
impl MapVisitor with fn visit_ArgumentKind(env, argument_kind) {
  MapVisitorBase::visit_ArgumentKind(env, argument_kind)
}

///|
impl MapVisitor with fn visit_ArgumentKind_Labelled(env, label~, equal_loc~) {
  MapVisitorBase::visit_ArgumentKind_Labelled(env, label~, equal_loc~)
}

///|
impl MapVisitor with fn visit_ArgumentKind_LabelledPun(env, label) {
  MapVisitorBase::visit_ArgumentKind_LabelledPun(env, label)
}

///|
impl MapVisitor with fn visit_ArgumentKind_LabelledOption(
  env,
  label~,
  question_loc~,
  equal_loc~,
) {
  MapVisitorBase::visit_ArgumentKind_LabelledOption(
    env,
    label~,
    question_loc~,
    equal_loc~,
  )
}

///|
impl MapVisitor with fn visit_ArgumentKind_LabelledOptionPun(
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
impl MapVisitor with fn visit_FnKind(env, fn_kind) {
  MapVisitorBase::visit_FnKind(env, fn_kind)
}

///|
impl MapVisitor with fn visit_Group(env, group) {
  MapVisitorBase::visit_Group(env, group)
}

///|
impl MapVisitor with fn visit_TrailingMark(env, trailing_mark) {
  MapVisitorBase::visit_TrailingMark(env, trailing_mark)
}

///|
impl MapVisitor with fn visit_TypeVarConstraint(env, type_var_constraint) {
  MapVisitorBase::visit_TypeVarConstraint(env, type_var_constraint)
}

///|
impl MapVisitor with fn visit_TypeVarBinder(env, type_var_binder) {
  MapVisitorBase::visit_TypeVarBinder(env, type_var_binder)
}

///|
impl MapVisitor with fn visit_TypeDeclBinder(env, type_decl_binder) {
  MapVisitorBase::visit_TypeDeclBinder(env, type_decl_binder)
}

///|
impl MapVisitor with fn visit_Binder(env, binder) {
  MapVisitorBase::visit_Binder(env, binder)
}

///|
impl MapVisitor with fn visit_Var(env, var_) {
  MapVisitorBase::visit_Var(env, var_)
}

///|
impl MapVisitor with fn visit_ConstructorExtraInfo(env, constructor_extra_info) {
  MapVisitorBase::visit_ConstructorExtraInfo(env, constructor_extra_info)
}

///|
impl MapVisitor with fn visit_ConstructorExtraInfo_TypeName(
  env,
  type_name~,
  coloncolon_loc~,
) {
  MapVisitorBase::visit_ConstructorExtraInfo_TypeName(
    env,
    type_name~,
    coloncolon_loc~,
  )
}

///|
impl MapVisitor with fn visit_ConstructorExtraInfo_Package(
  env,
  pkg~,
  pkg_loc~,
  dot_constr_loc~,
) {
  MapVisitorBase::visit_ConstructorExtraInfo_Package(
    env,
    pkg~,
    pkg_loc~,
    dot_constr_loc~,
  )
}

///|
impl MapVisitor with fn visit_Constructor(env, constructor_) {
  MapVisitorBase::visit_Constructor(env, constructor_)
}

///|
impl MapVisitor with fn visit_Accessor(env, accessor) {
  MapVisitorBase::visit_Accessor(env, accessor)
}

///|
impl MapVisitor with fn visit_Accessor_Label(env, label) {
  MapVisitorBase::visit_Accessor_Label(env, label)
}

///|
impl MapVisitor with fn visit_Accessor_Index(env, tuple_index~, loc~) {
  MapVisitorBase::visit_Accessor_Index(env, tuple_index~, loc~)
}

///|
impl MapVisitor with fn visit_Accessor_Newtype(env, loc~) {
  MapVisitorBase::visit_Accessor_Newtype(env, loc~)
}

///|
impl MapVisitor with fn visit_AliasTarget(env, alias_target) {
  MapVisitorBase::visit_AliasTarget(env, alias_target)
}

///|
impl MapVisitor with fn visit_Argument(env, argument) {
  MapVisitorBase::visit_Argument(env, argument)
}

///|
impl MapVisitor with fn visit_Parameter(env, parameter) {
  MapVisitorBase::visit_Parameter(env, parameter)
}

///|
impl MapVisitor with fn visit_Parameter_DiscardPositional(
  env,
  ty~,
  colon_loc~,
  loc~,
) {
  MapVisitorBase::visit_Parameter_DiscardPositional(env, ty~, colon_loc~, loc~)
}

///|
impl MapVisitor with fn visit_Parameter_Positional(
  env,
  binder~,
  ty~,
  colon_loc~,
) {
  MapVisitorBase::visit_Parameter_Positional(env, binder~, ty~, colon_loc~)
}

///|
impl MapVisitor with fn visit_Parameter_Labelled(
  env,
  binder~,
  ty~,
  post_label_loc~,
  colon_loc~,
) {
  MapVisitorBase::visit_Parameter_Labelled(
    env,
    binder~,
    ty~,
    post_label_loc~,
    colon_loc~,
  )
}

///|
impl MapVisitor with fn visit_Parameter_Optional(
  env,
  binder~,
  default~,
  ty~,
  post_label_loc~,
  question_loc~,
  colon_loc~,
  equal_loc~,
) {
  MapVisitorBase::visit_Parameter_Optional(
    env,
    binder~,
    default~,
    ty~,
    post_label_loc~,
    question_loc~,
    colon_loc~,
    equal_loc~,
  )
}

///|
impl MapVisitor with fn visit_Parameter_QuestionOptional(
  env,
  binder~,
  ty~,
  question_loc~,
  colon_loc~,
) {
  MapVisitorBase::visit_Parameter_QuestionOptional(
    env,
    binder~,
    ty~,
    question_loc~,
    colon_loc~,
  )
}

///|
impl MapVisitor with fn visit_Case(env, case) {
  MapVisitorBase::visit_Case(env, case)
}

///|
impl MapVisitor with fn visit_SpreadableElem(env, spreadable_elem) {
  MapVisitorBase::visit_SpreadableElem(env, spreadable_elem)
}

///|
impl MapVisitor with fn visit_SpreadableElem_Regular(env, expr) {
  MapVisitorBase::visit_SpreadableElem_Regular(env, expr)
}

///|
impl MapVisitor with fn visit_SpreadableElem_Spread(
  env,
  expr~,
  dotdot_loc~,
  loc~,
) {
  MapVisitorBase::visit_SpreadableElem_Spread(env, expr~, dotdot_loc~, loc~)
}

///|
impl MapVisitor with fn visit_MapExprElem(env, map_expr_elem) {
  MapVisitorBase::visit_MapExprElem(env, map_expr_elem)
}

///|
impl MapVisitor with fn visit_StaticAssertion(env, static_assertion) {
  MapVisitorBase::visit_StaticAssertion(env, static_assertion)
}

///|
impl MapVisitor with fn visit_Func(env, func) {
  MapVisitorBase::visit_Func(env, func)
}

///|
impl MapVisitor with fn visit_FieldDef(env, field_def) {
  MapVisitorBase::visit_FieldDef(env, field_def)
}

///|
impl MapVisitor with fn visit_InterpElem(env, interp_elem) {
  MapVisitorBase::visit_InterpElem(env, interp_elem)
}

///|
impl MapVisitor with fn visit_InterpElem_Literal(env, repr~, loc~) {
  MapVisitorBase::visit_InterpElem_Literal(env, repr~, loc~)
}

///|
impl MapVisitor with fn visit_InterpElem_Expr(env, expr~, loc~) {
  MapVisitorBase::visit_InterpElem_Expr(env, expr~, loc~)
}

///|
impl MapVisitor with fn visit_InterpElem_Source(env, interp_source) {
  MapVisitorBase::visit_InterpElem_Source(env, interp_source)
}

///|
impl MapVisitor with fn visit_MultilineStringElem(env, multiline_string_elem) {
  MapVisitorBase::visit_MultilineStringElem(env, multiline_string_elem)
}

///|
impl MapVisitor with fn visit_MultilineStringElem_String(env, string_val) {
  MapVisitorBase::visit_MultilineStringElem_String(env, string_val)
}

///|
impl MapVisitor with fn visit_MultilineStringElem_Interp(env, interp_elems) {
  MapVisitorBase::visit_MultilineStringElem_Interp(env, interp_elems)
}

///|
impl MapVisitor with fn visit_TryOperatorKind(env, try_operator_kind) {
  MapVisitorBase::visit_TryOperatorKind(env, try_operator_kind)
}

///|
impl MapVisitor with fn visit_DotDotBinder(env, dot_dot_binder) {
  MapVisitorBase::visit_DotDotBinder(env, dot_dot_binder)
}

///|
impl MapVisitor with fn visit_DotDotBinder_Underscore(
  env,
  dotdot_loc~,
  underscore_loc~,
) {
  MapVisitorBase::visit_DotDotBinder_Underscore(
    env,
    dotdot_loc~,
    underscore_loc~,
  )
}

///|
impl MapVisitor with fn visit_DotDotBinder_NoBinder(env, dotdot_loc~) {
  MapVisitorBase::visit_DotDotBinder_NoBinder(env, dotdot_loc~)
}

///|
impl MapVisitor with fn visit_DotDotBinder_BinderAs(
  env,
  binder~,
  dotdot_loc~,
  as_loc~,
) {
  MapVisitorBase::visit_DotDotBinder_BinderAs(
    env,
    binder~,
    dotdot_loc~,
    as_loc~,
  )
}

///|
impl MapVisitor with fn visit_DotDotBinder_Binder(env, binder~, dotdot_loc~) {
  MapVisitorBase::visit_DotDotBinder_Binder(env, binder~, dotdot_loc~)
}

///|
impl MapVisitor with fn visit_ArrayPattern(env, array_pattern) {
  MapVisitorBase::visit_ArrayPattern(env, array_pattern)
}

///|
impl MapVisitor with fn visit_ArrayPattern_Pattern(env, pattern) {
  MapVisitorBase::visit_ArrayPattern_Pattern(env, pattern)
}

///|
impl MapVisitor with fn visit_ArrayPattern_StringSpread(
  env,
  str~,
  dotdot_loc~,
  loc~,
) {
  MapVisitorBase::visit_ArrayPattern_StringSpread(env, str~, dotdot_loc~, loc~)
}

///|
impl MapVisitor with fn visit_ArrayPattern_BytesSpread(
  env,
  bytes~,
  dotdot_loc~,
  loc~,
) {
  MapVisitorBase::visit_ArrayPattern_BytesSpread(env, bytes~, dotdot_loc~, loc~)
}

///|
impl MapVisitor with fn visit_ArrayPattern_ConstSpread(
  env,
  binder~,
  pkg~,
  dotdot_loc~,
  pkg_loc~,
  dot_uident_loc~,
  loc~,
) {
  MapVisitorBase::visit_ArrayPattern_ConstSpread(
    env,
    binder~,
    pkg~,
    dotdot_loc~,
    pkg_loc~,
    dot_uident_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_ArrayPatterns(env, array_patterns) {
  MapVisitorBase::visit_ArrayPatterns(env, array_patterns)
}

///|
impl MapVisitor with fn visit_ArrayPatterns_Closed(
  env,
  array_patterns,
  comma_locs~,
) {
  MapVisitorBase::visit_ArrayPatterns_Closed(env, array_patterns, comma_locs~)
}

///|
impl MapVisitor with fn visit_ArrayPatterns_Open(
  env,
  prefix_patterns,
  suffix_patterns,
  dot_dot_binder,
  left_comma_locs~,
  dotdot_comma_loc~,
  right_comma_locs~,
) {
  MapVisitorBase::visit_ArrayPatterns_Open(
    env,
    prefix_patterns,
    suffix_patterns,
    dot_dot_binder,
    left_comma_locs~,
    dotdot_comma_loc~,
    right_comma_locs~,
  )
}

///|
impl MapVisitor with fn visit_WhereClause(env, where_clause) {
  MapVisitorBase::visit_WhereClause(env, where_clause)
}

///|
impl MapVisitor with fn visit_FieldPat(env, field_pat) {
  MapVisitorBase::visit_FieldPat(env, field_pat)
}

///|
impl MapVisitor with fn visit_ConstrPatArg(env, const_pat_arg) {
  MapVisitorBase::visit_ConstrPatArg(env, const_pat_arg)
}

///|
impl MapVisitor with fn visit_MapPatElem(env, map_pat_elem) {
  MapVisitorBase::visit_MapPatElem(env, map_pat_elem)
}

///|
impl MapVisitor with fn visit_RangeKind(env, kind) {
  MapVisitorBase::visit_RangeKind(env, kind)
}

///|
impl MapVisitor with fn visit_Pattern(env, pattern) {
  MapVisitorBase::visit_Pattern(env, pattern)
}

///|
impl MapVisitor with fn visit_Pattern_Alias(env, pat~, alias_~, as_loc~, loc~) {
  MapVisitorBase::visit_Pattern_Alias(env, pat~, alias_~, as_loc~, loc~)
}

///|
impl MapVisitor with fn visit_Pattern_Any(env, loc~) {
  MapVisitorBase::visit_Pattern_Any(env, loc~)
}

///|
impl MapVisitor with fn visit_Pattern_Array(
  env,
  pats~,
  lbracket_loc~,
  rbracket_loc~,
  loc~,
) {
  MapVisitorBase::visit_Pattern_Array(
    env,
    pats~,
    lbracket_loc~,
    rbracket_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Pattern_Constant(env, c~, loc~) {
  MapVisitorBase::visit_Pattern_Constant(env, c~, loc~)
}

///|
impl MapVisitor with fn visit_Pattern_Constraint(
  env,
  pat~,
  ty~,
  lparen_loc~,
  colon_loc~,
  rparen_loc~,
  loc~,
) {
  MapVisitorBase::visit_Pattern_Constraint(
    env,
    pat~,
    ty~,
    lparen_loc~,
    colon_loc~,
    rparen_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Pattern_Constr(
  env,
  constr~,
  args~,
  is_open~,
  args_lparen_loc~,
  args_rparen_loc~,
  arg_comma_locs~,
  open_dotdot_loc~,
  loc~,
) {
  MapVisitorBase::visit_Pattern_Constr(
    env,
    constr~,
    args~,
    is_open~,
    args_lparen_loc~,
    args_rparen_loc~,
    arg_comma_locs~,
    open_dotdot_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Pattern_Or(env, pat1~, pat2~, bar_loc~, loc~) {
  MapVisitorBase::visit_Pattern_Or(env, pat1~, pat2~, bar_loc~, loc~)
}

///|
impl MapVisitor with fn visit_Pattern_Tuple(
  env,
  pats~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Pattern_Tuple(
    env,
    pats~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Pattern_Var(env, binder) {
  MapVisitorBase::visit_Pattern_Var(env, binder)
}

///|
impl MapVisitor with fn visit_Pattern_Record(
  env,
  fields~,
  is_closed~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  dotdot_loc~,
  loc~,
) {
  MapVisitorBase::visit_Pattern_Record(
    env,
    fields~,
    is_closed~,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs~,
    dotdot_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Pattern_Map(
  env,
  elems~,
  is_closed~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  dotdot_loc~,
  loc~,
) {
  MapVisitorBase::visit_Pattern_Map(
    env,
    elems~,
    is_closed~,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs~,
    dotdot_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Pattern_Range(
  env,
  lhs~,
  rhs~,
  kind~,
  range_op_loc~,
  loc~,
) {
  MapVisitorBase::visit_Pattern_Range(
    env,
    lhs~,
    rhs~,
    kind~,
    range_op_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Pattern_SpecialConstr(
  env,
  binder~,
  args~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  MapVisitorBase::visit_Pattern_SpecialConstr(
    env,
    binder~,
    args~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_DerivingDirective(env, deriving_directive) {
  MapVisitorBase::visit_DerivingDirective(env, deriving_directive)
}

///|
impl MapVisitor with fn visit_TypeDecl(env, type_decl) {
  MapVisitorBase::visit_TypeDecl(env, type_decl)
}

///|
impl MapVisitor with fn visit_FuncStubs(env, func_stubs) {
  MapVisitorBase::visit_FuncStubs(env, func_stubs)
}

///|
impl MapVisitor with fn visit_FuncStubs_Import(
  env,
  module_name~,
  func_name~,
  language~,
  module_name_loc~,
  func_name_loc~,
  language_loc~,
) {
  MapVisitorBase::visit_FuncStubs_Import(
    env,
    module_name~,
    func_name~,
    language~,
    module_name_loc~,
    func_name_loc~,
    language_loc~,
  )
}

///|
impl MapVisitor with fn visit_FuncStubs_Embedded(
  env,
  language~,
  code~,
  language_loc~,
  code_loc~,
) {
  MapVisitorBase::visit_FuncStubs_Embedded(
    env,
    language~,
    code~,
    language_loc~,
    code_loc~,
  )
}

///|
impl MapVisitor with fn visit_EmbeddedCode(env, embedded_code) {
  MapVisitorBase::visit_EmbeddedCode(env, embedded_code)
}

///|
impl MapVisitor with fn visit_EmbeddedCode_CodeString(env, string_literal, loc~) {
  MapVisitorBase::visit_EmbeddedCode_CodeString(env, string_literal, loc~)
}

///|
impl MapVisitor with fn visit_EmbeddedCode_CodeMultilineString(
  env,
  strings,
  string_locs~,
) {
  MapVisitorBase::visit_EmbeddedCode_CodeMultilineString(
    env,
    strings,
    string_locs~,
  )
}

///|
impl MapVisitor with fn visit_DeclBody(env, decl_body) {
  MapVisitorBase::visit_DeclBody(env, decl_body)
}

///|
impl MapVisitor with fn visit_DeclBody_DeclBody(
  env,
  expr~,
  lbrace_loc~,
  rbrace_loc~,
) {
  MapVisitorBase::visit_DeclBody_DeclBody(env, expr~, lbrace_loc~, rbrace_loc~)
}

///|
impl MapVisitor with fn visit_DeclBody_DeclStubs(env, func_stubs, equal_loc~) {
  MapVisitorBase::visit_DeclBody_DeclStubs(env, func_stubs, equal_loc~)
}

///|
impl MapVisitor with fn visit_FunDecl(env, fun_decl) {
  MapVisitorBase::visit_FunDecl(env, fun_decl)
}

///|
impl MapVisitor with fn visit_TraitMethodDecl(env, trait_method_decl) {
  MapVisitorBase::visit_TraitMethodDecl(env, trait_method_decl)
}

///|
impl MapVisitor with fn visit_TraitDecl(env, trait_decl) {
  MapVisitorBase::visit_TraitDecl(env, trait_decl)
}

///|
impl MapVisitor with fn visit_Expr_IsLexMatch(
  env,
  expr~ : Expr,
  strategy~,
  pat~,
  pat_loc~,
  lexmatch_question_loc~,
  with_loc~,
  loc~,
) {
  MapVisitorBase::visit_Expr_IsLexMatch(
    env,
    expr~,
    strategy~,
    pat~,
    pat_loc~,
    lexmatch_question_loc~,
    with_loc~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_Expr_LexMatch(
  env,
  strategy~,
  expr~,
  match_loc~,
  with_loc~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
  cases~,
  loc~,
) {
  MapVisitorBase::visit_Expr_LexMatch(
    env,
    strategy~,
    expr~,
    match_loc~,
    with_loc~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
    cases~,
    loc~,
  )
}

///|
impl MapVisitor with fn visit_LexCase(env, lex_case) {
  MapVisitorBase::visit_LexCase(env, lex_case)
}

///|
impl MapVisitor with fn visit_LexTopPattern(env, lex_top_pattern) {
  MapVisitorBase::visit_LexTopPattern(env, lex_top_pattern)
}

///|
impl MapVisitor with fn visit_LexTopPattern_Pattern(env, lex_pattern) {
  MapVisitorBase::visit_LexTopPattern_Pattern(env, lex_pattern)
}

///|
impl MapVisitor with fn visit_LexTopPattern_Binder(env, binder) {
  MapVisitorBase::visit_LexTopPattern_Binder(env, binder)
}

///|
impl MapVisitor with fn visit_LexTopPattern_Wildcard(env, loc~) {
  MapVisitorBase::visit_LexTopPattern_Wildcard(env, loc~)
}

///|
impl MapVisitor with fn visit_LexPattern(env, lex_pattern) {
  MapVisitorBase::visit_LexPattern(env, lex_pattern)
}

///|
impl MapVisitor with fn visit_LexPattern_Regex(env, lit~, offset~, loc~) {
  MapVisitorBase::visit_LexPattern_Regex(env, lit~, offset~, loc~)
}

///|
impl MapVisitor with fn visit_LexPattern_RegexInterp(env, elems~, loc~) {
  MapVisitorBase::visit_LexPattern_RegexInterp(env, elems~, loc~)
}

///|
impl MapVisitor with fn visit_LexPattern_ConstantRef(env, lid~, loc~) {
  MapVisitorBase::visit_LexPattern_ConstantRef(env, lid~, loc~)
}

///|
impl MapVisitor with fn visit_LexPattern_Alias(
  env,
  pat~,
  binder~,
  as_loc~,
  loc~,
) {
  MapVisitorBase::visit_LexPattern_Alias(env, pat~, binder~, as_loc~, loc~)
}

///|
impl MapVisitor with fn visit_LexPattern_Sequence(env, pats~, semi_locs~, loc~) {
  MapVisitorBase::visit_LexPattern_Sequence(env, pats~, semi_locs~, loc~)
}

///|
pub(all) struct MapVisitorBase[T](T)

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl(
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
      enumview_loc~,
      type_params_lbracket_loc~,
      type_params_rbracket_loc~,
      type_param_comma_locs~,
      lbrace_loc~,
      rbrace_loc~,
      constructor_semi_locs~,
      for_loc~,
      with_loc~,
      params_lparen_loc~,
      params_rparen_loc~,
      param_comma_locs~,
      body_lbrace_loc~,
      body_rbrace_loc~,
      attrs~,
      loc~,
      doc~
    ) =>
      env.0.visit_Impl_TopView(
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
        enumview_loc~,
        type_params_lbracket_loc~,
        type_params_rbracket_loc~,
        type_param_comma_locs~,
        lbrace_loc~,
        rbrace_loc~,
        constructor_semi_locs~,
        for_loc~,
        with_loc~,
        params_lparen_loc~,
        params_rparen_loc~,
        param_comma_locs~,
        body_lbrace_loc~,
        body_rbrace_loc~,
        loc~,
        attrs~,
        doc~,
      )
    TopImpl(
      self_ty~,
      trait_~,
      method_name~,
      quantifiers~,
      params~,
      ret_ty~,
      err_ty~,
      body~,
      vis~,
      impl_loc~,
      type_params_lbracket_loc~,
      type_params_rbracket_loc~,
      type_param_comma_locs~,
      for_loc~,
      with_loc~,
      fn_loc~,
      params_lparen_loc~,
      params_rparen_loc~,
      param_comma_locs~,
      return_arrow_loc~,
      loc~,
      attrs~,
      doc~
    ) =>
      env.0.visit_Impl_TopImpl(
        self_ty~,
        trait_~,
        method_name~,
        quantifiers~,
        params~,
        ret_ty~,
        err_ty~,
        body~,
        vis~,
        impl_loc~,
        type_params_lbracket_loc~,
        type_params_rbracket_loc~,
        type_param_comma_locs~,
        for_loc~,
        with_loc~,
        fn_loc~,
        params_lparen_loc~,
        params_rparen_loc~,
        param_comma_locs~,
        return_arrow_loc~,
        loc~,
        attrs~,
        doc~,
      )
    TopTrait(trait_decl) => env.0.visit_Impl_TopTrait(trait_decl)
    TopLetDef(
      binder~,
      ty~,
      expr~,
      vis~,
      is_constant~,
      declare_loc~,
      let_or_const_loc~,
      annot_colon_loc~,
      equal_loc~,
      loc~,
      attrs~,
      doc~,
      is_declare~
    ) =>
      env.0.visit_Impl_TopLetDef(
        binder~,
        ty~,
        expr~,
        vis~,
        is_constant~,
        declare_loc~,
        let_or_const_loc~,
        annot_colon_loc~,
        equal_loc~,
        loc~,
        attrs~,
        doc~,
        is_declare~,
      )
    TopFuncDef(fun_decl~, decl_body~, where_clause~, equal_loc~, loc~) =>
      env.0.visit_Impl_TopFuncDef(
        fun_decl~,
        decl_body~,
        where_clause~,
        equal_loc~,
        loc~,
      )
    TopTypeDef(type_decl) => env.0.visit_Impl_TopTypeDef(type_decl)
    TopTest(
      expr~,
      name~,
      params~,
      is_async~,
      test_loc~,
      params_lparen_loc~,
      params_rparen_loc~,
      param_comma_locs~,
      body_lbrace_loc~,
      body_rbrace_loc~,
      loc~,
      attrs~,
      doc~
    ) =>
      env.0.visit_Impl_TopTest(
        expr~,
        name~,
        params~,
        is_async~,
        test_loc~,
        params_lparen_loc~,
        params_rparen_loc~,
        param_comma_locs~,
        body_lbrace_loc~,
        body_rbrace_loc~,
        loc~,
        attrs~,
        doc~,
      )
    TopExpr(expr~, is_main~, is_async~, loc~) =>
      env.0.visit_Impl_TopExpr(expr~, is_main~, is_async~, loc~)
    TopImplRelation(
      self_ty~,
      trait_~,
      quantifiers~,
      vis~,
      attrs~,
      declare_loc~,
      impl_loc~,
      type_params_lbracket_loc~,
      type_params_rbracket_loc~,
      type_param_comma_locs~,
      for_loc~,
      loc~,
      doc~,
      is_declare~
    ) =>
      env.0.visit_Impl_TopImplRelation(
        self_ty~,
        trait_~,
        quantifiers~,
        vis~,
        attrs~,
        declare_loc~,
        impl_loc~,
        type_params_lbracket_loc~,
        type_params_rbracket_loc~,
        type_param_comma_locs~,
        for_loc~,
        loc~,
        doc~,
        is_declare~,
      )
    TopUsing(
      pkg~,
      names~,
      vis~,
      attrs~,
      using_loc~,
      lbrace_loc~,
      rbrace_loc~,
      comma_locs~,
      loc~,
      doc~
    ) =>
      env.0.visit_Impl_TopUsing(
        pkg~,
        names~,
        vis~,
        attrs~,
        using_loc~,
        lbrace_loc~,
        rbrace_loc~,
        comma_locs~,
        loc~,
        doc~,
      )
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Visibility(
  env,
  visibility,
) {
  match visibility {
    Default => Default
    Pub(attr~, loc~) => env.0.visit_Visibility_Pub(attr~, loc~)
    Priv(loc~) => env.0.visit_Visibility_Priv(loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Visibility_Pub(
  _,
  attr~,
  loc~,
) {
  Pub(attr~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Visibility_Priv(
  _,
  loc~,
) {
  Priv(loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Constant(
  _,
  constant,
) {
  constant
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Label(
  _,
  label,
) {
  label
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ConstrName(
  _,
  constr_name,
) {
  constr_name
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LongIdent(
  env,
  long_ident,
) {
  match long_ident {
    Ident(name~) => env.0.visit_LongIdent_Ident(name~)
    Dot(pkg~, id~, pkg_loc~, dot_id_loc~) =>
      env.0.visit_LongIdent_Dot(pkg~, id~, pkg_loc~, dot_id_loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LongIdent_Ident(
  _,
  name~,
) {
  Ident(name~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LongIdent_Dot(
  _,
  pkg~,
  id~,
  pkg_loc~,
  dot_id_loc~,
) {
  Dot(pkg~, id~, pkg_loc~, dot_id_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeName(
  env,
  type_name,
) {
  let name = env.0.visit_LongIdent(type_name.name)
  let is_object = type_name.is_object
  let object_amp_loc = type_name.object_amp_loc
  let loc = type_name.loc
  TypeName::{ name, is_object, object_amp_loc, loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ConstrId(
  env,
  constr_id,
) {
  let id = env.0.visit_LongIdent(constr_id.id)
  let loc = constr_id.loc
  ConstrId::{ id, loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Type(
  env,
  type_,
) {
  match type_ {
    Any(loc~) => env.0.visit_Type_Any(loc~)
    Arrow(
      args~,
      res~,
      err~,
      is_async~,
      lparen_loc~,
      rparen_loc~,
      arrow_loc~,
      comma_locs~,
      loc~
    ) =>
      env.0.visit_Type_Arrow(
        args~,
        res~,
        err~,
        is_async~,
        lparen_loc~,
        rparen_loc~,
        arrow_loc~,
        comma_locs~,
        loc~,
      )
    Tuple(tys~, lparen_loc~, rparen_loc~, comma_locs~, loc~) =>
      env.0.visit_Type_Tuple(tys~, lparen_loc~, rparen_loc~, comma_locs~, loc~)
    Name(
      constr_id~,
      tys~,
      type_args_lbracket_loc~,
      type_args_rbracket_loc~,
      type_arg_comma_locs~,
      loc~
    ) =>
      env.0.visit_Type_Name(
        constr_id~,
        tys~,
        type_args_lbracket_loc~,
        type_args_rbracket_loc~,
        type_arg_comma_locs~,
        loc~,
      )
    Option(ty~, loc~, question_loc~) =>
      env.0.visit_Type_Option(ty~, loc~, question_loc~)
    Object(constr_id~, amp_loc~, loc~) =>
      env.0.visit_Type_Object(constr_id~, amp_loc~, loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Type_Any(
  _,
  loc~,
) {
  Any(loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Type_Arrow(
  env,
  args~,
  res~,
  err~,
  is_async~,
  lparen_loc~,
  rparen_loc~,
  arrow_loc~,
  comma_locs~,
  loc~,
) {
  let args = args.map(x => env.0.visit_Type(x))
  let res = env.0.visit_Type(res)
  let err = env.0.visit_ErrorType(err)
  Arrow(
    args~,
    res~,
    err~,
    is_async~,
    lparen_loc~,
    rparen_loc~,
    arrow_loc~,
    comma_locs~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Type_Tuple(
  env,
  tys~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  let tys = tys.map(x => env.0.visit_Type(x))
  Tuple(tys~, lparen_loc~, rparen_loc~, comma_locs~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Type_Name(
  env,
  constr_id~,
  tys~,
  type_args_lbracket_loc~,
  type_args_rbracket_loc~,
  type_arg_comma_locs~,
  loc~,
) {
  let constr_id = env.0.visit_ConstrId(constr_id)
  let tys = tys.map(x => env.0.visit_Type(x))
  Name(
    constr_id~,
    tys~,
    type_args_lbracket_loc~,
    type_args_rbracket_loc~,
    type_arg_comma_locs~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Type_Option(
  env,
  ty~,
  loc~,
  question_loc~,
) {
  let ty = env.0.visit_Type(ty)
  Option(ty~, loc~, question_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Type_Object(
  env,
  constr_id~,
  amp_loc~,
  loc~,
) {
  let constr_id = env.0.visit_ConstrId(constr_id)
  Object(constr_id~, amp_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ErrorType(
  env,
  error_type,
) {
  match error_type {
    ErrorType(ty~, raise_loc~, loc~) =>
      env.0.visit_ErrorType_ErrorType(ty~, raise_loc~, loc~)
    DefaultErrorType(loc~) => env.0.visit_ErrorType_DefaultErrorType(loc~)
    NoErrorType => NoErrorType
    Noraise(loc~) => env.0.visit_ErrorType_Noraise(loc~)
    MaybeError(ty~, raise_loc~, question_loc~, loc~) =>
      env.0.visit_ErrorType_MaybeError(ty~, raise_loc~, question_loc~, loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ErrorType_ErrorType(
  env,
  ty~,
  raise_loc~,
  loc~,
) {
  let ty = env.0.visit_Type(ty)
  ErrorType(ty~, raise_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ErrorType_DefaultErrorType(
  _,
  loc~,
) {
  DefaultErrorType(loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ErrorType_Noraise(
  _,
  loc~,
) {
  Noraise(loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ErrorType_MaybeError(
  env,
  ty~,
  raise_loc~,
  question_loc~,
  loc~,
) {
  let ty = env.0.visit_Type(ty)
  MaybeError(ty~, raise_loc~, question_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ConstrParam(
  env,
  constr_param,
) {
  let ty = env.0.visit_Type(constr_param.ty)
  let label = constr_param.label.map(x => env.0.visit_Label(x))
  let mut_ = constr_param.mut_
  let mut_loc = constr_param.mut_loc
  let post_label_loc = constr_param.post_label_loc
  let colon_loc = constr_param.colon_loc
  ConstrParam::{ ty, mut_, label, mut_loc, post_label_loc, colon_loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ConstrDecl(
  env,
  constr_decl,
) {
  let name = env.0.visit_ConstrName(constr_decl.name)
  let args = constr_decl.args.map(args => {
    args.map(x => env.0.visit_ConstrParam(x))
  })
  let args_lparen_loc = constr_decl.args_lparen_loc
  let args_rparen_loc = constr_decl.args_rparen_loc
  let arg_comma_locs = constr_decl.arg_comma_locs
  let tag = constr_decl.tag
  let tag_equal_loc = constr_decl.tag_equal_loc
  let loc = constr_decl.loc
  let attrs = constr_decl.attrs
  let doc = constr_decl.doc
  ConstrDecl::{
    name,
    args,
    args_lparen_loc,
    args_rparen_loc,
    arg_comma_locs,
    tag,
    tag_equal_loc,
    loc,
    attrs,
    doc,
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ExceptionDecl(
  env,
  exception_decl,
) {
  match exception_decl {
    NoPayload => NoPayload
    EnumPayload(constrs~, lbrace_loc~, rbrace_loc~, semi_locs~) =>
      env.0.visit_ExceptionDecl_EnumPayload(
        constrs~,
        lbrace_loc~,
        rbrace_loc~,
        semi_locs~,
      )
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ExceptionDecl_EnumPayload(
  env,
  constrs~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  let constrs = constrs.map(x => env.0.visit_ConstrDecl(x))
  EnumPayload(constrs~, lbrace_loc~, rbrace_loc~, semi_locs~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_FieldName(
  _,
  fieldname,
) {
  fieldname
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_FieldDecl(
  env,
  field_decl,
) {
  let name = env.0.visit_FieldName(field_decl.name)
  let ty = env.0.visit_Type(field_decl.ty)
  let vis = env.0.visit_Visibility(field_decl.vis)
  let mut_ = field_decl.mut_
  let mut_loc = field_decl.mut_loc
  let colon_loc = field_decl.colon_loc
  let loc = field_decl.loc
  let attrs = field_decl.attrs
  let doc = field_decl.doc
  FieldDecl::{ name, ty, mut_, mut_loc, colon_loc, vis, loc, attrs, doc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeDesc(
  env,
  type_desc,
) {
  match type_desc {
    Abstract => Abstract
    ExtensibleEnum(constrs~, lbrace_loc~, rbrace_loc~, semi_locs~) =>
      ExtensibleEnum(
        constrs=constrs.map(x => env.0.visit_ConstrDecl(x)),
        lbrace_loc~,
        rbrace_loc~,
        semi_locs~,
      )
    Error(exception_decl) => env.0.visit_TypeDesc_Error(exception_decl)
    Variant(constrs~, lbrace_loc~, rbrace_loc~, semi_locs~) =>
      env.0.visit_TypeDesc_Variant(
        constrs~,
        lbrace_loc~,
        rbrace_loc~,
        semi_locs~,
      )
    ExtendEnum(
      target~,
      constructors~,
      plus_equal_loc~,
      lbrace_loc~,
      rbrace_loc~,
      semi_locs~
    ) => {
      let target = env.0.visit_TypeName(target)
      let constructors = constructors.map(x => env.0.visit_ConstrDecl(x))
      ExtendEnum(
        target~,
        constructors~,
        plus_equal_loc~,
        lbrace_loc~,
        rbrace_loc~,
        semi_locs~,
      )
    }
    Record(fields~, constr_decl~, lbrace_loc~, rbrace_loc~, semi_locs~) =>
      env.0.visit_TypeDesc_Record(
        fields~,
        constr_decl~,
        lbrace_loc~,
        rbrace_loc~,
        semi_locs~,
      )
    TupleStruct(types~, lparen_loc~, rparen_loc~, comma_locs~) =>
      env.0.visit_TypeDesc_TupleStruct(
        types~,
        lparen_loc~,
        rparen_loc~,
        comma_locs~,
      )
    Alias(type_~, equal_loc~) => env.0.visit_TypeDesc_Alias(type_~, equal_loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeDesc_Error(
  env,
  exception_decl,
) {
  let exception_decl = env.0.visit_ExceptionDecl(exception_decl)
  Error(exception_decl)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeDesc_Variant(
  env,
  constrs~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  let constrs = constrs.map(x => env.0.visit_ConstrDecl(x))
  Variant(constrs~, lbrace_loc~, rbrace_loc~, semi_locs~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeDesc_Record(
  env,
  fields~,
  constr_decl~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  let fields = fields.map(x => env.0.visit_FieldDecl(x))
  let constr_decl = constr_decl.map(x => env.0.visit_FunDecl(x))
  Record(fields~, constr_decl~, lbrace_loc~, rbrace_loc~, semi_locs~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeDesc_TupleStruct(
  env,
  types~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
) {
  let types = types.map(x => env.0.visit_Type(x))
  TupleStruct(types~, lparen_loc~, rparen_loc~, comma_locs~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeDesc_Alias(
  env,
  type_~,
  equal_loc~,
) {
  let type_ = env.0.visit_Type(type_)
  Alias(type_~, equal_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Hole(
  _,
  hole,
) {
  return hole
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArgumentKind(
  env,
  argument_kind,
) {
  match argument_kind {
    Positional => Positional
    Labelled(label~, equal_loc~) =>
      env.0.visit_ArgumentKind_Labelled(label~, equal_loc~)
    LabelledPun(label) => env.0.visit_ArgumentKind_LabelledPun(label)
    LabelledOption(label~, question_loc~, equal_loc~) =>
      env.0.visit_ArgumentKind_LabelledOption(label~, question_loc~, equal_loc~)
    LabelledOptionPun(label~, question_loc~) =>
      env.0.visit_ArgumentKind_LabelledOptionPun(label~, question_loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArgumentKind_Labelled(
  _,
  label~,
  equal_loc~,
) {
  Labelled(label~, equal_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArgumentKind_LabelledPun(
  _,
  label,
) {
  LabelledPun(label)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArgumentKind_LabelledOption(
  _,
  label~,
  question_loc~,
  equal_loc~,
) {
  LabelledOption(label~, question_loc~, equal_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArgumentKind_LabelledOptionPun(
  _,
  label~,
  question_loc~,
) {
  LabelledOptionPun(label~, question_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_FnKind(
  _,
  fn_kind,
) {
  fn_kind
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Group(
  _,
  group,
) {
  group
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TrailingMark(
  _,
  trailing_mark,
) {
  trailing_mark
}

// Continue with remaining MapVisitorBase implementations that are missing

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeVarConstraint(
  env,
  type_var_constraint,
) {
  let trait_ = env.0.visit_LongIdent(type_var_constraint.trait_)
  let loc = type_var_constraint.loc
  TypeVarConstraint::{ trait_, loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeVarBinder(
  env,
  type_var_binder,
) {
  let constraints = type_var_binder.constraints.map(x => {
    env.0.visit_TypeVarConstraint(x)
  })
  let name = type_var_binder.name
  let loc = type_var_binder.name_loc
  let colon_loc = type_var_binder.colon_loc
  let plus_locs = type_var_binder.plus_locs
  TypeVarBinder::{ name, constraints, name_loc: loc, colon_loc, plus_locs }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeDeclBinder(
  _,
  type_decl_binder,
) {
  type_decl_binder
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Binder(
  _,
  binder,
) {
  binder
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Var(
  env,
  var_,
) {
  let name = env.0.visit_LongIdent(var_.name)
  let loc = var_.loc
  Var::{ name, loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ConstructorExtraInfo(
  env,
  constructor_extra_info,
) {
  match constructor_extra_info {
    TypeName(type_name~, coloncolon_loc~) =>
      env.0.visit_ConstructorExtraInfo_TypeName(type_name~, coloncolon_loc~)
    TypeNameWithConstrPackage(
      type_name~,
      pkg~,
      coloncolon_loc~,
      pkg_loc~,
      dot_constr_loc~
    ) => {
      let type_name = env.0.visit_TypeName(type_name)
      TypeNameWithConstrPackage(
        type_name~,
        pkg~,
        coloncolon_loc~,
        pkg_loc~,
        dot_constr_loc~,
      )
    }
    Package(pkg~, pkg_loc~, dot_constr_loc~) =>
      env.0.visit_ConstructorExtraInfo_Package(pkg~, pkg_loc~, dot_constr_loc~)
    NoExtraInfo => NoExtraInfo
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ConstructorExtraInfo_TypeName(
  env,
  type_name~,
  coloncolon_loc~,
) {
  let type_name = env.0.visit_TypeName(type_name)
  TypeName(type_name~, coloncolon_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ConstructorExtraInfo_Package(
  _,
  pkg~,
  pkg_loc~,
  dot_constr_loc~,
) {
  Package(pkg~, pkg_loc~, dot_constr_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Constructor(
  env,
  constructor_,
) {
  let name = env.0.visit_ConstrName(constructor_.name)
  let extra_info = env.0.visit_ConstructorExtraInfo(constructor_.extra_info)
  let loc = constructor_.loc
  Constructor::{ name, extra_info, loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Accessor(
  env,
  accessor,
) {
  match accessor {
    Label(label) => env.0.visit_Accessor_Label(label)
    Index(tuple_index~, loc~) => env.0.visit_Accessor_Index(tuple_index~, loc~)
    Newtype(loc~) => env.0.visit_Accessor_Newtype(loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Accessor_Label(
  env,
  label,
) {
  let label = env.0.visit_Label(label)
  Label(label)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Accessor_Index(
  _,
  tuple_index~,
  loc~,
) {
  Index(tuple_index~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Accessor_Newtype(
  _,
  loc~,
) {
  Newtype(loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_AliasTarget(
  env,
  alias_target,
) {
  let binder = env.0.visit_Binder(alias_target.binder)
  let target = alias_target.target
  let as_loc = alias_target.as_loc
  AliasTarget::{ binder, target, as_loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Argument(
  env,
  argument,
) {
  let value = env.0.visit_Expr(argument.value)
  let kind = env.0.visit_ArgumentKind(argument.kind)
  Argument::{ value, kind }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Parameter(
  env,
  parameter,
) {
  match parameter {
    DiscardPositional(ty~, colon_loc~, loc~) =>
      env.0.visit_Parameter_DiscardPositional(ty~, colon_loc~, loc~)
    Positional(binder~, ty~, colon_loc~) =>
      env.0.visit_Parameter_Positional(binder~, ty~, colon_loc~)
    Labelled(binder~, ty~, post_label_loc~, colon_loc~) =>
      env.0.visit_Parameter_Labelled(binder~, ty~, post_label_loc~, colon_loc~)
    Optional(
      binder~,
      default~,
      ty~,
      post_label_loc~,
      question_loc~,
      colon_loc~,
      equal_loc~
    ) =>
      env.0.visit_Parameter_Optional(
        binder~,
        default~,
        ty~,
        post_label_loc~,
        question_loc~,
        colon_loc~,
        equal_loc~,
      )
    QuestionOptional(binder~, ty~, question_loc~, colon_loc~) =>
      env.0.visit_Parameter_QuestionOptional(
        binder~,
        ty~,
        question_loc~,
        colon_loc~,
      )
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Parameter_DiscardPositional(
  env,
  ty~,
  colon_loc~,
  loc~,
) {
  let ty = ty.map(x => env.0.visit_Type(x))
  DiscardPositional(ty~, colon_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Parameter_Positional(
  env,
  binder~,
  ty~,
  colon_loc~,
) {
  let binder = env.0.visit_Binder(binder)
  let ty = ty.map(x => env.0.visit_Type(x))
  Positional(binder~, ty~, colon_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Parameter_Labelled(
  env,
  binder~,
  ty~,
  post_label_loc~,
  colon_loc~,
) {
  let binder = env.0.visit_Binder(binder)
  let ty = ty.map(x => env.0.visit_Type(x))
  Labelled(binder~, ty~, post_label_loc~, colon_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Parameter_Optional(
  env,
  binder~,
  default~,
  ty~,
  post_label_loc~,
  question_loc~,
  colon_loc~,
  equal_loc~,
) {
  let binder = env.0.visit_Binder(binder)
  let default = env.0.visit_Expr(default)
  let ty = ty.map(x => env.0.visit_Type(x))
  Optional(
    binder~,
    default~,
    ty~,
    post_label_loc~,
    question_loc~,
    colon_loc~,
    equal_loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Parameter_QuestionOptional(
  env,
  binder~,
  ty~,
  question_loc~,
  colon_loc~,
) {
  let binder = env.0.visit_Binder(binder)
  let ty = ty.map(x => env.0.visit_Type(x))
  QuestionOptional(binder~, ty~, question_loc~, colon_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Case(
  env,
  case,
) {
  let pattern = env.0.visit_Pattern(case.pattern)
  let guard_ = case.guard_.map(x => env.0.visit_Expr(x))
  let body = env.0.visit_Expr(case.body)
  let if_loc = case.if_loc
  let fat_arrow_loc = case.fat_arrow_loc
  let ellipsis_loc = case.ellipsis_loc
  Case::{ pattern, guard_, body, if_loc, fat_arrow_loc, ellipsis_loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_SpreadableElem(
  env,
  spreadable_elem,
) {
  match spreadable_elem {
    Regular(expr) => env.0.visit_SpreadableElem_Regular(expr)
    Spread(expr~, dotdot_loc~, loc~) =>
      env.0.visit_SpreadableElem_Spread(expr~, dotdot_loc~, loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_SpreadableElem_Regular(
  env,
  expr,
) {
  let expr = env.0.visit_Expr(expr)
  Regular(expr)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_SpreadableElem_Spread(
  env,
  expr~,
  dotdot_loc~,
  loc~,
) {
  let expr = env.0.visit_Expr(expr)
  Spread(expr~, dotdot_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_MapExprElem(
  env,
  mapexpr_elem,
) {
  let key = env.0.visit_Constant(mapexpr_elem.key)
  let expr = env.0.visit_Expr(mapexpr_elem.expr)
  let colon_loc = mapexpr_elem.colon_loc
  let key_loc = mapexpr_elem.key_loc
  let loc = mapexpr_elem.loc
  MapExprElem::{ key, expr, colon_loc, key_loc, loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_StaticAssertion(
  env,
  static_assertion,
) {
  let ty = env.0.visit_Type(static_assertion.ty)
  let trait_ = env.0.visit_LongIdent(static_assertion.trait_)
  let loc = static_assertion.loc
  let msg = static_assertion.msg
  StaticAssertion::{ ty, trait_, loc, msg }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Func(
  env,
  func,
) {
  let {
    is_async,
    parameters,
    params_loc,
    body,
    return_type,
    error_type,
    kind,
    fn_loc,
    lparen_loc,
    rparen_loc,
    comma_locs,
    return_arrow_loc,
    fat_arrow_loc,
    body_lbrace_loc,
    body_rbrace_loc,
    loc,
  } = func
  let parameters = parameters.map(p => env.0.visit_Parameter(p))
  let body = env.0.visit_Expr(body)
  let return_type = return_type.map(t => env.0.visit_Type(t))
  let error_type = env.0.visit_ErrorType(error_type)
  let kind = env.0.visit_FnKind(kind)
  {
    is_async,
    parameters,
    params_loc,
    body,
    return_type,
    error_type,
    kind,
    fn_loc,
    lparen_loc,
    rparen_loc,
    comma_locs,
    return_arrow_loc,
    fat_arrow_loc,
    body_lbrace_loc,
    body_rbrace_loc,
    loc,
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_FieldDef(
  env,
  field_def,
) {
  let label = env.0.visit_Label(field_def.label)
  let expr = env.0.visit_Expr(field_def.expr)
  let is_pun = field_def.is_pun
  let colon_loc = field_def.colon_loc
  let loc = field_def.loc
  FieldDef::{ label, expr, is_pun, colon_loc, loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_InterpElem(
  env,
  interp_elem,
) {
  match interp_elem {
    Literal(repr~, loc~) => env.0.visit_InterpElem_Literal(repr~, loc~)
    Expr(expr~, loc~) => env.0.visit_InterpElem_Expr(expr~, loc~)
    Source(interp_source) => env.0.visit_InterpElem_Source(interp_source)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_InterpElem_Literal(
  _,
  repr~,
  loc~,
) {
  Literal(repr~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_InterpElem_Expr(
  env,
  expr~,
  loc~,
) {
  let expr = env.0.visit_Expr(expr)
  Expr(expr~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_InterpElem_Source(
  _,
  interp_source,
) {
  Source(interp_source)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_MultilineStringElem(
  env,
  multiline_string_elem,
) {
  match multiline_string_elem {
    String(string_val) => env.0.visit_MultilineStringElem_String(string_val)
    Interp(interp_elems) => env.0.visit_MultilineStringElem_Interp(interp_elems)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_MultilineStringElem_String(
  _,
  string_val,
) {
  String(string_val)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_MultilineStringElem_Interp(
  _,
  interp_elems,
) {
  Interp(interp_elems)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TryOperatorKind(
  _,
  try_operator_kind,
) {
  try_operator_kind
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_DotDotBinder(
  env,
  dot_dot_binder,
) {
  match dot_dot_binder {
    Underscore(dotdot_loc~, underscore_loc~) =>
      env.0.visit_DotDotBinder_Underscore(dotdot_loc~, underscore_loc~)
    NoBinder(dotdot_loc~) => env.0.visit_DotDotBinder_NoBinder(dotdot_loc~)
    BinderAs(binder~, dotdot_loc~, as_loc~) =>
      env.0.visit_DotDotBinder_BinderAs(binder~, dotdot_loc~, as_loc~)
    Binder(binder~, dotdot_loc~) =>
      env.0.visit_DotDotBinder_Binder(binder~, dotdot_loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_DotDotBinder_Underscore(
  env,
  dotdot_loc~,
  underscore_loc~,
) {
  ignore(env)
  Underscore(dotdot_loc~, underscore_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_DotDotBinder_NoBinder(
  env,
  dotdot_loc~,
) {
  ignore(env)
  NoBinder(dotdot_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_DotDotBinder_BinderAs(
  env,
  binder~,
  dotdot_loc~,
  as_loc~,
) {
  let binder = env.0.visit_Binder(binder)
  BinderAs(binder~, dotdot_loc~, as_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_DotDotBinder_Binder(
  env,
  binder~,
  dotdot_loc~,
) {
  let binder = env.0.visit_Binder(binder)
  Binder(binder~, dotdot_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArrayPattern(
  env,
  array_pattern,
) {
  match array_pattern {
    Pattern(pattern) => env.0.visit_ArrayPattern_Pattern(pattern)
    StringSpread(str~, dotdot_loc~, loc~) =>
      env.0.visit_ArrayPattern_StringSpread(str~, dotdot_loc~, loc~)
    BytesSpread(bytes~, dotdot_loc~, loc~) =>
      env.0.visit_ArrayPattern_BytesSpread(bytes~, dotdot_loc~, loc~)
    ConstSpread(binder~, pkg~, dotdot_loc~, pkg_loc~, dot_uident_loc~, loc~) =>
      env.0.visit_ArrayPattern_ConstSpread(
        binder~,
        pkg~,
        dotdot_loc~,
        pkg_loc~,
        dot_uident_loc~,
        loc~,
      )
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArrayPattern_Pattern(
  env,
  pattern,
) {
  let pattern = env.0.visit_Pattern(pattern)
  Pattern(pattern)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArrayPattern_StringSpread(
  _,
  str~,
  dotdot_loc~,
  loc~,
) {
  StringSpread(str~, dotdot_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArrayPattern_BytesSpread(
  _,
  bytes~,
  dotdot_loc~,
  loc~,
) {
  BytesSpread(bytes~, dotdot_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArrayPattern_ConstSpread(
  env,
  binder~,
  pkg~,
  dotdot_loc~,
  pkg_loc~,
  dot_uident_loc~,
  loc~,
) {
  let binder = env.0.visit_Binder(binder)
  ConstSpread(binder~, pkg~, dotdot_loc~, pkg_loc~, dot_uident_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArrayPatterns(
  env,
  array_patterns,
) {
  match array_patterns {
    Closed(patterns, comma_locs~) =>
      env.0.visit_ArrayPatterns_Closed(patterns, comma_locs~)
    Open(
      prefix_patterns,
      suffix_patterns,
      dot_dot_binder,
      left_comma_locs~,
      dotdot_comma_loc~,
      right_comma_locs~
    ) =>
      env.0.visit_ArrayPatterns_Open(
        prefix_patterns,
        suffix_patterns,
        dot_dot_binder,
        left_comma_locs~,
        dotdot_comma_loc~,
        right_comma_locs~,
      )
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArrayPatterns_Closed(
  env,
  array_patterns,
  comma_locs~,
) {
  let array_patterns = array_patterns.map(x => env.0.visit_ArrayPattern(x))
  Closed(array_patterns, comma_locs~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ArrayPatterns_Open(
  env,
  prefix_patterns,
  suffix_patterns,
  dot_dot_binder,
  left_comma_locs~,
  dotdot_comma_loc~,
  right_comma_locs~,
) {
  let prefix_patterns = prefix_patterns.map(x => env.0.visit_ArrayPattern(x))
  let suffix_patterns = suffix_patterns.map(x => env.0.visit_ArrayPattern(x))
  let dot_dot_binder = env.0.visit_DotDotBinder(dot_dot_binder)
  Open(
    prefix_patterns,
    suffix_patterns,
    dot_dot_binder,
    left_comma_locs~,
    dotdot_comma_loc~,
    right_comma_locs~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_WhereClause(
  env,
  where_clause,
) {
  let fields = where_clause.fields.map(x => env.0.visit_FieldDef(x))
  let where_loc = where_clause.where_loc
  let lbrace_loc = where_clause.lbrace_loc
  let rbrace_loc = where_clause.rbrace_loc
  let comma_locs = where_clause.comma_locs
  let loc = where_clause.loc
  WhereClause::{ fields, where_loc, lbrace_loc, rbrace_loc, comma_locs, loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_FieldPat(
  env,
  field_pat,
) {
  let label = env.0.visit_Label(field_pat.label)
  let pattern = env.0.visit_Pattern(field_pat.pattern)
  let is_pun = field_pat.is_pun
  let colon_loc = field_pat.colon_loc
  let loc = field_pat.loc
  FieldPat::{ label, pattern, is_pun, colon_loc, loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_ConstrPatArg(
  env,
  constr_pat_arg,
) {
  let pat = env.0.visit_Pattern(constr_pat_arg.pat)
  let kind = env.0.visit_ArgumentKind(constr_pat_arg.kind)
  let equal_loc = constr_pat_arg.equal_loc
  ConstrPatArg::{ pat, kind, equal_loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_MapPatElem(
  env,
  mappat_elem,
) {
  let key = env.0.visit_Constant(mappat_elem.key)
  let pat = env.0.visit_Pattern(mappat_elem.pat)
  let match_absent = mappat_elem.match_absent
  let key_loc = mappat_elem.key_loc
  let question_loc = mappat_elem.question_loc
  let colon_loc = mappat_elem.colon_loc
  let loc = mappat_elem.loc
  MapPatElem::{ key, pat, match_absent, key_loc, question_loc, colon_loc, loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_RangeKind(
  env,
  kind,
) {
  ignore(env)
  kind
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern(
  env,
  pattern,
) {
  match pattern {
    Alias(pat~, alias_~, as_loc~, loc~) =>
      env.0.visit_Pattern_Alias(pat~, alias_~, as_loc~, loc~)
    Any(loc~) => env.0.visit_Pattern_Any(loc~)
    Array(pats~, lbracket_loc~, rbracket_loc~, loc~) =>
      env.0.visit_Pattern_Array(pats~, lbracket_loc~, rbracket_loc~, loc~)
    Constant(c~, loc~) => env.0.visit_Pattern_Constant(c~, loc~)
    Constraint(pat~, ty~, lparen_loc~, colon_loc~, rparen_loc~, loc~) =>
      env.0.visit_Pattern_Constraint(
        pat~,
        ty~,
        lparen_loc~,
        colon_loc~,
        rparen_loc~,
        loc~,
      )
    Constr(
      constr~,
      args~,
      is_open~,
      args_lparen_loc~,
      args_rparen_loc~,
      arg_comma_locs~,
      open_dotdot_loc~,
      loc~
    ) =>
      env.0.visit_Pattern_Constr(
        constr~,
        args~,
        is_open~,
        args_lparen_loc~,
        args_rparen_loc~,
        arg_comma_locs~,
        open_dotdot_loc~,
        loc~,
      )
    Or(pat1~, pat2~, bar_loc~, loc~) =>
      env.0.visit_Pattern_Or(pat1~, pat2~, bar_loc~, loc~)
    Tuple(pats~, lparen_loc~, rparen_loc~, comma_locs~, loc~) =>
      env.0.visit_Pattern_Tuple(
        pats~,
        lparen_loc~,
        rparen_loc~,
        comma_locs~,
        loc~,
      )
    Var(binder) => env.0.visit_Pattern_Var(binder)
    Record(
      fields~,
      is_closed~,
      lbrace_loc~,
      rbrace_loc~,
      comma_locs~,
      dotdot_loc~,
      loc~
    ) =>
      env.0.visit_Pattern_Record(
        fields~,
        is_closed~,
        lbrace_loc~,
        rbrace_loc~,
        comma_locs~,
        dotdot_loc~,
        loc~,
      )
    Map(
      elems~,
      is_closed~,
      lbrace_loc~,
      rbrace_loc~,
      comma_locs~,
      dotdot_loc~,
      loc~
    ) =>
      env.0.visit_Pattern_Map(
        elems~,
        is_closed~,
        lbrace_loc~,
        rbrace_loc~,
        comma_locs~,
        dotdot_loc~,
        loc~,
      )
    Range(lhs~, rhs~, kind~, range_op_loc~, loc~) =>
      env.0.visit_Pattern_Range(lhs~, rhs~, kind~, range_op_loc~, loc~)
    SpecialConstr(binder~, args~, lparen_loc~, rparen_loc~, comma_locs~, loc~) =>
      env.0.visit_Pattern_SpecialConstr(
        binder~,
        args~,
        lparen_loc~,
        rparen_loc~,
        comma_locs~,
        loc~,
      )
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Alias(
  env,
  pat~,
  alias_~,
  as_loc~,
  loc~,
) {
  let pat = env.0.visit_Pattern(pat)
  let alias_ = env.0.visit_Binder(alias_)
  Alias(pat~, alias_~, as_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Any(
  _,
  loc~,
) {
  Any(loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Array(
  env,
  pats~,
  lbracket_loc~,
  rbracket_loc~,
  loc~,
) {
  let pats = env.0.visit_ArrayPatterns(pats)
  Array(pats~, lbracket_loc~, rbracket_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Constant(
  env,
  c~,
  loc~,
) {
  let c = env.0.visit_Constant(c)
  Constant(c~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Constraint(
  env,
  pat~,
  ty~,
  lparen_loc~,
  colon_loc~,
  rparen_loc~,
  loc~,
) {
  let pat = env.0.visit_Pattern(pat)
  let ty = env.0.visit_Type(ty)
  Constraint(pat~, ty~, lparen_loc~, colon_loc~, rparen_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Constr(
  env,
  constr~,
  args~,
  is_open~,
  args_lparen_loc~,
  args_rparen_loc~,
  arg_comma_locs~,
  open_dotdot_loc~,
  loc~,
) {
  let constr = env.0.visit_Constructor(constr)
  let args = args.map(args => args.map(x => env.0.visit_ConstrPatArg(x)))
  Constr(
    constr~,
    args~,
    is_open~,
    args_lparen_loc~,
    args_rparen_loc~,
    arg_comma_locs~,
    open_dotdot_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Or(
  env,
  pat1~,
  pat2~,
  bar_loc~,
  loc~,
) {
  let pat1 = env.0.visit_Pattern(pat1)
  let pat2 = env.0.visit_Pattern(pat2)
  Or(pat1~, pat2~, bar_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Tuple(
  env,
  pats~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  let pats = pats.map(x => env.0.visit_Pattern(x))
  Tuple(pats~, lparen_loc~, rparen_loc~, comma_locs~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Var(
  env,
  binder,
) {
  let binder = env.0.visit_Binder(binder)
  Var(binder)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Record(
  env,
  fields~,
  is_closed~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  dotdot_loc~,
  loc~,
) {
  let fields = fields.map(x => env.0.visit_FieldPat(x))
  Record(
    fields~,
    is_closed~,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs~,
    dotdot_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Map(
  env,
  elems~,
  is_closed~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  dotdot_loc~,
  loc~,
) {
  let elems = elems.map(x => env.0.visit_MapPatElem(x))
  Map(
    elems~,
    is_closed~,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs~,
    dotdot_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_Range(
  env,
  lhs~,
  rhs~,
  kind~,
  range_op_loc~,
  loc~,
) {
  let lhs = env.0.visit_Pattern(lhs)
  let rhs = env.0.visit_Pattern(rhs)
  let kind = env.0.visit_RangeKind(kind)
  Range(lhs~, rhs~, kind~, range_op_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Pattern_SpecialConstr(
  env,
  binder~,
  args~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  let binder = env.0.visit_Binder(binder)
  let args = args.map(x => env.0.visit_ConstrPatArg(x))
  SpecialConstr(binder~, args~, lparen_loc~, rparen_loc~, comma_locs~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_DerivingDirective(
  env,
  deriving_directive,
) {
  let type_name = env.0.visit_TypeName(deriving_directive.type_name)
  let args = deriving_directive.args.map(x => env.0.visit_Argument(x))
  let args_lparen_loc = deriving_directive.args_lparen_loc
  let args_rparen_loc = deriving_directive.args_rparen_loc
  let arg_comma_locs = deriving_directive.arg_comma_locs
  let loc = deriving_directive.loc
  DerivingDirective::{
    type_name,
    args,
    args_lparen_loc,
    args_rparen_loc,
    arg_comma_locs,
    loc,
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TypeDecl(
  env,
  type_decl,
) {
  let params = type_decl.params.map(x => env.0.visit_TypeDeclBinder(x))
  let components = env.0.visit_TypeDesc(type_decl.components)
  let deriving = type_decl.deriving.map(x => env.0.visit_DerivingDirective(x))
  let type_vis = env.0.visit_Visibility(type_decl.type_vis)
  let tycon = type_decl.tycon
  let tycon_loc = type_decl.tycon_loc
  let declare_loc = type_decl.declare_loc
  let keyword_loc = type_decl.keyword_loc
  let params_lbracket_loc = type_decl.params_lbracket_loc
  let params_rbracket_loc = type_decl.params_rbracket_loc
  let param_comma_locs = type_decl.param_comma_locs
  let derive_loc = type_decl.derive_loc
  let derive_lparen_loc = type_decl.derive_lparen_loc
  let derive_rparen_loc = type_decl.derive_rparen_loc
  let derive_comma_locs = type_decl.derive_comma_locs
  let attrs = type_decl.attrs
  let doc = type_decl.doc
  let loc = type_decl.loc
  let is_declare = type_decl.is_declare
  TypeDecl::{
    tycon,
    tycon_loc,
    params,
    components,
    attrs,
    doc,
    type_vis,
    deriving,
    declare_loc,
    keyword_loc,
    params_lbracket_loc,
    params_rbracket_loc,
    param_comma_locs,
    derive_loc,
    derive_lparen_loc,
    derive_rparen_loc,
    derive_comma_locs,
    loc,
    is_declare,
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_FuncStubs(
  env,
  func_stubs,
) {
  match func_stubs {
    Import(
      module_name~,
      func_name~,
      language~,
      module_name_loc~,
      func_name_loc~,
      language_loc~
    ) =>
      env.0.visit_FuncStubs_Import(
        module_name~,
        func_name~,
        language~,
        module_name_loc~,
        func_name_loc~,
        language_loc~,
      )
    Embedded(language~, code~, language_loc~, code_loc~) =>
      env.0.visit_FuncStubs_Embedded(language~, code~, language_loc~, code_loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_FuncStubs_Import(
  _,
  module_name~,
  func_name~,
  language~,
  module_name_loc~,
  func_name_loc~,
  language_loc~,
) {
  Import(
    module_name~,
    func_name~,
    language~,
    module_name_loc~,
    func_name_loc~,
    language_loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_FuncStubs_Embedded(
  env,
  language~,
  code~,
  language_loc~,
  code_loc~,
) {
  let code = env.0.visit_EmbeddedCode(code)
  Embedded(language~, code~, language_loc~, code_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_EmbeddedCode(
  env,
  embedded_code,
) {
  match embedded_code {
    CodeString(string_literal, loc~) =>
      env.0.visit_EmbeddedCode_CodeString(string_literal, loc~)
    CodeMultilineString(strings, string_locs~) =>
      env.0.visit_EmbeddedCode_CodeMultilineString(strings, string_locs~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_EmbeddedCode_CodeString(
  _,
  string_literal,
  loc~,
) {
  CodeString(string_literal, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_EmbeddedCode_CodeMultilineString(
  _,
  strings,
  string_locs~,
) {
  CodeMultilineString(strings, string_locs~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_DeclBody(
  env,
  decl_body,
) {
  match decl_body {
    DeclBody(expr~, lbrace_loc~, rbrace_loc~) =>
      env.0.visit_DeclBody_DeclBody(expr~, lbrace_loc~, rbrace_loc~)
    DeclStubs(func_stubs, equal_loc~) =>
      env.0.visit_DeclBody_DeclStubs(func_stubs, equal_loc~)
    DeclNone => DeclNone
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_DeclBody_DeclBody(
  env,
  expr~,
  lbrace_loc~,
  rbrace_loc~,
) {
  let expr = env.0.visit_Expr(expr)
  DeclBody(expr~, lbrace_loc~, rbrace_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_DeclBody_DeclStubs(
  env,
  func_stubs,
  equal_loc~,
) {
  let func_stubs = env.0.visit_FuncStubs(func_stubs)
  DeclStubs(func_stubs, equal_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_FunDecl(
  env,
  fun_decl,
) {
  let type_name = fun_decl.type_name.map(x => env.0.visit_TypeName(x))
  let name = env.0.visit_Binder(fun_decl.name)
  let decl_params = fun_decl.decl_params.map(decl_params => {
    decl_params.map(param => env.0.visit_Parameter(param))
  })
  let quantifiers = fun_decl.quantifiers.map(x => env.0.visit_TypeVarBinder(x))
  let return_type = fun_decl.return_type.map(x => env.0.visit_Type(x))
  let error_type = env.0.visit_ErrorType(fun_decl.error_type)
  let vis = env.0.visit_Visibility(fun_decl.vis)
  let is_async = fun_decl.is_async
  let declare_loc = fun_decl.declare_loc
  let extern_loc = fun_decl.extern_loc
  let fn_loc = fun_decl.fn_loc
  let coloncolon_loc = fun_decl.coloncolon_loc
  let params_loc = fun_decl.params_loc
  let type_params_lbracket_loc = fun_decl.type_params_lbracket_loc
  let type_params_rbracket_loc = fun_decl.type_params_rbracket_loc
  let type_param_comma_locs = fun_decl.type_param_comma_locs
  let params_lparen_loc = fun_decl.params_lparen_loc
  let params_rparen_loc = fun_decl.params_rparen_loc
  let param_comma_locs = fun_decl.param_comma_locs
  let return_arrow_loc = fun_decl.return_arrow_loc
  let attrs = fun_decl.attrs
  let doc = fun_decl.doc
  let loc = fun_decl.loc
  FunDecl::{
    type_name,
    name,
    is_async,
    declare_loc,
    extern_loc,
    fn_loc,
    coloncolon_loc,
    decl_params,
    params_loc,
    type_params_lbracket_loc,
    type_params_rbracket_loc,
    type_param_comma_locs,
    params_lparen_loc,
    params_rparen_loc,
    param_comma_locs,
    quantifiers,
    return_type,
    return_arrow_loc,
    error_type,
    vis,
    attrs,
    doc,
    loc,
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TraitMethodDecl(
  env,
  method_decl,
) {
  let name = env.0.visit_Binder(method_decl.name)
  let quantifiers = method_decl.quantifiers.map(x => {
    env.0.visit_TypeVarBinder(x)
  })
  let params = method_decl.params.map(param => env.0.visit_Parameter(param))
  let return_type = method_decl.return_type.map(x => env.0.visit_Type(x))
  let error_type = env.0.visit_ErrorType(method_decl.error_type)
  let is_async = method_decl.is_async
  let fn_loc = method_decl.fn_loc
  let type_params_lbracket_loc = method_decl.type_params_lbracket_loc
  let type_params_rbracket_loc = method_decl.type_params_rbracket_loc
  let type_param_comma_locs = method_decl.type_param_comma_locs
  let params_lparen_loc = method_decl.params_lparen_loc
  let params_rparen_loc = method_decl.params_rparen_loc
  let param_comma_locs = method_decl.param_comma_locs
  let return_arrow_loc = method_decl.return_arrow_loc
  let has_default = method_decl.has_default
  let default_equal_loc = method_decl.default_equal_loc
  let attrs = method_decl.attrs
  let loc = method_decl.loc
  TraitMethodDecl::{
    name,
    is_async,
    fn_loc,
    quantifiers,
    type_params_lbracket_loc,
    type_params_rbracket_loc,
    type_param_comma_locs,
    params,
    params_lparen_loc,
    params_rparen_loc,
    param_comma_locs,
    return_type,
    return_arrow_loc,
    error_type,
    has_default,
    default_equal_loc,
    attrs,
    loc,
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_TraitDecl(
  env,
  trait_decl,
) {
  let name = env.0.visit_Binder(trait_decl.name)
  let supers = trait_decl.supers.map(x => env.0.visit_TypeVarConstraint(x))
  let methods = trait_decl.methods.map(x => env.0.visit_TraitMethodDecl(x))
  let vis = env.0.visit_Visibility(trait_decl.vis)
  let declare_loc = trait_decl.declare_loc
  let trait_loc = trait_decl.trait_loc
  let colon_loc = trait_decl.colon_loc
  let super_plus_locs = trait_decl.super_plus_locs
  let lbrace_loc = trait_decl.lbrace_loc
  let rbrace_loc = trait_decl.rbrace_loc
  let semi_locs = trait_decl.semi_locs
  let loc = trait_decl.loc
  let attrs = trait_decl.attrs
  let doc = trait_decl.doc
  let is_declare = trait_decl.is_declare
  TraitDecl::{
    name,
    supers,
    methods,
    vis,
    declare_loc,
    trait_loc,
    colon_loc,
    super_plus_locs,
    lbrace_loc,
    rbrace_loc,
    semi_locs,
    loc,
    attrs,
    doc,
    is_declare,
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl_TopExpr(
  env,
  expr~,
  is_main~,
  is_async~,
  loc~,
) {
  let expr = env.0.visit_Expr(expr)
  TopExpr(expr~, is_main~, is_async~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl_TopTest(
  env,
  expr~,
  name~,
  params~,
  is_async~,
  test_loc~,
  params_lparen_loc~,
  params_rparen_loc~,
  param_comma_locs~,
  body_lbrace_loc~,
  body_rbrace_loc~,
  loc~,
  attrs~,
  doc~,
) {
  let expr = env.0.visit_Expr(expr)
  let params = params.map(params => {
    params.map(param => env.0.visit_Parameter(param))
  })
  TopTest(
    expr~,
    name~,
    params~,
    is_async~,
    test_loc~,
    params_lparen_loc~,
    params_rparen_loc~,
    param_comma_locs~,
    body_lbrace_loc~,
    body_rbrace_loc~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl_TopTypeDef(
  env,
  type_decl,
) {
  let type_decl = env.0.visit_TypeDecl(type_decl)
  TopTypeDef(type_decl)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl_TopFuncDef(
  env,
  fun_decl~,
  decl_body~,
  where_clause~,
  equal_loc~,
  loc~,
) {
  let fun_decl = env.0.visit_FunDecl(fun_decl)
  let decl_body = env.0.visit_DeclBody(decl_body)
  let where_clause = where_clause.map(x => env.0.visit_WhereClause(x))
  TopFuncDef(fun_decl~, decl_body~, where_clause~, equal_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl_TopLetDef(
  env,
  binder~,
  ty~,
  expr~,
  vis~,
  is_constant~,
  declare_loc~,
  let_or_const_loc~,
  annot_colon_loc~,
  equal_loc~,
  loc~,
  attrs~,
  doc~,
  is_declare~,
) {
  let binder = env.0.visit_Binder(binder)
  let ty = ty.map(x => env.0.visit_Type(x))
  let expr = env.0.visit_Expr(expr)
  let vis = env.0.visit_Visibility(vis)
  TopLetDef(
    binder~,
    ty~,
    expr~,
    vis~,
    is_constant~,
    declare_loc~,
    let_or_const_loc~,
    annot_colon_loc~,
    equal_loc~,
    loc~,
    attrs~,
    doc~,
    is_declare~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl_TopTrait(
  env,
  trait_decl,
) {
  let trait_decl = env.0.visit_TraitDecl(trait_decl)
  TopTrait(trait_decl)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl_TopImpl(
  env,
  self_ty~,
  trait_~,
  method_name~,
  quantifiers~,
  params~,
  ret_ty~,
  err_ty~,
  body~,
  vis~,
  impl_loc~,
  type_params_lbracket_loc~,
  type_params_rbracket_loc~,
  type_param_comma_locs~,
  for_loc~,
  with_loc~,
  fn_loc~,
  params_lparen_loc~,
  params_rparen_loc~,
  param_comma_locs~,
  return_arrow_loc~,
  loc~,
  attrs~,
  doc~,
) {
  let self_ty = self_ty.map(x => env.0.visit_Type(x))
  let trait_ = env.0.visit_TypeName(trait_)
  let method_name = env.0.visit_Binder(method_name)
  let quantifiers = quantifiers.map(x => env.0.visit_TypeVarBinder(x))
  let params = params.map(param => env.0.visit_Parameter(param))
  let ret_ty = ret_ty.map(x => env.0.visit_Type(x))
  let err_ty = env.0.visit_ErrorType(err_ty)
  let body = env.0.visit_DeclBody(body)
  let vis = env.0.visit_Visibility(vis)
  TopImpl(
    self_ty~,
    trait_~,
    method_name~,
    quantifiers~,
    params~,
    ret_ty~,
    err_ty~,
    body~,
    vis~,
    impl_loc~,
    type_params_lbracket_loc~,
    type_params_rbracket_loc~,
    type_param_comma_locs~,
    for_loc~,
    with_loc~,
    fn_loc~,
    params_lparen_loc~,
    params_rparen_loc~,
    param_comma_locs~,
    return_arrow_loc~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl_TopView(
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
  enumview_loc~,
  type_params_lbracket_loc~,
  type_params_rbracket_loc~,
  type_param_comma_locs~,
  lbrace_loc~,
  rbrace_loc~,
  constructor_semi_locs~,
  for_loc~,
  with_loc~,
  params_lparen_loc~,
  params_rparen_loc~,
  param_comma_locs~,
  body_lbrace_loc~,
  body_rbrace_loc~,
  loc~,
  attrs~,
  doc~,
) {
  let quantifiers = quantifiers.map(x => env.0.visit_TypeVarBinder(x))
  let source_ty = env.0.visit_Type(source_ty)
  let view_constrs = view_constrs.map(x => env.0.visit_ConstrDecl(x))
  let view_func_name = env.0.visit_Binder(view_func_name)
  let parameters = parameters.map(param => env.0.visit_Parameter(param))
  let body = env.0.visit_Expr(body)
  let vis = env.0.visit_Visibility(vis)
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
    enumview_loc~,
    type_params_lbracket_loc~,
    type_params_rbracket_loc~,
    type_param_comma_locs~,
    lbrace_loc~,
    rbrace_loc~,
    constructor_semi_locs~,
    for_loc~,
    with_loc~,
    params_lparen_loc~,
    params_rparen_loc~,
    param_comma_locs~,
    body_lbrace_loc~,
    body_rbrace_loc~,
    loc~,
    attrs~,
    doc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl_TopImplRelation(
  env,
  self_ty~,
  trait_~,
  quantifiers~,
  vis~,
  attrs~,
  declare_loc~,
  impl_loc~,
  type_params_lbracket_loc~,
  type_params_rbracket_loc~,
  type_param_comma_locs~,
  for_loc~,
  loc~,
  doc~,
  is_declare~,
) {
  let self_ty = env.0.visit_Type(self_ty)
  let trait_ = env.0.visit_TypeName(trait_)
  let quantifiers = quantifiers.map(x => env.0.visit_TypeVarBinder(x))
  let vis = env.0.visit_Visibility(vis)
  let is_declare = is_declare
  TopImplRelation(
    self_ty~,
    trait_~,
    quantifiers~,
    vis~,
    attrs~,
    declare_loc~,
    impl_loc~,
    type_params_lbracket_loc~,
    type_params_rbracket_loc~,
    type_param_comma_locs~,
    for_loc~,
    loc~,
    doc~,
    is_declare~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Impl_TopUsing(
  env,
  pkg~,
  names~,
  vis~,
  attrs~,
  using_loc~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  loc~,
  doc~,
) {
  let pkg = env.0.visit_Label(pkg)
  let names = names.map(name => {
    (env.0.visit_AliasTarget(name.0), env.0.visit_UsingKind(name.1))
  })
  let vis = env.0.visit_Visibility(vis)
  TopUsing(
    pkg~,
    names~,
    vis~,
    attrs~,
    using_loc~,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs~,
    loc~,
    doc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Apply(
  env,
  func~,
  args~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  let func = env.0.visit_Expr(func)
  let args = args.map(x => env.0.visit_Argument(x))
  Expr::Apply(func~, args~, lparen_loc~, rparen_loc~, comma_locs~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Infix(
  env,
  op~,
  lhs~,
  rhs~,
  loc~,
) {
  let op = env.0.visit_Var(op)
  let lhs = env.0.visit_Expr(lhs)
  let rhs = env.0.visit_Expr(rhs)
  Expr::Infix(op~, lhs~, rhs~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Unary(
  env,
  op~,
  expr~,
  loc~,
) {
  let op = env.0.visit_Var(op)
  let expr = env.0.visit_Expr(expr)
  Expr::Unary(op~, expr~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Array(
  env,
  exprs~,
  lbracket_loc~,
  rbracket_loc~,
  comma_locs~,
  loc~,
) {
  let exprs = exprs.map(expr => env.0.visit_Expr(expr))
  Expr::Array(exprs~, lbracket_loc~, rbracket_loc~, comma_locs~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_ArraySpread(
  env,
  elems~,
  lbracket_loc~,
  rbracket_loc~,
  comma_locs~,
  loc~,
) {
  let elems = elems.map(fn(elem) {
    match elem {
      Regular(expr) => Regular(env.0.visit_Expr(expr))
      Spread(expr~, dotdot_loc~, loc~) =>
        Spread(expr=env.0.visit_Expr(expr), dotdot_loc~, loc~)
    }
  })
  Expr::ArraySpread(elems~, lbracket_loc~, rbracket_loc~, comma_locs~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_ArrayGet(
  env,
  array~,
  index~,
  lbracket_loc~,
  rbracket_loc~,
  loc~,
) {
  let array = env.0.visit_Expr(array)
  let index = env.0.visit_Expr(index)
  Expr::ArrayGet(array~, index~, lbracket_loc~, rbracket_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_ArrayGetSlice(
  env,
  array~,
  start_index~,
  end_index~,
  index_loc~,
  lbracket_loc~,
  colon_loc~,
  rbracket_loc~,
  loc~,
) {
  let array = env.0.visit_Expr(array)
  let start_index = start_index.map(e => env.0.visit_Expr(e))
  let end_index = end_index.map(e => env.0.visit_Expr(e))
  Expr::ArrayGetSlice(
    array~,
    start_index~,
    end_index~,
    index_loc~,
    lbracket_loc~,
    colon_loc~,
    rbracket_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_ArraySet(
  env,
  array~,
  index~,
  value~,
  lbracket_loc~,
  rbracket_loc~,
  equal_loc~,
  loc~,
) {
  let index = env.0.visit_Expr(index)
  let value = env.0.visit_Expr(value)
  let array = env.0.visit_Expr(array)
  Expr::ArraySet(
    array~,
    index~,
    value~,
    lbracket_loc~,
    rbracket_loc~,
    equal_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_ArrayAugmentedSet(
  env,
  op~,
  array~,
  index~,
  value~,
  lbracket_loc~,
  rbracket_loc~,
  augmented_assign_loc~,
  loc~,
) {
  let op = env.0.visit_Var(op)
  let array = env.0.visit_Expr(array)
  let index = env.0.visit_Expr(index)
  let value = env.0.visit_Expr(value)
  Expr::ArrayAugmentedSet(
    op~,
    array~,
    index~,
    value~,
    lbracket_loc~,
    rbracket_loc~,
    augmented_assign_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Constant(
  env,
  c~,
  loc~,
) {
  let c = env.0.visit_Constant(c)
  Expr::Constant(c~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_MultilineString(
  env,
  elems~,
  loc~,
) {
  let elems = elems.map(elem => env.0.visit_MultilineStringElem(elem))
  Expr::MultilineString(elems~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Interp(
  env,
  elems~,
  loc~,
) {
  let elems = elems.map(elem => env.0.visit_InterpElem(elem))
  Expr::Interp(elems~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Constraint(
  env,
  expr~,
  ty~,
  lparen_loc~,
  colon_loc~,
  rparen_loc~,
  loc~,
) {
  let ty = env.0.visit_Type(ty)
  let expr = env.0.visit_Expr(expr)
  Expr::Constraint(expr~, ty~, lparen_loc~, colon_loc~, rparen_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Constr(
  env,
  constr~,
  loc~,
) {
  let constr = env.0.visit_Constructor(constr)
  Expr::Constr(constr~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_While(
  env,
  loop_cond~,
  loop_body~,
  while_else~,
  label~,
  label_colon_loc~,
  while_loc~,
  body_lbrace_loc~,
  body_rbrace_loc~,
  nobreak_loc~,
  nobreak_lbrace_loc~,
  nobreak_rbrace_loc~,
  loc~,
) {
  let loop_cond = env.0.visit_Expr(loop_cond)
  let loop_body = env.0.visit_Expr(loop_body)
  let while_else = while_else.map(else_block => env.0.visit_Expr(else_block))
  let label = label.map(x => env.0.visit_Label(x))
  Expr::While(
    loop_cond~,
    loop_body~,
    while_else~,
    label~,
    label_colon_loc~,
    while_loc~,
    body_lbrace_loc~,
    body_rbrace_loc~,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Function(
  env,
  func~,
  loc~,
) {
  let func = env.0.visit_Func(func)
  Expr::Function(func~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Ident(
  env,
  id~,
  loc~,
) {
  let id = env.0.visit_Var(id)
  Expr::Ident(id~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_If(
  env,
  cond~,
  ifso~,
  ifnot~,
  if_loc~,
  then_lbrace_loc~,
  then_rbrace_loc~,
  else_loc~,
  else_lbrace_loc~,
  else_rbrace_loc~,
  loc~,
) {
  let cond = env.0.visit_Expr(cond)
  let ifso = env.0.visit_Expr(ifso)
  let ifnot = ifnot.map(expr => env.0.visit_Expr(expr))
  Expr::If(
    cond~,
    ifso~,
    ifnot~,
    if_loc~,
    then_lbrace_loc~,
    then_rbrace_loc~,
    else_loc~,
    else_lbrace_loc~,
    else_rbrace_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Guard(
  env,
  cond~,
  otherwise~,
  body~,
  guard_loc~,
  else_loc~,
  else_lbrace_loc~,
  else_rbrace_loc~,
  semi_loc~,
  loc~,
) {
  let cond = env.0.visit_Expr(cond)
  let body = env.0.visit_Expr(body)
  let otherwise = otherwise.map(expr => env.0.visit_Expr(expr))
  Expr::Guard(
    cond~,
    otherwise~,
    body~,
    guard_loc~,
    else_loc~,
    else_lbrace_loc~,
    else_rbrace_loc~,
    semi_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Is(
  env,
  expr~,
  pat~,
  is_loc~,
  loc~,
) {
  let expr = env.0.visit_Expr(expr)
  let pat = env.0.visit_Pattern(pat)
  Expr::Is(expr~, pat~, is_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Defer(
  env,
  expr~,
  body~,
  defer_loc~,
  semi_loc~,
  loc~,
) {
  let expr = env.0.visit_Expr(expr)
  let body = env.0.visit_Expr(body)
  Expr::Defer(expr~, body~, defer_loc~, semi_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_LetFn(
  env,
  name~,
  func~,
  body~,
  semi_loc~,
  loc~,
) {
  let name = env.0.visit_Binder(name)
  let func = env.0.visit_Func(func)
  let body = env.0.visit_Expr(body)
  Expr::LetFn(name~, func~, body~, semi_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_LetAnd(
  env,
  bindings~,
  body~,
  letrec_loc~,
  and_locs~,
  equal_locs~,
  annot_colon_locs~,
  semi_loc~,
  loc~,
) {
  let bindings = bindings.map(binding => {
    match binding {
      (binder, ty, func) =>
        (
          env.0.visit_Binder(binder),
          ty.map(x => env.0.visit_Type(x)),
          env.0.visit_Func(func),
        )
    }
  })
  let body = env.0.visit_Expr(body)
  Expr::LetAnd(
    bindings~,
    body~,
    letrec_loc~,
    and_locs~,
    equal_locs~,
    annot_colon_locs~,
    semi_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Let(
  env,
  pattern~,
  expr~,
  body~,
  let_loc~,
  annot_colon_loc~,
  equal_loc~,
  semi_loc~,
  loc~,
) {
  let pattern = env.0.visit_Pattern(pattern)
  let expr = env.0.visit_Expr(expr)
  let body = env.0.visit_Expr(body)
  Expr::Let(
    pattern~,
    expr~,
    body~,
    let_loc~,
    annot_colon_loc~,
    equal_loc~,
    semi_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Sequence(
  env,
  exprs~,
  last_expr~,
  semi_locs~,
  loc~,
) {
  let exprs = exprs.map(expr => env.0.visit_Expr(expr))
  let last_expr = env.0.visit_Expr(last_expr)
  Expr::Sequence(exprs~, last_expr~, semi_locs~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Tuple(
  env,
  exprs~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  let exprs = exprs.map(expr => env.0.visit_Expr(expr))
  Expr::Tuple(exprs~, lparen_loc~, rparen_loc~, comma_locs~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Record(
  env,
  type_name~,
  fields~,
  trailing~,
  coloncolon_loc~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  semi_locs~,
  loc~,
) {
  let type_name = type_name.map(x => env.0.visit_TypeName(x))
  let fields = fields.map(x => env.0.visit_FieldDef(x))
  let trailing = env.0.visit_TrailingMark(trailing)
  Expr::Record(
    type_name~,
    fields~,
    trailing~,
    coloncolon_loc~,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs~,
    semi_locs~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_RecordUpdate(
  env,
  type_name~,
  record~,
  fields~,
  coloncolon_loc~,
  lbrace_loc~,
  dotdot_loc~,
  rbrace_loc~,
  comma_locs~,
  loc~,
) {
  let type_name = type_name.map(x => env.0.visit_TypeName(x))
  let fields = fields.map(x => env.0.visit_FieldDef(x))
  let record = env.0.visit_Expr(record)
  Expr::RecordUpdate(
    type_name~,
    record~,
    fields~,
    coloncolon_loc~,
    lbrace_loc~,
    dotdot_loc~,
    rbrace_loc~,
    comma_locs~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Field(
  env,
  record~,
  accessor~,
  loc~,
) {
  let record = env.0.visit_Expr(record)
  let accessor = env.0.visit_Accessor(accessor)
  Expr::Field(record~, accessor~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Method(
  env,
  type_name~,
  method_name~,
  coloncolon_loc~,
  loc~,
) {
  let type_name = env.0.visit_TypeName(type_name)
  let method_name = env.0.visit_Label(method_name)
  Expr::Method(type_name~, method_name~, coloncolon_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_DotApply(
  env,
  self~,
  method_name~,
  args~,
  return_self~,
  dot_loc~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  let self = env.0.visit_Expr(self)
  let method_name = env.0.visit_Label(method_name)
  let args = args.map(x => env.0.visit_Argument(x))
  Expr::DotApply(
    self~,
    method_name~,
    args~,
    return_self~,
    dot_loc~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_As(
  env,
  expr~,
  trait_~,
  as_loc~,
  loc~,
) {
  let trait_ = env.0.visit_TypeName(trait_)
  let expr = env.0.visit_Expr(expr)
  Expr::As(expr~, trait_~, as_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Mutate(
  env,
  record~,
  accessor~,
  field~,
  augmented_by~,
  equal_loc~,
  augmented_assign_loc~,
  loc~,
) {
  let record = env.0.visit_Expr(record)
  let accessor = env.0.visit_Accessor(accessor)
  let field = env.0.visit_Expr(field)
  let augmented_by = augmented_by.map(x => env.0.visit_Var(x))
  Expr::Mutate(
    record~,
    accessor~,
    field~,
    augmented_by~,
    equal_loc~,
    augmented_assign_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Match(
  env,
  expr~,
  cases~,
  match_loc~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
  loc~,
) {
  let expr = env.0.visit_Expr(expr)
  let cases = cases.map(x => env.0.visit_Case(x))
  Expr::Match(
    expr~,
    cases~,
    match_loc~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_LetMut(
  env,
  binder~,
  ty~,
  expr~,
  body~,
  let_loc~,
  mut_loc~,
  annot_colon_loc~,
  equal_loc~,
  semi_loc~,
  loc~,
) {
  let binder = env.0.visit_Binder(binder)
  let ty = ty.map(x => env.0.visit_Type(x))
  let expr = env.0.visit_Expr(expr)
  let body = env.0.visit_Expr(body)
  Expr::LetMut(
    binder~,
    ty~,
    expr~,
    body~,
    let_loc~,
    mut_loc~,
    annot_colon_loc~,
    equal_loc~,
    semi_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Pipe(
  env,
  lhs~,
  rhs~,
  pipe_loc~,
  loc~,
) {
  let lhs = env.0.visit_Expr(lhs)
  let rhs = env.0.visit_Expr(rhs)
  Expr::Pipe(lhs~, rhs~, pipe_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Assign(
  env,
  var_~,
  expr~,
  augmented_by~,
  equal_loc~,
  augmented_assign_loc~,
  loc~,
) {
  let var_ = env.0.visit_Var(var_)
  let augmented_by = augmented_by.map(x => env.0.visit_Var(x))
  let expr = env.0.visit_Expr(expr)
  Expr::Assign(
    var_~,
    expr~,
    augmented_by~,
    equal_loc~,
    augmented_assign_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Hole(
  env,
  loc~,
  kind~,
) {
  let kind = env.0.visit_Hole(kind)
  Expr::Hole(loc~, kind~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Return(
  env,
  return_value~,
  return_loc~,
  loc~,
) {
  let return_value = return_value.map(value => env.0.visit_Expr(value))
  Expr::Return(return_value~, return_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Raise(
  env,
  err_value~,
  raise_loc~,
  loc~,
) {
  let err_value = env.0.visit_Expr(err_value)
  Expr::Raise(err_value~, raise_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Unit(
  _,
  loc~,
  faked~,
  lparen_loc~,
  rparen_loc~,
) {
  Expr::Unit(loc~, faked~, lparen_loc~, rparen_loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Break(
  env,
  arg~,
  label~,
  break_loc~,
  post_label_loc~,
  loc~,
) {
  let label = label.map(x => env.0.visit_Label(x))
  let arg = arg.map(x => env.0.visit_Expr(x))
  Expr::Break(arg~, label~, break_loc~, post_label_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Continue(
  env,
  args~,
  label~,
  continue_loc~,
  post_label_loc~,
  comma_locs~,
  loc~,
) {
  let args = args.map(arg => env.0.visit_Expr(arg))
  let label = label.map(x => env.0.visit_Label(x))
  Expr::Continue(
    args~,
    label~,
    continue_loc~,
    post_label_loc~,
    comma_locs~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_For(
  env,
  binders~,
  condition~,
  continue_block~,
  body~,
  for_else~,
  label~,
  label_colon_loc~,
  for_loc~,
  binder_comma_locs~,
  binder_equal_locs~,
  semi_locs~,
  continue_comma_locs~,
  continue_equal_locs~,
  body_lbrace_loc~,
  body_rbrace_loc~,
  nobreak_loc~,
  nobreak_lbrace_loc~,
  nobreak_rbrace_loc~,
  loc~,
  where_clause~,
) {
  let binders = binders.map(p => {
    (env.0.visit_Binder(p.0), env.0.visit_Expr(p.1))
  })
  let condition = condition.map(cond => env.0.visit_Expr(cond))
  let continue_block = continue_block.map(p => {
    (env.0.visit_Binder(p.0), env.0.visit_Expr(p.1))
  })
  let body = env.0.visit_Expr(body)
  let for_else = for_else.map(else_block => env.0.visit_Expr(else_block))
  let label = label.map(x => env.0.visit_Label(x))
  let where_clause = where_clause.map(x => env.0.visit_WhereClause(x))
  Expr::For(
    binders~,
    condition~,
    continue_block~,
    body~,
    for_else~,
    label~,
    label_colon_loc~,
    for_loc~,
    binder_comma_locs~,
    binder_equal_locs~,
    semi_locs~,
    continue_comma_locs~,
    continue_equal_locs~,
    body_lbrace_loc~,
    body_rbrace_loc~,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
    loc~,
    where_clause~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_ForEach(
  env,
  binders~,
  expr~,
  init~,
  continue_block~,
  body~,
  else_block~,
  where_clause~,
  label~,
  label_colon_loc~,
  for_loc~,
  binder_comma_locs~,
  in_loc~,
  semi_locs~,
  init_comma_locs~,
  init_equal_locs~,
  continue_comma_locs~,
  continue_equal_locs~,
  body_lbrace_loc~,
  body_rbrace_loc~,
  nobreak_loc~,
  nobreak_lbrace_loc~,
  nobreak_rbrace_loc~,
  loc~,
) {
  let binders = binders.map(binder => binder.map(x => env.0.visit_Binder(x)))
  let expr = env.0.visit_Expr(expr)
  let init = init.map(p => (env.0.visit_Binder(p.0), env.0.visit_Expr(p.1)))
  let continue_block = continue_block.map(p => {
    (env.0.visit_Binder(p.0), env.0.visit_Expr(p.1))
  })
  let body = env.0.visit_Expr(body)
  let else_block = else_block.map(else_block => env.0.visit_Expr(else_block))
  let where_clause = where_clause.map(x => env.0.visit_WhereClause(x))
  let label = label.map(x => env.0.visit_Label(x))
  Expr::ForEach(
    binders~,
    expr~,
    init~,
    continue_block~,
    body~,
    else_block~,
    where_clause~,
    label~,
    label_colon_loc~,
    for_loc~,
    binder_comma_locs~,
    in_loc~,
    semi_locs~,
    init_comma_locs~,
    init_equal_locs~,
    continue_comma_locs~,
    continue_equal_locs~,
    body_lbrace_loc~,
    body_rbrace_loc~,
    nobreak_loc~,
    nobreak_lbrace_loc~,
    nobreak_rbrace_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Try(
  env,
  body~,
  catch_~,
  try_else~,
  has_try~,
  try_loc~,
  catch_loc~,
  else_loc~,
  catch_lbrace_loc~,
  catch_rbrace_loc~,
  catch_semi_locs~,
  noraise_lbrace_loc~,
  noraise_rbrace_loc~,
  noraise_semi_locs~,
  loc~,
) {
  let body = env.0.visit_Expr(body)
  let catch_ = catch_.map(x => env.0.visit_Case(x))
  let try_else = try_else.map(try_else => try_else.map(x => env.0.visit_Case(x)))
  Expr::Try(
    body~,
    catch_~,
    try_else~,
    has_try~,
    try_loc~,
    catch_loc~,
    else_loc~,
    catch_lbrace_loc~,
    catch_rbrace_loc~,
    catch_semi_locs~,
    noraise_lbrace_loc~,
    noraise_rbrace_loc~,
    noraise_semi_locs~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_TryOperator(
  env,
  body~,
  kind~,
  try_loc~,
  loc~,
) {
  let body = env.0.visit_Expr(body)
  let kind = env.0.visit_TryOperatorKind(kind)
  Expr::TryOperator(body~, kind~, try_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Map(
  env,
  elems~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  loc~,
) {
  let elems = elems.map(x => env.0.visit_MapExprElem(x))
  Expr::Map(elems~, lbrace_loc~, rbrace_loc~, comma_locs~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_Group(
  env,
  expr~,
  group~,
  left_delim_loc~,
  right_delim_loc~,
  loc~,
) {
  let expr = env.0.visit_Expr(expr)
  let group = env.0.visit_Group(group)
  Expr::Group(expr~, group~, left_delim_loc~, right_delim_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_StaticAssert(
  env,
  asserts~,
  body~,
) {
  let asserts = asserts.map(x => env.0.visit_StaticAssertion(x))
  let body = env.0.visit_Expr(body)
  Expr::StaticAssert(asserts~, body~)
}

///|
fn[T : MapVisitor] map_regex_pattern(
  env : MapVisitorBase[T],
  pat : RegexPattern,
) -> RegexPattern {
  match pat {
    Literal(lit~, loc~) => RegexPattern::Literal(lit~, loc~)
    Reference(lid~, loc~) => {
      let lid = env.0.visit_LongIdent(lid)
      RegexPattern::Reference(lid~, loc~)
    }
    Sequence(pat1~, pat2~, plus_loc~, loc~) => {
      let pat1 = map_regex_pattern(env, pat1)
      let pat2 = map_regex_pattern(env, pat2)
      RegexPattern::Sequence(pat1~, pat2~, plus_loc~, loc~)
    }
    Alternation(pat1~, pat2~, bar_loc~, loc~) => {
      let pat1 = map_regex_pattern(env, pat1)
      let pat2 = map_regex_pattern(env, pat2)
      RegexPattern::Alternation(pat1~, pat2~, bar_loc~, loc~)
    }
    Alias(pat~, binder~, as_loc~, loc~) => {
      let pat = map_regex_pattern(env, pat)
      let binder = env.0.visit_Binder(binder)
      RegexPattern::Alias(pat~, binder~, as_loc~, loc~)
    }
  }
}

///|
fn[T : MapVisitor] map_list_comprehension_kind(
  env : MapVisitorBase[T],
  kind : ListComprehensionKind,
) -> ListComprehensionKind {
  match kind {
    Foreach(
      binders~,
      expr~,
      init~,
      continue_block~,
      for_loc~,
      in_loc~,
      binder_comma_locs~,
      semi_locs~,
      init_comma_locs~,
      init_equal_locs~,
      continue_comma_locs~,
      continue_equal_locs~
    ) => {
      let binders = binders.map(binder => binder.map(x => env.0.visit_Binder(x)))
      let expr = env.0.visit_Expr(expr)
      let init = init.map(p => (env.0.visit_Binder(p.0), env.0.visit_Expr(p.1)))
      let continue_block = continue_block.map(p => {
        (env.0.visit_Binder(p.0), env.0.visit_Expr(p.1))
      })
      ListComprehensionKind::Foreach(
        binders~,
        expr~,
        init~,
        continue_block~,
        for_loc~,
        in_loc~,
        binder_comma_locs~,
        semi_locs~,
        init_comma_locs~,
        init_equal_locs~,
        continue_comma_locs~,
        continue_equal_locs~,
      )
    }
    For(
      binders~,
      condition~,
      continue_block~,
      for_loc~,
      binder_comma_locs~,
      semi_locs~,
      binder_equal_locs~,
      continue_comma_locs~,
      continue_equal_locs~
    ) => {
      let binders = binders.map(p => {
        (env.0.visit_Binder(p.0), env.0.visit_Expr(p.1))
      })
      let condition = condition.map(cond => env.0.visit_Expr(cond))
      let continue_block = continue_block.map(p => {
        (env.0.visit_Binder(p.0), env.0.visit_Expr(p.1))
      })
      ListComprehensionKind::For(
        binders~,
        condition~,
        continue_block~,
        for_loc~,
        binder_comma_locs~,
        semi_locs~,
        binder_equal_locs~,
        continue_comma_locs~,
        continue_equal_locs~,
      )
    }
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr(
  env,
  expr,
) {
  match expr {
    Apply(func~, args~, lparen_loc~, rparen_loc~, comma_locs~, loc~) =>
      env.0.visit_Expr_Apply(
        func~,
        args~,
        lparen_loc~,
        rparen_loc~,
        comma_locs~,
        loc~,
      )
    Infix(op~, lhs~, rhs~, loc~) =>
      env.0.visit_Expr_Infix(op~, lhs~, rhs~, loc~)
    Unary(op~, expr~, loc~) => env.0.visit_Expr_Unary(op~, expr~, loc~)
    Array(exprs~, lbracket_loc~, rbracket_loc~, comma_locs~, loc~) =>
      env.0.visit_Expr_Array(
        exprs~,
        lbracket_loc~,
        rbracket_loc~,
        comma_locs~,
        loc~,
      )
    ArraySpread(elems~, lbracket_loc~, rbracket_loc~, comma_locs~, loc~) =>
      env.0.visit_Expr_ArraySpread(
        elems~,
        lbracket_loc~,
        rbracket_loc~,
        comma_locs~,
        loc~,
      )
    ListComprehension(
      kind~,
      guard_~,
      body~,
      lbracket_loc~,
      rbracket_loc~,
      if_loc~,
      fat_arrow_loc~,
      loc~
    ) => {
      let kind = map_list_comprehension_kind(env, kind)
      let guard_ = guard_.map(x => env.0.visit_Expr(x))
      let body = env.0.visit_Expr(body)
      Expr::ListComprehension(
        kind~,
        guard_~,
        body~,
        lbracket_loc~,
        rbracket_loc~,
        if_loc~,
        fat_arrow_loc~,
        loc~,
      )
    }
    ArrayGet(array~, index~, lbracket_loc~, rbracket_loc~, loc~) =>
      env.0.visit_Expr_ArrayGet(
        array~,
        index~,
        lbracket_loc~,
        rbracket_loc~,
        loc~,
      )
    ArrayGetSlice(
      array~,
      start_index~,
      end_index~,
      index_loc~,
      lbracket_loc~,
      colon_loc~,
      rbracket_loc~,
      loc~
    ) =>
      env.0.visit_Expr_ArrayGetSlice(
        array~,
        start_index~,
        end_index~,
        index_loc~,
        lbracket_loc~,
        colon_loc~,
        rbracket_loc~,
        loc~,
      )
    ArraySet(
      array~,
      index~,
      value~,
      lbracket_loc~,
      rbracket_loc~,
      equal_loc~,
      loc~
    ) =>
      env.0.visit_Expr_ArraySet(
        array~,
        index~,
        value~,
        lbracket_loc~,
        rbracket_loc~,
        equal_loc~,
        loc~,
      )
    ArrayAugmentedSet(
      op~,
      array~,
      index~,
      value~,
      lbracket_loc~,
      rbracket_loc~,
      augmented_assign_loc~,
      loc~
    ) =>
      env.0.visit_Expr_ArrayAugmentedSet(
        op~,
        array~,
        index~,
        value~,
        lbracket_loc~,
        rbracket_loc~,
        augmented_assign_loc~,
        loc~,
      )
    Constant(c~, loc~) => env.0.visit_Expr_Constant(c~, loc~)
    MultilineString(elems~, loc~) =>
      env.0.visit_Expr_MultilineString(elems~, loc~)
    Interp(elems~, loc~) => env.0.visit_Expr_Interp(elems~, loc~)
    Constraint(expr~, ty~, lparen_loc~, colon_loc~, rparen_loc~, loc~) =>
      env.0.visit_Expr_Constraint(
        expr~,
        ty~,
        lparen_loc~,
        colon_loc~,
        rparen_loc~,
        loc~,
      )
    Constr(constr~, loc~) => env.0.visit_Expr_Constr(constr~, loc~)
    While(
      loop_cond~,
      loop_body~,
      while_else~,
      label~,
      label_colon_loc~,
      while_loc~,
      body_lbrace_loc~,
      body_rbrace_loc~,
      nobreak_loc~,
      nobreak_lbrace_loc~,
      nobreak_rbrace_loc~,
      loc~
    ) =>
      env.0.visit_Expr_While(
        loop_cond~,
        loop_body~,
        while_else~,
        label~,
        label_colon_loc~,
        while_loc~,
        body_lbrace_loc~,
        body_rbrace_loc~,
        nobreak_loc~,
        nobreak_lbrace_loc~,
        nobreak_rbrace_loc~,
        loc~,
      )
    Function(func~, loc~) => env.0.visit_Expr_Function(func~, loc~)
    Ident(id~, loc~) => env.0.visit_Expr_Ident(id~, loc~)
    If(
      cond~,
      ifso~,
      ifnot~,
      if_loc~,
      then_lbrace_loc~,
      then_rbrace_loc~,
      else_loc~,
      else_lbrace_loc~,
      else_rbrace_loc~,
      loc~
    ) =>
      env.0.visit_Expr_If(
        cond~,
        ifso~,
        ifnot~,
        if_loc~,
        then_lbrace_loc~,
        then_rbrace_loc~,
        else_loc~,
        else_lbrace_loc~,
        else_rbrace_loc~,
        loc~,
      )
    Guard(
      cond~,
      otherwise~,
      body~,
      guard_loc~,
      else_loc~,
      else_lbrace_loc~,
      else_rbrace_loc~,
      semi_loc~,
      loc~
    ) =>
      env.0.visit_Expr_Guard(
        cond~,
        otherwise~,
        body~,
        guard_loc~,
        else_loc~,
        else_lbrace_loc~,
        else_rbrace_loc~,
        semi_loc~,
        loc~,
      )
    Is(expr~, pat~, is_loc~, loc~) =>
      env.0.visit_Expr_Is(expr~, pat~, is_loc~, loc~)
    IsLexMatch(
      expr~,
      strategy~,
      pat~,
      pat_loc~,
      lexmatch_question_loc~,
      with_loc~,
      loc~
    ) =>
      env.0.visit_Expr_IsLexMatch(
        expr~,
        strategy~,
        pat~,
        pat_loc~,
        lexmatch_question_loc~,
        with_loc~,
        loc~,
      )
    RegexMatch(
      expr~,
      pat~,
      bindings~,
      eq_tilde_loc~,
      rhs_lparen_loc~,
      rhs_rparen_loc~,
      binding_comma_locs~,
      binding_equal_locs~,
      loc~
    ) => {
      let expr = env.0.visit_Expr(expr)
      let pat = map_regex_pattern(env, pat)
      let bindings = bindings.map(binding => {
        let (label, binder) = binding
        (env.0.visit_Label(label), binder.map(x => env.0.visit_Binder(x)))
      })
      Expr::RegexMatch(
        expr~,
        pat~,
        bindings~,
        eq_tilde_loc~,
        rhs_lparen_loc~,
        rhs_rparen_loc~,
        binding_comma_locs~,
        binding_equal_locs~,
        loc~,
      )
    }
    Defer(expr~, body~, defer_loc~, semi_loc~, loc~) =>
      env.0.visit_Expr_Defer(expr~, body~, defer_loc~, semi_loc~, loc~)
    LetFn(name~, func~, body~, semi_loc~, loc~) =>
      env.0.visit_Expr_LetFn(name~, func~, body~, semi_loc~, loc~)
    LetAnd(
      bindings~,
      body~,
      letrec_loc~,
      and_locs~,
      equal_locs~,
      annot_colon_locs~,
      semi_loc~,
      loc~
    ) =>
      env.0.visit_Expr_LetAnd(
        bindings~,
        body~,
        letrec_loc~,
        and_locs~,
        equal_locs~,
        annot_colon_locs~,
        semi_loc~,
        loc~,
      )
    Let(
      pattern~,
      expr~,
      body~,
      let_loc~,
      annot_colon_loc~,
      equal_loc~,
      semi_loc~,
      loc~
    ) =>
      env.0.visit_Expr_Let(
        pattern~,
        expr~,
        body~,
        let_loc~,
        annot_colon_loc~,
        equal_loc~,
        semi_loc~,
        loc~,
      )
    Sequence(exprs~, last_expr~, semi_locs~, loc~) =>
      env.0.visit_Expr_Sequence(exprs~, last_expr~, semi_locs~, loc~)
    Tuple(exprs~, lparen_loc~, rparen_loc~, comma_locs~, loc~) =>
      env.0.visit_Expr_Tuple(
        exprs~,
        lparen_loc~,
        rparen_loc~,
        comma_locs~,
        loc~,
      )
    Record(
      type_name~,
      fields~,
      trailing~,
      coloncolon_loc~,
      lbrace_loc~,
      rbrace_loc~,
      comma_locs~,
      semi_locs~,
      loc~
    ) =>
      env.0.visit_Expr_Record(
        type_name~,
        fields~,
        trailing~,
        coloncolon_loc~,
        lbrace_loc~,
        rbrace_loc~,
        comma_locs~,
        semi_locs~,
        loc~,
      )
    RecordUpdate(
      type_name~,
      record~,
      fields~,
      coloncolon_loc~,
      lbrace_loc~,
      dotdot_loc~,
      rbrace_loc~,
      comma_locs~,
      loc~
    ) =>
      env.0.visit_Expr_RecordUpdate(
        type_name~,
        record~,
        fields~,
        coloncolon_loc~,
        lbrace_loc~,
        dotdot_loc~,
        rbrace_loc~,
        comma_locs~,
        loc~,
      )
    Field(record~, accessor~, loc~) =>
      env.0.visit_Expr_Field(record~, accessor~, loc~)
    Method(type_name~, method_name~, coloncolon_loc~, loc~) =>
      env.0.visit_Expr_Method(type_name~, method_name~, coloncolon_loc~, loc~)
    DotApply(
      self~,
      method_name~,
      args~,
      return_self~,
      dot_loc~,
      lparen_loc~,
      rparen_loc~,
      comma_locs~,
      loc~
    ) =>
      env.0.visit_Expr_DotApply(
        self~,
        method_name~,
        args~,
        return_self~,
        dot_loc~,
        lparen_loc~,
        rparen_loc~,
        comma_locs~,
        loc~,
      )
    As(expr~, trait_~, as_loc~, loc~) =>
      env.0.visit_Expr_As(expr~, trait_~, as_loc~, loc~)
    Mutate(
      record~,
      accessor~,
      field~,
      augmented_by~,
      equal_loc~,
      augmented_assign_loc~,
      loc~
    ) =>
      env.0.visit_Expr_Mutate(
        record~,
        accessor~,
        field~,
        augmented_by~,
        equal_loc~,
        augmented_assign_loc~,
        loc~,
      )
    Match(expr~, cases~, match_loc~, lbrace_loc~, rbrace_loc~, semi_locs~, loc~) =>
      env.0.visit_Expr_Match(
        expr~,
        cases~,
        match_loc~,
        lbrace_loc~,
        rbrace_loc~,
        semi_locs~,
        loc~,
      )
    LexMatch(
      strategy~,
      expr~,
      match_loc~,
      with_loc~,
      lbrace_loc~,
      rbrace_loc~,
      semi_locs~,
      cases~,
      loc~
    ) =>
      env.0.visit_Expr_LexMatch(
        strategy~,
        expr~,
        match_loc~,
        with_loc~,
        lbrace_loc~,
        rbrace_loc~,
        semi_locs~,
        cases~,
        loc~,
      )
    LetMut(
      binder~,
      ty~,
      expr~,
      body~,
      let_loc~,
      mut_loc~,
      annot_colon_loc~,
      equal_loc~,
      semi_loc~,
      loc~
    ) =>
      env.0.visit_Expr_LetMut(
        binder~,
        ty~,
        expr~,
        body~,
        let_loc~,
        mut_loc~,
        annot_colon_loc~,
        equal_loc~,
        semi_loc~,
        loc~,
      )
    Pipe(lhs~, rhs~, pipe_loc~, loc~) =>
      env.0.visit_Expr_Pipe(lhs~, rhs~, pipe_loc~, loc~)
    RevPipe(lhs~, rhs~, rev_pipe_loc~, loc~) => {
      let lhs = env.0.visit_Expr(lhs)
      let rhs = env.0.visit_Expr(rhs)
      Expr::RevPipe(lhs~, rhs~, rev_pipe_loc~, loc~)
    }
    Assign(var_~, expr~, augmented_by~, equal_loc~, augmented_assign_loc~, loc~) =>
      env.0.visit_Expr_Assign(
        var_~,
        expr~,
        augmented_by~,
        equal_loc~,
        augmented_assign_loc~,
        loc~,
      )
    Hole(loc~, kind~) => env.0.visit_Expr_Hole(loc~, kind~)
    Return(return_value~, return_loc~, loc~) =>
      env.0.visit_Expr_Return(return_value~, return_loc~, loc~)
    Raise(err_value~, raise_loc~, loc~) =>
      env.0.visit_Expr_Raise(err_value~, raise_loc~, loc~)
    Quantifier(
      kind~,
      binder~,
      binder_ty~,
      body~,
      quantifier_loc~,
      colon_loc~,
      comma_loc~,
      loc~
    ) => {
      let binder = env.0.visit_Binder(binder)
      let binder_ty = env.0.visit_Type(binder_ty)
      let body = env.0.visit_Expr(body)
      Expr::Quantifier(
        kind~,
        binder~,
        binder_ty~,
        body~,
        quantifier_loc~,
        colon_loc~,
        comma_loc~,
        loc~,
      )
    }
    Implies(lhs~, rhs~, implies_loc~, loc~) => {
      let lhs = env.0.visit_Expr(lhs)
      let rhs = env.0.visit_Expr(rhs)
      Expr::Implies(lhs~, rhs~, implies_loc~, loc~)
    }
    ProofAssert(expr~, proof_assert_loc~, loc~) => {
      let expr = env.0.visit_Expr(expr)
      Expr::ProofAssert(expr~, proof_assert_loc~, loc~)
    }
    ProofLet(binder~, expr~, proof_let_loc~, equal_loc~, loc~) => {
      let binder = env.0.visit_Binder(binder)
      let expr = env.0.visit_Expr(expr)
      Expr::ProofLet(binder~, expr~, proof_let_loc~, equal_loc~, loc~)
    }
    Unit(loc~, faked~, lparen_loc~, rparen_loc~) =>
      env.0.visit_Expr_Unit(loc~, faked~, lparen_loc~, rparen_loc~)
    Break(arg~, label~, break_loc~, post_label_loc~, loc~) =>
      env.0.visit_Expr_Break(arg~, label~, break_loc~, post_label_loc~, loc~)
    Continue(args~, label~, continue_loc~, post_label_loc~, comma_locs~, loc~) =>
      env.0.visit_Expr_Continue(
        args~,
        label~,
        continue_loc~,
        post_label_loc~,
        comma_locs~,
        loc~,
      )
    For(
      binders~,
      condition~,
      continue_block~,
      body~,
      for_else~,
      label~,
      label_colon_loc~,
      for_loc~,
      binder_comma_locs~,
      binder_equal_locs~,
      semi_locs~,
      continue_comma_locs~,
      continue_equal_locs~,
      body_lbrace_loc~,
      body_rbrace_loc~,
      nobreak_loc~,
      nobreak_lbrace_loc~,
      nobreak_rbrace_loc~,
      loc~,
      where_clause~
    ) =>
      env.0.visit_Expr_For(
        binders~,
        condition~,
        continue_block~,
        body~,
        for_else~,
        label~,
        label_colon_loc~,
        for_loc~,
        binder_comma_locs~,
        binder_equal_locs~,
        semi_locs~,
        continue_comma_locs~,
        continue_equal_locs~,
        body_lbrace_loc~,
        body_rbrace_loc~,
        nobreak_loc~,
        nobreak_lbrace_loc~,
        nobreak_rbrace_loc~,
        loc~,
        where_clause~,
      )
    ForEach(
      binders~,
      expr~,
      init~,
      continue_block~,
      body~,
      else_block~,
      where_clause~,
      label~,
      label_colon_loc~,
      for_loc~,
      binder_comma_locs~,
      in_loc~,
      semi_locs~,
      init_comma_locs~,
      init_equal_locs~,
      continue_comma_locs~,
      continue_equal_locs~,
      body_lbrace_loc~,
      body_rbrace_loc~,
      nobreak_loc~,
      nobreak_lbrace_loc~,
      nobreak_rbrace_loc~,
      loc~
    ) =>
      env.0.visit_Expr_ForEach(
        binders~,
        expr~,
        init~,
        continue_block~,
        body~,
        else_block~,
        where_clause~,
        label~,
        label_colon_loc~,
        for_loc~,
        binder_comma_locs~,
        in_loc~,
        semi_locs~,
        init_comma_locs~,
        init_equal_locs~,
        continue_comma_locs~,
        continue_equal_locs~,
        body_lbrace_loc~,
        body_rbrace_loc~,
        nobreak_loc~,
        nobreak_lbrace_loc~,
        nobreak_rbrace_loc~,
        loc~,
      )
    Try(
      body~,
      catch_~,
      try_else~,
      has_try~,
      try_loc~,
      catch_loc~,
      else_loc~,
      catch_lbrace_loc~,
      catch_rbrace_loc~,
      catch_semi_locs~,
      noraise_lbrace_loc~,
      noraise_rbrace_loc~,
      noraise_semi_locs~,
      loc~
    ) =>
      env.0.visit_Expr_Try(
        body~,
        catch_~,
        try_else~,
        has_try~,
        try_loc~,
        catch_loc~,
        else_loc~,
        catch_lbrace_loc~,
        catch_rbrace_loc~,
        catch_semi_locs~,
        noraise_lbrace_loc~,
        noraise_rbrace_loc~,
        noraise_semi_locs~,
        loc~,
      )
    TryOperator(body~, kind~, try_loc~, loc~) =>
      env.0.visit_Expr_TryOperator(body~, kind~, try_loc~, loc~)
    Map(elems~, lbrace_loc~, rbrace_loc~, comma_locs~, loc~) =>
      env.0.visit_Expr_Map(elems~, lbrace_loc~, rbrace_loc~, comma_locs~, loc~)
    Group(expr~, group~, left_delim_loc~, right_delim_loc~, loc~) =>
      env.0.visit_Expr_Group(
        expr~,
        group~,
        left_delim_loc~,
        right_delim_loc~,
        loc~,
      )
    TemplateWriting(expr~, template~, lt_plus_loc~, loc~) => {
      let expr = env.0.visit_Expr(expr)
      let template = env.0.visit_Expr(template)
      Expr::TemplateWriting(expr~, template~, lt_plus_loc~, loc~)
    }
    StaticAssert(asserts~, body~) =>
      env.0.visit_Expr_StaticAssert(asserts~, body~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_UsingKind(
  env,
  using_kind,
) {
  ignore(env)
  return using_kind
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_IsLexMatch(
  env,
  expr~,
  strategy~,
  pat~,
  pat_loc~,
  lexmatch_question_loc~,
  with_loc~,
  loc~,
) {
  let strategy = strategy.map(strategy => env.0.visit_Label(strategy))
  let pat = pat.map(pat => env.0.visit_LexTopPattern(pat))
  let expr = env.0.visit_Expr(expr)
  IsLexMatch(
    strategy~,
    expr~,
    pat~,
    pat_loc~,
    lexmatch_question_loc~,
    with_loc~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_Expr_LexMatch(
  env,
  strategy~,
  expr~,
  match_loc~,
  with_loc~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
  cases~,
  loc~,
) {
  let strategy = strategy.map(strategy => env.0.visit_Label(strategy))
  let expr = env.0.visit_Expr(expr)
  let cases = cases.map(case => env.0.visit_LexCase(case))
  Expr::LexMatch(
    strategy~,
    expr~,
    match_loc~,
    with_loc~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
    cases~,
    loc~,
  )
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexCase(
  env,
  lex_case,
) {
  let pat = lex_case.pat.map(pat => env.0.visit_LexTopPattern(pat))
  let guard_ = lex_case.guard_.map(guard_ => env.0.visit_Expr(guard_))
  let body = env.0.visit_Expr(lex_case.body)
  let pat_loc = lex_case.pat_loc
  let if_loc = lex_case.if_loc
  let fat_arrow_loc = lex_case.fat_arrow_loc
  let ellipsis_loc = lex_case.ellipsis_loc
  { pat, pat_loc, guard_, body, if_loc, fat_arrow_loc, ellipsis_loc }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexTopPattern(
  env,
  lex_top_pattern,
) {
  match lex_top_pattern {
    Pattern(lex_pattern) => env.0.visit_LexTopPattern_Pattern(lex_pattern)
    Binder(binder) => env.0.visit_LexTopPattern_Binder(binder)
    Wildcard(loc~) => env.0.visit_LexTopPattern_Wildcard(loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexTopPattern_Pattern(
  env,
  lex_pattern,
) {
  let lex_pattern = env.0.visit_LexPattern(lex_pattern)
  LexTopPattern::Pattern(lex_pattern)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexTopPattern_Binder(
  env,
  binder,
) {
  let binder = env.0.visit_Binder(binder)
  LexTopPattern::Binder(binder)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexTopPattern_Wildcard(
  env,
  loc~,
) {
  ignore(env)
  LexTopPattern::Wildcard(loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexPattern(
  env,
  lex_pattern,
) {
  match lex_pattern {
    Alias(pat~, binder~, as_loc~, loc~) =>
      env.0.visit_LexPattern_Alias(pat~, binder~, as_loc~, loc~)
    RegexInterp(elems~, loc~) =>
      env.0.visit_LexPattern_RegexInterp(elems~, loc~)
    Regex(lit~, offset~, loc~) =>
      env.0.visit_LexPattern_Regex(lit~, offset~, loc~)
    ConstantRef(lid~, loc~) => env.0.visit_LexPattern_ConstantRef(lid~, loc~)
    Sequence(pats~, semi_locs~, loc~) =>
      env.0.visit_LexPattern_Sequence(pats~, semi_locs~, loc~)
  }
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexPattern_Regex(
  env,
  lit~,
  offset~,
  loc~,
) {
  ignore(env)
  LexPattern::Regex(lit~, offset~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexPattern_ConstantRef(
  env,
  lid~,
  loc~,
) {
  let lid = env.0.visit_LongIdent(lid)
  LexPattern::ConstantRef(lid~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexPattern_RegexInterp(
  env,
  elems~,
  loc~,
) {
  let elems = elems.map(elem => env.0.visit_InterpElem(elem))
  LexPattern::RegexInterp(elems~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexPattern_Alias(
  env,
  pat~,
  binder~,
  as_loc~,
  loc~,
) {
  let pat = env.0.visit_LexPattern(pat)
  let binder = env.0.visit_Binder(binder)
  LexPattern::Alias(pat~, binder~, as_loc~, loc~)
}

///|
pub impl[T : MapVisitor] MapVisitor for MapVisitorBase[T] with fn visit_LexPattern_Sequence(
  env,
  pats~,
  semi_locs~,
  loc~,
) {
  let pats = pats.map(pat => env.0.visit_LexPattern(pat))
  LexPattern::Sequence(pats~, semi_locs~, loc~)
}
