///|
pub(open) trait IterVisitor {
  fn base(Self) -> IterVisitorBase[Self] = _

  // Visibility
  fn visit_Visibility(Self, Visibility) -> Unit = _
  fn visit_Visibility_Pub(Self, attr~ : String?, loc~ : Location) -> Unit = _
  fn visit_Visibility_Priv(Self, loc~ : Location) -> Unit = _

  // Constant
  fn visit_Constant(Self, Constant) -> Unit = _

  // Label
  fn visit_Label(Self, Label) -> Unit = _

  // ConstrName
  fn visit_ConstrName(Self, ConstrName) -> Unit = _

  // LongIdent
  fn visit_LongIdent(Self, LongIdent) -> Unit = _
  fn visit_LongIdent_Ident(Self, name~ : String) -> Unit = _
  fn visit_LongIdent_Dot(
    Self,
    pkg~ : String,
    id~ : String,
    pkg_loc~ : Location,
    dot_id_loc~ : Location,
  ) -> Unit = _

  // TypeName
  fn visit_TypeName(Self, TypeName) -> Unit = _

  // ConstrId
  fn visit_ConstrId(Self, ConstrId) -> Unit = _

  // Type
  fn visit_Type(Self, Type) -> Unit = _
  fn visit_Type_Any(Self, loc~ : Location) -> Unit = _
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
  ) -> Unit = _
  fn visit_Type_Name(
    Self,
    constr_id~ : ConstrId,
    tys~ : @list.List[Type],
    type_args_lbracket_loc~ : Location?,
    type_args_rbracket_loc~ : Location?,
    type_arg_comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
  fn visit_Type_Tuple(
    Self,
    tys~ : @list.List[Type],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
  fn visit_Type_Option(
    Self,
    ty~ : Type,
    loc~ : Location,
    question_loc~ : Location,
  ) -> Unit = _
  fn visit_Type_Object(
    Self,
    constr_id~ : ConstrId,
    amp_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _

  // ErrorType
  fn visit_ErrorType(Self, ErrorType) -> Unit = _
  fn visit_ErrorType_ErrorType(
    Self,
    ty~ : Type,
    raise_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_ErrorType_DefaultErrorType(Self, loc~ : Location) -> Unit = _
  fn visit_ErrorType_Noraise(Self, loc~ : Location) -> Unit = _
  fn visit_ErrorType_MaybeError(
    Self,
    ty~ : Type,
    raise_loc~ : Location,
    question_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _

  // ConstrParam
  fn visit_ConstrParam(Self, ConstrParam) -> Unit = _

  // ConstrDecl
  fn visit_ConstrDecl(Self, ConstrDecl) -> Unit = _

  // ExceptionDecl
  fn visit_ExceptionDecl(Self, ExceptionDecl) -> Unit = _
  fn visit_ExceptionDecl_EnumPayload(
    Self,
    constrs~ : @list.List[ConstrDecl],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
  ) -> Unit = _

  // FieldName
  fn visit_FieldName(Self, FieldName) -> Unit = _

  // FieldDecl
  fn visit_FieldDecl(Self, FieldDecl) -> Unit = _

  // TypeDesc
  fn visit_TypeDesc(Self, TypeDesc) -> Unit = _
  fn visit_TypeDesc_Error(Self, ExceptionDecl) -> Unit = _
  fn visit_TypeDesc_Variant(
    Self,
    constrs~ : @list.List[ConstrDecl],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
  ) -> Unit = _
  fn visit_TypeDesc_Record(
    Self,
    fields~ : @list.List[FieldDecl],
    constr_decl~ : FunDecl?,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
  ) -> Unit = _
  fn visit_TypeDesc_TupleStruct(
    Self,
    types~ : @list.List[Type],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
  ) -> Unit = _
  fn visit_TypeDesc_Alias(Self, type_~ : Type, equal_loc~ : Location) -> Unit = _

  // Hole
  fn visit_Hole(Self, Hole) -> Unit = _

  // ArgumentKind
  fn visit_ArgumentKind(Self, ArgumentKind) -> Unit = _
  fn visit_ArgumentKind_Labelled(Self, label~ : Label, equal_loc~ : Location) -> Unit = _
  fn visit_ArgumentKind_LabelledPun(Self, Label) -> Unit = _
  fn visit_ArgumentKind_LabelledOption(
    Self,
    label~ : Label,
    question_loc~ : Location,
    equal_loc~ : Location,
  ) -> Unit = _
  fn visit_ArgumentKind_LabelledOptionPun(
    Self,
    label~ : Label,
    question_loc~ : Location,
  ) -> Unit = _

  // FnKind
  fn visit_FnKind(Self, FnKind) -> Unit = _

  // Group
  fn visit_Group(Self, Group) -> Unit = _

  // TrailingMark
  fn visit_TrailingMark(Self, TrailingMark) -> Unit = _

  // TypeVarConstraint
  fn visit_TypeVarConstraint(Self, TypeVarConstraint) -> Unit = _

  // TypeVarBinder
  fn visit_TypeVarBinder(Self, TypeVarBinder) -> Unit = _

  // TypeDeclBinder
  fn visit_TypeDeclBinder(Self, TypeDeclBinder) -> Unit = _

  // Binder
  fn visit_Binder(Self, Binder) -> Unit = _

  // Var
  fn visit_Var(Self, Var) -> Unit = _

  // ConstructorExtraInfo
  fn visit_ConstructorExtraInfo(Self, ConstructorExtraInfo) -> Unit = _
  fn visit_ConstructorExtraInfo_TypeName(
    Self,
    type_name~ : TypeName,
    coloncolon_loc~ : Location,
  ) -> Unit = _
  fn visit_ConstructorExtraInfo_Package(
    Self,
    pkg~ : String,
    pkg_loc~ : Location,
    dot_constr_loc~ : Location,
  ) -> Unit = _

  // Constructor
  fn visit_Constructor(Self, Constructor) -> Unit = _

  // Accessor
  fn visit_Accessor(Self, Accessor) -> Unit = _
  fn visit_Accessor_Label(Self, Label) -> Unit = _
  fn visit_Accessor_Index(Self, tuple_index~ : Int, loc~ : Location) -> Unit = _
  fn visit_Accessor_Newtype(Self, loc~ : Location) -> Unit = _

  // AliasTarget
  fn visit_AliasTarget(Self, AliasTarget) -> Unit = _

  // Argument
  fn visit_Argument(Self, Argument) -> Unit = _

  // Parameter
  fn visit_Parameter(Self, Parameter) -> Unit = _
  fn visit_Parameter_DiscardPositional(
    Self,
    ty~ : Type?,
    colon_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Parameter_Positional(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    colon_loc~ : Location?,
  ) -> Unit = _
  fn visit_Parameter_Labelled(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    post_label_loc~ : Location?,
    colon_loc~ : Location?,
  ) -> Unit = _
  fn visit_Parameter_Optional(
    Self,
    binder~ : Binder,
    default~ : Expr,
    ty~ : Type?,
    post_label_loc~ : Location?,
    question_loc~ : Location?,
    colon_loc~ : Location?,
    equal_loc~ : Location,
  ) -> Unit = _
  fn visit_Parameter_QuestionOptional(
    Self,
    binder~ : Binder,
    ty~ : Type?,
    question_loc~ : Location,
    colon_loc~ : Location?,
  ) -> Unit = _

  // Case
  fn visit_Case(Self, Case) -> Unit = _

  // LexCase
  fn visit_LexCase(Self, LexCase) -> Unit = _

  // LexTopPattern
  fn visit_LexTopPattern(Self, LexTopPattern) -> Unit = _
  fn visit_LexTopPattern_Pattern(Self, LexPattern) -> Unit = _
  fn visit_LexTopPattern_Binder(Self, Binder) -> Unit = _
  fn visit_LexTopPattern_Wildcard(Self, loc~ : Location) -> Unit = _

  // LexPattern
  fn visit_LexPattern(Self, LexPattern) -> Unit = _
  fn visit_LexPattern_Regex(Self, lit~ : String, offset~ : Int, loc~ : Location) -> Unit = _
  fn visit_LexPattern_RegexInterp(
    Self,
    elems~ : @list.List[InterpElem],
    loc~ : Location,
  ) -> Unit = _
  fn visit_LexPattern_ConstantRef(Self, lid~ : LongIdent, loc~ : Location) -> Unit = _
  fn visit_LexPattern_Alias(
    Self,
    pat~ : LexPattern,
    binder~ : Binder,
    as_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_LexPattern_Sequence(
    Self,
    pats~ : @list.List[LexPattern],
    semi_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _

  // SpreadableElem
  fn visit_SpreadableElem(Self, SpreadableElem) -> Unit = _
  fn visit_SpreadableElem_Regular(Self, Expr) -> Unit = _
  fn visit_SpreadableElem_Spread(
    Self,
    expr~ : Expr,
    dotdot_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _

  // MapExprElem
  fn visit_MapExprElem(Self, MapExprElem) -> Unit = _

  // StaticAssertion
  fn visit_StaticAssertion(Self, StaticAssertion) -> Unit = _

  // Func
  fn visit_Func(Self, Func) -> Unit = _

  // FieldDef
  fn visit_FieldDef(Self, FieldDef) -> Unit = _

  // InterpElem
  fn visit_InterpElem(Self, InterpElem) -> Unit = _
  fn visit_InterpElem_Literal(Self, repr~ : StringLiteral, loc~ : Location) -> Unit = _
  fn visit_InterpElem_Expr(Self, expr~ : Expr, loc~ : Location) -> Unit = _
  fn visit_InterpElem_Source(Self, InterpSource) -> Unit = _

  // MultilineStringElem
  fn visit_MultilineStringElem(Self, MultilineStringElem) -> Unit = _
  fn visit_MultilineStringElem_String(Self, String) -> Unit = _
  fn visit_MultilineStringElem_Interp(Self, @list.List[InterpElem]) -> Unit = _

  // TryOperatorKind
  fn visit_TryOperatorKind(Self, TryOperatorKind) -> Unit = _

  // DotDotBinder
  fn visit_DotDotBinder(Self, DotDotBinder) -> Unit = _
  fn visit_DotDotBinder_Underscore(
    Self,
    dotdot_loc~ : Location,
    underscore_loc~ : Location,
  ) -> Unit = _
  fn visit_DotDotBinder_NoBinder(Self, dotdot_loc~ : Location) -> Unit = _
  fn visit_DotDotBinder_BinderAs(
    Self,
    binder~ : Binder,
    dotdot_loc~ : Location,
    as_loc~ : Location,
  ) -> Unit = _
  fn visit_DotDotBinder_Binder(Self, binder~ : Binder, dotdot_loc~ : Location) -> Unit = _

  // ArrayPattern
  fn visit_ArrayPattern(Self, ArrayPattern) -> Unit = _
  fn visit_ArrayPattern_Pattern(Self, Pattern) -> Unit = _
  fn visit_ArrayPattern_StringSpread(
    Self,
    str~ : StringLiteral,
    dotdot_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_ArrayPattern_BytesSpread(
    Self,
    bytes~ : BytesLiteral,
    dotdot_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_ArrayPattern_ConstSpread(
    Self,
    binder~ : Binder,
    pkg~ : String?,
    dotdot_loc~ : Location,
    pkg_loc~ : Location?,
    dot_uident_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _

  // ArrayPatterns
  fn visit_ArrayPatterns(Self, ArrayPatterns) -> Unit = _
  fn visit_ArrayPatterns_Closed(
    Self,
    @list.List[ArrayPattern],
    comma_locs~ : @list.List[Location],
  ) -> Unit = _
  fn visit_ArrayPatterns_Open(
    Self,
    @list.List[ArrayPattern],
    @list.List[ArrayPattern],
    DotDotBinder,
    left_comma_locs~ : @list.List[Location],
    dotdot_comma_loc~ : Location?,
    right_comma_locs~ : @list.List[Location],
  ) -> Unit = _

  // WhereClause
  fn visit_WhereClause(Self, WhereClause) -> Unit = _

  // FieldPat
  fn visit_FieldPat(Self, FieldPat) -> Unit = _

  // ConstrPatArg
  fn visit_ConstrPatArg(Self, ConstrPatArg) -> Unit = _

  // MapPatElem
  fn visit_MapPatElem(Self, MapPatElem) -> Unit = _

  // RangeKind
  fn visit_RangeKind(Self, RangeKind) -> Unit = _

  // Pattern
  fn visit_Pattern(Self, Pattern) -> Unit = _
  fn visit_Pattern_Alias(
    Self,
    pat~ : Pattern,
    alias_~ : Binder,
    as_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Pattern_Any(Self, loc~ : Location) -> Unit = _
  fn visit_Pattern_Array(
    Self,
    pats~ : ArrayPatterns,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Pattern_Constant(Self, c~ : Constant, loc~ : Location) -> Unit = _
  fn visit_Pattern_Constraint(
    Self,
    pat~ : Pattern,
    ty~ : Type,
    lparen_loc~ : Location?,
    colon_loc~ : Location,
    rparen_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Pattern_Or(
    Self,
    pat1~ : Pattern,
    pat2~ : Pattern,
    bar_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Pattern_Tuple(
    Self,
    pats~ : @list.List[Pattern],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
  fn visit_Pattern_Var(Self, Binder) -> Unit = _
  fn visit_Pattern_Record(
    Self,
    fields~ : @list.List[FieldPat],
    is_closed~ : Bool,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    dotdot_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Pattern_Map(
    Self,
    elems~ : @list.List[MapPatElem],
    is_closed~ : Bool,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    dotdot_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Pattern_Range(
    Self,
    lhs~ : Pattern,
    rhs~ : Pattern,
    kind~ : RangeKind,
    range_op_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Pattern_SpecialConstr(
    Self,
    binder~ : Binder,
    args~ : @list.List[ConstrPatArg],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _

  // DerivingDirective
  fn visit_DerivingDirective(Self, DerivingDirective) -> Unit = _

  // TypeDecl
  fn visit_TypeDecl(Self, TypeDecl) -> Unit = _

  // FuncStubs
  fn visit_FuncStubs(Self, FuncStubs) -> Unit = _
  fn visit_FuncStubs_Import(
    Self,
    module_name~ : StringLiteral,
    func_name~ : StringLiteral,
    language~ : StringLiteral?,
    module_name_loc~ : Location,
    func_name_loc~ : Location,
    language_loc~ : Location?,
  ) -> Unit = _
  fn visit_FuncStubs_Embedded(
    Self,
    language~ : StringLiteral?,
    code~ : EmbeddedCode,
    language_loc~ : Location?,
    code_loc~ : Location,
  ) -> Unit = _

  // EmbeddedCode
  fn visit_EmbeddedCode(Self, EmbeddedCode) -> Unit = _
  fn visit_EmbeddedCode_CodeString(Self, StringLiteral, loc~ : Location) -> Unit = _
  fn visit_EmbeddedCode_CodeMultilineString(
    Self,
    @list.List[String],
    string_locs~ : @list.List[Location],
  ) -> Unit = _

  // DeclBody
  fn visit_DeclBody(Self, DeclBody) -> Unit = _
  fn visit_DeclBody_DeclBody(
    Self,
    expr~ : Expr,
    lbrace_loc~ : Location?,
    rbrace_loc~ : Location?,
  ) -> Unit = _
  fn visit_DeclBody_DeclStubs(Self, FuncStubs, equal_loc~ : Location) -> Unit = _

  // FuncDecl
  fn visit_FunDecl(Self, FunDecl) -> Unit = _

  // TraitMethodDecl
  fn visit_TraitMethodDecl(Self, TraitMethodDecl) -> Unit = _

  // TraitDecl
  fn visit_TraitDecl(Self, TraitDecl) -> Unit = _

  // Impl
  fn visit_Impl(Self, Impl) -> Unit = _
  fn visit_Impl_TopExpr(
    Self,
    expr~ : Expr,
    is_main~ : Bool,
    is_async~ : Location?,
    loc~ : Location,
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Impl_TopTypeDef(Self, TypeDecl) -> Unit = _
  fn visit_Impl_TopFuncDef(
    Self,
    fun_decl~ : FunDecl,
    decl_body~ : DeclBody,
    where_clause~ : WhereClause?,
    equal_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Impl_TopTrait(Self, TraitDecl) -> Unit = _
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
  ) -> Unit = _
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
  ) -> Unit = _
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
  ) -> Unit = _
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
  ) -> Unit = _

  // UsingKind
  fn visit_UsingKind(Self, UsingKind) -> Unit = _

  // Expr
  fn visit_Expr(Self, Expr) -> Unit = _
  fn visit_Expr_Apply(
    Self,
    func~ : Expr,
    args~ : @list.List[Argument],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Infix(
    Self,
    op~ : Var,
    lhs~ : Expr,
    rhs~ : Expr,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Unary(Self, op~ : Var, expr~ : Expr, loc~ : Location) -> Unit = _
  fn visit_Expr_Array(
    Self,
    exprs~ : @list.List[Expr],
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_ArraySpread(
    Self,
    elems~ : @list.List[SpreadableElem],
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_ArrayGet(
    Self,
    array~ : Expr,
    index~ : Expr,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Expr_ArraySet(
    Self,
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    equal_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Expr_Constant(Self, c~ : Constant, loc~ : Location) -> Unit = _
  fn visit_Expr_MultilineString(
    Self,
    elems~ : @list.List[MultilineStringElem],
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Interp(Self, elems~ : @list.List[InterpElem], loc~ : Location) -> Unit = _
  fn visit_Expr_Constraint(
    Self,
    expr~ : Expr,
    ty~ : Type,
    lparen_loc~ : Location?,
    colon_loc~ : Location,
    rparen_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Constr(Self, constr~ : Constructor, loc~ : Location) -> Unit = _
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
  ) -> Unit = _
  fn visit_Expr_Function(Self, func~ : Func, loc~ : Location) -> Unit = _
  fn visit_Expr_Ident(Self, id~ : Var, loc~ : Location) -> Unit = _
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
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Expr_Is(
    Self,
    expr~ : Expr,
    pat~ : Pattern,
    is_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_IsLexMatch(
    Self,
    expr~ : Expr,
    strategy~ : Label?,
    pat~ : @list.List[LexTopPattern],
    pat_loc~ : Location,
    lexmatch_question_loc~ : Location,
    with_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Defer(
    Self,
    expr~ : Expr,
    body~ : Expr,
    defer_loc~ : Location,
    semi_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_LetFn(
    Self,
    name~ : Binder,
    func~ : Func,
    body~ : Expr,
    semi_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
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
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Expr_Sequence(
    Self,
    exprs~ : @list.List[Expr],
    last_expr~ : Expr,
    semi_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Tuple(
    Self,
    exprs~ : @list.List[Expr],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
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
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Expr_Field(
    Self,
    record~ : Expr,
    accessor~ : Accessor,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Method(
    Self,
    type_name~ : TypeName,
    method_name~ : Label,
    coloncolon_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Expr_As(
    Self,
    expr~ : Expr,
    trait_~ : TypeName,
    as_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Mutate(
    Self,
    record~ : Expr,
    accessor~ : Accessor,
    field~ : Expr,
    augmented_by~ : Var?,
    equal_loc~ : Location?,
    augmented_assign_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Match(
    Self,
    expr~ : Expr,
    cases~ : @list.List[Case],
    match_loc~ : Location,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
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
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Expr_Pipe(
    Self,
    lhs~ : Expr,
    rhs~ : Expr,
    pipe_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Assign(
    Self,
    var_~ : Var,
    expr~ : Expr,
    augmented_by~ : Var?,
    equal_loc~ : Location?,
    augmented_assign_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Hole(Self, loc~ : Location, kind~ : Hole) -> Unit = _
  fn visit_Expr_Return(
    Self,
    return_value~ : Expr?,
    return_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Raise(
    Self,
    err_value~ : Expr,
    raise_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Unit(
    Self,
    loc~ : Location,
    faked~ : Bool,
    lparen_loc~ : Location,
    rparen_loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Break(
    Self,
    arg~ : Expr?,
    label~ : Label?,
    break_loc~ : Location,
    post_label_loc~ : Location?,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Continue(
    Self,
    args~ : @list.List[Expr],
    label~ : Label?,
    continue_loc~ : Location,
    post_label_loc~ : Location?,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
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
  ) -> Unit = _
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
  ) -> Unit = _
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
  ) -> Unit = _
  fn visit_Expr_TryOperator(
    Self,
    body~ : Expr,
    kind~ : TryOperatorKind,
    try_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Map(
    Self,
    elems~ : @list.List[MapExprElem],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_Group(
    Self,
    expr~ : Expr,
    group~ : Group,
    left_delim_loc~ : Location,
    right_delim_loc~ : Location,
    loc~ : Location,
  ) -> Unit = _
  fn visit_Expr_StaticAssert(
    Self,
    asserts~ : @list.List[StaticAssertion],
    body~ : Expr,
  ) -> Unit = _
}

///|
impl IterVisitor with fn base(self) -> IterVisitorBase[Self] {
  IterVisitorBase::IterVisitorBase(self)
}

///|
impl IterVisitor with fn visit_Impl(env, impl_) {
  IterVisitorBase::visit_Impl(env, impl_)
}

///|
impl IterVisitor with fn visit_Impl_TopExpr(
  env,
  expr~,
  is_main~,
  is_async~,
  loc~,
) {
  IterVisitorBase::visit_Impl_TopExpr(env, expr~, is_main~, is_async~, loc~)
}

///|
impl IterVisitor with fn visit_Impl_TopTest(
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
  IterVisitorBase::visit_Impl_TopTest(
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
impl IterVisitor with fn visit_Impl_TopTypeDef(env, type_decl) {
  IterVisitorBase::visit_Impl_TopTypeDef(env, type_decl)
}

///|
impl IterVisitor with fn visit_Impl_TopFuncDef(
  env,
  fun_decl~,
  decl_body~,
  where_clause~,
  equal_loc~,
  loc~,
) {
  IterVisitorBase::visit_Impl_TopFuncDef(
    env,
    fun_decl~,
    decl_body~,
    where_clause~,
    equal_loc~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Impl_TopLetDef(
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
  IterVisitorBase::visit_Impl_TopLetDef(
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
impl IterVisitor with fn visit_Impl_TopTrait(env, trait_decl) {
  IterVisitorBase::visit_Impl_TopTrait(env, trait_decl)
}

///|
impl IterVisitor with fn visit_Impl_TopImpl(
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
  IterVisitorBase::visit_Impl_TopImpl(
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
impl IterVisitor with fn visit_Impl_TopView(
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
  IterVisitorBase::visit_Impl_TopView(
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
impl IterVisitor with fn visit_Impl_TopImplRelation(
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
  IterVisitorBase::visit_Impl_TopImplRelation(
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
impl IterVisitor with fn visit_Impl_TopUsing(
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
  IterVisitorBase::visit_Impl_TopUsing(
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
impl IterVisitor with fn visit_UsingKind(env, using_kind) {
  IterVisitorBase::visit_UsingKind(env, using_kind)
}

///|
impl IterVisitor with fn visit_Expr(env, expr) {
  IterVisitorBase::visit_Expr(env, expr)
}

///|
impl IterVisitor with fn visit_Expr_Apply(
  env,
  func~,
  args~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Apply(
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
impl IterVisitor with fn visit_Expr_Infix(env, op~, lhs~, rhs~, loc~) {
  IterVisitorBase::visit_Expr_Infix(env, op~, lhs~, rhs~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Unary(env, op~, expr~, loc~) {
  IterVisitorBase::visit_Expr_Unary(env, op~, expr~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Array(
  env,
  exprs~,
  lbracket_loc~,
  rbracket_loc~,
  comma_locs~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Array(
    env,
    exprs~,
    lbracket_loc~,
    rbracket_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Expr_ArraySpread(
  env,
  elems~,
  lbracket_loc~,
  rbracket_loc~,
  comma_locs~,
  loc~,
) {
  IterVisitorBase::visit_Expr_ArraySpread(
    env,
    elems~,
    lbracket_loc~,
    rbracket_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Expr_ArrayGet(
  env,
  array~,
  index~,
  lbracket_loc~,
  rbracket_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_ArrayGet(
    env,
    array~,
    index~,
    lbracket_loc~,
    rbracket_loc~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Expr_ArrayGetSlice(
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
  IterVisitorBase::visit_Expr_ArrayGetSlice(
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
impl IterVisitor with fn visit_Expr_ArraySet(
  env,
  array~,
  index~,
  value~,
  lbracket_loc~,
  rbracket_loc~,
  equal_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_ArraySet(
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
impl IterVisitor with fn visit_Expr_ArrayAugmentedSet(
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
  IterVisitorBase::visit_Expr_ArrayAugmentedSet(
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
impl IterVisitor with fn visit_Expr_Constant(env, c~, loc~) {
  IterVisitorBase::visit_Expr_Constant(env, c~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_MultilineString(env, elems~, loc~) {
  IterVisitorBase::visit_Expr_MultilineString(env, elems~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Interp(env, elems~, loc~) {
  IterVisitorBase::visit_Expr_Interp(env, elems~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Constraint(
  env,
  expr~,
  ty~,
  lparen_loc~,
  colon_loc~,
  rparen_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Constraint(
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
impl IterVisitor with fn visit_Expr_Constr(env, constr~, loc~) {
  IterVisitorBase::visit_Expr_Constr(env, constr~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_While(
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
  IterVisitorBase::visit_Expr_While(
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
impl IterVisitor with fn visit_Expr_Function(env, func~, loc~) {
  IterVisitorBase::visit_Expr_Function(env, func~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Ident(env, id~, loc~) {
  IterVisitorBase::visit_Expr_Ident(env, id~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_If(
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
  IterVisitorBase::visit_Expr_If(
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
impl IterVisitor with fn visit_Expr_Guard(
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
  IterVisitorBase::visit_Expr_Guard(
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
impl IterVisitor with fn visit_Expr_Is(env, expr~, pat~, is_loc~, loc~) {
  IterVisitorBase::visit_Expr_Is(env, expr~, pat~, is_loc~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Defer(
  env,
  expr~,
  body~,
  defer_loc~,
  semi_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Defer(
    env,
    expr~,
    body~,
    defer_loc~,
    semi_loc~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Expr_LetFn(
  env,
  name~,
  func~,
  body~,
  semi_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_LetFn(env, name~, func~, body~, semi_loc~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_LetAnd(
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
  IterVisitorBase::visit_Expr_LetAnd(
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
impl IterVisitor with fn visit_Expr_Let(
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
  IterVisitorBase::visit_Expr_Let(
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
impl IterVisitor with fn visit_Expr_Sequence(
  env,
  exprs~,
  last_expr~,
  semi_locs~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Sequence(
    env,
    exprs~,
    last_expr~,
    semi_locs~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Expr_Tuple(
  env,
  exprs~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Tuple(
    env,
    exprs~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Expr_Record(
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
  IterVisitorBase::visit_Expr_Record(
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
impl IterVisitor with fn visit_Expr_RecordUpdate(
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
  IterVisitorBase::visit_Expr_RecordUpdate(
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
impl IterVisitor with fn visit_Expr_Field(env, record~, accessor~, loc~) {
  IterVisitorBase::visit_Expr_Field(env, record~, accessor~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Method(
  env,
  type_name~,
  method_name~,
  coloncolon_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Method(
    env,
    type_name~,
    method_name~,
    coloncolon_loc~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Expr_DotApply(
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
  IterVisitorBase::visit_Expr_DotApply(
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
impl IterVisitor with fn visit_Expr_As(env, expr~, trait_~, as_loc~, loc~) {
  IterVisitorBase::visit_Expr_As(env, expr~, trait_~, as_loc~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Mutate(
  env,
  record~,
  accessor~,
  field~,
  augmented_by~,
  equal_loc~,
  augmented_assign_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Mutate(
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
impl IterVisitor with fn visit_Expr_Match(
  env,
  expr~,
  cases~,
  match_loc~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Match(
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
impl IterVisitor with fn visit_Expr_LetMut(
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
  IterVisitorBase::visit_Expr_LetMut(
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
impl IterVisitor with fn visit_Expr_Pipe(env, lhs~, rhs~, pipe_loc~, loc~) {
  IterVisitorBase::visit_Expr_Pipe(env, lhs~, rhs~, pipe_loc~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Assign(
  env,
  var_~,
  expr~,
  augmented_by~,
  equal_loc~,
  augmented_assign_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Assign(
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
impl IterVisitor with fn visit_Expr_Hole(env, loc~, kind~) {
  IterVisitorBase::visit_Expr_Hole(env, loc~, kind~)
}

///|
impl IterVisitor with fn visit_Expr_Return(
  env,
  return_value~,
  return_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Return(env, return_value~, return_loc~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Raise(env, err_value~, raise_loc~, loc~) {
  IterVisitorBase::visit_Expr_Raise(env, err_value~, raise_loc~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Unit(
  env,
  loc~,
  faked~,
  lparen_loc~,
  rparen_loc~,
) {
  IterVisitorBase::visit_Expr_Unit(env, loc~, faked~, lparen_loc~, rparen_loc~)
}

///|
impl IterVisitor with fn visit_Expr_Break(
  env,
  arg~,
  label~,
  break_loc~,
  post_label_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Break(
    env,
    arg~,
    label~,
    break_loc~,
    post_label_loc~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Expr_Continue(
  env,
  args~,
  label~,
  continue_loc~,
  post_label_loc~,
  comma_locs~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Continue(
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
impl IterVisitor with fn visit_Expr_For(
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
  IterVisitorBase::visit_Expr_For(
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
impl IterVisitor with fn visit_Expr_ForEach(
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
  IterVisitorBase::visit_Expr_ForEach(
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
impl IterVisitor with fn visit_Expr_Try(
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
  IterVisitorBase::visit_Expr_Try(
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
impl IterVisitor with fn visit_Expr_TryOperator(
  env,
  body~,
  kind~,
  try_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_TryOperator(env, body~, kind~, try_loc~, loc~)
}

///|
impl IterVisitor with fn visit_Expr_Map(
  env,
  elems~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Map(
    env,
    elems~,
    lbrace_loc~,
    rbrace_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Expr_Group(
  env,
  expr~,
  group~,
  left_delim_loc~,
  right_delim_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_Group(
    env,
    expr~,
    group~,
    left_delim_loc~,
    right_delim_loc~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Expr_StaticAssert(env, asserts~, body~) {
  IterVisitorBase::visit_Expr_StaticAssert(env, asserts~, body~)
}

///|
impl IterVisitor with fn visit_Visibility(env, visibility) {
  IterVisitorBase::visit_Visibility(env, visibility)
}

///|
impl IterVisitor with fn visit_Visibility_Pub(env, attr~, loc~) {
  IterVisitorBase::visit_Visibility_Pub(env, attr~, loc~)
}

///|
impl IterVisitor with fn visit_Visibility_Priv(env, loc~) {
  IterVisitorBase::visit_Visibility_Priv(env, loc~)
}

///|
impl IterVisitor with fn visit_Constant(env, constant) {
  IterVisitorBase::visit_Constant(env, constant)
}

///|
impl IterVisitor with fn visit_Label(env, label) {
  IterVisitorBase::visit_Label(env, label)
}

///|
impl IterVisitor with fn visit_ConstrName(env, constr_name) {
  IterVisitorBase::visit_ConstrName(env, constr_name)
}

///|
impl IterVisitor with fn visit_LongIdent(env, long_ident) {
  IterVisitorBase::visit_LongIdent(env, long_ident)
}

///|
impl IterVisitor with fn visit_LongIdent_Ident(env, name~) {
  IterVisitorBase::visit_LongIdent_Ident(env, name~)
}

///|
impl IterVisitor with fn visit_LongIdent_Dot(
  env,
  pkg~,
  id~,
  pkg_loc~,
  dot_id_loc~,
) {
  IterVisitorBase::visit_LongIdent_Dot(env, pkg~, id~, pkg_loc~, dot_id_loc~)
}

///|
impl IterVisitor with fn visit_TypeName(env, type_name) {
  IterVisitorBase::visit_TypeName(env, type_name)
}

///|
impl IterVisitor with fn visit_ConstrId(env, constr_id) {
  IterVisitorBase::visit_ConstrId(env, constr_id)
}

///|
impl IterVisitor with fn visit_Type(env, type_) {
  IterVisitorBase::visit_Type(env, type_)
}

///|
impl IterVisitor with fn visit_Type_Any(env, loc~) {
  IterVisitorBase::visit_Type_Any(env, loc~)
}

///|
impl IterVisitor with fn visit_Type_Arrow(
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
  IterVisitorBase::visit_Type_Arrow(
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
impl IterVisitor with fn visit_Type_Tuple(
  env,
  tys~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  IterVisitorBase::visit_Type_Tuple(
    env,
    tys~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Type_Name(
  env,
  constr_id~,
  tys~,
  type_args_lbracket_loc~,
  type_args_rbracket_loc~,
  type_arg_comma_locs~,
  loc~,
) {
  IterVisitorBase::visit_Type_Name(
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
impl IterVisitor with fn visit_Type_Option(env, ty~, loc~, question_loc~) {
  IterVisitorBase::visit_Type_Option(env, ty~, loc~, question_loc~)
}

///|
impl IterVisitor with fn visit_Type_Object(env, constr_id~, amp_loc~, loc~) {
  IterVisitorBase::visit_Type_Object(env, constr_id~, amp_loc~, loc~)
}

///|
impl IterVisitor with fn visit_ErrorType(env, error_type) {
  IterVisitorBase::visit_ErrorType(env, error_type)
}

///|
impl IterVisitor with fn visit_ErrorType_ErrorType(env, ty~, raise_loc~, loc~) {
  IterVisitorBase::visit_ErrorType_ErrorType(env, ty~, raise_loc~, loc~)
}

///|
impl IterVisitor with fn visit_ErrorType_DefaultErrorType(env, loc~) {
  IterVisitorBase::visit_ErrorType_DefaultErrorType(env, loc~)
}

///|
impl IterVisitor with fn visit_ErrorType_Noraise(env, loc~) {
  IterVisitorBase::visit_ErrorType_Noraise(env, loc~)
}

///|
impl IterVisitor with fn visit_ErrorType_MaybeError(
  env,
  ty~,
  raise_loc~,
  question_loc~,
  loc~,
) {
  IterVisitorBase::visit_ErrorType_MaybeError(
    env,
    ty~,
    raise_loc~,
    question_loc~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_ConstrParam(env, constr_param) {
  IterVisitorBase::visit_ConstrParam(env, constr_param)
}

///|
impl IterVisitor with fn visit_ConstrDecl(env, constr_decl) {
  IterVisitorBase::visit_ConstrDecl(env, constr_decl)
}

///|
impl IterVisitor with fn visit_ExceptionDecl(env, exception_decl) {
  IterVisitorBase::visit_ExceptionDecl(env, exception_decl)
}

///|
impl IterVisitor with fn visit_ExceptionDecl_EnumPayload(
  env,
  constrs~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  IterVisitorBase::visit_ExceptionDecl_EnumPayload(
    env,
    constrs~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
  )
}

///|
impl IterVisitor with fn visit_FieldName(env, field_name) {
  IterVisitorBase::visit_FieldName(env, field_name)
}

///|
impl IterVisitor with fn visit_FieldDecl(env, field_decl) {
  IterVisitorBase::visit_FieldDecl(env, field_decl)
}

///|
impl IterVisitor with fn visit_TypeDesc(env, type_desc) {
  IterVisitorBase::visit_TypeDesc(env, type_desc)
}

///|
impl IterVisitor with fn visit_TypeDesc_Error(env, exception_decl) {
  IterVisitorBase::visit_TypeDesc_Error(env, exception_decl)
}

///|
impl IterVisitor with fn visit_TypeDesc_Variant(
  env,
  constrs~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  IterVisitorBase::visit_TypeDesc_Variant(
    env,
    constrs~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
  )
}

///|
impl IterVisitor with fn visit_TypeDesc_Record(
  env,
  fields~,
  constr_decl~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  IterVisitorBase::visit_TypeDesc_Record(
    env,
    fields~,
    constr_decl~,
    lbrace_loc~,
    rbrace_loc~,
    semi_locs~,
  )
}

///|
impl IterVisitor with fn visit_TypeDesc_TupleStruct(
  env,
  types~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
) {
  IterVisitorBase::visit_TypeDesc_TupleStruct(
    env,
    types~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
  )
}

///|
impl IterVisitor with fn visit_TypeDesc_Alias(env, type_~, equal_loc~) {
  IterVisitorBase::visit_TypeDesc_Alias(env, type_~, equal_loc~)
}

///|
impl IterVisitor with fn visit_Hole(env, hole) {
  IterVisitorBase::visit_Hole(env, hole)
}

///|
impl IterVisitor with fn visit_ArgumentKind(env, argument_kind) {
  IterVisitorBase::visit_ArgumentKind(env, argument_kind)
}

///|
impl IterVisitor with fn visit_ArgumentKind_Labelled(env, label~, equal_loc~) {
  IterVisitorBase::visit_ArgumentKind_Labelled(env, label~, equal_loc~)
}

///|
impl IterVisitor with fn visit_ArgumentKind_LabelledPun(env, label) {
  IterVisitorBase::visit_ArgumentKind_LabelledPun(env, label)
}

///|
impl IterVisitor with fn visit_ArgumentKind_LabelledOption(
  env,
  label~,
  question_loc~,
  equal_loc~,
) {
  IterVisitorBase::visit_ArgumentKind_LabelledOption(
    env,
    label~,
    question_loc~,
    equal_loc~,
  )
}

///|
impl IterVisitor with fn visit_ArgumentKind_LabelledOptionPun(
  env,
  label~,
  question_loc~,
) {
  IterVisitorBase::visit_ArgumentKind_LabelledOptionPun(
    env,
    label~,
    question_loc~,
  )
}

///|
impl IterVisitor with fn visit_FnKind(env, fn_kind) {
  IterVisitorBase::visit_FnKind(env, fn_kind)
}

///|
impl IterVisitor with fn visit_Group(env, group) {
  IterVisitorBase::visit_Group(env, group)
}

///|
impl IterVisitor with fn visit_TrailingMark(env, trailing_mark) {
  IterVisitorBase::visit_TrailingMark(env, trailing_mark)
}

///|
impl IterVisitor with fn visit_TypeVarConstraint(env, type_var_constraint) {
  IterVisitorBase::visit_TypeVarConstraint(env, type_var_constraint)
}

///|
impl IterVisitor with fn visit_TypeVarBinder(env, type_var_binder) {
  IterVisitorBase::visit_TypeVarBinder(env, type_var_binder)
}

///|
impl IterVisitor with fn visit_TypeDeclBinder(env, type_decl_binder) {
  IterVisitorBase::visit_TypeDeclBinder(env, type_decl_binder)
}

///|
impl IterVisitor with fn visit_Binder(env, binder) {
  IterVisitorBase::visit_Binder(env, binder)
}

///|
impl IterVisitor with fn visit_Var(env, var_) {
  IterVisitorBase::visit_Var(env, var_)
}

///|
impl IterVisitor with fn visit_ConstructorExtraInfo(env, constructor_extra_info) {
  IterVisitorBase::visit_ConstructorExtraInfo(env, constructor_extra_info)
}

///|
impl IterVisitor with fn visit_ConstructorExtraInfo_TypeName(
  env,
  type_name~,
  coloncolon_loc~,
) {
  IterVisitorBase::visit_ConstructorExtraInfo_TypeName(
    env,
    type_name~,
    coloncolon_loc~,
  )
}

///|
impl IterVisitor with fn visit_ConstructorExtraInfo_Package(
  env,
  pkg~,
  pkg_loc~,
  dot_constr_loc~,
) {
  IterVisitorBase::visit_ConstructorExtraInfo_Package(
    env,
    pkg~,
    pkg_loc~,
    dot_constr_loc~,
  )
}

///|
impl IterVisitor with fn visit_Constructor(env, constructor_) {
  IterVisitorBase::visit_Constructor(env, constructor_)
}

///|
impl IterVisitor with fn visit_Accessor(env, accessor) {
  IterVisitorBase::visit_Accessor(env, accessor)
}

///|
impl IterVisitor with fn visit_Accessor_Label(env, label) {
  IterVisitorBase::visit_Accessor_Label(env, label)
}

///|
impl IterVisitor with fn visit_Accessor_Index(env, tuple_index~, loc~) {
  IterVisitorBase::visit_Accessor_Index(env, tuple_index~, loc~)
}

///|
impl IterVisitor with fn visit_Accessor_Newtype(env, loc~) {
  IterVisitorBase::visit_Accessor_Newtype(env, loc~)
}

///|
impl IterVisitor with fn visit_AliasTarget(env, alias_target) {
  IterVisitorBase::visit_AliasTarget(env, alias_target)
}

///|
impl IterVisitor with fn visit_Argument(env, argument) {
  IterVisitorBase::visit_Argument(env, argument)
}

///|
impl IterVisitor with fn visit_Parameter(env, parameter) {
  IterVisitorBase::visit_Parameter(env, parameter)
}

///|
impl IterVisitor with fn visit_Parameter_DiscardPositional(
  env,
  ty~,
  colon_loc~,
  loc~,
) {
  IterVisitorBase::visit_Parameter_DiscardPositional(env, ty~, colon_loc~, loc~)
}

///|
impl IterVisitor with fn visit_Parameter_Positional(
  env,
  binder~,
  ty~,
  colon_loc~,
) {
  IterVisitorBase::visit_Parameter_Positional(env, binder~, ty~, colon_loc~)
}

///|
impl IterVisitor with fn visit_Parameter_Labelled(
  env,
  binder~,
  ty~,
  post_label_loc~,
  colon_loc~,
) {
  IterVisitorBase::visit_Parameter_Labelled(
    env,
    binder~,
    ty~,
    post_label_loc~,
    colon_loc~,
  )
}

///|
impl IterVisitor with fn visit_Parameter_Optional(
  env,
  binder~,
  default~,
  ty~,
  post_label_loc~,
  question_loc~,
  colon_loc~,
  equal_loc~,
) {
  IterVisitorBase::visit_Parameter_Optional(
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
impl IterVisitor with fn visit_Parameter_QuestionOptional(
  env,
  binder~,
  ty~,
  question_loc~,
  colon_loc~,
) {
  IterVisitorBase::visit_Parameter_QuestionOptional(
    env,
    binder~,
    ty~,
    question_loc~,
    colon_loc~,
  )
}

///|
impl IterVisitor with fn visit_Case(env, case) {
  IterVisitorBase::visit_Case(env, case)
}

///|
impl IterVisitor with fn visit_SpreadableElem(env, spreadable_elem) {
  IterVisitorBase::visit_SpreadableElem(env, spreadable_elem)
}

///|
impl IterVisitor with fn visit_SpreadableElem_Regular(env, expr) {
  IterVisitorBase::visit_SpreadableElem_Regular(env, expr)
}

///|
impl IterVisitor with fn visit_SpreadableElem_Spread(
  env,
  expr~,
  dotdot_loc~,
  loc~,
) {
  IterVisitorBase::visit_SpreadableElem_Spread(env, expr~, dotdot_loc~, loc~)
}

///|
impl IterVisitor with fn visit_MapExprElem(env, map_pat_elem) {
  IterVisitorBase::visit_MapExprElem(env, map_pat_elem)
}

///|
impl IterVisitor with fn visit_StaticAssertion(env, static_assertion) {
  IterVisitorBase::visit_StaticAssertion(env, static_assertion)
}

///|
impl IterVisitor with fn visit_Func(env, func) {
  IterVisitorBase::visit_Func(env, func)
}

///|
impl IterVisitor with fn visit_FieldDef(env, field_def) {
  IterVisitorBase::visit_FieldDef(env, field_def)
}

///|
impl IterVisitor with fn visit_InterpElem(env, interp_elem) {
  IterVisitorBase::visit_InterpElem(env, interp_elem)
}

///|
impl IterVisitor with fn visit_InterpElem_Literal(env, repr~, loc~) {
  IterVisitorBase::visit_InterpElem_Literal(env, repr~, loc~)
}

///|
impl IterVisitor with fn visit_InterpElem_Expr(env, expr~, loc~) {
  IterVisitorBase::visit_InterpElem_Expr(env, expr~, loc~)
}

///|
impl IterVisitor with fn visit_InterpElem_Source(env, interp_source) {
  IterVisitorBase::visit_InterpElem_Source(env, interp_source)
}

///|
impl IterVisitor with fn visit_MultilineStringElem(env, multiline_string_elem) {
  IterVisitorBase::visit_MultilineStringElem(env, multiline_string_elem)
}

///|
impl IterVisitor with fn visit_MultilineStringElem_String(env, string_val) {
  IterVisitorBase::visit_MultilineStringElem_String(env, string_val)
}

///|
impl IterVisitor with fn visit_MultilineStringElem_Interp(env, interp_elems) {
  IterVisitorBase::visit_MultilineStringElem_Interp(env, interp_elems)
}

///|
impl IterVisitor with fn visit_TryOperatorKind(env, try_operator_kind) {
  IterVisitorBase::visit_TryOperatorKind(env, try_operator_kind)
}

///|
impl IterVisitor with fn visit_DotDotBinder(env, dot_dot_binder) {
  IterVisitorBase::visit_DotDotBinder(env, dot_dot_binder)
}

///|
impl IterVisitor with fn visit_DotDotBinder_Underscore(
  env,
  dotdot_loc~,
  underscore_loc~,
) {
  IterVisitorBase::visit_DotDotBinder_Underscore(
    env,
    dotdot_loc~,
    underscore_loc~,
  )
}

///|
impl IterVisitor with fn visit_DotDotBinder_NoBinder(env, dotdot_loc~) {
  IterVisitorBase::visit_DotDotBinder_NoBinder(env, dotdot_loc~)
}

///|
impl IterVisitor with fn visit_DotDotBinder_BinderAs(
  env,
  binder~,
  dotdot_loc~,
  as_loc~,
) {
  IterVisitorBase::visit_DotDotBinder_BinderAs(
    env,
    binder~,
    dotdot_loc~,
    as_loc~,
  )
}

///|
impl IterVisitor with fn visit_DotDotBinder_Binder(env, binder~, dotdot_loc~) {
  IterVisitorBase::visit_DotDotBinder_Binder(env, binder~, dotdot_loc~)
}

///|
impl IterVisitor with fn visit_ArrayPattern(env, array_pattern) {
  IterVisitorBase::visit_ArrayPattern(env, array_pattern)
}

///|
impl IterVisitor with fn visit_ArrayPattern_Pattern(env, pattern) {
  IterVisitorBase::visit_ArrayPattern_Pattern(env, pattern)
}

///|
impl IterVisitor with fn visit_ArrayPattern_StringSpread(
  env,
  str~,
  dotdot_loc~,
  loc~,
) {
  IterVisitorBase::visit_ArrayPattern_StringSpread(env, str~, dotdot_loc~, loc~)
}

///|
impl IterVisitor with fn visit_ArrayPattern_BytesSpread(
  env,
  bytes~,
  dotdot_loc~,
  loc~,
) {
  IterVisitorBase::visit_ArrayPattern_BytesSpread(
    env,
    bytes~,
    dotdot_loc~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_ArrayPattern_ConstSpread(
  env,
  binder~,
  pkg~,
  dotdot_loc~,
  pkg_loc~,
  dot_uident_loc~,
  loc~,
) {
  IterVisitorBase::visit_ArrayPattern_ConstSpread(
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
impl IterVisitor with fn visit_ArrayPatterns(env, array_patterns) {
  IterVisitorBase::visit_ArrayPatterns(env, array_patterns)
}

///|
impl IterVisitor with fn visit_ArrayPatterns_Closed(
  env,
  array_patterns,
  comma_locs~,
) {
  IterVisitorBase::visit_ArrayPatterns_Closed(env, array_patterns, comma_locs~)
}

///|
impl IterVisitor with fn visit_ArrayPatterns_Open(
  env,
  prefix_patterns,
  suffix_patterns,
  dot_dot_binder,
  left_comma_locs~,
  dotdot_comma_loc~,
  right_comma_locs~,
) {
  IterVisitorBase::visit_ArrayPatterns_Open(
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
impl IterVisitor with fn visit_WhereClause(env, where_clause) {
  IterVisitorBase::visit_WhereClause(env, where_clause)
}

///|
impl IterVisitor with fn visit_FieldPat(env, field_pat) {
  IterVisitorBase::visit_FieldPat(env, field_pat)
}

///|
impl IterVisitor with fn visit_ConstrPatArg(env, constr_pat_arg) {
  IterVisitorBase::visit_ConstrPatArg(env, constr_pat_arg)
}

///|
impl IterVisitor with fn visit_MapPatElem(env, map_pat_elem) {
  IterVisitorBase::visit_MapPatElem(env, map_pat_elem)
}

///|
impl IterVisitor with fn visit_RangeKind(env, kind) {
  IterVisitorBase::visit_RangeKind(env, kind)
}

///|
impl IterVisitor with fn visit_Pattern(env, pattern) {
  IterVisitorBase::visit_Pattern(env, pattern)
}

///|
impl IterVisitor with fn visit_Pattern_Alias(env, pat~, alias_~, as_loc~, loc~) {
  IterVisitorBase::visit_Pattern_Alias(env, pat~, alias_~, as_loc~, loc~)
}

///|
impl IterVisitor with fn visit_Pattern_Any(env, loc~) {
  IterVisitorBase::visit_Pattern_Any(env, loc~)
}

///|
impl IterVisitor with fn visit_Pattern_Array(
  env,
  pats~,
  lbracket_loc~,
  rbracket_loc~,
  loc~,
) {
  IterVisitorBase::visit_Pattern_Array(
    env,
    pats~,
    lbracket_loc~,
    rbracket_loc~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Pattern_Constant(env, c~, loc~) {
  IterVisitorBase::visit_Pattern_Constant(env, c~, loc~)
}

///|
impl IterVisitor with fn visit_Pattern_Constraint(
  env,
  pat~,
  ty~,
  lparen_loc~,
  colon_loc~,
  rparen_loc~,
  loc~,
) {
  IterVisitorBase::visit_Pattern_Constraint(
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
impl IterVisitor with fn visit_Pattern_Constr(
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
  IterVisitorBase::visit_Pattern_Constr(
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
impl IterVisitor with fn visit_Pattern_Or(env, pat1~, pat2~, bar_loc~, loc~) {
  IterVisitorBase::visit_Pattern_Or(env, pat1~, pat2~, bar_loc~, loc~)
}

///|
impl IterVisitor with fn visit_Pattern_Tuple(
  env,
  pats~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  IterVisitorBase::visit_Pattern_Tuple(
    env,
    pats~,
    lparen_loc~,
    rparen_loc~,
    comma_locs~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Pattern_Var(env, binder) {
  IterVisitorBase::visit_Pattern_Var(env, binder)
}

///|
impl IterVisitor with fn visit_Pattern_Record(
  env,
  fields~,
  is_closed~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  dotdot_loc~,
  loc~,
) {
  IterVisitorBase::visit_Pattern_Record(
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
impl IterVisitor with fn visit_Pattern_Map(
  env,
  elems~,
  is_closed~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  dotdot_loc~,
  loc~,
) {
  IterVisitorBase::visit_Pattern_Map(
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
impl IterVisitor with fn visit_Pattern_Range(
  env,
  lhs~,
  rhs~,
  kind~,
  range_op_loc~,
  loc~,
) {
  IterVisitorBase::visit_Pattern_Range(
    env,
    lhs~,
    rhs~,
    kind~,
    range_op_loc~,
    loc~,
  )
}

///|
impl IterVisitor with fn visit_Pattern_SpecialConstr(
  env,
  binder~,
  args~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  IterVisitorBase::visit_Pattern_SpecialConstr(
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
impl IterVisitor with fn visit_DerivingDirective(env, deriving_directive) {
  IterVisitorBase::visit_DerivingDirective(env, deriving_directive)
}

///|
impl IterVisitor with fn visit_TypeDecl(env, type_decl) {
  IterVisitorBase::visit_TypeDecl(env, type_decl)
}

///|
impl IterVisitor with fn visit_FuncStubs(env, func_stubs) {
  IterVisitorBase::visit_FuncStubs(env, func_stubs)
}

///|
impl IterVisitor with fn visit_FuncStubs_Import(
  env,
  module_name~,
  func_name~,
  language~,
  module_name_loc~,
  func_name_loc~,
  language_loc~,
) {
  IterVisitorBase::visit_FuncStubs_Import(
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
impl IterVisitor with fn visit_FuncStubs_Embedded(
  env,
  language~,
  code~,
  language_loc~,
  code_loc~,
) {
  IterVisitorBase::visit_FuncStubs_Embedded(
    env,
    language~,
    code~,
    language_loc~,
    code_loc~,
  )
}

///|
impl IterVisitor with fn visit_EmbeddedCode(env, embedded_code) {
  IterVisitorBase::visit_EmbeddedCode(env, embedded_code)
}

///|
impl IterVisitor with fn visit_EmbeddedCode_CodeString(
  env,
  string_literal,
  loc~,
) {
  IterVisitorBase::visit_EmbeddedCode_CodeString(env, string_literal, loc~)
}

///|
impl IterVisitor with fn visit_EmbeddedCode_CodeMultilineString(
  env,
  strings,
  string_locs~,
) {
  IterVisitorBase::visit_EmbeddedCode_CodeMultilineString(
    env,
    strings,
    string_locs~,
  )
}

///|
impl IterVisitor with fn visit_DeclBody(env, decl_body) {
  IterVisitorBase::visit_DeclBody(env, decl_body)
}

///|
impl IterVisitor with fn visit_DeclBody_DeclBody(
  env,
  expr~,
  lbrace_loc~,
  rbrace_loc~,
) {
  IterVisitorBase::visit_DeclBody_DeclBody(env, expr~, lbrace_loc~, rbrace_loc~)
}

///|
impl IterVisitor with fn visit_DeclBody_DeclStubs(env, func_stubs, equal_loc~) {
  IterVisitorBase::visit_DeclBody_DeclStubs(env, func_stubs, equal_loc~)
}

///|
impl IterVisitor with fn visit_FunDecl(env, fun_decl) {
  IterVisitorBase::visit_FunDecl(env, fun_decl)
}

///|
impl IterVisitor with fn visit_TraitMethodDecl(env, trait_method_decl) {
  IterVisitorBase::visit_TraitMethodDecl(env, trait_method_decl)
}

///|
impl IterVisitor with fn visit_TraitDecl(env, trait_decl) {
  IterVisitorBase::visit_TraitDecl(env, trait_decl)
}

///|
impl IterVisitor with fn visit_Expr_IsLexMatch(
  env,
  expr~ : Expr,
  strategy~,
  pat~,
  pat_loc~,
  lexmatch_question_loc~,
  with_loc~,
  loc~,
) {
  IterVisitorBase::visit_Expr_IsLexMatch(
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
impl IterVisitor with fn visit_Expr_LexMatch(
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
  IterVisitorBase::visit_Expr_LexMatch(
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
impl IterVisitor with fn visit_LexCase(env, lex_case) {
  IterVisitorBase::visit_LexCase(env, lex_case)
}

///|
impl IterVisitor with fn visit_LexTopPattern(env, lex_top_pattern) {
  IterVisitorBase::visit_LexTopPattern(env, lex_top_pattern)
}

///|
impl IterVisitor with fn visit_LexTopPattern_Pattern(env, lex_pattern) {
  IterVisitorBase::visit_LexTopPattern_Pattern(env, lex_pattern)
}

///|
impl IterVisitor with fn visit_LexTopPattern_Binder(env, binder) {
  IterVisitorBase::visit_LexTopPattern_Binder(env, binder)
}

///|
impl IterVisitor with fn visit_LexTopPattern_Wildcard(env, loc~) {
  IterVisitorBase::visit_LexTopPattern_Wildcard(env, loc~)
}

///|
impl IterVisitor with fn visit_LexPattern(env, lex_pattern) {
  IterVisitorBase::visit_LexPattern(env, lex_pattern)
}

///|
impl IterVisitor with fn visit_LexPattern_Regex(env, lit~, offset~, loc~) {
  IterVisitorBase::visit_LexPattern_Regex(env, lit~, offset~, loc~)
}

///|
impl IterVisitor with fn visit_LexPattern_RegexInterp(env, elems~, loc~) {
  IterVisitorBase::visit_LexPattern_RegexInterp(env, elems~, loc~)
}

///|
impl IterVisitor with fn visit_LexPattern_ConstantRef(env, lid~, loc~) {
  IterVisitorBase::visit_LexPattern_ConstantRef(env, lid~, loc~)
}

///|
impl IterVisitor with fn visit_LexPattern_Alias(
  env,
  pat~,
  binder~,
  as_loc~,
  loc~,
) {
  IterVisitorBase::visit_LexPattern_Alias(env, pat~, binder~, as_loc~, loc~)
}

///|
impl IterVisitor with fn visit_LexPattern_Sequence(env, pats~, semi_locs~, loc~) {
  IterVisitorBase::visit_LexPattern_Sequence(env, pats~, semi_locs~, loc~)
}

///|
pub struct IterVisitorBase[T](T)

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Visibility(
  env,
  visibility,
) {
  match visibility {
    Default => ()
    Pub(attr~, loc~) => env.0.visit_Visibility_Pub(attr~, loc~)
    Priv(loc~) => env.0.visit_Visibility_Priv(loc~)
  }
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Visibility_Pub(
  _,
  attr~,
  loc~,
) {
  ignore(attr)
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Visibility_Priv(
  _,
  loc~,
) {
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Constant(
  _,
  _,
) {
  ()
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Label(
  _,
  _,
) {
  ()
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ConstrName(
  _,
  _,
) {
  ()
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LongIdent(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LongIdent_Ident(
  _,
  name~,
) {
  ignore(name)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LongIdent_Dot(
  _,
  pkg~,
  id~,
  pkg_loc~,
  dot_id_loc~,
) {
  ignore(pkg)
  ignore(id)
  ignore(pkg_loc)
  ignore(dot_id_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeName(
  env,
  type_name,
) {
  ignore(type_name.object_amp_loc)
  env.0.visit_LongIdent(type_name.name)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ConstrId(
  env,
  constr_id,
) {
  env.0.visit_LongIdent(constr_id.id)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Type(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Type_Any(
  _,
  loc~,
) {
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Type_Arrow(
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
  ignore(is_async)
  ignore(lparen_loc)
  ignore(rparen_loc)
  ignore(arrow_loc)
  ignore(comma_locs)
  ignore(loc)
  args.each(x => env.0.visit_Type(x))
  env.0.visit_Type(res)
  env.0.visit_ErrorType(err)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Type_Tuple(
  env,
  tys~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  ignore(lparen_loc)
  ignore(rparen_loc)
  ignore(comma_locs)
  ignore(loc)
  tys.each(x => env.0.visit_Type(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Type_Name(
  env,
  constr_id~,
  tys~,
  type_args_lbracket_loc~,
  type_args_rbracket_loc~,
  type_arg_comma_locs~,
  loc~,
) {
  ignore(type_args_lbracket_loc)
  ignore(type_args_rbracket_loc)
  ignore(type_arg_comma_locs)
  ignore(loc)
  env.0.visit_ConstrId(constr_id)
  tys.each(x => env.0.visit_Type(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Type_Option(
  env,
  ty~,
  loc~,
  question_loc~,
) {
  ignore(loc)
  ignore(question_loc)
  env.0.visit_Type(ty)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Type_Object(
  env,
  constr_id~,
  amp_loc~,
  loc~,
) {
  ignore(amp_loc)
  ignore(loc)
  env.0.visit_ConstrId(constr_id)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ErrorType(
  env,
  error_type,
) {
  match error_type {
    ErrorType(ty~, raise_loc~, loc~) =>
      env.0.visit_ErrorType_ErrorType(ty~, raise_loc~, loc~)
    DefaultErrorType(loc~) => env.0.visit_ErrorType_DefaultErrorType(loc~)
    NoErrorType => ()
    Noraise(loc~) => env.0.visit_ErrorType_Noraise(loc~)
    MaybeError(ty~, raise_loc~, question_loc~, loc~) =>
      env.0.visit_ErrorType_MaybeError(ty~, raise_loc~, question_loc~, loc~)
  }
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ErrorType_ErrorType(
  env,
  ty~,
  raise_loc~,
  loc~,
) {
  ignore(raise_loc)
  ignore(loc)
  env.0.visit_Type(ty)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ErrorType_DefaultErrorType(
  _,
  loc~,
) {
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ErrorType_Noraise(
  _,
  loc~,
) {
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ErrorType_MaybeError(
  env,
  ty~,
  raise_loc~,
  question_loc~,
  loc~,
) {
  ignore(raise_loc)
  ignore(question_loc)
  ignore(loc)
  env.0.visit_Type(ty)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ConstrParam(
  env,
  constr_param,
) {
  env.0.visit_Type(constr_param.ty)
  constr_param.label.each(x => env.0.visit_Label(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ConstrDecl(
  env,
  constr_decl,
) {
  env.0.visit_ConstrName(constr_decl.name)
  constr_decl.args.each(args => args.each(x => env.0.visit_ConstrParam(x)))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ExceptionDecl(
  env,
  exception_decl,
) {
  match exception_decl {
    NoPayload => ()
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ExceptionDecl_EnumPayload(
  env,
  constrs~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  ignore(semi_locs)
  constrs.each(x => env.0.visit_ConstrDecl(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_FieldName(
  _,
  _,
) {
  ()
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_FieldDecl(
  env,
  field_decl,
) {
  env.0.visit_FieldName(field_decl.name)
  env.0.visit_Type(field_decl.ty)
  env.0.visit_Visibility(field_decl.vis)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeDesc(
  env,
  type_desc,
) {
  match type_desc {
    Abstract => ()
    ExtensibleEnum(constrs~, ..) => constrs.each(x => env.0.visit_ConstrDecl(x))
    Error(exception_decl) => env.0.visit_TypeDesc_Error(exception_decl)
    Variant(constrs~, lbrace_loc~, rbrace_loc~, semi_locs~) =>
      env.0.visit_TypeDesc_Variant(
        constrs~,
        lbrace_loc~,
        rbrace_loc~,
        semi_locs~,
      )
    ExtendEnum(target~, constructors~, ..) => {
      env.0.visit_TypeName(target)
      constructors.each(x => env.0.visit_ConstrDecl(x))
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeDesc_Error(
  env,
  exception_decl,
) {
  env.0.visit_ExceptionDecl(exception_decl)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeDesc_Variant(
  env,
  constrs~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  ignore(semi_locs)
  constrs.each(x => env.0.visit_ConstrDecl(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeDesc_Record(
  env,
  fields~,
  constr_decl~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
) {
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  ignore(semi_locs)
  fields.each(x => env.0.visit_FieldDecl(x))
  constr_decl.each(x => env.0.visit_FunDecl(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeDesc_TupleStruct(
  env,
  types~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
) {
  ignore(lparen_loc)
  ignore(rparen_loc)
  ignore(comma_locs)
  types.each(x => env.0.visit_Type(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeDesc_Alias(
  env,
  type_~,
  equal_loc~,
) {
  ignore(equal_loc)
  env.0.visit_Type(type_)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Hole(
  _,
  hole,
) {
  ignore(hole)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArgumentKind(
  env,
  argument_kind,
) {
  match argument_kind {
    Positional => ()
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArgumentKind_Labelled(
  _,
  label~,
  equal_loc~,
) {
  ignore(label)
  ignore(equal_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArgumentKind_LabelledPun(
  _,
  label,
) {
  ignore(label)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArgumentKind_LabelledOption(
  _,
  label~,
  question_loc~,
  equal_loc~,
) {
  ignore(label)
  ignore(question_loc)
  ignore(equal_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArgumentKind_LabelledOptionPun(
  _,
  label~,
  question_loc~,
) {
  ignore(label)
  ignore(question_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_FnKind(
  _,
  _,
) {
  ()
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Group(
  _,
  _,
) {
  ()
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TrailingMark(
  _,
  _,
) {
  ()
}

// Continue with remaining IterVisitorBase implementations that are missing

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeVarConstraint(
  env,
  type_var_constraint,
) {
  env.0.visit_LongIdent(type_var_constraint.trait_)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeVarBinder(
  env,
  type_var_binder,
) {
  type_var_binder.constraints.each(x => env.0.visit_TypeVarConstraint(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeDeclBinder(
  _,
  _,
) {
  ()
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Binder(
  _,
  _,
) {
  ()
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Var(
  env,
  var_,
) {
  env.0.visit_LongIdent(var_.name)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ConstructorExtraInfo(
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
      env.0.visit_TypeName(type_name)
      ignore(pkg)
      ignore(coloncolon_loc)
      ignore(pkg_loc)
      ignore(dot_constr_loc)
    }
    Package(pkg~, pkg_loc~, dot_constr_loc~) =>
      env.0.visit_ConstructorExtraInfo_Package(pkg~, pkg_loc~, dot_constr_loc~)
    NoExtraInfo => ()
  }
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ConstructorExtraInfo_TypeName(
  env,
  type_name~,
  coloncolon_loc~,
) {
  env.0.visit_TypeName(type_name)
  ignore(coloncolon_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ConstructorExtraInfo_Package(
  _,
  pkg~,
  pkg_loc~,
  dot_constr_loc~,
) {
  ignore(pkg)
  ignore(pkg_loc)
  ignore(dot_constr_loc)
  ()
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Constructor(
  env,
  constructor_,
) {
  env.0.visit_ConstrName(constructor_.name)
  env.0.visit_ConstructorExtraInfo(constructor_.extra_info)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Accessor(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Accessor_Label(
  env,
  label,
) {
  env.0.visit_Label(label)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Accessor_Index(
  _,
  tuple_index~,
  loc~,
) {
  ignore(tuple_index)
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Accessor_Newtype(
  _,
  loc~,
) {
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_AliasTarget(
  env,
  alias_target,
) {
  env.0.visit_Binder(alias_target.binder)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Argument(
  env,
  argument,
) {
  env.0.visit_Expr(argument.value)
  env.0.visit_ArgumentKind(argument.kind)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Parameter(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Parameter_DiscardPositional(
  env,
  ty~,
  colon_loc~,
  loc~,
) {
  ignore(colon_loc)
  ignore(loc)
  ty.each(x => env.0.visit_Type(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Parameter_Positional(
  env,
  binder~,
  ty~,
  colon_loc~,
) {
  ignore(colon_loc)
  env.0.visit_Binder(binder)
  ty.each(x => env.0.visit_Type(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Parameter_Labelled(
  env,
  binder~,
  ty~,
  post_label_loc~,
  colon_loc~,
) {
  ignore(post_label_loc)
  ignore(colon_loc)
  env.0.visit_Binder(binder)
  ty.each(x => env.0.visit_Type(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Parameter_Optional(
  env,
  binder~,
  default~,
  ty~,
  post_label_loc~,
  question_loc~,
  colon_loc~,
  equal_loc~,
) {
  ignore(post_label_loc)
  ignore(question_loc)
  ignore(colon_loc)
  ignore(equal_loc)
  env.0.visit_Binder(binder)
  env.0.visit_Expr(default)
  ty.each(x => env.0.visit_Type(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Parameter_QuestionOptional(
  env,
  binder~,
  ty~,
  question_loc~,
  colon_loc~,
) {
  ignore(question_loc)
  ignore(colon_loc)
  env.0.visit_Binder(binder)
  ty.each(x => env.0.visit_Type(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Case(
  env,
  case,
) {
  ignore(case.if_loc)
  ignore(case.fat_arrow_loc)
  ignore(case.ellipsis_loc)
  env.0.visit_Pattern(case.pattern)
  case.guard_.each(x => env.0.visit_Expr(x))
  env.0.visit_Expr(case.body)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_SpreadableElem(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_SpreadableElem_Regular(
  env,
  expr,
) {
  env.0.visit_Expr(expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_SpreadableElem_Spread(
  env,
  expr~,
  dotdot_loc~,
  loc~,
) {
  ignore(dotdot_loc)
  ignore(loc)
  env.0.visit_Expr(expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_MapExprElem(
  env,
  map_expr_elem,
) {
  ignore(map_expr_elem.colon_loc)
  ignore(map_expr_elem.key_loc)
  ignore(map_expr_elem.loc)
  env.0.visit_Constant(map_expr_elem.key)
  env.0.visit_Expr(map_expr_elem.expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_StaticAssertion(
  env,
  static_assertion,
) {
  env.0.visit_Type(static_assertion.ty)
  env.0.visit_LongIdent(static_assertion.trait_)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Func(
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
  ignore(is_async)
  ignore(params_loc)
  ignore(fn_loc)
  ignore(lparen_loc)
  ignore(rparen_loc)
  ignore(comma_locs)
  ignore(return_arrow_loc)
  ignore(fat_arrow_loc)
  ignore(body_lbrace_loc)
  ignore(body_rbrace_loc)
  ignore(loc)
  parameters.each(p => env.0.visit_Parameter(p))
  env.0.visit_Expr(body)
  return_type.each(t => env.0.visit_Type(t))
  env.0.visit_ErrorType(error_type)
  env.0.visit_FnKind(kind)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_FieldDef(
  env,
  field_def,
) {
  ignore(field_def.colon_loc)
  env.0.visit_Label(field_def.label)
  env.0.visit_Expr(field_def.expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_InterpElem(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_InterpElem_Literal(
  _,
  repr~,
  loc~,
) {
  ignore(repr)
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_InterpElem_Expr(
  env,
  expr~,
  loc~,
) {
  ignore(loc)
  env.0.visit_Expr(expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_InterpElem_Source(
  _,
  interp_source,
) {
  ignore(interp_source)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_MultilineStringElem(
  env,
  multiline_string_elem,
) {
  match multiline_string_elem {
    String(string_val) => env.0.visit_MultilineStringElem_String(string_val)
    Interp(interp_elems) => env.0.visit_MultilineStringElem_Interp(interp_elems)
  }
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_MultilineStringElem_String(
  _,
  string_val,
) {
  ignore(string_val)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_MultilineStringElem_Interp(
  _,
  interp_elems,
) {
  ignore(interp_elems)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TryOperatorKind(
  _,
  try_operator_kind,
) {
  ignore(try_operator_kind)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_DotDotBinder(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_DotDotBinder_Underscore(
  _,
  dotdot_loc~,
  underscore_loc~,
) {
  ignore(dotdot_loc)
  ignore(underscore_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_DotDotBinder_NoBinder(
  _,
  dotdot_loc~,
) {
  ignore(dotdot_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_DotDotBinder_BinderAs(
  env,
  binder~,
  dotdot_loc~,
  as_loc~,
) {
  ignore(dotdot_loc)
  ignore(as_loc)
  env.0.visit_Binder(binder)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_DotDotBinder_Binder(
  env,
  binder~,
  dotdot_loc~,
) {
  ignore(dotdot_loc)
  env.0.visit_Binder(binder)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArrayPattern(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArrayPattern_Pattern(
  env,
  pattern,
) {
  env.0.visit_Pattern(pattern)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArrayPattern_StringSpread(
  _,
  str~,
  dotdot_loc~,
  loc~,
) {
  ignore((str, dotdot_loc, loc))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArrayPattern_BytesSpread(
  _,
  bytes~,
  dotdot_loc~,
  loc~,
) {
  ignore((bytes, dotdot_loc, loc))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArrayPattern_ConstSpread(
  env,
  binder~,
  pkg~,
  dotdot_loc~,
  pkg_loc~,
  dot_uident_loc~,
  loc~,
) {
  ignore(pkg)
  ignore(dotdot_loc)
  ignore(pkg_loc)
  ignore(dot_uident_loc)
  ignore(loc)
  env.0.visit_Binder(binder)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArrayPatterns(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArrayPatterns_Closed(
  env,
  array_patterns,
  comma_locs~,
) {
  comma_locs.each(fn(loc) { ignore(loc) })
  array_patterns.each(x => env.0.visit_ArrayPattern(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ArrayPatterns_Open(
  env,
  prefix_patterns,
  suffix_patterns,
  dot_dot_binder,
  left_comma_locs~,
  dotdot_comma_loc~,
  right_comma_locs~,
) {
  left_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(dotdot_comma_loc)
  right_comma_locs.each(fn(loc) { ignore(loc) })
  prefix_patterns.each(x => env.0.visit_ArrayPattern(x))
  suffix_patterns.each(x => env.0.visit_ArrayPattern(x))
  env.0.visit_DotDotBinder(dot_dot_binder)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_WhereClause(
  env,
  where_clause,
) {
  where_clause.fields.each(x => env.0.visit_FieldDef(x))
  ignore(where_clause.where_loc)
  ignore(where_clause.lbrace_loc)
  ignore(where_clause.rbrace_loc)
  where_clause.comma_locs.each(fn(loc) { ignore(loc) })
  ignore(where_clause.loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_FieldPat(
  env,
  field_pat,
) {
  env.0.visit_Label(field_pat.label)
  env.0.visit_Pattern(field_pat.pattern)
  ignore(field_pat.colon_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_ConstrPatArg(
  env,
  constr_pat_arg,
) {
  env.0.visit_Pattern(constr_pat_arg.pat)
  env.0.visit_ArgumentKind(constr_pat_arg.kind)
  ignore(constr_pat_arg.equal_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_MapPatElem(
  env,
  map_pat_elem,
) {
  env.0.visit_Constant(map_pat_elem.key)
  env.0.visit_Pattern(map_pat_elem.pat)
  ignore(map_pat_elem.key_loc)
  ignore(map_pat_elem.question_loc)
  ignore(map_pat_elem.colon_loc)
  ignore(map_pat_elem.loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_RangeKind(
  env,
  kind,
) {
  ignore(env)
  ignore(kind)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Alias(
  env,
  pat~,
  alias_~,
  as_loc~,
  loc~,
) {
  ignore(as_loc)
  ignore(loc)
  env.0.visit_Pattern(pat)
  env.0.visit_Binder(alias_)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Any(
  _,
  loc~,
) {
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Array(
  env,
  pats~,
  lbracket_loc~,
  rbracket_loc~,
  loc~,
) {
  ignore(lbracket_loc)
  ignore(rbracket_loc)
  ignore(loc)
  env.0.visit_ArrayPatterns(pats)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Constant(
  env,
  c~,
  loc~,
) {
  ignore(loc)
  env.0.visit_Constant(c)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Constraint(
  env,
  pat~,
  ty~,
  lparen_loc~,
  colon_loc~,
  rparen_loc~,
  loc~,
) {
  ignore(lparen_loc)
  ignore(colon_loc)
  ignore(rparen_loc)
  ignore(loc)
  env.0.visit_Pattern(pat)
  env.0.visit_Type(ty)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Constr(
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
  ignore(is_open)
  ignore(args_lparen_loc)
  ignore(args_rparen_loc)
  arg_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(open_dotdot_loc)
  ignore(loc)
  env.0.visit_Constructor(constr)
  args.each(args => args.each(x => env.0.visit_ConstrPatArg(x)))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Or(
  env,
  pat1~,
  pat2~,
  bar_loc~,
  loc~,
) {
  ignore(bar_loc)
  ignore(loc)
  env.0.visit_Pattern(pat1)
  env.0.visit_Pattern(pat2)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Tuple(
  env,
  pats~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  ignore(lparen_loc)
  ignore(rparen_loc)
  comma_locs.each(fn(loc) { ignore(loc) })
  ignore(loc)
  pats.each(x => env.0.visit_Pattern(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Var(
  env,
  binder,
) {
  env.0.visit_Binder(binder)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Record(
  env,
  fields~,
  is_closed~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  dotdot_loc~,
  loc~,
) {
  ignore(is_closed)
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  comma_locs.each(fn(loc) { ignore(loc) })
  ignore(dotdot_loc)
  ignore(loc)
  fields.each(x => env.0.visit_FieldPat(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Map(
  env,
  elems~,
  is_closed~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  dotdot_loc~,
  loc~,
) {
  ignore(is_closed)
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  comma_locs.each(fn(loc) { ignore(loc) })
  ignore(dotdot_loc)
  ignore(loc)
  elems.each(x => env.0.visit_MapPatElem(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_Range(
  env,
  lhs~,
  rhs~,
  kind~,
  range_op_loc~,
  loc~,
) {
  ignore(range_op_loc)
  ignore(loc)
  env.0.visit_Pattern(lhs)
  env.0.visit_Pattern(rhs)
  env.0.visit_RangeKind(kind)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Pattern_SpecialConstr(
  env,
  binder~,
  args~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  ignore(lparen_loc)
  ignore(rparen_loc)
  comma_locs.each(fn(loc) { ignore(loc) })
  ignore(loc)
  env.0.visit_Binder(binder)
  args.each(x => env.0.visit_ConstrPatArg(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_DerivingDirective(
  env,
  deriving_directive,
) {
  env.0.visit_TypeName(deriving_directive.type_name)
  deriving_directive.args.each(x => env.0.visit_Argument(x))
  ignore(deriving_directive.args_lparen_loc)
  ignore(deriving_directive.args_rparen_loc)
  deriving_directive.arg_comma_locs.each(fn(loc) { ignore(loc) })
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TypeDecl(
  env,
  type_decl,
) {
  type_decl.params.each(x => env.0.visit_TypeDeclBinder(x))
  env.0.visit_TypeDesc(type_decl.components)
  type_decl.deriving.each(x => env.0.visit_DerivingDirective(x))
  env.0.visit_Visibility(type_decl.type_vis)
  ignore(type_decl.declare_loc)
  ignore(type_decl.keyword_loc)
  ignore(type_decl.params_lbracket_loc)
  ignore(type_decl.params_rbracket_loc)
  type_decl.param_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(type_decl.derive_loc)
  ignore(type_decl.derive_lparen_loc)
  ignore(type_decl.derive_rparen_loc)
  type_decl.derive_comma_locs.each(fn(loc) { ignore(loc) })
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_FuncStubs(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_FuncStubs_Import(
  _,
  module_name~,
  func_name~,
  language~,
  module_name_loc~,
  func_name_loc~,
  language_loc~,
) {
  ignore(module_name)
  ignore(func_name)
  ignore(language)
  ignore(module_name_loc)
  ignore(func_name_loc)
  ignore(language_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_FuncStubs_Embedded(
  env,
  language~,
  code~,
  language_loc~,
  code_loc~,
) {
  ignore(language)
  ignore(language_loc)
  ignore(code_loc)
  env.0.visit_EmbeddedCode(code)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_EmbeddedCode(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_EmbeddedCode_CodeString(
  _,
  _,
  loc~,
) {
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_EmbeddedCode_CodeMultilineString(
  _,
  _,
  string_locs~,
) {
  ignore(string_locs)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_DeclBody(
  env,
  decl_body,
) {
  match decl_body {
    DeclBody(expr~, lbrace_loc~, rbrace_loc~) =>
      env.0.visit_DeclBody_DeclBody(expr~, lbrace_loc~, rbrace_loc~)
    DeclStubs(func_stubs, equal_loc~) =>
      env.0.visit_DeclBody_DeclStubs(func_stubs, equal_loc~)
    DeclNone => ()
  }
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_DeclBody_DeclBody(
  env,
  expr~,
  lbrace_loc~,
  rbrace_loc~,
) {
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  env.0.visit_Expr(expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_DeclBody_DeclStubs(
  env,
  func_stubs,
  equal_loc~,
) {
  ignore(equal_loc)
  env.0.visit_FuncStubs(func_stubs)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_FunDecl(
  env,
  fun_decl,
) {
  fun_decl.type_name.each(x => env.0.visit_TypeName(x))
  env.0.visit_Binder(fun_decl.name)
  fun_decl.decl_params.each(decl_params => {
    decl_params.each(x => env.0.visit_Parameter(x))
  })
  fun_decl.quantifiers.each(x => env.0.visit_TypeVarBinder(x))
  fun_decl.return_type.each(x => env.0.visit_Type(x))
  env.0.visit_ErrorType(fun_decl.error_type)
  env.0.visit_Visibility(fun_decl.vis)
  ignore(fun_decl.declare_loc)
  ignore(fun_decl.extern_loc)
  ignore(fun_decl.fn_loc)
  ignore(fun_decl.coloncolon_loc)
  ignore(fun_decl.type_params_lbracket_loc)
  ignore(fun_decl.type_params_rbracket_loc)
  fun_decl.type_param_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(fun_decl.params_lparen_loc)
  ignore(fun_decl.params_rparen_loc)
  fun_decl.param_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(fun_decl.return_arrow_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TraitMethodDecl(
  env,
  trait_method_decl,
) {
  env.0.visit_Binder(trait_method_decl.name)
  trait_method_decl.quantifiers.each(x => env.0.visit_TypeVarBinder(x))
  trait_method_decl.params.each(param => env.0.visit_Parameter(param))
  trait_method_decl.return_type.each(x => env.0.visit_Type(x))
  env.0.visit_ErrorType(trait_method_decl.error_type)
  ignore(trait_method_decl.fn_loc)
  ignore(trait_method_decl.type_params_lbracket_loc)
  ignore(trait_method_decl.type_params_rbracket_loc)
  trait_method_decl.type_param_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(trait_method_decl.params_lparen_loc)
  ignore(trait_method_decl.params_rparen_loc)
  trait_method_decl.param_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(trait_method_decl.return_arrow_loc)
  ignore(trait_method_decl.default_equal_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_TraitDecl(
  env,
  trait_decl,
) {
  env.0.visit_Binder(trait_decl.name)
  trait_decl.supers.each(x => env.0.visit_TypeVarConstraint(x))
  trait_decl.methods.each(x => env.0.visit_TraitMethodDecl(x))
  env.0.visit_Visibility(trait_decl.vis)
  ignore(trait_decl.declare_loc)
  ignore(trait_decl.trait_loc)
  ignore(trait_decl.colon_loc)
  trait_decl.super_plus_locs.each(fn(loc) { ignore(loc) })
  ignore(trait_decl.lbrace_loc)
  ignore(trait_decl.rbrace_loc)
  trait_decl.semi_locs.each(fn(loc) { ignore(loc) })
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_UsingKind(
  env,
  using_kind,
) {
  ignore(env)
  ignore(using_kind)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl_TopExpr(
  env,
  expr~,
  is_main~,
  is_async~,
  loc~,
) {
  ignore(is_main)
  ignore(loc)
  ignore(is_async)
  env.0.visit_Expr(expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl_TopTest(
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
  ignore(name)
  ignore(loc)
  ignore(attrs)
  ignore(doc)
  ignore(is_async)
  ignore(test_loc)
  ignore(params_lparen_loc)
  ignore(params_rparen_loc)
  param_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(body_lbrace_loc)
  ignore(body_rbrace_loc)
  env.0.visit_Expr(expr)
  params.each(params => params.each(param => env.0.visit_Parameter(param)))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl_TopTypeDef(
  env,
  type_decl,
) {
  env.0.visit_TypeDecl(type_decl)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl_TopFuncDef(
  env,
  fun_decl~,
  decl_body~,
  where_clause~,
  equal_loc~,
  loc~,
) {
  ignore(equal_loc)
  ignore(loc)
  env.0.visit_FunDecl(fun_decl)
  env.0.visit_DeclBody(decl_body)
  where_clause.each(x => env.0.visit_WhereClause(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl_TopLetDef(
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
  ignore(is_constant)
  ignore(loc)
  ignore(attrs)
  ignore(doc)
  ignore(is_declare)
  ignore(declare_loc)
  ignore(let_or_const_loc)
  ignore(annot_colon_loc)
  ignore(equal_loc)
  env.0.visit_Binder(binder)
  ty.each(x => env.0.visit_Type(x))
  env.0.visit_Expr(expr)
  env.0.visit_Visibility(vis)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl_TopTrait(
  env,
  trait_decl,
) {
  env.0.visit_TraitDecl(trait_decl)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl_TopImpl(
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
  ignore(loc)
  ignore(attrs)
  ignore(doc)
  ignore(impl_loc)
  ignore(type_params_lbracket_loc)
  ignore(type_params_rbracket_loc)
  type_param_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(for_loc)
  ignore(with_loc)
  ignore(fn_loc)
  ignore(params_lparen_loc)
  ignore(params_rparen_loc)
  param_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(return_arrow_loc)
  self_ty.each(x => env.0.visit_Type(x))
  env.0.visit_TypeName(trait_)
  env.0.visit_Binder(method_name)
  quantifiers.each(x => env.0.visit_TypeVarBinder(x))
  params.each(param => env.0.visit_Parameter(param))
  ret_ty.each(x => env.0.visit_Type(x))
  env.0.visit_ErrorType(err_ty)
  env.0.visit_DeclBody(body)
  env.0.visit_Visibility(vis)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl_TopView(
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
  ignore(view_type_name)
  ignore(view_type_loc)
  ignore(params_loc)
  ignore(enumview_loc)
  ignore(type_params_lbracket_loc)
  ignore(type_params_rbracket_loc)
  ignore(type_param_comma_locs)
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  ignore(constructor_semi_locs)
  ignore(for_loc)
  ignore(with_loc)
  ignore(params_lparen_loc)
  ignore(params_rparen_loc)
  ignore(param_comma_locs)
  ignore(body_lbrace_loc)
  ignore(body_rbrace_loc)
  ignore(loc)
  ignore(attrs)
  ignore(doc)
  quantifiers.each(x => env.0.visit_TypeVarBinder(x))
  env.0.visit_Type(source_ty)
  view_constrs.each(x => env.0.visit_ConstrDecl(x))
  env.0.visit_Binder(view_func_name)
  parameters.each(param => env.0.visit_Parameter(param))
  env.0.visit_Expr(body)
  env.0.visit_Visibility(vis)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl_TopImplRelation(
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
  ignore(is_declare)
  ignore(attrs)
  ignore(loc)
  ignore(doc)
  ignore(declare_loc)
  ignore(impl_loc)
  ignore(type_params_lbracket_loc)
  ignore(type_params_rbracket_loc)
  type_param_comma_locs.each(fn(loc) { ignore(loc) })
  ignore(for_loc)
  env.0.visit_Type(self_ty)
  env.0.visit_TypeName(trait_)
  quantifiers.each(x => env.0.visit_TypeVarBinder(x))
  env.0.visit_Visibility(vis)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Impl_TopUsing(
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
  ignore(using_loc)
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  comma_locs.each(fn(loc) { ignore(loc) })
  ignore(loc)
  ignore(attrs)
  ignore(doc)
  env.0.visit_Label(pkg)
  names.each(name => {
    env.0.visit_AliasTarget(name.0)
    env.0.visit_UsingKind(name.1)
  })
  env.0.visit_Visibility(vis)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Apply(
  env,
  func~,
  args~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  ignore(lparen_loc)
  ignore(rparen_loc)
  ignore(comma_locs)
  ignore(loc)
  env.0.visit_Expr(func)
  args.each(x => env.0.visit_Argument(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Infix(
  env,
  op~,
  lhs~,
  rhs~,
  loc~,
) {
  ignore(loc)
  env.0.visit_Var(op)
  env.0.visit_Expr(lhs)
  env.0.visit_Expr(rhs)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Unary(
  env,
  op~,
  expr~,
  loc~,
) {
  ignore(loc)
  env.0.visit_Var(op)
  env.0.visit_Expr(expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Array(
  env,
  exprs~,
  lbracket_loc~,
  rbracket_loc~,
  comma_locs~,
  loc~,
) {
  ignore(lbracket_loc)
  ignore(rbracket_loc)
  ignore(comma_locs)
  ignore(loc)
  exprs.each(expr => env.0.visit_Expr(expr))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_ArraySpread(
  env,
  elems~,
  lbracket_loc~,
  rbracket_loc~,
  comma_locs~,
  loc~,
) {
  ignore(lbracket_loc)
  ignore(rbracket_loc)
  ignore(comma_locs)
  ignore(loc)
  elems.each(fn(elem) {
    match elem {
      Regular(expr) => env.0.visit_Expr(expr)
      Spread(expr~, dotdot_loc~, loc~) => {
        ignore(dotdot_loc)
        ignore(loc)
        env.0.visit_Expr(expr)
      }
    }
  })
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_ArrayGet(
  env,
  array~,
  index~,
  lbracket_loc~,
  rbracket_loc~,
  loc~,
) {
  ignore(lbracket_loc)
  ignore(rbracket_loc)
  ignore(loc)
  env.0.visit_Expr(array)
  env.0.visit_Expr(index)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_ArrayGetSlice(
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
  ignore(loc)
  ignore(index_loc)
  ignore(lbracket_loc)
  ignore(colon_loc)
  ignore(rbracket_loc)
  env.0.visit_Expr(array)
  start_index.each(e => env.0.visit_Expr(e))
  end_index.each(e => env.0.visit_Expr(e))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_ArraySet(
  env,
  array~,
  index~,
  value~,
  lbracket_loc~,
  rbracket_loc~,
  equal_loc~,
  loc~,
) {
  ignore(lbracket_loc)
  ignore(rbracket_loc)
  ignore(equal_loc)
  ignore(loc)
  env.0.visit_Expr(index)
  env.0.visit_Expr(value)
  env.0.visit_Expr(array)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_ArrayAugmentedSet(
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
  ignore(lbracket_loc)
  ignore(rbracket_loc)
  ignore(augmented_assign_loc)
  ignore(loc)
  env.0.visit_Var(op)
  env.0.visit_Expr(array)
  env.0.visit_Expr(index)
  env.0.visit_Expr(value)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Constant(
  env,
  c~,
  loc~,
) {
  ignore(loc)
  env.0.visit_Constant(c)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_MultilineString(
  env,
  elems~,
  loc~,
) {
  ignore(loc)
  elems.each(elem => env.0.visit_MultilineStringElem(elem))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Interp(
  env,
  elems~,
  loc~,
) {
  ignore(loc)
  elems.each(elem => env.0.visit_InterpElem(elem))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Constraint(
  env,
  expr~,
  ty~,
  lparen_loc~,
  colon_loc~,
  rparen_loc~,
  loc~,
) {
  ignore(lparen_loc)
  ignore(colon_loc)
  ignore(rparen_loc)
  ignore(loc)
  env.0.visit_Type(ty)
  env.0.visit_Expr(expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Constr(
  env,
  constr~,
  loc~,
) {
  ignore(loc)
  env.0.visit_Constructor(constr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_While(
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
  ignore(loc)
  ignore(label_colon_loc)
  ignore(while_loc)
  ignore(body_lbrace_loc)
  ignore(body_rbrace_loc)
  ignore(nobreak_loc)
  ignore(nobreak_lbrace_loc)
  ignore(nobreak_rbrace_loc)
  env.0.visit_Expr(loop_cond)
  env.0.visit_Expr(loop_body)
  while_else.each(else_block => env.0.visit_Expr(else_block))
  label.each(x => env.0.visit_Label(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Function(
  env,
  func~,
  loc~,
) {
  ignore(loc)
  env.0.visit_Func(func)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Ident(
  env,
  id~,
  loc~,
) {
  ignore(loc)
  env.0.visit_Var(id)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_If(
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
  ignore(if_loc)
  ignore(then_lbrace_loc)
  ignore(then_rbrace_loc)
  ignore(else_loc)
  ignore(else_lbrace_loc)
  ignore(else_rbrace_loc)
  ignore(loc)
  env.0.visit_Expr(cond)
  env.0.visit_Expr(ifso)
  ifnot.each(expr => env.0.visit_Expr(expr))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Guard(
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
  ignore(guard_loc)
  ignore(else_loc)
  ignore(else_lbrace_loc)
  ignore(else_rbrace_loc)
  ignore(semi_loc)
  ignore(loc)
  env.0.visit_Expr(cond)
  env.0.visit_Expr(body)
  otherwise.each(expr => env.0.visit_Expr(expr))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Is(
  env,
  expr~,
  pat~,
  is_loc~,
  loc~,
) {
  ignore(is_loc)
  ignore(loc)
  env.0.visit_Expr(expr)
  env.0.visit_Pattern(pat)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Defer(
  env,
  expr~,
  body~,
  defer_loc~,
  semi_loc~,
  loc~,
) {
  ignore(defer_loc)
  ignore(semi_loc)
  ignore(loc)
  env.0.visit_Expr(expr)
  env.0.visit_Expr(body)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_LetFn(
  env,
  name~,
  func~,
  body~,
  semi_loc~,
  loc~,
) {
  ignore(semi_loc)
  ignore(loc)
  env.0.visit_Binder(name)
  env.0.visit_Func(func)
  env.0.visit_Expr(body)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_LetAnd(
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
  ignore(letrec_loc)
  ignore(and_locs)
  ignore(equal_locs)
  ignore(annot_colon_locs)
  ignore(semi_loc)
  ignore(loc)
  bindings.each(binding => {
    match binding {
      (binder, ty, func) => {
        env.0.visit_Binder(binder)
        ty.each(x => env.0.visit_Type(x))
        env.0.visit_Func(func)
      }
    }
  })
  env.0.visit_Expr(body)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Let(
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
  ignore(let_loc)
  ignore(annot_colon_loc)
  ignore(equal_loc)
  ignore(semi_loc)
  ignore(loc)
  env.0.visit_Pattern(pattern)
  env.0.visit_Expr(expr)
  env.0.visit_Expr(body)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Sequence(
  env,
  exprs~,
  last_expr~,
  semi_locs~,
  loc~,
) {
  ignore(semi_locs)
  ignore(loc)
  exprs.each(expr => env.0.visit_Expr(expr))
  env.0.visit_Expr(last_expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Tuple(
  env,
  exprs~,
  lparen_loc~,
  rparen_loc~,
  comma_locs~,
  loc~,
) {
  ignore(lparen_loc)
  ignore(rparen_loc)
  ignore(comma_locs)
  ignore(loc)
  exprs.each(expr => env.0.visit_Expr(expr))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Record(
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
  ignore(coloncolon_loc)
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  ignore(comma_locs)
  ignore(semi_locs)
  ignore(loc)
  type_name.each(x => env.0.visit_TypeName(x))
  fields.each(x => env.0.visit_FieldDef(x))
  env.0.visit_TrailingMark(trailing)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_RecordUpdate(
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
  ignore(coloncolon_loc)
  ignore(lbrace_loc)
  ignore(dotdot_loc)
  ignore(rbrace_loc)
  ignore(comma_locs)
  ignore(loc)
  type_name.each(x => env.0.visit_TypeName(x))
  fields.each(x => env.0.visit_FieldDef(x))
  env.0.visit_Expr(record)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Field(
  env,
  record~,
  accessor~,
  loc~,
) {
  ignore(loc)
  env.0.visit_Expr(record)
  env.0.visit_Accessor(accessor)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Method(
  env,
  type_name~,
  method_name~,
  coloncolon_loc~,
  loc~,
) {
  ignore(coloncolon_loc)
  ignore(loc)
  env.0.visit_TypeName(type_name)
  env.0.visit_Label(method_name)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_DotApply(
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
  ignore(return_self)
  ignore(dot_loc)
  ignore(lparen_loc)
  ignore(rparen_loc)
  ignore(comma_locs)
  ignore(loc)
  env.0.visit_Expr(self)
  env.0.visit_Label(method_name)
  args.each(x => env.0.visit_Argument(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_As(
  env,
  expr~,
  trait_~,
  as_loc~,
  loc~,
) {
  ignore(as_loc)
  ignore(loc)
  env.0.visit_TypeName(trait_)
  env.0.visit_Expr(expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Mutate(
  env,
  record~,
  accessor~,
  field~,
  augmented_by~,
  equal_loc~,
  augmented_assign_loc~,
  loc~,
) {
  ignore(equal_loc)
  ignore(augmented_assign_loc)
  ignore(loc)
  env.0.visit_Expr(record)
  env.0.visit_Accessor(accessor)
  env.0.visit_Expr(field)
  augmented_by.each(x => env.0.visit_Var(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Match(
  env,
  expr~,
  cases~,
  match_loc~,
  lbrace_loc~,
  rbrace_loc~,
  semi_locs~,
  loc~,
) {
  ignore(match_loc)
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  ignore(semi_locs)
  ignore(loc)
  env.0.visit_Expr(expr)
  cases.each(x => env.0.visit_Case(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_LetMut(
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
  ignore(let_loc)
  ignore(mut_loc)
  ignore(annot_colon_loc)
  ignore(equal_loc)
  ignore(semi_loc)
  ignore(loc)
  env.0.visit_Binder(binder)
  ty.each(x => env.0.visit_Type(x))
  env.0.visit_Expr(expr)
  env.0.visit_Expr(body)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Pipe(
  env,
  lhs~,
  rhs~,
  pipe_loc~,
  loc~,
) {
  ignore(pipe_loc)
  ignore(loc)
  env.0.visit_Expr(lhs)
  env.0.visit_Expr(rhs)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Assign(
  env,
  var_~,
  expr~,
  augmented_by~,
  equal_loc~,
  augmented_assign_loc~,
  loc~,
) {
  ignore(equal_loc)
  ignore(augmented_assign_loc)
  ignore(loc)
  env.0.visit_Var(var_)
  augmented_by.each(x => env.0.visit_Var(x))
  env.0.visit_Expr(expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Hole(
  env,
  loc~,
  kind~,
) {
  ignore(loc)
  env.0.visit_Hole(kind)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Return(
  env,
  return_value~,
  return_loc~,
  loc~,
) {
  ignore(return_loc)
  ignore(loc)
  return_value.each(value => env.0.visit_Expr(value))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Raise(
  env,
  err_value~,
  raise_loc~,
  loc~,
) {
  ignore(raise_loc)
  ignore(loc)
  env.0.visit_Expr(err_value)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Unit(
  _,
  loc~,
  faked~,
  lparen_loc~,
  rparen_loc~,
) {
  ignore(loc)
  ignore(faked)
  ignore(lparen_loc)
  ignore(rparen_loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Break(
  env,
  arg~,
  label~,
  break_loc~,
  post_label_loc~,
  loc~,
) {
  ignore(break_loc)
  ignore(post_label_loc)
  ignore(loc)
  label.each(x => env.0.visit_Label(x))
  arg.each(x => env.0.visit_Expr(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Continue(
  env,
  args~,
  label~,
  continue_loc~,
  post_label_loc~,
  comma_locs~,
  loc~,
) {
  ignore(continue_loc)
  ignore(post_label_loc)
  ignore(comma_locs)
  ignore(loc)
  args.each(arg => env.0.visit_Expr(arg))
  label.each(x => env.0.visit_Label(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_For(
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
  ignore(loc)
  ignore(label_colon_loc)
  ignore(for_loc)
  ignore(binder_comma_locs)
  ignore(binder_equal_locs)
  ignore(semi_locs)
  ignore(continue_comma_locs)
  ignore(continue_equal_locs)
  ignore(body_lbrace_loc)
  ignore(body_rbrace_loc)
  ignore(nobreak_loc)
  ignore(nobreak_lbrace_loc)
  ignore(nobreak_rbrace_loc)
  binders.each(p => {
    env.0.visit_Binder(p.0)
    env.0.visit_Expr(p.1)
  })
  condition.each(cond => env.0.visit_Expr(cond))
  continue_block.each(p => {
    env.0.visit_Binder(p.0)
    env.0.visit_Expr(p.1)
  })
  env.0.visit_Expr(body)
  for_else.each(else_block => env.0.visit_Expr(else_block))
  label.each(x => env.0.visit_Label(x))
  where_clause.each(x => env.0.visit_WhereClause(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_ForEach(
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
  ignore(loc)
  ignore(label_colon_loc)
  ignore(for_loc)
  ignore(binder_comma_locs)
  ignore(in_loc)
  ignore(semi_locs)
  ignore(init_comma_locs)
  ignore(init_equal_locs)
  ignore(continue_comma_locs)
  ignore(continue_equal_locs)
  ignore(body_lbrace_loc)
  ignore(body_rbrace_loc)
  ignore(nobreak_loc)
  ignore(nobreak_lbrace_loc)
  ignore(nobreak_rbrace_loc)
  binders.each(binder => binder.each(x => env.0.visit_Binder(x)))
  env.0.visit_Expr(expr)
  init.each(p => {
    env.0.visit_Binder(p.0)
    env.0.visit_Expr(p.1)
  })
  continue_block.each(p => {
    env.0.visit_Binder(p.0)
    env.0.visit_Expr(p.1)
  })
  env.0.visit_Expr(body)
  else_block.each(else_block => env.0.visit_Expr(else_block))
  where_clause.each(x => env.0.visit_WhereClause(x))
  label.each(x => env.0.visit_Label(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Try(
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
  ignore(loc)
  ignore(try_loc)
  ignore(catch_loc)
  ignore(else_loc)
  ignore(catch_lbrace_loc)
  ignore(catch_rbrace_loc)
  ignore(catch_semi_locs)
  ignore(noraise_lbrace_loc)
  ignore(noraise_rbrace_loc)
  ignore(noraise_semi_locs)
  ignore(has_try)
  env.0.visit_Expr(body)
  catch_.each(x => env.0.visit_Case(x))
  try_else.each(try_else => try_else.each(x => env.0.visit_Case(x)))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_TryOperator(
  env,
  body~,
  kind~,
  try_loc~,
  loc~,
) {
  ignore(loc)
  ignore(try_loc)
  env.0.visit_Expr(body)
  env.0.visit_TryOperatorKind(kind)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Map(
  env,
  elems~,
  lbrace_loc~,
  rbrace_loc~,
  comma_locs~,
  loc~,
) {
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  ignore(comma_locs)
  ignore(loc)
  elems.each(x => env.0.visit_MapExprElem(x))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_Group(
  env,
  expr~,
  group~,
  left_delim_loc~,
  right_delim_loc~,
  loc~,
) {
  ignore(left_delim_loc)
  ignore(right_delim_loc)
  ignore(loc)
  env.0.visit_Expr(expr)
  env.0.visit_Group(group)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_StaticAssert(
  env,
  asserts~,
  body~,
) {
  asserts.each(x => env.0.visit_StaticAssertion(x))
  env.0.visit_Expr(body)
}

///|
fn[T : IterVisitor] iter_regex_pattern(
  env : IterVisitorBase[T],
  pat : RegexPattern,
) -> Unit {
  match pat {
    Literal(lit~, loc~) => {
      ignore(lit)
      ignore(loc)
    }
    Reference(lid~, loc~) => {
      env.0.visit_LongIdent(lid)
      ignore(loc)
    }
    Sequence(pat1~, pat2~, plus_loc~, loc~) => {
      iter_regex_pattern(env, pat1)
      iter_regex_pattern(env, pat2)
      ignore(plus_loc)
      ignore(loc)
    }
    Alternation(pat1~, pat2~, bar_loc~, loc~) => {
      iter_regex_pattern(env, pat1)
      iter_regex_pattern(env, pat2)
      ignore(bar_loc)
      ignore(loc)
    }
    Alias(pat~, binder~, as_loc~, loc~) => {
      iter_regex_pattern(env, pat)
      env.0.visit_Binder(binder)
      ignore(as_loc)
      ignore(loc)
    }
  }
}

///|
fn[T : IterVisitor] iter_list_comprehension_kind(
  env : IterVisitorBase[T],
  kind : ListComprehensionKind,
) -> Unit {
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
      ignore(for_loc)
      ignore(in_loc)
      ignore(binder_comma_locs)
      ignore(semi_locs)
      ignore(init_comma_locs)
      ignore(init_equal_locs)
      ignore(continue_comma_locs)
      ignore(continue_equal_locs)
      binders.each(binder => binder.each(x => env.0.visit_Binder(x)))
      env.0.visit_Expr(expr)
      init.each(p => {
        env.0.visit_Binder(p.0)
        env.0.visit_Expr(p.1)
      })
      continue_block.each(p => {
        env.0.visit_Binder(p.0)
        env.0.visit_Expr(p.1)
      })
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
      ignore(for_loc)
      ignore(binder_comma_locs)
      ignore(semi_locs)
      ignore(binder_equal_locs)
      ignore(continue_comma_locs)
      ignore(continue_equal_locs)
      binders.each(p => {
        env.0.visit_Binder(p.0)
        env.0.visit_Expr(p.1)
      })
      condition.each(cond => env.0.visit_Expr(cond))
      continue_block.each(p => {
        env.0.visit_Binder(p.0)
        env.0.visit_Expr(p.1)
      })
    }
  }
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr(
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
      ignore(loc)
      ignore(lbracket_loc)
      ignore(rbracket_loc)
      ignore(if_loc)
      ignore(fat_arrow_loc)
      iter_list_comprehension_kind(env, kind)
      guard_.each(x => env.0.visit_Expr(x))
      env.0.visit_Expr(body)
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
      ignore(eq_tilde_loc)
      ignore(rhs_lparen_loc)
      ignore(rhs_rparen_loc)
      ignore(binding_comma_locs)
      ignore(binding_equal_locs)
      ignore(loc)
      env.0.visit_Expr(expr)
      iter_regex_pattern(env, pat)
      bindings.each(binding => {
        let (label, binder) = binding
        env.0.visit_Label(label)
        binder.each(x => env.0.visit_Binder(x))
      })
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
      ignore(rev_pipe_loc)
      ignore(loc)
      env.0.visit_Expr(lhs)
      env.0.visit_Expr(rhs)
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
      ignore(kind)
      ignore(quantifier_loc)
      ignore(colon_loc)
      ignore(comma_loc)
      ignore(loc)
      env.0.visit_Binder(binder)
      env.0.visit_Type(binder_ty)
      env.0.visit_Expr(body)
    }
    Implies(lhs~, rhs~, implies_loc~, loc~) => {
      ignore(implies_loc)
      ignore(loc)
      env.0.visit_Expr(lhs)
      env.0.visit_Expr(rhs)
    }
    ProofAssert(expr~, proof_assert_loc~, loc~) => {
      ignore(proof_assert_loc)
      ignore(loc)
      env.0.visit_Expr(expr)
    }
    ProofLet(binder~, expr~, proof_let_loc~, equal_loc~, loc~) => {
      ignore(proof_let_loc)
      ignore(equal_loc)
      ignore(loc)
      env.0.visit_Binder(binder)
      env.0.visit_Expr(expr)
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
      ignore(lt_plus_loc)
      ignore(loc)
      env.0.visit_Expr(expr)
      env.0.visit_Expr(template)
    }
    StaticAssert(asserts~, body~) =>
      env.0.visit_Expr_StaticAssert(asserts~, body~)
  }
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_IsLexMatch(
  env,
  expr~,
  strategy~,
  pat~,
  pat_loc~,
  lexmatch_question_loc~,
  with_loc~,
  loc~,
) {
  ignore(pat_loc)
  ignore(lexmatch_question_loc)
  ignore(with_loc)
  ignore(loc)
  strategy.each(strategy => env.0.visit_Label(strategy))
  pat.each(pat => env.0.visit_LexTopPattern(pat))
  env.0.visit_Expr(expr)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_Expr_LexMatch(
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
  ignore(match_loc)
  ignore(with_loc)
  ignore(lbrace_loc)
  ignore(rbrace_loc)
  ignore(semi_locs)
  ignore(loc)
  strategy.each(strategy => env.0.visit_Label(strategy))
  env.0.visit_Expr(expr)
  cases.each(case => env.0.visit_LexCase(case))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexCase(
  env,
  lex_case,
) {
  lex_case.pat.each(pat => env.0.visit_LexTopPattern(pat))
  lex_case.guard_.each(guard_ => env.0.visit_Expr(guard_))
  env.0.visit_Expr(lex_case.body)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexTopPattern(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexTopPattern_Pattern(
  env,
  lex_pattern,
) {
  env.0.visit_LexPattern(lex_pattern)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexTopPattern_Binder(
  env,
  binder,
) {
  env.0.visit_Binder(binder)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexTopPattern_Wildcard(
  env,
  loc~,
) {
  ignore(env)
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexPattern(
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
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexPattern_Regex(
  env,
  lit~,
  offset~,
  loc~,
) {
  ignore(env)
  ignore(lit)
  ignore(offset)
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexPattern_ConstantRef(
  env,
  lid~,
  loc~,
) {
  env.0.visit_LongIdent(lid)
  ignore(loc)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexPattern_RegexInterp(
  env,
  elems~,
  loc~,
) {
  ignore(loc)
  elems.each(elem => env.0.visit_InterpElem(elem))
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexPattern_Alias(
  env,
  pat~,
  binder~,
  as_loc~,
  loc~,
) {
  ignore(as_loc)
  ignore(loc)
  env.0.visit_LexPattern(pat)
  env.0.visit_Binder(binder)
}

///|
pub impl[T : IterVisitor] IterVisitor for IterVisitorBase[T] with fn visit_LexPattern_Sequence(
  env,
  pats~,
  semi_locs~,
  loc~,
) {
  semi_locs.each(ignore)
  ignore(loc)
  pats.each(pat => env.0.visit_LexPattern(pat))
}

///|
fn[T] Option::each(self : T?, f : (T) -> Unit raise?) -> Unit raise? {
  match self {
    Some(t) => f(t)
    None => ()
  }
}
