///|
pub(all) enum Visibility {
  Default
  Pub(attr~ : String?, loc~ : Location)
  Priv(loc~ : Location)
}

///|
pub(all) enum Constant {
  Bool(Bool)
  Byte(ByteLiteral)
  Bytes(BytesLiteral)
  Char(CharLiteral)
  Int(String)
  Int64(String)
  UInt(String)
  UInt64(String)
  Float(String)
  Double(String)
  String(StringLiteral)
  BigInt(String)
}

///|
pub(all) struct Label {
  name : String
  loc : Location
}

///|
pub(all) struct ConstrName {
  name : String
  loc : Location
}

///|
// checked!
pub(all) enum LongIdent {
  Ident(name~ : String)
  Dot(pkg~ : String, id~ : String)
}

///| The dummy LongIdent is used in typer to represent an parsing error.
pub fn LongIdent::dummy() -> LongIdent {
  Ident(name="")
}

///|
pub(all) struct TypeName {
  name : LongIdent
  is_object : Bool
  loc : Location
}

///|
pub(all) struct ConstrId {
  id : LongIdent
  loc : Location
}

///|
pub(all) enum Type {
  Any(loc~ : Location)
  Arrow(
    args~ : @list.List[Type],
    res~ : Type,
    err~ : ErrorType,
    is_async~ : Bool,
    loc~ : Location
  )
  Tuple(tys~ : @list.List[Type], loc~ : Location)
  Name(constr_id~ : ConstrId, tys~ : @list.List[Type], loc~ : Location)
  Option(ty~ : Type, loc~ : Location, question_loc~ : Location)
  Object(ConstrId)
}

///|
pub(all) enum ErrorType {
  ErrorType(ty~ : Type)
  DefaultErrorType(loc~ : Location)
  NoErrorType
  Noraise(loc~ : Location)
  MaybeError(ty~ : Type)
}

///|
pub(all) struct ConstrParam {
  ty : Type
  mut_ : Bool
  label : Label?
}

///|
pub(all) struct ConstrDecl {
  name : ConstrName
  args : @list.List[ConstrParam]?
  tag : (String, Location)?
  loc : Location
  mut doc : DocString
}

///|
pub(all) enum ExceptionDecl {
  NoPayload
  SinglePayload(Type)
  EnumPayload(@list.List[ConstrDecl])
}

///|
pub(all) struct FieldName {
  label : String
  loc : Location
}

///|
pub(all) struct FieldDecl {
  name : FieldName
  ty : Type
  mut_ : Bool
  vis : Visibility
  loc : Location
  mut doc : DocString
}

///|
pub(all) enum TypeDesc {
  Abstract
  Extern
  Newtype(Type)
  Error(ExceptionDecl)
  Variant(@list.List[ConstrDecl])
  Record(@list.List[FieldDecl])
  TupleStruct(@list.List[Type])
  Alias(Type)
}

///|
pub(all) enum Hole {
  Synthesized
  Incomplete
  Todo
}

///|
pub(all) enum ArgumentKind {
  Positional
  Labelled(Label)
  LabelledPun(Label)
  LabelledOption(label~ : Label, question_loc~ : Location)
  LabelledOptionPun(label~ : Label, question_loc~ : Location)
}

///|
pub(all) enum FnKind {
  Lambda
  Matrix
  Arrow
}

///|
pub(all) enum Group {
  Brace
  Paren
}

///|
pub(all) enum TrailingMark {
  Comma
  Semi
  None
}

///|
// checked!
pub(all) enum ApplyAttr {
  NoAttr
  Exclamation
  Question
}

///|
pub(all) struct TypeVarConstraint {
  trait_ : LongIdent
  loc : Location
}

///|
pub(all) struct TypeVarBinder {
  name : String
  constraints : @list.List[TypeVarConstraint]
  loc : Location
}

///|
pub(all) struct TypeDeclBinder {
  name : String?
  loc : Location
}

///|
pub(all) struct Binder {
  name : String
  loc : Location
}

///|
pub(all) struct Var {
  name : LongIdent
  loc : Location
}

///|
pub(all) enum ConstructorExtraInfo {
  TypeName(TypeName)
  Package(String)
  NoExtraInfo
}

///|
pub(all) struct Constructor {
  name : ConstrName
  extra_info : ConstructorExtraInfo
  loc : Location
}

///|
pub(all) enum Accessor {
  Label(Label)
  Index(tuple_index~ : Int, loc~ : Location)
  Newtype(loc~ : Location)
}

///|
pub(all) struct AliasTarget {
  binder : Binder
  target : Label?
}

///|
pub(all) struct Argument {
  value : Expr
  kind : ArgumentKind
}

///|
pub(all) enum Parameter {
  DiscardPositional(ty~ : Type?, loc~ : Location)
  Positional(binder~ : Binder, ty~ : Type?)
  Labelled(binder~ : Binder, ty~ : Type?)
  Optional(binder~ : Binder, default~ : Expr, ty~ : Type?)
  QuestionOptional(binder~ : Binder, ty~ : Type?)
}

///|
pub typealias @list.List[Parameter] as Parameters

///|
pub(all) struct Case {
  pattern : Pattern
  guard_ : Expr?
  body : Expr
}

///|
pub(all) struct MultiArgCase {
  patterns : @list.List[Pattern]
  guard_ : Expr?
  body : Expr
}

///|
pub(all) enum SpreadableElem {
  Regular(Expr)
  Spread(expr~ : Expr, loc~ : Location)
}

///|
pub(all) struct MapExprElem {
  key : Constant
  expr : Expr
  key_loc : Location
  loc : Location
}

///|
pub(all) struct StaticAssertion {
  ty : Type
  trait_ : LongIdent
  loc : Location
  msg : String
}

///|
pub(all) enum Func {
  Lambda(
    parameters~ : Parameters,
    params_loc~ : Location,
    body~ : Expr,
    return_type~ : Type?,
    error_type~ : ErrorType,
    kind~ : FnKind,
    has_error~ : Location?,
    is_async~ : Bool,
    loc~ : Location
  )
  Match(
    cases~ : @list.List[MultiArgCase],
    has_error~ : Location?,
    is_async~ : Bool,
    fn_loc~ : Location,
    loc~ : Location
  )
}

///|
pub(all) struct FieldDef {
  label : Label
  expr : Expr
  is_pun : Bool
  loc : Location
}

///|
pub(all) enum InterpElem {
  Literal(repr~ : StringLiteral, loc~ : Location)
  Expr(expr~ : Expr, loc~ : Location)
  Source(@syntax_types.InterpSource)
}

///|
pub(all) enum MultilineStringElem {
  String(String)
  Interp(@list.List[InterpElem])
}

///|
// checked!
pub(all) enum Expr {
  Apply(
    func~ : Expr,
    args~ : @list.List[Argument],
    attr~ : ApplyAttr,
    loc~ : Location
  )
  Infix(op~ : Var, lhs~ : Expr, rhs~ : Expr, loc~ : Location)
  Unary(op~ : Var, expr~ : Expr, loc~ : Location)
  Array(exprs~ : @list.List[Expr], loc~ : Location)
  ArraySpread(elems~ : @list.List[SpreadableElem], loc~ : Location)
  ArrayGet(array~ : Expr, index~ : Expr, loc~ : Location)
  ArrayGetSlice(
    array~ : Expr,
    start_index~ : Expr?,
    end_index~ : Expr?,
    index_loc~ : Location,
    loc~ : Location
  )
  ArraySet(array~ : Expr, index~ : Expr, value~ : Expr, loc~ : Location)
  ArrayAugmentedSet(
    op~ : Var,
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    loc~ : Location
  )
  Constant(c~ : Constant, loc~ : Location)
  MultilineString(elems~ : @list.List[MultilineStringElem], loc~ : Location)
  Interp(elems~ : @list.List[InterpElem], loc~ : Location)
  Constraint(expr~ : Expr, ty~ : Type, loc~ : Location)
  Constr(constr~ : Constructor, loc~ : Location)
  While(
    loop_cond~ : Expr,
    loop_body~ : Expr,
    while_else~ : Expr?,
    label~ : Label?,
    loc~ : Location
  )
  Function(func~ : Func, loc~ : Location)
  Ident(id~ : Var, loc~ : Location)
  If(cond~ : Expr, ifso~ : Expr, ifnot~ : Expr?, loc~ : Location)
  Guard(cond~ : Expr, otherwise~ : Expr?, body~ : Expr, loc~ : Location)
  Is(expr~ : Expr, pat~ : Pattern, loc~ : Location)
  Defer(expr~ : Expr, body~ : Expr, loc~ : Location)
  LetFn(name~ : Binder, func~ : Func, body~ : Expr, loc~ : Location)
  LetRec(bindings~ : @list.List[(Binder, Func)], body~ : Expr, loc~ : Location)
  LetAnd(
    bindings~ : @list.List[(Binder, Type?, Func)],
    body~ : Expr,
    loc~ : Location
  )
  Let(pattern~ : Pattern, expr~ : Expr, body~ : Expr, loc~ : Location)
  Sequence(exprs~ : @list.List[Expr], last_expr~ : Expr, loc~ : Location)
  Tuple(exprs~ : @list.List[Expr], loc~ : Location)
  Record(
    type_name~ : TypeName?,
    fields~ : @list.List[FieldDef],
    trailing~ : TrailingMark,
    loc~ : Location
  )
  RecordUpdate(
    type_name~ : TypeName?,
    record~ : Expr,
    fields~ : @list.List[FieldDef],
    loc~ : Location
  )
  Field(record~ : Expr, accessor~ : Accessor, loc~ : Location)
  Method(type_name~ : TypeName, method_name~ : Label, loc~ : Location)
  DotApply(
    self~ : Expr,
    method_name~ : Label,
    args~ : @list.List[Argument],
    return_self~ : Bool,
    attr~ : ApplyAttr,
    loc~ : Location
  )
  As(expr~ : Expr, trait_~ : TypeName, loc~ : Location)
  Mutate(
    record~ : Expr,
    accessor~ : Accessor,
    field~ : Expr,
    augmented_by~ : Var?,
    loc~ : Location
  )
  Match(
    expr~ : Expr,
    cases~ : @list.List[Case],
    match_loc~ : Location,
    using_~ : Label?,
    loc~ : Location
  )
  LetMut(
    binder~ : Binder,
    ty~ : Type?,
    expr~ : Expr,
    body~ : Expr,
    loc~ : Location
  )
  Pipe(lhs~ : Expr, rhs~ : Expr, loc~ : Location)
  Assign(var_~ : Var, expr~ : Expr, augmented_by~ : Var?, loc~ : Location)
  Hole(loc~ : Location, kind~ : Hole)
  Return(return_value~ : Expr?, loc~ : Location)
  Raise(err_value~ : Expr, loc~ : Location)
  Unit(loc~ : Location, faked~ : Bool)
  Break(arg~ : Expr?, label~ : Label?, loc~ : Location)
  Continue(args~ : @list.List[Expr], label~ : Label?, loc~ : Location)
  Loop(
    args~ : @list.List[Expr],
    body~ : @list.List[MultiArgCase],
    label~ : Label?,
    loop_loc~ : Location,
    loc~ : Location
  )
  For(
    binders~ : @list.List[(Binder, Expr)],
    condition~ : Expr?,
    continue_block~ : @list.List[(Binder, Expr)],
    body~ : Expr,
    for_else~ : Expr?,
    label~ : Label?,
    loc~ : Location
  )
  ForEach(
    binders~ : @list.List[Binder?],
    expr~ : Expr,
    body~ : Expr,
    else_block~ : Expr?,
    label~ : Label?,
    loc~ : Location
  )
  Try(
    body~ : Expr,
    catch_~ : @list.List[Case],
    catch_all~ : Bool,
    try_else~ : @list.List[Case]?,
    has_try~ : Bool,
    try_loc~ : Location,
    catch_loc~ : Location,
    else_loc~ : Location,
    loc~ : Location
  )
  TryOperator(
    body~ : Expr,
    kind~ : TryOperatorKind,
    try_loc~ : Location,
    loc~ : Location
  )
  Map(elems~ : @list.List[MapExprElem], loc~ : Location)
  Group(expr~ : Expr, group~ : Group, loc~ : Location)
  StaticAssert(asserts~ : @list.List[StaticAssertion], body~ : Expr)
}

///|
pub(all) enum TryOperatorKind {
  Question
  Exclamation
}

///|
pub(all) enum DotDotBinder {
  Underscore
  NoBinder
  BinderAs(Binder)
  Binder(Binder)
}

///|
pub(all) enum ArrayPattern {
  Pattern(Pattern)
  StringSpread(StringLiteral)
  BytesSpread(BytesLiteral)
  ConstSpread(binder~ : Binder, pkg~ : String?, loc~ : Location)
}

///|
pub(all) enum ArrayPatterns {
  Closed(@list.List[ArrayPattern])
  Open(@list.List[ArrayPattern], @list.List[ArrayPattern], DotDotBinder)
}

///|
pub(all) struct FieldPat {
  label : Label
  pattern : Pattern
  is_pun : Bool
  loc : Location
}

///|
pub(all) struct ConstrPatArg {
  pat : Pattern
  kind : ArgumentKind
}

///|
pub(all) struct MapPatElem {
  key : Constant
  pat : Pattern
  match_absent : Bool
  key_loc : Location
  loc : Location
}

///|
// checked!
pub(all) enum Pattern {
  Alias(pat~ : Pattern, alias_~ : Binder, loc~ : Location)
  Any(loc~ : Location)
  Array(pats~ : ArrayPatterns, loc~ : Location)
  Constant(c~ : Constant, loc~ : Location)
  Constraint(pat~ : Pattern, ty~ : Type, loc~ : Location)
  Constr(
    constr~ : Constructor,
    args~ : @list.List[ConstrPatArg]?,
    is_open~ : Bool,
    loc~ : Location
  )
  Or(pat1~ : Pattern, pat2~ : Pattern, loc~ : Location)
  Tuple(pats~ : @list.List[Pattern], loc~ : Location)
  Var(Binder)
  Record(fields~ : @list.List[FieldPat], is_closed~ : Bool, loc~ : Location)
  Map(elems~ : @list.List[MapPatElem], is_closed~ : Bool, loc~ : Location)
  Range(lhs~ : Pattern, rhs~ : Pattern, inclusive~ : Bool, loc~ : Location)
}

///|
pub typealias (StringLiteral, Location)? as TestName

///|
pub(all) struct LocalTypeDecl {
  tycon : String
  tycon_loc : Location
  components : TypeDesc
  deriving : @list.List[DerivingDirective]
}

///|
// checked!
pub(all) struct DerivingDirective {
  type_name : TypeName
  args : @list.List[Argument]
  loc : Location
}

///|
pub(all) struct TypeDecl {
  tycon : String
  tycon_loc : Location
  params : @list.List[TypeDeclBinder]
  components : TypeDesc
  attrs : @list.List[Attribute]
  mut doc : DocString
  type_vis : Visibility
  deriving : @list.List[DerivingDirective]
  loc : Location
}

///|
pub(all) enum FuncStubs {
  Import(module_name~ : StringLiteral, func_name~ : StringLiteral)
  Embedded(language~ : StringLiteral?, code~ : EmbeddedCode)
}

///|
pub(all) enum EmbeddedCode {
  CodeString(StringLiteral)
  CodeMultilineString(@list.List[String])
}

///|
pub(all) enum DeclBody {
  DeclBody(local_types~ : @list.List[LocalTypeDecl], expr~ : Expr)
  DeclStubs(FuncStubs)
}

///|
pub(all) struct FunDecl {
  type_name : TypeName?
  name : Binder
  has_error : Location?
  is_async : Bool
  decl_params : Parameters?
  params_loc : Location
  quantifiers : @list.List[TypeVarBinder]
  return_type : Type?
  error_type : ErrorType
  vis : Visibility
  attrs : @list.List[Attribute]
  mut doc : DocString
}

///|
pub(all) struct TraitMethodDecl {
  name : Binder
  has_error : Location?
  is_async : Bool
  quantifiers : @list.List[TypeVarBinder]
  params : Parameters
  return_type : Type?
  error_type : ErrorType
  has_default : Location?
  loc : Location
}

///|
pub(all) struct TraitDecl {
  name : Binder
  supers : @list.List[TypeVarConstraint]
  methods : @list.List[TraitMethodDecl]
  vis : Visibility
  loc : Location
  attrs : @list.List[Attribute]
  mut doc : DocString
}

///|
pub(all) enum Impl {
  TopExpr(
    expr~ : Expr,
    is_main~ : Bool,
    local_types~ : @list.List[LocalTypeDecl],
    loc~ : Location
  )
  TopTest(
    expr~ : Expr,
    name~ : TestName,
    params~ : Parameters?,
    local_types~ : @list.List[LocalTypeDecl],
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    mut doc~ : DocString
  )
  TopTypeDef(TypeDecl)
  TopFuncDef(fun_decl~ : FunDecl, decl_body~ : DeclBody, loc~ : Location)
  TopFuncAlias(
    pkg~ : Label?,
    type_name~ : Label?,
    targets~ : @list.List[AliasTarget],
    vis~ : Visibility,
    attrs~ : @list.List[Attribute],
    is_list~ : Bool,
    mut doc~ : DocString,
    loc~ : Location
  )
  TopLetDef(
    binder~ : Binder,
    ty~ : Type?,
    expr~ : Expr,
    vis~ : Visibility,
    is_constant~ : Bool,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    mut doc~ : DocString
  )
  TopTrait(TraitDecl)
  TopBatchTypeAlias(
    pkg~ : Label?,
    targets~ : @list.List[AliasTarget],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    is_list~ : Bool,
    mut doc~ : DocString
  )
  TopBatchTraitAlias(
    pkg~ : Label?,
    targets~ : @list.List[AliasTarget],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    is_list~ : Bool,
    mut doc~ : DocString
  )
  TopImpl(
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
    mut doc~ : DocString
  )
  TopView(
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
    mut doc~ : DocString
  )
  TopImplRelation(
    self_ty~ : Type,
    trait_~ : TypeName,
    quantifiers~ : @list.List[TypeVarBinder],
    vis~ : Visibility,
    attrs~ : @list.List[Attribute],
    loc~ : Location,
    mut doc~ : DocString
  )
}

///|
pub typealias @list.List[Impl] as Impls
