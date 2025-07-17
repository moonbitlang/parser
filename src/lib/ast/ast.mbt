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
    args~ : @list.T[Type],
    res~ : Type,
    err~ : ErrorType,
    is_async~ : Bool,
    loc~ : Location
  )
  Tuple(tys~ : @list.T[Type], loc~ : Location)
  Name(constr_id~ : ConstrId, tys~ : @list.T[Type], loc~ : Location)
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
  args : @list.T[ConstrParam]?
  tag : (String, Location)?
  loc : Location
  doc : DocString
}

///|
pub(all) enum ExceptionDecl {
  NoPayload
  SinglePayload(Type)
  EnumPayload(@list.T[ConstrDecl])
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
}

///|
pub(all) enum TypeDesc {
  Abstract
  Extern
  Newtype(Type)
  Error(ExceptionDecl)
  Variant(@list.T[ConstrDecl])
  Record(@list.T[FieldDecl])
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
  constraints : @list.T[TypeVarConstraint]
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
pub typealias @list.T[Parameter] as Parameters

///|
pub(all) struct Case {
  pattern : Pattern
  guard_ : Expr?
  body : Expr
}

///|
pub(all) struct MultiArgCase {
  patterns : @list.T[Pattern]
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
    cases~ : @list.T[MultiArgCase],
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
  Source(@ast_types.InterpSource)
}

///|
pub(all) enum MultilineStringElem {
  String(String)
  Interp(@list.T[InterpElem])
}

///|
// checked!
pub(all) enum Expr {
  Apply(
    func~ : Expr,
    args~ : @list.T[Argument],
    attr~ : ApplyAttr,
    loc~ : Location
  )
  Infix(op~ : Var, lhs~ : Expr, rhs~ : Expr, loc~ : Location)
  Unary(op~ : Var, expr~ : Expr, loc~ : Location)
  Array(exprs~ : @list.T[Expr], loc~ : Location)
  ArraySpread(elems~ : @list.T[SpreadableElem], loc~ : Location)
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
  MultilineString(elems~ : @list.T[MultilineStringElem], loc~ : Location)
  Interp(elems~ : @list.T[InterpElem], loc~ : Location)
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
  LetRec(bindings~ : @list.T[(Binder, Func)], body~ : Expr, loc~ : Location)
  LetAnd(
    bindings~ : @list.T[(Binder, Type?, Func)],
    body~ : Expr,
    loc~ : Location
  )
  Let(pattern~ : Pattern, expr~ : Expr, body~ : Expr, loc~ : Location)
  Sequence(exprs~ : @list.T[Expr], last_expr~ : Expr, loc~ : Location)
  Tuple(exprs~ : @list.T[Expr], loc~ : Location)
  Record(
    type_name~ : TypeName?,
    fields~ : @list.T[FieldDef],
    trailing~ : TrailingMark,
    loc~ : Location
  )
  RecordUpdate(
    type_name~ : TypeName?,
    record~ : Expr,
    fields~ : @list.T[FieldDef],
    loc~ : Location
  )
  Field(record~ : Expr, accessor~ : Accessor, loc~ : Location)
  Method(type_name~ : TypeName, method_name~ : Label, loc~ : Location)
  DotApply(
    self~ : Expr,
    method_name~ : Label,
    args~ : @list.T[Argument],
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
    cases~ : @list.T[Case],
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
  Continue(args~ : @list.T[Expr], label~ : Label?, loc~ : Location)
  Loop(
    args~ : @list.T[Expr],
    body~ : @list.T[MultiArgCase],
    label~ : Label?,
    loop_loc~ : Location,
    loc~ : Location
  )
  For(
    binders~ : @list.T[(Binder, Expr)],
    condition~ : Expr?,
    continue_block~ : @list.T[(Binder, Expr)],
    body~ : Expr,
    for_else~ : Expr?,
    label~ : Label?,
    loc~ : Location
  )
  ForEach(
    binders~ : @list.T[Binder?],
    expr~ : Expr,
    body~ : Expr,
    else_block~ : Expr?,
    label~ : Label?,
    loc~ : Location
  )
  Try(
    body~ : Expr,
    catch_~ : @list.T[Case],
    catch_all~ : Bool,
    try_else~ : @list.T[Case]?,
    has_try~ : Bool,
    legacy_else~ : Bool,
    try_loc~ : Location,
    catch_loc~ : Location,
    else_loc~ : Location,
    loc~ : Location
  )
  TryQuestion(body~ : Expr, try_loc~ : Location, loc~ : Location)
  Map(elems~ : @list.T[MapExprElem], loc~ : Location)
  Group(expr~ : Expr, group~ : Group, loc~ : Location)
  StaticAssert(asserts~ : @list.T[StaticAssertion], body~ : Expr)
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
  Closed(@list.T[ArrayPattern])
  Open(@list.T[ArrayPattern], @list.T[ArrayPattern], DotDotBinder)
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
    args~ : @list.T[ConstrPatArg]?,
    is_open~ : Bool,
    loc~ : Location
  )
  Or(pat1~ : Pattern, pat2~ : Pattern, loc~ : Location)
  Tuple(pats~ : @list.T[Pattern], loc~ : Location)
  Var(Binder)
  Record(fields~ : @list.T[FieldPat], is_closed~ : Bool, loc~ : Location)
  Map(elems~ : @list.T[MapPatElem], is_closed~ : Bool, loc~ : Location)
  Range(lhs~ : Pattern, rhs~ : Pattern, inclusive~ : Bool, loc~ : Location)
}

///|
pub typealias (StringLiteral, Location)? as TestName

///|
pub(all) struct LocalTypeDecl {
  tycon : String
  tycon_loc : Location
  components : TypeDesc
  deriving : @list.T[DerivingDirective]
}

///|
// checked!
pub(all) struct DerivingDirective {
  type_name : TypeName
  args : @list.T[Argument]
  loc : Location
}

///|
pub(all) struct TypeDecl {
  tycon : String
  tycon_loc : Location
  params : @list.T[TypeDeclBinder]
  components : TypeDesc
  attrs : @list.T[Attribute]
  mut doc : DocString
  type_vis : Visibility
  deriving : @list.T[DerivingDirective]
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
  CodeMultilineString(@list.T[String])
}

///|
pub(all) enum DeclBody {
  DeclBody(local_types~ : @list.T[LocalTypeDecl], expr~ : Expr)
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
  quantifiers : @list.T[TypeVarBinder]
  return_type : Type?
  error_type : ErrorType
  vis : Visibility
  attrs : @list.T[Attribute]
  mut doc : DocString
}

///|
pub(all) struct TraitMethodDecl {
  name : Binder
  has_error : Location?
  is_async : Bool
  quantifiers : @list.T[TypeVarBinder]
  params : Parameters
  return_type : Type?
  error_type : ErrorType
  has_default : Location?
  loc : Location
}

///|
pub(all) struct TraitDecl {
  name : Binder
  supers : @list.T[TypeVarConstraint]
  methods : @list.T[TraitMethodDecl]
  vis : Visibility
  loc : Location
  attrs : @list.T[Attribute]
  mut doc : DocString
}

///|
pub(all) enum Impl {
  TopExpr(
    expr~ : Expr,
    is_main~ : Bool,
    local_types~ : @list.T[LocalTypeDecl],
    loc~ : Location
  )
  TopTest(
    expr~ : Expr,
    name~ : TestName,
    params~ : Parameters?,
    local_types~ : @list.T[LocalTypeDecl],
    loc~ : Location,
    attrs~ : @list.T[Attribute],
    mut doc~ : DocString
  )
  TopTypeDef(TypeDecl)
  TopFuncDef(fun_decl~ : FunDecl, decl_body~ : DeclBody, loc~ : Location)
  TopFuncAlias(
    pkg~ : Label?,
    type_name~ : TypeName?,
    targets~ : @list.T[AliasTarget],
    vis~ : Visibility,
    attrs~ : @list.T[Attribute],
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
    attrs~ : @list.T[Attribute],
    mut doc~ : DocString
  )
  TopTrait(TraitDecl)
  TopBatchTypeAlias(
    pkg~ : Label?,
    targets~ : @list.T[AliasTarget],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.T[Attribute],
    is_list~ : Bool,
    mut doc~ : DocString
  )
  TopBatchTraitAlias(
    pkg~ : Label?,
    targets~ : @list.T[AliasTarget],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.T[Attribute],
    is_list~ : Bool,
    mut doc~ : DocString
  )
  TopImpl(
    self_ty~ : Type?,
    trait_~ : TypeName,
    method_name~ : Binder,
    has_error~ : Location?,
    quantifiers~ : @list.T[TypeVarBinder],
    params~ : Parameters,
    ret_ty~ : Type?,
    err_ty~ : ErrorType,
    body~ : DeclBody,
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.T[Attribute],
    mut doc~ : DocString
  )
  TopView(
    quantifiers~ : @list.T[TypeVarBinder],
    source_ty~ : Type,
    view_constrs~ : @list.T[ConstrDecl],
    body~ : @list.T[Case],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @list.T[Attribute],
    mut doc~ : DocString
  )
  TopImplRelation(
    self_ty~ : Type,
    trait_~ : TypeName,
    quantifiers~ : @list.T[TypeVarBinder],
    vis~ : Visibility,
    attrs~ : @list.T[Attribute],
    loc~ : Location,
    mut doc~ : DocString
  )
}

///|
pub typealias @list.T[Impl] as Impls
