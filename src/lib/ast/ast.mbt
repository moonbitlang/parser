///|
pub(all) enum Visibility {
  Default
  Pub(attr~ : String?, loc~ : Location)
  Priv(loc~ : Location)
} derive(ToJson)

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
} derive(ToJson)

///|
pub(all) struct Name {
  name : String
  loc : Location
} derive(ToJson)

///|
pub(all) struct QualifiedName {
  pkg : String?
  name : String
  loc : Location
} derive(ToJson)

///|
pub(all) enum Type {
  TAny(Location)
  TArrow(
    @immut/list.T[Type],
    Type,
    err~ : ErrorType,
    is_async~ : Bool,
    loc~ : Location
  )
  TTuple(@immut/list.T[Type], loc~ : Location)
  TName(QualifiedName, @immut/list.T[Type], loc~ : Location)
  TOption(Type, loc~ : Location, question_loc~ : Location)
  TObject(QualifiedName)
} derive(ToJson)

///|
pub(all) enum ErrorType {
  ErrorType(Type)
  DefaultErrorType(Location)
  NoErrorType
  MaybeError(Type)
} derive(ToJson)

///|
pub(all) struct ConstrParam {
  type_ : Type
  mut_ : Bool
  label : Name?
} derive(ToJson)

///|
pub(all) struct ConstrDecl {
  name : Name
  args : @immut/list.T[ConstrParam]?
  tag : (String, Location)?
  loc : Location
} derive(ToJson)

///|
pub(all) enum ExceptionDecl {
  NoPayload
  SinglePayload(Type)
  EnumPayload(@immut/list.T[ConstrDecl])
} derive(ToJson)

///|
pub(all) struct FieldDecl {
  name : Name
  type_ : Type
  mut_ : Bool
  vis : Visibility
  loc : Location
} derive(ToJson)

///|
pub(all) enum TypeDesc {
  Abstract
  Extern
  Newtype(Type)
  Error(ExceptionDecl)
  Variant(@immut/list.T[ConstrDecl])
  Record(@immut/list.T[FieldDecl])
  Alias(Type)
} derive(ToJson)

///|
pub(all) enum Hole {
  Synthesized
  Incomplete
  Todo
}

///|
pub(all) enum ArgumentKind {
  Positional
  Labelled(Name)
  LabelledPun(Name)
  LabelledOption(label~ : Name, question_loc~ : Location)
  LabelledOptionPun(label~ : Name, question_loc~ : Location)
}

///|
pub(all) enum FnKind {
  Lambda
  Matrix
  Arrow
}

///|
pub(all) enum Group {
  GroupBrace
  GroupParen
}

///|
pub(all) enum TrailingMark {
  TrailingComma
  TrailingSemi
  TrailingNone
}

///|
pub(all) enum ApplyAttr {
  NoAttr
  Exclamation
  Question
}

///|
pub(all) struct TypeVarConstraint {
  trait_ : QualifiedName
  loc : Location
}

///|
pub(all) struct TypeVarBinder {
  name : String
  constraints : @immut/list.T[TypeVarConstraint]
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
  name : QualifiedName
  loc : Location
}

///|
pub(all) enum ConstructorExtraInfo {
  TypeName(QualifiedName)
  Package(String)
  NoExtraInfo
}

///|
pub(all) struct Constructor {
  name : Name
  extra_info : ConstructorExtraInfo
  loc : Location
}

///|
pub(all) enum Accessor {
  Label(Name)
  Index(tuple_index~ : Int, loc~ : Location)
  Newtype(loc~ : Location)
}

///|
pub(all) struct AliasTarget {
  binder : Binder
  target : Name?
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
pub typealias @immut/list.T[Parameter] as Parameters

///|
pub(all) struct Case {
  pattern : Pattern
  guard_ : Expr?
  body : Expr
}

///|
pub(all) struct MultiArgCase {
  patterns : @immut/list.T[Pattern]
  guard_ : Expr?
  body : Expr
}

///|
pub(all) enum SpreadableElem {
  ElemRegular(Expr)
  ElemSpread(expr~ : Expr, loc~ : Location)
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
  assert_type : Type
  assert_trait : QualifiedName
  assert_loc : Location
  assert_msg : String
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
    cases~ : @immut/list.T[MultiArgCase],
    has_error~ : Location?,
    is_async~ : Bool,
    fn_loc~ : Location,
    loc~ : Location
  )
}

///|
pub(all) struct FieldDef {
  label : Name
  expr : Expr
  is_pun : Bool
  loc : Location
}

///|
pub(all) enum InterpElem {
  InterpLit(repr~ : StringLiteral, loc~ : Location)
  InterpExpr(expr~ : Expr, loc~ : Location)
  InterpSource(@ast_types.InterpSource)
}

///|
pub(all) enum MultilineStringElem {
  MultilineString(String)
  MultilineInterp(@immut/list.T[InterpElem])
}

///|
pub(all) enum Expr {
  Apply(
    func~ : Expr,
    args~ : @immut/list.T[Argument],
    attr~ : ApplyAttr,
    loc~ : Location
  )
  Infix(op~ : Var, lhs~ : Expr, rhs~ : Expr, loc~ : Location)
  Unary(op~ : Var, expr~ : Expr, loc~ : Location)
  Array_(exprs~ : @immut/list.T[Expr], loc~ : Location)
  ArraySpread(elems~ : @immut/list.T[SpreadableElem], loc~ : Location)
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
  MultilineString(elems~ : @immut/list.T[MultilineStringElem], loc~ : Location)
  Interp(elems~ : @immut/list.T[InterpElem], loc~ : Location)
  Constraint(expr~ : Expr, ty~ : Type, loc~ : Location)
  Constr(constr~ : Constructor, loc~ : Location)
  While(
    loop_cond~ : Expr,
    loop_body~ : Expr,
    while_else~ : Expr?,
    label~ : Name?,
    loc~ : Location
  )
  Function_(func~ : Func, loc~ : Location)
  Ident(id~ : Var, loc~ : Location)
  If(cond~ : Expr, ifso~ : Expr, ifnot~ : Expr?, loc~ : Location)
  Guard(cond~ : Expr, otherwise~ : Expr?, body~ : Expr, loc~ : Location)
  Is(expr~ : Expr, pat~ : Pattern, loc~ : Location)
  LetFn(name~ : Binder, func~ : Func, body~ : Expr, loc~ : Location)
  LetRec(bindings~ : @immut/list.T[(Binder, Func)], body~ : Expr, loc~ : Location)
  LetAnd(
    bindings~ : @immut/list.T[(Binder, Type?, Func)],
    body~ : Expr,
    loc~ : Location
  )
  Let(pattern~ : Pattern, expr~ : Expr, body~ : Expr, loc~ : Location)
  Sequence(
    exprs~ : @immut/list.T[Expr],
    last_expr~ : Expr,
    loc~ : Location
  )
  Tuple(exprs~ : @immut/list.T[Expr], loc~ : Location)
  Record(
    type_name~ : QualifiedName?,
    fields~ : @immut/list.T[FieldDef],
    trailing~ : TrailingMark,
    loc~ : Location
  )
  RecordUpdate(
    type_name~ : QualifiedName?,
    record~ : Expr,
    fields~ : @immut/list.T[FieldDef],
    loc~ : Location
  )
  Field(record~ : Expr, accessor~ : Accessor, loc~ : Location)
  Method(type_name~ : QualifiedName, method_name~ : Name, loc~ : Location)
  DotApply(
    self~ : Expr,
    method_name~ : Name,
    args~ : @immut/list.T[Argument],
    return_self~ : Bool,
    attr~ : ApplyAttr,
    loc~ : Location
  )
  As(expr~ : Expr, trait_~ : QualifiedName, loc~ : Location)
  Mutate(
    record~ : Expr,
    accessor~ : Accessor,
    field~ : Expr,
    augmented_by~ : Var?,
    loc~ : Location
  )
  Match(
    expr~ : Expr,
    cases~ : @immut/list.T[Case],
    match_loc~ : Location,
    using_~ : Name?,
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
  Hole_(loc~ : Location, kind~ : Hole)
  Return(return_value~ : Expr?, loc~ : Location)
  Raise(err_value~ : Expr, loc~ : Location)
  Unit_(loc~ : Location, faked~ : Bool)
  Break(arg~ : Expr?, label~ : Name?, loc~ : Location)
  Continue(args~ : @immut/list.T[Expr], label~ : Name?, loc~ : Location)
  Loop(
    args~ : @immut/list.T[Expr],
    body~ : @immut/list.T[MultiArgCase],
    label~ : Name?,
    loop_loc~ : Location,
    loc~ : Location
  )
  For(
    binders~ : @immut/list.T[(Binder, Expr)],
    condition~ : Expr?,
    continue_block~ : @immut/list.T[(Binder, Expr)],
    body~ : Expr,
    for_else~ : Expr?,
    label~ : Name?,
    loc~ : Location
  )
  ForEach(
    binders~ : @immut/list.T[Binder?],
    expr~ : Expr,
    body~ : Expr,
    else_block~ : Expr?,
    label~ : Name?,
    loc~ : Location
  )
  Try(
    body~ : Expr,
    catch_~ : @immut/list.T[Case],
    catch_all~ : Bool,
    try_else~ : @immut/list.T[Case]?,
    has_try~ : Bool,
    legacy_else~ : Bool,
    try_loc~ : Location,
    catch_loc~ : Location,
    else_loc~ : Location,
    loc~ : Location
  )
  TryQuestion(body~ : Expr, try_loc~ : Location, loc~ : Location)
  Map(elems~ : @immut/list.T[MapExprElem], loc~ : Location)
  Group(expr~ : Expr, group~ : Group, loc~ : Location)
  StaticAssert(asserts~ : @immut/list.T[StaticAssertion], body~ : Expr)
}

///|
pub(all) enum DotDotBinder {
  Underscore
  NoBinder
  BinderAs(Binder)
  Binder_(Binder)
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
  Closed(@immut/list.T[ArrayPattern])
  Open(
    @immut/list.T[ArrayPattern],
    @immut/list.T[ArrayPattern],
    DotDotBinder
  )
}

///|
pub(all) struct FieldPat {
  label : Name
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
pub(all) enum Pattern {
  Alias(pat~ : Pattern, alias_~ : Binder, loc~ : Location)
  Any(loc~ : Location)
  Array_(pats~ : ArrayPatterns, loc~ : Location)
  Constant(c~ : Constant, loc~ : Location)
  Constraint(pat~ : Pattern, ty~ : Type, loc~ : Location)
  Constr(
    constr~ : Constructor,
    args~ : @immut/list.T[ConstrPatArg]?,
    is_open~ : Bool,
    loc~ : Location
  )
  Or(pat1~ : Pattern, pat2~ : Pattern, loc~ : Location)
  Tuple(pats~ : @immut/list.T[Pattern], loc~ : Location)
  Var(Binder)
  Record(
    fields~ : @immut/list.T[FieldPat],
    is_closed~ : Bool,
    loc~ : Location
  )
  Map(
    elems~ : @immut/list.T[MapPatElem],
    is_closed~ : Bool,
    loc~ : Location
  )
  Range(
    lhs~ : Pattern,
    rhs~ : Pattern,
    inclusive~ : Bool,
    loc~ : Location
  )
}

///|
pub typealias StringLiteral? as TestName

///|
pub(all) struct LocalTypeDecl {
  tycon : String
  tycon_loc : Location
  components : TypeDesc
  deriving : @immut/list.T[DerivingDirective]
}

///|
pub(all) struct DerivingDirective {
  type_name : QualifiedName
  args : @immut/list.T[Argument]
  loc : Location
}

///|
pub(all) struct TypeDecl {
  tycon : String
  tycon_loc : Location
  params : @immut/list.T[TypeDeclBinder]
  components : TypeDesc
  attrs : @immut/list.T[String] // Simplified attributes
  type_vis : Visibility
  deriving : @immut/list.T[DerivingDirective]
  loc : Location
  deprecated_type_bang : Bool
}

///|
pub(all) enum FuncStubs {
  Import(module_name~ : StringLiteral, func_name~ : StringLiteral)
  Embedded(language~ : StringLiteral?, code~ : EmbeddedCode)
}

///|
pub(all) enum EmbeddedCode {
  CodeString(StringLiteral)
  CodeMultilineString(@immut/list.T[String])
}

///|
pub(all) enum DeclBody {
  DeclBody(local_types~ : @immut/list.T[LocalTypeDecl], expr~ : Expr)
  DeclStubs(FuncStubs)
}

///|
pub(all) struct FunDecl {
  type_name : QualifiedName?
  name : Binder
  has_error : Location?
  is_async : Bool
  decl_params : Parameters?
  params_loc : Location
  quantifiers : @immut/list.T[TypeVarBinder]
  return_type : Type?
  error_type : ErrorType
  vis : Visibility
  attrs : @immut/list.T[String] // Simplified attributes
}

///|
pub(all) struct TraitMethodDecl {
  name : Binder
  has_error : Location?
  is_async : Bool
  quantifiers : @immut/list.T[TypeVarBinder]
  params : Parameters
  return_type : Type?
  error_type : ErrorType
  has_default : Location?
  loc : Location
}

///|
pub(all) struct TraitDecl {
  trait_name : Binder
  trait_supers : @immut/list.T[TypeVarConstraint]
  trait_methods : @immut/list.T[TraitMethodDecl]
  trait_vis : Visibility
  trait_loc : Location
  trait_attrs : @immut/list.T[String] // Simplified attributes
}

///|
pub(all) enum Impl {
  TopExpr(
    expr~ : Expr,
    is_main~ : Bool,
    local_types~ : @immut/list.T[LocalTypeDecl],
    loc~ : Location
  )
  TopTest(
    expr~ : Expr,
    name~ : TestName,
    params~ : Parameters?,
    local_types~ : @immut/list.T[LocalTypeDecl],
    loc~ : Location,
    attrs~ : @immut/list.T[String]
  )
  TopTypeDef(TypeDecl)
  TopFuncDef(fun_decl~ : FunDecl, decl_body~ : DeclBody, loc~ : Location)
  TopFuncAlias(
    pkg~ : Name?,
    type_name~ : Name?,
    targets~ : @immut/list.T[AliasTarget],
    vis~ : Visibility,
    attrs~ : @immut/list.T[String],
    is_list~ : Bool,
    loc~ : Location
  )
  TopLetDef(
    binder~ : Binder,
    ty~ : Type?,
    expr~ : Expr,
    vis~ : Visibility,
    is_constant~ : Bool,
    loc~ : Location,
    attrs~ : @immut/list.T[String]
  )
  TopTrait(TraitDecl)
  TopLegacyTypeAlias(
    binder~ : Type,
    target~ : Type,
    vis~ : Visibility,
    deriving~ : @immut/list.T[DerivingDirective],
    loc~ : Location,
    attrs~ : @immut/list.T[String]
  )
  TopBatchTypeAlias(
    pkg~ : Name?,
    targets~ : @immut/list.T[AliasTarget],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @immut/list.T[String],
    is_list~ : Bool
  )
  TopBatchTraitAlias(
    pkg~ : Name?,
    targets~ : @immut/list.T[AliasTarget],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @immut/list.T[String],
    is_list~ : Bool,
    is_old_syntax~ : Bool
  )
  TopImpl(
    self_ty~ : Type?,
    trait_~ : QualifiedName,
    method_name~ : Binder,
    has_error~ : Location?,
    quantifiers~ : @immut/list.T[TypeVarBinder],
    params~ : Parameters,
    ret_ty~ : Type?,
    err_ty~ : ErrorType,
    body~ : DeclBody,
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @immut/list.T[String]
  )
  TopView(
    quantifiers~ : @immut/list.T[TypeVarBinder],
    source_ty~ : Type,
    view_constrs~ : @immut/list.T[ConstrDecl],
    body~ : @immut/list.T[Case],
    vis~ : Visibility,
    loc~ : Location,
    attrs~ : @immut/list.T[String]
  )
  TopImplRelation(
    self_ty~ : Type,
    trait_~ : QualifiedName,
    quantifiers~ : @immut/list.T[TypeVarBinder],
    vis~ : Visibility,
    attrs~ : @immut/list.T[String],
    loc~ : Location
  )
}

///|
pub typealias @immut/list.T[Impl] as Impls