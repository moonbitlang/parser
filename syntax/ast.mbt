///|
pub(all) enum RangeKind {
  Inclusive
  Exclusive
  InclusiveMissingEqual
} derive(Debug)

///|
pub(all) enum Visibility {
  Default
  Pub(attr~ : String?, loc~ : Location)
  Priv(loc~ : Location)
} derive(Debug)

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
  Regex(StringLiteral)
  BigInt(String)
} derive(Debug)

///|
pub(all) struct Label {
  name : String
  loc : Location
} derive(Debug)

///|
pub(all) struct ConstrName {
  name : String
  loc : Location
} derive(Debug)

///|
// checked!
pub(all) enum LongIdent {
  Ident(name~ : String)
  Dot(pkg~ : String, id~ : String)
} derive(Debug)

///|
/// The dummy LongIdent is used in typer to represent an parsing error.
pub fn LongIdent::dummy() -> LongIdent {
  Ident(name="")
}

///|
pub(all) struct TypeName {
  name : LongIdent
  is_object : Bool
  loc : Location
} derive(Debug)

///|
pub(all) struct ConstrId {
  id : LongIdent
  loc : Location
} derive(Debug)

///|
pub(all) enum Type {
  Any(loc~ : Location)
  Arrow(
    args~ : @list.List[Type],
    res~ : Type,
    err~ : ErrorType,
    is_async~ : Location?,
    loc~ : Location
  )
  Tuple(tys~ : @list.List[Type], loc~ : Location)
  Name(constr_id~ : ConstrId, tys~ : @list.List[Type], loc~ : Location)
  Option(ty~ : Type, loc~ : Location, question_loc~ : Location)
  Object(ConstrId)
} derive(Debug)

///|
pub(all) enum ErrorType {
  ErrorType(ty~ : Type)
  DefaultErrorType(loc~ : Location)
  NoErrorType
  Noraise(loc~ : Location)
  MaybeError(ty~ : Type)
} derive(Debug)

///|
pub(all) struct ConstrParam {
  ty : Type
  mut_ : Bool
  label : Label?
} derive(Debug)

///|
pub(all) struct ConstrDecl {
  name : ConstrName
  args : @list.List[ConstrParam]?
  tag : (String, Location)?
  loc : Location
  attrs : @list.List[Attribute]
  mut doc : DocString
} derive(Debug)

///|
pub(all) enum ExceptionDecl {
  NoPayload
  EnumPayload(@list.List[ConstrDecl])
} derive(Debug)

///|
pub(all) struct FieldName {
  label : String
  loc : Location
} derive(Debug)

///|
pub(all) struct FieldDecl {
  name : FieldName
  ty : Type
  mut_ : Bool
  vis : Visibility
  loc : Location
  attrs : @list.List[Attribute]
  mut doc : DocString
} derive(Debug)

///|
pub(all) enum TypeDesc {
  /// `type T`
  Abstract
  /// `extenum T { C1; C2 }`
  ExtensibleEnum(@list.List[ConstrDecl])
  /// `suberror`
  Error(ExceptionDecl)
  /// `enum T { C1; C2 }`
  Variant(@list.List[ConstrDecl])
  /// `extenum T += { C1; C2 }`
  ExtendEnum(target~ : TypeName, constructors~ : @list.List[ConstrDecl])
  /// `struct T { field1: T1; field2: T2 }`
  Record(fields~ : @list.List[FieldDecl])
  /// `struct T (T1,T2)`
  TupleStruct(@list.List[Type])
  Alias(Type)
} derive(Debug)

///|
pub(all) enum Hole {
  Synthesized
  Incomplete
  Todo
} derive(Debug)

///|
pub(all) enum ArgumentKind {
  Positional
  Labelled(Label)
  LabelledPun(Label)
  LabelledOption(label~ : Label, question_loc~ : Location)
  LabelledOptionPun(label~ : Label, question_loc~ : Location)
} derive(Debug)

///|
pub(all) enum FnKind {
  Lambda
  Arrow
} derive(Debug)

///|
pub(all) enum Group {
  Brace
  Paren
} derive(Debug)

///|
pub(all) enum TrailingMark {
  Comma
  Semi
  None
} derive(Debug)

///|
pub(all) struct TypeVarConstraint {
  trait_ : LongIdent
  loc : Location
} derive(Debug)

///|
/// Represent type variable and optional constraints.
/// 
/// ```skip
/// fn [T] f() -> Unit {...}
///  // ^---- TypeVarBinder
/// fn [T : TraitA + TraitB] f() -> Unit {...}
///  // ^^^^^^^^^^^^^^^^^^^----- TypeVarBinder
/// ```
pub(all) struct TypeVarBinder {
  /// name of type variable
  name : String
  /// location of type variable
  name_loc : Location
  /// type constraints for type variable, e.g. `Show + Compare`
  constraints : @list.List[TypeVarConstraint]
} derive(Debug)

///|
pub(all) struct TypeDeclBinder {
  name : String?
  loc : Location
} derive(Debug)

///|
pub(all) struct Binder {
  name : String
  loc : Location
} derive(Debug)

///|
pub(all) struct Var {
  name : LongIdent
  loc : Location
} derive(Debug)

///|
pub(all) enum ConstructorExtraInfo {
  TypeName(TypeName)
  TypeNameWithConstrPackage(type_name~ : TypeName, pkg~ : String)
  Package(String)
  NoExtraInfo
} derive(Debug)

///|
pub(all) struct Constructor {
  name : ConstrName
  extra_info : ConstructorExtraInfo
  loc : Location
} derive(Debug)

///|
pub(all) enum Accessor {
  Label(Label)
  Index(tuple_index~ : Int, loc~ : Location)
  Newtype(loc~ : Location)
} derive(Debug)

///|
pub(all) struct AliasTarget {
  binder : Binder
  target : Label?
} derive(Debug)

///|
pub(all) struct Argument {
  value : Expr
  kind : ArgumentKind
} derive(Debug)

///|
pub(all) enum Parameter {
  /// `_ : Int`
  DiscardPositional(ty~ : Type?, loc~ : Location)
  /// `param : Int`
  Positional(binder~ : Binder, ty~ : Type?)
  /// `label~ : Int`
  Labelled(binder~ : Binder, ty~ : Type?)
  /// `label? : Int = default`
  Optional(binder~ : Binder, default~ : Expr, ty~ : Type?)
  /// `label? : Int`
  QuestionOptional(binder~ : Binder, ty~ : Type?)
} derive(Debug)

///|
pub type Parameters = @list.List[Parameter]

///|
pub(all) struct Case {
  pattern : Pattern
  guard_ : Expr?
  body : Expr
} derive(Debug)

///|
pub(all) enum SpreadableElem {
  /// `expr`
  Regular(Expr)
  /// `..expr`
  Spread(expr~ : Expr, loc~ : Location)
} derive(Debug)

///|
pub(all) struct MapExprElem {
  key : Constant
  expr : Expr
  key_loc : Location
  loc : Location
} derive(Debug)

///|
pub(all) struct StaticAssertion {
  ty : Type
  trait_ : LongIdent
  loc : Location
  msg : String
} derive(Debug)

///|
pub(all) struct Func {
  parameters : Parameters
  params_loc : Location
  body : Expr
  return_type : Type?
  error_type : ErrorType
  kind : FnKind
  is_async : Location?
  loc : Location
} derive(Debug)

///|
pub(all) struct FieldDef {
  label : Label
  expr : Expr
  is_pun : Bool
  loc : Location
} derive(Debug)

///|
pub(all) enum InterpElem {
  Literal(repr~ : StringLiteral, loc~ : Location)
  Expr(expr~ : Expr, loc~ : Location)
  Source(@tokens.InterpSource)
} derive(Debug)

///|
pub(all) enum MultilineStringElem {
  String(String)
  Interp(@list.List[InterpElem])
} derive(Debug)

///|
pub(all) enum LexPattern {
  Regex(lit~ : String, offset~ : Int, loc~ : Location)
  RegexInterp(elems~ : @list.List[InterpElem], loc~ : Location)
  ConstantRef(lid~ : LongIdent, loc~ : Location)
  Alias(pat~ : LexPattern, binder~ : Binder, loc~ : Location)
  Sequence(pats~ : @list.List[LexPattern], loc~ : Location)
} derive(Debug)

///|
pub(all) enum LexTopPattern {
  Pattern(LexPattern)
  Binder(Binder)
  Wildcard(loc~ : Location)
} derive(Debug)

///|
pub(all) struct LexCase {
  pat : @list.List[LexTopPattern]
  pat_loc : Location
  guard_ : Expr?
  body : Expr
} derive(Debug)

///|
pub(all) struct LexScanCase {
  pat : LexScanCasePattern
  guard_ : Expr?
  body : Expr
} derive(Debug)

///|
pub(all) enum RegexPattern {
  Literal(lit~ : String, loc~ : Location)
  Reference(lid~ : LongIdent, loc~ : Location)
  Sequence(pat1~ : RegexPattern, pat2~ : RegexPattern, loc~ : Location)
  Alternation(pat1~ : RegexPattern, pat2~ : RegexPattern, loc~ : Location)
  Alias(pat~ : RegexPattern, binder~ : Binder, loc~ : Location)
} derive(Debug)

///|
pub(all) enum LexScanCasePattern {
  Pattern(RegexPattern)
  Binder(Binder)
  Wildcard(loc~ : Location)
} derive(Debug)

///|
pub(all) enum PredicateQuantifierKind {
  Forall
  Exists
} derive(Debug)

///|
pub(all) enum ListComprehensionKind {
  Foreach(
    binders~ : @list.List[Binder?],
    expr~ : Expr,
    init~ : @list.List[(Binder, Expr)],
    continue_block~ : @list.List[(Binder, Expr)]
  )
  For(
    binders~ : @list.List[(Binder, Expr)],
    condition~ : Expr?,
    continue_block~ : @list.List[(Binder, Expr)],
    for_loc~ : Location
  )
} derive(Debug)

///|
pub(all) enum Expr {
  /// f(args)
  Apply(func~ : Expr, args~ : @list.List[Argument], loc~ : Location)
  /// e1 + e2
  Infix(op~ : Var, lhs~ : Expr, rhs~ : Expr, loc~ : Location)
  /// -e
  Unary(op~ : Var, expr~ : Expr, loc~ : Location)
  /// [e1, e2, e3]
  Array(exprs~ : @list.List[Expr], is_iter~ : Bool, loc~ : Location)
  /// [..arr1, e1, ..arr2, e2]
  ArraySpread(
    elems~ : @list.List[SpreadableElem],
    is_iter~ : Bool,
    loc~ : Location
  )
  /// `[for x in xs if guard => body]`
  ListComprehension(
    kind~ : ListComprehensionKind,
    guard_~ : Expr?,
    body~ : Expr,
    is_iter~ : Bool,
    loc~ : Location
  )
  /// e1[e2]
  ArrayGet(array~ : Expr, index~ : Expr, loc~ : Location)
  /// e1[e2:e3]
  ArrayGetSlice(
    array~ : Expr,
    start_index~ : Expr?,
    end_index~ : Expr?,
    index_loc~ : Location,
    loc~ : Location
  )
  /// e1[i] = e2
  ArraySet(array~ : Expr, index~ : Expr, value~ : Expr, loc~ : Location)
  /// e1[i] += e2
  ArrayAugmentedSet(
    op~ : Var,
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    loc~ : Location
  )
  Constant(c~ : Constant, loc~ : Location)
  /// ```skip
  /// #| multiline
  /// #| string
  /// ```
  MultilineString(elems~ : @list.List[MultilineStringElem], loc~ : Location)
  /// `"text \{e1}"`
  Interp(elems~ : @list.List[InterpElem], loc~ : Location)
  /// `b"text \{e1}"`
  BytesInterp(elems~ : @list.List[InterpElem], loc~ : Location)
  /// `(expr : Type)`
  Constraint(expr~ : Expr, ty~ : Type, loc~ : Location)
  /// `Constructor(e1, e2, e3)`
  Constr(constr~ : Constructor, loc~ : Location)
  /// `while e1 { e2 } else { e3 }`
  While(
    loop_cond~ : Expr,
    loop_body~ : Expr,
    while_else~ : Expr?,
    label~ : Label?,
    loc~ : Location
  )
  /// `fn(args){ expr }` or `(args) => expr`
  Function(func~ : Func, loc~ : Location)
  /// `id`
  Ident(id~ : Var, loc~ : Location)
  /// `if e1 { e2 } else { e3 }`
  If(cond~ : Expr, ifso~ : Expr, ifnot~ : Expr?, loc~ : Location)
  /// `guard e1 else {e2}; e3`
  Guard(cond~ : Expr, otherwise~ : Expr?, body~ : Expr, loc~ : Location)
  /// `expr is pattern`
  Is(expr~ : Expr, pat~ : Pattern, loc~ : Location)
  RegexMatch(
    expr~ : Expr,
    pat~ : RegexPattern,
    bindings~ : @list.List[(Label, Binder?)],
    loc~ : Location
  )
  /// `defer e1; e2`
  Defer(expr~ : Expr, body~ : Expr, loc~ : Location)
  /// `fn local(args){ e1 }; e2`
  LetFn(name~ : Binder, func~ : Func, body~ : Expr, loc~ : Location)
  LetAnd(
    bindings~ : @list.List[(Binder, Type?, Func)],
    body~ : Expr,
    loc~ : Location
  )
  /// `let pattern = e1; e2`
  Let(pattern~ : Pattern, expr~ : Expr, body~ : Expr, loc~ : Location)
  /// `e1; e2`
  Sequence(exprs~ : @list.List[Expr], last_expr~ : Expr, loc~ : Location)
  /// `(e1, e2, e3)`
  Tuple(exprs~ : @list.List[Expr], loc~ : Location)
  /// `TypeName::{ field1: e1, field2: e2 }`
  /// The `trailing` field is used to determine whether the source has a trailing separator.
  /// If the trailing separator is SEMI, the AST is invalid.
  Record(
    type_name~ : TypeName?,
    fields~ : @list.List[FieldDef],
    trailing~ : TrailingMark,
    loc~ : Location
  )
  /// `{ ..record, field1: expr, field2: expr }`
  RecordUpdate(
    type_name~ : TypeName?,
    record~ : Expr,
    fields~ : @list.List[FieldDef],
    loc~ : Location
  )
  /// `record.field`
  Field(record~ : Expr, accessor~ : Accessor, loc~ : Location)
  /// `TypeName::method`
  Method(type_name~ : TypeName, method_name~ : Label, loc~ : Location)
  /// `self.method(args)`
  DotApply(
    self~ : Expr,
    method_name~ : Label,
    args~ : @list.List[Argument],
    return_self~ : Bool,
    loc~ : Location
  )
  /// `expr as Trait`
  As(expr~ : Expr, trait_~ : TypeName, loc~ : Location)
  /// `record.field = expr`
  Mutate(
    record~ : Expr,
    accessor~ : Accessor,
    field~ : Expr,
    augmented_by~ : Var?,
    loc~ : Location
  )
  /// `match e0 { p1 => e1; p2 => e2 }`
  Match(
    expr~ : Expr,
    cases~ : @list.List[Case],
    match_loc~ : Location,
    loc~ : Location
  )
  /// `lexmatch using strategy { c1 => e1; c2 => e2 }`
  LexMatch(
    strategy~ : Label?,
    expr~ : Expr,
    match_loc~ : Location,
    cases~ : @list.List[LexCase],
    loc~ : Location
  )
  LexScan(
    expr~ : Expr,
    match_loc~ : Location,
    cases~ : @list.List[LexScanCase],
    loc~ : Location
  )
  /// `let mut binder : Type = expr`
  LetMut(
    binder~ : Binder,
    ty~ : Type?,
    expr~ : Expr,
    body~ : Expr,
    loc~ : Location
  )
  /// `lhs |> rhs`
  /// The rhs is restricted to `f`, `f(args)`, `_.method(args)`, `Constructor`, or `Constructor(args)`.
  Pipe(lhs~ : Expr, rhs~ : Expr, loc~ : Location)
  /// `lhs <| rhs`
  RevPipe(lhs~ : Expr, rhs~ : Expr, loc~ : Location)
  /// `n = e` or `n += e`
  Assign(var_~ : Var, expr~ : Expr, augmented_by~ : Var?, loc~ : Location)
  /// `_`
  /// This expression is only allowed in partial application.
  Hole(loc~ : Location, kind~ : Hole)
  /// `return expr`
  Return(return_value~ : Expr?, loc~ : Location)
  /// `raise expr`
  Raise(err_value~ : Expr, loc~ : Location)
  Quantifier(
    kind~ : PredicateQuantifierKind,
    binder~ : Binder,
    binder_ty~ : Type,
    body~ : Expr,
    loc~ : Location
  )
  Implies(lhs~ : Expr, rhs~ : Expr, loc~ : Location)
  ProofAssert(expr~ : Expr, loc~ : Location)
  ProofLet(binder~ : Binder, expr~ : Expr, loc~ : Location)
  /// `()`
  /// If `faked` is true, it means it was inserted by the parser.
  Unit(loc~ : Location, faked~ : Bool)
  /// `break expr`
  Break(arg~ : Expr?, label~ : Label?, loc~ : Location)
  /// `continue e1, e2, e3`
  Continue(args~ : @list.List[Expr], label~ : Label?, loc~ : Location)
  /// `for binders; condition; continue_block { body } else { expr }`
  For(
    binders~ : @list.List[(Binder, Expr)],
    condition~ : Expr?,
    continue_block~ : @list.List[(Binder, Expr)],
    body~ : Expr,
    for_else~ : Expr?,
    where_clause~ : WhereClause?,
    label~ : Label?,
    loc~ : Location
  )
  /// `for b1,b2 in e1; init; continue_block { e2 }`
  ForEach(
    binders~ : @list.List[Binder?],
    expr~ : Expr,
    init~ : @list.List[(Binder, Expr)],
    continue_block~ : @list.List[(Binder, Expr)],
    body~ : Expr,
    else_block~ : Expr?,
    where_clause~ : WhereClause?,
    label~ : Label?,
    loc~ : Location
  )
  /// `try e0 catch { p1 => e1 } noraise { p2 => e2 }` or `e0 catch { p1 => e1 }`
  Try(
    body~ : Expr,
    catch_~ : @list.List[Case],
    try_else~ : @list.List[Case]?,
    has_try~ : Bool,
    try_loc~ : Location,
    catch_loc~ : Location,
    else_loc~ : Location,
    loc~ : Location
  )
  /// `try! expr` or `try? expr`
  TryOperator(
    body~ : Expr,
    kind~ : TryOperatorKind,
    try_loc~ : Location,
    loc~ : Location
  )
  /// `{ k1: e1, k2: e2 }`
  Map(elems~ : @list.List[MapExprElem], loc~ : Location)
  /// `(expr)` or `{expr}`
  Group(expr~ : Expr, group~ : Group, loc~ : Location)
  /// `expr <+ template`
  TemplateWriting(
    expr~ : Expr,
    template~ : Expr,
    is_conditional~ : Bool,
    loc~ : Location
  )
  /// generated by compiler
  StaticAssert(asserts~ : @list.List[StaticAssertion], body~ : Expr)
} derive(Debug)

///|
pub(all) enum TryOperatorKind {
  Question
  Exclamation
} derive(Debug)

///|
pub(all) enum DotDotBinder {
  Underscore
  NoBinder
  BinderAs(Binder)
  Binder(Binder)
} derive(Debug)

///|
pub(all) enum ArrayPattern {
  Pattern(Pattern)
  StringSpread(str~ : StringLiteral, loc~ : Location)
  BytesSpread(bytes~ : BytesLiteral, loc~ : Location)
  ConstSpread(binder~ : Binder, pkg~ : String?, loc~ : Location)
} derive(Debug)

///|
pub(all) enum ArrayPatterns {
  Closed(@list.List[ArrayPattern])
  Open(@list.List[ArrayPattern], @list.List[ArrayPattern], DotDotBinder)
} derive(Debug)

///|
pub(all) struct WhereClause {
  fields : @list.List[FieldDef]
  loc : Location
} derive(Debug)

///|
pub(all) struct FieldPat {
  label : Label
  pattern : Pattern
  is_pun : Bool
  loc : Location
} derive(Debug)

///|
pub(all) struct ConstrPatArg {
  pat : Pattern
  kind : ArgumentKind
} derive(Debug)

///|
pub(all) struct MapPatElem {
  key : Constant
  pat : Pattern
  match_absent : Bool
  key_loc : Location
  loc : Location
} derive(Debug)

///|
pub(all) enum Pattern {
  /// `pattern as binder`
  Alias(pat~ : Pattern, alias_~ : Binder, loc~ : Location)
  /// `_`
  Any(loc~ : Location)
  /// `[p1, p2, p3]`
  Array(pats~ : ArrayPatterns, loc~ : Location)
  Constant(c~ : Constant, loc~ : Location)
  /// `(pattern : Type)`
  Constraint(pat~ : Pattern, ty~ : Type, loc~ : Location)
  /// `Constructor` or `Constructor(p1, p2, p3)`
  Constr(
    constr~ : Constructor,
    args~ : @list.List[ConstrPatArg]?,
    is_open~ : Bool,
    loc~ : Location
  )
  /// `pat1 | pat2`
  Or(pat1~ : Pattern, pat2~ : Pattern, loc~ : Location)
  /// `(p1, p2, p3)`
  Tuple(pats~ : @list.List[Pattern], loc~ : Location)
  /// `binder`
  Var(Binder)
  /// `{ field1, field2, .. }`
  Record(fields~ : @list.List[FieldPat], is_closed~ : Bool, loc~ : Location)
  /// `{ "k1": e1, "k2": e2, .. }`
  Map(elems~ : @list.List[MapPatElem], is_closed~ : Bool, loc~ : Location)
  /// `p1..=p2` or `p1..<p2` 
  Range(lhs~ : Pattern, rhs~ : Pattern, kind~ : RangeKind, loc~ : Location)
  SpecialConstr(
    binder~ : Binder,
    args~ : @list.List[ConstrPatArg],
    loc~ : Location
  )
} derive(Debug)

///|
pub type TestName = (StringLiteral, Location)?

///|
// checked!
pub(all) struct DerivingDirective {
  type_name : TypeName
  args : @list.List[Argument]
  loc : Location
} derive(Debug)

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
  is_declare : Bool
} derive(Debug)

///|
pub(all) enum FuncStubs {
  Import(
    module_name~ : StringLiteral,
    func_name~ : StringLiteral,
    language~ : StringLiteral?
  )
  Embedded(language~ : StringLiteral?, code~ : EmbeddedCode)
} derive(Debug)

///|
pub(all) enum EmbeddedCode {
  CodeString(StringLiteral)
  CodeMultilineString(@list.List[String])
} derive(Debug)

///|
pub(all) enum DeclBody {
  DeclBody(expr~ : Expr)
  DeclStubs(FuncStubs)
  DeclNone
} derive(Debug)

///|
pub(all) struct FunDecl {
  type_name : TypeName?
  name : Binder
  is_async : Location?
  decl_params : Parameters?
  params_loc : Location
  quantifiers : @list.List[TypeVarBinder]
  return_type : Type?
  error_type : ErrorType
  vis : Visibility
  attrs : @list.List[Attribute]
  mut doc : DocString
  loc : Location
} derive(Debug)

///|
pub(all) struct TraitMethodDecl {
  name : Binder
  is_async : Location?
  quantifiers : @list.List[TypeVarBinder]
  params : Parameters
  return_type : Type?
  error_type : ErrorType
  has_default : Location?
  attrs : @list.List[Attribute]
  loc : Location
} derive(Debug)

///|
pub(all) struct TraitDecl {
  name : Binder
  supers : @list.List[TypeVarConstraint]
  methods : @list.List[TraitMethodDecl]
  vis : Visibility
  loc : Location
  attrs : @list.List[Attribute]
  mut doc : DocString
  is_declare : Bool
} derive(Debug)

///|
pub(all) enum UsingKind {
  Value
  Type
  Trait
} derive(Debug)

///|
pub(all) enum Impl {
  TopExpr(expr~ : Expr, is_main~ : Bool, is_async~ : Location?, loc~ : Location)
  TopTest(
    expr~ : Expr,
    name~ : TestName,
    params~ : Parameters?,
    is_async~ : Location?,
    loc~ : Location,
    attrs~ : @list.List[Attribute],
    mut doc~ : DocString
  )
  TopTypeDef(TypeDecl)
  TopFuncDef(
    fun_decl~ : FunDecl,
    decl_body~ : DeclBody,
    where_clause~ : WhereClause?,
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
    mut doc~ : DocString,
    is_declare~ : Bool
  )
  TopTrait(TraitDecl)
  TopImpl(
    self_ty~ : Type?,
    trait_~ : TypeName,
    method_name~ : Binder,
    quantifiers~ : @list.List[TypeVarBinder],
    method_quantifiers~ : @list.List[TypeVarBinder],
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
    mut doc~ : DocString,
    is_declare~ : Bool
  )
  TopUsing(
    pkg~ : Label,
    names~ : @list.List[(AliasTarget, UsingKind)],
    vis~ : Visibility,
    attrs~ : @list.List[Attribute],
    loc~ : Location,
    mut doc~ : DocString
  )
} derive(Debug)

///|
pub type Impls = @list.List[Impl]
