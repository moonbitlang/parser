///|
pub(all) enum RangeKind {
  Inclusive
  Exclusive
  InclusiveMissingEqual
}

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
  Regex(StringLiteral)
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
  Dot(pkg~ : String, id~ : String, pkg_loc~ : Location, dot_id_loc~ : Location)
}

///|
/// The dummy LongIdent is used in typer to represent an parsing error.
pub fn LongIdent::dummy() -> LongIdent {
  Ident(name="")
}

///|
pub(all) struct TypeName {
  name : LongIdent
  is_object : Bool
  object_amp_loc : Location?
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
    is_async~ : Location?,
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    arrow_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  Tuple(
    tys~ : @list.List[Type],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  Name(
    constr_id~ : ConstrId,
    tys~ : @list.List[Type],
    type_args_lbracket_loc~ : Location?,
    type_args_rbracket_loc~ : Location?,
    type_arg_comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  Option(ty~ : Type, loc~ : Location, question_loc~ : Location)
  Object(constr_id~ : ConstrId, amp_loc~ : Location, loc~ : Location)
}

///|
pub(all) enum ErrorType {
  ErrorType(ty~ : Type, raise_loc~ : Location, loc~ : Location)
  DefaultErrorType(loc~ : Location)
  NoErrorType
  Noraise(loc~ : Location)
  MaybeError(
    ty~ : Type,
    raise_loc~ : Location,
    question_loc~ : Location,
    loc~ : Location
  )
}

///|
pub(all) struct ConstrParam {
  ty : Type
  mut_ : Bool
  label : Label?
  mut_loc : Location?
  post_label_loc : Location?
  colon_loc : Location?
}

///|
pub(all) struct ConstrDecl {
  name : ConstrName
  args : @list.List[ConstrParam]?
  args_lparen_loc : Location?
  args_rparen_loc : Location?
  arg_comma_locs : @list.List[Location]
  tag : (String, Location)?
  tag_equal_loc : Location?
  loc : Location
  attrs : @list.List[Attribute]
  mut doc : DocString
}

///|
pub(all) enum ExceptionDecl {
  NoPayload
  EnumPayload(
    constrs~ : @list.List[ConstrDecl],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location]
  )
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
  mut_loc : Location?
  colon_loc : Location
  vis : Visibility
  loc : Location
  attrs : @list.List[Attribute]
  mut doc : DocString
}

///|
pub(all) enum TypeDesc {
  /// `type T`
  Abstract
  /// `extenum T { C1; C2 }`
  ExtensibleEnum(
    constrs~ : @list.List[ConstrDecl],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location]
  )
  /// `suberror`
  Error(ExceptionDecl)
  /// `enum T { C1; C2 }`
  Variant(
    constrs~ : @list.List[ConstrDecl],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location]
  )
  /// `extenum T += { C1; C2 }`
  ExtendEnum(
    target~ : TypeName,
    constructors~ : @list.List[ConstrDecl],
    plus_equal_loc~ : Location,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location]
  )
  /// `struct T { field1: T1; field2: T2 }`
  Record(
    fields~ : @list.List[FieldDecl],
    constr_decl~ : FunDecl?,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location]
  )
  /// `struct T (T1,T2)`
  TupleStruct(
    types~ : @list.List[Type],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location]
  )
  Alias(type_~ : Type, equal_loc~ : Location)
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
  Labelled(label~ : Label, equal_loc~ : Location)
  LabelledPun(Label)
  LabelledOption(
    label~ : Label,
    question_loc~ : Location,
    equal_loc~ : Location
  )
  LabelledOptionPun(label~ : Label, question_loc~ : Location)
}

///|
pub(all) enum FnKind {
  Lambda
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
pub(all) struct TypeVarConstraint {
  trait_ : LongIdent
  loc : Location
}

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
  /// location of `:` before constraints, if any
  colon_loc : Location?
  /// locations of `+` tokens between constraints
  plus_locs : @list.List[Location]
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
  TypeName(type_name~ : TypeName, coloncolon_loc~ : Location)
  TypeNameWithConstrPackage(
    type_name~ : TypeName,
    pkg~ : String,
    coloncolon_loc~ : Location,
    pkg_loc~ : Location,
    dot_constr_loc~ : Location
  )
  Package(pkg~ : String, pkg_loc~ : Location, dot_constr_loc~ : Location)
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
  as_loc : Location?
}

///|
pub(all) struct Argument {
  value : Expr
  kind : ArgumentKind
}

///|
pub(all) enum Parameter {
  /// `_ : Int`
  DiscardPositional(ty~ : Type?, colon_loc~ : Location?, loc~ : Location)
  /// `param : Int`
  Positional(binder~ : Binder, ty~ : Type?, colon_loc~ : Location?)
  /// `label~ : Int`
  Labelled(
    binder~ : Binder,
    ty~ : Type?,
    post_label_loc~ : Location?,
    colon_loc~ : Location?
  )
  /// `label? : Int = default`
  Optional(
    binder~ : Binder,
    default~ : Expr,
    ty~ : Type?,
    post_label_loc~ : Location?,
    question_loc~ : Location?,
    colon_loc~ : Location?,
    equal_loc~ : Location
  )
  /// `label? : Int`
  QuestionOptional(
    binder~ : Binder,
    ty~ : Type?,
    question_loc~ : Location,
    colon_loc~ : Location?
  )
}

///|
pub type Parameters = @list.List[Parameter]

///|
pub(all) struct Case {
  pattern : Pattern
  guard_ : Expr?
  body : Expr
  if_loc : Location?
  fat_arrow_loc : Location?
  ellipsis_loc : Location?
}

///|
pub(all) enum SpreadableElem {
  /// `expr`
  Regular(Expr)
  /// `..expr`
  Spread(expr~ : Expr, dotdot_loc~ : Location, loc~ : Location)
}

///|
pub(all) struct MapExprElem {
  key : Constant
  expr : Expr
  colon_loc : Location
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
pub(all) struct Func {
  parameters : Parameters
  params_loc : Location
  body : Expr
  return_type : Type?
  error_type : ErrorType
  kind : FnKind
  is_async : Location?
  fn_loc : Location?
  lparen_loc : Location?
  rparen_loc : Location?
  comma_locs : @list.List[Location]
  return_arrow_loc : Location?
  fat_arrow_loc : Location?
  body_lbrace_loc : Location?
  body_rbrace_loc : Location?
  loc : Location
}

///|
pub(all) struct FieldDef {
  label : Label
  expr : Expr
  is_pun : Bool
  colon_loc : Location?
  loc : Location
}

///|
pub(all) enum InterpElem {
  Literal(repr~ : StringLiteral, loc~ : Location)
  Expr(expr~ : Expr, loc~ : Location)
  Source(@tokens.InterpSource)
}

///|
pub(all) enum MultilineStringElem {
  String(String)
  Interp(@list.List[InterpElem])
}

///|
pub(all) enum LexPattern {
  Regex(lit~ : String, offset~ : Int, loc~ : Location)
  RegexInterp(elems~ : @list.List[InterpElem], loc~ : Location)
  ConstantRef(lid~ : LongIdent, loc~ : Location)
  Alias(
    pat~ : LexPattern,
    binder~ : Binder,
    as_loc~ : Location,
    loc~ : Location
  )
  Sequence(
    pats~ : @list.List[LexPattern],
    semi_locs~ : @list.List[Location],
    loc~ : Location
  )
}

///|
pub(all) enum LexTopPattern {
  Pattern(LexPattern)
  Binder(Binder)
  Wildcard(loc~ : Location)
}

///|
pub(all) struct LexCase {
  pat : @list.List[LexTopPattern]
  pat_loc : Location
  guard_ : Expr?
  body : Expr
  if_loc : Location?
  fat_arrow_loc : Location?
  ellipsis_loc : Location?
}

///|
pub(all) enum RegexPattern {
  Literal(lit~ : String, loc~ : Location)
  Reference(lid~ : LongIdent, loc~ : Location)
  Sequence(
    pat1~ : RegexPattern,
    pat2~ : RegexPattern,
    plus_loc~ : Location,
    loc~ : Location
  )
  Alternation(
    pat1~ : RegexPattern,
    pat2~ : RegexPattern,
    bar_loc~ : Location,
    loc~ : Location
  )
  Alias(
    pat~ : RegexPattern,
    binder~ : Binder,
    as_loc~ : Location,
    loc~ : Location
  )
}

///|
pub(all) enum PredicateQuantifierKind {
  Forall
  Exists
}

///|
pub(all) enum ListComprehensionKind {
  Foreach(
    binders~ : @list.List[Binder?],
    expr~ : Expr,
    init~ : @list.List[(Binder, Expr)],
    continue_block~ : @list.List[(Binder, Expr)],
    for_loc~ : Location,
    in_loc~ : Location,
    binder_comma_locs~ : @list.List[Location],
    semi_locs~ : @list.List[Location],
    init_comma_locs~ : @list.List[Location],
    init_equal_locs~ : @list.List[Location],
    continue_comma_locs~ : @list.List[Location],
    continue_equal_locs~ : @list.List[Location]
  )
  For(
    binders~ : @list.List[(Binder, Expr)],
    condition~ : Expr?,
    continue_block~ : @list.List[(Binder, Expr)],
    for_loc~ : Location,
    binder_comma_locs~ : @list.List[Location],
    semi_locs~ : @list.List[Location],
    binder_equal_locs~ : @list.List[Location],
    continue_comma_locs~ : @list.List[Location],
    continue_equal_locs~ : @list.List[Location]
  )
}

///|
pub(all) enum Expr {
  /// f(args)
  Apply(
    func~ : Expr,
    args~ : @list.List[Argument],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// e1 + e2
  Infix(op~ : Var, lhs~ : Expr, rhs~ : Expr, loc~ : Location)
  /// -e
  Unary(op~ : Var, expr~ : Expr, loc~ : Location)
  /// [e1, e2, e3]
  Array(
    exprs~ : @list.List[Expr],
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// [..arr1, e1, ..arr2, e2]
  ArraySpread(
    elems~ : @list.List[SpreadableElem],
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `[for x in xs if guard => body]`
  ListComprehension(
    kind~ : ListComprehensionKind,
    guard_~ : Expr?,
    body~ : Expr,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    if_loc~ : Location?,
    fat_arrow_loc~ : Location,
    loc~ : Location
  )
  /// e1[e2]
  ArrayGet(
    array~ : Expr,
    index~ : Expr,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    loc~ : Location
  )
  /// e1[e2:e3]
  ArrayGetSlice(
    array~ : Expr,
    start_index~ : Expr?,
    end_index~ : Expr?,
    index_loc~ : Location,
    lbracket_loc~ : Location,
    colon_loc~ : Location,
    rbracket_loc~ : Location,
    loc~ : Location
  )
  /// e1[i] = e2
  ArraySet(
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    equal_loc~ : Location,
    loc~ : Location
  )
  /// e1[i] += e2
  ArrayAugmentedSet(
    op~ : Var,
    array~ : Expr,
    index~ : Expr,
    value~ : Expr,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    augmented_assign_loc~ : Location,
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
  /// `(expr : Type)`
  Constraint(
    expr~ : Expr,
    ty~ : Type,
    lparen_loc~ : Location?,
    colon_loc~ : Location,
    rparen_loc~ : Location?,
    loc~ : Location
  )
  /// `Constructor(e1, e2, e3)`
  Constr(constr~ : Constructor, loc~ : Location)
  /// `while e1 { e2 } else { e3 }`
  While(
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
    loc~ : Location
  )
  /// `fn(args){ expr }` or `(args) => expr`
  Function(func~ : Func, loc~ : Location)
  /// `id`
  Ident(id~ : Var, loc~ : Location)
  /// `if e1 { e2 } else { e3 }`
  If(
    cond~ : Expr,
    ifso~ : Expr,
    ifnot~ : Expr?,
    if_loc~ : Location,
    then_lbrace_loc~ : Location,
    then_rbrace_loc~ : Location,
    else_loc~ : Location?,
    else_lbrace_loc~ : Location?,
    else_rbrace_loc~ : Location?,
    loc~ : Location
  )
  /// `guard e1 else {e2}; e3`
  Guard(
    cond~ : Expr,
    otherwise~ : Expr?,
    body~ : Expr,
    guard_loc~ : Location,
    else_loc~ : Location?,
    else_lbrace_loc~ : Location?,
    else_rbrace_loc~ : Location?,
    semi_loc~ : Location?,
    loc~ : Location
  )
  /// `expr is pattern`
  Is(expr~ : Expr, pat~ : Pattern, is_loc~ : Location, loc~ : Location)
  /// `expr lexmatch? pattern with strategy`
  IsLexMatch(
    expr~ : Expr,
    strategy~ : Label?,
    pat~ : @list.List[LexTopPattern],
    pat_loc~ : Location,
    lexmatch_question_loc~ : Location,
    with_loc~ : Location?,
    loc~ : Location
  )
  RegexMatch(
    expr~ : Expr,
    pat~ : RegexPattern,
    bindings~ : @list.List[(Label, Binder?)],
    eq_tilde_loc~ : Location,
    rhs_lparen_loc~ : Location?,
    rhs_rparen_loc~ : Location?,
    binding_comma_locs~ : @list.List[Location],
    binding_equal_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `defer e1; e2`
  Defer(
    expr~ : Expr,
    body~ : Expr,
    defer_loc~ : Location,
    semi_loc~ : Location?,
    loc~ : Location
  )
  /// `fn local(args){ e1 }; e2`
  LetFn(
    name~ : Binder,
    func~ : Func,
    body~ : Expr,
    semi_loc~ : Location?,
    loc~ : Location
  )
  LetAnd(
    bindings~ : @list.List[(Binder, Type?, Func)],
    body~ : Expr,
    letrec_loc~ : Location,
    and_locs~ : @list.List[Location],
    equal_locs~ : @list.List[Location],
    annot_colon_locs~ : @list.List[Location],
    semi_loc~ : Location?,
    loc~ : Location
  )
  /// `let pattern = e1; e2`
  Let(
    pattern~ : Pattern,
    expr~ : Expr,
    body~ : Expr,
    let_loc~ : Location,
    annot_colon_loc~ : Location?,
    equal_loc~ : Location,
    semi_loc~ : Location?,
    loc~ : Location
  )
  /// `e1; e2`
  Sequence(
    exprs~ : @list.List[Expr],
    last_expr~ : Expr,
    semi_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `(e1, e2, e3)`
  Tuple(
    exprs~ : @list.List[Expr],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `TypeName::{ field1: e1, field2: e2 }`
  /// The `trailing` field is used to determine whether the source has a trailing separator.
  /// If the trailing separator is SEMI, the AST is invalid.
  Record(
    type_name~ : TypeName?,
    fields~ : @list.List[FieldDef],
    trailing~ : TrailingMark,
    coloncolon_loc~ : Location?,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    semi_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `{ ..record, field1: expr, field2: expr }`
  RecordUpdate(
    type_name~ : TypeName?,
    record~ : Expr,
    fields~ : @list.List[FieldDef],
    coloncolon_loc~ : Location?,
    lbrace_loc~ : Location,
    dotdot_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `record.field`
  Field(record~ : Expr, accessor~ : Accessor, loc~ : Location)
  /// `TypeName::method`
  Method(
    type_name~ : TypeName,
    method_name~ : Label,
    coloncolon_loc~ : Location,
    loc~ : Location
  )
  /// `self.method(args)`
  DotApply(
    self~ : Expr,
    method_name~ : Label,
    args~ : @list.List[Argument],
    return_self~ : Bool,
    dot_loc~ : Location,
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `expr as Trait`
  As(expr~ : Expr, trait_~ : TypeName, as_loc~ : Location, loc~ : Location)
  /// `record.field = expr`
  Mutate(
    record~ : Expr,
    accessor~ : Accessor,
    field~ : Expr,
    augmented_by~ : Var?,
    equal_loc~ : Location?,
    augmented_assign_loc~ : Location?,
    loc~ : Location
  )
  /// `match e0 { p1 => e1; p2 => e2 }`
  Match(
    expr~ : Expr,
    cases~ : @list.List[Case],
    match_loc~ : Location,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `lexmatch using strategy { c1 => e1; c2 => e2 }`
  LexMatch(
    strategy~ : Label?,
    expr~ : Expr,
    match_loc~ : Location,
    with_loc~ : Location?,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    semi_locs~ : @list.List[Location],
    cases~ : @list.List[LexCase],
    loc~ : Location
  )
  /// `let mut binder : Type = expr`
  LetMut(
    binder~ : Binder,
    ty~ : Type?,
    expr~ : Expr,
    body~ : Expr,
    let_loc~ : Location,
    mut_loc~ : Location,
    annot_colon_loc~ : Location?,
    equal_loc~ : Location,
    semi_loc~ : Location?,
    loc~ : Location
  )
  /// `lhs |> rhs`
  /// The rhs is restricted to `f`, `f(args)`, `_.method(args)`, `Constructor`, or `Constructor(args)`.
  Pipe(lhs~ : Expr, rhs~ : Expr, pipe_loc~ : Location, loc~ : Location)
  /// `lhs <| rhs`
  RevPipe(lhs~ : Expr, rhs~ : Expr, rev_pipe_loc~ : Location, loc~ : Location)
  /// `n = e` or `n += e`
  Assign(
    var_~ : Var,
    expr~ : Expr,
    augmented_by~ : Var?,
    equal_loc~ : Location?,
    augmented_assign_loc~ : Location?,
    loc~ : Location
  )
  /// `_`
  /// This expression is only allowed in partial application.
  Hole(loc~ : Location, kind~ : Hole)
  /// `return expr`
  Return(return_value~ : Expr?, return_loc~ : Location, loc~ : Location)
  /// `raise expr`
  Raise(err_value~ : Expr, raise_loc~ : Location, loc~ : Location)
  Quantifier(
    kind~ : PredicateQuantifierKind,
    binder~ : Binder,
    binder_ty~ : Type,
    body~ : Expr,
    quantifier_loc~ : Location,
    colon_loc~ : Location,
    comma_loc~ : Location,
    loc~ : Location
  )
  Implies(lhs~ : Expr, rhs~ : Expr, implies_loc~ : Location, loc~ : Location)
  ProofAssert(expr~ : Expr, proof_assert_loc~ : Location, loc~ : Location)
  ProofLet(
    binder~ : Binder,
    expr~ : Expr,
    proof_let_loc~ : Location,
    equal_loc~ : Location,
    loc~ : Location
  )
  /// `()`
  /// If `faked` is true, it means it was inserted by the parser.
  Unit(
    loc~ : Location,
    faked~ : Bool,
    lparen_loc~ : Location,
    rparen_loc~ : Location
  )
  /// `break expr`
  Break(
    arg~ : Expr?,
    label~ : Label?,
    break_loc~ : Location,
    post_label_loc~ : Location?,
    loc~ : Location
  )
  /// `continue e1, e2, e3`
  Continue(
    args~ : @list.List[Expr],
    label~ : Label?,
    continue_loc~ : Location,
    post_label_loc~ : Location?,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `for binders; condition; continue_block { body } else { expr }`
  For(
    binders~ : @list.List[(Binder, Expr)],
    condition~ : Expr?,
    continue_block~ : @list.List[(Binder, Expr)],
    body~ : Expr,
    for_else~ : Expr?,
    where_clause~ : WhereClause?,
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
    catch_lbrace_loc~ : Location,
    catch_rbrace_loc~ : Location,
    catch_semi_locs~ : @list.List[Location],
    noraise_lbrace_loc~ : Location?,
    noraise_rbrace_loc~ : Location?,
    noraise_semi_locs~ : @list.List[Location],
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
  Map(
    elems~ : @list.List[MapExprElem],
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `(expr)` or `{expr}`
  Group(
    expr~ : Expr,
    group~ : Group,
    left_delim_loc~ : Location,
    right_delim_loc~ : Location,
    loc~ : Location
  )
  /// `expr <+ template`
  TemplateWriting(
    expr~ : Expr,
    template~ : Expr,
    lt_plus_loc~ : Location,
    loc~ : Location
  )
  /// generated by compiler
  StaticAssert(asserts~ : @list.List[StaticAssertion], body~ : Expr)
}

///|
pub(all) enum TryOperatorKind {
  Question
  Exclamation
}

///|
pub(all) enum DotDotBinder {
  Underscore(dotdot_loc~ : Location, underscore_loc~ : Location)
  NoBinder(dotdot_loc~ : Location)
  BinderAs(binder~ : Binder, dotdot_loc~ : Location, as_loc~ : Location)
  Binder(binder~ : Binder, dotdot_loc~ : Location)
}

///|
pub(all) enum ArrayPattern {
  Pattern(Pattern)
  StringSpread(str~ : StringLiteral, dotdot_loc~ : Location, loc~ : Location)
  BytesSpread(bytes~ : BytesLiteral, dotdot_loc~ : Location, loc~ : Location)
  ConstSpread(
    binder~ : Binder,
    pkg~ : String?,
    dotdot_loc~ : Location,
    pkg_loc~ : Location?,
    dot_uident_loc~ : Location?,
    loc~ : Location
  )
}

///|
pub(all) enum ArrayPatterns {
  Closed(@list.List[ArrayPattern], comma_locs~ : @list.List[Location])
  Open(
    @list.List[ArrayPattern],
    @list.List[ArrayPattern],
    DotDotBinder,
    left_comma_locs~ : @list.List[Location],
    dotdot_comma_loc~ : Location?,
    right_comma_locs~ : @list.List[Location]
  )
}

///|
pub(all) struct WhereClause {
  fields : @list.List[FieldDef]
  where_loc : Location
  lbrace_loc : Location
  rbrace_loc : Location
  comma_locs : @list.List[Location]
  loc : Location
}

///|
pub(all) struct FieldPat {
  label : Label
  pattern : Pattern
  is_pun : Bool
  colon_loc : Location?
  loc : Location
}

///|
pub(all) struct ConstrPatArg {
  pat : Pattern
  kind : ArgumentKind
  equal_loc : Location?
}

///|
pub(all) struct MapPatElem {
  key : Constant
  pat : Pattern
  match_absent : Bool
  key_loc : Location
  question_loc : Location?
  colon_loc : Location
  loc : Location
}

///|
pub(all) enum Pattern {
  /// `pattern as binder`
  Alias(pat~ : Pattern, alias_~ : Binder, as_loc~ : Location, loc~ : Location)
  /// `_`
  Any(loc~ : Location)
  /// `[p1, p2, p3]`
  Array(
    pats~ : ArrayPatterns,
    lbracket_loc~ : Location,
    rbracket_loc~ : Location,
    loc~ : Location
  )
  Constant(c~ : Constant, loc~ : Location)
  /// `(pattern : Type)`
  Constraint(
    pat~ : Pattern,
    ty~ : Type,
    lparen_loc~ : Location?,
    colon_loc~ : Location,
    rparen_loc~ : Location?,
    loc~ : Location
  )
  /// `Constructor` or `Constructor(p1, p2, p3)`
  Constr(
    constr~ : Constructor,
    args~ : @list.List[ConstrPatArg]?,
    is_open~ : Bool,
    args_lparen_loc~ : Location?,
    args_rparen_loc~ : Location?,
    arg_comma_locs~ : @list.List[Location],
    open_dotdot_loc~ : Location?,
    loc~ : Location
  )
  /// `pat1 | pat2`
  Or(pat1~ : Pattern, pat2~ : Pattern, bar_loc~ : Location, loc~ : Location)
  /// `(p1, p2, p3)`
  Tuple(
    pats~ : @list.List[Pattern],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
  /// `binder`
  Var(Binder)
  /// `{ field1, field2, .. }`
  Record(
    fields~ : @list.List[FieldPat],
    is_closed~ : Bool,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    dotdot_loc~ : Location?,
    loc~ : Location
  )
  /// `{ "k1": e1, "k2": e2, .. }`
  Map(
    elems~ : @list.List[MapPatElem],
    is_closed~ : Bool,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    dotdot_loc~ : Location?,
    loc~ : Location
  )
  /// `p1..=p2` or `p1..<p2` 
  Range(
    lhs~ : Pattern,
    rhs~ : Pattern,
    kind~ : RangeKind,
    range_op_loc~ : Location,
    loc~ : Location
  )
  SpecialConstr(
    binder~ : Binder,
    args~ : @list.List[ConstrPatArg],
    lparen_loc~ : Location,
    rparen_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location
  )
}

///|
pub type TestName = (StringLiteral, Location)?

///|
// checked!
pub(all) struct DerivingDirective {
  type_name : TypeName
  args : @list.List[Argument]
  args_lparen_loc : Location?
  args_rparen_loc : Location?
  arg_comma_locs : @list.List[Location]
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
  declare_loc : Location?
  keyword_loc : Location
  params_lbracket_loc : Location?
  params_rbracket_loc : Location?
  param_comma_locs : @list.List[Location]
  derive_loc : Location?
  derive_lparen_loc : Location?
  derive_rparen_loc : Location?
  derive_comma_locs : @list.List[Location]
  loc : Location
  is_declare : Bool
}

///|
pub(all) enum FuncStubs {
  Import(
    module_name~ : StringLiteral,
    func_name~ : StringLiteral,
    language~ : StringLiteral?,
    module_name_loc~ : Location,
    func_name_loc~ : Location,
    language_loc~ : Location?
  )
  Embedded(
    language~ : StringLiteral?,
    code~ : EmbeddedCode,
    language_loc~ : Location?,
    code_loc~ : Location
  )
}

///|
pub(all) enum EmbeddedCode {
  CodeString(StringLiteral, loc~ : Location)
  CodeMultilineString(@list.List[String], string_locs~ : @list.List[Location])
}

///|
pub(all) enum DeclBody {
  DeclBody(expr~ : Expr, lbrace_loc~ : Location?, rbrace_loc~ : Location?)
  DeclStubs(FuncStubs, equal_loc~ : Location)
  DeclNone
}

///|
pub(all) struct FunDecl {
  type_name : TypeName?
  name : Binder
  is_async : Location?
  declare_loc : Location?
  extern_loc : Location?
  fn_loc : Location
  coloncolon_loc : Location?
  decl_params : Parameters?
  params_loc : Location
  type_params_lbracket_loc : Location?
  type_params_rbracket_loc : Location?
  type_param_comma_locs : @list.List[Location]
  params_lparen_loc : Location?
  params_rparen_loc : Location?
  param_comma_locs : @list.List[Location]
  quantifiers : @list.List[TypeVarBinder]
  return_type : Type?
  return_arrow_loc : Location?
  error_type : ErrorType
  vis : Visibility
  attrs : @list.List[Attribute]
  mut doc : DocString
  loc : Location
}

///|
pub(all) struct TraitMethodDecl {
  name : Binder
  is_async : Location?
  fn_loc : Location?
  quantifiers : @list.List[TypeVarBinder]
  type_params_lbracket_loc : Location?
  type_params_rbracket_loc : Location?
  type_param_comma_locs : @list.List[Location]
  params : Parameters
  params_lparen_loc : Location
  params_rparen_loc : Location
  param_comma_locs : @list.List[Location]
  return_type : Type?
  return_arrow_loc : Location?
  error_type : ErrorType
  has_default : Location?
  default_equal_loc : Location?
  attrs : @list.List[Attribute]
  loc : Location
}

///|
pub(all) struct TraitDecl {
  name : Binder
  supers : @list.List[TypeVarConstraint]
  methods : @list.List[TraitMethodDecl]
  vis : Visibility
  declare_loc : Location?
  trait_loc : Location
  colon_loc : Location?
  super_plus_locs : @list.List[Location]
  lbrace_loc : Location
  rbrace_loc : Location
  semi_locs : @list.List[Location]
  loc : Location
  attrs : @list.List[Attribute]
  mut doc : DocString
  is_declare : Bool
}

///|
pub(all) enum UsingKind {
  Value
  Type(loc~ : Location)
  Trait(loc~ : Location)
}

///|
pub(all) enum Impl {
  TopExpr(expr~ : Expr, is_main~ : Bool, is_async~ : Location?, loc~ : Location)
  TopTest(
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
    mut doc~ : DocString
  )
  TopTypeDef(TypeDecl)
  TopFuncDef(
    fun_decl~ : FunDecl,
    decl_body~ : DeclBody,
    where_clause~ : WhereClause?,
    equal_loc~ : Location?,
    loc~ : Location
  )
  TopLetDef(
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
    mut doc~ : DocString,
    is_declare~ : Bool
  )
  TopTrait(TraitDecl)
  TopImpl(
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
    mut doc~ : DocString
  )
  TopImplRelation(
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
    mut doc~ : DocString,
    is_declare~ : Bool
  )
  TopUsing(
    pkg~ : Label,
    names~ : @list.List[(AliasTarget, UsingKind)],
    vis~ : Visibility,
    attrs~ : @list.List[Attribute],
    using_loc~ : Location,
    lbrace_loc~ : Location,
    rbrace_loc~ : Location,
    comma_locs~ : @list.List[Location],
    loc~ : Location,
    mut doc~ : DocString
  )
}

///|
pub type Impls = @list.List[Impl]
