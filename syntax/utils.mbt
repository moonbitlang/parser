///|
/// MoonBit parsing utilities - translated from OCaml parsing_util.ml
// Translated from src/ast/parsing_util.ml

// Global counter for generating unique variable names
let counter : Ref[Int] = Ref(0)

///|
/// Create constant expression with location
pub fn make_constant_expr(loc~ : Location, c : Constant) -> Expr {
  Constant(c~, loc~)
}

///|
/// Create identifier expression
pub fn make_ident_expr(loc~ : Location, id : Var) -> Expr {
  Ident(id~, loc~)
}

///|
/// Create tuple expression
pub fn make_tuple_expr(
  loc~ : Location,
  lparen_loc? : Location = no_location,
  rparen_loc? : Location = no_location,
  comma_locs? : @list.List[Location] = @list.empty(),
  exprs : @list.List[Expr],
) -> Expr {
  Tuple(exprs~, lparen_loc~, rparen_loc~, comma_locs~, loc~)
}

///|
/// Create array expression with possible spread elements
pub fn make_array_expr(
  loc~ : Location,
  lbracket_loc? : Location = no_location,
  rbracket_loc? : Location = no_location,
  comma_locs? : @list.List[Location] = @list.empty(),
  elems : @list.List[SpreadableElem],
) -> Expr {
  // Try to convert to regular array if no spread elements
  let mut acc : @list.List[Expr] = @list.empty()
  for elem in elems {
    match elem {
      Regular(e) => acc = acc.add(e)
      Spread(_) =>
        return ArraySpread(
          elems~,
          lbracket_loc~,
          rbracket_loc~,
          comma_locs~,
          loc~,
        )
    }
  } nobreak {
    return Array(
      // If we have only regular elements, create a simple array
      exprs=acc.rev(),
      lbracket_loc~,
      rbracket_loc~,
      comma_locs~,
      loc~,
    )
  }
}

///|
/// Create interpolation elements from literal components
pub fn make_interps(c : @tokens.InterpLiteral) -> @list.List[InterpElem] {
  let result = c.map(fn(lit) {
    match lit {
      InterpLit(repr~, loc~) => InterpElem::Literal(repr~, loc~)
      InterpSource(s) => InterpElem::Source(s)
    }
  })
  @list.from_array(result)
}

///|
/// Create interpolation expression
pub fn make_interp_expr(loc~ : Location, c : @tokens.InterpLiteral) -> Expr {
  let elems = make_interps(c)
  Interp(elems~, loc~)
}

///|
/// Create record expression
pub fn make_record_expr(
  loc~ : Location,
  trailing~ : TrailingMark,
  coloncolon_loc? : Location? = None,
  lbrace_loc? : Location = no_location,
  rbrace_loc? : Location = no_location,
  comma_locs? : @list.List[Location] = @list.empty(),
  semi_locs? : @list.List[Location] = @list.empty(),
  type_name : TypeName?,
  fields : @list.List[FieldDef],
) -> Expr {
  Record(
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
/// Create alias pattern
pub fn make_alias_pattern(
  loc~ : Location,
  as_loc? : Location = no_location,
  pat : Pattern,
  alias_ : Binder,
) -> Pattern {
  Alias(pat~, alias_~, as_loc~, loc~)
}

///|
/// Create constant pattern
pub fn make_constant_pattern(loc~ : Location, c : Constant) -> Pattern {
  Constant(c~, loc~)
}

///|
/// Create tuple pattern
pub fn make_tuple_pattern(
  loc~ : Location,
  lparen_loc? : Location = no_location,
  rparen_loc? : Location = no_location,
  comma_locs? : @list.List[Location] = @list.empty(),
  pats : @list.List[Pattern],
) -> Pattern {
  Tuple(pats~, lparen_loc~, rparen_loc~, comma_locs~, loc~)
}

///|
/// Create constructor pattern
pub fn make_constr_pattern(
  loc~ : Location,
  args_lparen_loc? : Location? = None,
  args_rparen_loc? : Location? = None,
  arg_comma_locs? : @list.List[Location] = @list.empty(),
  open_dotdot_loc? : Location? = None,
  constr : Constructor,
  args : @list.List[ConstrPatArg]?,
  is_open : Bool,
) -> Pattern {
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
/// Create tuple type
pub fn make_tuple_type(
  loc~ : Location,
  lparen_loc? : Location = no_location,
  rparen_loc? : Location = no_location,
  comma_locs? : @list.List[Location] = @list.empty(),
  tys : @list.List[Type],
) -> Type {
  Tuple(tys~, lparen_loc~, rparen_loc~, comma_locs~, loc~)
}

///|
/// Create option type
pub fn make_option_type(
  loc~ : Location,
  constr_loc~ : Location,
  ty : Type,
) -> Type {
  Option(ty~, loc~, question_loc=constr_loc)
}

///|
/// Create field pattern
pub fn make_field_pat(
  loc~ : Location,
  colon_loc? : Location? = None,
  label : Label,
  pattern : Pattern,
  is_pun : Bool,
) -> FieldPat {
  FieldPat::{ label, pattern, is_pun, colon_loc, loc }
}

///|
/// Create unary plus expression
pub fn make_uplus(loc~ : Location, name : String, arg : Expr) -> Expr {
  match (name, arg) {
    ("+", Constant(c=Constant::Int(_), loc=_)) => arg
    ("+", Constant(c=Constant::Double(_), loc=_)) => arg
    _ => {
      let op = Var::{ name: Ident(name="~+"), loc }
      Apply(
        func=make_ident_expr(loc~, op),
        args=@list.singleton(Argument::{
          value: arg,
          kind: ArgumentKind::Positional,
        }),
        lparen_loc=no_location,
        rparen_loc=no_location,
        comma_locs=@list.empty(),
        loc~,
      )
    }
  }
}

///|
/// Create unary minus expression
pub fn make_uminus(loc~ : Location, name : String, arg : Expr) -> Expr {
  match (name, arg) {
    ("-", Constant(c=Constant::Int(x), loc=_)) => {
      let new_val = match x {
        ['-', .. num] => num.to_owned()
        _ => "-" + x
      }
      make_constant_expr(loc~, Constant::Int(new_val))
    }
    ("-", Constant(c=Constant::Int64(x), loc=_)) => {
      let new_val = match x {
        ['-', .. num] => num.to_owned()
        _ => "-" + x
      }
      make_constant_expr(loc~, Constant::Int64(new_val))
    }
    ("-", Constant(c=Constant::Double(x), loc=_)) => {
      let new_val = match x {
        ['-', .. num] => num.to_owned()
        _ => "-" + x
      }
      make_constant_expr(loc~, Constant::Double(new_val))
    }
    _ => {
      let op = Var::{ name: Ident(name~), loc }
      Unary(op~, expr=arg, loc~)
    }
  }
}

///|
/// Create logical not expression
pub fn make_unot(loc~ : Location, arg : Expr) -> Expr {
  let op = Var::{ name: Ident(name="!"), loc }
  Unary(op~, expr=arg, loc~)
}

///|
pub fn bracket_loc(index : Expr) -> Location {
  let loc = loc_of_expression(index)
  let start = shift_col(loc.start, -1)
  let end = shift_col(loc.end, 1)
  Location::{ start, end }
}

///|
/// Desugar array get operation to method call
pub fn desugar_array_get(loc~ : Location, obj : Expr, index : Expr) -> Expr {
  DotApply(
    self=obj,
    method_name=Label::{
      name: operators_table.get("_[_]").unwrap(),
      loc: bracket_loc(index),
    },
    args=@list.from_array([
      Argument::{ value: index, kind: ArgumentKind::Positional },
    ]),
    return_self=false,
    dot_loc=no_location,
    lparen_loc=no_location,
    rparen_loc=no_location,
    comma_locs=@list.empty(),
    loc~,
  )
}

///|
/// Desugar array set operation to method call
pub fn desugar_array_set(
  loc~ : Location,
  obj : Expr,
  index : Expr,
  value : Expr,
) -> Expr {
  DotApply(
    self=obj,
    method_name=Label::{
      name: operators_table.get("_[_]=_").unwrap(),
      loc: bracket_loc(index),
    },
    args=@list.from_array([
      Argument::{ value: index, kind: ArgumentKind::Positional },
      Argument::{ value, kind: ArgumentKind::Positional },
    ]),
    return_self=false,
    dot_loc=no_location,
    lparen_loc=no_location,
    rparen_loc=no_location,
    comma_locs=@list.empty(),
    loc~,
  )
}

///|
/// Desugar augmented array assignment
pub fn desugar_array_augmented_set(
  loc : Location,
  op : Var,
  array : Expr,
  index : Expr,
  value : Expr,
) -> Expr {

  // Generate unique variable names for array and index
  // Use a simple counter instead of random for reproducibility
  let arr_name = "*array_" + counter.val.to_string()
  counter.val = counter.val + 1
  let arr_loc = loc_of_expression(array)
  let arr_var = Var::{ name: Ident(name=arr_name), loc: arr_loc }
  let arr_ident = make_ident_expr(loc=arr_loc, arr_var)
  let idx_name = "*index_" + counter.val.to_string()
  counter.val = counter.val + 1
  let idx_loc = loc_of_expression(index)
  let idx_var = Var::{ name: Ident(name=idx_name), loc: idx_loc }
  let idx_ident = make_ident_expr(loc=idx_loc, idx_var)
  Let(
    pattern=Pattern::Var(Binder::{ name: arr_name, loc: no_location }),
    expr=array,
    body=Let(
      pattern=Pattern::Var(Binder::{ name: idx_name, loc: no_location }),
      expr=index,
      body=desugar_array_set(
        loc~,
        arr_ident,
        idx_ident,
        Infix(
          op~,
          lhs=desugar_array_get(loc~, arr_ident, idx_ident),
          rhs=value,
          loc~,
        ),
      ),
      let_loc=no_location,
      annot_colon_loc=None,
      equal_loc=no_location,
      semi_loc=None,
      loc~,
    ),
    let_loc=no_location,
    annot_colon_loc=None,
    equal_loc=no_location,
    semi_loc=None,
    loc~,
  )
}

///|
/// Try to create assignment expression
pub fn make_assign_opt(
  loc~ : Location,
  lhs : Expr,
  rhs : Expr,
  equal_loc? : Location = no_location,
) -> Expr? {
  match lhs {
    Ident(id~, loc=_) =>
      Some(
        Assign(
          var_=id,
          expr=rhs,
          augmented_by=None,
          equal_loc=Some(equal_loc),
          augmented_assign_loc=None,
          loc~,
        ),
      )
    Field(record~, accessor~, loc=_) =>
      Some(
        Mutate(
          record~,
          accessor~,
          field=rhs,
          augmented_by=None,
          equal_loc=None,
          augmented_assign_loc=None,
          loc~,
        ),
      )
    ArrayGet(array~, index~, lbracket_loc~, rbracket_loc~, loc=_) =>
      Some(
        ArraySet(
          array~,
          index~,
          value=rhs,
          lbracket_loc~,
          rbracket_loc~,
          equal_loc~,
          loc~,
        ),
      )
    _ => None
  }
}

///|
/// Try to create augmented assignment expression
pub fn make_augmented_assign_opt(
  loc~ : Location,
  op : Var,
  lhs : Expr,
  rhs : Expr,
  augmented_assign_loc? : Location = op.loc,
) -> Expr? {
  match lhs {
    Ident(id~, loc=_) =>
      Some(
        Assign(
          var_=id,
          expr=rhs,
          augmented_by=Some(op),
          equal_loc=None,
          augmented_assign_loc=Some(augmented_assign_loc),
          loc~,
        ),
      )
    Field(record~, accessor~, loc=_) =>
      Some(
        Mutate(
          record~,
          accessor~,
          field=rhs,
          augmented_by=Some(op),
          equal_loc=None,
          augmented_assign_loc=Some(augmented_assign_loc),
          loc~,
        ),
      )
    ArrayGet(array~, index~, lbracket_loc~, rbracket_loc~, loc=_) =>
      Some(
        ArrayAugmentedSet(
          op~,
          array~,
          index~,
          value=rhs,
          lbracket_loc~,
          rbracket_loc~,
          augmented_assign_loc~,
          loc~,
        ),
      )
    _ => None
  }
}

///|
/// Create field definition
pub fn make_field_def(
  loc~ : Location,
  colon_loc? : Location? = None,
  label : Label,
  expr : Expr,
  is_pun : Bool,
) -> FieldDef {
  FieldDef::{ label, expr, is_pun, colon_loc, loc }
}

///|
/// Convert label to expression
pub fn label_to_expr(loc~ : Location, label : Label) -> Expr {
  make_ident_expr(loc~, Var::{ name: Ident(name=label.name), loc })
}

///|
/// Convert label to pattern
pub fn label_to_pat(loc~ : Location, label : Label) -> Pattern {
  Pattern::Var(Binder::{ name: label.name, loc })
}

///|
/// Parse integer literal with suffix
pub fn make_int(i : String) -> Constant {
  if i.has_suffix("L") {
    let base = i.sub(end=i.length() - 1)
    if base.has_suffix("U") {
      let u64 = base.sub(end=base.length() - 1)
      UInt64(u64.to_owned())
    } else {
      Int64(base.to_owned())
    }
  } else if i.has_suffix("U") {
    let u32 = i.sub(end=i.length() - 1)
    UInt(u32.to_owned())
  } else if i.has_suffix("N") {
    let bigint = i.sub(end=i.length() - 1)
    BigInt(bigint.to_owned())
  } else {
    Int(i)
  }
}

///|
/// Create double constant
pub fn make_double(f : String) -> Constant {
  Constant::Double(f)
}

///|
/// Create float constant
pub fn make_float(f : String) -> Constant {
  if f.has_suffix("F") {
    let base = f.sub(end=f.length() - 1)
    Float(base.to_owned())
  } else {
    abort("make_float: string must end with F")
  }
}

///|
/// Arrow function parameter type
pub(all) enum ArrowFnParam {
  Named(Binder)
  Unnamed(Location)
}

///|
/// Convert binder to expression
pub fn binder_to_expr(p : Binder) -> Expr {
  make_ident_expr(loc=p.loc, Var::{ name: Ident(name=p.name), loc: p.loc })
}

///|
/// Convert arrow function parameter to expression
pub fn arrow_fn_param_to_expr(p : ArrowFnParam) -> Expr {
  match p {
    Named(b) => binder_to_expr(b)
    Unnamed(loc) => Hole(loc~, kind=Hole::Incomplete)
  }
}

///|
/// Create arrow function
pub fn make_arrow_fn(
  params : @list.List[(ArrowFnParam, Type?)],
  body : Expr,
  params_loc~ : Location,
  lparen_loc? : Location? = None,
  rparen_loc? : Location? = None,
  comma_locs? : @list.List[Location] = @list.empty(),
  fat_arrow_loc? : Location? = None,
  body_lbrace_loc? : Location? = None,
  body_rbrace_loc? : Location? = None,
  loc~ : Location,
) -> Func {
  let parameters = params.map(fn(p) {
    let (param, ty) = p
    match param {
      Named(binder) => Parameter::Positional(binder~, ty~, colon_loc=None)
      Unnamed(loc) => Parameter::DiscardPositional(ty~, colon_loc=None, loc~)
    }
  })
  Func::{
    parameters,
    params_loc,
    body,
    return_type: None,
    error_type: ErrorType::NoErrorType,
    kind: FnKind::Arrow,
    is_async: None,
    fn_loc: None,
    lparen_loc,
    rparen_loc,
    comma_locs,
    return_arrow_loc: None,
    fat_arrow_loc,
    body_lbrace_loc,
    body_rbrace_loc,
    loc,
  }
}

///|
pub fn Expr::loc(self : Self) -> Location {
  self |> loc_of_expression
}

///|
pub fn Type::loc(self : Type) -> Location {
  match self {
    Option(loc~, ..)
    | Name(loc~, ..)
    | Tuple(loc~, ..)
    | Arrow(loc~, ..)
    | Any(loc~)
    | Object(loc~, ..) => loc
  }
}

// Helper functions that need to be defined elsewhere

///|
pub fn loc_of_expression(expr : Expr) -> Location {
  match expr {
    Apply(loc~, ..) => loc
    Infix(op=_, lhs=_, rhs=_, loc~) => loc
    Unary(op=_, expr=_, loc~) => loc
    Array(loc~, ..) => loc
    ArraySpread(loc~, ..) => loc
    ListComprehension(loc~, ..) => loc
    ArrayGet(loc~, ..) => loc
    ArrayGetSlice(loc~, ..) => loc
    ArraySet(loc~, ..) => loc
    ArrayAugmentedSet(loc~, ..) => loc
    Constant(c=_, loc~) => loc
    MultilineString(elems=_, loc~) => loc
    Interp(elems=_, loc~) => loc
    Constraint(loc~, ..) => loc
    Constr(constr=_, loc~) => loc
    While(loc~, ..) => loc
    Function(func=_, loc~) => loc
    Ident(id=_, loc~) => loc
    If(loc~, ..) => loc
    Guard(cond=_, otherwise=_, body=_, loc~, ..) => loc
    Is(loc~, ..) => loc
    IsLexMatch(loc~, ..) => loc
    RegexMatch(loc~, ..) => loc
    Defer(loc~, ..) => loc
    LetFn(name=_, func=_, body=_, loc~, ..) => loc
    LetAnd(bindings=_, body=_, loc~, ..) => loc
    Let(pattern=_, expr=_, body=_, loc~, ..) => loc
    Sequence(exprs=_, last_expr=_, loc~, ..) => loc
    Tuple(loc~, ..) => loc
    Record(loc~, ..) => loc
    RecordUpdate(loc~, ..) => loc
    Field(record=_, accessor=_, loc~) => loc
    Method(loc~, ..) => loc
    DotApply(loc~, ..) => loc
    As(loc~, ..) => loc
    Mutate(loc~, ..) => loc
    Match(loc~, ..) => loc
    LexMatch(loc~, ..) => loc
    LetMut(binder=_, ty=_, expr=_, body=_, loc~, ..) => loc
    Pipe(loc~, ..) => loc
    RevPipe(loc~, ..) => loc
    Assign(loc~, ..) => loc
    Hole(loc~, kind=_) => loc
    Return(loc~, ..) => loc
    Raise(loc~, ..) => loc
    Quantifier(loc~, ..) => loc
    Implies(loc~, ..) => loc
    ProofAssert(expr=_, loc~, ..) => loc
    ProofLet(binder=_, expr=_, loc~, ..) => loc
    Unit(loc~, ..) => loc
    Break(loc~, ..) => loc
    Continue(loc~, ..) => loc
    For(loc~, ..) => loc
    ForEach(loc~, ..) => loc
    Try(loc~, ..) => loc
    TryOperator(loc~, ..) => loc
    Map(loc~, ..) => loc
    Group(loc~, ..) => loc
    TemplateWriting(loc~, ..) => loc
    StaticAssert(asserts=_, body=_) => no_location
  }
}

///|
pub fn Pattern::loc(self : Self) -> Location {
  match self {
    Map(loc~, ..)
    | Record(loc~, ..)
    | Var({ loc, .. })
    | Tuple(loc~, ..)
    | Or(loc~, ..)
    | Constr(loc~, ..)
    | Constraint(loc~, ..)
    | Constant(loc~, ..)
    | Array(loc~, ..)
    | Any(loc~)
    | Alias(loc~, ..)
    | Range(loc~, ..)
    | SpecialConstr(loc~, ..) => loc
  }
}

///|
pub fn Impl::loc(self : Self) -> Location {
  match self {
    TopTypeDef({ loc, .. })
    | TopFuncDef(loc~, ..)
    | TopLetDef(loc~, ..)
    | TopExpr(loc~, ..)
    | TopImplRelation(loc~, ..)
    | TopTest(loc~, ..)
    | TopTrait({ loc, .. })
    | TopView(loc~, ..)
    | TopImpl(loc~, ..)
    | TopUsing(loc~, ..) => loc
  }
}

///|
pub fn Parameter::loc(self : Self) -> Location {
  match self {
    Optional(binder~, ty=_, default~, ..) => binder.loc.merge(default.loc())
    Labelled(binder~, ty~, ..)
    | Positional(binder~, ty~, ..)
    | QuestionOptional(binder~, ty~, ..) =>
      match ty {
        None => binder.loc
        Some(ty) => binder.loc.merge(ty.loc())
      }
    DiscardPositional(ty=_, loc~, ..) => loc
  }
}

///|
pub fn AliasTarget::loc(self : AliasTarget) -> Location {
  match self.target {
    None => self.binder.loc
    Some(target) => self.binder.loc.merge(target.loc)
  }
}

///|
fn shift_col(pos : Position, offset : Int) -> Position {
  Position::{
    lnum: pos.lnum,
    fname: pos.fname,
    bol: pos.bol + offset,
    cnum: pos.cnum + offset,
  }
}
