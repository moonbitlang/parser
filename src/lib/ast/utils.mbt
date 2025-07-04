///| MoonBit parsing utilities - translated from OCaml parsing_util.ml
// Translated from src/lib/ast/parsing_util.ml

// Global counter for generating unique variable names
let counter : Ref[Int] = Ref::new(0)

/// Create constant expression with location
pub fn make_constant_expr(loc : Location, c : Constant) -> Expr {
  Constant(c=c, loc=loc)
}

/// Create identifier expression
pub fn make_ident_expr(loc : Location, id : Var) -> Expr {
  Ident(id=id, loc=loc)
}

/// Create tuple expression
pub fn make_tuple_expr(loc : Location, exprs : @immut/list.T[Expr]) -> Expr {
  Tuple(exprs=exprs, loc=loc)
}

/// Create array expression with possible spread elements
pub fn make_array_expr(loc : Location, elems : @immut/list.T[SpreadableElem]) -> Expr {
  // Try to convert to regular array if no spread elements
  let mut has_spread = false
  let mut regular_exprs : @immut/list.T[Expr] = @immut/list.Nil
  @immut/list.each(elems, fn(elem) {
    match elem {
      ElemRegular(e) => 
        if not(has_spread) {
          regular_exprs = @immut/list.Cons(e, regular_exprs)
        }
      ElemSpread(_) => has_spread = true
    }
  })
  
  if has_spread {
    ArraySpread(elems=elems, loc=loc)
  } else {
    Array_(exprs=regular_exprs, loc=loc)
  }
}

/// Create interpolation elements from literal components
pub fn make_interps(components : Array[InterpolationComponent]) -> @immut/list.T[InterpElem] {
  let result = components.map(fn(lit) {
    match lit {
      InterpolationComponent::InterpLit(repr, loc) => InterpElem::InterpLit(repr=repr, loc=loc)
      InterpolationComponent::InterpSource(s) => InterpElem::InterpSource(s)
    }
  })
  @immut/list.from_array(result)
}

/// Create interpolation expression
pub fn make_interp_expr(loc : Location, components : Array[InterpolationComponent]) -> Expr {
  let elems = make_interps(components)
  Interp(elems=elems, loc=loc)
}

/// Create record expression
pub fn make_record_expr(
  loc : Location,
  trailing : TrailingMark,
  type_name : QualifiedName?,
  fields : @immut/list.T[FieldDef]
) -> Expr {
  Record(type_name=type_name, fields=fields, trailing=trailing, loc=loc)
}

/// Create alias pattern
pub fn make_alias_pattern(loc : Location, pat : Pattern, alias : Binder) -> Pattern {
  Alias(pat=pat, alias_=alias, loc=loc)
}

/// Create constant pattern
pub fn make_constant_pattern(loc : Location, c : Constant) -> Pattern {
  Constant(c=c, loc=loc)
}

/// Create tuple pattern
pub fn make_tuple_pattern(loc : Location, pats : @immut/list.T[Pattern]) -> Pattern {
  Tuple(pats=pats, loc=loc)
}

/// Create constructor pattern
pub fn make_constr_pattern(
  loc : Location,
  constr : Constructor,
  args : @immut/list.T[ConstrPatArg]?,
  is_open : Bool
) -> Pattern {
  Constr(constr=constr, args=args, is_open=is_open, loc=loc)
}

/// Create tuple type
pub fn make_tuple_type(loc : Location, tys : @immut/list.T[Type]) -> Type {
  TTuple(tys, loc=loc)
}

/// Create option type
pub fn make_option_type(loc : Location, constr_loc : Location, ty : Type) -> Type {
  TOption(ty, loc=loc, question_loc=constr_loc)
}

/// Create field pattern
pub fn make_field_pat(
  loc : Location,
  label : Name,
  pattern : Pattern,
  is_pun : Bool
) -> FieldPat {
  FieldPat::{ label, pattern, is_pun, loc }
}

/// Create unary plus expression  
pub fn make_uplus(loc : Location, name : String, arg : Expr) -> Expr {
  match (name, arg) {
    ("+", Constant(c=Constant::Int(_), loc=_)) => arg
    ("+", Constant(c=Constant::Double(_), loc=_)) => arg 
    _ => {
      let op = Var::{ 
        name: QualifiedName::{ pkg: None, name: "~+", loc }, 
        loc 
      }
      Apply(
        func=make_ident_expr(loc, op),
        args=@immut/list.from_array([Argument::{ value: arg, kind: ArgumentKind::Positional }]),
        attr=ApplyAttr::NoAttr,
        loc=loc
      )
    }
  }
}

/// Create unary minus expression
pub fn make_uminus(loc : Location, name : String, arg : Expr) -> Expr {
  match (name, arg) {
    ("-", Constant(c=Constant::Int(x), loc=_)) => {
      let new_val = if x.length() > 0 && x[0] == '-' {
        x.substring(start=1)
      } else {
        "-" + x
      }
      make_constant_expr(loc, Constant::Int(new_val))
    }
    ("-", Constant(c=Constant::Int64(x), loc=_)) => {
      let new_val = if x.length() > 0 && x[0] == '-' {
        x.substring(start=1) 
      } else {
        "-" + x
      }
      make_constant_expr(loc, Constant::Int64(new_val))
    }
    ("-", Constant(c=Constant::Double(x), loc=_)) => {
      let new_val = if x.length() > 0 && x[0] == '-' {
        x.substring(start=1)
      } else {
        "-" + x
      }
      make_constant_expr(loc, Constant::Double(new_val))
    }
    _ => {
      let op = Var::{ 
        name: QualifiedName::{ pkg: None, name, loc }, 
        loc 
      }
      Unary(op=op, expr=arg, loc=loc)
    }
  }
}

/// Create logical not expression
pub fn make_unot(loc : Location, arg : Expr) -> Expr {
  let op = Var::{ 
    name: QualifiedName::{ pkg: None, name: "!", loc }, 
    loc 
  }
  Unary(op=op, expr=arg, loc=loc)
}

/// Get bracket location for array access
pub fn bracket_loc(index : Expr) -> Location {
  let loc = loc_of_expression(index)
  let start = shift_col(loc.start, -1)
  let end = shift_col(loc.end, 1)
  Location::{ start, end }
}

/// Desugar array get operation to method call
pub fn desugar_array_get(loc : Location, obj : Expr, index : Expr) -> Expr {
  DotApply(
    self=obj,
    method_name=Name::{ name: "get", loc: bracket_loc(index) },
    args=@immut/list.from_array([Argument::{ value: index, kind: ArgumentKind::Positional }]),
    return_self=false,
    attr=ApplyAttr::NoAttr,
    loc=loc
  )
}

/// Desugar array set operation to method call  
pub fn desugar_array_set(
  loc : Location, 
  obj : Expr, 
  index : Expr, 
  value : Expr
) -> Expr {
  DotApply(
    self=obj,
    method_name=Name::{ name: "set", loc: bracket_loc(index) },
    args=@immut/list.from_array([
      Argument::{ value: index, kind: ArgumentKind::Positional },
      Argument::{ value: value, kind: ArgumentKind::Positional }
    ]),
    return_self=false,
    attr=ApplyAttr::NoAttr,
    loc=loc
  )
}

/// Desugar augmented array assignment
pub fn desugar_array_augmented_set(
  loc : Location,
  op : Var,
  array : Expr,
  index : Expr,
  value : Expr
) -> Expr {
  // Generate unique variable names for array and index
  // Use a simple counter instead of random for reproducibility
  let arr_name = "*array_" + counter.val.to_string()
  counter.val = counter.val + 1
  let arr_loc = loc_of_expression(array)
  let arr_var = Var::{ 
    name: QualifiedName::{ pkg: None, name: arr_name, loc: arr_loc }, 
    loc: arr_loc 
  }
  let arr_ident = make_ident_expr(arr_loc, arr_var)
  
  let idx_name = "*index_" + counter.val.to_string()
  counter.val = counter.val + 1
  let idx_loc = loc_of_expression(index)
  let idx_var = Var::{ 
    name: QualifiedName::{ pkg: None, name: idx_name, loc: idx_loc }, 
    loc: idx_loc 
  }
  let idx_ident = make_ident_expr(idx_loc, idx_var)
  
  Let(
    pattern=Pattern::Var(Binder::{ name: arr_name, loc: no_location }),
    expr=array,
    body=Let(
      pattern=Pattern::Var(Binder::{ name: idx_name, loc: no_location }),
      expr=index,
      body=desugar_array_set(
        loc,
        arr_ident,
        idx_ident,
        Infix(
          op=op,
          lhs=desugar_array_get(loc, arr_ident, idx_ident),
          rhs=value,
          loc=loc
        )
      ),
      loc=loc
    ),
    loc=loc
  )
}

/// Try to create assignment expression
pub fn make_assign_opt(loc : Location, lhs : Expr, rhs : Expr) -> Expr? {
  match lhs {
    Ident(id=id, loc=_) => 
      Some(Assign(var_=id, expr=rhs, augmented_by=None, loc=loc))
    Field(record=record, accessor=accessor, loc=_) => 
      Some(Mutate(record=record, accessor=accessor, field=rhs, augmented_by=None, loc=loc))
    ArrayGet(array=array, index=index, loc=_) => 
      Some(ArraySet(array=array, index=index, value=rhs, loc=loc))
    _ => None
  }
}

/// Try to create augmented assignment expression
pub fn make_augmented_assign_opt(
  loc : Location,
  op : Var,
  lhs : Expr,
  rhs : Expr
) -> Expr? {
  match lhs {
    Ident(id=id, loc=_) => 
      Some(Assign(var_=id, expr=rhs, augmented_by=Some(op), loc=loc))
    Field(record=record, accessor=accessor, loc=_) => 
      Some(Mutate(record=record, accessor=accessor, field=rhs, augmented_by=Some(op), loc=loc))
    ArrayGet(array=array, index=index, loc=_) => 
      Some(ArrayAugmentedSet(op=op, array=array, index=index, value=rhs, loc=loc))
    _ => None
  }
}

/// Create field definition
pub fn make_field_def(
  loc : Location,
  label : Name,
  expr : Expr,
  is_pun : Bool
) -> FieldDef {
  FieldDef::{ label, expr, is_pun, loc }
}

/// Convert label to expression
pub fn label_to_expr(loc : Location, label : Name) -> Expr {
  make_ident_expr(loc, Var::{ 
    name: QualifiedName::{ pkg: None, name: label.name, loc }, 
    loc 
  })
}

/// Convert label to pattern
pub fn label_to_pat(loc : Location, label : Name) -> Pattern {
  Pattern::Var(Binder::{ name: label.name, loc })
}

/// Parse integer literal with suffix
pub fn make_int(i : String) -> Constant {
  if i.has_suffix("L") {
    let base = i.substring(end=(i.length() - 1))
    if base.has_suffix("U") {
      let u64 = base.substring(end=(base.length() - 1))
      Constant::UInt64(u64)
    } else {
      Constant::Int64(base)
    }
  } else if i.has_suffix("U") {
    let u32 = i.substring(end=(i.length() - 1))
    Constant::UInt(u32)
  } else if i.has_suffix("N") {
    let bigint = i.substring(end=(i.length() - 1))
    Constant::BigInt(bigint)
  } else {
    Constant::Int(i)
  }
}

/// Create double constant
pub fn make_double(f : String) -> Constant {
  Constant::Double(f)
}

/// Create float constant 
pub fn make_float(f : String) -> Constant {
  if f.has_suffix("F") {
    let base = f.substring(end=(f.length() - 1))
    Constant::Float(base)
  } else {
    abort("make_float: string must end with F")
  }
}

/// Create legacy type alias
pub fn make_legacy_type_alias(
  binder_expr : Type,
  target : Type,
  attrs : @immut/list.T[String],
  vis : Visibility,
  deriving : @immut/list.T[DerivingDirective],
  loc : Location
) -> TypeDecl? {
  match binder_expr {
    TName(constr_id, tys) => {
      let tycon = constr_id.name
      let tycon_loc = constr_id.loc
      // Check if all type parameters are simple names
      let params_valid = @immut/list.all(tys, fn(ty) {
        match ty {
          TName(qname, @immut/list.Nil) => match qname.pkg {
            None => true
            Some(_) => false
          }
          _ => false
        }
      })
      if params_valid {
        let params = @immut/list.map(tys, fn(ty) {
          match ty {
            TName(qname, _) => 
              TypeDeclBinder::{ name: Some(qname.name), loc: qname.loc }
            _ => abort("Invalid type parameter")
          }
        })
        Some(TypeDecl::{
          tycon,
          tycon_loc,
          params,
          components: TypeDesc::Alias(target),
          attrs,
          type_vis: vis,
          deriving,
          loc,
          deprecated_type_bang: false
        })
      } else {
        None
      }
    }
    _ => None
  }
}

/// Arrow function parameter type
pub enum ArrowFnParam {
  Named(Binder)
  Unnamed(Location)
}

/// Convert binder to expression
pub fn binder_to_expr(p : Binder) -> Expr {
  make_ident_expr(
    p.loc,
    Var::{ name: QualifiedName::{ pkg: None, name: p.name, loc: p.loc }, loc: p.loc }
  )
}

/// Convert arrow function parameter to expression
pub fn arrow_fn_param_to_expr(p : ArrowFnParam) -> Expr {
  match p {
    Named(b) => binder_to_expr(b)
    Unnamed(loc) => Hole_(loc=loc, kind=Hole::Incomplete)
  }
}

/// Create arrow function
pub fn make_arrow_fn(
  params : @immut/list.T[(ArrowFnParam, Type?)],
  body : Expr,
  loc : Location
) -> Func {
  let parameters = @immut/list.map(params, fn(p) {
    let (param, ty) = p
    match param {
      Named(binder) => Parameter::Positional(binder=binder, ty=ty)
      Unnamed(loc) => Parameter::DiscardPositional(ty=ty, loc=loc)
    }
  })
  Func::Lambda(
    parameters=parameters,
    params_loc=loc,
    body=body,
    return_type=None,
    error_type=ErrorType::NoErrorType,
    kind=FnKind::Arrow,
    has_error=None,
    is_async=false,
    loc=loc
  )
}

// Helper functions that need to be defined elsewhere
fn loc_of_expression(expr : Expr) -> Location {
  match expr {
    Apply(func=_, args=_, attr=_, loc=loc) => loc
    Infix(op=_, lhs=_, rhs=_, loc=loc) => loc  
    Unary(op=_, expr=_, loc=loc) => loc
    Array_(exprs=_, loc=loc) => loc
    ArraySpread(elems=_, loc=loc) => loc
    ArrayGet(array=_, index=_, loc=loc) => loc
    ArrayGetSlice(array=_, start_index=_, end_index=_, index_loc=_, loc=loc) => loc
    ArraySet(array=_, index=_, value=_, loc=loc) => loc
    ArrayAugmentedSet(op=_, array=_, index=_, value=_, loc=loc) => loc
    Constant(c=_, loc=loc) => loc
    MultilineString(elems=_, loc=loc) => loc
    Interp(elems=_, loc=loc) => loc
    Constraint(expr=_, ty=_, loc=loc) => loc
    Constr(constr=_, loc=loc) => loc
    While(loop_cond=_, loop_body=_, while_else=_, label=_, loc=loc) => loc
    Function_(func=_, loc=loc) => loc
    Ident(id=_, loc=loc) => loc
    If(cond=_, ifso=_, ifnot=_, loc=loc) => loc
    Guard(cond=_, otherwise=_, body=_, loc=loc) => loc
    Is(expr=_, pat=_, loc=loc) => loc
    LetFn(name=_, func=_, body=_, loc=loc) => loc
    LetRec(bindings=_, body=_, loc=loc) => loc
    LetAnd(bindings=_, body=_, loc=loc) => loc
    Let(pattern=_, expr=_, body=_, loc=loc) => loc
    Sequence(exprs=_, last_expr=_, loc=loc) => loc
    Tuple(exprs=_, loc=loc) => loc
    Record(type_name=_, fields=_, trailing=_, loc=loc) => loc
    RecordUpdate(type_name=_, record=_, fields=_, loc=loc) => loc
    Field(record=_, accessor=_, loc=loc) => loc
    Method(type_name=_, method_name=_, loc=loc) => loc
    DotApply(self=_, method_name=_, args=_, return_self=_, attr=_, loc=loc) => loc
    As(expr=_, trait_=_, loc=loc) => loc
    Mutate(record=_, accessor=_, field=_, augmented_by=_, loc=loc) => loc
    Match(expr=_, cases=_, match_loc=_, using_=_, loc=loc) => loc
    LetMut(binder=_, ty=_, expr=_, body=_, loc=loc) => loc
    Pipe(lhs=_, rhs=_, loc=loc) => loc
    Assign(var_=_, expr=_, augmented_by=_, loc=loc) => loc
    Hole_(loc=loc, kind=_) => loc
    Return(return_value=_, loc=loc) => loc
    Raise(err_value=_, loc=loc) => loc
    Unit_(loc=loc, faked=_) => loc
    Break(arg=_, label=_, loc=loc) => loc
    Continue(args=_, label=_, loc=loc) => loc
    Loop(args=_, body=_, label=_, loop_loc=_, loc=loc) => loc
    For(binders=_, condition=_, continue_block=_, body=_, for_else=_, label=_, loc=loc) => loc
    ForEach(binders=_, expr=_, body=_, else_block=_, label=_, loc=loc) => loc
    Try(body=_, catch_=_, catch_all=_, try_else=_, has_try=_, legacy_else=_, try_loc=_, catch_loc=_, else_loc=_, loc=loc) => loc
    TryQuestion(body=_, try_loc=_, loc=loc) => loc
    Map(elems=_, loc=loc) => loc
    Group(expr=_, group=_, loc=loc) => loc
    StaticAssert(asserts=_, body=_) => no_location
  }
}

fn shift_col(pos : Position, offset : Int) -> Position {
  Position::{
    lnum: pos.lnum,
    fname: pos.fname,
    bol: pos.bol, 
    cnum: pos.cnum + offset,
  }
}

// Placeholder types that need to be defined
pub enum InterpolationComponent {
  InterpLit(StringLiteral, Location)
  InterpSource(@ast_types.InterpSource)
}