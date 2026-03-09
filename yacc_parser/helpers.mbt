///|
priv enum LeftValue {
  Var(Var)
  Field(Expr, Accessor)
  Array(Expr, Expr)
}

///|
fn mk_loc(loc : (Position, Position)) -> Location {
  Location::{ start: loc.0, end: loc.1 }
}

///|
fn first_char_loc(loc : Location) -> Location {
  {
    start: loc.start,
    end: Position::{
      fname: loc.start.fname,
      lnum: loc.start.lnum,
      bol: loc.start.bol,
      cnum: loc.start.cnum + 1,
    },
  }
}

///|
fn last_char_loc(loc : Location) -> Location {
  {
    start: Position::{
      fname: loc.end.fname,
      lnum: loc.end.lnum,
      bol: loc.end.bol,
      cnum: loc.end.cnum - 1,
    },
    end: loc.end,
  }
}

///|
fn mk_dot_long_ident(
  pkg : String,
  pkg_loc : (Position, Position),
  id : String,
  dot_id_loc : (Position, Position),
) -> LongIdent {
  let pkg_loc = mk_loc(pkg_loc)
  let dot_id_loc = mk_loc(dot_id_loc)
  LongIdent::Dot(
    pkg~,
    pkg_loc~,
    dot_loc=first_char_loc(dot_id_loc),
    id~,
    id_loc=Location::trim_first_char(dot_id_loc),
  )
}

///|
fn mk_located_string(
  value : StringLiteral,
  loc : (Position, Position),
) -> LocatedString {
  { value, loc: mk_loc(loc) }
}

///|
fn mk_discard_parameter(
  ty : Type?,
  loc : Location,
  colon_loc : Location?,
) -> Parameter {
  DiscardPositional(ty~, loc~, colon_loc~)
}

///|
fn mk_positional_parameter(
  binder : Binder,
  ty : Type?,
  loc : Location,
  colon_loc : Location?,
) -> Parameter {
  Positional(binder~, ty~, loc~, colon_loc~)
}

///|
fn mk_labelled_parameter(
  binder : Binder,
  ty : Type?,
  loc : Location,
  label_mark_loc : Location?,
  colon_loc : Location?,
) -> Parameter {
  Labelled(binder~, ty~, loc~, label_mark_loc~, colon_loc~)
}

///|
fn mk_optional_parameter(
  binder : Binder,
  ty : Type?,
  default : Expr,
  loc : Location,
  question_loc : Location?,
  colon_loc : Location?,
  equal_loc : Location?,
) -> Parameter {
  Optional(binder~, ty~, default~, loc~, question_loc~, colon_loc~, equal_loc~)
}

///|
fn mk_question_optional_parameter(
  binder : Binder,
  ty : Type?,
  loc : Location,
  question_loc : Location?,
  colon_loc : Location?,
) -> Parameter {
  QuestionOptional(binder~, ty~, loc~, question_loc~, colon_loc~)
}

///|
fn mk_argument(value : Expr, kind : ArgumentKind, loc : Location) -> Argument {
  { value, kind, loc }
}

///|
fn mk_case(
  pattern : Pattern,
  guard_ : Expr?,
  body : Expr,
  loc : Location,
  arrow_loc : Location,
  guard_loc : Location?,
) -> Case {
  { pattern, guard_, body, loc, arrow_loc, guard_loc }
}

///|
fn mk_default_error_type(
  loc : Location,
  kind : DefaultErrorTypeKind,
) -> ErrorType {
  DefaultErrorType(loc~, kind~)
}

///|
fn mk_maybe_error(ty : Type, question_loc : Location) -> ErrorType {
  MaybeError(ty~, question_loc~)
}

///|
fn mk_record_pattern(
  fields : List[FieldPat],
  is_closed : Bool,
  loc : Location,
  lbrace_loc : Location,
  rbrace_loc : Location,
) -> Pattern {
  Record(fields~, is_closed~, loc~, lbrace_loc~, rbrace_loc~)
}

///|
fn mk_map_pattern(
  elems : List[MapPatElem],
  is_closed : Bool,
  loc : Location,
  lbrace_loc : Location,
  rbrace_loc : Location,
) -> Pattern {
  Map(elems~, is_closed~, loc~, lbrace_loc~, rbrace_loc~)
}

///|
fn mk_pattern_constraint(
  pat : Pattern,
  ty : Type,
  loc : Location,
  colon_loc : Location,
) -> Pattern {
  Constraint(pat~, ty~, loc~, colon_loc~)
}

///|
fn mk_range_pattern(
  lhs : Pattern,
  rhs : Pattern,
  kind : @syntax.RangeKind,
  loc : Location,
  op_loc : Location,
) -> Pattern {
  Range(lhs~, rhs~, kind~, loc~, op_loc~)
}

///|
fn mk_special_constr_pattern(
  binder : Binder,
  args : List[ConstrPatArg],
  loc : Location,
  lparen_loc : Location,
  rparen_loc : Location,
) -> Pattern {
  Pattern::SpecialConstr(binder~, args~, loc~, lparen_loc~, rparen_loc~)
}

///|
fn mk_dotdot_binder(binder : Binder, dotdot_loc : Location) -> DotDotBinder {
  Binder(binder, dotdot_loc~)
}

///|
fn mk_dotdot_binder_as(binder : Binder, dotdot_loc : Location) -> DotDotBinder {
  BinderAs(binder, dotdot_loc~)
}

///|
fn mk_dotdot_underscore(dotdot_loc : Location) -> DotDotBinder {
  Underscore(dotdot_loc~)
}

///|
fn mk_dotdot_no_binder(dotdot_loc : Location) -> DotDotBinder {
  NoBinder(dotdot_loc~)
}

///|
fn mk_closed_array_patterns(
  pats : List[ArrayPattern],
  loc : Location,
  lbrack_loc : Location,
  rbrack_loc : Location,
) -> ArrayPatterns {
  Closed(pats, loc~, lbrack_loc~, rbrack_loc~)
}

///|
fn mk_open_array_patterns(
  pats1 : List[ArrayPattern],
  pats2 : List[ArrayPattern],
  binder : DotDotBinder,
  loc : Location,
  lbrack_loc : Location,
  rbrack_loc : Location,
) -> ArrayPatterns {
  Open(pats1, pats2, binder, loc~, lbrack_loc~, rbrack_loc~)
}

///|
fn mk_constr_pat_arg(
  pat : Pattern,
  kind : ArgumentKind,
  loc : Location,
) -> ConstrPatArg {
  { pat, kind, loc }
}

///|
fn mk_local_type_decl(
  tycon : String,
  tycon_loc : Location,
  components : @syntax.TypeDesc,
  deriving : List[DerivingDirective],
  loc : Location,
  type_loc : Location,
) -> LocalTypeDecl {
  { tycon, tycon_loc, components, deriving, loc, type_loc }
}

///|
fn mk_decl_none(loc : Location?) -> DeclBody {
  DeclNone(loc~)
}
