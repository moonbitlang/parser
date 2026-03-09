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
) -> QualifiedIdent {
  let pkg_loc = mk_loc(pkg_loc)
  let dot_id_loc = mk_loc(dot_id_loc)
  Dot(
    pkg~,
    pkg_loc~,
    dot_loc=first_char_loc(dot_id_loc),
    id~,
    id_loc=Location::trim_first_char(dot_id_loc),
  )
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
fn mk_labelled_parameter(
  binder : Label,
  ty : Type?,
  loc : Location,
  label_mark_loc : Location?,
  colon_loc : Location?,
) -> Parameter {
  Labelled(
    binder={ name: binder.name, loc: binder.loc },
    ty~,
    loc~,
    label_mark_loc~,
    colon_loc~,
  )
}

///|
fn mk_question_optional_parameter(
  binder : Label,
  ty : Type?,
  loc : Location,
  question_loc : Location?,
  colon_loc : Location?,
) -> Parameter {
  QuestionOptional(
    binder={ name: binder.name, loc: binder.loc },
    ty~,
    loc~,
    question_loc~,
    colon_loc~,
  )
}

///|
fn mk_default_error_type(loc : Location) -> ErrorType {
  DefaultErrorType(loc~, kind=@syntax.DefaultErrorTypeKind::Raise)
}

///|
fn mk_maybe_error(ty : Type, question_loc : Location) -> ErrorType {
  MaybeError(ty~, question_loc~)
}
