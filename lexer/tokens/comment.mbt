///|
pub(all) struct Comment {
  content : String
  kind : CommentKind
  mut consumed_by_docstring : Bool
} derive(Debug, Eq)

///|
pub(all) enum CommentKind {
  InlineTrailing
  Ownline(leading_blank_line~ : Bool, trailing_blank_line~ : Bool)
} derive(Debug, Eq)

///|
#deprecated
#doc(hidden)
pub extend Comment with Debug::{to_repr}

///|
#deprecated
#doc(hidden)
pub extend Comment with Eq::{not_equal, equal}

///|
#deprecated
#doc(hidden)
pub extend CommentKind with Debug::{to_repr}

///|
#deprecated
#doc(hidden)
pub extend CommentKind with Eq::{not_equal, equal}
