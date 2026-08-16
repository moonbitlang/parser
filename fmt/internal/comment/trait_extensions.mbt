///|
pub extend Comment with Debug::{to_repr}

///|
pub extend Comment with @moonbit-community/prettyprinter.Pretty::{pretty}

///|
pub extend CommentKind with Debug::{to_repr}

///|
pub extend CommentKind with @moonbit-community/prettyprinter.Pretty::{pretty}

///|
pub extend NodeArray with @moonbit-community/prettyprinter.Pretty::{pretty}

///|
pub extend NodeTable with @moonbit-community/prettyprinter.Pretty::{pretty}

///|
pub extend TokenGroup with @moonbit-community/prettyprinter.Pretty::{pretty}

///|
pub extend Key with Eq::{not_equal, equal}

///|
pub extend Key with Hash::{hash, hash_combine}

///|
pub extend Key with Compare::{op_lt, op_le, op_ge, compare, op_gt}

///|
pub extend Key with @moonbit-community/prettyprinter.Pretty::{pretty}

///|
pub extend NodeTableArray with @moonbit-community/prettyprinter.Pretty::{pretty}
