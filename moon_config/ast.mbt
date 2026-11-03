///|
/// Configuration syntax is preserved for validation, formatting and editing.
#derive(Debug, Eq)
pub(all) struct Ast(Vector[Statement])

///|
#derive(Debug, Eq)
pub(all) enum Expr {
  Bool(value~ : Bool, loc~ : Location)
  Int(repr~ : String, loc~ : Location)
  String(str~ : String, loc~ : Location)
  Array(items~ : Array[Expr], loc~ : Location)
  Map(fields~ : Array[Field], loc~ : Location)
  Apply(
    name~ : String,
    name_loc~ : Location,
    args~ : Array[Field],
    args_loc~ : Location,
    loc~ : Location
  )
  Hole(Location)
}

///|
#derive(Debug, Eq)
pub(all) struct Field {
  key : String
  key_loc : Location
  value : Expr
}

///|
#derive(Debug, Eq)
pub(all) enum ImportKind {
  Normal
  Test
  Wbtest
}

///|
#derive(Debug, Eq)
pub(all) struct ImportItem {
  path : String
  path_loc : Location
  alias_ : (String, Location)?
  loc : Location
}

///|
#derive(Debug, Eq)
pub(all) enum Statement {
  Import(
    items~ : Array[ImportItem],
    kind~ : ImportKind,
    keyword_loc~ : Location,
    list_loc~ : Location,
    loc~ : Location
  )
  Assign(Field)
  Expr(Expr)
}

///|
pub fn Expr::loc(self : Self) -> Location {
  match self {
    Bool(loc~, ..)
    | Int(loc~, ..)
    | String(loc~, ..)
    | Array(loc~, ..)
    | Map(loc~, ..)
    | Apply(loc~, ..)
    | Hole(loc) => loc
  }
}

///|
pub fn find_field(fields : ArrayView[Field], key : String) -> Expr? {
  let mut found = None
  for field in fields {
    if field.key == key {
      found = Some(field.value)
    }
  }
  found
}

///|
pub fn find_statements(ast : Ast, key : String) -> Ast {
  Ast(
    ast.0.filter(statement => {
      match statement {
        Assign({ key: name, .. }) | Expr(Apply(name~, ..)) => name == key
        Import(_) => key == "import"
        _ => false
      }
    }),
  )
}

///|
pub fn find_field_in_options(ast : Ast, key : String) -> Expr? {
  let mut found = None
  for statement in ast.0 {
    if statement is Expr(Apply(name="options", args~, ..)) {
      if find_field(args, key) is Some(value) {
        found = Some(value)
      }
    }
  }
  found
}

///|
pub extend Ast with Debug::{to_repr}

///|
pub extend Ast with Eq::{equal, not_equal}

///|
pub extend Expr with Debug::{to_repr}

///|
pub extend Expr with Eq::{equal, not_equal}

///|
pub extend Field with Debug::{to_repr}

///|
pub extend Field with Eq::{equal, not_equal}

///|
pub extend ImportKind with Debug::{to_repr}

///|
pub extend ImportKind with Eq::{equal, not_equal}

///|
pub extend ImportItem with Debug::{to_repr}

///|
pub extend ImportItem with Eq::{equal, not_equal}

///|
pub extend Statement with Debug::{to_repr}

///|
pub extend Statement with Eq::{equal, not_equal}
