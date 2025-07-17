///|
priv enum LeftValue {
  Var(Var)
  Field(Expr, Accessor)
  Array(Expr, Expr)
}

///|
fn mk_loc(loc : (@syntax.Position, @syntax.Position)) -> Location {
  Location::{ start: loc.0, end: loc.1 }
}
