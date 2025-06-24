///|
fn mk_loc(loc : (@ast.Position, @ast.Position)) -> Location {
  Location::{ start: loc.0, end: loc.1 }
}
