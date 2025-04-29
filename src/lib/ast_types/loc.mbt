///|
pub(all) struct Position {
  fname : String
  lnum : Int
  bol : Int
  cnum : Int
} derive(Show)

///|
fn column(self : Position) -> Int {
  self.cnum - self.bol + 1
}

///|
pub(all) struct Location {
  start : Position
  end : Position
} derive(Show)
