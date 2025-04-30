///|
pub(all) struct Position {
  fname : String
  lnum : Int
  bol : Int
  cnum : Int
} derive(Show, ToJson)

///|
pub impl Eq for Position with op_equal(self, other) {
  self.fname == other.fname && self.cnum == other.cnum
}

///|
fn column(self : Position) -> Int {
  self.cnum - self.bol + 1
}

///|
pub(all) struct Location {
  start : Position
  end : Position
} derive(Show, ToJson)
