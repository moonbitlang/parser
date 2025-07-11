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
} derive(Show)

///|
pub fn Location::trim_first_char(self : Location) -> Location {
  Location::{
    start: Position::{
      fname: self.start.fname,
      lnum: self.start.lnum,
      bol: self.start.bol + 1,
      cnum: self.start.cnum + 1,
    },
    end: self.end,
  }
}

///|
pub fn Location::trim_last_char(self : Location) -> Location {
  Location::{
    start: self.start,
    end: Position::{
      fname: self.end.fname,
      lnum: self.end.lnum,
      bol: self.end.bol - 1,
      cnum: self.end.cnum - 1,
    },
  }
}

///|
pub impl ToJson for Location with to_json(self) {
  Json::string(
    "\{self.start.fname}:\{self.start.lnum}:\{self.start.cnum-self.start.bol}-\{self.end.lnum}:\{self.end.cnum-self.end.bol}",
  )
}
