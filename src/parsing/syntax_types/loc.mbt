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
pub fn column(self : Position) -> Int {
  self.cnum - self.bol + 1
}

///|
pub(all) struct Location {
  start : Position
  end : Position
}

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
pub impl Show for Location with output(self, buf) {
  buf
  ..write_string(self.start.fname)
  ..write_string(":")
  ..write_string(self.start.lnum.to_string())
  ..write_string("-")
  ..write_string((self.start.cnum - self.start.bol).to_string())
  ..write_string("-")
  ..write_string(self.end.lnum.to_string())
  ..write_string(":")
  ..write_string((self.end.cnum - self.end.bol).to_string())
}

///|
pub impl ToJson for Location with to_json(_) {
  Json::null()
}
