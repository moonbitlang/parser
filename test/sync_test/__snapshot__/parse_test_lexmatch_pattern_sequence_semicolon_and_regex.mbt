fn main {
  lexmatch s {
    (re"^ab$" + re"cd$", after=_) => ()
    (re"^a" + re"b", after=_) => ()
    _ => ()
  }
}
