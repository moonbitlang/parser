fn main {
  lexmatch s with longest {
    (re"^ab$", after=_) => ()
    (re"^a\\{x}", after=_) => ()
    (re"^a", after=_) => ()
    (re"^a" + re"b", after=_) => ()
    _ => ()
  }
}
