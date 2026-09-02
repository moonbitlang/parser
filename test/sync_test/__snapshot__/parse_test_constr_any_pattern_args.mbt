// The wildcard constructor pattern takes no arguments:
// `T::_(..)` is a parse error.
fn f(x : Error) -> Unit {
  match x {
    E1::_(y) => ignore(y)
    _ => ()
  }
}
