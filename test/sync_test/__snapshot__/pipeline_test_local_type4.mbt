//!build: -w -deprecated_syntax
fn main {
  struct S {
    flag : Bool
    y : Int
  } derive(Debug)
  let s = { y: 1, flag: true }
  println(to_repr(s))
}
