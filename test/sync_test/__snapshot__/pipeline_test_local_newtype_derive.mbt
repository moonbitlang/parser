//!build: -w -deprecated_syntax
//|
pub fn[T : Debug] f(x : T) -> Unit {
  struct A(T) derive(Debug)
  let a = A(x)
  println(to_repr(a))
}

///|
fn main {
  f(42)
}
