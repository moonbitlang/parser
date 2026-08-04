//!build: -w -deprecated_syntax
///|
pub fn[T : Debug] f(x : T) -> Unit {
  struct A(T) derive(Debug)
  struct B(Int, A) derive(Debug)
  let a = A(x)
  println(Repr(a))
  let b = B(1, a)
  println(Repr(b))
}

///|
fn main {
  f(42)
}
