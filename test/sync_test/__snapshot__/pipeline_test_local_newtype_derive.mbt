//!build: -w -27
///|
pub fn[T : Show] f(x : T) -> Unit {
  type A T derive(Show)
  let a = A(x)
  println(a)
}

///|
fn main {
  f(42)
}
