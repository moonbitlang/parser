//!build: -w -deprecated_syntax
///|
fn[T : Default + Debug] f(_a : T) -> Unit {
  struct U(T) derive(Default, Debug)
  let u1 = U::default()
  println(to_repr(u1))
  let u2 : U = Default::default()
  println(to_repr(u2))
}

///|
fn main {
  f(1)
  f(true)
}
