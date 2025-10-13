//!build: -w -27
///|
pub(all) type T Int

///|
pub(open) trait I {
  f(Self) -> Int
}

///|
pub fn T::g(self : T) -> Int {
  enum E {
    A(Int)
  } derive(Show)
  println(A(self.inner()))
  self.inner()
}

///|
pub impl I for T with f(self) {
  type N Int derive(Show)
  println(N(self.inner()))
  self.inner()
}

///|
fn main {
  ignore(T::g(T(42)))
  ignore(I::f(T(42)))
}
