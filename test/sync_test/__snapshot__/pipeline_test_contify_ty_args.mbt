//!build: -w -27
///|
type A[T] T

///|
fn[T : Show] A::f(self : A[T]) -> Unit {
  if self.inner().to_string() == "" {

  } else {
    A("").f()
  }
}

///|
fn main {
  A(1).f()
}
