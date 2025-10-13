//!build: -w -27
///|
enum E {
  T1(Int, Int)
} derive(Show)

///|
type K1 E derive(Show)

///|
fn f(self : K1) -> Unit {
  match self {
    K1(T1(0, 0)) => ()
    K1(T1(_)) as a => println(a)
  }
}

///|
fn main {
  let s = K1(T1(12, 34))
  s.f()
}
