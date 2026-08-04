//!build: -w -deprecated_syntax
///|
fn[A : Show + Debug, B : Show] f(a : A, b : B) -> String {
  struct B {
    f1 : Array[A]
    f2 : String
  } derive(Debug)
  let b1 = { f1: [a], f2: "b1" }
  "\{a} \{b} \{Repr(b1)}"
}

///|
fn main {
  println(f("xxx", 42))
}
