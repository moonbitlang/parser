///|
#valtype
struct S {
  a : Int
  b : Int64?
  c : FixedArray[S]
  d : Bool
} derive(Debug)

///|
fn f(x : FixedArray[S]) -> Int {
  x[0].a + x[0].b.map(n => n.to_int()).unwrap_or(x[1].a)
}

///|
fn main {
  let s1 = S::{
    a: 1,
    b: Some(2),
    c: FixedArray::makei(2, i => S::{ a: i, b: None, c: [], d: true }),
    d: true,
  }
  let s2 = S::{
    a: 4,
    b: None,
    c: FixedArray::makei(2, i => S::{ a: i, b: Some(6), c: [], d: false }),
    d: false,
  }
  debug(f([s1, s2, s1]))
  debug(s1)
  debug(s2)
}
