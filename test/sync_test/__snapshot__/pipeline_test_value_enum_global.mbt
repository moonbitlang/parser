///|
#valtype
enum E {
  A
  B(Int)
  C(Double, Double)
} derive(Debug)

///|
let e1 : E = E::A

///|
let e2 : E = E::B(1)

///|
let e3 : E = E::C(1.0, 2.0)

///|
let arr3 : FixedArray[E] = FixedArray::make(5, e3)

///|
fn f(e : E) -> Unit {
  debug(e)
}

///|
fn main {
  arr3[1] = e2
  f(e1)
  debug(arr3)
}
