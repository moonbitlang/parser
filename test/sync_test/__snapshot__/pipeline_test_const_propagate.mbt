//!build: -w -27
///|
type S String

///|
fn main {
  let a = S("xxx")
  println(a.inner())
  println(a0.inner())
}

///|
type MyInt Int

///|
let a0 : MyInt = MyInt(1)

///|
let f1 : Float = 3.14

///|
let f2 : Double = 3.14

///|
fn init {
  println(f1.to_double() + f2)
}
