//!build: -w -27
///|
trait I {
  obj_meth(Self) -> Int
}

///|
typealias &I as Obj

///|
fnalias Obj::obj_meth

///|
type MyInt Int

///|
impl I for MyInt with obj_meth(s) {
  s.inner() * 1000
}

///|
fn main {
  obj_meth(MyInt(1)) |> println
  let _ : (&I) -> Int = obj_meth
}
