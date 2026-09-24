///|
pub extenum E

///|
extenum E += {
  A(Int)
}

///|
fn f(e : E) -> Unit {
  match e {
    A(i) => println(i)
    _ => ()
  }
}

///|
fn main {
  f(A(42))
}
