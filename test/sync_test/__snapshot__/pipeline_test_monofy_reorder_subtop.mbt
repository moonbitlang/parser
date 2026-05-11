///|
let b : ((Int, Int), Int) = (a, 1)

///|
fn g() -> Unit {
  let c = (a, b)
  debug(c)
}

///|
let a : (Int, Int) = (3, 4)

///|
fn main {
  g()
}
