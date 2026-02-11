///|
fn init {
  struct S(Int, String)
  let s = S(4, "2")
  let S(x, y) = s 
  println("\{x}\{y}")
}

///|
fn init {
  struct S(Int, String)
  let s = S(4, "2")
  match s {
    S(x, y) => println("\{x}\{y}")
  }
}
