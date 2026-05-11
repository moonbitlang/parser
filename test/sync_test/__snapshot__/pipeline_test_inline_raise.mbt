///|
suberror E {
  E(Int)
} derive(Debug)

///|
fn g(x : Int, b : Bool) -> Int raise E {
  fn f() -> Int raise E {
    if b {
      raise E(x)
    } else if x < 0 {
      return x
    }
    x + 42
  }

  f()
}

///|
fn main {
  debug(try? g(-1, false))
}
