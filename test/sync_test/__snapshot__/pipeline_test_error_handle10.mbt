fn f(x : UInt64) -> UInt64 raise {
  if x == 0 {
    fail("x is zero")
  } else {
    x
  }
}

fn main {
  debug(try? f(0))
  debug(try? f(1))
}
