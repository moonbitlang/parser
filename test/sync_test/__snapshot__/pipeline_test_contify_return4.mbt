fn main {
  fn f(x: Int) -> Int raise {
    if x < 0 { fail("negative") } else { return f(f(-1)) }
    42
  }
  debug(try? f(0))
}
