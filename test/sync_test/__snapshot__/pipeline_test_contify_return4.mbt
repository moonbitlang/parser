fn main {
  fn f(x: Int) -> Int raise {
    if x < 0 { fail("negative") } else { return f(f(-1)) }
    42
  }
  debug(to_result(() => f(0)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
