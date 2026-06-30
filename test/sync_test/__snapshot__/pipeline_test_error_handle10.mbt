fn f(x : UInt64) -> UInt64 raise {
  if x == 0 {
    fail("x is zero")
  } else {
    x
  }
}

fn main {
  debug(to_result(() => f(0)))
  debug(to_result(() => f(1)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
