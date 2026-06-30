fn f() -> Int raise {
    raise { return 42 }
}

fn main {
    debug(to_result(() => f()))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
