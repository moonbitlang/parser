suberror StringError {
  StringError(String)
} derive(Debug)

fn f(x: Int) -> Int raise StringError {
  if x < 0 {
    raise StringError("error")
  }
  x
}

fn init {
  let a = to_result(() => f(42))
  debug(a)
  let b = to_result(() => f(-42))
  debug(b)
}

fn g(x: Int) -> Option[Int] raise StringError {
  if x < 0 {
    raise StringError("error")
  }
  Some(x)
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
