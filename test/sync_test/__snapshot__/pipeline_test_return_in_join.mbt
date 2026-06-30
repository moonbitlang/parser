fn f(x : Int) -> String raise Error {
  if x == 42 {
    fail("!!")
  }
  x.to_string()
}

fn g(x : Int) -> String raise Error {
  match x {
    0 | 1 => return f(0)
    2 | 3 => return f(1)
    x => x.to_string()
  }
}

fn init {
  debug(to_result(() => g(42)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
