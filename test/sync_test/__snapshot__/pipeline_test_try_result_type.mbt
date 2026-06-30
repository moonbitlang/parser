fn f(c : Bool) ->  Int64 raise {
  if c {
    0L
  } else {
    fail("1")
  }
}

fn g(c : Bool) -> Unit raise {
  try {
    f(c)
  } catch {
    _ => ()
  } noraise {
    _ => fail("!")
  }
}

fn init {
  debug(to_result(() => g(true)))
  debug(to_result(() => g(false)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
