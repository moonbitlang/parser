///|
suberror Err {
  Err1
  Err2
} derive(Debug)

///|
fn error_poly_but_may_raise(flag : Bool, f : () -> Unit raise?) -> Unit raise {
  if flag {
    f()
  } else {
    raise Err1
  }
}

///|
fn init {
  debug(to_result(() => error_poly_but_may_raise(true, () => println("callback"))))
  debug(to_result(() => error_poly_but_may_raise(false, () => println("callback"))))
  debug(to_result(() => error_poly_but_may_raise(true, () => raise Err2)))
  debug(to_result(() => error_poly_but_may_raise(false, () => raise Err2)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
