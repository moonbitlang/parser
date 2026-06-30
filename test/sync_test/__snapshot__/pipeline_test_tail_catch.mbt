///|
fn r(b : Bool) -> Int raise {
  if b {
    fail("rrr")
  }
  42
}

///|
fn f(b1 : Bool, b2 : Bool) -> Int raise {
  try {
    r(b1)
  } catch {
    _ => if b2 { fail("fff") } else { 24 }
  }
}

///|
fn main {
  debug(to_result(() => f(true, true)))
  debug(to_result(() => f(false, false)))
  debug(to_result(() => f(true, false)))
  debug(to_result(() => f(false, true)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
