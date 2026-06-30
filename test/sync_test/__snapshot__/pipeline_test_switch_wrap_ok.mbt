///|
pub fn f(x : Int?) -> Int raise {
  guard x is Some(_) else { fail("xxx") }
  1
}

///|
fn main {
  debug(to_result(() => f(Some(42))))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
