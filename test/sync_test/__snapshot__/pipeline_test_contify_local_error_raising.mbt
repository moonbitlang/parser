///|
fn main {
  pipe_failed()
}

///|
suberror MyErr {
  MyErr(String)
} derive(Debug)

///|
fn pipe_failed() -> Unit {
  debug(to_result(() => (x => raise MyErr(x))("msg")))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
