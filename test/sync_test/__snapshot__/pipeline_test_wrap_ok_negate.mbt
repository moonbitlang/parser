///|
fn is_file(s : String) -> Bool raise Failure {
  if s == "file" {
    true
  } else {
    raise Failure("not a file")
  }
}

///|
fn h(f : (String) -> Bool raise?, x : String) -> Bool raise? {
  f(x)
}

///|
fn main {
  let x = to_result(() => h(x => !is_file(x), "cmp"))
  debug(x)
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
