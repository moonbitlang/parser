suberror StringError {
  StringError(String)
} derive(Debug)

fn f() -> Unit raise StringError {
  raise StringError("xxx")
}

fn init {
  let tests : Array[() -> Unit raise StringError] = [f]
  for k = 0; k < tests.length(); k = k + 1 {
    let f = tests[k]
    let result = to_result(() => f())
    debug(result)
  }
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
