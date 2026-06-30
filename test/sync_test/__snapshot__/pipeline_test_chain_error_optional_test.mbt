///|
fn id(f : () -> Unit raise?) -> () -> Unit raise? {
  f
}

///|
fn main {
  id(fn() { println("hello") })()
  let f = id(() => raise Failure::Failure("error")) // can not infer
  // let f : () -> Unit raise Failure = id(fn ()  { raise Failure("error")})
  // let f = id(fn () -> Unit ! Error { raise Failure("error")})
  to_result(() => f()) |> ignore
}

///|
test {
  let f : () -> Unit raise Failure = id(() => raise Failure("error"))
  to_result(() => f()) |> ignore
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
