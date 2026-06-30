fn f(x : Int) -> String raise {
  let m = { "x": 1, "y": 2 }
  for _, _ in m {
    while true {
      match x {
        0 => return "return"
        1 => fail("error")
        _ => break
      }
    }
    break
  }
  "normal"
}

fn init {
  debug(to_result(() => f(0)))
  debug(to_result(() => f(1)))
  debug(to_result(() => f(2)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
