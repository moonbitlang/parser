///|
suberror E {
  E(Int)
} derive(Debug)

///|
fn g(x : Int, b : Bool) -> Int raise E {
  fn f() -> Int raise E {
    if b {
      raise E(x)
    } else if x < 0 {
      return x
    }
    x + 42
  }

  f()
}

///|
fn main {
  debug(to_result(() => g(-1, false)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
