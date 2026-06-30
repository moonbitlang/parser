///|
suberror E {
  E(Int)
} derive(Debug)

///|
fn e(x : Int) -> Int raise E {
  raise E(x)
}

///| test loopify
fn l(x : Int) -> Int raise {
  fn f(x : Int) -> Int raise {
    if x < 0 {
      raise E(x)
    } else if x > 1 {
      e(x) + 42
    } else {
      f(x + 1)
    }
  }

  f(x)
}

///| test contify
fn c(x : Int) -> Int raise {
  fn f(x : Int) -> Int raise {
    if x < 0 {
      raise E(x)
    } else if x > 1 {
      e(x) + 42
    } else {
      x
    }
  }

  if x is 100 {
    f(x)
  } else {
    f(x)
  }
}

///|
fn main {
  debug(to_result(() => l(42)))
  debug(to_result(() => l(0)))
  debug(to_result(() => c(42)))
  debug(to_result(() => c(0)))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
