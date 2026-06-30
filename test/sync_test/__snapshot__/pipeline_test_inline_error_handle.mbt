suberror E {
  E(Int)
}

pub fn f(x: Int) -> Unit raise {
  fn f1(x: Int) -> _ raise {
    if x > 0 {
      raise E(x)
    } else if x < 0 {
      fail(x.to_string())
    }
    0
  }
  println(f1(x))

}

pub fn g(x: Int) -> Int raise {
  fn f1(x: Int) -> _ raise {
    if x > 0 {
      raise E(x)
    } else if x < 0 {
      fail(x.to_string())
    }
    0
  }
  f1(x)

}

fn main {
  let a = to_result(() => f(42))
  debug(a)
  let b = to_result(() => g(0))
  debug(b)
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
