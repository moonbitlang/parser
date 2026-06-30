suberror E1 derive(Debug)
suberror E2 derive(Debug)

fn f1() -> Unit raise E1 { raise E1 }
fn f2() -> Unit raise E2 { raise E2 }

fn f3() -> Unit raise {
  try {
    f1()
    f2()
  } catch {
    E1 => println("E1")
    e => raise e
  }
}

fn f4() -> Unit raise {
  try {
    f2()
    f1()
  } catch {
    E1 => println("E1")
    e => raise e
  }
}

fn main {
  debug(to_result(() => f3()))
  debug(to_result(() => f4()))
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
