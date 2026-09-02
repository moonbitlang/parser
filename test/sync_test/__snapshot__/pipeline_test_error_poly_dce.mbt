///|
#warnings("-fragile_catch_all")
fn h(f : (Int) -> Unit raise?) -> Unit raise? {
  f(42) catch {
    e => raise e
  }
  try {
    f(43)
    fail("hhh")
  } catch {
    _ => println("hhh")
  }
}

///|
suberror E {
  E(Int)
} derive(Debug)

///|
fn main {
  debug(to_result(() => h(x => raise E(x))))
  h(fn(x) { println(x) })
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
