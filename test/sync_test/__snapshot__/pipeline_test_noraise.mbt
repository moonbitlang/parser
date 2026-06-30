fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

///|
fn ff(a : async () -> Int raise) -> Unit {
  run_async(() => debug(to_result_async(async fn () raise { a() })))
}

///|
#warnings("-unused_async") 
fn main {
  ff(async fn() noraise { 42 })
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}

async fn[T, E : Error] to_result_async(
  f : async () -> T raise E
) -> Result[T, E] noraise {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
