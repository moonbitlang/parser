async fn[T, E : Error] suspend(
  cb : ((T) -> Unit, (E) -> Unit) -> Unit
) -> T raise E = "%async.suspend"

fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

suberror Err derive(Debug)

async fn f(c : Bool) -> Int raise Err {
  suspend(fn (ok, err) {
    if c { ok(42) } else { err(Err) }
  })
}

fn main {
  run_async(() => debug(to_result_async(async fn () raise { f(true) })))
  run_async(() => debug(to_result_async(async fn () raise { f(false) })))
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
