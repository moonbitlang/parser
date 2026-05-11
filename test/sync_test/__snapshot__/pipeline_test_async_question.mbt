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
  run_async(() => debug(try? f(true)))
  run_async(() => debug(try? f(false)))
}
