///|
suberror Err derive(Debug)

///|
fn has_err(default? : Int = { raise Err }) -> Int raise {
  default
}

///|
fn init {
  debug(to_result(() => has_err()))
  debug(to_result(() => has_err(default=42)))
}

///|
async fn some_async_fn(x : Int) -> Int {
  suspend((ok, _err) => {
    println("before invoking continuation with \{x}")
    ok(x)
    println("after invoking continuation with \{x}")
  })
}

///|
async fn has_async(
  default1? : Int = some_async_fn(1),
  default2? : Int = some_async_fn(2),
) -> Int {
  println("after evaluating default arguments")
  default1 * default2
}

///|
fn init {
  println("====")
  run_async(() => {
    debug(to_result_async(async fn () raise { has_async() }))
  })
  println("====")
  run_async(() => {
    debug(to_result_async(async fn () raise { has_async(default1=21) }))
  })
  println("====")
  run_async(() => {
    debug(to_result_async(async fn () raise { has_async(default2=42) }))
  })
  println("====")
  run_async(() => {
    debug(to_result_async(async fn () raise { has_async(default1=6, default2=7) }))
  })
}

///|
async fn[X] suspend(
  f : ((X) -> Unit, (Error) -> Unit) -> Unit,
) -> X = "%async.suspend"

///|
fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

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
