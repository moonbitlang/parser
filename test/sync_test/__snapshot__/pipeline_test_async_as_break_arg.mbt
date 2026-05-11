///|
async fn[X] suspend(f : ((X) -> Unit) -> Unit) -> X noraise = "%async.suspend"

///|
fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

///|
async fn[X] some_async_fn(x : X) -> X noraise {
  println("some_async_fn()")
  suspend(k => {
    k(x)
    println("after cont")
  })
}

///|
async fn async_as_break_arg() -> Unit noraise {
  for x in 0..<5 {
    println(x)
    if x == 2 {
      break some_async_fn(())
    }
  } nobreak {
    println("nobreak")
  }
  println("after loop")
}

///|
fn init {
  run_async(async_as_break_arg)
}

///|
async fn async_as_continue_arg() -> Unit noraise {
  for x = 0; x < 5; x = x + 1 {
    println(x)
    if x == 2 {
      continue some_async_fn(4)
    }
  } nobreak {
    println("nobreak")
  }
  println("after loop")
}

///|
fn init {
  run_async(async_as_continue_arg)
}
