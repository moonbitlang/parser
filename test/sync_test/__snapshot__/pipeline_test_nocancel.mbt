///|
async fn[T] suspend(k : ((T) -> Unit) -> Unit) -> T noraise + nocancel = "%async.suspend" 

///|
fn run_async(f : async () -> Unit noraise + nocancel) -> Unit = "%async.run"

///|
async fn raise_cancel_signal() -> Unit noraise = "%async.raise_cancellation_signal"

///|
async fn[T] capture_cancellation(
  f : async () -> T,
  handler~ : async () -> T nocancel,
) -> T nocancel = "%async.capture_cancellation"

///|
#warnings("-fragile_catch_all")
fn init {
  run_async <| () => {
    try {
      capture_cancellation(
        handler=() => println("capture_cancellation: received cancellation"),
      ) <| () => {
        try {
          defer println("`defer` triggered")
          errdefer println("`errdefer` triggered")
          suspend(k => {
            k(())
            println("after continuation")
          })
          println("after suspend")
          raise_cancel_signal()
        } catch {
          err => {
            println("inner try: received \{err}")
            raise err
          }
        }
      }
    } catch {
      err => println("outer try: received \{err}")
    }
  }
}

///|
async fn[T] capture_cancellation_noraise(
  f : async () -> T noraise,
  handler~ : async () -> T noraise + nocancel,
) -> T noraise + nocancel = "%async.capture_cancellation"

///|
fn init {
  run_async <| () => {
    capture_cancellation_noraise(
      handler=() => println("capture_cancellation: received cancellation"),
    ) <| () => {
      defer println("`defer` triggered")
      errdefer println("`errdefer` triggered")
      suspend(k => {
        k(())
        println("after continuation")
      })
      println("after suspend")
      raise_cancel_signal()
    }
  }
}
