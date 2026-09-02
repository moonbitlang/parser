///|
async fn[T] suspend(k : ((T) -> Unit) -> Unit) -> T noraise = "%async.suspend" 

///|
fn run_async(f : async () -> Unit noraise + nocancel) -> Unit = "%async.run"

///|
fn raise_cancel_signal() -> Unit raise = "%async.raise_cancellation_signal"

///|
async fn[T] capture_cancellation(
  f : async () -> T,
  handler~ : async () -> T nocancel,
) -> T nocancel = "%async.capture_cancellation"

///|
let cancellation_signal : Error = try {
  raise_cancel_signal()
} catch {
  err => err
} noraise {
  _ => panic()
}

///|
fn main {
  run_async <| () => {
    try {
      capture_cancellation(
        handler=() => println("capture_cancellation: received cancellation"),
      ) <| () => {
        try {
          suspend(k => {
            k(())
            println("after continuation")
          })
          println("after suspend")
          raise_cancel_signal()
        } catch {
          err if physical_equal(err, cancellation_signal) => {
            println("inner try: received cancellation signal")
            raise err
          }
          err => raise err
        }
      }
    } catch {
      err => println("outer try: received \{err}")
    }
  }
}
