///|
fn run_async(f : async () -> Unit nocancel + noraise) -> Unit = "%async.run"

///|
#warnings("-unused_async")
async fn tee(b : Bool) -> Bool noraise + nocancel {
  println(b)
  b
}

///|
fn main {
  run_async <| () => {
    println("==== true && true ====")
    ignore(tee(true) && tee(true))
  }
  run_async <| () => {
    println("==== false && true ====")
    ignore(tee(false) && tee(true))
  }
  run_async <| () => {
    println("==== true || true ====")
    ignore(tee(true) || tee(true))
  }
  run_async <| () => {
    println("==== false || true ====")
    ignore(tee(false) || tee(true))
  }
}
