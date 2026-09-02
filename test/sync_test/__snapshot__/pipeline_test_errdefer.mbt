///|
enum Action {
  Fallthrough
  Raise
  Return
  Break
  Continue
  Labelled_break
  Labelled_continue
} derive(Debug)

///|
fn test_errdefer(action : Action) -> Unit raise {
  let mut errdefer_triggered = false
  defer (if errdefer_triggered {
    println("`errdefer` triggered for \{repr(action)}")
  } else {
    println("`errdefer` not triggered for \{repr(action)}")
  })
  label~: for i in 0..<1 {
    for j = 0; j < 1; j = j + 1 {
      errdefer {
        errdefer_triggered = true
      }
      match action {
        Fallthrough => ()
        Raise => raise Failure::Failure("msg")
        Return => return
        Break => break
        Continue => continue
        Labelled_break => break label~
        Labelled_continue => continue label~
      }
    }
  }
}

///|
fn main {
  test_errdefer(Fallthrough) catch { _ => () }
  test_errdefer(Raise) catch { _ => () }
  test_errdefer(Return) catch { _ => () }
  test_errdefer(Break) catch { _ => () }
  test_errdefer(Continue) catch { _ => () }
  test_errdefer(Labelled_break) catch { _ => () }
  test_errdefer(Labelled_continue) catch { _ => () }
}
