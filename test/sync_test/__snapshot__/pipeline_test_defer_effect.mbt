///|
enum Action {
  Fallthrough
  Raise
  Jump
  JumpInNestedTry
} derive(Debug)

///|
async fn[T, E : Error] suspend(
  f : ((T) -> Unit, (E) -> Unit) -> Unit,
) -> T raise E = "%async.suspend"

///|
fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

///|
fn test_defer_with_error(
  action : Action,
  defer_should_raise_error~ : ArrayView[Bool],
) -> Result[Unit, Error] {
  let config_desc = [
    for i, should_raise in defer_should_raise_error => "#\{i} => \{should_raise}"
  ]
  println("===== action: \{repr(action)}, defer block should raise: \{repr(config_desc)} =====")
  fn defer_func(index : Int, is_errdefer : Bool) -> Unit raise {
    let kind = if is_errdefer { "errdefer" } else { "defer" }
    if defer_should_raise_error[index] {
      println("\{kind} block #\{index} failed")
      raise Failure("\{kind} block #\{index}")
    } else {
      println("\{kind} block #\{index} completed")
    }
  }

  try {
    defer defer_func(0, false)
    errdefer defer_func(1, true)
    defer defer_func(2, false)
    errdefer defer_func(3, true)
    match action {
      Fallthrough => ()
      Raise => raise Failure("error")
      Jump => return Ok(())
      JumpInNestedTry => {
        try {
          if true {
            return Ok(())
          } else {
            raise Failure("")
          }
        } catch {
          err => println("inner handler catched \{err}")
        }
      }
    }
  } catch {
    err => Err(err)
  } noraise {
    _ => Ok(())
  }
}

///|
enum AsyncAction {
  Suspend
  SuspendRaise
} derive(Debug)

///|
async fn test_defer_with_async(
  action : AsyncAction,
  defer_should_raise_error~ : ArrayView[Bool],
) -> Result[Unit, Error] noraise {
  let config_desc = [
    for i, should_raise in defer_should_raise_error => "#\{i} => \{should_raise}"
  ]
  println("===== action: \{repr(action)}, defer block should raise: \{repr(config_desc)} =====")
  async fn defer_func(index : Int, is_errdefer : Bool) -> Unit raise {
    suspend <| (k, err) => {
      let kind = if is_errdefer { "errdefer" } else { "defer" }
      if defer_should_raise_error[index] {
        err(Failure("\{kind} block #\{index}"))
        println("\{kind} block #\{index} failed")
      } else {
        k(())
        println("\{kind} block #\{index} completed")
      }
    }
  }

  try {
    defer defer_func(0, false)
    errdefer defer_func(1, true)
    defer defer_func(2, false)
    errdefer defer_func(3, true)
    suspend <| (k, err) => {
      match action {
        Suspend => k(())
        SuspendRaise => err(Failure("async failure"))
      }
      println("after resuming continuation")
    }
  } catch {
    err => Err(err)
  } noraise {
    _ => Ok(())
  }
}

///|
fn main {
  for config in [
    (Fallthrough, [ false, false, false, false ]), // all defer block succeed
    (Raise, [ false, false, false, false ]), // all defer block succeed
    (Raise, [ true, true, true, true ]), // all defer block fail
    (Fallthrough, [ false, true, true, false ]), // mixed setup
    (Jump, [ false, false, false, false ]), // jump should not trigger `errdefer`
    (JumpInNestedTry, [ true, true, true, true ]),
  ] {
    let (action, defer_should_raise_error) = config
    debug(test_defer_with_error(action, defer_should_raise_error~))
  }

  for config in [
    (Suspend, [false, false, false, false]),
    (SuspendRaise, [false, false, false, false]),
    (Suspend, [false, true, true, false]),
    (SuspendRaise, [true, true, true, true]),
  ] {
    let (action, defer_should_raise_error) = config
    run_async <| () => {
      debug(test_defer_with_async(action, defer_should_raise_error~))
    }
  }
}
