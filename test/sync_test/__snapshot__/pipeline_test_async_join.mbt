fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

// use [Ref] to prevent constant inlining
let state : Ref[Int?] = { val: None }

suberror FailAsExpected derive(Debug)

fn f() -> Unit raise {
  match state.val {
    Some(1 | 2) => ()
    _ => raise FailAsExpected
  }
}

fn init {
  run_async(fn () {
    try f() catch { err => debug(err) }
  })
}

fn join_with_non_unit_type(opt : Int??) -> Unit noraise {
  let x = {
    match opt {
      Some((None | Some(42)) as x) => x
      _ => None
    }
  }
  debug(x)
}

fn init {
  run_async(fn () { join_with_non_unit_type(Some(Some(42))) })
}
