//!build: -w -67

async fn[T, E : Error] suspend(f : ((T) -> Unit, (E) -> Unit) -> Unit) -> T raise E = "%async.suspend"
fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

suberror Err derive(Debug)

fn sync_with_err() -> Unit raise Err {
  raise Err
}

async fn async_to_result() -> Unit noraise {
  debug(to_result(() => sync_with_err()))
}

fn init {
  run_async(async_to_result)
}

async fn async_with_err() -> Unit raise {
  sync_with_err()
}

fn init {
  run_async(async fn () -> Unit noraise {
    try {
      async_with_err()
    } catch {
      err => println("catched: \{Debug::to_repr(err)}")
    }
  })
}

async fn async_with_err_non_tail() -> Unit raise {
  sync_with_err()
  println("!!!")
}

fn init {
  run_async(() => {
    try {
      async_with_err_non_tail()
    } catch {
      err => println("catched: \{Debug::to_repr(err)}")
    }
  })
}

async fn async_with_try() -> Unit noraise {
  try {
    sync_with_err()
  } catch {
    err => println("catched: \{Debug::to_repr(err)}")
  }
}

fn init {
  run_async(async_with_try)
}

async fn async_with_tail_err_in_if(c : Bool) -> Unit raise Err {
  if c {
    sync_with_err()
  }
}

fn init {
  run_async(() => {
    try {
      async_with_tail_err_in_if(true)
    } catch {
      err => println("catched: \{Debug::to_repr(err)}")
    }
  })
}

trait Obj_with_err {
  fn method_with_err(Self) -> Unit raise Err
}

impl Obj_with_err for Int with method_with_err(_self) {
  raise Err
}

async fn async_with_tail_err_obj_method(obj : &Obj_with_err) -> Unit raise Err {
  obj.method_with_err()
}

fn init {
  run_async(() => {
    try {
      async_with_tail_err_obj_method(42)
    } catch {
      err => println("catched: \{Debug::to_repr(err)}")
    }
  })
}

async fn raise_in_sync_with_async_catch(input : String?) -> Unit {
  try {
    let result = match input {
      None => raise Err
      Some(input) => input
    }
    println(result)
  } catch {
    _ => async_with_err()
  }
}

fn init {
  run_async(() => try! raise_in_sync_with_async_catch(Some("msg")))
}

fn sync_with_err_and_arg(_ : Unit) -> Unit raise {
  sync_with_err()
}

async fn raise_to_async_handler_in_arg() -> Unit {
  try {
    println(sync_with_err_and_arg(sync_with_err()))
  } catch {
    _ => async_with_err()
  }
}

fn init {
  run_async(() => debug(to_result_async(async fn () raise { raise_to_async_handler_in_arg() })))
}

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
