pub(all) suberror FooError derive(Debug)

fn foo(r : Ref[Int], err : Bool) -> Unit raise FooError {
  if err {
    raise FooError
  }
  r.val = 1
}

fn main {
  let r = to_result(() => foo(Ref(0), false))
  debug(r)
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
