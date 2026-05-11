pub(all) suberror FooError derive(Debug)

fn foo(r : Ref[Int], err : Bool) -> Unit raise FooError {
  if err {
    raise FooError
  }
  r.val = 1
}

fn main {
  let r = try? foo(Ref(0), false)
  debug(r)
}
