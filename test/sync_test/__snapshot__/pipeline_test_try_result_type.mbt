fn f(c : Bool) ->  Int64 raise {
  if c {
    0L
  } else {
    fail("1")
  }
}

fn g(c : Bool) -> Unit raise {
  try {
    f(c)
  } catch {
    _ => ()
  } noraise {
    _ => fail("!")
  }
}

fn init {
  debug(try? g(true))
  debug(try? g(false))
}
