///|
fn r(b : Bool) -> Int raise {
  if b {
    fail("rrr")
  }
  42
}

///|
fn f(b1 : Bool, b2 : Bool) -> Int raise {
  try {
    r(b1)
  } catch {
    _ => if b2 { fail("fff") } else { 24 }
  }
}

///|
fn main {
  debug(try? f(true, true))
  debug(try? f(false, false))
  debug(try? f(true, false))
  debug(try? f(false, true))
}
