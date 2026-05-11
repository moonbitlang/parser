///|
pub fn f(x : Int?) -> Int raise {
  guard x is Some(_) else { fail("xxx") }
  1
}

///|
fn main {
  debug(try? f(Some(42)))
}
