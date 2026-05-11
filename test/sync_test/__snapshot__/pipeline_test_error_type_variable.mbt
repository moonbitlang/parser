///|
fn[E : Error] f(x : E) -> Unit raise E {
  raise x
}

///|
fn[A, B] id(x : A) -> B = "%identity"

///|
suberror E

///|
fn main {
  f(id(E)) catch {
    e => println(e.to_string())
  }
}

impl Show for E with output(self, logger) {
  logger.write_string("E")
}
