///|
suberror E

///|
pub fn f(a : Int, b : Int, e : Error) -> Unit {
  match (a, b, e) {
    (1, 1, _) => ()
    (_, 0, E as a) => println(a)
    _ => ()
  }
}

///|
fn main {
  f(0, 0, E)
}

impl Show for E with output(self, logger) {
  logger.write_string("E")
}
