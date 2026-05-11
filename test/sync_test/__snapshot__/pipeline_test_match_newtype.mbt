///|
enum E {
  T1(Int, Int)
}

///|
struct K1(E)

///|
fn K1::f(self : K1) -> Unit {
  match self {
    K1(T1(0, 0)) => ()
    K1(T1(_)) as a => println(a)
  }
}

///|
fn main {
  let s = K1(T1(12, 34))
  s.f()
}

impl Show for E with output(self, logger) {
  match self {
    T1(v0, v1) => {
      logger.write_string("T1(")
      Show::output(v0, logger)
      logger.write_string(", ")
      Show::output(v1, logger)
      logger.write_string(")")
    }
  }
}

impl Show for K1 with output(self, logger) {
  logger.write_string("K1(")
  Show::output(self.0, logger)
  logger.write_string(")")
}
