///|
#valtype
enum E {
  A
  B(Int)
  C(Double, Double)
}

///|
fn make_capture(e : E, i : Int) -> () -> String {
  () => e.to_string() + i.to_string()
}

///|
fn main {
  let e = E::A
  let f = make_capture(e, 1)
  println(f())
  let f2 = make_capture(E::B(1), 2)
  println(f2())
  let f3 = make_capture(E::C(1.0, 2.0), 3)
  println(f3())
}

impl Show for E with output(self, logger) {
  match self {
    A => logger.write_string("A")
    B(v0) => {
      logger.write_string("B(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
    C(v0, v1) => {
      logger.write_string("C(")
      Show::output(v0, logger)
      logger.write_string(", ")
      Show::output(v1, logger)
      logger.write_string(")")
    }
  }
}
