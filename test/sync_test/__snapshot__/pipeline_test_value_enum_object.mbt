///|
#valtype
enum E {
  A
  B(Int)
  C(Double, Double)
}

///|
fn use_show(x : &Show) -> Unit {
  println(x.to_string())
}

///|
fn main {
  let e = E::A
  use_show(e)
  let e2 = E::B(1)
  use_show(e2)
  let e3 = E::C(1.0, 2.0)
  use_show(e3)
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
