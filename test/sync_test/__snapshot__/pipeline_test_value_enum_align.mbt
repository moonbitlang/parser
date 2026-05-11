///|
#valtype
enum E {
  A(Byte)
  B(Byte)
}

///|
fn f(e : E) -> E {
  match e {
    A(b) => B(b + 1)
    B(b) => A(b - 1)
  }
}

///|
fn main {
  let e1 = A(1)
  let e2 = B(2)
  println(f(e1))
  println(f(e2))
}

impl Show for E with output(self, logger) {
  match self {
    A(v0) => {
      logger.write_string("A(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
    B(v0) => {
      logger.write_string("B(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
  }
}
