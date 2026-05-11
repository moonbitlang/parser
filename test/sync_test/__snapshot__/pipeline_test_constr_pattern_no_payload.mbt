//!build: -w -57

///|
pub(all) enum E {
  A(Int)
  B(i~ : Int, Int)
  C(i~ : Int, j~ : Int)
}

///|
pub fn f(e : E) -> Unit {
  match e {
    A as a => println(a)
    B as b => println(b.i)
    C as c => println(c.i + c.j)
  }
}

///|
fn main {
  f(A(1))
  f(B(1, i=2))
  f(C(i=1, j=2))
}

impl Show for E with output(self, logger) {
  match self {
    A(v0) => {
      logger.write_string("A(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
    B(i=v0, v1) => {
      logger.write_string("B(")
      logger.write_string("i=")
      Show::output(v0, logger)
      logger.write_string(", ")
      Show::output(v1, logger)
      logger.write_string(")")
    }
    C(i=v0, j=v1) => {
      logger.write_string("C(")
      logger.write_string("i=")
      Show::output(v0, logger)
      logger.write_string(", ")
      logger.write_string("j=")
      Show::output(v1, logger)
      logger.write_string(")")
    }
  }
}
