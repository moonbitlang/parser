//!build: -w -57

enum H {
  A(mut i~ : Int )
  B
}

pub fn hello(x : H) -> Unit {
  if x is (A as y) {
    y.i = 200
  }
}

fn main {
  let v = A(i=2)
  hello(v)
  println(v)
}

impl Show for H with output(self, logger) {
  match self {
    A(i=v0) => {
      logger.write_string("A(")
      logger.write_string("i=")
      Show::output(v0, logger)
      logger.write_string(")")
    }
    B => logger.write_string("B")
  }
}
