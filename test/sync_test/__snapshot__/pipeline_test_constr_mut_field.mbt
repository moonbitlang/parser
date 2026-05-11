enum E {
  C(mut x~ : Int, mut y~ : Int)
}

fn swap_xy(x : E) -> Unit {
  match x {
    C(y~, ..) as c => {
      c.y = c.x
      c.x = y
    }
  }
}

fn init {
  let e : E = C(x=1, y=2)
  println(e)
  swap_xy(e)
  println(e)
}

impl Show for E with output(self, logger) {
  match self {
    C(x=v0, y=v1) => {
      logger.write_string("C(")
      logger.write_string("x=")
      Show::output(v0, logger)
      logger.write_string(", ")
      logger.write_string("y=")
      Show::output(v1, logger)
      logger.write_string(")")
    }
  }
}
