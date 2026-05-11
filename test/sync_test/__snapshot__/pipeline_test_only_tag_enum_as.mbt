enum E {
  A
  B
}

fn f(x : E) -> Unit {
  match x {
    A as a => println(a)
    B as b => println(b)
  }
}

fn init {
  f(A)
  f(B)
}

impl Show for E with output(self, logger) {
  match self {
    A => logger.write_string("A")
    B => logger.write_string("B")
  }
}
