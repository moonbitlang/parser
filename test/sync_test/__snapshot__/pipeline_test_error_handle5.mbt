suberror StringError {
  StringError(String)
}

fn f(x: Int) -> Int raise {
  return g(x)
}

fn g(x: Int) -> Int raise {
  if x > 0 { x } else { raise StringError("xxx") }
}

fn init {
  println((try! f(42)))
}

impl Show for StringError with output(self, logger) {
  match self {
    StringError(v0) => {
      logger.write_string("StringError(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
  }
}
