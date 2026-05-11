///|
struct S {
  a : Int
  b : Int
  c : Int
  d : Int
  e : Int
  f : Int
  g : Int
}

///|
fn main {
  fn f(a : Int) -> Unit {
    let s = { a: 42, b: 42, c: 42, d: 42, e: 42, f: 42, g: 42 }
    let s1 = { ..(if a > 0 { s } else { return f(1) }), a: 43 }
    ignore(s1)
  }
  f(1)
  f(1)
}

impl Show for S with output(self, logger) {
  logger.write_string("{a: ")
  Show::output(self.a, logger)
  logger.write_string(", b: ")
  Show::output(self.b, logger)
  logger.write_string(", c: ")
  Show::output(self.c, logger)
  logger.write_string(", d: ")
  Show::output(self.d, logger)
  logger.write_string(", e: ")
  Show::output(self.e, logger)
  logger.write_string(", f: ")
  Show::output(self.f, logger)
  logger.write_string(", g: ")
  Show::output(self.g, logger)
  logger.write_string("}")
}
