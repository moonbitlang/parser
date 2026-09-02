///|
struct S {
  x1 : Int
  x2 : Int
  x3 : Int
  x4 : Int
  x5 : Int
  x6 : Int
  x7 : Int
  x8 : Int
  x9 : Int
  default : Int
  default_ : Int
} derive(Default)

///|
fn f1(x : S) -> S {
  { ..x, x1: 100 }
}

///|
fn main {
  let s0 : S = Default::default()
  println(f1(s0))
}

impl Show for S with output(self, logger) {
  logger.write_string("{x1: ")
  Show::output(self.x1, logger)
  logger.write_string(", x2: ")
  Show::output(self.x2, logger)
  logger.write_string(", x3: ")
  Show::output(self.x3, logger)
  logger.write_string(", x4: ")
  Show::output(self.x4, logger)
  logger.write_string(", x5: ")
  Show::output(self.x5, logger)
  logger.write_string(", x6: ")
  Show::output(self.x6, logger)
  logger.write_string(", x7: ")
  Show::output(self.x7, logger)
  logger.write_string(", x8: ")
  Show::output(self.x8, logger)
  logger.write_string(", x9: ")
  Show::output(self.x9, logger)
  logger.write_string(", default: ")
  Show::output(self.default, logger)
  logger.write_string(", default_: ")
  Show::output(self.default_, logger)
  logger.write_string("}")
}
