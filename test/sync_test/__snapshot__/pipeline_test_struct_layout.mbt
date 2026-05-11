///|
struct S {
  y : Int16
  mut x : Byte
  z : Int
}

///|
fn f(s : S) -> Unit {
  println(s.x)
  println(s.y)
  println(s.z)
  s.x = 10
}

///|
fn main {
  let s = S::{ x: 1, y: 2, z: 3 }
  f(s)
  println(s)
}

impl Show for S with output(self, logger) {
  logger.write_string("{y: ")
  Show::output(self.y, logger)
  logger.write_string(", x: ")
  Show::output(self.x, logger)
  logger.write_string(", z: ")
  Show::output(self.z, logger)
  logger.write_string("}")
}
