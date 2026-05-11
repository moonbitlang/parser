trait I {
  f(Self) -> Unit
  g(Self) -> Unit = _
}

impl I with g(self) {
  self.f()
}

struct No_param (Int)

impl I for No_param with f(self) {
  println(self)
}


struct More_param[X, Y] {
  x : X
  y : Y
}

impl[X : Show, Y : Show + Eq + Default] I for More_param[X, Y] with f(self) {
  let is_default = if self.y == Y::default() {
    "is default"
  } else {
    "is not default"
  }
  println("\{self} where y \{is_default}")
}

fn init {
  println("===")
  No_param::g(42)
  More_param::g({ x: "x", y: 42 })
  More_param::g({ x: "x", y: 0 })
}

impl Show for No_param with output(self, logger) {
  logger.write_string("No_param(")
  Show::output(self.0, logger)
  logger.write_string(")")
}

impl[X : Show, Y : Show] Show for More_param[X, Y] with output(self, logger) {
  logger.write_string("{x: ")
  Show::output(self.x, logger)
  logger.write_string(", y: ")
  Show::output(self.y, logger)
  logger.write_string("}")
}
