
trait X : Show {
  name (Self) -> String
}

struct Stock(String)

impl X for Stock with name( self : Stock) -> String {
  self.0
}


fn init {
  let v = Stock::Stock("x") as &X
  println(v)
}

impl Show for Stock with output(self, logger) {
  logger.write_string("Stock(")
  Show::output(self.0, logger)
  logger.write_string(")")
}
