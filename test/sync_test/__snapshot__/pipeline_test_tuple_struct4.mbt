struct T(T)

impl Show for T with output(self, logger) {
  logger.write_string("T(")
  Show::output(self.0, logger)
  logger.write_string(")")
}
