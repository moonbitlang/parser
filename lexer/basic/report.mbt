///|
pub(all) struct Report {
  loc : Location
  msg : String
} derive(Debug)

///|
pub impl Show for Report with fn output(self, logger) {
  logger.write_string("error (")
  logger.write_string(self.loc.to_string())
  logger.write_char(':')
  logger.write_char(' ')
  logger.write_string(self.msg.to_string())
}

///|
pub impl ToJson for Report with fn to_json(self) {
  { "type": "Error", "loc": self.loc, "msg": self.msg }
}
