///|
pub(all) struct Report {
  loc : Location
  msg : String
} derive(Debug)

///|
pub impl Show for Report with fn output(self, logger) {
  logger.write_string("error (")
  logger.write_string(Show::to_string(self.loc))
  logger.write_char(':')
  logger.write_char(' ')
  logger.write_string(self.msg.to_string())
}

///|
pub impl ToJson for Report with fn to_json(self) {
  { "type": "Error", "loc": self.loc, "msg": self.msg }
}

///|
#deprecated
#doc(hidden)
pub extend Report with Debug::{to_repr}

///|
#deprecated
#doc(hidden)
pub extend Report with Show::{to_string, output}

///|
#deprecated
#doc(hidden)
pub extend Report with ToJson::{to_json}
