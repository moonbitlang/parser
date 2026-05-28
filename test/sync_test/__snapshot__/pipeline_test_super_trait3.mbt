pub(all) struct Requirement(Int)

pub(all) enum Doc {
  Empty
  String(String)
  Blank(Int)
  IfFlat(Doc, Doc)
  HardLine
  Cat(Requirement, Doc, Doc)
  Nest(Requirement, Int, Doc)
  Group(Requirement, Doc)
  Align(Requirement, Doc)
  Custom(&Custom)
}

pub(open) trait Custom : Show  {
  fn requirement(Self) -> Requirement
  fn pretty(Self, Output, State, Int, Bool) -> Unit
}

pub(all) type Output
pub(all) type State

fn init {
  let a = Doc::Cat(Requirement(3), Empty,Empty)
  println(a)
}

impl Show for Requirement with output(self, logger) {
  logger.write_string("Requirement(")
  Show::output(self.0, logger)
  logger.write_string(")")
}

impl Show for Doc with output(self, logger) {
  match self {
    Empty => logger.write_string("Empty")
    String(v0) => {
      logger.write_string("String(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
    Blank(v0) => {
      logger.write_string("Blank(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
    IfFlat(v0, v1) => {
      logger.write_string("IfFlat(")
      Show::output(v0, logger)
      logger.write_string(", ")
      Show::output(v1, logger)
      logger.write_string(")")
    }
    HardLine => logger.write_string("HardLine")
    Cat(v0, v1, v2) => {
      logger.write_string("Cat(")
      Show::output(v0, logger)
      logger.write_string(", ")
      Show::output(v1, logger)
      logger.write_string(", ")
      Show::output(v2, logger)
      logger.write_string(")")
    }
    Nest(v0, v1, v2) => {
      logger.write_string("Nest(")
      Show::output(v0, logger)
      logger.write_string(", ")
      Show::output(v1, logger)
      logger.write_string(", ")
      Show::output(v2, logger)
      logger.write_string(")")
    }
    Group(v0, v1) => {
      logger.write_string("Group(")
      Show::output(v0, logger)
      logger.write_string(", ")
      Show::output(v1, logger)
      logger.write_string(")")
    }
    Align(v0, v1) => {
      logger.write_string("Align(")
      Show::output(v0, logger)
      logger.write_string(", ")
      Show::output(v1, logger)
      logger.write_string(")")
    }
    Custom(v0) => {
      logger.write_string("Custom(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
  }
}
