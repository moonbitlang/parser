///|
suberror MyErr

///|
fn main raise {
  raise MyErr
}

impl Show for MyErr with output(self, logger) {
  logger.write_string("MyErr")
}
