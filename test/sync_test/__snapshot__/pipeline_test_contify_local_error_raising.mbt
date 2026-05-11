///|
fn main {
  pipe_failed()
}

///|
suberror MyErr {
  MyErr(String)
} derive(Debug)

///|
fn pipe_failed() -> Unit {
  debug(try? (x => raise MyErr(x))("msg"))
}
