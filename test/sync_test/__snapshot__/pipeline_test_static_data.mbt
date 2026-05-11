///|
fn f(x : ReadOnlyArray[(String, String)]) -> Unit {
  debug(x)
}

///|
fn main {
  f([("x1", "y1"), ("x2", "y2")])
}
