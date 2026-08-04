///|
fn unwrap(value : Int?) -> Int {
  guard! value is Some(inner)
  inner
}

///|
fn init {
  println(unwrap(None))
}
