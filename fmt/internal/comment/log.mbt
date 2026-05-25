///|
#cfg(false)
fn log(s : String) -> Unit {
  println(s)
}

///|
#cfg(true)
fn log(s : String) -> Unit {
  ignore(s)
}
