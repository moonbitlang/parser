fn f(x: &Debug) -> Unit {
  debug(x)
}

fn[X: Debug] poly(x: X) -> Unit {
  f(Option::Some(x))
}

fn init {
  f(Option::Some(42))
  f(Option::Some("xy"))
  poly(42)
  poly("xy")
}
