fn f(x: Int) -> Unit {
  let mut a = Option::Some(x)
  while true {
    let b = match a {
      None => break
      Some(i) => Option::Some(i)
    }
    debug(b)
    a = None
  }
}

fn g(x: Int) -> Unit {
  let mut a = Option::Some(x)
  while true {
    let b = match a {
      None => continue
      Some(i) => Option::Some(i)
    }
    debug(b)
    break
  }
}

fn init {
  f(42)
  g(42)
}
