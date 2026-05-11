enum T {
  A(Char?)
} derive(Debug)

fn f(t: T) -> Unit {
  match t {
    A(Some(c)) => println(c)
    _ => println("none")
  }
}

fn g(t: Char?) -> Unit {
  match h(t) {
    Some(c) => debug(c)
    _ => println("none")
  }
}

fn[T: Debug] h(t: T) -> T {
  debug(t)
  t
}

fn init {
  let t = T::A(Some('a'));
  f(t)
  debug(t)
  g(Some('b'))
}
