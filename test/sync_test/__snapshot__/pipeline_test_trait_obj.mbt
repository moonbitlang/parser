trait T {
  fn f(Self, Int) -> Int
  fn g(Self, String) -> String
}

struct S {
  x: Int
  s: String
}

impl T for S with f(self: S, a: Int) -> Int {
  self.x + a
}

impl T for S with g(self: S, a: String) -> String {
  self.s + a.to_string()
}

fn use_T(a: &T) -> Int {
  a.f(432) + a.g("object").length()
}

fn init {
  let s = S::{x: 234, s: "trait"} as &T
  println(use_T(s))
}

fn test_debug(x: &Debug) -> Unit {
  println(x.to_repr())
}
fn init {
  test_debug(42 as &Debug)
  test_debug("xxx" as &Debug)
  test_debug(3.14 as &Debug)
}

fn test_array_debug(x: Array[&Debug]) -> Unit {
  x.each(debug)
}
fn init {
  let shows = [42 as &Debug, "xxx" as &Debug, 3.14 as &Debug]
  test_array_debug(shows)
  let debugs = shows as &Debug
  debug(debugs)
}

fn init {
  let x = Option::Some(Option::Some(Option::Some(42))) as &Debug
  test_debug(x)
  generic_debug(x)
}

fn[T: Debug] generic_debug(x: T) -> Unit {
  debug(x)
}
