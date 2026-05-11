
fn init {
  println(1)
  println(1.23)
  debug(Ref("abc"))
  debug(Ref("a\"b\"c"))
  debug(Ref("a\nb\nc"))
  debug(Ref("中文"))
  debug(Ref("with space and \\"))
  println('x')
  println('中')
  println('\'')
  println('\n')
  println('\\')
  let t = { x: 42, y: ("abc", 'x', Some(3.14)), z: fn (x) { x } }
  debug(t)
  debug(U::A("42", t))
  debug(U::B(Some((42, "42", 42.0))))
}


struct T {
  x: Int
  y: (String, Char, Double?)
  z: (Int) -> Int
} derive(Debug)

enum U[X] {
  A(X, T)
  B(X?)
} derive(Debug)
