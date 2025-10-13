
fn init {
  println(1)
  println(1.23)
  println({ val: "abc" })
  println({ val: "a\"b\"c" })
  println({ val: "a\nb\nc" })
  println({ val: "中文" })
  println({ val: "with space and \\" })
  println('x')
  println('中')
  println('\'')
  println('\n')
  println('\\')
  let t = { x: 42, y: ("abc", 'x', Some(3.14)), z: fn (x) { x } }
  println(t)
  println(U::A("42", t))
  println(U::B(Some((42, "42", 42.0))))
}


struct T {
  x: Int
  y: (String, Char, Double?)
  z: (Int) -> Int
} derive(Show)

enum U[X] {
  A(X, T)
  B(X?)
} derive(Show)
