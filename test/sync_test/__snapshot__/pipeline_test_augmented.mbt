struct T {
  mut x : Int
} derive(Debug)

fn init {
  let mut a = 10
  a += 10
  println(a)
  a -= 5
  println(a)
  a *= 3
  println(a)
  a /= 5
  println(a)
  a %= 5
  println(a)
  let a = { x: 10 }
  a.x += 2 * 5
  debug(a)
  a.x -= 2 + 3
  debug(a)
  a.x *= 5 - 2
  debug(a)
  a.x /= 5 % 9
  debug(a)
  a.x %= 10 / 2
  debug(a)
  let a: FixedArray[_] = [10]
  a[0]
  +=
  ((2 * 5))
  debug(a)
  a[0]
  -=
  ((2 + 3))
  debug(a)
  a[0]
  *=
  ((5 - 2))
  debug(a)
  a[0]
  /=
  ((5 % 9))
  debug(a)
  a[0]
  %=
  ((10 / 2))
  debug(a)
}
