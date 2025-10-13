///|
fn f(chars : @array.View[Char]) -> Char {
  loop chars {
    [.. chars] =>
      continue {
          return chars[0]
        }
  }
}

///|
fn main {
  println(f(['a', 'b', 'c']))
}
