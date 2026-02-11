///|
struct S {
  a : Int
  b : Int
  c : Int
  d : Int
  e : Int
  f : Int
  g : Int
} derive(Show)

///|
fn main {
  fn f(a : Int) -> Unit {
    let s = { a: 42, b: 42, c: 42, d: 42, e: 42, f: 42, g: 42 }
    let s1 = { ..(if a > 0 { s } else { return f(1) }), a: 43 }
    ignore(s1)
  }
  f(1)
  f(1)
}
