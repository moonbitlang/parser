///|
struct S {
  mut a : &Debug
  b : (Int, &Debug)
} derive(Debug)

///|
fn main {
  let s1 = S::{ a: 42, b: (42, (42, 42)) }
  debug(s1)
  let bs = b"abcde"
  s1.a = bs[1:3]
  debug(s1)
}
