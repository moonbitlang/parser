///|
// Value enum whose constructors carry reference fields (String, Bytes), mixed
// with scalars of different widths to exercise payload slot alignment.
#valtype
enum E {
  A
  B(Int)
  C(String)
  D(String, Int)
  E(Int64, String)
  F(Bytes, Double)
}

///|
fn describe(e : E) -> String {
  match e {
    A => "A"
    B(x) => "B(" + x.to_string() + ")"
    C(s) => "C(" + s + ")"
    D(s, x) => "D(" + s + ", " + x.to_string() + ")"
    E(n, s) => "E(" + n.to_string() + ", " + s + ")"
    F(b, d) => "F(" + b.length().to_string() + ", " + d.to_string() + ")"
  }
}

///|
fn main {
  let s = "hello" + "!"
  let a = E::A
  let b = E::B(42)
  let c = E::C(s)
  let d = E::D(s, 7)
  let e = E::E(1234567890123L, s)
  let f = E::F(b"\x01\x02\x03", 3.5)
  println(describe(a))
  println(describe(b))
  println(describe(c))
  println(describe(d))
  println(describe(e))
  println(describe(f))
  // the same reference is shared across several value enums (exercises incref)
  let dup = c
  println(describe(dup))
  // reassign a mutable binding (exercises decref of the previous value)
  let mut g = E::C("first" + "?")
  println(describe(g))
  g = E::D("second" + "?", 9)
  println(describe(g))
}
