///|
// A value enum with reference fields captured inside a closure: the closure
// object embeds the value enum, so dropping the closure must scan the embedded
// enum's tag-dependent references.
#valtype
enum E {
  A
  B(Int)
  C(String)
  D(String, Int)
} derive(Debug)

///|
fn make_capture(e : E, suffix : String) -> () -> String {
  () => Repr(e).to_string() + suffix
}

///|
fn main {
  let f1 = make_capture(E::A, "!")
  println(f1())
  let f2 = make_capture(E::B(7), "?")
  println(f2())
  let s = "captured" + "!"
  let f3 = make_capture(E::C(s), ".")
  println(f3())
  let f4 = make_capture(E::D(s, 3), "..")
  println(f4())
  // call one closure several times (the captured reference stays alive)
  println(f3())
  println(f3())
}
