///|
// A value enum with reference fields embedded as a field of a heap struct.
// Dropping/overwriting the container must scan the embedded value enum's
// tag-dependent references.
#valtype
enum E {
  A
  B(Int)
  C(String)
  D(Bytes, Int)
} derive(Debug)

///|
struct Box {
  mut e : E
  label : String
}

///|
fn main {
  let s = "field" + "!"
  let box = Box::{ e: E::C(s), label: "box" + "1" }
  debug(box.e)
  println(box.label)
  // overwrite the value-enum field: the old E::C(s) must be released
  box.e = E::D(b"\xCC\xDD", 4)
  debug(box.e)
  box.e = E::A
  debug(box.e)
  // arrays of heap structs that embed value enums
  let boxes = [box, Box::{ e: E::B(2), label: "box2" }]
  for b in boxes {
    println(Repr(b.e).to_string() + " / " + b.label)
  }
}
