// Deep nesting of value types: a value enum inside a value enum (directly and
// behind an intermediate value struct), three levels of value structs, and the
// RC-sensitive array paths (FixedArray::make sharing one init value, growable
// Array, an element retained past its source array's lifetime). Payload
// strings are built at runtime so they are genuine refcounted heap objects,
// not folded literals.

///|
#valtype
enum InnerChoice {
  IntOnly(Int)
  Tagged(String, Int)
}

///|
#valtype
struct InnerWrap {
  choice : InnerChoice
  serial : Int
}

///|
#valtype
enum OuterChoice {
  Nothing
  Direct(InnerChoice, Int)
  Wrapped(InnerWrap)
  Plain(String)
}

///|
#valtype
struct DeepLeaf {
  text : String
  code : Int
}

///|
#valtype
struct DeepMid {
  leaf : DeepLeaf
  extra : Int64
}

///|
#valtype
struct DeepTop {
  mid : DeepMid
  outer : OuterChoice
}

///|
fn show_inner(e : InnerChoice) -> String {
  match e {
    IntOnly(n) => "IntOnly(\{n})"
    Tagged(s, n) => "Tagged(\{s},\{n})"
  }
}

///|
fn show_outer(e : OuterChoice) -> String {
  match e {
    Nothing => "Nothing"
    Direct(inner, n) => "Direct(\{show_inner(inner)},\{n})"
    Wrapped(w) => "Wrapped(\{show_inner(w.choice)},\{w.serial})"
    Plain(s) => "Plain(\{s})"
  }
}

///|
fn retained_from_make(seed : Int) -> OuterChoice {
  let init = OuterChoice::Direct(Tagged(seed.to_string() + "!", seed), seed * 2)
  let arr : FixedArray[OuterChoice] = FixedArray::make(4, init)
  arr[2]
}

///|
fn retained_top_from_make(seed : Int) -> DeepTop {
  let leaf = DeepLeaf::{ text: seed.to_string() + "?", code: seed }
  let top = DeepTop::{
    mid: DeepMid::{ leaf, extra: 1L },
    outer: Wrapped(InnerWrap::{ choice: Tagged(seed.to_string() + "*", 0), serial: 5 }),
  }
  let arr : FixedArray[DeepTop] = FixedArray::make(3, top)
  arr[1]
}

///|
fn main {
  // enum-in-enum, direct and behind a value struct
  let mut e : OuterChoice = Direct(Tagged("a" + "b", 1), 2)
  println(show_outer(e))
  e = Wrapped(InnerWrap::{ choice: IntOnly(3), serial: 4 })
  println(show_outer(e))
  e = Plain("p" + "q")
  println(show_outer(e))
  e = Nothing
  println(show_outer(e))

  // three levels of value structs, with an enum-in-enum sibling field
  let top = DeepTop::{
    mid: DeepMid::{ leaf: DeepLeaf::{ text: "d" + "eep", code: 9 }, extra: 7L },
    outer: Direct(IntOnly(6), 5),
  }
  println("\{top.mid.leaf.text} \{top.mid.leaf.code} \{top.mid.extra} \{show_outer(top.outer)}")

  // closure capture of a nested value
  let captured = top
  let read = fn() -> String { "\{captured.mid.leaf.text}/\{show_outer(captured.outer)}" }
  println(read())

  // FixedArray of enum-in-enum values: literal, make-fill, overwrite
  let xs : FixedArray[OuterChoice] = [
    Nothing,
    Direct(Tagged("x" + "s", 1), 2),
    Wrapped(InnerWrap::{ choice: Tagged("w" + "s", 3), serial: 4 }),
  ]
  for x in xs {
    println(show_outer(x))
  }
  let filled : FixedArray[OuterChoice] = FixedArray::make(
    3,
    Direct(Tagged("f" + "ill", 8), 9),
  )
  filled[1] = Plain("re" + "placed")
  println("\{show_outer(filled[0])} \{show_outer(filled[1])} \{show_outer(filled[2])}")

  // growable Array of nested values
  let grow : Array[DeepTop] = []
  for i in 0..<3 {
    grow.push(DeepTop::{
      mid: DeepMid::{ leaf: DeepLeaf::{ text: "g" + i.to_string(), code: i }, extra: 0L },
      outer: if i == 1 { Direct(IntOnly(i), i) } else { Nothing },
    })
  }
  for t in grow {
    println("\{t.mid.leaf.text} \{show_outer(t.outer)}")
  }

  // values that outlive the FixedArray::make array they came from
  println(show_outer(retained_from_make(12)))
  let kept = retained_top_from_make(34)
  println("\{kept.mid.leaf.text} \{show_outer(kept.outer)}")
}
