///|
// Arrays of value enums with reference fields: the element scanning must
// dispatch on each element's own tag, since different elements hold their
// references at different offsets (or none at all).
#valtype
enum E {
  A
  B(Int)
  C(String)
  D(Bytes, Int)
} derive(Debug)

///|
fn sum_lengths(xs : FixedArray[E]) -> Int {
  let mut total = 0
  for e in xs {
    total = total +
      (match e {
        A => 0
        B(x) => x
        C(s) => s.length()
        D(b, x) => b.length() + x
      })
  }
  total
}

///|
// Regression test for FixedArray::make refcounting of reference-carrying value
// enums. `make(n, init)` copies `init` into every slot, so the shared payload
// must be incref'd by n-1; otherwise dropping the array frees the payload while
// a retained element still points at it. The payload is built at runtime so it
// is a genuine refcounted heap string (not a folded literal), and one element
// is kept alive past the source array's lifetime.
fn retained_from_make(seed : Int) -> E {
  let arr : FixedArray[E] = FixedArray::make(4, E::C(seed.to_string() + "!"))
  arr[2]
}

///|
fn main {
  let s = "abcd"
  // array literal of mixed-tag value enums with references
  let xs : FixedArray[E] = [E::A, E::B(3), E::C(s), E::D(b"\xAA\xBB", 5)]
  for e in xs {
    debug(e)
  }
  println(sum_lengths(xs).to_string())
  // FixedArray::make fills N copies of one ref-carrying value enum
  let filled : FixedArray[E] = FixedArray::make(3, E::C("xyz"))
  for e in filled {
    debug(e)
  }
  println(sum_lengths(filled).to_string())
  // a growable array as well
  let ys = [E::C("p" + "q"), E::D(b"\x01", 2)]
  ys.push(E::B(8))
  for e in ys {
    debug(e)
  }
  // a value-enum reference that outlives the FixedArray::make array it came from
  debug(retained_from_make(12))
}
