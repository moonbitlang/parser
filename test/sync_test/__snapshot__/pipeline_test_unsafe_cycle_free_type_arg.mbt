// Regression test for `#unsafe_cycle_free` and type-argument elements.
//
// The analysis keeps the complete type reference graph and, in a component
// with marked declarations, searches for a closed walk from an element node
// back to itself that crosses a mutable edge and visits each marked
// declaration at most once.  A cycle that merely routes through the marked
// container must therefore stay visible:
//
//     Looped -> FixedArray[CycleFreeList[Looped]] -> CycleFreeList -> Looped
//
// `Looped` is the container's element and closes a real cycle through the
// mutable array slot, so it must stay cycle-capable.  `ScalarHolder`
// instantiates the same container at a scalar, which contributes no element
// node, so its component is never searched and it legitimately stays
// cycle-free.
//
// The signal lives in the generated C: `Looped`'s allocation should be followed
// by a `set_cycle_capable` stamp, `ScalarHolder`'s should not.  See
// `test/c_backend_test/irs/unsafe_cycle_free_type_arg.mbt.exp.c`.

///|
#unsafe_cycle_free
enum CycleFreeList[A] {
  Nil
  Cons(A, mut tail~ : CycleFreeList[A])
}

///|
struct Looped {
  mut links : FixedArray[CycleFreeList[Looped]]
}

///|
struct ScalarHolder {
  mut links : FixedArray[CycleFreeList[Int]]
}

///|
fn main {
  let looped : Looped = { links: FixedArray::make(1, Nil) }
  looped.links[0] = Cons(looped, tail=Nil) // closes the cycle
  println(looped.links.length())
  // The cycle is left live on purpose: on backends with no cycle collector it
  // simply leaks, so this file is listed in `tests_with_leak` in
  // test/memory_safety_test/autogen.js alongside dlist.mbt and rc_cycle.mbt.
  let scalar : ScalarHolder = { links: FixedArray::make(1, Nil) }
  scalar.links[0] = Cons(7, tail=Nil)
  println(scalar.links.length())
}
