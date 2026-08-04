// Exercises the general (non-foreach) `code_units` intrinsic: the returned
// `ArrayView[UInt16]` is bound to a local and then indexed / measured. On the
// native, wasm-linear and llvm backends this is specialized in
// `Core_util.try_apply_intrinsic` into a zero-copy view aliasing the string's
// code-unit buffer; on js / wasm-gc it falls back to the body below.
// (The foreach form is covered separately by `iter_string.mbt`.)

#intrinsic("%string.code_units")
fn String::code_units_for_test(self : String) -> ArrayView[UInt16] {
  self.code_units()
}

#intrinsic("%stringview.code_units")
fn StringView::code_units_for_test(self : StringView) -> ArrayView[UInt16] {
  self.code_units()
}

fn main {
  let s = "hello"

  // String -> ArrayView[UInt16], bound then indexed.
  let cu = s.code_units_for_test()
  println("string len")
  println(cu.length())
  println("string elems")
  println(cu[0].to_int())
  println(cu[4].to_int())

  // StringView with non-zero start/end ("ell") -> ArrayView[UInt16].
  let sv = "hello"[1:4]
  let cu2 = sv.code_units_for_test()
  println("view len")
  println(cu2.length())
  println("view elems")
  println(cu2[0].to_int())
  println(cu2[2].to_int())

  // Empty string.
  println("empty len")
  println("".code_units_for_test().length())

  // Source string is still usable after the aliasing view is built and used,
  // which exercises the refcount handling of the shared buffer.
  println("source after")
  println(s)
}
