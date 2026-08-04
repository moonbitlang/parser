///|
#borrow(lhs, rhs)
#intrinsic("%string.unsafe_range_equal")
fn unsafe_range_equal(
  lhs : String,
  lhs_offset : Int,
  rhs : String,
  rhs_offset : Int,
  len : Int,
) -> Bool {
  for i in 0..<len {
    guard
      lhs.unsafe_get(lhs_offset + i).to_int() ==
      rhs.unsafe_get(rhs_offset + i).to_int()
    else {
      return false
    }
  }
  true
}

///|
fn check(actual : Bool, expected : Bool) -> Unit {
  if actual == expected {
    println("ok")
  } else {
    println("not ok")
  }
}

///|
fn main {
  let repeated = "abcabc"
  check(unsafe_range_equal(repeated, 0, repeated, 3, 3), true)
  check(unsafe_range_equal("prefix", 3, "xxfixyy", 2, 3), true)
  check(unsafe_range_equal("prefix", 3, "xxfoxyy", 2, 3), false)
  check(unsafe_range_equal("abc", 3, "xyz", 3, 0), true)
  check(unsafe_range_equal("x中y", 1, "zz中q", 2, 1), true)
  check(unsafe_range_equal("\u{0100}", 0, "\u{0000}", 0, 1), false)
  check(unsafe_range_equal("x\u{1F600}y", 1, "zz\u{1F600}q", 2, 2), true)
  check(unsafe_range_equal("\u{1F600}", 0, "\u{1F601}", 0, 2), false)
}
