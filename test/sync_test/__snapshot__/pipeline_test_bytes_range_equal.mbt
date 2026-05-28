///|
#borrow(lhs, rhs)
#intrinsic("%bytes.unsafe_range_equal")
fn unsafe_range_equal(
  lhs : Bytes,
  lhs_offset : Int,
  rhs : Bytes,
  rhs_offset : Int,
  len : Int,
) -> Bool {
  for i in 0..<len {
    guard lhs.unsafe_get(lhs_offset + i) == rhs.unsafe_get(rhs_offset + i) else {
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
  let repeated = b"abcabc"
  check(unsafe_range_equal(repeated, 0, repeated, 3, 3), true)
  check(unsafe_range_equal(b"prefix", 3, b"xxfixyy", 2, 3), true)
  check(unsafe_range_equal(b"prefix", 3, b"xxfoxyy", 2, 3), false)
  check(unsafe_range_equal(b"abc", 3, b"xyz", 3, 0), true)
  check(unsafe_range_equal(b"\b00a\bFF", 0, b"zz\b00a\bFFq", 2, 3), true)
}
