///|
fn test_equal(a : Bytes, b : Bytes, res : Bool) -> Unit {
  if res == (a == b) {
    println("ok")
  } else {
    println("not ok")
  }
}

///|
fn test_equal_length_boundary(length : Int) -> Unit {
  test_equal(Bytes::make(length, b'a'), Bytes::make(length, b'a'), true)
}

///|
fn main {
  // Basic cases
  test_equal(b"hello", b"hello", true)
  test_equal(b"hello", b"world", false)

  // Empty bytes
  test_equal(b"", b"", true)
  test_equal(b"", b"x", false)

  // Single byte
  test_equal(b"a", b"a", true)
  test_equal(b"a", b"b", false)

  // Test scalar lengths.
  test_equal(b"1234567", b"1234567", true)
  test_equal(b"1234567", b"1234568", false)
  test_equal(b"12345678", b"12345678", true)
  test_equal(b"12345678", b"12345679", false)
  test_equal(b"123456789", b"123456789", true)
  test_equal(b"123456789", b"123456780", false)

  // Test lengths around one SIMD vector.
  test_equal_length_boundary(15)
  test_equal(b"aaaaaaaaaaaaaaa", b"aaaaaaaaaaaaaab", false)
  test_equal_length_boundary(16)
  test_equal(b"aaaaaaaaaaaaaaaa", b"aaaaaaaaaaaaaaab", false)
  test_equal_length_boundary(17)
  test_equal(b"aaaaaaaaaaaaaaaaa", b"aaaaaaaaaaaaaaaab", false)

  // Test with null bytes
  test_equal(b"\b00\b00\b00", b"\b00\b00\b00", true)
  test_equal(b"\b00abc\b00", b"\b00abc\b00", true)
  test_equal(b"\b00abc\b00", b"\b00abd\b00", false)

  // Test with all possible byte values
  test_equal(b"\bFF\b00\bFF", b"\bFF\b00\bFF", true)
  test_equal(b"\bFF\b00\bFF", b"\bFF\b01\bFF", false)

  // Long values test multiple SIMD comparisons.
  test_equal(
    b"This is a long string to test multiple word comparisons!", b"This is a long string to test multiple word comparisons!",
    true,
  )
  test_equal(
    b"This is a long string to test multiple word comparisons!", b"This is a long string to test multiple word comparison?",
    false,
  )
}
