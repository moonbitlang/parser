///|
fn test_exact() -> Unit raise {
  let str = "hello"
  assert_true(str lexmatch? "hello")
  assert_true(
    str lexmatch? (x, "^hello$", y) &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 0 &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 5 &&
    y.length() == 0,
  )
}

///|
fn test_exact_bytes() -> Unit raise {
  let str = b"hello"
  assert_true(str lexmatch? "hello")
  assert_true(
    str lexmatch? (x, "^hello$", y) &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 0 &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 5 &&
    y.length() == 0,
  )
}

///|
fn test_empty() -> Unit raise {
  let str = "hello"
  assert_false(str lexmatch? "")
  assert_true(
    str lexmatch? (x, "") &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 5,
  )
  assert_true(
    str lexmatch? ("", y) &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 0 &&
    y.length() == 5,
  )
  assert_true(
    str lexmatch? (x, "", y) &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 0 &&
    y.start_offset() == 0 &&
    y.length() == 5,
  )
}

///|
fn test_empty_bytes() -> Unit raise {
  let str = b"hello"
  assert_false(str lexmatch? "")
  assert_true(
    str lexmatch? (x, "") &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 5,
  )
  assert_true(
    str lexmatch? ("", y) &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 0 &&
    y.length() == 5,
  )
  assert_true(
    str lexmatch? (x, "", y) &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 0 &&
    y.start_offset() == 0 &&
    y.length() == 5,
  )
}

///|
fn test_prefix() -> Unit raise {
  let str = "hello world"
  assert_true(
    str lexmatch? ("hello", y) &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 5 &&
    y.length() == 6,
  )
  assert_true(
    str lexmatch? (x, "^hello", y) &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 0 &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 5 &&
    y.length() == 6,
  )
}

///|
fn test_prefix_bytes() -> Unit raise {
  let str = b"hello world"
  assert_true(
    str lexmatch? ("hello", y) &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 5 &&
    y.length() == 6,
  )
  assert_true(
    str lexmatch? (x, "hello", y) &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 0 &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 5 &&
    y.length() == 6,
  )
}

///|
fn test_suffix() -> Unit raise {
  let str = "hello 中国𠮷"
  assert_true(
    str lexmatch? (x, "中国𠮷") &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 6,
  )
  assert_true(
    str lexmatch? (x, "中国𠮷$", y) &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 6 &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 10 &&
    y.length() == 0,
  )
}

///|
fn test_suffix_bytes() -> Unit raise {
  let str = b"hello world"
  assert_true(
    str lexmatch? (x, "world") &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 6,
  )
  assert_true(
    str lexmatch? (x, "world$", y) &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 6 &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 11 &&
    y.length() == 0,
  )
}

///|
fn test_search() -> Unit raise {
  let str = "hello"
  assert_true(
    str lexmatch? (x, "el", y) &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 1 &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 3 &&
    y.length() == 2,
  )
}

///|
fn test_search_bytes() -> Unit raise {
  let str = b"hello"
  assert_true(
    str lexmatch? (x, "el", y) &&
    physical_equal(x.data(), str) &&
    x.start_offset() == 0 &&
    x.length() == 1 &&
    physical_equal(y.data(), str) &&
    y.start_offset() == 3 &&
    y.length() == 2,
  )
}

///|
fn main {
  println("test_exact")
  println(try? test_exact())
  println("test_exact_bytes")
  println(try? test_exact_bytes())
  println("test_empty")
  println(try? test_empty())
  println("test_empty_bytes")
  println(try? test_empty_bytes())
  println("test_prefix")
  println(try? test_prefix())
  println("test_prefix_bytes")
  println(try? test_prefix_bytes())
  println("test_suffix")
  println(try? test_suffix())
  println("test_suffix_bytes")
  println(try? test_suffix_bytes())
  println("test_search")
  println(try? test_search())
  println("test_search_bytes")
  println(try? test_search_bytes())
}
