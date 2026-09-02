//!build: -w -20-27-unused_lexcase

///|
fn test_exact() -> Unit raise {
  let str = "hello"
  assert_true(lexmatch str { re"^hello$" => true; _ => false })
  assert_false(lexmatch "a" with longest { (re"a^", after=_) => true; _ => false })
  let (matched, x, y) = lexmatch str {
    (re"^hello$", before=x, after=y) => (true, x, y)
    _ => (false, ""[:], ""[:])
  }
  assert_true(matched)
  assert_true(physical_equal(x.data(), str))
  assert_eq(x.start_offset(), 0)
  assert_eq(x.length(), 0)
  assert_true(physical_equal(y.data(), str))
  assert_eq(y.start_offset(), 5)
  assert_eq(y.length(), 0)
}

///|
fn test_empty() -> Unit raise {
  let str = "hello"
  assert_false(lexmatch str { re"^$" => true; _ => false })
  let x = lexmatch str { (re"$", before=x) => x; _ => ""[:] }
  assert_true(physical_equal(x.data(), str))
  assert_eq(x.start_offset(), 0)
  assert_eq(x.length(), 5)
  let y = lexmatch str { (re"^", after=y) => y; _ => ""[:] }
  assert_true(physical_equal(y.data(), str))
  assert_eq(y.start_offset(), 0)
  assert_eq(y.length(), 5)
  let (x, y) = lexmatch str {
    (re"^", before=x, after=y) => (x, y)
    _ => (""[:], ""[:])
  }
  assert_true(physical_equal(x.data(), str))
  assert_eq(x.start_offset(), 0)
  assert_eq(x.length(), 0)
  assert_eq(y.start_offset(), 0)
  assert_eq(y.length(), 5)
}

///|
fn test_prefix() -> Unit raise {
  let str = "hello world"
  let y = lexmatch str { (re"^hello", after=y) => y; _ => ""[:] }
  assert_true(physical_equal(y.data(), str))
  assert_eq(y.start_offset(), 5)
  assert_eq(y.length(), 6)
  let (x, y) = lexmatch str {
    (re"^hello", before=x, after=y) => (x, y)
    _ => (""[:], ""[:])
  }
  assert_true(physical_equal(x.data(), str))
  assert_eq(x.start_offset(), 0)
  assert_eq(x.length(), 0)
  assert_true(physical_equal(y.data(), str))
  assert_eq(y.start_offset(), 5)
  assert_eq(y.length(), 6)
}

///|
fn test_suffix() -> Unit raise {
  let str = "hello 中国𠮷"
  let x = lexmatch str { (re"中国𠮷$", before=x) => x; _ => ""[:] }
  assert_true(physical_equal(x.data(), str))
  assert_eq(x.start_offset(), 0)
  assert_eq(x.length(), 6)
  let (x, y) = lexmatch str {
    (re"中国𠮷$", before=x, after=y) => (x, y)
    _ => (""[:], ""[:])
  }
  assert_true(physical_equal(x.data(), str))
  assert_eq(x.start_offset(), 0)
  assert_eq(x.length(), 6)
  assert_true(physical_equal(y.data(), str))
  assert_eq(y.start_offset(), 10)
  assert_eq(y.length(), 0)
}

///|
fn test_search() -> Unit raise {
  let str = "hello"
  let (x, y) = lexmatch str {
    (re"el", before=x, after=y) => (x, y)
    _ => (""[:], ""[:])
  }
  assert_true(physical_equal(x.data(), str))
  assert_eq(x.start_offset(), 0)
  assert_eq(x.length(), 1)
  assert_true(physical_equal(y.data(), str))
  assert_eq(y.start_offset(), 3)
  assert_eq(y.length(), 2)
}

///|
fn main {
  println("test_exact")
  println(try? test_exact())
  println("test_empty")
  println(try? test_empty())
  println("test_prefix")
  println(try? test_prefix())
  println("test_suffix")
  println(try? test_suffix())
  println("test_search")
  println(try? test_search())
}
