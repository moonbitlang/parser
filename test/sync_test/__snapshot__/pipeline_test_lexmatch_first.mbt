//!build: -w -20-27

///|
fn test_firstmatch() -> Unit raise {
  assert_eq(
    lexmatch "ab" {
      (re"^(a|ab)" as t, after=_) => Some(t)
      _ => None
    },
    Some("a"),
  )
  assert_eq(
    lexmatch "ba" {
      (re"^a", after=_) => true
      _ => false
    },
    false,
  )
}

///|
fn test_search() -> Unit raise {
  assert_eq(
    lexmatch "xxxxxyxxxx" {
      (re"y", before=before, after=_) => Some(before.length())
      _ => None
    },
    Some(5),
  )
}

fn test_multicases_firstmatch() -> Unit raise {
  fn f(x : String) -> Int {
    lexmatch x {
      (re"a", before=_, after=_) => 1
      (re"b", before=_, after=_) => 2
      (re"c", before=_, after=_) => 3
      _ => -1
    }
  }
  assert_eq(f("xxa"), 1)
  assert_eq(f("xxb"), 2)
  assert_eq(f("xxc"), 3)
  assert_eq(f("xxd"), -1)
}

fn test_nested_captures() -> Unit raise {
  assert_eq(
    lexmatch "abcd" {
      (
        ((re"b" as b) + (re"c" as c)) as bc,
        before=before,
        after=after,
      ) => Some((before, b, c, bc, after))
      _ => None
    },
    Some(("a", 'b', 'c', "bc", "d")),
  )
}

fn test_quantifier() -> Unit raise {
  fn f(x : String) -> Int? {
    lexmatch x {
      ((re"^a{2,4}" as a) + re"b", after=_) => Some(a.length())
      _ => None
    }
  }
  assert_eq(
    f("aaab"),
    Some(3),
  )
  assert_eq(f("ab"), None)
  assert_eq(f("baaab"), None)
}

fn test_non_greedy_quantifier() -> Unit raise {
  fn f(x : String) -> StringView? {
    lexmatch x {
      ((re"^(a|b)*?" as x) + re"b", after=_) => Some(x)
      _ => None
    }
  }
  assert_eq(
    f("aaabbb"),
    Some("aaa"),
  )
}

fn test_simple_assertions() -> Unit raise {
  fn f(x : String) -> Bool {
    lexmatch x {
      (re"a|^b", before=_, after=_) => true
      _ => false
    }
  }
  assert_eq(f("xxa"), true)
  assert_eq(f("xxb"), false)
  assert_eq(f("bxx"), true)
}

fn test_catchall_binder() -> Unit raise {
  let input = "123"
  let rest = lexmatch input {
    re"^[a-z]+$" => ""[:]
    rest => rest
  }
  assert_true(physical_equal(rest.data(), input))
  assert_eq(rest.start_offset(), 0)
  assert_eq(rest.length(), 3)
}

///|
fn test_emoji() -> Unit raise {
  lexmatch "😋😋" {
    ((re"^." as u) + (re".$" as v)) => {
      assert_eq(u, '😋')
      assert_eq(v, '😋')
    }
    _ => fail("mismatch")
  }
}
///|
fn main {
  println("test_firstmatch: \{try? test_firstmatch()}")
  println("test_search: \{try? test_search()}")
  println("test_multicases_firstmatch: \{try? test_multicases_firstmatch()}")
  println("test_nested_captures: \{try? test_nested_captures()}")
  println("test_quantifier: \{try? test_quantifier()}")
  println("test_non_greedy_quantifier: \{try? test_non_greedy_quantifier()}")
  println("test_simple_assertions: \{try? test_simple_assertions()}")
  println("test_catchall_binder: \{try? test_catchall_binder()}")
  // non bmp lexmatch
  println("testemoji:\{try? test_emoji()}")  
}
