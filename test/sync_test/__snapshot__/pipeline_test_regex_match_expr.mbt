///|
const PREFIX = re"a"

///|
const SUFFIX = re"bc"

///|
const ALT = re"xy"

///|
const HIDDEN_CAPTURE = re"(?<id>[0-9]+)"

///|
fn test_value_context() -> Unit raise {
  let s = "zabc!"
  let matched = s =~ re"abc"
  let anchored = s =~ re"^abc"
  let by_const = "abc" =~ (PREFIX + SUFFIX)
  let by_view = "abc"[:] =~ (PREFIX + SUFFIX)

  assert_true(matched)
  assert_true(!anchored)
  assert_true(by_const)
  assert_true(by_view)
}

///|
fn test_hidden_capture_in_regex_constant() -> Unit raise {
  assert_true("123" =~ HIDDEN_CAPTURE)
  assert_true(!("abc" =~ HIDDEN_CAPTURE))
}

///|
fn test_hidden_capture_in_composed_regex_constant() -> Unit raise {
  assert_true("x123y" =~ (re"x" + HIDDEN_CAPTURE + re"y"))
  assert_true(
    ("ax123yb" =~ (re"x" + HIDDEN_CAPTURE + re"y", before=head, after=tail)) &&
    head is "a" &&
    tail is "b",
  )
}

///|
fn test_before_after_rest() -> Unit raise {
  assert_true(
    ("zabc!" =~ ((PREFIX + SUFFIX) as whole, before=head, after=tail)) &&
    whole is "abc" &&
    head is "z" &&
    tail is "!",
  )
}

///|
fn test_anchored_before_regression() -> Unit raise {
  if ("abc" =~ (re"^ab" as whole, before=head, after=tail)) {
    assert_true(head is "")
    assert_true(whole is "ab")
    assert_true(tail is "c")
  } else {
    fail("test_anchored_before_regression")
  }
}

///|
fn test_anchored_empty_regression() -> Unit raise {
  assert_true(!("abc" =~ re"^$"))

  if ("" =~ (re"^$" as whole, before=head, after=tail)) {
    assert_true(head is "")
    assert_true(whole is "")
    assert_true(tail is "")
  } else {
    fail("test_anchored_empty_regression")
  }
}

///|
fn test_multiple_captures() -> Unit raise {
  if ("zzabcd!" =~ (
      ((re"ab" as ab) + (re"c" as c) + (re"d" as d)) as whole,
      before=head,
      after=tail
    )) {
    assert_true(ab is "ab")
    assert_eq(c, 'c')
    assert_eq(d, 'd')
    assert_true(whole is "abcd")
    assert_true(head is "zz")
    assert_true(tail is "!")
  } else {
    fail("test_multiple_captures")
  }
}

///|
fn test_char_capture() -> Unit raise {
  if ("abc" =~ (re"." as ch, after=tail)) {
    assert_eq(ch, 'a')
    assert_true(tail is "bc")
  } else {
    fail("test_char_capture")
  }
}

///|
fn test_short_circuit_scope() -> Unit raise {
  assert_true(("abc" =~ (re"^a", before=head)) && head is "")
  assert_true(("abc" =~ (re"." as ch, after=tail)) && ch == 'a' && tail is "bc")
}

///|
fn test_alternation_and_no_match() -> Unit raise {
  if ("zzxy!" =~ ((re"abc" | ALT) as whole, before=head, after=tail)) {
    assert_true(whole is "xy")
    assert_true(head is "zz")
    assert_true(tail is "!")
  } else {
    fail("test_alternation_and_no_match")
  }

  assert_true(!("zzxy!" =~ re"^xy"))
}

///|
fn main {
  println("test_value_context")
  debug(try? test_value_context())
  println("test_hidden_capture_in_regex_constant")
  debug(try? test_hidden_capture_in_regex_constant())
  println("test_hidden_capture_in_composed_regex_constant")
  debug(try? test_hidden_capture_in_composed_regex_constant())
  println("test_before_after_rest")
  debug(try? test_before_after_rest())
  println("test_anchored_before_regression")
  debug(try? test_anchored_before_regression())
  println("test_anchored_empty_regression")
  debug(try? test_anchored_empty_regression())
  println("test_multiple_captures")
  debug(try? test_multiple_captures())
  println("test_char_capture")
  debug(try? test_char_capture())
  println("test_short_circuit_scope")
  debug(try? test_short_circuit_scope())
  println("test_alternation_and_no_match")
  debug(try? test_alternation_and_no_match())
}
