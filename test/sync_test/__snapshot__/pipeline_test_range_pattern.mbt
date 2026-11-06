fn sign(x : Int) -> Int {
  match x {
    _..<0 => -1
    0 => 0
    0..<_ => 1
  }
}

fn init {
  println("sign(1) = \{sign(1)}")
  println("sign(0) = \{sign(0)}")
  println("sign(-1) = \{sign(-1)}")
}

fn classify_char(c : Char) -> String {
  match c {
    'a'..='z' => "lowercase"
    'A'..='Z' => "uppercase"
    '0'..='9' => "digit"
    _ => "other"
  }
}

fn init {
  println("classify_char('z') = \{classify_char('z')}")
  println("classify_char('X') = \{classify_char('X')}")
  println("classify_char('3') = \{classify_char('3')}")
  println("classify_char('字') = \{classify_char('字')}")
}

fn overlap(x : UInt) -> Int {
  match x {
    100..<200 => 1
    0..<150 => 2
    200..<_ => 3
  }
}

fn init {
  println(overlap(50))
  println(overlap(100))
  println(overlap(150))
  println(overlap(200))
}


const Zero = 0
fn is_positive(x : Int) -> Bool {
  match x {
    Zero ..< _ => true
    _ ..< Zero => false
  }
}

fn init {
  println(is_positive(1))
  println(is_positive(-1))
}

const UZero : UInt = 0
fn is_positive_unsigned_int(x : UInt) -> Bool {
  match x {
    1 ..< _ => true
    UZero => false
  }
}

fn init {
  println(is_positive_unsigned_int(1))
  println(is_positive_unsigned_int(0))
}

///|
fn canonical(x : Int) -> Bool {
  x is -2..<=3
}

///|
fn legacy(x : Int) -> Bool {
  x is -2..=3
}

///|
fn singleton(x : Int) -> Bool {
  x is 3..<=3
}

///|
fn unbounded(x : Int) -> Int {
  match x {
    _..<=-1 => -1
    0..<=0 => 0
    1..<_ => 1
  }
}

///|
fn character(x : Char) -> Bool {
  x is 'a'..<='z'
}

///|
const Lower = -2

///|
const Upper = 3

///|
#warnings("-syntax_lint")
fn descending(x : Int) -> Bool {
  x is Upper>=..Lower
}

///|
#warnings("-syntax_lint")
fn descending_open(x : Int) -> Bool {
  x is 3>..-2
}

///|
#warnings("-syntax_lint")
fn descending_singleton(x : Int) -> Bool {
  x is 3>=..3
}

///|
#warnings("-syntax_lint")
fn descending_unbounded(x : Int) -> Int {
  match x {
    -1>=.._ => -1
    0>=..0 => 0
    _>..1 => 1
  }
}

///|
#warnings("-syntax_lint")
fn descending_character(x : Char) -> Bool {
  x is 'z'>=..'a'
}

///|
fn unsigned_empty(x : UInt) -> Bool {
  x is _..<0U
}

///|
fn unsigned_full(x : UInt) -> Bool {
  x is 0U..<=0xffff_ffffU
}

///|
fn signed_full(x : Int64) -> Bool {
  x is -9223372036854775808L..<=9223372036854775807L
}

///|
fn signed_empty(x : Int64) -> Bool {
  x is _..<-9223372036854775808L
}

///|
fn all_values(x : Int) -> Bool {
  x is _..<_
}

///|
fn unsigned64_full(x : UInt64) -> Bool {
  x is 0UL..<=18446744073709551615UL
}

///|
fn unsigned64_empty(x : UInt64) -> Bool {
  x is _..<0UL
}

///|
fn check_range_condition(condition : Bool) -> Unit {
  if !condition {
    abort("range pattern mismatch")
  }
}

///|
fn check_range_membership() -> Unit {
  for x in [-2147483648, -3, -2, -1, 0, 1, 2, 3, 4, 2147483647] {
    check_range_condition(canonical(x) == (-2 <= x && x <= 3))
    check_range_condition(legacy(x) == canonical(x))
    check_range_condition(singleton(x) == (x == 3))
    let sign = if x < 0 { -1 } else if x == 0 { 0 } else { 1 }
    check_range_condition(unbounded(x) == sign)
    check_range_condition(descending(x) == canonical(x))
    check_range_condition(descending_open(x) == (-2 <= x && x < 3))
    check_range_condition(descending_singleton(x) == (x == 3))
    check_range_condition(descending_unbounded(x) == sign)
    check_range_condition(all_values(x))
  }
  for x in ['`', 'a', 'z', '{', '\u{10ffff}'] {
    check_range_condition(character(x) == ('a' <= x && x <= 'z'))
    check_range_condition(descending_character(x) == character(x))
  }
  for x in [0U, 1U, 0x7fff_ffffU, 0xffff_ffffU] {
    check_range_condition(!unsigned_empty(x))
    check_range_condition(unsigned_full(x))
  }
  for x in [-9223372036854775808L, -1L, 0L, 9223372036854775807L] {
    check_range_condition(!signed_empty(x))
    check_range_condition(signed_full(x))
  }
  for x in [0UL, 1UL, 9223372036854775808UL, 18446744073709551615UL] {
    check_range_condition(!unsigned64_empty(x))
    check_range_condition(unsigned64_full(x))
  }
}

///|
fn main {
  check_range_membership()
  println("range membership checks passed")
}
