///|
/// Parentheses delimit the complete `with` pattern, so this default belongs
/// only to the `None` alternative.
fn parenthesized_alternative(x : Int?) -> Int {
  match x {
    Some(a) | (None with a = 0) => a
  }
}

///|
/// An alias outside the parentheses applies to the complete `with` pattern.
fn alias_outside_with(x : Int?) -> Int {
  match x {
    (Some(a) with b = 0) as c => b
  }
}

///|
/// An alias before `with` is nested inside the complete `with` pattern.
fn alias_inside_with(x : Int?) -> Int {
  match x {
    (Some(a) as c with b = 0) => b
  }
}

///|
/// Per-alternative defaults across a three-way or-pattern.
fn per_alternative(x : Int?) -> Int {
  match x {
    (Some(a) with b = 0) | (None with a = 0, b = 0) => a + b
  }
}

///|
/// A `with` pattern is an atomic non-final tuple element.
fn nested_in_tuple(p : (Int?, Int)) -> Int {
  match p {
    ((Some(a) with x = 0), y) => a + x + y
    _ => 0
  }
}

///|
/// `with` on an alternative nested inside a constructor pattern.
fn nested_or_in_constr(x : Foo) -> Int {
  match x {
    Foo(A(y) | (B with y = 0)) => y
  }
}

///|
/// The operand before `with` is an or-pattern, so the parentheses make the
/// default apply to both nested alternatives as one outer alternative.
fn wraps_or_pattern(x : Int?) -> Int {
  match x {
    Some(a) | (None | None with a = 0) => a
  }
}

///|
/// A range pattern can be the operand of a parenthesized `with` pattern.
fn range_operand(x : Int) -> Int {
  match x {
    (1..=5 with y = 0) => y
    _ => 1
  }
}
