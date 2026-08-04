struct S {
  x : Int
} derive(Debug)

fn S::S(x: Int) -> S {
  println(x)
  S::{ x }
}

fn f(b: Bool) -> Unit {
  match b {
    (true with s = S(1)) | (false with s = S(2)) => debug(s)
  }
}

enum E {
  A(Bool)
  B
  C(S)
}

fn structurally_bound_or_defaulted(value : S?) -> Unit {
  match value {
    Some(s) | (None with s = S(3)) => debug(s)
  }
}

fn unmatched_alternative_has_no_effect(value : E) -> Unit {
  match value {
    (A(true) with s = S(4)) | C(s) => debug(s)
    _ => println("unmatched")
  }
}

fn guard_uses_default(value : E) -> Unit {
  match value {
    (A(true) with s = S(5)) | C(s) if s.x > 0 => debug(s)
    _ => println("guard fallback")
  }
}

fn failed_guard_evaluates_default_once(value : E) -> Unit {
  match value {
    (A(true) with s = S(-6)) | C(s) if s.x > 0 => debug(s)
    _ => println("guard fallback")
  }
}

enum PairInput {
  UseDefaults
  ExistingPair(S, S)
  NoPair
}

fn defaults_are_evaluated_in_order(value : PairInput) -> Unit {
  match value {
    (UseDefaults with first = S(7), second = S(8))
    | ExistingPair(first, second) => debug((first, second))
    NoPair => println("unmatched")
  }
}

enum Box {
  Box(S)
} derive(Debug)

enum BoxInput {
  MakeBox
  ExistingBox(Box)
  NoBox
}

fn nested_default_expression(value : BoxInput) -> Unit {
  match value {
    (MakeBox with box = Box(S(9))) | ExistingBox(box) => debug(box)
    NoBox => println("unmatched")
  }
}

fn is_pattern_only_evaluates_default_on_match(value : E) -> Unit {
  if value is ((A(true) with s = S(11)) | C(s)) {
    debug(s)
  } else {
    println("unmatched")
  }
}

fn guard_pattern_only_evaluates_default_on_match(value : E) -> Unit {
  guard value is ((A(true) with s = S(12)) | C(s)) else {
    println("guard fallback")
    return
  }
  debug(s)
}

fn main {
  f(true)
  f(false)
  structurally_bound_or_defaulted(Some(S::{ x: 30 }))
  structurally_bound_or_defaulted(None)
  unmatched_alternative_has_no_effect(A(false))
  unmatched_alternative_has_no_effect(B)
  unmatched_alternative_has_no_effect(A(true))
  guard_uses_default(A(true))
  guard_uses_default(B)
  failed_guard_evaluates_default_once(A(true))
  failed_guard_evaluates_default_once(B)
  defaults_are_evaluated_in_order(UseDefaults)
  defaults_are_evaluated_in_order(NoPair)
  nested_default_expression(MakeBox)
  nested_default_expression(NoBox)
  is_pattern_only_evaluates_default_on_match(A(true))
  is_pattern_only_evaluates_default_on_match(B)
  guard_pattern_only_evaluates_default_on_match(A(true))
  guard_pattern_only_evaluates_default_on_match(B)
}
