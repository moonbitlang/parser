///|
/// The complete `with` pattern must be parenthesized.
fn missing_outer_parentheses(x : Int?) -> Int {
  match x {
    Some(a) | None with a = 0 => a
  }
}

///|
/// Bindings are no longer enclosed in a second pair of parentheses.
fn obsolete_binding_parentheses(x : Int?) -> Int {
  match x {
    Some(a) | (None with (a = 0, b = 1)) => a + b
  }
}

///|
/// A `with` pattern must contain at least one binding.
fn empty_bindings(x : Int?) -> Int {
  match x {
    Some(a) | (None with) => a
  }
}

///|
/// Non-constant `with` defaults are accepted by the parser. Their names and
/// types are checked later during typing.
fn non_constant_defaults(x : Int?) -> Unit {
  match x {
    _ | (_ with b = [], c = f(), e = m.field) => ignore(b)
  }
}
