// Arbitrary defaults are evaluated in the scope surrounding the pattern, but
// only on the alternative that supplies the default.
///|
fn lazy_enclosing_default(x : Int?, fallback : Int) -> Unit {
  let mut evaluations = 0
  let result = match x {
    Some(value) | (None with value = {
      evaluations += 1
      fallback * 2 + evaluations
    }) => value
  }
  println(result)
  println(evaluations)
}

// A local binding in the default may shadow the `with` binder. In particular,
// or-pattern binder renaming must not descend into the default expression.
///|
fn shadowed_local_default(x : Int?, fallback : Int) -> Int {
  match x {
    Some(value) | (None with value = {
      let value = fallback + 1
      value + value
    }) => value
  }
}

///|
enum DefaultSource {
  Bound(Int)
  MissingFirst
  MissingSecond
}

// The `with` is directly below the outer or-pattern and is reached from both
// arms of its nested or-pattern. The match compiler therefore copies this
// default into multiple success paths. Its local let, recursive function,
// parameters, loop state, and loop label must all be alpha-renamed independently
// in each copy.
///|
fn duplicated_binder_rich_default(x : DefaultSource, fallback : Int) -> Unit {
  let mut evaluations = 0
  let result = match x {
    Bound(value) | (MissingFirst | MissingSecond with value = {
      evaluations += 1
      let local_value = fallback + evaluations
      fn recurse(n : Int) -> Int {
        if n == 0 { local_value } else { recurse(n - 1) + 1 }
      }
      for current = recurse(2) {
        if current >= local_value + 4 {
          break current
        } else {
          continue current + 1
        }
      }
    }) => value
  }
  println(result)
  println(evaluations)
}

// Multiple defaults are evaluated once each and in source order.
///|
fn ordered_defaults(seed : Int) -> Unit {
  let mut state = seed
  let input : (Int, Int)? = None
  match input {
    Some((first, second)) | (None with
      first = {
        state = state * 10 + 1
        state
      },
      second = {
        state = state * 10 + 2
        state
      },
    ) => {
      println(first)
      println(second)
      println(state)
    }
  }
}

// A default in an irrefutable let pattern may use the enclosing scope, and a
// binder local to the default is not a free reference to the pattern binder.
///|
fn default_in_let(pair : (Int, Int), fallback : Int) -> Int {
  let input : (Int, Int, Int)? = None
  let (Some((left, right, extra)) | (None with
    left = pair.0,
    right = pair.1,
    extra = {
      let extra = fallback + 1
      extra * 2
    },
  )) = input
  left + right + extra
}

// The same enclosing-scope rule applies to patterns in `is` expressions.
///|
fn default_in_is(x : Int?, fallback : Int) -> Int {
  if x is (Some(value) | (None with value = (fallback + 1))) {
    value
  } else {
    -1
  }
}

// A `try` inside the default establishes an error context local to the default.
///|
fn handled_error_in_default(x : Int?, fallback : Int) -> Int {
  match x {
    Some(value) | (None with value = {
      (if fallback < 0 { fail("negative") } else { fallback + 1 }) catch {
        _ => 99
      }
    }) => value * 2
  }
}

// A `for` inside the default establishes its own break/continue context. The
// surrounding or-pattern also makes the match compiler duplicate this default.
///|
fn nested_for_in_duplicated_default(x : DefaultSource, fallback : Int) -> Int {
  match x {
    Bound(value) | (MissingFirst | MissingSecond with value = {
      for i = 0; i < 4; {
        if i == 2 {
          break fallback + i
        }
        continue i + 1
      } nobreak {
        -1
      }
    }) => value
  }
}

///|
fn main {
  lazy_enclosing_default(Some(7), 20)
  lazy_enclosing_default(None, 20)
  println(shadowed_local_default(Some(7), 20))
  println(shadowed_local_default(None, 20))
  duplicated_binder_rich_default(MissingFirst, 10)
  duplicated_binder_rich_default(MissingSecond, 10)
  ordered_defaults(1)
  println(default_in_let((2, 3), 4))
  println(default_in_is(Some(8), 4))
  println(default_in_is(None, 4))
  println(handled_error_in_default(Some(3), -1))
  println(handled_error_in_default(None, -1))
  println(handled_error_in_default(None, 4))
  println(nested_for_in_duplicated_default(MissingFirst, 4))
  println(nested_for_in_duplicated_default(MissingSecond, -1))
}
