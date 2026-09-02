// A raising result and a capture-free FuncRef must both use the recursively
// flattened three-slot ABI: i64 (inner.wide), i32 (inner.narrow), i32 (tail).

///|
#valtype
struct NestedErrorInner {
  wide : Int64
  narrow : Int
}

///|
#valtype
struct NestedErrorOuter {
  inner : NestedErrorInner
  tail : Int
}

///|
suberror NestedFailure {
  NestedFailure(Int)
}

///|
fn make_nested_error_value(fail : Bool) -> NestedErrorOuter raise NestedFailure {
  if fail {
    raise NestedFailure(7)
  } else {
    NestedErrorOuter::{
      inner: NestedErrorInner::{ wide: 40L, narrow: 2 },
      tail: 3,
    }
  }
}

///|
let sum_nested_error_value : FuncRef[(NestedErrorOuter) -> Int64] = value =>
  value.inner.wide + value.inner.narrow.to_int64() + value.tail.to_int64()

///|
fn nested_error_total(fail : Bool) -> Int64 {
  let value =
    try make_nested_error_value(fail)
    catch {
      NestedFailure(code) => NestedErrorOuter::{
        inner: NestedErrorInner::{ wide: code.to_int64() * 10L, narrow: 8 },
        tail: 9,
      }
    } noraise {
      value => value
    }
  sum_nested_error_value(value)
}

///|
fn main {
  let success = nested_error_total(false)
  let failure = nested_error_total(true)
  if success != 45L || failure != 87L {
    abort("nested value-type error ABI failure")
  }
}
