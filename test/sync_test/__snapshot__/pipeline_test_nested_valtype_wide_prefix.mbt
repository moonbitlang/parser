// Regression test for nested value-enum RC ranges. A 64-bit field occupies two
// encoded words in an outer value enum even though it is one wasm value.
// Keeping the low word equal to the nested reference constructor's tag makes
// an off-by-one range treat the nested enum header as a heap reference.

///|
#valtype
enum NestedRef {
  NoRef
  HasRef(String)
}

///|
#valtype
enum WidePrefix {
  AfterInt64(Int64, NestedRef)
  AfterUInt64(UInt64, NestedRef)
  AfterDouble(Double, NestedRef)
}

///|
fn check(condition : Bool) -> Unit {
  if !condition {
    abort("nested value enum wide-prefix failure")
  }
}

///|
fn check_int64_prefix() -> Unit {
  let text = (64).to_string() + "-bit"
  let values : FixedArray[WidePrefix] = FixedArray::make(
    2,
    AfterInt64(1L, HasRef(text)),
  )
  match values[1] {
    AfterInt64(n, HasRef(s)) => check(n == 1L && s == "64-bit")
    _ => abort("unexpected Int64 value")
  }
}

///|
fn check_uint64_prefix() -> Unit {
  let text = (64).to_string() + "-ubit"
  let values : FixedArray[WidePrefix] = FixedArray::make(
    2,
    AfterUInt64(1UL, HasRef(text)),
  )
  match values[1] {
    AfterUInt64(n, HasRef(s)) => check(n == 1UL && s == "64-ubit")
    _ => abort("unexpected UInt64 value")
  }
}

///|
fn check_double_prefix() -> Unit {
  let text = (64).to_string() + "-double"
  let bits = 1L
  let values : FixedArray[WidePrefix] = FixedArray::make(
    2,
    AfterDouble(bits.reinterpret_as_double(), HasRef(text)),
  )
  match values[1] {
    AfterDouble(n, HasRef(s)) =>
      check(n.reinterpret_as_int64() == bits && s == "64-double")
    _ => abort("unexpected Double value")
  }
}

///|
fn main {
  check_int64_prefix()
  check_uint64_prefix()
  check_double_prefix()
  println("ok")
}
