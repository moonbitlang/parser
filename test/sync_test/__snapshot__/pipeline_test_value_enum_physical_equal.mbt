///|
#valtype
enum ScalarValue {
  ScalarEmpty
  ScalarPair(Int, Int)
}

///|
#valtype
enum RefValue {
  RefEmpty
  RefText(String)
  RefPair(String, Int)
}

///|
fn scalar_physical_equal(lhs : ScalarValue, rhs : ScalarValue) -> Bool {
  physical_equal(lhs, rhs)
}

///|
fn ref_physical_equal(lhs : RefValue, rhs : RefValue) -> Bool {
  physical_equal(lhs, rhs)
}

///|
fn main {
  let scalar = ScalarPair(1, 2)
  println(scalar_physical_equal(scalar, ScalarPair(1, 3)))
  println(scalar_physical_equal(scalar, ScalarEmpty))

  let text = "same" + " reference"
  let ref_value = RefText(text)
  println(ref_physical_equal(ref_value, RefText("different" + " reference")))
  println(ref_physical_equal(ref_value, RefPair(text, 0)))
}
