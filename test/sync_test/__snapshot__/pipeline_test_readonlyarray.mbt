///|
fn[T] unsafe_reinterpret_to_fixed_array(
  self : ReadOnlyArray[T],
) -> FixedArray[T] = "%identity"

///|
#alias("_[_]")
fn[T] ReadOnlyArray::at(self : ReadOnlyArray[T], index : Int) -> T {
  self.unsafe_reinterpret_to_fixed_array()[index]
}

///|
#alias("_[_:_]")
fn[T] ReadOnlyArray::sub(
  self : ReadOnlyArray[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  self.unsafe_reinterpret_to_fixed_array().sub(start~, end?)
}

///|
fn f(xs : ArrayView[Int]) -> Unit {
  println(xs[1:])
}

///|
fn main {
  let arr : ReadOnlyArray[Int] = [1, 2, 3]
  println(arr[0])
  f(arr)
  f(arr[:2])
}
