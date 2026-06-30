///|
pub extern "wasm" fn[T] use_fixedarray_generic(arr : FixedArray[T]) =
  "(func (param $arr i32))"

///|
extern "wasm" fn legacy[T](
  arr : FixedArray[T]
) = "(func (param $arr i32))"
