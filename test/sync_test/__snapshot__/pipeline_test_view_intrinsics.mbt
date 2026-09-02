///|
#intrinsic("%array.length")
fn[T] array_length(arr : Array[T]) -> Int {
  arr.length()
}

///|
#intrinsic("%array.get")
fn[T] array_get(arr : Array[T], index : Int) -> T {
  arr[index]
}

///|
#intrinsic("%array.unsafe_get")
fn[T] array_unsafe_get(arr : Array[T], index : Int) -> T {
  arr.unsafe_get(index)
}

///|
#intrinsic("%array.set")
fn[T] array_set(arr : Array[T], index : Int, value : T) -> Unit {
  arr[index] = value
}

///|
#intrinsic("%array.unsafe_set")
fn[T] array_unsafe_set(arr : Array[T], index : Int, value : T) -> Unit {
  arr.unsafe_set(index, value)
}

///|
#intrinsic("%arrayview.length")
fn[T] arrayview_length(view : ArrayView[T]) -> Int {
  view.length()
}

///|
#intrinsic("%arrayview.get")
fn[T] arrayview_get(view : ArrayView[T], index : Int) -> T {
  view[index]
}

///|
#intrinsic("%arrayview.unsafe_get")
fn[T] arrayview_unsafe_get(view : ArrayView[T], index : Int) -> T {
  view.unsafe_get(index)
}

///|
#intrinsic("%bytesview.length")
fn bytesview_length(view : BytesView) -> Int {
  view.length()
}

///|
#intrinsic("%bytesview.get")
fn bytesview_get(view : BytesView, index : Int) -> Byte {
  view[index]
}

///|
#intrinsic("%bytesview.unsafe_get")
fn bytesview_unsafe_get(view : BytesView, index : Int) -> Byte {
  view.unsafe_get(index)
}

///|
#intrinsic("%stringview.length")
fn stringview_length(view : StringView) -> Int {
  view.length()
}

///|
#intrinsic("%stringview.get")
fn stringview_get(view : StringView, index : Int) -> UInt16 {
  view[index]
}

///|
#intrinsic("%stringview.unsafe_get")
fn stringview_unsafe_get(view : StringView, index : Int) -> UInt16 {
  view.unsafe_get(index)
}

///|
#intrinsic("%mutarrayview.length")
fn[T] mutarrayview_length(view : MutArrayView[T]) -> Int {
  view.length()
}

///|
#intrinsic("%mutarrayview.get")
fn[T] mutarrayview_get(view : MutArrayView[T], index : Int) -> T {
  view[index]
}

///|
#intrinsic("%mutarrayview.unsafe_get")
fn[T] mutarrayview_unsafe_get(view : MutArrayView[T], index : Int) -> T {
  view.unsafe_get(index)
}

///|
#intrinsic("%mutarrayview.set")
fn[T] mutarrayview_set(view : MutArrayView[T], index : Int, value : T) -> Unit {
  view[index] = value
}

///|
#intrinsic("%mutarrayview.unsafe_set")
fn[T] mutarrayview_unsafe_set(
  view : MutArrayView[T],
  index : Int,
  value : T,
) -> Unit {
  view.unsafe_set(index, value)
}

///|
fn main {
  let arr = [10, 20, 30, 40]
  println(array_length(arr))
  println(array_get(arr, 0))
  println(array_unsafe_get(arr, 2))
  array_set(arr, 1, 25)
  array_unsafe_set(arr, 3, 45)
  println(array_get(arr, 1))
  println(array_unsafe_get(arr, 3))

  let av = arr[1:4]
  println(arrayview_length(av))
  println(arrayview_get(av, 0))
  println(arrayview_unsafe_get(av, 2))

  let bytes = b"\x01\x02\x03\x04"
  let bv = bytes[1:4]
  println(bytesview_length(bv))
  println(bytesview_get(bv, 0).to_int())
  println(bytesview_unsafe_get(bv, 2).to_int())

  let sv = "abcd"[1:3]
  println(stringview_length(sv))
  println(stringview_get(sv, 0).to_int())
  println(stringview_unsafe_get(sv, 1).to_int())

  let mut_arr = [1, 2, 3, 4]
  let mv = mut_arr.mut_view(start=1, end=4)
  println(mutarrayview_length(mv))
  println(mutarrayview_get(mv, 0))
  println(mutarrayview_unsafe_get(mv, 1))
  mutarrayview_set(mv, 1, 20)
  mutarrayview_unsafe_set(mv, 2, 40)
  println(mutarrayview_get(mv, 1))
  println(mut_arr[2])
  println(mut_arr[3])

  let slice_arr = [5, 6, 7, 8]
  let arr_all = slice_arr[:]
  println(arrayview_length(arr_all))
  let arr_tail = slice_arr[1:]
  println(arrayview_get(arr_tail, 0))
  let arr_head = slice_arr[:3]
  println(arrayview_get(arr_head, 2))
  let arr_mid = slice_arr[1:3]
  println(arrayview_length(arr_mid))

  let fixed : FixedArray[Int] = [10, 20, 30, 40]
  let fixed_view = fixed[1:3]
  println(arrayview_get(fixed_view, 0))
  let ro_array : ReadOnlyArray[Int] = [11, 22, 33, 44]
  let readonly_view = ro_array[2:]
  println(arrayview_get(readonly_view, 0))
  let nested_view = arr_mid[1:]
  println(arrayview_get(nested_view, 0))

  let nested_mut_view = mv.mut_view(start=1, end=3)
  mutarrayview_set(nested_mut_view, 0, 200)
  println(mut_arr[2])
  let readonly_mut_view = mv[1:3]
  println(arrayview_get(readonly_mut_view, 0))
  let fixed_mut_view = fixed.mut_view(start=1, end=3)
  mutarrayview_set(fixed_mut_view, 0, 2000)
  println(fixed[1])

  let bytes_all = bytes[:]
  println(bytesview_length(bytes_all))
  let bytes_tail = bytes[1:]
  println(bytesview_get(bytes_tail, 0).to_int())
  let bytes_head = bytes[:3]
  println(bytesview_get(bytes_head, 2).to_int())
  let bytes_mid = bytes[1:3]
  println(bytesview_length(bytes_mid))
  let nested_bytes = bytes_mid[1:]
  println(bytesview_get(nested_bytes, 0).to_int())
}
