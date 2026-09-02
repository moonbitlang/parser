///|
#intrinsic("%array.view")
fn[T] array_view_intrinsic(
  source : Array[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%array.unsafe_view")
fn[T] array_unsafe_view_intrinsic(
  source : Array[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%array.mut_view")
fn[T] array_mut_view_intrinsic(
  source : Array[T],
  start? : Int = 0,
  end? : Int,
) -> MutArrayView[T] {
  match end {
    None => source.mut_view(start=start)
    Some(end) => source.mut_view(start=start, end=end)
  }
}

///|
#intrinsic("%array.unsafe_mut_view")
fn[T] array_unsafe_mut_view_intrinsic(
  source : Array[T],
  start? : Int = 0,
  end? : Int,
) -> MutArrayView[T] {
  match end {
    None => source.mut_view(start=start)
    Some(end) => source.mut_view(start=start, end=end)
  }
}

///|
#intrinsic("%fixedarray.view")
fn[T] fixedarray_view_intrinsic(
  source : FixedArray[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%fixedarray.unsafe_view")
fn[T] fixedarray_unsafe_view_intrinsic(
  source : FixedArray[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%fixedarray.mut_view")
fn[T] fixedarray_mut_view_intrinsic(
  source : FixedArray[T],
  start? : Int = 0,
  end? : Int,
) -> MutArrayView[T] {
  match end {
    None => source.mut_view(start=start)
    Some(end) => source.mut_view(start=start, end=end)
  }
}

///|
#intrinsic("%fixedarray.unsafe_mut_view")
fn[T] fixedarray_unsafe_mut_view_intrinsic(
  source : FixedArray[T],
  start? : Int = 0,
  end? : Int,
) -> MutArrayView[T] {
  match end {
    None => source.mut_view(start=start)
    Some(end) => source.mut_view(start=start, end=end)
  }
}

///|
#intrinsic("%readonlyarray.view")
fn[T] readonlyarray_view_intrinsic(
  source : ReadOnlyArray[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%readonlyarray.unsafe_view")
fn[T] readonlyarray_unsafe_view_intrinsic(
  source : ReadOnlyArray[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%arrayview.view")
fn[T] arrayview_view_intrinsic(
  source : ArrayView[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%arrayview.unsafe_view")
fn[T] arrayview_unsafe_view_intrinsic(
  source : ArrayView[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%bytes.view")
fn bytes_view_intrinsic(
  source : Bytes,
  start? : Int = 0,
  end? : Int,
) -> BytesView {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%bytes.unsafe_view")
fn bytes_unsafe_view_intrinsic(
  source : Bytes,
  start? : Int = 0,
  end? : Int,
) -> BytesView {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%bytesview.view")
fn bytesview_view_intrinsic(
  source : BytesView,
  start? : Int = 0,
  end? : Int,
) -> BytesView {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%bytesview.unsafe_view")
fn bytesview_unsafe_view_intrinsic(
  source : BytesView,
  start? : Int = 0,
  end? : Int,
) -> BytesView {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%mutarrayview.view")
fn[T] mutarrayview_view_intrinsic(
  source : MutArrayView[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%mutarrayview.unsafe_view")
fn[T] mutarrayview_unsafe_view_intrinsic(
  source : MutArrayView[T],
  start? : Int = 0,
  end? : Int,
) -> ArrayView[T] {
  match end {
    None => source[start:]
    Some(end) => source[start:end]
  }
}

///|
#intrinsic("%mutarrayview.mut_view")
fn[T] mutarrayview_mut_view_intrinsic(
  source : MutArrayView[T],
  start? : Int = 0,
  end? : Int,
) -> MutArrayView[T] {
  match end {
    None => source.mut_view(start=start)
    Some(end) => source.mut_view(start=start, end=end)
  }
}

///|
#intrinsic("%mutarrayview.unsafe_mut_view")
fn[T] mutarrayview_unsafe_mut_view_intrinsic(
  source : MutArrayView[T],
  start? : Int = 0,
  end? : Int,
) -> MutArrayView[T] {
  match end {
    None => source.mut_view(start=start)
    Some(end) => source.mut_view(start=start, end=end)
  }
}

///|
fn check_array_view(view : ArrayView[Int], first : Int, length : Int) -> Unit {
  guard view.length() == length && view[0] == first else {
    abort("unexpected ArrayView")
  }
}

///|
fn check_mut_array_view(
  view : MutArrayView[Int],
  first : Int,
  length : Int,
) -> Unit {
  guard view.length() == length && view[0] == first else {
    abort("unexpected MutArrayView")
  }
}

///|
fn check_bytes_view(view : BytesView, first : Byte, length : Int) -> Unit {
  guard view.length() == length && view[0] == first else {
    abort("unexpected BytesView")
  }
}

///|
fn main {
  let array = [0, 10, 20, 30]
  check_array_view(array_view_intrinsic(array), 0, 4)
  check_array_view(array_view_intrinsic(array, start=1), 10, 3)
  check_array_view(array_view_intrinsic(array, end=3), 0, 3)
  check_array_view(array_view_intrinsic(array, start=1, end=3), 10, 2)
  check_array_view(array_unsafe_view_intrinsic(array), 0, 4)
  check_array_view(array_unsafe_view_intrinsic(array, start=1), 10, 3)
  check_array_view(array_unsafe_view_intrinsic(array, end=3), 0, 3)
  check_array_view(
    array_unsafe_view_intrinsic(array, start=1, end=3),
    10,
    2,
  )

  check_mut_array_view(array_mut_view_intrinsic(array, start=1, end=3), 10, 2)
  check_mut_array_view(
    array_unsafe_mut_view_intrinsic(array, start=1, end=3),
    10,
    2,
  )

  let fixed : FixedArray[Int] = [1, 11, 21, 31]
  check_array_view(fixedarray_view_intrinsic(fixed, start=1, end=3), 11, 2)
  check_array_view(
    fixedarray_unsafe_view_intrinsic(fixed, start=1, end=3),
    11,
    2,
  )
  check_mut_array_view(
    fixedarray_mut_view_intrinsic(fixed, start=1, end=3),
    11,
    2,
  )
  check_mut_array_view(
    fixedarray_unsafe_mut_view_intrinsic(fixed, start=1, end=3),
    11,
    2,
  )

  let readonly_array : ReadOnlyArray[Int] = [2, 12, 22, 32]
  check_array_view(
    readonlyarray_view_intrinsic(readonly_array, start=1, end=3),
    12,
    2,
  )
  check_array_view(
    readonlyarray_unsafe_view_intrinsic(readonly_array, start=1, end=3),
    12,
    2,
  )

  let array_view = array_view_intrinsic(array, start=1, end=4)
  check_array_view(arrayview_view_intrinsic(array_view, start=1, end=2), 20, 1)
  check_array_view(
    arrayview_unsafe_view_intrinsic(array_view, start=1, end=2),
    20,
    1,
  )

  let bytes = b"\x03\x0d\x17\x21"
  check_bytes_view(bytes_view_intrinsic(bytes, start=1, end=3), b'\x0d', 2)
  check_bytes_view(
    bytes_unsafe_view_intrinsic(bytes, start=1, end=3),
    b'\x0d',
    2,
  )

  let bytes_view = bytes_view_intrinsic(bytes, start=1, end=4)
  check_bytes_view(bytesview_view_intrinsic(bytes_view, start=1, end=2), b'\x17', 1)
  check_bytes_view(
    bytesview_unsafe_view_intrinsic(bytes_view, start=1, end=2),
    b'\x17',
    1,
  )

  let mut_array_view = array_mut_view_intrinsic(array, start=1, end=4)
  check_array_view(
    mutarrayview_view_intrinsic(mut_array_view, start=1, end=2),
    20,
    1,
  )
  check_array_view(
    mutarrayview_unsafe_view_intrinsic(mut_array_view, start=1, end=2),
    20,
    1,
  )
  let start = 1
  let end = 2
  check_mut_array_view(
    mutarrayview_mut_view_intrinsic(mut_array_view, start~, end~),
    20,
    1,
  )
  check_mut_array_view(
    mutarrayview_unsafe_mut_view_intrinsic(mut_array_view, end=2, start=1),
    20,
    1,
  )

  println("ok")
}
