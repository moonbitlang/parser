///|
fn make_u16_arr(a : UInt16, b : UInt16, c : UInt16) -> FixedArray[UInt16] {
  [a, b, c, 65535]
}

///|
fn make_u16_arr2(a : UInt16, b : UInt16, c : UInt16) -> FixedArray[UInt16] {
  [a, b, c, 65535]
}

///|
fn make_u16_arr3(a : UInt16, b : UInt16, c : UInt16) -> Array[UInt16] {
  [a, b, c, 65535]
}

///|
fn main {
  let arr1 = make_u16_arr(0, 65535, 32767)
  let arr2 = make_u16_arr2(0, 65535, 32767)
  let arr3 = make_u16_arr3(0, 65535, 32767)
  debug(arr1)
  debug(arr2)
  debug(arr3)
  let x = arr1[1]
  println(x)
  arr1[2] = 65535
  debug(arr1)
  let arr4 : FixedArray[UInt16] = FixedArray::make(5, 42)
  debug(arr4)
  arr3.push(0)
  arr3.push(1)
  arr3.push(2)
  arr3.push(3)
  arr3.push(4)
  debug(arr3)
  let arr5 : FixedArray[UInt16] = [
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 65535, 65534, 65533, 65532, 65531,
  ]
  debug(arr5)
}
