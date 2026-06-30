fn check_bytes_nonzero() -> Bool {
  let bytes = Bytes::make(6, b'\xff')
  bytes is [b'\xff', b'\xff', b'\xff', b'\xff', b'\xff', b'\xff']
}

fn check_bytes_zero() -> Bool {
  let bytes = Bytes::make(6, b'\x00')
  bytes is [b'\x00', b'\x00', b'\x00', b'\x00', b'\x00', b'\x00']
}

fn check_int_zero() -> Bool {
  let xs : FixedArray[Int] = FixedArray::make(4, 0)
  xs[0] == 0 && xs[1] == 0 && xs[2] == 0 && xs[3] == 0
}

fn check_int64_zero() -> Bool {
  let xs : FixedArray[Int64] = FixedArray::make(4, 0L)
  xs[0] == 0L && xs[1] == 0L && xs[2] == 0L && xs[3] == 0L
}

fn check_uint16_zero() -> Bool {
  let xs : FixedArray[UInt16] = FixedArray::make(4, (0 : UInt16))
  xs[0] == 0 && xs[1] == 0 && xs[2] == 0 && xs[3] == 0
}

fn main {
  if check_bytes_nonzero() &&
    check_bytes_zero() &&
    check_int_zero() &&
    check_int64_zero() &&
    check_uint16_zero() {
    println("ok")
  } else {
    println("bad")
  }
}
