///|
fn aligned_bytes() -> Bytes {
  [
    0x7F,
    0x80,
    0x34,
    0x12,
    0x12,
    0x34,
    0xFF,
    0xFF,
    0x80,
    0x00,
    0x78,
    0x56,
    0x34,
    0x12,
    0x12,
    0x34,
    0x56,
    0x78,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0x80,
    0x00,
    0x00,
    0x00,
    0x08,
    0x07,
    0x06,
    0x05,
    0x04,
    0x03,
    0x02,
    0x01,
    0x01,
    0x02,
    0x03,
    0x04,
    0x05,
    0x06,
    0x07,
    0x08,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xFF,
    0xAA,
    0xBB,
  ]
}

///|
fn padded_aligned_bytes() -> Bytes {
  let payload = aligned_bytes()
  Bytes::makei(payload.length() + 2, i => {
    if i == 0 {
      b'\xEE'
    } else if i == payload.length() + 1 {
      b'\xCC'
    } else {
      payload[i - 1]
    }
  })
}

///|
fn expect_bytesview_aligned(data : BytesView) -> Unit {
  match data {
    [
      u8be(a),
      i8be(b),
      u16le(c),
      u16be(d),
      i16le(e),
      i16be(f),
      u32le(g),
      u32be(h),
      i32le(i),
      i32be(j),
      u64le(k),
      u64be(l),
      i64le(m),
      i64be(n),
      .. rest
    ] => {
      guard a == 0x7FU
      guard b == -128
      guard c == 0x1234U
      guard d == 0x1234U
      guard e == -1
      guard f == -32768
      guard g == 0x12345678U
      guard h == 0x12345678U
      guard i == -1
      guard j == -2147483648
      guard k == 0x0102030405060708UL
      guard l == 0x0102030405060708UL
      guard m == -1L
      guard n == -1L
      guard rest.length() == 2
      println("bytesview aligned ok")
    }
    _ => abort("bytesview aligned failed")
  }
}

///|
fn expect_arrayview_aligned(data : ArrayView[Byte]) -> Unit {
  match data {
    [
      u8be(a),
      i8be(b),
      u16le(c),
      u16be(d),
      i16le(e),
      i16be(f),
      u32le(g),
      u32be(h),
      i32le(i),
      i32be(j),
      u64le(k),
      u64be(l),
      i64le(m),
      i64be(n),
      .. rest
    ] => {
      guard a == 0x7FU
      guard b == -128
      guard c == 0x1234U
      guard d == 0x1234U
      guard e == -1
      guard f == -32768
      guard g == 0x12345678U
      guard h == 0x12345678U
      guard i == -1
      guard j == -2147483648
      guard k == 0x0102030405060708UL
      guard l == 0x0102030405060708UL
      guard m == -1L
      guard n == -1L
      guard rest.length() == 2
      println("arrayview aligned ok")
    }
    _ => abort("arrayview aligned failed")
  }
}

///|
fn combine_bytesview(value : Bytes) -> UInt {
  let mut result : UInt = 0
  for data = value.view() {
    if data is [u64le(x), .. rest] {
      result += x.to_uint()
      result += (x >> 32).to_uint()
      continue rest
    } else if data is [u32le(x), .. rest] {
      result += x
      continue rest
    } else {
      if data is [b0, .. data] {
        result += b0.to_uint()
        if data is [b1, .. data] {
          result += b1.to_uint()
          if data is [b2, ..] {
            result += b2.to_uint()
          }
        }
      }
      break
    }
  }
  result
}

///|
fn combine_arrayview(data : ArrayView[Byte]) -> UInt {
  let mut result : UInt = 0
  for data = data {
    if data is [u64le(x), .. rest] {
      result += x.to_uint()
      result += (x >> 32).to_uint()
      continue rest
    } else if data is [u32le(x), .. rest] {
      result += x
      continue rest
    } else {
      if data is [b0, .. data] {
        result += b0.to_uint()
        if data is [b1, .. data] {
          result += b1.to_uint()
          if data is [b2, ..] {
            result += b2.to_uint()
          }
        }
      }
      break
    }
  }
  result
}

///|
fn main {
  let data = aligned_bytes()
  expect_bytesview_aligned(data.view())
  let padded = padded_aligned_bytes()
  expect_bytesview_aligned(padded.view(start=1, end=padded.length() - 1))
  let arr = Array::makei(data.length(), i => data[i])
  let view : ArrayView[Byte] = arr
  expect_arrayview_aligned(view)
  let padded_arr = Array::makei(padded.length(), i => padded[i])
  let padded_view : ArrayView[Byte] = padded_arr
  expect_arrayview_aligned(padded_view[1:padded_view.length() - 1])
  let loop_data = Bytes::makei(23, i => i.to_byte())
  println(combine_bytesview(loop_data))
  let loop_arr = Array::makei(loop_data.length(), i => loop_data[i])
  let loop_view : ArrayView[Byte] = loop_arr
  println(combine_arrayview(loop_view))
}
