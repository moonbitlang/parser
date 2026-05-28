///|
fn fixedarray_as_bytes(arr : FixedArray[Byte]) -> Bytes = "%identity"

///|
fn string_from_uint16_fixedarray(arr : FixedArray[UInt16]) -> String = "%string.unsafe_from_uint16_fixedarray"

///|
fn make_bytesview(bytes : Bytes, start : Int, len : Int) -> BytesView = "%bytesview.make"

///|
fn put_utf16(dst : FixedArray[UInt16], offset : Int, codepoint : Int) -> Int {
  if codepoint <= 0xFFFF {
    dst.unsafe_set(offset, Int::to_uint16(codepoint))
    1
  } else {
    let ch = codepoint - 0x10000
    dst.unsafe_set(offset, Int::to_uint16((ch >> 10) + 0xD800))
    dst.unsafe_set(offset + 1, Int::to_uint16((ch & 0x3FF) + 0xDC00))
    2
  }
}

///|
#borrow(src)
#intrinsic("%utf16.len_from_utf8")
fn utf16_len_from_utf8(
  src : Bytes,
  src_offset : Int,
  src_length : Int,
) -> Int {
  // Length probes mirror runtime allocation behavior; they are not validators.
  let end = src_offset + src_length
  for index = src_offset, len = 0; index < end; {
    let byte = src.unsafe_get(index).to_int()
    if byte < 0x80 {
      continue index + 1, len + 1
    } else if byte < 0xC0 {
      continue index + 1, len
    } else if byte < 0xF0 {
      continue index + 1, len + 1
    } else {
      continue index + 1, len + 2
    }
  } nobreak {
    len
  }
}

///|
#borrow(src, dst)
#intrinsic("%utf8.decode_into_utf16")
fn utf8_decode_into_utf16(
  src : Bytes,
  src_offset : Int,
  src_length : Int,
  dst : FixedArray[UInt16],
  dst_offset : Int,
) -> Int {
  let view = make_bytesview(src, src_offset, src_length)
  for written = 0, bytes = view {
    match (written, bytes) {
      (written, []) => break written
      (
        written,
        [
          _..=0x7F as b0,
          _..=0x7F as b1,
          _..=0x7F as b2,
          _..=0x7F as b3,
          _..=0x7F as b4,
          _..=0x7F as b5,
          _..=0x7F as b6,
          _..=0x7F as b7,
          .. rest,
        ],
      ) => {
        dst.unsafe_set(dst_offset + written, Int::to_uint16(b0.to_int()))
        dst.unsafe_set(dst_offset + written + 1, Int::to_uint16(b1.to_int()))
        dst.unsafe_set(dst_offset + written + 2, Int::to_uint16(b2.to_int()))
        dst.unsafe_set(dst_offset + written + 3, Int::to_uint16(b3.to_int()))
        dst.unsafe_set(dst_offset + written + 4, Int::to_uint16(b4.to_int()))
        dst.unsafe_set(dst_offset + written + 5, Int::to_uint16(b5.to_int()))
        dst.unsafe_set(dst_offset + written + 6, Int::to_uint16(b6.to_int()))
        dst.unsafe_set(dst_offset + written + 7, Int::to_uint16(b7.to_int()))
        continue written + 8, rest
      }
      (written, [0..=0x7F as b, .. rest]) => {
        dst.unsafe_set(dst_offset + written, Int::to_uint16(b.to_int()))
        continue written + 1, rest
      }
      (written, [0xC2..=0xDF as b0, 0x80..=0xBF as b1, .. rest]) => {
        let ch = ((b0.to_int() & 0x1F) << 6) | (b1.to_int() & 0x3F)
        dst.unsafe_set(dst_offset + written, Int::to_uint16(ch))
        continue written + 1, rest
      }
      (
        written,
        [0xE0 as b0, 0xA0..=0xBF as b1, 0x80..=0xBF as b2, .. rest]
        | [0xE1..=0xEC as b0, 0x80..=0xBF as b1, 0x80..=0xBF as b2, .. rest]
        | [0xED as b0, 0x80..=0x9F as b1, 0x80..=0xBF as b2, .. rest]
        | [0xEE..=0xEF as b0, 0x80..=0xBF as b1, 0x80..=0xBF as b2, .. rest],
      ) => {
        let ch = ((b0.to_int() & 0x0F) << 12) |
          ((b1.to_int() & 0x3F) << 6) |
          (b2.to_int() & 0x3F)
        dst.unsafe_set(dst_offset + written, Int::to_uint16(ch))
        continue written + 1, rest
      }
      (
        written,
        [
          0xF0 as b0,
          0x90..=0xBF as b1,
          0x80..=0xBF as b2,
          0x80..=0xBF as b3,
          .. rest,
        ]
        | [
          0xF1..=0xF3 as b0,
          0x80..=0xBF as b1,
          0x80..=0xBF as b2,
          0x80..=0xBF as b3,
          .. rest,
        ]
        | [
          0xF4 as b0,
          0x80..=0x8F as b1,
          0x80..=0xBF as b2,
          0x80..=0xBF as b3,
          .. rest,
        ],
      ) => {
        let ch = ((b0.to_int() & 0x07) << 18) |
          ((b1.to_int() & 0x3F) << 12) |
          ((b2.to_int() & 0x3F) << 6) |
          (b3.to_int() & 0x3F)
        continue written + put_utf16(dst, dst_offset + written, ch), rest
      }
      (_, malformed) => break -(malformed.start_offset() - src_offset + 1)
    }
  }
}

///|
#borrow(src, dst)
#intrinsic("%utf8.decode_lossy_into_utf16")
fn utf8_decode_lossy_into_utf16(
  src : Bytes,
  src_offset : Int,
  src_length : Int,
  dst : FixedArray[UInt16],
  dst_offset : Int,
) -> Int {
  let view = make_bytesview(src, src_offset, src_length)
  for written = 0, bytes = view {
    match (written, bytes) {
      (written, []) => break written
      (
        written,
        [
          _..=0x7F as b0,
          _..=0x7F as b1,
          _..=0x7F as b2,
          _..=0x7F as b3,
          _..=0x7F as b4,
          _..=0x7F as b5,
          _..=0x7F as b6,
          _..=0x7F as b7,
          .. rest,
        ],
      ) => {
        dst.unsafe_set(dst_offset + written, Int::to_uint16(b0.to_int()))
        dst.unsafe_set(dst_offset + written + 1, Int::to_uint16(b1.to_int()))
        dst.unsafe_set(dst_offset + written + 2, Int::to_uint16(b2.to_int()))
        dst.unsafe_set(dst_offset + written + 3, Int::to_uint16(b3.to_int()))
        dst.unsafe_set(dst_offset + written + 4, Int::to_uint16(b4.to_int()))
        dst.unsafe_set(dst_offset + written + 5, Int::to_uint16(b5.to_int()))
        dst.unsafe_set(dst_offset + written + 6, Int::to_uint16(b6.to_int()))
        dst.unsafe_set(dst_offset + written + 7, Int::to_uint16(b7.to_int()))
        continue written + 8, rest
      }
      (written, [0..=0x7F as b, .. rest]) => {
        dst.unsafe_set(dst_offset + written, Int::to_uint16(b.to_int()))
        continue written + 1, rest
      }
      (written, [0xC2..=0xDF as b0, 0x80..=0xBF as b1, .. rest]) => {
        let ch = ((b0.to_int() & 0x1F) << 6) | (b1.to_int() & 0x3F)
        dst.unsafe_set(dst_offset + written, Int::to_uint16(ch))
        continue written + 1, rest
      }
      (
        written,
        [0xE0 as b0, 0xA0..=0xBF as b1, 0x80..=0xBF as b2, .. rest]
        | [0xE1..=0xEC as b0, 0x80..=0xBF as b1, 0x80..=0xBF as b2, .. rest]
        | [0xED as b0, 0x80..=0x9F as b1, 0x80..=0xBF as b2, .. rest]
        | [0xEE..=0xEF as b0, 0x80..=0xBF as b1, 0x80..=0xBF as b2, .. rest],
      ) => {
        let ch = ((b0.to_int() & 0x0F) << 12) |
          ((b1.to_int() & 0x3F) << 6) |
          (b2.to_int() & 0x3F)
        dst.unsafe_set(dst_offset + written, Int::to_uint16(ch))
        continue written + 1, rest
      }
      (
        written,
        [
          0xF0 as b0,
          0x90..=0xBF as b1,
          0x80..=0xBF as b2,
          0x80..=0xBF as b3,
          .. rest,
        ]
        | [
          0xF1..=0xF3 as b0,
          0x80..=0xBF as b1,
          0x80..=0xBF as b2,
          0x80..=0xBF as b3,
          .. rest,
        ]
        | [
          0xF4 as b0,
          0x80..=0x8F as b1,
          0x80..=0xBF as b2,
          0x80..=0xBF as b3,
          .. rest,
        ],
      ) => {
        let ch = ((b0.to_int() & 0x07) << 18) |
          ((b1.to_int() & 0x3F) << 12) |
          ((b2.to_int() & 0x3F) << 6) |
          (b3.to_int() & 0x3F)
        continue written + put_utf16(dst, dst_offset + written, ch), rest
      }
      (written, [0xE0, 0xA0..=0xBF, .. rest])
      | (written, [0xE1..=0xEC, 0x80..=0xBF, .. rest])
      | (written, [0xED, 0x80..=0x9F, .. rest])
      | (written, [0xEE..=0xEF, 0x80..=0xBF, .. rest])
      | (written, [0xF0, 0x90..=0xBF, 0x80..=0xBF, .. rest])
      | (written, [0xF1..=0xF3, 0x80..=0xBF, 0x80..=0xBF, .. rest])
      | (written, [0xF4, 0x80..=0x8F, 0x80..=0xBF, .. rest])
      | (written, [0xF0, 0x90..=0xBF, .. rest])
      | (written, [0xF1..=0xF3, 0x80..=0xBF, .. rest])
      | (written, [0xF4, 0x80..=0x8F, .. rest]) => {
        dst.unsafe_set(dst_offset + written, 0xFFFD)
        continue written + 1, rest
      }
      (written, [_, .. rest]) => {
        dst.unsafe_set(dst_offset + written, 0xFFFD)
        continue written + 1, rest
      }
    }
  }
}

///|
#borrow(src)
#intrinsic("%utf8.len_from_utf16")
fn utf8_len_from_utf16(
  src : String,
  src_offset : Int,
  src_length : Int,
) -> Int {
  // Length probes mirror runtime allocation behavior; they are not validators.
  let end = src_offset + src_length
  for index = src_offset, len = 0; index < end; {
    let code = src.unsafe_get(index)
    let code_int = code.to_int()
    if code_int < 0x80 {
      continue index + 1, len + 1
    } else if code_int < 0x800 {
      continue index + 1, len + 2
    } else if code.is_leading_surrogate() || code.is_trailing_surrogate() {
      continue index + 1, len + 2
    } else {
      continue index + 1, len + 3
    }
  } nobreak {
    len
  }
}

///|
#borrow(src, dst)
#intrinsic("%utf8.encode_from_utf16")
fn utf8_encode_from_utf16(
  src : String,
  src_offset : Int,
  src_length : Int,
  dst : FixedArray[Byte],
  dst_offset : Int,
) -> Int {
  let end = src_offset + src_length
  for index = src_offset, written = 0; index < end; {
    let unit = src.unsafe_get(index)
    let code = unit.to_int()
    if code < 0x80 {
      dst.unsafe_set(dst_offset + written, code.to_byte())
      continue index + 1, written + 1
    }
    if code < 0x800 {
      dst.unsafe_set(dst_offset + written, (0xC0 | (code >> 6)).to_byte())
      dst.unsafe_set(dst_offset + written + 1, (0x80 | (code & 0x3F)).to_byte())
      continue index + 1, written + 2
    }
    if unit.is_leading_surrogate() {
      if index + 1 < end {
        let trail = src.unsafe_get(index + 1)
        if trail.is_trailing_surrogate() {
          let scalar = ((code - 0xD800) << 10) +
            (trail.to_int() - 0xDC00) +
            0x10000
          dst.unsafe_set(
            dst_offset + written,
            (0xF0 | (scalar >> 18)).to_byte(),
          )
          dst.unsafe_set(
            dst_offset + written + 1,
            (0x80 | ((scalar >> 12) & 0x3F)).to_byte(),
          )
          dst.unsafe_set(
            dst_offset + written + 2,
            (0x80 | ((scalar >> 6) & 0x3F)).to_byte(),
          )
          dst.unsafe_set(
            dst_offset + written + 3,
            (0x80 | (scalar & 0x3F)).to_byte(),
          )
          continue index + 2, written + 4
        }
      }
      break -(index - src_offset + 1)
    }
    if unit.is_trailing_surrogate() {
      break -(index - src_offset + 1)
    }
    dst.unsafe_set(dst_offset + written, (0xE0 | (code >> 12)).to_byte())
    dst.unsafe_set(
      dst_offset + written + 1,
      (0x80 | ((code >> 6) & 0x3F)).to_byte(),
    )
    dst.unsafe_set(dst_offset + written + 2, (0x80 | (code & 0x3F)).to_byte())
    continue index + 1, written + 3
  } nobreak {
    written
  }
}

///|
fn encode_utf16_view_to_utf8(view : StringView) -> Bytes {
  let src_length = view.length()
  let src_offset = view.start_offset()
  let src = view.data()
  let len = utf8_len_from_utf16(src, src_offset, src_length)
  let dst = FixedArray::make(len, b'\x00')
  ignore(utf8_encode_from_utf16(src, src_offset, src_length, dst, 0))
  fixedarray_as_bytes(dst)
}

///|
fn decode_utf8_to_utf16_strict(src : BytesView) -> Result[String, Int] {
  let input = src.data()
  let src_offset = src.start_offset()
  let src_length = src.length()
  let len = utf16_len_from_utf8(input, src_offset, src_length)
  let dst = FixedArray::make(len, (0 : UInt16))
  match utf8_decode_into_utf16(input, src_offset, src_length, dst, 0) {
    written if written >= 0 => Ok(string_from_uint16_fixedarray(dst))
    err => Err(-err - 1)
  }
}

///|
fn decode_utf8_to_utf16_lossy(src : BytesView) -> String {
  let input = src.data()
  let src_offset = src.start_offset()
  let src_length = src.length()
  let dst = FixedArray::make(src_length * 2, (0 : UInt16))
  let written = match
    utf8_decode_into_utf16(input, src_offset, src_length, dst, 0) {
    ok if ok >= 0 => ok
    _ => utf8_decode_lossy_into_utf16(input, src_offset, src_length, dst, 0)
  }
  let result = FixedArray::make(written, (0 : UInt16))
  result.unsafe_blit(0, dst, 0, written)
  string_from_uint16_fixedarray(result)
}

///|
fn print_strict_decode(src : BytesView) -> Unit {
  match decode_utf8_to_utf16_strict(src) {
    Ok(value) => println(value)
    Err(pos) => println(pos)
  }
}

///|
fn init {
  // Low-level UTF-8 -> UTF-16 ABI: valid mixed-width input through a non-zero
  // source offset, plus a non-zero destination offset.
  let valid : FixedArray[Byte] = [
    b'\x2e', b'\x41', b'\xc2', b'\xa9', b'\xe4', b'\xb8', b'\xad', b'\xf0', b'\x9f',
    b'\x98', b'\x80', b'\x42', b'\x2e',
  ]
  let valid_bytes = fixedarray_as_bytes(valid)
  println(utf16_len_from_utf8(valid_bytes, 1, 11))
  let decoded = FixedArray::make(8, (0 : UInt16))
  println(utf8_decode_into_utf16(valid_bytes, 1, 11, decoded, 1))
  println(decoded[0])
  println(decoded[1])
  println(decoded[2])
  println(decoded[3])
  println(decoded[4])
  println(decoded[5])
  println(decoded[6])

  // Low-level UTF-8 error paths: length probing is allocation-oriented, strict
  // decode reports the first malformed byte after writing the valid prefix, and
  // lossy decode writes U+FFFD.
  let invalid : FixedArray[Byte] = [b'\x41', b'\x80', b'\xff', b'\x42']
  let invalid_bytes = fixedarray_as_bytes(invalid)
  println(utf16_len_from_utf8(invalid_bytes, 0, 4))
  println(utf8_decode_into_utf16(invalid_bytes, 0, 4, decoded, 0))
  let partial_decoded = FixedArray::make(3, (777 : UInt16))
  println(utf8_decode_into_utf16(invalid_bytes, 0, 4, partial_decoded, 0))
  println(partial_decoded[0])
  println(partial_decoded[1])
  let lossy = FixedArray::make(4, (0 : UInt16))
  println(utf8_decode_lossy_into_utf16(invalid_bytes, 0, 4, lossy, 0))
  println(lossy[0])
  println(lossy[1])
  println(lossy[2])
  println(lossy[3])

  // Lossy replacement grouping for partial 3-byte and 4-byte prefixes.
  let truncated_prefix : FixedArray[Byte] = [
    b'\xe1', b'\x80', b'\xe2', b'\xf0', b'\x91', b'\x92', b'\xf1', b'\xbf', b'\x41',
  ]
  let truncated_prefix_bytes = fixedarray_as_bytes(truncated_prefix)
  let lossy_prefix = FixedArray::make(5, (0 : UInt16))
  println(
    utf8_decode_lossy_into_utf16(truncated_prefix_bytes, 0, 9, lossy_prefix, 0),
  )
  println(lossy_prefix[0])
  println(lossy_prefix[1])
  println(lossy_prefix[2])
  println(lossy_prefix[3])
  println(lossy_prefix[4])

  // Low-level UTF-16 -> UTF-8 ABI: valid BMP and surrogate-pair length probes,
  // strict partial writes on invalid surrogates, and source/destination offsets.
  // The length intrinsic assumes well-formed UTF-16. SIMDUTF documents invalid
  // UTF-16 length results as implementation-defined, so malformed slices are
  // covered by strict encode error checks instead of exact length assertions.
  let utf16 : FixedArray[UInt16] = [
    0x0041, 0x00A9, 0x4E2D, 0xD83D, 0xDE00, 0x0042,
  ]
  let utf16_string = string_from_uint16_fixedarray(utf16)
  println(utf8_len_from_utf16(utf16_string, 0, 6))
  let encoded = FixedArray::make(13, b'\x00')
  println(utf8_encode_from_utf16(utf16_string, 0, 6, encoded, 1))
  println(encoded[0])
  println(encoded[1])
  println(encoded[2])
  println(encoded[3])
  println(encoded[4])
  println(encoded[5])
  println(encoded[6])
  println(encoded[7])
  println(encoded[8])
  println(encoded[9])
  println(encoded[10])
  println(encoded[11])

  let bad_utf16 : FixedArray[UInt16] = [0x0041, 0xD83D, 0x0042]
  let bad_utf16_string = string_from_uint16_fixedarray(bad_utf16)
  println(utf8_encode_from_utf16(bad_utf16_string, 0, 3, encoded, 0))
  let partial_encoded = FixedArray::make(4, b'\x7f')
  println(utf8_encode_from_utf16(bad_utf16_string, 0, 3, partial_encoded, 0))
  println(partial_encoded[0])
  println(partial_encoded[1])
  let bad_trailing_utf16 : FixedArray[UInt16] = [0x0041, 0xDE00]
  let bad_trailing_utf16_string = string_from_uint16_fixedarray(bad_trailing_utf16)
  println(utf8_encode_from_utf16(bad_trailing_utf16_string, 0, 2, encoded, 0))
  let utf16_with_offset : FixedArray[UInt16] = [
    0x0058, 0x0041, 0x00A9, 0xD83D, 0xDE00, 0x0059,
  ]
  let utf16_with_offset_string = string_from_uint16_fixedarray(utf16_with_offset)
  let offset_encoded = FixedArray::make(10, b'\x00')
  println(utf8_len_from_utf16(utf16_with_offset_string, 1, 4))
  println(
    utf8_encode_from_utf16(utf16_with_offset_string, 1, 4, offset_encoded, 2),
  )
  println(offset_encoded[0])
  println(offset_encoded[1])
  println(offset_encoded[2])
  println(offset_encoded[3])
  println(offset_encoded[4])
  println(offset_encoded[5])
  println(offset_encoded[6])
  println(offset_encoded[7])
  println(offset_encoded[8])

  // High-level wrappers: strict and lossy UTF-8 -> UTF-16, and UTF-16 -> UTF-8.
  // Malformed inputs cover truncated prefixes, overlong encodings, surrogate
  // encodings, out-of-range scalars, and invalid leading bytes.
  println(encode_utf16_view_to_utf8("ZA©中😀BY"[1:7]))
  print_strict_decode(b".A\xC2\xA9\xE4\xB8\xAD\xF0\x9F\x98\x80B."[1:12])
  print_strict_decode(b".Hi!"[1:3])
  print_strict_decode(b".A\x80\xFFB."[1:5])
  print_strict_decode(b"xy\xC2"[2:3])
  print_strict_decode(b"xy\xE1\x80"[2:4])
  print_strict_decode(b"xy\xC0\xAFz"[2:5])
  print_strict_decode(b"xy\xE0\x80\x80z"[2:6])
  print_strict_decode(b"xy\xED\xA0\x80z"[2:6])
  print_strict_decode(b"xy\xF0\x91\x92z"[2:6])
  print_strict_decode(b"xy\xF4\x90\x80\x80z"[2:7])
  print_strict_decode(b"xy\xFFz"[2:4])
  println(decode_utf8_to_utf16_lossy(b".A\x80\xFFB."[1:5]))
  println(decode_utf8_to_utf16_lossy(b"xy\xC2"[2:3]))
  println(decode_utf8_to_utf16_lossy(b"xy\xE1\x80"[2:4]))
  println(decode_utf8_to_utf16_lossy(b"xy\xC0\xAFz"[2:5]))
  println(decode_utf8_to_utf16_lossy(b"xy\xE0\x80\x80z"[2:6]))
  println(decode_utf8_to_utf16_lossy(b"xy\xED\xA0\x80z"[2:6]))
  println(decode_utf8_to_utf16_lossy(b"xy\xF0\x91\x92z"[2:6]))
  println(decode_utf8_to_utf16_lossy(b"xy\xF4\x90\x80\x80z"[2:7]))
  println(decode_utf8_to_utf16_lossy(b"xy\xFFz"[2:4]))
  println(encode_utf16_view_to_utf8(""[:]))
  print_strict_decode(b""[:])
  println(decode_utf8_to_utf16_lossy(b""[:]).length())
}
