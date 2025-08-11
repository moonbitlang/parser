///|
fn digit_value(c : Byte) -> UInt? {
  match c {
    'a'..='f' => Some(c.to_uint() + 10 - 'a'.to_uint())
    'A'..='F' => Some(c.to_uint() + 10 - 'A'.to_uint())
    '0'..='9' => Some(c.to_uint() - '0'.to_uint())
    _ => None
  }
}

///|
fn char_for_hex_escape(hex : @bytes.View) -> UInt? {
  let mut codep = 0U
  for c in hex {
    match digit_value(c) {
      Some(digit) => {
        codep = codep * 16 + digit
        if codep > 0x10ffff {
          return None
        }
      }
      None => return None
    }
  } else {
    return Some(codep)
  }
}

///|
fn calc_offset(base~ : @bytes.View, view : @bytes.View) -> Int {
  view.start_offset() - base.start_offset()
}

///|
fn decode_utf8(bytes : @bytes.View) -> String {
  let buf = StringBuilder::new()
  loop bytes[:] {
    // 1-byte sequence (ASCII): 0xxxxxxx
    [0x00..=0x7F as b, .. next] => {
      buf.write_char(b.to_char())
      continue next
    }

    // 2-byte sequence: 110xxxxx 10xxxxxx
    [0xC0..=0xDF as b1, 0x80..=0xBF as b2, .. next] => {
      let code_point = ((b1.to_int() & 0x1F) << 6) | (b2.to_int() & 0x3F)
      buf.write_char(code_point.unsafe_to_char())
      continue next
    }

    // 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
    [0xE0..=0xEF as b1, 0x80..=0xBF as b2, 0x80..=0xBF as b3, .. next] => {
      let code_point = ((b1.to_int() & 0x0F) << 12) |
        ((b2.to_int() & 0x3F) << 6) |
        (b3.to_int() & 0x3F)
      buf.write_char(code_point.unsafe_to_char())
      continue next
    }

    // 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    [
      0xF0..=0xF4 as b1,
      0x80..=0xBF as b2,
      0x80..=0xBF as b3,
      0x80..=0xBF as b4,
      .. next,
    ] => {
      let code_point = ((b1.to_int() & 0x07) << 18) |
        ((b2.to_int() & 0x3F) << 12) |
        ((b3.to_int() & 0x3F) << 6) |
        (b4.to_int() & 0x3F)
      if code_point > 0x10FFFF {
        abort("invalid UTF-8 sequence")
      }
      buf.write_char(code_point.unsafe_to_char())
      continue next
    }

    // Invalid sequence - skip one byte and continue
    [_, ..] => panic()
    [] => ()
  }
  buf.to_string()
}
