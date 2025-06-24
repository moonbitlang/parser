///|
struct Lexbuf {
  content : Bytes
  mut pos : Int
  end : Int
}

///|
pub fn Lexbuf::from_bytes(
  bytes : Bytes,
  start~ : Int = 0,
  end~ : Int = bytes.length()
) -> Lexbuf {
  return { content: bytes, pos: start, end }
}

///|
fn Lexbuf::next_as_int(self : Lexbuf) -> Int {
  let pos = self.pos
  let end = self.end
  if pos < end {
    let c = self.content.unsafe_get(pos)
    self.pos += 1
    return c.to_int()
  }
  -1
}

///|
pub fn Lexbuf::curr_pos(self : Lexbuf) -> Int {
  return self.pos
}

///|
pub fn Lexbuf::reset(self : Lexbuf, pos~ : Int) -> Unit {
  self.pos = pos
}

///|
/// Get the current character in the lexbuf.
fn Lexbuf::get_char(self : Lexbuf, start : Int, end : Int) -> Char {
  match self.content[start:end] {
    // 1-byte sequence (ASCII): 0xxxxxxx
    [0x00..=0x7F as b] => b.to_int().unsafe_to_char()
    // 2-byte sequence: 110xxxxx 10xxxxxx
    [0xC0..=0xDF as b1, 0x80..=0xBF as b2] =>
      (((b1.to_int() & 0x1F) << 6) | (b2.to_int() & 0x3F)).unsafe_to_char()
    // 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
    [0xE0..=0xEF as b1, 0x80..=0xBF as b2, 0x80..=0xBF as b3] =>
      (((b1.to_int() & 0x0F) << 12) |
      ((b2.to_int() & 0x3F) << 6) |
      (b3.to_int() & 0x3F)).unsafe_to_char()
    // 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    [0xF0..=0xF7 as b1, 0x80..=0xBF as b2, 0x80..=0xBF as b3, 0x80..=0xBF as b4] =>
      (((b1.to_int() & 0x07) << 18) |
      ((b2.to_int() & 0x3F) << 12) |
      ((b3.to_int() & 0x3F) << 6) |
      (b4.to_int() & 0x3F)).unsafe_to_char()
    _ => panic()
  }
}

///|
fn Lexbuf::get_string(self : Lexbuf, start : Int, end : Int) -> String {
  let buf = StringBuilder::new()
  loop self.content[start:end] {
    // 1-byte sequence (ASCII): 0xxxxxxx
    [0x00..=0x7F as b, .. next] => {
      buf.write_char(b.to_int().unsafe_to_char())
      continue next
    }

    // 2-byte sequence: 110xxxxx 10xxxxxx
    [0xC0..=0xDF as b1, 0x80..=0xBF as b2, .. next] => {
      buf.write_char(
        (((b1.to_int() & 0x1F) << 6) | (b2.to_int() & 0x3F)).unsafe_to_char(),
      )
      continue next
    }

    // 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
    [0xE0..=0xEF as b1, 0x80..=0xBF as b2, 0x80..=0xBF as b3, .. next] => {
      buf.write_char(
        (((b1.to_int() & 0x0F) << 12) |
        ((b2.to_int() & 0x3F) << 6) |
        (b3.to_int() & 0x3F)).unsafe_to_char(),
      )
      continue next
    }

    // 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    [
      0xF0..=0xF7 as b1,
      0x80..=0xBF as b2,
      0x80..=0xBF as b3,
      0x80..=0xBF as b4,
      .. next,
    ] => {
      buf.write_char(
        (((b1.to_int() & 0x07) << 18) |
        ((b2.to_int() & 0x3F) << 12) |
        ((b3.to_int() & 0x3F) << 6) |
        (b4.to_int() & 0x3F)).unsafe_to_char(),
      )
      continue next
    }

    // Invalid sequence - skip one byte and continue
    [_, .. next] => continue next
    [] => ()
  }
  buf.to_string()
}
