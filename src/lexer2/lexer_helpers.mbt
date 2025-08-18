///|
fn digit_value(c : Char) -> UInt? {
  match c {
    'a'..='f' => Some(c.to_uint() + 10 - 'a'.to_uint())
    'A'..='F' => Some(c.to_uint() + 10 - 'A'.to_uint())
    '0'..='9' => Some(c.to_uint() - '0'.to_uint())
    _ => None
  }
}

///|
fn char_for_hex_escape(hex : @string.View) -> UInt? {
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
