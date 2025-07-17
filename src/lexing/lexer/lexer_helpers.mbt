///|
fn digit_value(c : Char) -> UInt {
  match c {
    'a'..='f' => c.to_uint() + 10 - 'a'.to_uint()
    'A'..='F' => c.to_uint() + 10 - 'A'.to_uint()
    '0'..='9' => c.to_uint() - '0'.to_uint()
    _ => panic()
  }
}

///|
fn char_for_hex_escape(hex : String) -> UInt? {
  let mut codep = 0U
  for c in hex {
    let digit = digit_value(c)
    codep = codep * 16 + digit
    if codep > 0x10ffff {
      return None
    }
  } else {
    return Some(codep)
  }
}
