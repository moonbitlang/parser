
fn escape_in_char_and_string() -> Unit {
  println(('\u{5154}', '\u5154', '\o115', '\x42', '\n', '\r', '\b', '\t', '\\'))
  println(("\u{5154}", "\u5154", "\o115", "\x42", "\n", "\r", "\b", "\t", "\\"))
}

fn escape_in_string_interpolation() -> Unit {
  let a = "_"
  println("\\ \u{5154} \u5154 \{a} \o115 \x42 \n") 
}

fn short_unicode_eacape() -> Unit {
  println("\u{8}abc")
  println('\u{8}')
}

