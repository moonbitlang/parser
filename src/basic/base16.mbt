///|
pub fn encode_base16(input : String) -> String {
  let input = @encoding.encode(UTF8, input)
  let buf = StringBuilder::new()
  for i = 0; i < input.length() * 2; i = i + 1 {
    let byte = input[i / 2].to_int()
    let value = if i % 2 == 0 { byte / 16 } else { byte % 16 }
    let chr = if value >= 10 {
      Int::unsafe_to_char(value - 10 + 'a')
    } else {
      Int::unsafe_to_char(value + '0')
    }
    buf.write_char(chr)
  } else {
    buf.to_string()
  }
}
