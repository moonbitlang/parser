///|
/// Print a constant value.
pub fn print_constant(buf : StringBuilder, c : @syntax.Constant) -> Unit {
  match c {
    Bool(b) => buf.write_string(if b { "true" } else { "false" })
    Byte(repr) => {
      buf.write_string("b'")
      buf.write_string(repr)
      buf.write_char('\'')
    }
    Bytes(repr) => {
      buf.write_string("b\"")
      buf.write_string(repr)
      buf.write_char('"')
    }
    Char(repr) => {
      buf.write_char('\'')
      buf.write_string(repr)
      buf.write_char('\'')
    }
    Int(s) => buf.write_string(s)
    Int64(s) => {
      buf.write_string(s)
      buf.write_char('L')
    }
    UInt(s) => {
      buf.write_string(s)
      buf.write_char('U')
    }
    UInt64(s) => {
      buf.write_string(s)
      buf.write_string("UL")
    }
    Float(s) => {
      buf.write_string(s)
      if not(s.contains(".")) {
        buf.write_string(".0")
      }
      buf.write_char('F')
    }
    Double(s) => {
      buf.write_string(s)
      if not(s.contains(".")) {
        buf.write_string(".0")
      }
    }
    String(repr) => {
      buf.write_char('"')
      buf.write_string(repr)
      buf.write_char('"')
    }
    BigInt(s) => {
      buf.write_string(s)
      buf.write_char('N')
    }
  }
}
