
#warnings("-71")

fn parse_digit(s : StringView) -> (Int64?, StringView) {
  lexscan s with longest {
    (re"^-?[0-9][0-9_]+[0-9]" as digit, after=rest) => {
      (Some (try! @string.parse_int64(digit)),rest)
    }
    _ => (None,s)
  }
}


fn main {
  debug(parse_digit("123 ab"))
  debug(parse_digit("-323c"))
  debug(parse_digit("123_456"))
  debug(parse_digit("123_456_789d"))
  debug(parse_digit("123_456_789_0"))
  debug(parse_digit("123_456_789_01"))
  debug(parse_digit("123_456_789_012"))
  debug(parse_digit("123_456_789_0123"))
  debug(parse_digit("123_456_789_01234"))
  debug(parse_digit("123_456_789_012345"))
  debug(parse_digit("123_456_789_0123456"))
  println("failed cases")
  debug(parse_digit("_323"))
  debug(parse_digit("323_"))
  debug(parse_digit("3_23_"))
  let (digit, _) = parse_digit("x123")
  if digit is Some(_) {
    panic()
  }
}
