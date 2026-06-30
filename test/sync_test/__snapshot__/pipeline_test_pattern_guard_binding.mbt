
///|
priv enum Parsed {
  Integer(Int64)
  String(String)
}

///|
fn parse_scalar(v : String) -> Parsed {
  match v {
    ['0', 'x', .. number] if (try @string.parse_int64(number, base=16) catch {
        err => Err(err)
      } noraise {
        value => Ok(value)
      })
      is Ok(i) => Parsed::Integer(i)
    ['+', .. number] if (try @string.parse_int64(number) catch {
        err => Err(err)
      } noraise {
        value => Ok(value)
      })
      is Ok(i) => Parsed::Integer(i)
    _ => Parsed::String(v)
  }
}

///|
fn main {
  match parse_scalar("+42") {
    Parsed::Integer(i) => println(i)
    _ => panic()
  }
  match parse_scalar("plain") {
    Parsed::String(s) => println(s)
    _ => panic()
  }
}
