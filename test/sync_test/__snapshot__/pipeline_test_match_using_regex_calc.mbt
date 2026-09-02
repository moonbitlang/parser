///|
fn parse_integer(input : StringView) -> Int {
  for acc = 0, rest = input {
    match (acc, rest) {
      (acc, []) => break acc
      (acc, ['0'..='9' as c, .. rest]) =>
        continue acc * 10 + (c.to_int() - b'0'.to_int()), rest
      (_, _) => panic()
    }
  }
}

///|
fn eval(input : StringView) -> Int? {
  let stack : Array[Int] = []
  for curr = input {
    lexmatch curr with longest {
      re"^$" => break
      (re"^ ", after=next) => continue next
      (re"^[0-9]+" as num, after=next) => {
        let value = parse_integer(num)
        stack.push(value)
        continue next
      }
      (re"^\+", after=next) =>
        if stack.pop() is Some(b) && stack.pop() is Some(a) {
          stack.push(a + b)
          continue next
        } else {
          return None
        }
      (re"^-", after=next) =>
        if stack.pop() is Some(b) && stack.pop() is Some(a) {
          stack.push(a - b)
          continue next
        } else {
          return None
        }
      _ => return None
    }
  }
  stack.pop()
}

///|
fn main {
  debug(eval("42"))
  debug(eval("3 2 +"))
  debug(eval("3 2 + 5 -"))
  debug(eval("5 3 2 + -"))
}
