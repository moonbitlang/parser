//!build: -w -lexmatch_longest_match

///|
fn parse_integer(input : BytesView) -> Int {
  for acc = 0, rest = input {
    match (acc, rest) {
      (acc, []) => break acc
      (acc, [b'0'..=b'9' as c, .. rest]) =>
        continue acc * 10 + (c.to_int() - b'0'.to_int()), rest
      (_, _) => panic()
    }
  }
}

///|
fn eval(input : BytesView) -> Int? {
  let stack : Array[Int] = []
  for curr = input {
    lexmatch curr with longest {
      "" => break
      (" ", next) => continue next
      ("[0-9]+" as num, next) => {
        let value = parse_integer(num)
        stack.push(value)
        continue next
      }
      ("\+", next) =>
        if stack.pop() is Some(b) && stack.pop() is Some(a) {
          stack.push(a + b)
          continue next
        } else {
          return None
        }
      ("-", next) =>
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
  debug(eval(b"42"))
  debug(eval(b"3 2 +"))
  debug(eval(b"3 2 + 5 -"))
  debug(eval(b"5 3 2 + -"))
}
