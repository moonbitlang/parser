//!build: -w -20-27

///|
enum FizzBuzz {
  Fizz
  Buzz
  FizzBuzz
  Number(String)
}

///|
fn fizzbuzz(n : Int) -> FizzBuzz {
  if n % 15 == 0 {
    return FizzBuzz
  } else if n % 3 == 0 {
    return Fizz
  } else if n % 5 == 0 {
    return Buzz
  } else {
    return Number(n.to_string())
  }
}

fn hack(x : StringView) -> (StringView, Int) {
  return (x, 0)
}

///|
fn first_wrong(curr : StringView) -> Int? {
  for curr = curr, i = 1 {
    let skip_space = lexmatch curr {
      (re"^[ ]+", after=rest) => Some(rest)
      _ => None
    }
    match skip_space {
      Some(rest) => continue rest, i
      None => ()
    }
    let at_end = lexmatch curr {
      re"^$" => true
      _ => false
    }
    if at_end {
      return None
    }
    let next = match fizzbuzz(i) {
      Number(expected) => lexmatch curr {
        (re"^[0-9]+" as n, after=rest) =>
          if hack(rest) is (rest, _) && expected[:] == n {
            Some((rest, i + 1))
          } else {
            None
          }
        _ => None
      }
      Fizz => lexmatch curr {
        (re"^fizz", after=rest) => Some((rest, i + 1))
        _ => None
      }
      Buzz => lexmatch curr {
        (re"^buzz", after=rest) => Some((rest, i + 1))
        _ => None
      }
      FizzBuzz => lexmatch curr {
        (re"^fizzbuzz", after=rest) => Some((rest, i + 1))
        _ => None
      }
    }
    match next { Some((rest, i)) => continue rest, i; None => return Some(i) }
  }
}

///|
fn main {
  match first_wrong("1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz 16") {
    Some(i) => println("First wrong at position \{i}")
    None => println("All correct!")
  }
  match first_wrong("1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz fizz") {
    Some(i) => println("First wrong at position \{i}")
    None => println("All correct!")
  }
}
