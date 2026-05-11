///|
type TestMap = Map[String, Map[Int, (() -> Unit raise, ReadOnlyArray[String])]]

///|
fn test1() -> Unit raise {
  println("test1")
}

///|
fn test2() -> Unit raise {
  println("test2")
  raise Failure::Failure("test2")
}

///|
let tests : TestMap = {
  "test_map.mbt": {
    0: (test1, ["test1"]),
    1: (test2, ["test2"]),
  }
}

///|
fn run_tests() -> Unit {
  for filename, tests in tests {
    for index, test_case in tests {
      let (f, attrs) = test_case
      let name = if attrs is [ name, .. ] { name } else { "" }
      try f() catch {
        err => println("test \{name} failed with \{err}")
      } noraise {
        _ => println("test \{name} completed")
      }
    }
  }
}

///|
fn main {
  run_tests()
}
