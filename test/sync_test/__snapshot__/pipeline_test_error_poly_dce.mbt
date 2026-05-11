///|
fn h(f : (Int) -> Unit raise?) -> Unit raise? {
  f(42) catch {
    e => raise e
  }
  try {
    f(43)
    fail("hhh")
  } catch {
    _ => println("hhh")
  }
}

///|
suberror E {
  E(Int)
} derive(Debug)

///|
fn main {
  debug(try? h(x => raise E(x)))
  h(fn(x) { println(x) })
}
