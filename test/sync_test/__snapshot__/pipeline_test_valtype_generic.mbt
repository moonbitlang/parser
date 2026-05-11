///|
#valtype
struct T[A, B] {
  a : (A, B)
  b : A?
  c : FixedArray[B]
} derive(Debug)

///|
fn make(i : Int) -> T[Int, (Int, Int)] {
  let t = {
    a: (42, (i, i + 1)),
    b: Some(i + 2),
    c: [(i + 3, i + 4), (i + 5, i + 6)],
  }
  t
}

///|
fn main {
  let t = make(0)
  debug(t)
  let arr = []
  for i in 1..=10 {
    arr.push(make(i))
  }
  debug(arr)
  for _ in 0..=5 {
    let _ = arr.pop()

  }
  debug(arr)
}
