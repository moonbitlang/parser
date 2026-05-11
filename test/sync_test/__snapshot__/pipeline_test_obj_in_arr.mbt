///|
#valtype
enum E {
  A
  B(Int)
} derive(Debug)

///|
#valtype
struct S {
  a : Int
  b : (Int, Int)
} derive(Debug)

///|
fn g(xs : FixedArray[&Debug]) -> Unit {
  debug(xs)
  match xs {
    [a, .., b] => {
      debug(a)
      debug(b)
    }
    _ => println("other")
  }
}

///|
fn main {
  let e = E::B(1) as &Debug
  let s = S::{ a: 1, b: (2, 3) } as &Debug
  g([e, 42, s])
  g([(1, 2), s, e])
  let arr2 = []
  for i in 0..<10 {
    if i % 3 == 0 {
      arr2.push(e)
    } else if i % 3 == 1 {
      arr2.push(s)
    } else {
      arr2.push((i, i + 1))
    }
  }
  debug(arr2)
  for i in 0..<5 {
    let _ = arr2.pop()

  }
  debug(arr2)
  let arr = FixedArray::make(5, (1, 2) as &Debug)
  arr[1] = e
  arr[2] = s
  debug(arr)
}
