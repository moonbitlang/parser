fn main {
  let x = Ref::{val: 0}
  let a = f(x).iter().take(3)
  a.each(x => println(x))
  a.each(x => println(x))
}

fn f(x: Ref[Int]) -> FixedArray[Int] {
  if x.val == 0 {
    x.val = 1
    [1, 2, 3, 4]
  } else {
    [4, 3, 2, 1]
  }
}
