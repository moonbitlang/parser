///|
let c : ReadOnlyArray[Int] = [1, 2, 3, 4, 5]

///|
let d : ReadOnlyArray[(Int, Int)] = [(1, 2), (3, 4), (5, 6), (7, 8), (9, 10)]

///|
fn[T : Debug] use_readonly_array(xs : ReadOnlyArray[T]) -> Unit {
  debug(xs[:])
}

///|
fn[T : Debug] use_readonly_array2(xs : ReadOnlyArray[ReadOnlyArray[T]]) -> Unit {
  for x in xs[:] {
    debug(x[:])
  }
}

///|
fn main {
  debug(c[:])
  debug(d[:])
  use_readonly_array([1.2, 2.3, 3.4, 4.5, 5.6])
  let x : ReadOnlyArray[ReadOnlyArray[_]] = [
    [1.2, 2.3, 3.4, 4.5, 5.6],
    [6.7, 8.9, 10.1, 12.1, 14.1],
  ]
  use_readonly_array2(x)
  println(use_readonly_array3([1, 2, 3, 4, 5567]))
}

fn use_readonly_array3(xs : ArrayView[Int]) -> Int {
  xs.fold(init=0,(a,b)=> a + b)
}