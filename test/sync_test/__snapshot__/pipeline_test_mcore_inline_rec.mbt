///|
#locals(f)
fn[T, U] map(arr : @array.View[T], f : (T) -> U) -> @array.View[U] {
  let result = []
  for a in arr {
    result.push(f(a))
  }
  result[:]
}

///|
#locals(f)
fn[T : Show] iter(arr : @array.View[T], f : (T) -> Unit) -> Unit {
  let n = arr.length()
  if arr is [] {
    return
  }
  for i in 0..<arr.length() {
    f(arr[i])
  }
  iter(map(arr[1:], x => x.to_string().length() + n), println)
  println("---")
}

///|
fn main {
  let arr = [1, 2, 3, 4, 5]
  iter(arr[:], println)
}
