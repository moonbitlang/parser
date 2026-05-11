fn main {
  let arr = []
  for i in 0..<10 {
    arr.push([ i ][:])
  }
  for _ in 0..<5 {
    debug(arr.pop())
  }
  debug(arr)
}
