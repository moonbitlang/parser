///|
test {
  let _ : () -> Iter[Int] = () => { return [||] }
  let xs = [| 1, 2, 3 |]
  let _ = [| 0, ..xs, 5 |]
  let _ = [| for x in 0..<10 => x + 1 |]
  let _ = [| for i = 0; i < 10; i = i + 1 => i |]
}
