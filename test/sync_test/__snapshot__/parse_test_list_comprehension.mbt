///|
test {
  let _ = [ for x in xs => x + 1 ]
  let _ = [ for i, x in xs => (i, x) ]
  let _ = [ for x in xs if map[x] is Some(y) => y ]
  let _ = [ for x in xs + ys => x ]
  let _ = [ for x in xs => x |> pipe ]
  let _ = [
    for _ in 0..<10; p1 = 1, p2 = 0; p1 = p1 + p2, p2 = p1 => p1
  ]
  let _ = [
    for
      _ in 0..<10
      p1 = 1, p2 = 0
      p1 = p1 + p2, p2 = p1 if (p1 % 2) is 0 => p1
  ]
}

///|
test {
  let _ = [ for i = 0; i < 10; i = i + 1 if (i % 2) is 0 => i ]
  let _ = [ for p1 = 1, p2 = 1, i = 0; i < n; p1 = p1 + p2, p2 = p1, i = i + 1 => p1 ]
  // correct parsing: `x` is a tuple, body is `c => d` (arrow function)
  let _ = [ for x = (a, b) => c => d ]
  // kept here just for better error recovery & reuse parser component.
  // the formatter will turn this into `for ;; => 42`
  let _ = [ for => 42 ]
  // kept here just for better error recovery & reuse parser component.
  // this is actually equivalent to `[ while f() is Some(x) => x ]` (if we support it).
  // But honestly this looks way worse than `[ while .. ]`
  let _ = [ for if f() is Some(x) => x ]
}
