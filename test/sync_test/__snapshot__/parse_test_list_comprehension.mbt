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
  // Invalid infinite-loop header retained to test error recovery.
  // The explicit form is `[ for ;; => 42 ]`.
  let _ = [ for => 42 ]
  // An infinite list-comprehension header must contain `;;`.
  // Keep this malformed guarded form to test error recovery.
  // The valid spelling begins `[ for ;; if ...`.
  let _ = [ for if f() is Some(x) => x ]
}
