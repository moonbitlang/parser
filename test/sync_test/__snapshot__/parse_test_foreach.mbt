fn init {
  for x in [ 1, 2, 3 ] {
    println(x)
  }

  for k, v in dict.iter2() {
    println(k)
    println(v)
  } else {
    println("done")
  }
}

fn init {
  for in [ 1, 2, 3 ] {}
  for x {}
  for x + 1 {}
  for ? {}
}

fn init {
  let _ = for x in 0..<10; acc = 0 {
    continue acc + x
  } nobreak {
    acc
  }
  let _ = for x in [ 1, 2, 3 ]; acc = 0 {
    continue acc + x
  } nobreak {
    acc
  }
  let _ = for x in [ 1, 2, 3 ]; sum = 0, prefix_sum = 0 {
    let sum = sum + x
    continue sum, prefix_sum + sum
  } nobreak {
    prefix_sum
  }
  let _ = for
    x in 0..<10
    p1 = 1, p2 = 1
    p1 = p1 + p2, p2 = p1 {
  } nobreak {
    p1
  }
}
