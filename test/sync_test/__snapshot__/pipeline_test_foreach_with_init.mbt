///|
fn main {
  let arr = Array::makei(10, x => x)
  for x in 0..<10; sum = 0 {
    continue sum + x
  } nobreak {
    println(sum)
  }

  for x in arr; sum = 0 {
    continue sum + x
  } nobreak {
    println(sum)
  }

  for x in arr.iter(); sum = 0 {
    continue sum + x
  } nobreak {
    println(sum)
  }

  // test `continue` without arguments

  for x in 0..<10; sum = 0 {
    continue sum + x
  } nobreak {
    println(sum)
  }

  for x in arr; sum = 0 {
    if x % 2 == 0 {
      continue
    }
    continue sum + x
  }

  for x in arr.iter(); sum = 0 {
    if x % 2 == 0 {
      continue
    }
    continue sum + x
  }

  let fib10 = for _ in 0..<10; p1 = 1, p2 = 0; p1 = p1 + p2, p2 = p1 {
  } nobreak {
    p1
  }
  println(fib10)

  let fib10 = for _ in (0).until(10); p1 = 1, p2 = 0; p1 = p1 + p2, p2 = p1 {
  } nobreak {
    p1
  }
  println(fib10)

  let one_to_ten = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

  let fib10 = for _ in one_to_ten; p1 = 1, p2 = 0; p1 = p1 + p2, p2 = p1 {
  } nobreak {
    p1
  }
  println(fib10)

  let even_sum = for x in one_to_ten; acc = 0; acc = acc + x {
    if x % 2 == 1 {
      continue acc
    }
  } nobreak {
    acc
  }
  println(even_sum)

  let even_sum = for x in one_to_ten.iter(); acc = 0; acc = acc + x {
    if x % 2 == 1 {
      continue acc
    }
  } nobreak {
    acc
  }
  println(even_sum)

  let even_sum = for x in 1..<=10; acc = 0; acc = acc + x {
    if x % 2 == 1 {
      continue acc
    }
  } nobreak {
    acc
  }
  println(even_sum)
}
