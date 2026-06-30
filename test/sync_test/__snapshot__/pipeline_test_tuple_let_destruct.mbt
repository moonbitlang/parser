//!build: -w -27

///|
fn f(flag : Bool) -> (Int, Int) {
  let (x, y) = if flag {
    (1, 2)
  } else {
    return (4, 2)
  }
  (x + 1, y + 1)
}

///|
fn return_in_match(x : Int) -> (Int, Int) {
  let (a, b) = match x {
    1 => (10, 20)
    2 => return (99, 88)
    _ => (30, 40)
  }
  (a + 1, b + 1)
}

///|
fn return_in_nested_if(x : Int, y : Int) -> (Int, Int) {
  let (a, b) = if x > 0 {
    if y > 0 {
      (1, 2)
    } else {
      return (100, 200)
    }
  } else {
    (3, 4)
  }
  (a * 10, b * 10)
}

///|
fn return_with_computation(flag : Bool, n : Int) -> (Int, Int) {
  let (x, y) = if flag {
    (n + 1, n + 2)
  } else {
    return (n * 10, n * 20)
  }
  (x + y, x - y)
}

///|
fn return_in_sequence(flag : Bool) -> (Int, Int) {
  let (x, y) = if flag {
    (1, 2)
  } else {
    let _ = 42
    return (7, 8)
  }
  (x + 1, y + 1)
}

///|
fn triple_tuple_return(flag : Bool) -> (Int, Int, Int) {
  let (a, b, c) = if flag {
    (1, 2, 3)
  } else {
    return (10, 20, 30)
  }
  (a + 1, b + 1, c + 1)
}

///|
fn init {
  debug(f(true))
  debug(f(false))
  debug(return_in_match(1))
  debug(return_in_match(2))
  debug(return_in_match(3))
  debug(return_in_nested_if(1, 1))
  debug(return_in_nested_if(1, -1))
  debug(return_in_nested_if(-1, 0))
  debug(return_with_computation(true, 5))
  debug(return_with_computation(false, 5))
  debug(return_in_sequence(true))
  debug(return_in_sequence(false))
  debug(triple_tuple_return(true))
  debug(triple_tuple_return(false))
}

///|
fn optimize_raise(cond : Bool) -> Int raise {
  let (x, y) = if cond {
    (1, 2)
  } else {
    raise Failure::Failure("err")
  }
  x + y
}

///|
fn init {
  debug(to_result(() => optimize_raise(true)))
  debug(to_result(() => optimize_raise(false)))
}

///|
// return sum of largest and second largest elements in the array
fn optimize_loop(xs : Array[Int]) -> Int {
  let (max1, max2) = for x in xs; max1 = 0, max2 = 0 {
    if x > max1 {
      continue x, max1
    } else if x > max2 {
      continue max1, x
    } else {
      continue max1, max2
    }
  } nobreak {
    (max1, max2)
  }
  max1 + max2
}

///|
fn init {
  println(optimize_loop([ 1, 5, 3, 4 ]))
}

///|
fn optimize_break(xs : Array[Int]) -> Unit {
  let (i, x) = for i, x in xs {
    if x == 42 {
      break (i, x)
    }
  } nobreak {
    (-1, -1)
  }
  if i >= 0 {
    println("\{x} found at index #\{i}")
  } else {
    println("not found")
  }
}

///|
fn init {
  optimize_break([ 1, 2, 3 ])
  optimize_break([ 1, 2, 42, 3 ])
}

///|
fn loop_cannot_optimize(xs : Array[Int], default~ : (Int, Int)) -> Unit {
  let (i, x) = loop (0, xs[:]) {
    (_, []) =>
      // this branch prevents optimization
      default
    (i, [42, ..]) => (i, 42)
    (i, [_, ..rest]) => continue (i + 1, rest)
  }
  println("\{x} at #\{i}")
}

///|
fn init {
  loop_cannot_optimize([ 1, 2, 3 ], default=(-1, -1))
  loop_cannot_optimize([ 1, 42, 3 ], default=(-1, -1))
}

///|
fn break_cannot_optimize(xs : Array[Int], magic~ : (Int, Int)) -> Unit {
  let (i, x) = for i, x in xs {
    if x == 42 {
      // this branch prevents optimization
      break magic
    } else if x > 42 {
      break (i, x)
    }
  } nobreak {
    (-1, -1)
  }
  if i >= 0 {
    println("\{x} at #\{i}")
  }
}

///|
fn init {
  break_cannot_optimize([ 1, 2, 3 ], magic=(42, 42))
  break_cannot_optimize([ 1, 2, 42 ], magic=(42, 42))
  break_cannot_optimize([ 1, 43, 42 ], magic=(42, 42))
}

///|
fn outer_loop(xss : Array[Array[Int]]) -> Unit {
  outer~ : for xs in xss; isum = 0, vsum = 0 {
    let (i, x) = for i, x in xs {
      if x > 42 {
        break (i, x)
      } else if x == 42 {
        break outer~
      }
    } nobreak {
      (0, 0)
    }
    continue isum + i, vsum + x
  } nobreak {
    println("\{isum}, \{vsum}")
  }
}

///|
fn init {
  outer_loop([[1, 44], [50, 2], [3, 4]])
  outer_loop([[1, 44], [50, 2], [42, 4]])
}

///|
fn inner_loop(xss : Array[Array[Int]]) -> Unit {
  let (max_index, max_value) = for i, xs in xss; max_index= 0, max_value = 0 {
    let sum = for i = 0, sum = 0 {
      guard i < xs.length() else { break sum }
      continue i + 1, sum + xs[i]
    }
    if sum > max_value {
      continue i, sum
    } else {
      continue max_index, max_value
    }
  } nobreak {
    (max_index, max_value)
  }
  println("sub array #\{max_index} has the largest sum \{max_value}")
}

///|
fn init {
  inner_loop([[ 2, 3 ], [ 5, 4 ], [ 1, 9 ]])
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
