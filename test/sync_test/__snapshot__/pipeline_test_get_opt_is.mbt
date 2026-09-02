///|
fn sum_all(xs : Array[Int]) -> Int {
  let mut i = 0
  let mut sum = 0
  while xs.get(i) is Some(x) {
    sum = sum + x
    i = i + 1
  }
  sum
}

///|
fn is_answer(xs : Array[Int], i : Int) -> Bool {
  xs.get(i) is Some(42)
}

///|
fn next_idx(r : Ref[Int]) -> Int {
  r.val = r.val + 1
  r.val
}

///|
fn effectful(xs : Array[Int], r : Ref[Int]) -> Int {
  if xs.get(next_idx(r)) is Some(x) {
    x
  } else {
    -100
  }
}

///|
fn first_byte(b : Bytes) -> Int {
  if b.get(0) is Some(x) {
    x.to_int()
  } else {
    -1
  }
}

///|
fn code_unit(s : String, i : Int) -> Int {
  if s.get(i) is Some(c) {
    c.to_int()
  } else {
    -1
  }
}

///|
fn fixed_get(xs : FixedArray[Int], i : Int) -> Int {
  if xs.get(i) is Some(x) {
    x
  } else {
    -1
  }
}

///|
fn view_get(xs : ArrayView[Int], i : Int) -> Int {
  if xs.get(i) is Some(x) {
    x
  } else {
    -1
  }
}

///|
fn pair_sum(xs : Array[Int], i : Int, j : Int) -> Int {
  if xs.get(i) is Some(a) && xs.get(j) is Some(b) {
    a + b
  } else {
    -1
  }
}

///|
fn guard_test(xs : Array[Int], i : Int) -> Int {
  match i {
    n if xs.get(n) is Some(x) => x
    _ => -1
  }
}

///|
fn nested(xs : Array[Int?], i : Int) -> Int {
  if xs.get(i) is Some(Some(x)) {
    x
  } else {
    -1
  }
}

///|
fn main {
  let xs = [1, 2, 3]
  println(sum_all(xs))
  println(is_answer(xs, 1))
  println(is_answer([40, 41, 42], 2))
  println(is_answer(xs, -1))
  println(is_answer(xs, 3))
  let r : Ref[Int] = { val: -1 }
  println(effectful(xs, r))
  println(r.val)
  println(first_byte(b"AB"))
  println(first_byte(b""))
  println(code_unit("A", 0))
  println(code_unit("A", 1))
  println(code_unit("A", -1))
  println(fixed_get([10, 20], 1))
  println(fixed_get([10, 20], 2))
  println(view_get(xs[:], 2))
  println(view_get(xs[1:], 2))
  println(pair_sum(xs, 0, 2))
  println(pair_sum(xs, 0, 5))
  println(guard_test(xs, 2))
  println(guard_test(xs, 9))
  println(nested([Some(7), None], 0))
  println(nested([Some(7), None], 1))
}
