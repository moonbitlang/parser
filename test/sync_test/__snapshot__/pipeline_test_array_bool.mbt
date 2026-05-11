///|
fn bit(x : Bool) -> Int {
  match x {
    true => 1
    false => 0
  }
}

///|
fn score_fixed(xs : FixedArray[Bool]) -> Int {
  let mut score = 0
  for i = 0; i < xs.length(); i = i + 1 {
    score = score * 2 + bit(xs[i])
  }
  score
}

///|
fn score_array(xs : Array[Bool]) -> Int {
  let mut score = 0
  for i = 0; i < xs.length(); i = i + 1 {
    score = score * 2 + bit(xs[i])
  }
  score
}

///|
fn score_readonly(xs : ReadOnlyArray[Bool]) -> Int {
  let mut score = 0
  for i = 0; i < xs.length(); i = i + 1 {
    score = score * 2 + bit(xs[i])
  }
  score
}

///|
fn match_fixed(xs : FixedArray[Bool]) -> Int {
  match xs {
    [] => 0
    [true] => 1
    [false] => 2
    [true, false] => 3
    [false, true] => 4
    [.. _, true, true] => 5
    _ => 6
  }
}

///|
fn match_array(xs : Array[Bool]) -> Int {
  match xs {
    [] => 10
    [true] => 11
    [false] => 12
    [true, false] => 13
    [false, true] => 14
    [.. _, true, true] => 15
    _ => 16
  }
}

///|
fn match_readonly(xs : ReadOnlyArray[Bool]) -> Int {
  match xs {
    [true, true, false, true] => 20
    [.. _, true, true] => 21
    _ => 22
  }
}

///|
let static_fixed : FixedArray[Bool] = [true, true, false, true]

///|
let static_readonly : ReadOnlyArray[Bool] = [true, true, false, true]

///|
fn main {
  let empty_fixed : FixedArray[Bool] = []
  println(empty_fixed.length())
  println(match_fixed(empty_fixed))
  println(score_fixed(static_fixed))
  println(match_fixed(static_fixed))
  println(score_readonly(static_readonly))
  println(match_readonly(static_readonly))

  let fixed : FixedArray[Bool] = [true, false, true, true, false, true, false, true]
  println(fixed.length())
  println(score_fixed(fixed))
  println(match_fixed(fixed))

  fixed[1] = true
  fixed[4] = true
  println(score_fixed(fixed))

  let made = FixedArray::make(6, false)
  made[0] = true
  made[3] = true
  made[5] = true
  println(score_fixed(made))

  made.fill(true, start=1, end=4)
  println(score_fixed(made))

  let copied : FixedArray[Bool] = [false, false, false, false, false, false]
  made.blit_to(copied, len=4, dst_offset=1)
  println(score_fixed(copied))

  made.blit_to(made, len=5, src_offset=1)
  println(score_fixed(made))

  println(match_fixed([]))
  println(match_fixed([true]))
  println(match_fixed([false]))
  println(match_fixed([true, false]))
  println(match_fixed([false, true]))
  println(match_fixed([false, true, true]))
  println(match_fixed([true, false, true]))

  let arr = Array::from_fixed_array(fixed)
  println(arr.length())
  println(score_array(arr))
  println(match_array(arr))

  arr[2] = false
  arr.push(false)
  arr.push(true)
  println(arr.length())
  println(score_array(arr))

  let arr2 : Array[Bool] = [false, true, true, false, true]
  arr2[0] = true
  arr2.push(true)
  println(score_array(arr2))
  println(match_array(arr2))

  println(match_array([]))
  println(match_array([true]))
  println(match_array([false]))
  println(match_array([true, false]))
  println(match_array([false, true]))
  println(match_array([false, true, true]))
  println(match_array([true, false, true]))
}
