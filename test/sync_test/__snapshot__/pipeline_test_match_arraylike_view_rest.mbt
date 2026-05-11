///|
fn score_string(s : String) -> Int {
  match s {
    ['h', .. rest, 'o'] => 10 + rest.length()
    _ => 0
  }
}

///|
fn score_string_view(s : StringView) -> Int {
  match s {
    ['h', .. rest, 'o'] => 20 + rest.length()
    _ => 0
  }
}

///|
fn score_bytes(b : Bytes) -> Int {
  match b {
    [b'h', .. rest, b'o'] => 30 + rest.length()
    _ => 0
  }
}

///|
fn score_bytes_view(b : BytesView) -> Int {
  match b {
    [b'e', .. rest, b'o'] => 40 + rest.length()
    _ => 0
  }
}

///|
fn score_array(a : Array[Int]) -> Int {
  match a {
    [1, .. rest, 5] => 50 + rest.length()
    _ => 0
  }
}

///|
fn score_fixed(a : FixedArray[Int]) -> Int {
  match a {
    [1, .. rest, 5] => 60 + rest.length()
    _ => 0
  }
}

///|
fn score_readonly(a : ReadOnlyArray[Int]) -> Int {
  match a {
    [1, .. rest, 5] => 70 + rest.length()
    _ => 0
  }
}

///|
fn score_array_view(v : ArrayView[Int]) -> Int {
  match v {
    [1, .. rest, 5] => 80 + rest.length()
    _ => 0
  }
}

///|
fn init {
  let fixed : FixedArray[Int] = [1, 2, 3, 4, 5]
  let ro : ReadOnlyArray[Int] = [1, 2, 3, 4, 5]
  let view : ArrayView[Int] = [1, 2, 3, 4, 5][:]

  println(score_string("hello"))
  println(score_string_view("hello".view()))
  println(score_bytes(b"hello"))
  println(score_bytes_view(b"hello"[1:5]))
  println(score_array([1, 2, 3, 4, 5]))
  println(score_fixed(fixed))
  println(score_readonly(ro))
  println(score_array_view(view))
}
