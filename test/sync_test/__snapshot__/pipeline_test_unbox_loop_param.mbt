///|
struct S {
  x : Int
  y : Int
}

///|
fn g1(s : S) -> Unit {
  match s {
    { x: 0, y } => println(y)
    { x, y: _ } => g1({ ..{ x, y: s.y + 1 }, x: x - 1 })
  }
}

///|
fn mixed_record_tuple_params(state : S, pair : (Int, Int), acc : Int) -> Int {
  for state = state, pair = pair, acc = acc {
    if state.x == 0 {
      break state.y + pair.0 + pair.1 + acc
    }
    continue { x: state.x - 1, y: state.y + 2, },
      (pair.0 + 3, pair.1 + 4),
      acc + state.x
  }
}

///|
#inline(never)
fn consume_state(state : S) -> Int {
  state.x + state.y
}

///|
fn selective_params(active : S, escaped : S, steps : Int) -> Int {
  for active = active, escaped = escaped, steps = steps {
    if steps == 0 {
      break active.x + active.y + consume_state(escaped)
    }
    let next_active = { x: active.x + 1, y: active.y + 2, }
    let next_escaped = { x: escaped.x + 3, y: escaped.y + 4, }
    continue next_active, next_escaped, steps - 1
  }
}

///|
#inline(never)
fn mark(value : Int) -> Int {
  println(value)
  value
}

///|
#inline(never)
fn make_state(x : Int, y : Int) -> S {
  { x, y, }
}

///|
fn argument_order(state : S) -> Int {
  for before = 0, state = state, after = 0, steps = 1 {
    if steps == 0 {
      break before + state.x + state.y + after
    }
    continue mark(1), make_state(mark(2), mark(3)), mark(4), steps - 1
  }
}

///|
fn initial_record_argument_order() -> Int {
  for before = mark(5), state = { x: mark(6), y: mark(7) }, after = mark(8), steps = 1 {
    if steps == 0 {
      break before + state.x + state.y + after
    }
    continue before + 10,
      { x: state.x + 20, y: state.y + 30 },
      after + 40,
      steps - 1
  }
}

///|
#intrinsic("%v128.i8x16_bitmask")
fn i8x16_bitmask(_value : V128) -> Int {
  0
}

///|
#inline(never)
fn scan_v128_pattern(input : BytesView) -> Int {
  for rest = input, bits = 0 {
    match rest {
      [v128le(chunk), .. tail] => continue tail, bits ^ i8x16_bitmask(chunk)
      _ => break bits ^ rest.length()
    }
  }
}

///|
fn main {
  g1(S::{ x: 2, y: 0 })
  println(mixed_record_tuple_params(S::{ x: 2, y: 5, }, (7, 11), 13))
  println(selective_params(S::{ x: 1, y: 2, }, S::{ x: 3, y: 4, }, 2))
  println(argument_order(S::{ x: 99, y: 99, }))
  println(initial_record_argument_order())
  println(scan_v128_pattern(Bytes::make(33, b'A')[:]))
}
