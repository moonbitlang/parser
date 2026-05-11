///|
let arr : Array[Int] = [ 1, 2, 3 ]

///|
let map : Map[Int, Int] = { 1: 2, 2: 4, 3: 8 }

///}
fn main {
  let xs = [ for x in 0..<10 => x ]
  debug(xs)
  let xs : FixedArray[_] = [ for x in arr => x ]
  debug(xs)
  let xs : ReadOnlyArray[_] = [ for k, v in map => (k, v) ]
  debug(xs)
  let json : Json = [ for i, x in arr => (i, x) ]
  println(json.stringify())
  let str : String = [ for x in 0..<10 => (x + 'a').unsafe_to_char() ]
  println(str)
  let bytes : Bytes = [ for x in arr.iter() => x.to_byte() ]
  debug(bytes)
  let filter = [ for i, _ in arr if map.get(i) is Some(v) => v ]
  debug(filter)
  let fib = [
    for i = 0, p1 = 1, p2 = 1
        i < 10
        i = i + 1, p1 = p1 + p2, p2 = p1 =>
      p1
  ]
  debug(fib)
  let for_cond_binder = [
    for i = 1; map.get(i) is Some(x); i = i + 1 if map.get(x) is Some(y) => y
  ]
  debug(for_cond_binder)
  let fib_alt = [
    for i in 0..<10
        p1 = 1, p2 = 0
        p1 = p1 + p2, p2 = p1 =>
      p1
  ]
  debug(fib_alt)
}
