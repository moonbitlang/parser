///|
let arr : Array[Int] = [ 1, 2, 3 ]

///|
let map : Map[Int, Int] = { 1: 2, 2: 4, 3: 8 }

///|
fn[X : Debug] dump_iter(it : Iter[X]) -> Unit {
  println("<<<<<<")
  while it.next() is Some(elem) {
    println("got element \{Debug::to_repr(elem)}")
  }
  println(">>>>>>")
}

///|
fn[X : Show] tee(x : X) -> X {
  println("tee(\{x})")
  x
}

///|
fn main {
  // range
  dump_iter([ for x in 0..<3 => tee(x) ])
  dump_iter([ for x in 1..<=4 => tee(x - 1) ])
  dump_iter([ for x in 3>..0 => tee(x) ])
  dump_iter([ for x in 4>=..1 => tee(x - 1) ])
  dump_iter([ for x in tee(0)..<tee(10) if (tee(x) % 2) is 0 => x ])
  // array-like
  dump_iter([ for x in { println("evaluating arr"); arr } => tee(x) ])
  dump_iter([ for x in arr if map.get(tee(x)) is Some(y) => y ])
  dump_iter([ for i, _ in arr => tee(i) ])
  dump_iter([ for c in "Hello" if !(c is 'A'..='Z') => c ])
  dump_iter([ for b in b"hello" => b ])
  // iterator
  dump_iter([ for k, v in { println("evaluating iter"); map } => tee("\{k}: \{v}") ])
  let nats : Iter[Int] = [ for i = 0;; i = i + 1 => tee(i) ]
  let evens : Iter[Int] = [
    for x in nats if x % 2 == 0 => {
      println("\{x} is even")
      x
    }
  ]
  dump_iter(evens.take(5))
  // normal for loop
  dump_iter([
    for
      i = { println("init"); 0 }
      { println("condition"); i < 3 }
      i = { println("update"); i + 1 } => tee(i)
  ])
  dump_iter([
    for i = 0, x = 1, y = 2
        i < 3
        i = i + 1, x = y, y = x
    => (x, y)
  ])
  dump_iter([
    for i in 0..<10
        p1 = 1, p2 = 0
        p1 = p1 + p2, p2 = p1 =>
      p1
  ])
}
