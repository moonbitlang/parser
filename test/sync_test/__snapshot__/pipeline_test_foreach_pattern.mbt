fn main {
  for (x, y) in [ (1, 10), (2, 20) ] {
    println(x + y)
  }
  for (x, y) in [ (5, 50), (6, 60) ]; sum = 0; sum = sum + x + y {
    println(sum)
  }
  for (x, y) in [ (3, 30), (4, 40) ].iter(); sum = 0; sum = sum + x + y {
    println(sum)
  }
  for (i : Int) in 0..<2 {
    println(i)
  }
  for (c : Char) in "ab" {
    println(c)
  }
  for (i : Int), (x, y) in [ (7, 70), (8, 80) ] {
    println(i + x + y)
  }
  for i, (x, y) in [ (1, 10), (2, 20) ]; sum = 0; sum = sum + i + x + y {
    println(sum)
  }
  for _, (x, y) in [ (9, 90) ] {
    println(x + y)
  }

  let pairs : Map[String, (Int, Int)] = { "a": (10, 100), "b": (20, 200) }
  for key, (x, y) in pairs {
    println(key + ":" + (x + y).to_string())
  }

  let sums = [ for (x, y) in [ (1, 2), (3, 4) ] => x + y ]
  debug(sums)
  let indexed_sums = [ for (i : Int), (x, y) in [ (1, 2), (3, 4) ] => i + x + y ]
  debug(indexed_sums)
  let sums = [| for (x, y) in [ (1, 2), (3, 4) ].iter() if x < 3 => x + y |]
  while sums.next() is Some(sum) {
    println(sum)
  }
  let map_sums = [| for key, (x, y) in pairs if key == "b" => x + y |]
  while map_sums.next() is Some(sum) {
    println(sum)
  }
  _test_additional_foreach_patterns()
}

fn _consume_pair_patterns(
  pairs : Iter2[(Int, Int), (Int, Int)],
) -> Unit {
  for (a, b), (c, d) in pairs {
    println(a + b + c + d)
  }
}

struct Point {
  x : Int
  y : Int
}

struct WeightedPoint {
  point : Point
  weight : Int
}

enum PairBox {
  PairBox((Int, Int))
}

fn _test_additional_foreach_patterns() -> Unit {
  for (x, _) as pair in [(5, 50), (6, 60)] {
    println(x + pair.1)
  }
  for ((x, y) : (Int, Int)) in [(7, 80)] {
    println(x + y)
  }
  for PairBox((x, y)) in [PairBox((8, 90))] {
    println(x + y)
  }

  let weighted_points : Array[WeightedPoint] = [
    { point: { x: 1, y: 2, }, weight: 10, },
    { point: { x: 3, y: 4, }, weight: 20, },
  ]
  for { point: { x, y, }, weight, } in weighted_points {
    println(x + y + weight)
  }
  for i, { point: { x, y, }, weight: _, } in weighted_points {
    println(i + x + y)
  }

  let fixed_pairs : FixedArray[(Int, Int)] = [(10, 20), (30, 40)]
  for (x, y) in fixed_pairs {
    println(x + y)
  }
  let readonly_pairs : ReadOnlyArray[(Int, Int)] = [(50, 60)]
  for (x, y) in readonly_pairs {
    println(x + y)
  }
  let pair_view : ArrayView[(Int, Int)] = [(70, 80)][:]
  for (x, y) in pair_view {
    println(x + y)
  }

  _consume_pair_patterns(Iter2([((1, 2), (3, 4)), ((5, 6), (7, 8))].iter()))
  let pair_sums = [|
    for
      (a, b), (c, d) in Iter2(
        [((1, 10), (100, 1000)), ((2, 20), (200, 2000))].iter(),
      ) => a + b + c + d
  |]
  while pair_sums.next() is Some(sum) {
    println(sum)
  }
}
