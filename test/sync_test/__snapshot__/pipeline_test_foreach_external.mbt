fn init {
  println("====")
  for x in [ 1, 2, 3, 4 ].iterator() {
    println(x)
  }
  for i, x in [ 1, 2, 3, 4 ].iterator2() {
    println("\{i} => \{x}")
  }
}

fn init {
  println("====")
  for pair in { "x": 1, "y": 2, "z": 3 }.iterator() {
    println("\{pair.0} => \{pair.1}")
  }
  for k, v in { "x": 1, "y": 2, "z": 3 }.iterator2() {
    println("\{k} => \{v}")
  }
}
