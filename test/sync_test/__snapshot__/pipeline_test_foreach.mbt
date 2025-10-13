fn init {
  for x in [ 1, 2, 3, 4 ].iter() {
    println(x)
  }
  for k, v in { "x": 1, "y": 2, "z": 3 } {
    println("\{k} => \{v}")
  }
}

// control flow
fn f1(mode : Int) -> String {
  (for x in [ 1, 2, 3, 42 ].iter() {
    if x == 42 {
      if mode == 0 {
        return "return"
      } else if mode == 1 {
        break 42
      }
    }
  } else {
    0
  })
  |> Int::to_string
}

fn init {
  for mode in [ 0, 1, 2 ].iter() {
    println(f1(mode))
  }
}

// error handling
fn f2(mode : Int) -> String raise {
  for x in [ 1, 2, 3, 42 ].iter() {
    if x == 42 {
      if mode == 0 {
        return "return"
      } else if mode == 1 {
        break "break"
      } else if mode == 2 {
        raise Failure("error")
      } else {
        continue
      }
    }
  } else {
    "continue"
  }
}

fn init {
  for mode in [ 0, 1, 2, 3 ].iter() {
    println(try? f2(mode))
  }
}


// nested for
fn init {
  let it = Iter::new(fn (yield_) {
    for x in [ 1, 2, 3 ].iter() {
      for y in [ 3, 2, 1 ].iter() {
        if yield_((x, y)) == IterEnd {
          return IterEnd
        }
      }
    } else {
      IterContinue
    }
  })
  it.each(println)
  println(it.find_first(fn (t) { match t { (x, y) => x == 3 && y == 2 } }))
  println(it.find_first(fn (t) { match t { (x, _) => x > 3 } }))
}

fn init {
  let it = Iter::new(fn (yield_) {
    if yield_(0) == IterEnd { return IterEnd }
    if yield_(1) == IterEnd { return IterEnd }
    if yield_(2) == IterEnd { return IterEnd }
    IterContinue
  })
  // if the iterated expression is already an `Iter`,
  // don't insert `.iter()` call
  for x in it {
    println(x)
  }
}

fn init {
  let arr = [ 1, 2, 3 ]
  // `Iter::iter2`
  for i, x in arr.iter() {
    println("\{i} => \{x}")
  }
}
