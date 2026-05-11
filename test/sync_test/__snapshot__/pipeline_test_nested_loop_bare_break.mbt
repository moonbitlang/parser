//!build: -w -27

// Test: bare break inside inner loop within loop construct
// Regression tests for nested loop bare break codegen bug

///|
#warnings("-useless_loop")
fn test_for_break_in_loop(x : Int) -> Unit {
  loop x {
    x =>
      if x is 42 {
        for ;; {
          break
        }
      } else {
        println("test1: not 42")
      }
  }
}

///|
#warnings("-useless_loop")
fn test_while_break_in_loop(x : Int) -> Unit {
  loop x {
    x =>
      if x is 42 {
        while true {
          break
        }
      } else {
        println("test2: not 42")
      }
  }
}

///|
#warnings("-useless_loop")
fn test_break_with_side_effect_in_loop(x : Int) -> Unit {
  loop x {
    x => {
      println("test3: before inner loop")
      for ;; {
        break
      }
    }
  }
}

///|
fn test_bare_break_then_continue(x : Int) -> Unit {
  loop x {
    0 => println("test4: done")
    x => {
      for ;; {
        break
      }
      continue x - 1
    }
  }
}

///|
fn test_multiple_inner_loops_with_break(x : Int) -> Unit {
  loop x {
    x =>
      if x > 0 {
        for ;; {
          break
        }
        println("test5: between loops")
        while true {
          break
        }
        continue x - 1
      } else {
        println("test5: done")
      }
  }
}

///|
fn test_nested_for_break_in_loop() -> Unit {
  loop 3 {
    0 => println("test6: done")
    x => {
      for ;; {
        for ;; {
          break
        }
        break
      }
      continue x - 1
    }
  }
}

///|
fn init {
  test_for_break_in_loop(42)
  println("---")
  test_while_break_in_loop(42)
  println("---")
  test_break_with_side_effect_in_loop(1)
  println("---")
  test_bare_break_then_continue(3)
  println("---")
  test_multiple_inner_loops_with_break(2)
  println("---")
  test_nested_for_break_in_loop()
}
