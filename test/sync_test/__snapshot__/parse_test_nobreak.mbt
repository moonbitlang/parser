// Test nobreak keyword for while loops
fn test_while_nobreak() -> Unit {
  while true {
    break
  } nobreak {
    println("completed without break")
  }
}

// Test nobreak keyword for for loops
fn test_for_nobreak() -> Unit {
  for i = 0; i < 10; i = i + 1 {
    if i == 5 {
      break
    }
  } nobreak {
    println("completed without break")
  }
}

// Test nobreak keyword for foreach loops
fn test_foreach_nobreak() -> Unit {
  for x in [1, 2, 3] {
    println(x)
  } nobreak {
    println("completed without break")
  }
}

// Test nobreak with multiple binders in foreach
fn test_foreach_nobreak_multi() -> Unit {
  for k, v in [(1, "a"), (2, "b")] {
    println(k)
  } nobreak {
    println("done")
  }
}

// Test nested loops with nobreak
fn test_nested_nobreak() -> Unit {
  while true {
    for i = 0; i < 5; i = i + 1 {
      break
    } nobreak {
      println("inner loop completed")
    }
    break
  } nobreak {
    println("outer loop completed")
  }
}

// Test nobreak with labeled loops
fn test_labeled_nobreak() -> Unit {
  outer: while true {
    inner: for x in [1, 2, 3] {
      break outer
    } nobreak {
      println("inner done")
    }
  } nobreak {
    println("outer done")
  }
}

// Keep the nobreak block and following statement when recovering to `{`.
fn test_nobreak_recovery() -> Unit {
  for ;; {} nobreak junk { for_nobreak_block }
  after_for
  while true {} nobreak junk { while_nobreak_block }
  after_while
}
