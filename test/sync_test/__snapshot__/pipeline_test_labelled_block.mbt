///|
fn classify(n : Int) -> Int {
  result~: {
    if n < 0 {
      break result~ -n
    }
    n + 1
  }
}

///|
fn nested(exit_outer : Bool) -> Int {
  outer~: {
    let inner = inner~: {
      if exit_outer {
        break outer~ 42
      }
      break inner~ 7
    }
    inner + 1
  }
}

fn exit_with_defer(take_break : Bool) -> Int {
  exit~: {
    defer println("labelled block defer")
    if take_break {
      println("breaking from labelled block")
      break exit~ 10
    }
    println("falling through labelled block")
    20
  }
}

///|
fn exit_from_nested_loop() -> Int {
  done~: {
    defer println("labelled block defer after nested loop")
    for ;; {
      defer println("nested loop defer")
      break done~ 30
    }
  }
}

///|
fn continue_through_labelled_block() -> Unit {
  outer~: for i in 0..<2 {
    step~: {
      defer println("continue through labelled block defer")
      if i == 0 {
        continue outer~
      }
      break step~
    }
    println("after labelled block")
  }
}

///|
fn labelled_block_inside_defer() -> Unit {
  defer {
    deferred~: {
      println("inside deferred labelled block")
      break deferred~
    }
  }
  println("before deferred labelled block")
}

///|
fn exit_from_initializer() -> Int {
  result~: {
    let value = { break result~ 50 }
    value + 1
  }
}

///|
fn main {
  println(classify(-3))
  println(classify(3))
  println(nested(false))
  println(nested(true))
  println(exit_with_defer(false))
  println(exit_with_defer(true))
  println(exit_from_nested_loop())
  continue_through_labelled_block()
  labelled_block_inside_defer()
  println(exit_from_initializer())
}
