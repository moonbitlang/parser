// A `label~~` block is one inserted by a source-to-source transformation. Such a
// block is transparent: an unlabelled `break` or `continue` inside it binds to
// the enclosing loop rather than being captured by the block. The block can
// still be exited by name with `break label~~`, which is a distinct token from
// `label~` and so can never collide with a label the original source wrote.

///|
fn break_binds_to_enclosing_loop() -> Int {
  let mut count = 0
  for i = 0; i < 5; i = i + 1 {
    join~~: {
      if i == 3 {
        break
      }
      count = count + 1
    }
  }
  // 3: i = 0, 1, 2 increment, then i = 3 leaves the `for`.
  // Had the block captured the `break`, i = 4 would run too and give 4.
  count
}

///|
fn continue_binds_to_enclosing_loop() -> Int {
  let mut sum = 0
  for i = 0; i < 5; i = i + 1 {
    join~~: {
      if i % 2 == 0 {
        continue
      }
      sum = sum + i
    }
  }
  // 4: only the odd iterations reach the addition.
  sum
}

///|
fn nested_transparent_blocks() -> Int {
  let mut count = 0
  for i = 0; i < 5; i = i + 1 {
    outer~~: {
      inner~~: {
        if i == 2 {
          break
        }
        count = count + 100
      }
      count = count + 1
    }
  }
  // 202: i = 0 and 1 add 101 each, then i = 2 leaves the `for` from two blocks
  // deep. Had the inner block captured the `break`, the loop would run to
  // completion and give 405.
  count
}

///|
fn labelled_break_still_exits_the_block() -> Int {
  join~~: {
    for i = 0; i < 10; i = i + 1 {
      if i == 4 {
        break join~~ i * 10
      }
    }
    -1
  }
}

///|
fn transparent_block_yields_a_value() -> Int {
  let mut total = 0
  for i = 0; i < 4; i = i + 1 {
    let v = join~~: {
      if i == 1 {
        break join~~ 0
      }
      i * 2
    }
    total = total + v
  }
  // 10: 0 + 0 + 4 + 6
  total
}

///|
fn break_with_payload_binds_to_enclosing_loop() -> Int {
  for i = 0; i < 10; i = i + 1 {
    join~~: {
      if i == 6 {
        break i
      }
    }
  } nobreak {
    -1
  }
}

///|
fn same_name_in_both_namespaces() -> Int {
  blk~: {
    let mut count = 0
    for i = 0; i < 5; i = i + 1 {
      blk~~: {
        if i == 3 {
          break
        }
        // Reached only if the inserted block wrongly captured the `break` above.
        if i == 4 {
          break blk~ 999
        }
        count = count + 1
      }
    }
    count
  }
}

///|
fn main {
  println(break_binds_to_enclosing_loop())
  println(continue_binds_to_enclosing_loop())
  println(nested_transparent_blocks())
  println(labelled_break_still_exits_the_block())
  println(transparent_block_yields_a_value())
  println(break_with_payload_binds_to_enclosing_loop())
  println(same_name_in_both_namespaces())
}
