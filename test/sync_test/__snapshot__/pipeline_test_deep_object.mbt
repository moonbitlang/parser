enum Deep {
  Done
  More(Deep)
}

fn make(depth : Int) -> Deep {
  for curr_depth = depth, acc = Done {
    match (curr_depth, acc) {
      (0, acc) => break acc
      (n, acc) => continue n - 1, More(acc)
    }
  }
}

fn main {
  ignore(make(1 << 18))
}
