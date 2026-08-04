#valtype
priv struct W {
  value : Double
}

priv suberror E

fn make(i : Int64) -> W raise E {
  if i < 0L {
    raise E
  }
  { value: i.to_double() }
}

fn run(i : Int64) -> Double {
  try make(i).value catch {
    _ => -1.0
  }
}

fn main {
  println(run(123L))
}
