priv struct Tuple2(Int, Int,) derive(Debug)
// Int can be replaced by any type

fn main {
  // error!
  let x = 1 |> Tuple2(1)
  debug(x)
  // no error.
  // let _ = Tuple2(1) <| 1
  // let _ = Tuple2(1, 1)
}
