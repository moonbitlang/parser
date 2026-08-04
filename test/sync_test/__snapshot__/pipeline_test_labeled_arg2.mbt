fn[T : Add + Default] sum(xs : FixedArray[T], init~ : T = Default::default()) -> T {
  for i = 0, sum = init; i < xs.length(); {
    continue i + 1,
      sum + xs[i]
  } nobreak {
    sum
  }
}

fn init {
  println(sum([1, 2, 3]))
  println(sum([1.0, 2.0, 3.0]))
}
