#alias(plus)
#alias(plus2)
fn add(x : Int, y : Int) -> Int {
  x + y
}




struct Counter {
  mut value : Int
}
#as_free_fn(bump_counter)
fn Counter::bump(self : Counter, step? : Int = 1) -> Int {
  self.value += step
  self.value
}



fn init {
  let c = Counter::{ value: 0 }
  let _ = plus2(1, 2)
  let _ = bump_counter(c, step=3)
  ()
}
