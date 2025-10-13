fn use_unit(_x : Unit) -> Unit {
}

struct S {
  u : Unit
} derive(Show)

fn init {
  let r = { val: 42 }
  use_unit({ r.val += 1 })
  let s : S = { u: { r.val += 1 } }
  println(s)
  use_unit(ignore(println("!")))
}
