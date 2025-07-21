struct single {
  r1:int
}

struct multi {
  r1:int
  r2:bool
}

struct many {
  r1:int
  r2:bool
  r3:int
  r4:bool
}

fn init {
  let r1 = 1
  let r2 = true
  let one = 1
  let r4 = false
  let block = r1
  let single = { r1, }
  let multi = { r1, r2 }
  let multi_comma = { r1, r2 }
  let hybrid = { r1:one, r2 }
  let hybrid_comma = { r1:one, r2 }
  let no_punning = { r1:r1, r2:r2 }
  let no_punning_comma = { r1:r1, r2:r2 }
  let many = { r1, r2, r3:one, r4 }
  ()
}

