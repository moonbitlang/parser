#intrinsic("%string.code_units")
fn String::code_units_for_test(self : String) -> ArrayView[UInt16] {
  self.code_units()
}

#intrinsic("%stringview.code_units")
fn StringView::code_units_for_test(self : StringView) -> ArrayView[UInt16] {
  self.code_units()
}

fn main {
  let str = "A🤣B"
  println("String chars")
  for c in str {
    println(c.to_int())
  }

  println("String indexed chars")
  for i, c in str {
    println("\{i}:\{c.to_int()}")
  }

  let view = str[:]
  println("StringView chars")
  for c in view {
    println(c.to_int())
  }

  println("StringView indexed chars")
  for i, c in view {
    println("\{i}:\{c.to_int()}")
  }

  println("String loop vars")
  for c in str; i = 0 {
    println("\{i}:\{c.to_int()}")
    continue i + 1
  }

  println("StringView loop vars")
  for c in view; i = 0 {
    println("\{i}:\{c.to_int()}")
    continue i + 1
  }

  let shifted_view = "ZA🤣B"[1:]
  println("StringView nonzero start")
  for i, c in shifted_view {
    println("\{i}:\{c.to_int()}")
  }

  println("StringView nonzero start loop vars")
  for c in shifted_view; i = 0 {
    println("\{i}:\{c.to_int()}")
    continue i + 1
  }

  println("String code units")
  for u in str.code_units_for_test() {
    println(u.to_int())
  }

  println("String code units loop vars")
  for u in str.code_units_for_test(); i = 0 {
    println("\{i}:\{u.to_int()}")
    continue i + 1
  }

  println("String indexed code units")
  for i, u in str.code_units_for_test(); sum = 0; sum = sum + i + u.to_int() {
    println("\{i}:\{u.to_int()}")
  } nobreak {
    println(sum)
  }

  println("String direct call code units")
  for u in String::code_units_for_test(str); i = 0 {
    println("\{i}:\{u.to_int()}")
    continue i + 1
  }

  println("String pipe code units")
  for u in (str |> String::code_units_for_test); i = 0 {
    println("\{i}:\{u.to_int()}")
    continue i + 1
  }

  println("String reverse pipe code units")
  for u in (String::code_units_for_test <| str); i = 0 {
    println("\{i}:\{u.to_int()}")
    continue i + 1
  }

  println("StringView code units loop vars")
  for u in shifted_view.code_units_for_test(); i = 0 {
    println("\{i}:\{u.to_int()}")
    continue i + 1
  }
}
