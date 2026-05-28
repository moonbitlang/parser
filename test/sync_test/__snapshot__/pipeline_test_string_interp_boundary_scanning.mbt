///|
fn main {
  let name = "moon"
  println("brace block \{{ let x = 1; x + 2 }}")
  println("nested block \{{ let x = { let y = 4; y }; x }}")
  println("char braces \{{ let _ = '{'; let _ = '}'; 1 }}")
  println("byte char braces \{{ let _ = b'{'; let _ = b'}'; 2 }}")
  println("string braces \{{ let _ = "{"; let _ = "}"; 3 }}")
  println("byte string braces \{{ let _ = b"{"; let _ = b"}"; 4 }}")
  println("recursive \{"hello \{name}"}")
  println("recursive quoted \{"hello \{"world"}"}")
  println("deep braces \{{ let a = { let b = { let c = { 5 }; c }; b }; a }}")
  println("mixed brace atoms \{{ let _ = "{"; let _ = '}'; let _ = b"{"; let _ = b'}'; "ok" }}")
  println("deep recursive \{"level1 \{"level2 \{"level3 \{"level4 \{name}"}"}"}"}")
  println("recursive mixed \{"value \{{ let _ = "{"; let _ = '}'; name }}"}")
}
