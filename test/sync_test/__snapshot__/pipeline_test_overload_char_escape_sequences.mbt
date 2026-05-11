///|
fn main {
  println("--- Overloaded char literal for Byte ---")
  b'a' |> println
  b'\xFF' |> println
  b'\o377' |> println
  b'\\' |> println
  println("--- Overloaded char literal for Int ---")
  ('a' : Char) |> println
  ('\\' : Char) |> println
  ('\u{1F430}' : Char) |> println
  println("--- Array of overloaded char literal for Bytes ---")
  println(([b'a', b'\xFF', b'\o377', b'\\'] : Bytes))
  println("--- Array of overloaded char literal for String ---")
  println((['a', '\\', '\u{1F430}'] : String))
}
 
