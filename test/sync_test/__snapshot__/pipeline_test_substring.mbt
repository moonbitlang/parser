/// @intrinsic %string.substring
fn substring(str : String, start : Int, end : Int) -> String {
  let len = end - start
  let buf = @buffer.new(size_hint=len)
  buf.write_substring(str, start, len)
  buf.contents().to_unchecked_string()
}

fn use_substring(substring : (String, Int, Int) -> String, str : String, start : Int, end : Int) -> String {
  substring(str, start, end)
}

fn init {
  let s = "Hello, World!"
  let substring2 = substring
  println(substring(s, 0, 5)) // Hello
  println(substring(s, 7, 12)) // World
  println(use_substring(substring, s, 0, 12)) // Hello, World!
  println(substring(s, 0, 0)) // ""
  println(use_substring(substring, s, 0, 1)) // H
}
