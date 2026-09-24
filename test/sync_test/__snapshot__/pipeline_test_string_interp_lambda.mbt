///|
fn StringBuilder::f(self : StringBuilder) -> Unit {
  self.write_string("[f]")
}

///|
fn main {
  let value = "value"
  let direct = "direct \{sb => sb.f()} \{value}"
  println(direct)
  let block = "block \{sb => { println("callback"); sb.f() }} end\{_ => ()}"
  println(block)
  println("return: " + callback_return(true))
  println("fallthrough: " + callback_return(false))
  println("loop return: " + callback_loop_return(2))
  println("loop fallthrough: " + callback_loop_return(4))
  println("break/continue: " + callback_loop_control())
  println("guard none: " + callback_guard_match(None))
  println("match zero: " + callback_guard_match(Some(0)))
  println("match value: " + callback_guard_match(Some(7)))
  println("nested interpolation: " + callback_nested_interpolation())
  println("nested function: " + callback_nested_function())
  println("defer return: " + callback_defer(true))
  println("defer fallthrough: " + callback_defer(false))
  println("evaluation order: " + callback_evaluation_order())
  println("discarded parameter: " + callback_discard())
  println("caught error: " + callback_catch(-1))
  println("no error: " + callback_catch(7))
  println("multiline:")
  println(callback_multiline())
}

///|
// Returning a Unit expression must leave the callback, not this String function.
fn callback_return(stop : Bool) -> String {
  let mut visits = 0
  let text = "before\{sb => { visits += 1; if stop { return sb.write_string("[stop]") }; sb.write_string("[keep]"); visits += 10 }}after\{visits}"
  visits += 100
  "\{text}/outer=\{visits}"
}

///|
fn callback_loop_return(limit : Int) -> String {
  let mut visited = 0
  let text = "<\{sb => { for n in 0..<4 { if n == limit { return }; visited += 1; sb.write(n) }; sb.write_string("done") }}>/\{visited}"
  text + "/outer"
}

///|
fn callback_loop_control() -> String {
  "<\{sb => { for n in 0..<6 { if n == 1 { continue }; if n == 4 { break }; sb.write(n) }; sb.write_string("done") }}>"
}

///|
fn callback_guard_match(value : Int?) -> String {
  "\{sb => { guard value is Some(n) else { return sb.write_string("none") }; match n { 0 => return sb.write_string("zero"); _ => sb.write(n) }; sb.write_string("done") }}/outer"
}

///|
// The inner callback shadows the builder parameter and returns independently.
fn callback_nested_interpolation() -> String {
  "outer<\{sb => { let inner = "inner<\{sb => { sb.write_string("x"); return }}>"; sb.write_string(inner); sb.write_string("/continued") }}>tail"
}

///|
fn callback_nested_function() -> String {
  "<\{sb => { let render = n => { if n == 0 { return }; sb.write(n) }; render(0); render(3); sb.write_string("done") }}>tail"
}

///|
// Deferred writes happen in reverse order before the enclosing literal resumes.
fn callback_defer(stop : Bool) -> String {
  "<\{sb => { defer sb.write_string("D1"); defer sb.write_string("D2"); sb.write_string("B"); if stop { return }; sb.write_string("E") }}>tail"
}

///|
fn callback_evaluation_order() -> String {
  let mut state = 0
  "\{{ state += 1; state }}|\{sb => { sb.write(state); state += 1; return }}|\{state}|\{sb => { state += 1; sb.write(state) }}|\{state}"
}

///|
suberror RenderError {
  RenderError(Int)
}

///|
fn render_or_raise(n : Int) -> Int raise RenderError {
  if n < 0 {
    raise RenderError(n)
  }
  n
}

///|
fn callback_catch(n : Int) -> String {
  "\{sb => { try { sb.write(render_or_raise(n)) } catch { RenderError(code) => { sb.write_string("error"); sb.write(code); return } }; sb.write_string("done") }}/outer"
}

///|
fn callback_multiline() -> String {
  let text =
    $|first \{sb => { sb.write_string("one"); return }}
    $|second \{sb => { sb.write_string("two"); return }}
    $|third
  text
}

///|
fn callback_discard() -> String {
  let mut calls = 0
  let text = "before\{_ => { calls += 1; return }}after"
  "\{text}/calls=\{calls}"
}
