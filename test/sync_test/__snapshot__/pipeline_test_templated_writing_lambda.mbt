///|
struct Writer(StringBuilder)

///|
fn Writer::write_string_interpolation(self : Self, value : String) -> Unit {
  self.0.write_string("[write:")
  self.0.write_string(value)
  self.0.write_string("]")
}

///|
fn Writer::write_string(self : Self, value : String) -> Unit {
  self.0.write_string("[literal:")
  self.0.write_string(value)
  self.0.write_string("]")
}

///|
fn Writer::f(self : Self) -> Unit {
  self.0.write_string("[callback:f]")
}

///|
fn main {
  let lhs : Writer = StringBuilder::new()
  let value = "value"
  lhs <+ "prefix\{sb => { println("hi"); sb.f() }}suffix\{value}"
  println(lhs.0.to_string())
  println("return: " + callback_return(true))
  println("fallthrough: " + callback_return(false))
  println("loop return: " + callback_loop_return(2))
  println("loop fallthrough: " + callback_loop_return(4))
  println("break/continue: " + callback_loop_control())
  println("guard none: " + callback_guard_match(None))
  println("match zero: " + callback_guard_match(Some(0)))
  println("match value: " + callback_guard_match(Some(7)))
  println("nested template: " + callback_nested_template())
  println("defer return: " + callback_defer(true))
  println("defer fallthrough: " + callback_defer(false))
  println("writer once: " + callback_writer_once())
  println("conditional some: " + callback_conditional(true))
  println("conditional none: " + callback_conditional(false))
  println("outer loop: " + callback_in_outer_loop())
  println("propagated error: " + callback_error(true))
  println("no error: " + callback_error(false))
  println("multiline:")
  println(callback_multiline())
}

///|
fn Writer::emit(self : Self, text : String) -> Unit {
  self.0.write_string(text)
}

///|
// The callback receives Writer, and its return must not exit this String function.
fn callback_return(stop : Bool) -> String {
  let writer : Writer = StringBuilder()
  let mut visits = 0
  writer <+
    "before\{w => { visits += 1; if stop { return w.emit("[stop]") }; w.emit("[keep]"); visits += 10 }}after\{visits.to_string()}"
  visits += 100
  "\{writer.0.to_string()}/outer=\{visits}"
}

///|
fn callback_loop_return(limit : Int) -> String {
  let writer : Writer = StringBuilder()
  let mut visited = 0
  writer <+
    "<\{w => { for n in 0..<4 { if n == limit { return }; visited += 1; w.emit(n.to_string()) }; w.emit("done") }}>\{visited.to_string()}"
  writer.0.to_string() + "/outer"
}

///|
fn callback_loop_control() -> String {
  let writer : Writer = StringBuilder()
  writer <+
    "<\{w => { for n in 0..<6 { if n == 1 { continue }; if n == 4 { break }; w.emit(n.to_string()) }; w.emit("done") }}>"
  writer.0.to_string()
}

///|
fn callback_guard_match(value : Int?) -> String {
  let writer : Writer = StringBuilder()
  writer <+
    "\{w => { guard value is Some(n) else { return w.emit("none") }; match n { 0 => return w.emit("zero"); _ => w.emit(n.to_string()) }; w.emit("done") }}/outer"
  writer.0.to_string()
}

///|
fn callback_nested_template() -> String {
  let writer : Writer = StringBuilder()
  writer <+
    "outer\{w => { w <+ "inner\{w => { w.emit("x"); return }}end"; w.emit("continued") }}tail"
  writer.0.to_string()
}

///|
fn callback_defer(stop : Bool) -> String {
  let writer : Writer = StringBuilder()
  writer <+
    "<\{w => { defer w.emit("D1"); defer w.emit("D2"); w.emit("B"); if stop { return }; w.emit("E") }}>tail"
  writer.0.to_string()
}

///|
fn counted_writer(writer : Writer, calls : Array[Int]) -> Writer {
  calls[0] += 1
  writer
}

///|
fn callback_writer_once() -> String {
  let writer : Writer = StringBuilder()
  let calls = [0]
  counted_writer(writer, calls) <+
    "A\{w => { w.emit("B"); return }}C\{w => w.emit("D")}"
  "\{writer.0.to_string()}/evaluations=\{calls[0]}"
}

///|
// None skips both callbacks and ordinary interpolation expressions.
fn callback_conditional(present : Bool) -> String {
  let writer : Writer = StringBuilder()
  let optional = if present { Some(writer) } else { None }
  let mut callbacks = 0
  let mut expressions = 0
  optional <?
    "before\{w => { callbacks += 1; w.emit("body"); return }}after\{{ expressions += 1; "value" }}"
  "\{writer.0.to_string()}/callbacks=\{callbacks}/expressions=\{expressions}"
}

///|
fn callback_in_outer_loop() -> String {
  let writer : Writer = StringBuilder()
  for i in 0..<3 {
    writer <+ "\{w => { if i == 1 { return }; w.emit(i.to_string()) }}|"
  }
  writer.0.to_string()
}

///|
suberror CallbackError

///|
fn write_with_error(writer : Writer, stop : Bool) -> Unit raise CallbackError {
  writer <+
    "before\{w => { defer w.emit("defer"); w.emit("body"); if stop { raise CallbackError }; w.emit("ok") }}after"
  writer.emit("/continued")
}

///|
fn callback_error(stop : Bool) -> String {
  let writer : Writer = StringBuilder()
  let status = try {
    write_with_error(writer, stop)
    "ok"
  } catch {
    CallbackError => "caught"
  }
  "\{status}:\{writer.0.to_string()}"
}

///|
fn callback_multiline() -> String {
  let writer : Writer = StringBuilder()
  writer <+
    $|first \{w => { w.emit("one"); return }}
    $|second \{w => { w.emit("two"); return }}
    $|third
  writer.0.to_string()
}
