struct Writer(StringBuilder)

fn Writer::new() -> Writer {
  StringBuilder::new()
}

fn Writer::write_string(self : Self, value : String) -> Unit {
  self.0.write_string(value)
}

fn Writer::write_string_interpolation(self : Self, value : String) -> Unit {
  self.0.write_string(value)
}

fn Writer::write_object_begin(self : Self) -> Unit {
  self.0.write_string("{")
}

fn Writer::write_object_field(self : Self, key : String, value : String) -> Unit {
  self.0.write_string(key)
  self.0.write_string(":")
  self.0.write_string(value)
  self.0.write_string(",")
}

fn Writer::write_object_end(self : Self) -> Unit {
  self.0.write_string("}\n")
}

fn Writer::snapshot(self : Self) -> String {
  self.0.to_string()
}

struct Holder {
  writer : Writer?
}

fn get_writer(writer : Writer?) -> Writer? {
  writer
}

fn take_unit(_ : Unit) -> Unit {
  ()
}

fn print_optional_writer(writer : Writer?) -> Unit {
  match writer {
    Some(writer) => println(writer.snapshot())
    None => println("<none>")
  }
}

fn main {
  let writer = Writer::new()
  let some = Some(writer)
  some <? "value \{"one"}\n"
  some <? #|raw
          #|line
          #|
  some <? $|interp \{"two"}
          $|line \{"three"}
          $|
  some <? { "a": "A", "b": "B" }
  some <? {}
  let none : Writer? = None
  none <? "skipped"
  none <? { "skip": "me" }
  take_unit(some <? "nested")
  let _ = some <? "as expr"
  println(writer.snapshot())

  let direct = Writer::new()
  direct <+ { "direct": "value" }
  println(direct.snapshot())

  let paren_writer = Writer::new()
  (Some(paren_writer)) <? "paren"
  println(paren_writer.snapshot())

  let call_writer = Writer::new()
  get_writer(Some(call_writer)) <? "call"
  println(call_writer.snapshot())

  let holder = { writer: Some(Writer::new()) }
  holder.writer <? "field"
  print_optional_writer(holder.writer)

  let writers = [Some(Writer::new())]
  writers[0] <? "index"
  print_optional_writer(writers[0])
}
