struct Writer (StringBuilder)
fn[T : Debug] Writer::write(self : Self, x : T) -> Unit {
  self.0.write_string(to_repr(x).to_string())
}
fn Writer::write_string(self : Self, s : String) -> Unit {
  self.0.write_string(s)
}

struct TraceWriter (StringBuilder)
fn[T : Debug] TraceWriter::write(self : Self, x : T) -> Unit {
  self.0.write_string("[value:")
  self.0.write_string(to_repr(x).to_string())
  self.0.write_string("]")
}
fn TraceWriter::write_string(self : Self, s : String) -> Unit {
  self.0.write_string("[literal:")
  self.0.write_string(s)
  self.0.write_string("]")
}

struct Sql {
  builder : StringBuilder
}
fn Sql::Sql() -> Sql {
  println("Sql::Sql() was called. It should only be called once.")
  { builder: StringBuilder::new() }
}
fn[T : Show] Sql::write(self : Self, x : T) -> Unit {
  self.builder.write_string(Show::to_string(x))
}
fn Sql::write_string(self : Self, s : String) -> Unit {
  self.builder.write_string(s)
}
fn Sql::write_end(self : Self) -> String {
  self.builder.to_string()
}

fn consume_sql(sql : String) -> Unit {
  println(sql)
}

fn main {
  let writer : Writer = StringBuilder::new()
  let def = Some("text")

  writer <+ "abc\{def}ghi\u{1F430}\n"
  writer <+ #|\n\t\{def}
            $|\t\{def}jkl
            #|mn
            #|

  writer
    <+
    $|0000
    $|1111\{def}2222
    $|3333
    $|

  writer
    <+
    #|end

  println(writer.0.to_string())

  let trace : TraceWriter = StringBuilder::new()
  trace <+ ""
  trace <+ "asdf\{def}"
  println(trace.0.to_string())

  consume_sql(Sql <+ "select \{def} from dual")

  let builder = StringBuilder::new()
  builder <+ "abc\{to_repr(def)}ghi"
  println(builder.to_string())
}
