struct Writer (StringBuilder)
fn[T : Debug] Writer::write_string_interpolation(self : Self, x : T) -> Unit {
  self.0.write_string(to_repr(x).to_string())
}
fn Writer::write_string(self : Self, s : String) -> Unit {
  self.0.write_string(s)
}

struct TraceWriter (StringBuilder)
fn[T : Debug] TraceWriter::write_string_interpolation(self : Self, x : T) -> Unit {
  self.0.write_string("[value:")
  self.0.write_string(to_repr(x).to_string())
  self.0.write_string("]")
}
fn TraceWriter::write_string(self : Self, s : String) -> Unit {
  self.0.write_string("[literal:")
  self.0.write_string(s)
  self.0.write_string("]")
}

struct Holder {
  builder : StringBuilder
}

struct Sql {
  builder : StringBuilder
}
fn Sql::Sql() -> Sql {
  println("Sql::Sql() was called. It should only be called once.")
  { builder: StringBuilder::new() }
}
fn[T : Show] Sql::write_string_interpolation(self : Self, x : T) -> Unit {
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

fn get_builder(builder : StringBuilder) -> StringBuilder {
  builder
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

  let sql = Sql::Sql()
  sql <+ "select \{def} from dual"
  consume_sql(sql.write_end())

  let builder = StringBuilder::new()
  builder <+ "abc\{to_repr(def)}ghi"
  println(builder.to_string())

  let call_builder = StringBuilder::new()
  get_builder(call_builder) <+ "call"
  println(call_builder.to_string())

  let holder : Holder = { builder: StringBuilder::new() }
  holder.builder <+ "field"
  println(holder.builder.to_string())

  let builders = [StringBuilder::new()]
  builders[0] <+ "index"
  println(builders[0].to_string())

  let paren_builder = StringBuilder::new()
  (paren_builder) <+ "paren \{1}"
  println(paren_builder.to_string())
}
