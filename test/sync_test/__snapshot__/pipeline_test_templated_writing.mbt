struct Writer (StringBuilder)
fn[T : Show] Writer::write(self : Self, x : T) -> Unit {
  self.0.write_string(repr(x))
}
fn Writer::write_string(self : Self, s : String) -> Unit {
  self.0.write_string(s)
}

struct TraceWriter (StringBuilder)
fn[T : Show] TraceWriter::write(self : Self, x : T) -> Unit {
  self.0.write_string("[value:")
  self.0.write_string(repr(x))
  self.0.write_string("]")
}
fn TraceWriter::write_string(self : Self, s : String) -> Unit {
  self.0.write_string("[literal:")
  self.0.write_string(s)
  self.0.write_string("]")
}

struct Channels (Array[Writer])

#alias("_[_]")
fn Channels::get(self : Self, i : Int) -> Writer {
  println("Channels::get")
  self.0[i]
}

impl Show for Channels with output(s, buf) {
  buf.write_string((s.0)[0].0.to_string())
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

  let chan = Channels([StringBuilder::new()])
  chan[0] <+ "abc\{def}ghi"
  println(chan)

  let trace : TraceWriter = StringBuilder::new()
  trace <+ ""
  trace <+ "asdf\{def}"
  println(trace.0.to_string())

  let builder = StringBuilder::new()
  builder <+ "abc\{def}ghi"
  println(builder.to_string())
}
