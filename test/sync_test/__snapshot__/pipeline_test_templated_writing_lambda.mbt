struct Writer(StringBuilder)

fn Writer::write(self : Self, value : String) -> Unit {
  self.0.write_string("[write:")
  self.0.write_string(value)
  self.0.write_string("]")
}

fn Writer::write_string(self : Self, value : String) -> Unit {
  self.0.write_string("[literal:")
  self.0.write_string(value)
  self.0.write_string("]")
}

fn Writer::f(self : Self) -> Unit {
  self.0.write_string("[callback:f]")
}

fn main {
  let lhs : Writer = StringBuilder::new()
  let value = "value"
  lhs <+ "prefix\{sb => { println("hi"); sb.f() }}suffix\{value}"
  println(lhs.0.to_string())
}
