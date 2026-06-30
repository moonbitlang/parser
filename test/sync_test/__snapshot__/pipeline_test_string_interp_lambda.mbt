fn StringBuilder::f(self : StringBuilder) -> Unit {
  self.write_string("[f]")
}

fn main {
  let value = "value"
  let direct = "direct \{sb => sb.f()} \{value}"
  println(direct)
  let block = "block \{sb => { println("callback"); sb.f() }} end\{_ => ()}"
  println(block)
}
