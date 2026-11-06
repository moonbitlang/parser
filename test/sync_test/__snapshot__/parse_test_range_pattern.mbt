const C1 = 0
const C2 = 42

fn i(x : Int) -> Unit {
  match x {
    C1..=C2 => ()
    100..<200 => ()
    _..<0 => ()
    0..<_ => ()
    x..<Some(_) => println("invalid operand")
    _..=_ => println("inclusive no upper bound")
  }
}

fn c(x : Char) -> Unit {
  match c {
    'a'..='z' => ()
    'A'..='Z' => ()
    _ => ()
  }
}

fn canonical(x : Int) -> Bool {
  match x {
    -3..<=0 | 1..<=1 => true
    _..<=-4 => false
    _ => false
  }
}

fn descending(x : Int) -> Bool {
  x is 3>..0 | 3>=..0
}
