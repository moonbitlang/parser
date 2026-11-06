fn f(x: Int) -> Unit {
  match x {
    1 if x is 0 //cc1
    => ()
    // cc2
    2 if { match x { 1 => true; _ => false } } => // cc3
     ()
    3 if //cc4
    x >= 0 && //cc5
    x <= 100 => () //cc6
    // cc7
    c if c < 0 // cc8
     => ()
  }
}
