enumview[A: Show] V {
  SNil
  SCons(String, List[A])
} for List[A] with view(self: List[A]) {
  match self {
    Nil => SNil
    Cons(x, xs) => SCons(x.to_string(), xs)
  }
}

enumview V { // cc1 
  // cc2
  C // cc3
  // cc4
  D // cc5
  // cc6
} for A with v(x) { // cc7
  // cc8
  C // cc9
  // cc10
} // cc11

enumview V {  C
  D } for A with f() { C }
