

//!build: -w -27


trait X : Show {
  name (Self) -> String
}

type Stock String derive(Show)

impl X for Stock with name( self : Stock) -> String {
  self.inner()
}


fn init {
  let v = Stock::Stock("x") as &X
  println(v)
}
