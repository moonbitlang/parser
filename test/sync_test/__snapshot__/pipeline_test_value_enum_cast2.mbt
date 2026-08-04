#valtype
enum Color {
  RGB(Int, Int, Int)
  Named(name~: String)
} derive(Debug)

fn main {
  let c = Named(name="blue")
  g(c)
}

fn f(c: Color) -> Unit {
  debug(c)
}

fn g(c: Color) -> Unit {
  match c {                 // ...and match it in the same scope
    RGB(_) => ()
    Named(..) as x => {
      println(x.name)
      f(x)
    }
  }

}
