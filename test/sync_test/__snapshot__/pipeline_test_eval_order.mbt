
fn[X : Show] tee(x : X) -> X {
  println(x)
  x
}

struct R {
  x : Int
  y : Int
} derive(Debug)

enum E {
  C(Int, Int)
} derive(Debug)

fn f(x : Int, y : Int) -> Unit {
  println("f(\{x}, \{y})")
}

fn init {
  let r = R::{ x: tee(1), y: tee(2) }
  debug(r)
  let e = E::C(tee(1), tee(2))
  debug(e)
  let arr: FixedArray[_] = [ tee(1), tee(2) ]
  debug(arr)
  f(tee(1), tee(2))
  let t = (tee(1), tee(2))
  debug(t)
}
