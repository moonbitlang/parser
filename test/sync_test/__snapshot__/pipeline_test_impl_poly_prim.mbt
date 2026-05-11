///|
trait Seq {
  length(Self) -> Int
}

///|
impl[X] Seq for FixedArray[X] with length(self) = "%fixedarray.length"

///|
fn[X : Seq] len(s : X) -> Int {
  s.length()
}

///|
fn main {
  let arr : FixedArray[_] = [ 1, 2, 3 ]
  println(arr |> len)
}
