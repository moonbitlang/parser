struct T0(T0)

fn hello(x : T0) -> Unit {
  
}

struct S(FixedArray[S])

fn children_from_construct_or_existing(b : Bool, s : S) -> FixedArray[S] {
  (if b { S([]) } else { s }).0
}

#valtype
struct T {
  a: FixedArray[T]
}

fn children_from_construct_or_existing_t(b : Bool, t : T) -> FixedArray[T] {
  (if b { T::{a:[]} } else { t }).a
}

fn main {
  let leaf = S([])
  let parent = S([leaf])
  println(children_from_construct_or_existing(true, parent).length())
  println(children_from_construct_or_existing(false, parent).length())
  let leaf = T::{a: []}
  let parent = T::{a :[leaf]}
  println(children_from_construct_or_existing_t(true, parent).length())
  println(children_from_construct_or_existing_t(false, parent).length())
}
