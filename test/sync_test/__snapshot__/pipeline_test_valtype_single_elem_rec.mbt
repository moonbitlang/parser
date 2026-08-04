///|
/// A transparent value struct whose field is recursively parameterized by the
/// struct type. `FixedArray` provides the indirection that makes it representable.
#valtype
struct S {
  a : FixedArray[S]
} derive(Debug)

///|
fn count(s : S) -> Int {
  let children = s.a
  let mut n = 1
  for i = 0; i < children.length(); i = i + 1 {
    n = n + count(children[i])
  }
  n
}

///|
fn depth(s : S) -> Int {
  let children = s.a
  let mut d = 0
  for i = 0; i < children.length(); i = i + 1 {
    let cd = depth(children[i])
    if cd > d {
      d = cd
    }
  }
  d + 1
}

///|
fn make_leaf() -> S {
  S::{ a: [] }
}

///|
#warnings("-unused_struct_update")
fn main {
  let leaf = S::{ a: [] }
  let node = S::{ a: [leaf, leaf, leaf] }
  let root = S::{ a: [node, leaf] }
  println(count(leaf))
  println(count(node))
  println(count(root))
  println(depth(root))
  println(make_leaf().a.length())
  println(root.a[0].a.length())
  let root2 = { ..root, a: [leaf] }
  println(count(root2))
  debug(leaf)
  debug(node)
}
