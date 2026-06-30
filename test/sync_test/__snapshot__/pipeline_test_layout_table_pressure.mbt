///|
#valtype
struct LayoutLeaf {
  i0 : Int
  r0 : String
  i1 : Int64
  r1 : Bytes
  d0 : Double
  r2 : String
} derive(Debug)

///|
struct LayoutBranch {
  first : LayoutLeaf
  mark : String
  serial : Int64
  second : LayoutLeaf
  payload : Bytes
  tail : String
  alternate : String?
} derive(Debug)

///|
struct LayoutBox {
  name : String
  n0 : Int
  left : LayoutBranch
  blob : Bytes
  n1 : Int64
  right : LayoutBranch
  checkpoint : String
  factor : Double
  leaf : LayoutLeaf
  payload2 : Bytes
  trailer : String
  n2 : Int
  flag : String?
} derive(Debug)

///|
enum LayoutEnvelope {
  Empty
  Packed(LayoutBox, FixedArray[LayoutBranch], FixedArray[LayoutLeaf], String)
} derive(Debug)

///|
fn mk_bytes(seed : Int, len : Int) -> Bytes {
  Bytes::makei(len, i => (seed + i + 65).to_byte())
}

///|
fn mk_leaf(n : Int, ch : Char, label : String) -> LayoutLeaf {
  LayoutLeaf::{
    i0: n,
    r0: label + ":" + String::make(n + 1, ch),
    i1: n.to_int64(),
    r1: mk_bytes(n, n + 3),
    d0: n.to_double() + 0.25,
    r2: label + ":mid",
  }
}

///|
fn mk_branch(a : LayoutLeaf, b : LayoutLeaf, label : String) -> LayoutBranch {
  LayoutBranch::{
    first: a,
    mark: label + ":mark",
    serial: 1000,
    second: b,
    payload: mk_bytes(20, 7),
    tail: label + ":tail",
    alternate: Some(label + ":alternate"),
  }
}

///|
fn mk_box(
  leaf0 : LayoutLeaf,
  leaf1 : LayoutLeaf,
  leaf2 : LayoutLeaf,
  branch0 : LayoutBranch,
  branch1 : LayoutBranch
) -> LayoutBox {
  LayoutBox::{
    name: "box-root",
    n0: 7,
    left: branch0,
    blob: mk_bytes(30, 8),
    n1: 64,
    right: branch1,
    checkpoint: "checkpoint",
    factor: 3.5,
    leaf: leaf2,
    payload2: mk_bytes(40, 9),
    trailer: leaf0.r2 + ":" + leaf1.r2,
    n2: 9,
    flag: Some("maybe"),
  }
}

///|
fn main {
  let leaf0 = mk_leaf(0, 'a', "leaf0")
  let leaf1 = mk_leaf(1, 'b', "leaf1")
  let leaf2 = mk_leaf(2, 'c', "leaf2")
  let branch0 = mk_branch(leaf0, leaf1, "branch0")
  let branch1 = mk_branch(leaf2, leaf1, "branch1")
  let branch2 = mk_branch(leaf1, leaf0, "branch2")
  let box = mk_box(leaf0, leaf1, leaf2, branch0, branch1)
  let branches : FixedArray[LayoutBranch] = [branch0, branch1, branch2]
  let leaves : FixedArray[LayoutLeaf] = [leaf0, leaf1, leaf2, leaf1]
  let envelope = LayoutEnvelope::Packed(box, branches, leaves, "done")

  println(box.name)
  println(box.left.first.r0)
  println(box.left.second.r0)
  println(box.right.first.r2)
  println(branches[1].mark)
  println(branches[2].second.r0)
  println(branches[0].payload.length())
  println(leaves[2].r1.length())
  match envelope {
    Packed(box2, branches2, leaves2, suffix) => {
      println(branches2[1].tail)
      println(suffix)
      println(box2.leaf.r1.length())
      println(leaves2[1].r2)
      match box2.flag {
        Some(flag) => println(flag)
        None => println("none")
      }
    }
    Empty => println("empty")
  }
}
