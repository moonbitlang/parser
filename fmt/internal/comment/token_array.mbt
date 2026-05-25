///|
struct NodeArray(Array[Key?])

///|
let empty_node_array : NodeArray = []

///|
pub fn NodeArray::empty() -> NodeArray {
  []
}

///|
pub fn NodeArray::op_get(self : Self, idx : Int) -> Key? {
  self.0.get(idx).unwrap_or(None)
}

///|
pub fn NodeArray::last(self : Self) -> Key? {
  self.0.last().unwrap_or(None)
}

///|
pub impl Pretty for NodeArray with fn pretty(self) {
  @pp.pretty(self.0)
}
