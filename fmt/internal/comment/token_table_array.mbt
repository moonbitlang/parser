///|
struct NodeTableArray(Array[NodeTable])

///|
let empty_table_array : NodeTableArray = NodeTableArray::empty()

///|
pub fn NodeTableArray::empty() -> NodeTableArray {
  []
}

///|
pub fn NodeTableArray::op_get(self : Self, idx : Int) -> NodeTable {
  self.0.get(idx).unwrap_or_else(fn() { NodeTable::empty() })
}

///|
pub impl Pretty for NodeTableArray with fn pretty(self) {
  @pp.pretty(self.0)
}
