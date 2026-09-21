///|
struct NodeTable(Map[String, TokenGroup])

///|
let empty_table : NodeTable = NodeTable::empty()

///|
pub impl Pretty for NodeTable with fn pretty(node) {
  @pp.pretty(node.0)
}

///|
pub fn NodeTable::empty() -> NodeTable {
  NodeTable(Map([]))
}

///|
#warnings("-unused_constructor")
enum TokenGroup {
  Token(Key?)
  Table(NodeTable)
  TokenArray(NodeArray)
  TableArray(NodeTableArray)
}

///|
pub impl Pretty for TokenGroup with fn pretty(group) {
  match group {
    Token(k) => @pp.ctor("Token", [@pp.pretty(k)])
    Table(t) => @pp.ctor("Table", [@pp.pretty(t)])
    TokenArray(a) => @pp.ctor("TokenArray", [@pp.pretty(a)])
    TableArray(a) => @pp.ctor("TableArray", [@pp.pretty(a)])
  }
}

///|
pub fn NodeTable::new(token_keys : Map[String, TokenGroup]) -> NodeTable {
  token_keys
}

///|
pub(all) enum Key {
  Token(Location)
  Node(Location)
} derive(Eq, Hash, Compare)

///|
pub impl Pretty for Key with fn pretty(self) {
  match self {
    Token(loc) | Node(loc) => text(Show::to_string(loc))
  }
}

///|
pub fn Key::loc(self : Self) -> Location {
  match self {
    Token(loc) | Node(loc) => loc
  }
}

///|
pub fn NodeTable::get_token(self : Self, name : String) -> Key? {
  match self.0.get(name) {
    Some(Token(key)) => key
    _ => None
  }
}

///|
pub fn NodeTable::get_table(self : Self, name : String) -> NodeTable {
  match self.0.get(name) {
    Some(Table(tbl)) => tbl
    _ => empty_table
  }
}

///|
pub fn NodeTable::get_table_array(self : Self, name : String) -> NodeTableArray {
  match self.0.get(name) {
    Some(TableArray(arr)) => arr
    _ => empty_table_array
  }
}

///|
pub fn NodeTable::get_array(self : Self, name : String) -> NodeArray {
  match self.0.get(name) {
    Some(TokenArray(arr)) => arr
    _ => empty_node_array
  }
}

///|
#deprecated
#doc(hidden)
pub extend Key with @moonbit-community/prettyprinter.Pretty::{pretty}

///|
#deprecated
#doc(hidden)
pub extend Key with Compare::{op_lt, op_le, op_ge, compare, op_gt}

///|
#deprecated
#doc(hidden)
pub extend Key with Eq::{not_equal, equal}

///|
#deprecated
#doc(hidden)
pub extend Key with Hash::{hash, hash_combine}

///|
#deprecated
#doc(hidden)
pub extend NodeTable with @moonbit-community/prettyprinter.Pretty::{pretty}

///|
#deprecated
#doc(hidden)
pub extend TokenGroup with @moonbit-community/prettyprinter.Pretty::{pretty}
