///|
pub impl ToJson for Expr with fn to_json(self) {
  self.json_repr()
}

///|
pub impl ToJson for Impl with fn to_json(self) {
  self.json_repr()
}

///|
#deprecated
#doc(hidden)
pub extend Expr with ToJson::{to_json}

///|
#deprecated
#doc(hidden)
pub extend Impl with ToJson::{to_json}
