///|
pub impl ToJson for Expr with fn to_json(self) {
  self.json_repr()
}

///|
pub impl ToJson for Impl with fn to_json(self) {
  self.json_repr()
}
