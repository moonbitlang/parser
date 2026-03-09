///|
pub impl ToJson for Expr with to_json(self) {
  self.json_repr()
}

///|
pub impl ToJson for Impl with to_json(self) {
  self.json_repr()
}
