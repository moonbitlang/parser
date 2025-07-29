///|
pub(all) struct AttributeId {
  qual : String?
  name : String
}

///|
pub(all) enum AttributeExpr {
  Ident(AttributeId)
  Str(StringLiteral)
  Apply(AttributeId, @list.T[AttributeProp])
}

///|
pub(all) enum AttributeProp {
  Labeled(String, AttributeExpr)
  Expr(AttributeExpr)
}

///|
pub(all) struct Attribute {
  loc : Location
  raw : String
  parsed : AttributeExpr?
}

///|
pub impl ToJson for Attribute with to_json(_) {
  Json::null()
}
