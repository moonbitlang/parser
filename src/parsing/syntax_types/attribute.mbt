///|
pub(all) struct AttributeId {
  qual : String?
  name : String
}

///|
pub(all) enum AttributeExpr {
  Ident(AttributeId)
  Str(StringLiteral)
  Apply(AttributeId, @list.List[AttributeProp])
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
pub impl ToJson for Attribute with to_json(attr) {
  { "type": "Attribute", "raw": attr.raw, "loc": attr.loc }
}
