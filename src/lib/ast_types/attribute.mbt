///|
pub(all) struct AttributeId {
  qual : String?
  name : String
} derive(ToJson)

///|
pub(all) enum AttributeExpr {
  Ident(AttributeId)
  Str(StringLiteral)
  Apply(AttributeId, @list.T[AttributeProp])
} derive(ToJson)

///|
pub(all) enum AttributeProp {
  Labeled(String, AttributeExpr)
  Expr(AttributeExpr)
} derive(ToJson)

///|
pub(all) struct Attribute {
  loc : Location
  raw : String
  parsed : AttributeExpr?
} derive(ToJson)
