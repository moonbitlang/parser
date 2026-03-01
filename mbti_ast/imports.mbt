///|
using @basic {type Location}

///|
pub using @syntax {
  type Visibility,
  type Constant,
  type Type,
  type ErrorType,
  type TypeDesc,
  type FieldDecl,
  type FieldName,
  type ConstrDecl,
  type ConstrId,
  type ConstrName,
  type ConstrParam,
  type Label,
  type Parameter,
}

///|
pub type QualifiedIdent = @syntax.LongIdent

///|
pub type Attributes = @list.List[@attribute.Attribute]
