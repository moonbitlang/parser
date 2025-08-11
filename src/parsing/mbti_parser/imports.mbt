///|
typealias @tokens.(Token, TokenKind)

///|
typealias @basic.(Location, Position)

///|
typealias @tokens.(Comment, CharLiteral, StringLiteral, InterpLiteral)

///|
typealias @syntax.(DocString, Attribute)

///|
typealias @mbti_ast.(
  QualifiedIdent,
  Name,
  QualifiedName,
  Label,
  Visibility,
  Constant,
  Type,
  ErrorType,
  Parameter,
  TypeParamNoConstraints,
  TypeParamWithConstraints,
  TraitMethodParameter,
  FieldDecl,
  FieldName,
  ConstrDecl,
  ConstrId,
  ConstrName,
  ConstrParam,
  AliasSig,
  TraitSig,
  TraitMethodSig,
  ImplSig,
  FuncSig,
  ValueSig,
  ConstSig,
  TypeSig,
  Sig,
  PackageImport,
  Mbti
)

///|
fnalias @syntax.make_attribute
