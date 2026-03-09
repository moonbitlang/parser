///|
/// Import types from ast_types package
using @basic {type Location, type Position, type Report}

///|
/// Import AST types 
using @syntax {
  type DocString,
  type Impls,
  type ParsedImpls,
  type TopLevelTrivia,
}

///|
using @tokens {type Token, type TokenKind}

///|
type Triple = (Token, Position, Position)

///|
using @util {type Statement}

///|
using @list {type List}

///|
using @attribute {type Attribute}
