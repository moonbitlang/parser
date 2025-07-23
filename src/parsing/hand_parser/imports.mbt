///| Import types from ast_types package
typealias @syntax_types.(
  Location,
  Position,
  CharLiteral,
  StringLiteral,
  InterpLiteral,
  DocString
)

///| Import AST types 
typealias @syntax.Impls

///|
typealias @tokens.(Token, TokenKind)

///|
typealias (Token, Position, Position) as Triple

///|
typealias @compact.SemiExprProp

///|
typealias @list.T as List
