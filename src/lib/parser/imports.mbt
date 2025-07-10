///| Import types from ast_types package
typealias @ast_types.(
  Location,
  Position,
  Comment,
  CharLiteral,
  StringLiteral,
  InterpLiteral
)

///| Import AST types 
typealias @ast.(
  Constant,
  Visibility,
  Impl,
  TypeDecl,
  TypeDeclBinder,
  DerivingDirective,
  LongIdent,
  Argument,
  Expr,
  ArgumentKind,
  Var,
  TypeDesc,
  Accessor
)

fnalias @ast.(
  make_interps,
  make_int,
  make_double,
  make_float,
  make_constr_pattern,
  make_constant_pattern
)