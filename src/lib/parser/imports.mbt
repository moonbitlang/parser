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
  Case,
  Func,
  Type,
  ErrorType,
  ArrayPattern,
  ArrayPatterns,
  Label,
  Binder,
  ConstrPatArg,
  Pattern,
  FieldDef,
  FieldDecl,
  FieldPat,
  Parameter,
  ConstrParam,
  ConstrDecl,
  TypeName,
  TrailingMark,
  MapPatElem,
  DotDotBinder,
  SpreadableElem,
  MapExprElem,
  MultilineStringElem,
  TypeVarConstraint,
  ApplyAttr,
  Constructor,
  Visibility,
  Impl,
  TypeVarBinder,
  TypeDecl,
  TypeDeclBinder,
  DerivingDirective,
  LongIdent,
  Argument,
  Expr,
  ArgumentKind,
  Var,
  TypeDesc,
  Accessor,
  ArrowFnParam,
  LocalTypeDecl,
  DeclBody,
  Attribute
)

///|
fnalias @ast.(
  make_interps,
  make_int,
  make_double,
  make_float,
  make_constr_pattern,
  make_constant_pattern,
  make_attribute
)
