///|
typealias @syntax_types.(
  Location,
  Comment,
  CharLiteral,
  StringLiteral,
  InterpLiteral
)

///|
typealias @tokens.(Token, TokenKind)

///| Import AST types 
typealias @syntax.(
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
  ConstrName,
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
  Accessor,
  ArrowFnParam,
  LocalTypeDecl,
  DeclBody,
  Attribute,
  AliasTarget,
  TraitMethodDecl,
  MultiArgCase,
  DocString,
  FunDecl,
  ExceptionDecl,
  TraitDecl,
  FieldName,
  ConstrId
)

///|
fnalias @syntax.(
  make_interps,
  make_int,
  make_double,
  make_float,
  make_attribute,
  make_arrow_fn,
  make_constant_expr,
  make_interp_expr,
  make_uplus,
  make_uminus,
  make_unot
)

///|
typealias @compact.SemiExprProp

///|
typealias @list.List
