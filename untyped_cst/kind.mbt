///|
pub enum NodeKind {
  SourceFile
  ExpressionFragment
  Token(Token)
  Missing(TokenKind)
  Error
  Impl_Using
  Expr_Identifier
  Expr_Literal
  Expr_Block
  Expr_Apply
  Expr_DotApply
  Expr_Field
  Expr_ArrayGet
  Expr_ArrayGetSlice
  Expr_Array
  Expr_ListComprehension
  Expr_Tuple
  Expr_Constraint
  Expr_Record
  Expr_Map
  Expr_Infix
  Expr_Prefix
  Expr_Range
  Expr_Assign
  Expr_AugmentedAssign
  Expr_If
  Expr_Match
  Expr_Try
  Expr_TryOperator
  Expr_For
  Expr_ForEach
  Expr_While
  Expr_Function
  Expr_Method
  Expr_As
  Expr_Is
  Expr_LexMatch
  Expr_LexScan
  Expr_RegexMatch
  Expr_TemplateWriting
  Expr_Let
  Expr_LetMut
  Expr_LetRec
  Expr_LetFn
  Expr_Return
  Expr_Raise
  Expr_Break
  Expr_Continue
  Expr_Defer
  Expr_Guard
  Expr_Quantifier
  Expr_Hole
  Expr_Group
  Expr_Constructor
  Expr_MultilineString
  Expr_MissingExpr
  Pattern_Binder
  Pattern_Underscore
  Pattern_Literal
  Pattern_Tuple
  Pattern_Array
  Pattern_Rest
  Pattern_Record
  Pattern_Map
  Pattern_Constructor
  Pattern_SpecialConstructor
  Pattern_Or
  Pattern_Alias
  Pattern_Range
  Pattern_Constraint
  Pattern_MissingPattern
  Type_Name
  Type_Tuple
  Type_Function
  Type_Application
  Type_Option
  Type_Object
  Type_Hole
  Type_Group
  Type_MissingType
  LexPattern_Literal
  LexPattern_RegexInterp
  LexPattern_Sequence
  LexPattern_Alias
  LexPattern_ConstantRef
  RegexPattern_Placeholder
  RegexPattern_Literal
  RegexPattern_Reference
  RegexPattern_Sequence
  RegexPattern_Alternation
  RegexPattern_Alias
  Case_MatchCase
  Case_LexCase
  Argument_Positional
  Argument_Labelled
  Argument_LabelledPun
  Argument_LabelledOption
  Argument_LabelledOptionPun
  Stmt_ProofAssert
  Stmt_ProofLet
  Decl_BalancedDecl
  Decl_Function
  Decl_Let
  Decl_Const
  Decl_Type
  Decl_Struct
  Decl_Enum
  Decl_EnumView
  Decl_Trait
  Decl_Impl
  Decl_Test
  Clause_Visibility
  Clause_Parameter
  Clause_ReturnType
  Clause_ErrorAnnotation
  Clause_Where
  Name_Identifier
  Name_Constructor
  List_ArgumentList
  List_ParameterList
  List_FieldList
  List_StatementList
  List_TypeArgumentList
  List_PatternList
  List_ConstructorList
  InterpSegment_InterpLiteralSegment
  InterpSegment_InterpSourceSegment
} derive(@debug.Debug)

///|
/// Returns whether this kind represents a top-level implementation item.
pub fn NodeKind::is_impl(self : NodeKind) -> Bool {
  match self {
    Impl_Using => true
    _ => false
  }
}

///|
/// Returns whether this kind represents an expression.
pub fn NodeKind::is_expr(self : NodeKind) -> Bool {
  match self {
    Expr_Identifier
    | Expr_Literal
    | Expr_Block
    | Expr_Apply
    | Expr_DotApply
    | Expr_Field
    | Expr_ArrayGet
    | Expr_ArrayGetSlice
    | Expr_Array
    | Expr_ListComprehension
    | Expr_Tuple
    | Expr_Constraint
    | Expr_Record
    | Expr_Map
    | Expr_Infix
    | Expr_Prefix
    | Expr_Range
    | Expr_Assign
    | Expr_AugmentedAssign
    | Expr_If
    | Expr_Match
    | Expr_Try
    | Expr_TryOperator
    | Expr_For
    | Expr_ForEach
    | Expr_While
    | Expr_Function
    | Expr_Method
    | Expr_As
    | Expr_Is
    | Expr_LexMatch
    | Expr_LexScan
    | Expr_RegexMatch
    | Expr_TemplateWriting
    | Expr_Let
    | Expr_LetMut
    | Expr_LetRec
    | Expr_LetFn
    | Expr_Return
    | Expr_Raise
    | Expr_Break
    | Expr_Continue
    | Expr_Defer
    | Expr_Guard
    | Expr_Quantifier
    | Expr_Hole
    | Expr_Group
    | Expr_Constructor
    | Expr_MultilineString
    | Expr_MissingExpr => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a pattern.
pub fn NodeKind::is_pattern(self : NodeKind) -> Bool {
  match self {
    Pattern_Binder
    | Pattern_Underscore
    | Pattern_Literal
    | Pattern_Tuple
    | Pattern_Array
    | Pattern_Rest
    | Pattern_Record
    | Pattern_Map
    | Pattern_Constructor
    | Pattern_SpecialConstructor
    | Pattern_Or
    | Pattern_Alias
    | Pattern_Range
    | Pattern_Constraint
    | Pattern_MissingPattern => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a type.
pub fn NodeKind::is_type(self : NodeKind) -> Bool {
  match self {
    Type_Name
    | Type_Tuple
    | Type_Function
    | Type_Application
    | Type_Option
    | Type_Object
    | Type_Hole
    | Type_Group
    | Type_MissingType => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a lex pattern.
pub fn NodeKind::is_lex_pattern(self : NodeKind) -> Bool {
  match self {
    LexPattern_Literal
    | LexPattern_RegexInterp
    | LexPattern_Sequence
    | LexPattern_Alias
    | LexPattern_ConstantRef => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a regex pattern.
pub fn NodeKind::is_regex_pattern(self : NodeKind) -> Bool {
  match self {
    RegexPattern_Placeholder
    | RegexPattern_Literal
    | RegexPattern_Reference
    | RegexPattern_Sequence
    | RegexPattern_Alternation
    | RegexPattern_Alias => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a case.
pub fn NodeKind::is_case(self : NodeKind) -> Bool {
  match self {
    Case_MatchCase | Case_LexCase => true
    _ => false
  }
}

///|
/// Returns whether this kind represents an argument.
pub fn NodeKind::is_argument(self : NodeKind) -> Bool {
  match self {
    Argument_Positional
    | Argument_Labelled
    | Argument_LabelledPun
    | Argument_LabelledOption
    | Argument_LabelledOptionPun => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a statement.
pub fn NodeKind::is_stmt(self : NodeKind) -> Bool {
  match self {
    Stmt_ProofAssert | Stmt_ProofLet => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a declaration.
pub fn NodeKind::is_decl(self : NodeKind) -> Bool {
  match self {
    Decl_BalancedDecl
    | Decl_Function
    | Decl_Let
    | Decl_Const
    | Decl_Type
    | Decl_Struct
    | Decl_Enum
    | Decl_EnumView
    | Decl_Trait
    | Decl_Impl
    | Decl_Test => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a clause.
pub fn NodeKind::is_clause(self : NodeKind) -> Bool {
  match self {
    Clause_Visibility
    | Clause_Parameter
    | Clause_ReturnType
    | Clause_ErrorAnnotation
    | Clause_Where => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a name.
pub fn NodeKind::is_name(self : NodeKind) -> Bool {
  match self {
    Name_Identifier | Name_Constructor => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a list.
pub fn NodeKind::is_list(self : NodeKind) -> Bool {
  match self {
    List_ArgumentList
    | List_ParameterList
    | List_FieldList
    | List_StatementList
    | List_TypeArgumentList
    | List_PatternList
    | List_ConstructorList => true
    _ => false
  }
}

///|
/// Returns whether this kind represents an interpolation segment.
pub fn NodeKind::is_interp_segment(self : NodeKind) -> Bool {
  match self {
    InterpSegment_InterpLiteralSegment | InterpSegment_InterpSourceSegment =>
      true
    _ => false
  }
}
