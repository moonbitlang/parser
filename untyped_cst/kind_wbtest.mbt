///|
fn category_count(kind : NodeKind) -> Int {
  let mut count = 0
  if kind.is_impl() {
    count += 1
  }
  if kind.is_expr() {
    count += 1
  }
  if kind.is_pattern() {
    count += 1
  }
  if kind.is_type() {
    count += 1
  }
  if kind.is_lex_pattern() {
    count += 1
  }
  if kind.is_regex_pattern() {
    count += 1
  }
  if kind.is_case() {
    count += 1
  }
  if kind.is_argument() {
    count += 1
  }
  if kind.is_stmt() {
    count += 1
  }
  if kind.is_decl() {
    count += 1
  }
  if kind.is_clause() {
    count += 1
  }
  if kind.is_name() {
    count += 1
  }
  if kind.is_list() {
    count += 1
  }
  if kind.is_interp_segment() {
    count += 1
  }
  count
}

///|
fn assert_category(
  kinds : Array[NodeKind],
  pred : (NodeKind) -> Bool,
) -> Unit raise {
  for kind in kinds {
    @test.assert_eq(pred(kind), true)
    @test.assert_eq(category_count(kind), 1)
  }
}

///|
test "top-level node kinds have no detailed category" {
  let kinds : Array[NodeKind] = [
    SourceFile,
    ExpressionFragment,
    Token(@tokens.EOF),
    Missing(@tokens.TokenKind::TK_EOF),
    Error,
  ]
  for kind in kinds {
    @test.assert_eq(category_count(kind), 0)
  }
}

///|
test "impl node kinds" {
  assert_category([Impl_Using], fn(kind) { kind.is_impl() })
}

///|
test "expr node kinds" {
  assert_category(
    [
      Expr_Identifier,
      Expr_Literal,
      Expr_Block,
      Expr_Apply,
      Expr_DotApply,
      Expr_Field,
      Expr_ArrayGet,
      Expr_ArrayGetSlice,
      Expr_Array,
      Expr_ListComprehension,
      Expr_Tuple,
      Expr_Constraint,
      Expr_Record,
      Expr_Map,
      Expr_Infix,
      Expr_Prefix,
      Expr_Range,
      Expr_Assign,
      Expr_AugmentedAssign,
      Expr_If,
      Expr_Match,
      Expr_Try,
      Expr_TryOperator,
      Expr_For,
      Expr_ForEach,
      Expr_While,
      Expr_Function,
      Expr_Method,
      Expr_As,
      Expr_Is,
      Expr_LexMatch,
      Expr_LexScan,
      Expr_RegexMatch,
      Expr_TemplateWriting,
      Expr_Let,
      Expr_LetMut,
      Expr_LetRec,
      Expr_LetFn,
      Expr_Return,
      Expr_Raise,
      Expr_Break,
      Expr_Continue,
      Expr_Defer,
      Expr_Guard,
      Expr_Quantifier,
      Expr_Hole,
      Expr_Group,
      Expr_Constructor,
      Expr_MultilineString,
      Expr_MissingExpr,
    ],
    fn(kind) { kind.is_expr() },
  )
}

///|
test "pattern node kinds" {
  assert_category(
    [
      Pattern_Binder,
      Pattern_Underscore,
      Pattern_Literal,
      Pattern_Tuple,
      Pattern_Array,
      Pattern_Rest,
      Pattern_Record,
      Pattern_Map,
      Pattern_Constructor,
      Pattern_SpecialConstructor,
      Pattern_Or,
      Pattern_Alias,
      Pattern_Range,
      Pattern_Constraint,
      Pattern_MissingPattern,
    ],
    fn(kind) { kind.is_pattern() },
  )
}

///|
test "type node kinds" {
  assert_category(
    [
      Type_Name,
      Type_Tuple,
      Type_Function,
      Type_Application,
      Type_Option,
      Type_Object,
      Type_Hole,
      Type_Group,
      Type_MissingType,
    ],
    fn(kind) { kind.is_type() },
  )
}

///|
test "lex pattern node kinds" {
  assert_category(
    [
      LexPattern_Literal,
      LexPattern_RegexInterp,
      LexPattern_Sequence,
      LexPattern_Alias,
      LexPattern_ConstantRef,
    ],
    fn(kind) { kind.is_lex_pattern() },
  )
}

///|
test "regex pattern node kinds" {
  assert_category(
    [
      RegexPattern_Placeholder,
      RegexPattern_Literal,
      RegexPattern_Reference,
      RegexPattern_Sequence,
      RegexPattern_Alternation,
      RegexPattern_Alias,
    ],
    fn(kind) { kind.is_regex_pattern() },
  )
}

///|
test "case node kinds" {
  assert_category([Case_MatchCase, Case_LexCase], fn(kind) { kind.is_case() })
}

///|
test "argument node kinds" {
  assert_category(
    [
      Argument_Positional,
      Argument_Labelled,
      Argument_LabelledPun,
      Argument_LabelledOption,
      Argument_LabelledOptionPun,
    ],
    fn(kind) { kind.is_argument() },
  )
}

///|
test "statement node kinds" {
  assert_category([Stmt_ProofAssert, Stmt_ProofLet], fn(kind) { kind.is_stmt() })
}

///|
test "declaration node kinds" {
  assert_category(
    [
      Decl_BalancedDecl,
      Decl_Function,
      Decl_Let,
      Decl_Const,
      Decl_Type,
      Decl_Struct,
      Decl_Enum,
      Decl_EnumView,
      Decl_Trait,
      Decl_Impl,
      Decl_Test,
    ],
    fn(kind) { kind.is_decl() },
  )
}

///|
test "clause node kinds" {
  assert_category(
    [
      Clause_Visibility,
      Clause_Parameter,
      Clause_ReturnType,
      Clause_ErrorAnnotation,
      Clause_Where,
    ],
    fn(kind) { kind.is_clause() },
  )
}

///|
test "name node kinds" {
  assert_category([Name_Identifier, Name_Constructor], fn(kind) {
    kind.is_name()
  })
}

///|
test "list node kinds" {
  assert_category(
    [
      List_ArgumentList,
      List_ParameterList,
      List_FieldList,
      List_StatementList,
      List_TypeArgumentList,
      List_PatternList,
      List_ConstructorList,
    ],
    fn(kind) { kind.is_list() },
  )
}

///|
test "interpolation segment node kinds" {
  assert_category(
    [InterpSegment_InterpLiteralSegment, InterpSegment_InterpSourceSegment],
    fn(kind) { kind.is_interp_segment() },
  )
}
