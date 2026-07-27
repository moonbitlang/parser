///|
fn category_count(kind : NodeKind) -> Int {
  let mut count = 0
  if kind.is_syntax_token() {
    count += 1
  }
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
  if kind.is_error_type() {
    count += 1
  }
  if kind.is_type_desc() {
    count += 1
  }
  if kind.is_argument() {
    count += 1
  }
  if kind.is_name() {
    count += 1
  }
  if kind.is_constant() {
    count += 1
  }
  if kind.is_operator() {
    count += 1
  }
  if kind.is_dot_dot_binder() {
    count += 1
  }
  if kind.is_constructor_extra_info() {
    count += 1
  }
  if kind.is_lex_literal() {
    count += 1
  }
  if kind.is_lex_top_pattern() {
    count += 1
  }
  if kind.is_lex_scan_case_pattern() {
    count += 1
  }
  if kind.is_scalar_value() {
    count += 1
  }
  if kind.is_stub() {
    count += 1
  }
  if kind.is_accessor() {
    count += 1
  }
  if kind.is_decl_body() {
    count += 1
  }
  if kind.is_syntax_flag() {
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
test "unclassified kinds do not match available categories" {
  for
    kind in [
      Impls,
      Expression,
      Missing,
      Error,
      Leaf(NodePayload::String("value")),
      Aggregate_Span,
      ArrayPattern_Pattern,
      LexPattern_Literal,
      RegexPattern_Literal,
      Parameter_Positional,
      Field_Declaration,
      Attribute_Simple,
      Comment_Ownline,
      List_ArgumentList,
    ] {
    @test.assert_eq(category_count(kind), 0)
  }
  @test.assert_eq(
    Leaf(NodePayload::String("value")) == Leaf(NodePayload::String("value")),
    true,
  )
}

///|
test "source syntax token kinds are classified exhaustively" {
  assert_category(
    [
      Syntax_Keyword,
      Syntax_Delimiter,
      Syntax_Separator,
      Syntax_Operator,
      Syntax_Identifier,
      Syntax_Label,
      Syntax_Accessor,
      Syntax_Literal,
      Syntax_Interpolation,
      Syntax_Attribute,
      Syntax_Comment,
      Syntax_Eof,
      Syntax_Unexpected,
    ],
    fn(kind) { kind.is_syntax_token() },
  )
}

///|
test "implementation node kinds are classified exhaustively" {
  assert_category(
    [
      Impl_Using,
      Impl_Function,
      Impl_Let,
      Impl_Const,
      Impl_TypeAbstract,
      Impl_TypeAlias,
      Impl_RecordStruct,
      Impl_TupleStruct,
      Impl_EnumVariant,
      Impl_ExtensibleEnum,
      Impl_ExtendEnum,
      Impl_Suberror,
      Impl_SuberrorEnum,
      Impl_EnumView,
      Impl_Trait,
      Impl_ImplMethod,
      Impl_ImplRelation,
      Impl_Test,
    ],
    fn(kind) { kind.is_impl() },
  )
}

///|
test "expression node kinds are classified exhaustively" {
  assert_category(
    [
      Expr_Ident,
      Expr_Constant,
      Expr_Interp,
      Expr_BytesInterp,
      Expr_Block,
      Expr_Apply,
      Expr_DotApply,
      Expr_Field,
      Expr_ArrayGet,
      Expr_ArrayGetSlice,
      Expr_Array,
      Expr_ArraySpread,
      Expr_ArraySet,
      Expr_ArrayAugmentedSet,
      Expr_ListComprehension,
      Expr_Unit,
      Expr_Group,
      Expr_Tuple,
      Expr_Sequence,
      Expr_Constraint,
      Expr_Record,
      Expr_RecordUpdate,
      Expr_Map,
      Expr_Infix,
      Expr_Pipe,
      Expr_RevPipe,
      Expr_Implies,
      Expr_Unary,
      Expr_Assign,
      Expr_Mutate,
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
      Expr_LetAnd,
      Expr_LetFn,
      Expr_Return,
      Expr_Raise,
      Expr_Break,
      Expr_Continue,
      Expr_Defer,
      Expr_Guard,
      Expr_Quantifier,
      Expr_Hole,
      Expr_Constr,
      Expr_MultilineString,
      Expr_MissingExpr,
      Expr_ProofAssert,
      Expr_ProofLet,
    ],
    fn(kind) { kind.is_expr() },
  )
}

///|
test "pattern node kinds are classified exhaustively" {
  assert_category(
    [
      Pattern_Var,
      Pattern_Any,
      Pattern_Constant,
      Pattern_Tuple,
      Pattern_Array,
      Pattern_Rest,
      Pattern_Record,
      Pattern_Map,
      Pattern_Constr,
      Pattern_SpecialConstr,
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
test "type node kinds are classified exhaustively" {
  assert_category(
    [
      Type_Name,
      Type_Tuple,
      Type_Arrow,
      Type_Application,
      Type_Option,
      Type_Object,
      Type_Any,
      Type_Group,
      Type_MissingType,
    ],
    fn(kind) { kind.is_type() },
  )
  assert_category(
    [ErrorType_Error, ErrorType_Default, ErrorType_Noraise, ErrorType_Maybe],
    fn(kind) { kind.is_error_type() },
  )
  assert_category(
    [
      TypeDesc_Abstract,
      TypeDesc_ExtensibleEnum,
      TypeDesc_ErrorNoPayload,
      TypeDesc_ErrorEnumPayload,
      TypeDesc_Variant,
      TypeDesc_ExtendEnum,
      TypeDesc_Record,
      TypeDesc_TupleStruct,
      TypeDesc_Alias,
    ],
    fn(kind) { kind.is_type_desc() },
  )
}

///|
test "argument node kinds are classified exhaustively" {
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
test "name accessor and declaration-body kinds are classified" {
  assert_category(
    [
      Name_Identifier,
      Name_Package,
      LongIdent_Ident,
      LongIdent_Dot,
      Name_Binder,
      Name_Label,
      Name_Type,
      Name_Constr,
      Name_ConstrId,
      Name_Var,
      Name_Constructor,
      Name_Field,
      Name_AliasTarget,
      Name_Test,
    ],
    fn(kind) { kind.is_name() },
  )
  assert_category([Accessor_Label, Accessor_Index, Accessor_Newtype], fn(kind) {
    kind.is_accessor()
  })
  assert_category(
    [
      DeclBody_Expr,
      DeclBody_Import,
      DeclBody_EmbeddedString,
      DeclBody_EmbeddedMultilineString,
      DeclBody_None,
    ],
    fn(kind) { kind.is_decl_body() },
  )
}

///|
test "constant node kinds are classified exhaustively" {
  assert_category(
    [
      Constant_Bool,
      Constant_Byte,
      Constant_Bytes,
      Constant_Char,
      Constant_Int,
      Constant_Int64,
      Constant_UInt,
      Constant_UInt64,
      Constant_Float,
      Constant_Double,
      Constant_String,
      Constant_Regex,
      Constant_BigInt,
    ],
    fn(kind) { kind.is_constant() },
  )
  assert_category(
    [
      Operator_Infix,
      Operator_Augmented,
      Operator_UnaryPlus,
      Operator_UnaryMinus,
      Operator_UnaryNot,
    ],
    fn(kind) { kind.is_operator() },
  )
  assert_category([ConstructorTag_Value, Language_Value], fn(kind) {
    kind.is_scalar_value()
  })
  assert_category([Stub_ModuleName, Stub_FunctionName, Stub_Code], fn(kind) {
    kind.is_stub()
  })
}

///|
test "semantic helper enum kinds are classified exhaustively" {
  assert_category(
    [
      DotDotBinder_None,
      DotDotBinder_Underscore,
      DotDotBinder_Binder,
      DotDotBinder_BinderAs,
    ],
    fn(kind) { kind.is_dot_dot_binder() },
  )
  assert_category(
    [
      ConstructorExtraInfo_None,
      ConstructorExtraInfo_Package,
      ConstructorExtraInfo_TypeName,
      ConstructorExtraInfo_TypeNameWithConstrPackage,
    ],
    fn(kind) { kind.is_constructor_extra_info() },
  )
  assert_category([LexLiteral_String, LexLiteral_Regex], fn(kind) {
    kind.is_lex_literal()
  })
  assert_category(
    [LexTopPattern_Pattern, LexTopPattern_Binder, LexTopPattern_Wildcard],
    fn(kind) { kind.is_lex_top_pattern() },
  )
  assert_category(
    [
      LexScanCasePattern_Pattern,
      LexScanCasePattern_Binder,
      LexScanCasePattern_Wildcard,
    ],
    fn(kind) { kind.is_lex_scan_case_pattern() },
  )
}

///|
test "syntax flag kinds are classified exhaustively" {
  assert_category(
    [
      Visibility_Default,
      Visibility_Pub,
      Visibility_Priv,
      RangeKind_Inclusive,
      RangeKind_Exclusive,
      HoleKind_Incomplete,
      HoleKind_Todo,
      FnKind_Lambda,
      FnKind_Arrow,
      Group_Brace,
      Group_Paren,
      TrailingMark_Comma,
      TrailingMark_Semi,
      TrailingMark_None,
      TryOperatorKind_Question,
      TryOperatorKind_Exclamation,
      QuantifierKind_Forall,
      QuantifierKind_Exists,
      UsingKind_Value,
      UsingKind_Type,
      UsingKind_Trait,
      Openness_Open,
      Openness_Closed,
      ArrayKind_Array,
      ArrayKind_Iter,
      Flag_Async,
      Flag_Declare,
      Flag_Mutable,
      Flag_Pun,
      Flag_ReturnSelf,
      Flag_Conditional,
      Flag_HasTry,
      Flag_MatchAbsent,
      Flag_Faked,
      Flag_Augmented,
      Flag_Object,
    ],
    fn(kind) { kind.is_syntax_flag() },
  )
}

///|
test "interpolation segment kinds are classified exhaustively" {
  assert_category([InterpSegment_Literal, InterpSegment_Source], fn(kind) {
    kind.is_interp_segment()
  })
}
