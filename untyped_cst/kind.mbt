///|
/// Identifies the source-faithful CST node kinds and documents how each kind
/// contributes to the typed definitions in `@syntax`.
///
/// Children whose labels end in `_loc` are non-owning location metadata.
/// Token-backed location metadata reuses the corresponding `Syntax_*` kind,
/// while locations synthesized from multiple syntax elements use
/// `Aggregate_Span`.
pub enum NodeKind {
  /// Root node lowered to `@syntax.Impls`.
  Impls
  /// Root wrapper whose `expr` child is lowered to `@syntax.Expr`.
  Expression
  /// CST recovery placeholder with no direct `@syntax` AST node.
  Missing
  /// Invalid or recovered CST region with no direct `@syntax` AST node.
  Error
  /// Scalar payload child used by CST metadata; it has no direct AST node.
  Leaf(NodePayload)
  /// Keyword token retained for source fidelity; AST lowering uses its enclosing node.
  Syntax_Keyword
  /// Parenthesis, bracket, or brace token; delimiters are represented by AST locations.
  Syntax_Delimiter
  /// Separator token such as `,`, `:`, `::`, `;`, or an arrow; it has no AST node.
  Syntax_Separator
  /// Operator token retained verbatim; semantic operators use `Operator_*` children.
  Syntax_Operator
  /// Identifier token retained verbatim; semantic names use the `Name_*` nodes.
  Syntax_Identifier
  /// Post-label token retained verbatim; lowering incorporates it into labels or binders.
  Syntax_Label
  /// Dot accessor token retained verbatim; semantic accessors use `Accessor_*` nodes.
  Syntax_Accessor
  /// Literal token retained verbatim; semantic values use `Constant_*` nodes.
  Syntax_Literal
  /// Interpolated-literal token retained verbatim; segments lower to `@syntax.InterpElem`.
  Syntax_Interpolation
  /// Attribute token retained verbatim; its metadata builds an `@attribute.Attribute`.
  Syntax_Attribute
  /// Comment token retained verbatim; comments have no direct `@syntax` expression node.
  Syntax_Comment
  /// End-of-file token retained in the CST; it has no direct AST node.
  Syntax_Eof
  /// Unexpected token preserved during recovery; it has no direct AST node.
  Syntax_Unexpected

  // Top-level implementation items.
  /// Corresponds to `@syntax.Impl::TopImport`.
  Impl_Import
  /// Corresponds to `@syntax.Impl::TopExtend`.
  Impl_Extend
  /// Corresponds to `@syntax.Impl::TopUsing`.
  Impl_Using
  /// Corresponds to `@syntax.Impl::TopFuncDef`.
  Impl_Function
  /// Corresponds to `@syntax.Impl::TopLetDef` with `is_constant=false`.
  Impl_Let
  /// Corresponds to `@syntax.Impl::TopLetDef` with `is_constant=true`.
  Impl_Const
  /// Corresponds to `TopTypeDef` with `@syntax.TypeDesc::Abstract`.
  Impl_TypeAbstract
  /// Corresponds to `TopTypeDef` with `@syntax.TypeDesc::Alias`.
  Impl_TypeAlias
  /// Corresponds to `TopTypeDef` with `@syntax.TypeDesc::Record`.
  Impl_RecordStruct
  /// Corresponds to `TopTypeDef` with `@syntax.TypeDesc::TupleStruct`.
  Impl_TupleStruct
  /// Corresponds to `TopTypeDef` with `@syntax.TypeDesc::Variant`.
  Impl_EnumVariant
  /// Corresponds to `TopTypeDef` with `@syntax.TypeDesc::ExtensibleEnum`.
  Impl_ExtensibleEnum
  /// Corresponds to `TopTypeDef` with `@syntax.TypeDesc::ExtendEnum`.
  Impl_ExtendEnum
  /// Corresponds to `TopTypeDef` with `TypeDesc::Error(ExceptionDecl::NoPayload)`.
  Impl_Suberror
  /// Corresponds to `TopTypeDef` with `TypeDesc::Error(ExceptionDecl::EnumPayload)`.
  Impl_SuberrorEnum
  /// Corresponds to `@syntax.Impl::TopView`.
  Impl_EnumView
  /// Corresponds to `@syntax.Impl::TopTrait`.
  Impl_Trait
  /// Corresponds to `@syntax.Impl::TopImpl`.
  Impl_ImplMethod
  /// Corresponds to `@syntax.Impl::TopImplRelation`.
  Impl_ImplRelation
  /// Corresponds to `@syntax.Impl::TopTest`.
  Impl_Test

  // Expressions.
  /// Corresponds to `@syntax.Expr::Ident`.
  Expr_Ident
  /// Corresponds to `@syntax.Expr::Constant`.
  Expr_Constant
  /// Corresponds to `@syntax.Expr::Interp`.
  Expr_Interp
  /// Corresponds to `@syntax.Expr::BytesInterp`.
  Expr_BytesInterp
  /// Usually lowers to `Expr::Group` with `Group::Brace`; function bodies unwrap it.
  Expr_Block
  /// Corresponds to `@syntax.Expr::Apply`.
  Expr_Apply
  /// Corresponds to `@syntax.Expr::DotApply`.
  Expr_DotApply
  /// Corresponds to `@syntax.Expr::Field`.
  Expr_Field
  /// Corresponds to `@syntax.Expr::ArrayGet`.
  Expr_ArrayGet
  /// Corresponds to `@syntax.Expr::ArrayGetSlice`.
  Expr_ArrayGetSlice
  /// Corresponds to `@syntax.Expr::Array`.
  Expr_Array
  /// Corresponds to `@syntax.Expr::ArraySpread`.
  Expr_ArraySpread
  /// Corresponds to `@syntax.Expr::ArraySet`.
  Expr_ArraySet
  /// Corresponds to `@syntax.Expr::ArrayAugmentedSet`.
  Expr_ArrayAugmentedSet
  /// Corresponds to `@syntax.Expr::ListComprehension`.
  Expr_ListComprehension
  /// Corresponds to `@syntax.Expr::Unit`.
  Expr_Unit
  /// Corresponds to `@syntax.Expr::Group` with `@syntax.Group::Paren`.
  Expr_Group
  /// Corresponds to `@syntax.Expr::Tuple`.
  Expr_Tuple
  /// Corresponds to `@syntax.Expr::Sequence`.
  Expr_Sequence
  /// Corresponds to `@syntax.Expr::Constraint`.
  Expr_Constraint
  /// Corresponds to `@syntax.Expr::Record`.
  Expr_Record
  /// Corresponds to `@syntax.Expr::RecordUpdate`.
  Expr_RecordUpdate
  /// Corresponds to `@syntax.Expr::Map`.
  Expr_Map
  /// Corresponds to `@syntax.Expr::Infix`.
  Expr_Infix
  /// Corresponds to `@syntax.Expr::Pipe`.
  Expr_Pipe
  /// Corresponds to `@syntax.Expr::RevPipe`.
  Expr_RevPipe
  /// Corresponds to `@syntax.Expr::Implies`.
  Expr_Implies
  /// Corresponds to `@syntax.Expr::Unary`.
  Expr_Unary
  /// Corresponds to `@syntax.Expr::Assign`.
  Expr_Assign
  /// Corresponds to `@syntax.Expr::Mutate`.
  Expr_Mutate
  /// Corresponds to `@syntax.Expr::If`.
  Expr_If
  /// Corresponds to `@syntax.Expr::Match`.
  Expr_Match
  /// Corresponds to `@syntax.Expr::Try`.
  Expr_Try
  /// Corresponds to `@syntax.Expr::TryOperator`.
  Expr_TryOperator
  /// Corresponds to `@syntax.Expr::For`.
  Expr_For
  /// Corresponds to `@syntax.Expr::ForEach`.
  Expr_ForEach
  /// Corresponds to `@syntax.Expr::While`.
  Expr_While
  /// Corresponds to `@syntax.Expr::Function`.
  Expr_Function
  /// Corresponds to `@syntax.Expr::Method`.
  Expr_Method
  /// Corresponds to `@syntax.Expr::As`.
  Expr_As
  /// Corresponds to `@syntax.Expr::Is`.
  Expr_Is
  /// Corresponds to `@syntax.Expr::LexMatch`.
  Expr_LexMatch
  /// Corresponds to `@syntax.Expr::LexScan`.
  Expr_LexScan
  /// Corresponds to `@syntax.Expr::RegexMatch`.
  Expr_RegexMatch
  /// Corresponds to `@syntax.Expr::TemplateWriting`.
  Expr_TemplateWriting
  /// Corresponds to `@syntax.Expr::Let`; following statements become its `body`.
  Expr_Let
  /// Corresponds to `@syntax.Expr::LetMut`; following statements become its `body`.
  Expr_LetMut
  /// Corresponds to `@syntax.Expr::LetAnd`.
  Expr_LetAnd
  /// Corresponds to `@syntax.Expr::LetFn`.
  Expr_LetFn
  /// Corresponds to `@syntax.Expr::Return`.
  Expr_Return
  /// Corresponds to `@syntax.Expr::Raise`.
  Expr_Raise
  /// Corresponds to `@syntax.Expr::Break`.
  Expr_Break
  /// Corresponds to `@syntax.Expr::Continue`.
  Expr_Continue
  /// Corresponds to `@syntax.Expr::LabelledBlock`.
  Expr_LabelledBlock
  /// Corresponds to `@syntax.Expr::Defer`; following statements become its `body`.
  Expr_Defer
  /// Corresponds to `@syntax.Expr::Guard`; following statements become its `body`.
  Expr_Guard
  /// Corresponds to `@syntax.Expr::Quantifier`.
  Expr_Quantifier
  /// Corresponds to `Expr::Hole` with `Hole::Incomplete` or `Hole::Todo`.
  Expr_Hole
  /// Corresponds to `@syntax.Expr::Constr`.
  Expr_Constr
  /// Corresponds to `@syntax.Expr::MultilineString`.
  Expr_MultilineString
  /// Missing-expression recovery node lowered to `Expr::Hole(Hole::Synthesized)`.
  Expr_MissingExpr
  /// Corresponds to `@syntax.Expr::ProofAssert`.
  Expr_ProofAssert
  /// Corresponds to `@syntax.Expr::ProofLet`.
  Expr_ProofLet

  // Patterns and pattern helper values.
  /// Corresponds to `@syntax.Pattern::Var`.
  Pattern_Var
  /// Corresponds to `@syntax.Pattern::Any`.
  Pattern_Any
  /// Corresponds to `@syntax.Pattern::Constant`.
  Pattern_Constant
  /// Corresponds to `@syntax.Pattern::Tuple`.
  Pattern_Tuple
  /// Corresponds to `@syntax.Pattern::Array`.
  Pattern_Array
  /// Array-rest syntax; it contributes the split and `DotDotBinder` to `ArrayPatterns::Open`.
  Pattern_Rest
  /// Corresponds to `@syntax.Pattern::Record`.
  Pattern_Record
  /// Corresponds to `@syntax.Pattern::Map`.
  Pattern_Map
  /// Corresponds to `@syntax.Pattern::Constr`.
  Pattern_Constr
  /// Corresponds to `@syntax.Pattern::SpecialConstr`.
  Pattern_SpecialConstr
  /// Corresponds to `@syntax.Pattern::Or`.
  Pattern_Or
  /// Corresponds to `@syntax.Pattern::Alias`.
  Pattern_Alias
  /// Corresponds to `@syntax.Pattern::Range`.
  Pattern_Range
  /// Corresponds to `@syntax.Pattern::Constraint`.
  Pattern_Constraint
  /// Corresponds to `@syntax.Pattern::With`.
  Pattern_With
  /// Missing-pattern recovery node lowered to `@syntax.Pattern::Any`.
  Pattern_MissingPattern
  /// Parenthesized-single-pattern marker; grouping is removed during lowering.
  Pattern_Group
  /// Corresponds to `@syntax.ArrayPattern::Pattern`.
  ArrayPattern_Pattern
  /// Corresponds to `@syntax.ArrayPattern::StringSpread`.
  ArrayPattern_StringSpread
  /// Corresponds to `@syntax.ArrayPattern::BytesSpread`.
  ArrayPattern_BytesSpread
  /// Corresponds to `@syntax.ArrayPattern::ConstSpread`.
  ArrayPattern_ConstSpread
  /// Corresponds to `@syntax.ArrayPatterns::Closed`.
  ArrayPatterns_Closed
  /// Corresponds to `@syntax.ArrayPatterns::Open`.
  ArrayPatterns_Open

  // Types and non-recursive type helper values.
  /// Corresponds to `@syntax.Type::Name`.
  Type_Name
  /// Corresponds to `@syntax.Type::Tuple`.
  Type_Tuple
  /// Corresponds to `@syntax.Type::Arrow`.
  Type_Arrow
  /// Type-application CST shape lowered to `Type::Name` with non-empty `tys`.
  Type_Application
  /// Corresponds to `@syntax.Type::Option`.
  Type_Option
  /// Corresponds to `@syntax.Type::Object` when it contains a named object type.
  Type_Object
  /// Corresponds to `@syntax.Type::Any`.
  Type_Any
  /// Parenthesized-single-type CST wrapper removed during lowering.
  Type_Group
  /// Missing-type recovery node lowered to `@syntax.Type::Any`.
  Type_MissingType
  /// Corresponds to `@syntax.ErrorType::ErrorType`.
  ErrorType_Error
  /// Corresponds to `@syntax.ErrorType::DefaultErrorType`.
  ErrorType_Default
  /// Corresponds to `@syntax.ErrorType::Noraise`.
  ErrorType_Noraise
  /// Corresponds to `@syntax.ErrorType::MaybeError`.
  ErrorType_Maybe
  /// Corresponds to `@syntax.TypeDesc::Abstract`.
  TypeDesc_Abstract
  /// Corresponds to `@syntax.TypeDesc::ExtensibleEnum`.
  TypeDesc_ExtensibleEnum
  /// Corresponds to `TypeDesc::Error(ExceptionDecl::NoPayload)`.
  TypeDesc_ErrorNoPayload
  /// Corresponds to `TypeDesc::Error(ExceptionDecl::EnumPayload)`.
  TypeDesc_ErrorEnumPayload
  /// Corresponds to `@syntax.TypeDesc::Variant`.
  TypeDesc_Variant
  /// Corresponds to `@syntax.TypeDesc::ExtendEnum`.
  TypeDesc_ExtendEnum
  /// Corresponds to `@syntax.TypeDesc::Record`.
  TypeDesc_Record
  /// Corresponds to `@syntax.TypeDesc::TupleStruct`.
  TypeDesc_TupleStruct
  /// Corresponds to `@syntax.TypeDesc::Alias`.
  TypeDesc_Alias

  // Lexing and regex patterns.
  /// Literal lex pattern lowered to `@syntax.LexPattern::Regex`.
  LexPattern_Literal
  /// Corresponds to `@syntax.LexPattern::RegexInterp`.
  LexPattern_RegexInterp
  /// Corresponds to `@syntax.LexPattern::Sequence`.
  LexPattern_Sequence
  /// Corresponds to `@syntax.LexPattern::Alias`.
  LexPattern_Alias
  /// Corresponds to `@syntax.LexPattern::ConstantRef`.
  LexPattern_ConstantRef
  /// Recovery wrapper removed before producing an `@syntax.RegexPattern`.
  RegexPattern_Placeholder
  /// Corresponds to `@syntax.RegexPattern::Literal`.
  RegexPattern_Literal
  /// Corresponds to `@syntax.RegexPattern::Reference`.
  RegexPattern_Reference
  /// Corresponds to `@syntax.RegexPattern::Sequence`.
  RegexPattern_Sequence
  /// Corresponds to `@syntax.RegexPattern::Alternation`.
  RegexPattern_Alternation
  /// Corresponds to `@syntax.RegexPattern::Alias`.
  RegexPattern_Alias

  // Cases, arguments, parameters, fields, and bindings.
  /// Corresponds to `@syntax.Case`.
  Case_MatchCase
  /// Corresponds to `@syntax.LexCase`.
  Case_LexCase
  /// Corresponds to `@syntax.LexScanCase`.
  Case_LexScanCase
  /// Corresponds to `@syntax.ArgumentKind::Positional`.
  Argument_Positional
  /// Corresponds to `@syntax.ArgumentKind::Labelled`.
  Argument_Labelled
  /// Corresponds to `@syntax.ArgumentKind::LabelledPun`.
  Argument_LabelledPun
  /// Corresponds to `@syntax.ArgumentKind::LabelledOption`.
  Argument_LabelledOption
  /// Corresponds to `@syntax.ArgumentKind::LabelledOptionPun`.
  Argument_LabelledOptionPun
  /// Corresponds to `@syntax.SpreadableElem::Regular`.
  ArrayElement_Regular
  /// Corresponds to `@syntax.SpreadableElem::Spread`.
  ArrayElement_Spread
  /// Corresponds to `@syntax.Parameter::DiscardPositional`.
  Parameter_DiscardPositional
  /// Corresponds to `@syntax.Parameter::Positional`.
  Parameter_Positional
  /// Corresponds to `@syntax.Parameter::Labelled`.
  Parameter_Labelled
  /// Corresponds to `@syntax.Parameter::Optional`.
  Parameter_Optional
  /// Corresponds to `@syntax.Parameter::QuestionOptional`.
  Parameter_QuestionOptional
  /// Corresponds to `@syntax.TypeDeclBinder`.
  Parameter_TypeDecl
  /// Corresponds to `@syntax.TypeVarBinder`.
  Parameter_TypeVar
  /// Corresponds to `@syntax.ConstrParam` with `label=None`.
  Parameter_ConstructorPositional
  /// Corresponds to `@syntax.ConstrParam` with a labelled parameter.
  Parameter_ConstructorLabelled
  /// Corresponds to `@syntax.FieldDecl`.
  Field_Declaration
  /// Corresponds to `@syntax.FieldDef`.
  Field_Definition
  /// Corresponds to `@syntax.FieldPat`.
  Field_Pattern
  /// Corresponds to `@syntax.MapExprElem`.
  Field_MapExpression
  /// Corresponds to `@syntax.MapPatElem`.
  Field_MapPattern
  /// Corresponds to a `FieldDef` stored in `@syntax.WhereClause::fields`.
  Field_Where
  /// Record-update spread source stored in `@syntax.Expr::RecordUpdate::record`.
  Field_RecordSpread
  /// Let-rec binding lowered to a `(Binder, Type?, Func)` entry in `Expr::LetAnd`.
  Binding_LetRec
  /// Loop binding lowered to a `(Binder, Expr)` entry in `Expr::For`.
  Binding_For
  /// List-comprehension state binding lowered to a `(Binder, Expr)` entry.
  Binding_ForEach
  /// Regex capture binding lowered to `(Label, Binder?)` in `Expr::RegexMatch`.
  Binding_Regex
  /// Corresponds to `@syntax.TraitMethodDecl`.
  TraitMethod_Declaration

  // Names and accessors corresponding to AST helper types.
  /// Scalar identifier component used to build AST names; no standalone AST type.
  Name_Identifier
  /// Scalar package-name component used by `LongIdent::Dot` and constructor metadata.
  Name_Package
  /// Corresponds to `@syntax.LongIdent::Ident`.
  LongIdent_Ident
  /// Corresponds to `@syntax.LongIdent::Dot`.
  LongIdent_Dot
  /// Corresponds to `@syntax.Binder`.
  Name_Binder
  /// Corresponds to `@syntax.Label`.
  Name_Label
  /// Corresponds to `@syntax.TypeName`.
  Name_Type
  /// Corresponds to `@syntax.ConstrName`.
  Name_Constr
  /// Corresponds to `@syntax.ConstrId`.
  Name_ConstrId
  /// Corresponds to `@syntax.Var`.
  Name_Var
  /// Corresponds to `@syntax.Constructor`.
  Name_Constructor
  /// Corresponds to `@syntax.FieldName`.
  Name_Field
  /// Corresponds to `@syntax.AliasTarget`.
  Name_AliasTarget
  /// Corresponds to `@syntax.Accessor::Label`.
  Accessor_Label
  /// Corresponds to `@syntax.Accessor::Index`.
  Accessor_Index
  /// Corresponds to `@syntax.Accessor::Newtype`.
  Accessor_Newtype
  /// Corresponds to `@syntax.TestName`.
  Name_Test

  // Constants corresponding to syntax.Constant.
  /// Corresponds to `@syntax.Constant::Bool`.
  Constant_Bool
  /// Corresponds to `@syntax.Constant::Byte`.
  Constant_Byte
  /// Corresponds to `@syntax.Constant::Bytes`.
  Constant_Bytes
  /// Corresponds to `@syntax.Constant::Char`.
  Constant_Char
  /// Corresponds to `@syntax.Constant::Int`.
  Constant_Int
  /// Corresponds to `@syntax.Constant::Int64`.
  Constant_Int64
  /// Corresponds to `@syntax.Constant::UInt`.
  Constant_UInt
  /// Corresponds to `@syntax.Constant::UInt64`.
  Constant_UInt64
  /// Corresponds to `@syntax.Constant::Float`.
  Constant_Float
  /// Corresponds to `@syntax.Constant::Double`.
  Constant_Double
  /// Corresponds to `@syntax.Constant::String`.
  Constant_String
  /// Corresponds to `@syntax.Constant::Regex`.
  Constant_Regex
  /// Corresponds to `@syntax.Constant::BigInt`.
  Constant_BigInt

  // Operators whose spelling is needed by lowering.
  /// Infix spelling lowered to the `Var` operator of `@syntax.Expr::Infix`.
  Operator_Infix
  /// Augmented-assignment spelling stored in `Assign`, `Mutate`, or `ArrayAugmentedSet`.
  Operator_Augmented
  /// Unary `+` spelling used to build or fold `@syntax.Expr::Unary`.
  Operator_UnaryPlus
  /// Unary `-` spelling used to build or fold `@syntax.Expr::Unary`.
  Operator_UnaryMinus
  /// Unary `!` spelling used to build `@syntax.Expr::Unary`.
  Operator_UnaryNot

  // Non-recursive syntax helper values.
  /// Corresponds to `@syntax.ListComprehensionKind::For`.
  ListComprehensionKind_For
  /// Corresponds to `@syntax.ListComprehensionKind::Foreach`.
  ListComprehensionKind_ForEach
  /// Corresponds to `@syntax.DotDotBinder::NoBinder`.
  DotDotBinder_None
  /// Corresponds to `@syntax.DotDotBinder::Underscore`.
  DotDotBinder_Underscore
  /// Corresponds to `@syntax.DotDotBinder::Binder`.
  DotDotBinder_Binder
  /// Corresponds to `@syntax.DotDotBinder::BinderAs`.
  DotDotBinder_BinderAs
  /// Corresponds to `@syntax.ConstructorExtraInfo::NoExtraInfo`.
  ConstructorExtraInfo_None
  /// Corresponds to `@syntax.ConstructorExtraInfo::Package`.
  ConstructorExtraInfo_Package
  /// Corresponds to `@syntax.ConstructorExtraInfo::TypeName`.
  ConstructorExtraInfo_TypeName
  /// Corresponds to `@syntax.ConstructorExtraInfo::TypeNameWithConstrPackage`.
  ConstructorExtraInfo_TypeNameWithConstrPackage
  /// Corresponds to `@syntax.MultilineStringElem::String`.
  MultilineStringElement_String
  /// Corresponds to `@syntax.MultilineStringElem::Interp`.
  MultilineStringElement_Interp
  /// String lex-pattern payload lowered to `LexPattern::Regex` with string offset.
  LexLiteral_String
  /// Regex lex-pattern payload lowered to `LexPattern::Regex` with regex offset.
  LexLiteral_Regex
  /// Corresponds to `@syntax.LexTopPattern::Pattern`.
  LexTopPattern_Pattern
  /// Corresponds to `@syntax.LexTopPattern::Binder`.
  LexTopPattern_Binder
  /// Corresponds to `@syntax.LexTopPattern::Wildcard`.
  LexTopPattern_Wildcard
  /// Corresponds to `@syntax.LexScanCasePattern::Pattern`.
  LexScanCasePattern_Pattern
  /// Corresponds to `@syntax.LexScanCasePattern::Binder`.
  LexScanCasePattern_Binder
  /// Corresponds to `@syntax.LexScanCasePattern::Wildcard`.
  LexScanCasePattern_Wildcard
  /// Scalar constructor tag stored in `@syntax.ConstrDecl::tag`.
  ConstructorTag_Value
  /// Scalar foreign-language name stored in `@syntax.FuncStubs`.
  Language_Value
  /// Scalar module name stored in `@syntax.FuncStubs::Import`.
  Stub_ModuleName
  /// Scalar function name stored in `@syntax.FuncStubs::Import`.
  Stub_FunctionName
  /// Scalar embedded code stored in an `@syntax.EmbeddedCode` variant.
  Stub_Code
  /// Non-owning location metadata synthesized from one or more syntax elements.
  ///
  /// Its kind remains `Aggregate_Span` when recovery collapses the range to a
  /// single token or a zero-width point.
  Aggregate_Span

  // Declaration bodies.
  /// Corresponds to `@syntax.DeclBody::DeclBody`.
  DeclBody_Expr
  /// Corresponds to `DeclBody::DeclStubs(FuncStubs::Import)`.
  DeclBody_Import
  /// Corresponds to `DeclStubs(FuncStubs::Embedded(EmbeddedCode::CodeString))`.
  DeclBody_EmbeddedString
  /// Corresponds to `DeclStubs` with `EmbeddedCode::CodeMultilineString`.
  DeclBody_EmbeddedMultilineString
  /// Corresponds to `@syntax.DeclBody::DeclNone`.
  DeclBody_None

  // Syntax-level enum and flag values.
  /// Corresponds to `@syntax.Visibility::Default`.
  Visibility_Default
  /// Corresponds to `@syntax.Visibility::Pub`.
  Visibility_Pub
  /// Corresponds to `@syntax.Visibility::Priv`.
  Visibility_Priv
  /// Corresponds to `@syntax.RangeKind::Inclusive`.
  RangeKind_Inclusive
  /// Corresponds to `@syntax.RangeKind::Exclusive`.
  RangeKind_Exclusive
  /// Corresponds to `@syntax.Hole::Incomplete`.
  HoleKind_Incomplete
  /// Corresponds to `@syntax.Hole::Todo`.
  HoleKind_Todo
  /// Corresponds to `@syntax.FnKind::Lambda`.
  FnKind_Lambda
  /// Corresponds to `@syntax.FnKind::Arrow`.
  FnKind_Arrow
  /// Corresponds to `@syntax.Group::Brace`.
  Group_Brace
  /// Corresponds to `@syntax.Group::Paren`.
  Group_Paren
  /// Corresponds to `@syntax.TrailingMark::Comma`.
  TrailingMark_Comma
  /// Corresponds to `@syntax.TrailingMark::Semi`.
  TrailingMark_Semi
  /// Corresponds to `@syntax.TrailingMark::None`.
  TrailingMark_None
  /// Corresponds to `@syntax.TryOperatorKind::Exclamation`.
  TryOperatorKind_Exclamation
  /// Corresponds to `@syntax.PredicateQuantifierKind::Forall`.
  QuantifierKind_Forall
  /// Corresponds to `@syntax.PredicateQuantifierKind::Exists`.
  QuantifierKind_Exists
  /// Corresponds to `@syntax.UsingKind::Value`.
  UsingKind_Value
  /// Corresponds to `@syntax.UsingKind::Type`.
  UsingKind_Type
  /// Corresponds to `@syntax.UsingKind::Trait`.
  UsingKind_Trait
  /// Open-pattern marker used for `Pattern::Constr::is_open` or `is_closed=false`.
  Openness_Open
  /// Closed-pattern marker used for `Pattern::Constr::is_open=false` or `is_closed=true`.
  Openness_Closed
  /// Array-literal marker represented by `is_iter=false` on array AST expressions.
  ArrayKind_Array
  /// Iterator-literal marker represented by `is_iter=true` on array AST expressions.
  ArrayKind_Iter
  /// Boolean marker controlling AST `is_async` locations; it has no standalone AST node.
  Flag_Async
  /// Boolean marker for declaration `is_declare` fields; no standalone AST node.
  Flag_Declare
  /// Boolean marker for `ConstrParam::mut_` or `FieldDecl::mut_`.
  Flag_Mutable
  /// Boolean marker for `FieldDef::is_pun` or `FieldPat::is_pun`.
  Flag_Pun
  /// Boolean marker for `@syntax.Expr::DotApply::return_self`.
  Flag_ReturnSelf
  /// Boolean marker for `@syntax.Expr::TemplateWriting::is_conditional`.
  Flag_Conditional
  /// Boolean marker for `@syntax.Expr::Try::has_try`.
  Flag_HasTry
  /// Boolean marker for `@syntax.MapPatElem::match_absent`.
  Flag_MatchAbsent
  /// Boolean marker for parser-inserted `@syntax.Expr::Unit::faked`.
  Flag_Faked
  /// Boolean marker selecting augmented assignment AST fields or variants.
  Flag_Augmented
  /// Boolean marker for `@syntax.TypeName::is_object`.
  Flag_Object
  /// Boolean marker for `guard!`.
  Flag_Exclamation

  // Attributes, comments, and interpolation token metadata.
  /// Unqualified attribute metadata used to build an `@attribute.Attribute`.
  Attribute_Simple
  /// Qualified attribute metadata used to build an `@attribute.Attribute`.
  Attribute_Qualified
  /// Scalar attribute name; it has no standalone `@syntax` AST node.
  Attribute_Name
  /// Scalar attribute qualifier; it has no standalone `@syntax` AST node.
  Attribute_Qualifier
  /// Raw attribute payload used by `@attribute.Attribute::new`.
  Attribute_RawPayload
  /// Inline-trailing comment metadata retained only by the CST.
  Comment_InlineTrailing
  /// Own-line comment metadata retained only by the CST.
  Comment_Ownline
  /// Scalar comment text mirrored by docstring extraction; it has no direct AST node.
  Comment_Content
  /// Boolean own-line-comment spacing metadata with no direct AST field.
  Comment_LeadingBlankLine
  /// Boolean own-line-comment spacing metadata with no direct AST field.
  Comment_TrailingBlankLine
  /// Boolean metadata recording docstring consumption; no direct AST field.
  Comment_ConsumedByDocstring
  /// Corresponds to `@syntax.InterpElem::Literal`.
  InterpSegment_Literal
  /// Corresponds to `@syntax.InterpElem::Source`.
  InterpSegment_Source

  // Clauses retained for grammar structure and error recovery.
  /// Return/error annotation feeding `return_type` and `error_type` AST fields.
  Clause_ReturnType
  /// Corresponds to `@syntax.WhereClause`.
  Clause_Where
  /// Corresponds to `@syntax.ConstrDecl`.
  Clause_ConstructorDecl
  /// Using entry lowered to an `(AliasTarget, UsingKind)` pair in `Impl::TopUsing`.
  Clause_UsingItem
  /// Import package entry lowered to `@syntax.ImportPackage`.
  Clause_ImportPackage
  /// Method entry lowered to `@syntax.AliasTarget` in `Impl::TopExtend`.
  Clause_ExtendItem
  /// Binder/expression entry in a `Pattern_With` binding list.
  Clause_WithBinding
  /// Corresponds to `@syntax.DerivingDirective`.
  Clause_DerivingDirective

  // Context-specific lists.
  /// CST container lowered to `List[@syntax.Argument]`.
  List_ArgumentList
  /// CST container lowered to `@syntax.Parameters`.
  List_ParameterList
  /// CST container lowered to `TraitMethodDecl::params`.
  List_TraitMethodParameterList
  /// CST container lowered to `TypeDecl::params : List[TypeDeclBinder]`.
  List_TypeDeclParameterList
  /// CST container lowered to declaration quantifiers `List[TypeVarBinder]`.
  List_TypeVarBinderList
  /// CST container lowered to `ConstrDecl::args : List[ConstrParam]`.
  List_ConstructorParameterList
  /// CST container lowered to `TypeDesc::TupleStruct` elements.
  List_TupleStructTypeList
  /// CST container lowered to `TypeDesc::Record::fields`.
  List_StructFieldList
  /// CST container lowered to `TraitDecl::methods`.
  List_TraitMethodList
  /// CST container lowered to record-expression `List[FieldDef]`.
  List_RecordFieldList
  /// CST container lowered to `Expr::Map::elems`.
  List_MapFieldList
  /// Statement container folded into nested statements and `@syntax.Expr::Sequence`.
  List_BlockStatementList
  /// CST container lowered to `Expr::Continue::args`.
  List_ContinueArgumentList
  /// CST container lowered to `Expr::Tuple::exprs`.
  List_TupleElementList
  /// CST container lowered to `Type::Tuple::tys` or arrow arguments.
  List_TupleTypeList
  /// CST container lowered to array `SpreadableElem` values.
  List_ArrayElementList
  /// CST container lowered to `Type::Name::tys`.
  List_TypeArgumentList
  /// CST container lowered to `Pattern::Tuple::pats`.
  List_TuplePatternList
  /// CST container lowered to `Pattern::Constr::args`.
  List_ConstructorPatternArgumentList
  /// CST container lowered to `LexCase::pat`.
  List_LexTopPatternList
  /// CST container lowered to the elements of `@syntax.ArrayPatterns`.
  List_ArrayPatternList
  /// CST container lowered to `Pattern::Record::fields`.
  List_RecordPatternFieldList
  /// CST container lowered to `Pattern::Map::elems`.
  List_MapPatternFieldList
  /// CST container lowered to `WhereClause::fields`.
  List_WhereFieldList
  /// Object-type-literal field container; the current AST has no matching field list.
  List_ObjectFieldList
  /// CST container lowered to `List[@syntax.ConstrDecl]`.
  List_ConstructorList
  /// CST container lowered to `Impl::TopUsing::names`.
  List_UsingItemList
  /// CST container lowered to `Impl::TopImport::packages`.
  List_ImportPackageList
  /// CST container lowered to `Impl::TopExtend::names`.
  List_ExtendItemList
  /// CST container lowered to `TypeDecl::deriving`.
  List_DerivingList
  /// CST container lowered to `Expr::Match` or `Expr::Try` cases.
  List_MatchCaseList
  /// CST container lowered to `Expr::LexMatch::cases`.
  List_LexCaseList
  /// CST container lowered to `Expr::LexScan::cases`.
  List_LexScanCaseList
  /// CST container lowered to `Expr::RegexMatch::bindings`.
  List_RegexBindingList
  /// CST container lowered to `@syntax.LexPattern::Sequence`.
  List_LexPatternList
  /// CST container lowered to `(Binder, Expr)` loop binding lists.
  List_ForBindingList
  /// CST container lowered to list-comprehension `(Binder, Expr)` state bindings.
  List_ForEachBindingList
  /// CST container lowered to `Pattern::With::bindings`.
  List_WithBindingList
} derive(@debug.Debug, Eq)

///|
/// Returns whether this kind has a source-token shape.
///
/// A child labeled with an `_loc` suffix may reuse a `Syntax_*` kind as
/// non-owning location metadata; callers enumerating original tokens must skip
/// those labeled children.
fn NodeKind::is_syntax_token(self : NodeKind) -> Bool {
  match self {
    Syntax_Keyword
    | Syntax_Delimiter
    | Syntax_Separator
    | Syntax_Operator
    | Syntax_Identifier
    | Syntax_Label
    | Syntax_Accessor
    | Syntax_Literal
    | Syntax_Interpolation
    | Syntax_Attribute
    | Syntax_Comment
    | Syntax_Eof
    | Syntax_Unexpected => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a top-level implementation item.
pub fn NodeKind::is_impl(self : NodeKind) -> Bool {
  match self {
    Impl_Import
    | Impl_Extend
    | Impl_Using
    | Impl_Function
    | Impl_Let
    | Impl_Const
    | Impl_TypeAbstract
    | Impl_TypeAlias
    | Impl_RecordStruct
    | Impl_TupleStruct
    | Impl_EnumVariant
    | Impl_ExtensibleEnum
    | Impl_ExtendEnum
    | Impl_Suberror
    | Impl_SuberrorEnum
    | Impl_EnumView
    | Impl_Trait
    | Impl_ImplMethod
    | Impl_ImplRelation
    | Impl_Test => true
    _ => false
  }
}

///|
/// Returns whether this kind represents an expression.
pub fn NodeKind::is_expr(self : NodeKind) -> Bool {
  match self {
    Expr_Ident
    | Expr_Constant
    | Expr_Interp
    | Expr_BytesInterp
    | Expr_Block
    | Expr_Apply
    | Expr_DotApply
    | Expr_Field
    | Expr_ArrayGet
    | Expr_ArrayGetSlice
    | Expr_Array
    | Expr_ArraySpread
    | Expr_ArraySet
    | Expr_ArrayAugmentedSet
    | Expr_ListComprehension
    | Expr_Unit
    | Expr_Group
    | Expr_Tuple
    | Expr_Sequence
    | Expr_Constraint
    | Expr_Record
    | Expr_RecordUpdate
    | Expr_Map
    | Expr_Infix
    | Expr_Pipe
    | Expr_RevPipe
    | Expr_Implies
    | Expr_Unary
    | Expr_Assign
    | Expr_Mutate
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
    | Expr_LetAnd
    | Expr_LetFn
    | Expr_Return
    | Expr_Raise
    | Expr_Break
    | Expr_Continue
    | Expr_LabelledBlock
    | Expr_Defer
    | Expr_Guard
    | Expr_Quantifier
    | Expr_Hole
    | Expr_Constr
    | Expr_MultilineString
    | Expr_ProofAssert
    | Expr_ProofLet
    | Expr_MissingExpr => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a pattern.
pub fn NodeKind::is_pattern(self : NodeKind) -> Bool {
  match self {
    Pattern_Var
    | Pattern_Any
    | Pattern_Constant
    | Pattern_Tuple
    | Pattern_Array
    | Pattern_Rest
    | Pattern_Record
    | Pattern_Map
    | Pattern_Constr
    | Pattern_SpecialConstr
    | Pattern_Or
    | Pattern_Alias
    | Pattern_Range
    | Pattern_Constraint
    | Pattern_With
    | Pattern_MissingPattern => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a type expression.
pub fn NodeKind::is_type(self : NodeKind) -> Bool {
  match self {
    Type_Name
    | Type_Tuple
    | Type_Arrow
    | Type_Application
    | Type_Option
    | Type_Object
    | Type_Any
    | Type_Group
    | Type_MissingType => true
    _ => false
  }
}

///|
fn NodeKind::is_error_type(self : NodeKind) -> Bool {
  match self {
    ErrorType_Error | ErrorType_Default | ErrorType_Noraise | ErrorType_Maybe =>
      true
    _ => false
  }
}

///|
fn NodeKind::is_type_desc(self : NodeKind) -> Bool {
  match self {
    TypeDesc_Abstract
    | TypeDesc_ExtensibleEnum
    | TypeDesc_ErrorNoPayload
    | TypeDesc_ErrorEnumPayload
    | TypeDesc_Variant
    | TypeDesc_ExtendEnum
    | TypeDesc_Record
    | TypeDesc_TupleStruct
    | TypeDesc_Alias => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a call argument.
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
fn NodeKind::is_name(self : NodeKind) -> Bool {
  match self {
    Name_Identifier
    | Name_Package
    | LongIdent_Ident
    | LongIdent_Dot
    | Name_Binder
    | Name_Label
    | Name_Type
    | Name_Constr
    | Name_ConstrId
    | Name_Var
    | Name_Constructor
    | Name_Field
    | Name_AliasTarget
    | Name_Test => true
    _ => false
  }
}

///|
/// Returns whether this kind represents a literal constant.
pub fn NodeKind::is_constant(self : NodeKind) -> Bool {
  match self {
    Constant_Bool
    | Constant_Byte
    | Constant_Bytes
    | Constant_Char
    | Constant_Int
    | Constant_Int64
    | Constant_UInt
    | Constant_UInt64
    | Constant_Float
    | Constant_Double
    | Constant_String
    | Constant_Regex
    | Constant_BigInt => true
    _ => false
  }
}

///|
fn NodeKind::is_operator(self : NodeKind) -> Bool {
  match self {
    Operator_Infix
    | Operator_Augmented
    | Operator_UnaryPlus
    | Operator_UnaryMinus
    | Operator_UnaryNot => true
    _ => false
  }
}

///|
fn NodeKind::is_dot_dot_binder(self : NodeKind) -> Bool {
  match self {
    DotDotBinder_None
    | DotDotBinder_Underscore
    | DotDotBinder_Binder
    | DotDotBinder_BinderAs => true
    _ => false
  }
}

///|
fn NodeKind::is_constructor_extra_info(self : NodeKind) -> Bool {
  match self {
    ConstructorExtraInfo_None
    | ConstructorExtraInfo_Package
    | ConstructorExtraInfo_TypeName
    | ConstructorExtraInfo_TypeNameWithConstrPackage => true
    _ => false
  }
}

///|
fn NodeKind::is_lex_literal(self : NodeKind) -> Bool {
  match self {
    LexLiteral_String | LexLiteral_Regex => true
    _ => false
  }
}

///|
fn NodeKind::is_lex_top_pattern(self : NodeKind) -> Bool {
  match self {
    LexTopPattern_Pattern | LexTopPattern_Binder | LexTopPattern_Wildcard =>
      true
    _ => false
  }
}

///|
fn NodeKind::is_lex_scan_case_pattern(self : NodeKind) -> Bool {
  match self {
    LexScanCasePattern_Pattern
    | LexScanCasePattern_Binder
    | LexScanCasePattern_Wildcard => true
    _ => false
  }
}

///|
fn NodeKind::is_scalar_value(self : NodeKind) -> Bool {
  match self {
    ConstructorTag_Value | Language_Value => true
    _ => false
  }
}

///|
fn NodeKind::is_stub(self : NodeKind) -> Bool {
  match self {
    Stub_ModuleName | Stub_FunctionName | Stub_Code => true
    _ => false
  }
}

///|
fn NodeKind::is_accessor(self : NodeKind) -> Bool {
  match self {
    Accessor_Label | Accessor_Index | Accessor_Newtype => true
    _ => false
  }
}

///|
fn NodeKind::is_decl_body(self : NodeKind) -> Bool {
  match self {
    DeclBody_Expr
    | DeclBody_Import
    | DeclBody_EmbeddedString
    | DeclBody_EmbeddedMultilineString
    | DeclBody_None => true
    _ => false
  }
}

///|
fn NodeKind::is_syntax_flag(self : NodeKind) -> Bool {
  match self {
    Visibility_Default
    | Visibility_Pub
    | Visibility_Priv
    | RangeKind_Inclusive
    | RangeKind_Exclusive
    | HoleKind_Incomplete
    | HoleKind_Todo
    | FnKind_Lambda
    | FnKind_Arrow
    | Group_Brace
    | Group_Paren
    | TrailingMark_Comma
    | TrailingMark_Semi
    | TrailingMark_None
    | TryOperatorKind_Exclamation
    | QuantifierKind_Forall
    | QuantifierKind_Exists
    | UsingKind_Value
    | UsingKind_Type
    | UsingKind_Trait
    | Openness_Open
    | Openness_Closed
    | ArrayKind_Array
    | ArrayKind_Iter
    | Flag_Async
    | Flag_Declare
    | Flag_Mutable
    | Flag_Pun
    | Flag_ReturnSelf
    | Flag_Conditional
    | Flag_HasTry
    | Flag_MatchAbsent
    | Flag_Faked
    | Flag_Augmented
    | Flag_Object
    | Flag_Exclamation => true
    _ => false
  }
}

///|
fn NodeKind::is_interp_segment(self : NodeKind) -> Bool {
  match self {
    InterpSegment_Literal | InterpSegment_Source => true
    _ => false
  }
}
