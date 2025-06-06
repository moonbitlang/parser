pub(all) typealias Position = @ast_types.Position

pub type! ParseError {
  UnexpectedToken(Token, (Position, Position), Array[TokenKind])
  UnexpectedEndOfInput(Position, Array[TokenKind])
} derive(Show)

typealias YYObj = Error

priv type! YYObj_Void

priv type! YYObj_Int Int

priv type! YYObj_Sig Sig

priv type! YYObj_Bool Bool

priv type! YYObj_Mbti Mbti

priv type! YYObj_Name Name

priv type! YYObj_Type Type

priv type! YYObj_Unit Unit

priv type! YYObj_Option_Bool_ Option[Bool]

priv type! YYObj_Option_Name_ Option[Name]

priv type! YYObj_Option__immut_list_T_Type__ Option[@immut/list.T[Type]]

priv type! YYObj_Option__immut_list_T_ConstrParam__ Option[@immut/list.T[ConstrParam]]

priv type! YYObj_Option__String__Location__ Option[(String, Location)]

priv type! YYObj_String String

priv type! YYObj_Comment Comment

priv type! YYObj_FuncSig FuncSig

priv type! YYObj_ImplSig ImplSig

priv type! YYObj_TypeSig TypeSig

priv type! YYObj_AliasSig AliasSig

priv type! YYObj_ConstSig ConstSig

priv type! YYObj_Constant Constant

priv type! YYObj_TraitSig TraitSig

priv type! YYObj_ValueSig ValueSig

priv type! YYObj_FieldDecl FieldDecl

priv type! YYObj_Parameter Parameter

priv type! YYObj_ConstrDecl ConstrDecl

priv type! YYObj_Visibility Visibility

priv type! YYObj_CharLiteral CharLiteral

priv type! YYObj_ConstrParam ConstrParam

priv type! YYObj_InterpLiteral InterpLiteral

priv type! YYObj_PackageImport PackageImport

priv type! YYObj_QualifiedName QualifiedName

priv type! YYObj_StringLiteral StringLiteral

priv type! YYObj_TraitMethodSig TraitMethodSig

priv type! YYObj_TraitMethodParameter TraitMethodParameter

priv type! YYObj_TypeParamNoConstraints TypeParamNoConstraints

priv type! YYObj_TypeParamWithConstraints TypeParamWithConstraints

priv type! YYObj__immut_list_T_Type_ @immut/list.T[Type]

priv type! YYObj__immut_list_T_String_ @immut/list.T[String]

priv type! YYObj__immut_list_T_FieldDecl_ @immut/list.T[FieldDecl]

priv type! YYObj__immut_list_T_Parameter_ @immut/list.T[Parameter]

priv type! YYObj__immut_list_T_ConstrDecl_ @immut/list.T[ConstrDecl]

priv type! YYObj__immut_list_T_ConstrParam_ @immut/list.T[ConstrParam]

priv type! YYObj__immut_list_T_PackageImport_ @immut/list.T[PackageImport]

priv type! YYObj__immut_list_T_QualifiedName_ @immut/list.T[QualifiedName]

priv type! YYObj__immut_list_T_TraitMethodSig_ @immut/list.T[TraitMethodSig]

priv type! YYObj__immut_list_T_TraitMethodParameter_ @immut/list.T[TraitMethodParameter]

priv type! YYObj__immut_list_T_TypeParamNoConstraints_ @immut/list.T[TypeParamNoConstraints]

priv type! YYObj__immut_list_T_TypeParamWithConstraints_ @immut/list.T[TypeParamWithConstraints]

priv type! YYObj__immut_list_T__Sig__Location__ @immut/list.T[(Sig, Location)]

priv type! YYObj_String_ String?

priv type! YYObj__Name___immut_list_T_TypeParamNoConstraints__ (Name, @immut/list.T[TypeParamNoConstraints])

priv type! YYObj__Type__ErrorType_ (Type, ErrorType)

priv type! YYObj__String__Location_ (String, Location)

typealias YYState = (YYSymbol) -> YYDecision

typealias YYAction = (Position, ArrayView[(YYObj, Position, Position)]) -> YYObj

priv enum YYDecision {
  Accept
  Shift(YYState)
  Reduce(Int, YYSymbol, YYAction)
  ReduceNoLookahead(Int, YYSymbol, YYAction)
  Error
}

priv enum YYSymbol {
  T_CHAR
  T_INT
  T_BYTE
  T_BYTES
  T_FLOAT
  T_STRING
  T_MULTILINE_STRING
  T_MULTILINE_INTERP
  T_INTERP
  T_ATTRIBUTE
  T_LIDENT
  T_UIDENT
  T_POST_LABEL
  T_COMMENT
  T_NEWLINE
  T_INFIX1
  T_INFIX2
  T_INFIX3
  T_INFIX4
  T_AUGMENTED_ASSIGNMENT
  T_EOF
  T_FALSE
  T_TRUE
  T_PUB
  T_PRIV
  T_READONLY
  T_IMPORT
  T_EXTERN
  T_BREAK
  T_CONTINUE
  T_STRUCT
  T_ENUM
  T_TRAIT
  T_DERIVE
  T_IMPL
  T_WITH
  T_RAISE
  T_THROW
  T_TRY
  T_CATCH
  T_ASYNC
  T_TYPEALIAS
  T_TRAITALIAS
  T_FNALIAS
  T_EQUAL
  T_LPAREN
  T_RPAREN
  T_COMMA
  T_MINUS
  T_QUESTION
  T_EXCLAMATION
  T_DOT_LIDENT
  T_DOT_UIDENT
  T_DOT_INT
  T_DOT_LPAREN
  T_COLONCOLON
  T_COLON
  T_SEMI
  T_LBRACKET
  T_PLUS
  T_RBRACKET
  T_UNDERSCORE
  T_BAR
  T_LBRACE
  T_RBRACE
  T_AMPERAMPER
  T_AMPER
  T_CARET
  T_BARBAR
  T_PACKAGE_NAME
  T_AS
  T_PIPE
  T_ELSE
  T_FN
  T_IF
  T_LET
  T_CONST
  T_MATCH
  T_MUTABLE
  T_TYPE
  T_FAT_ARROW
  T_THIN_ARROW
  T_WHILE
  T_RETURN
  T_DOTDOT
  T_RANGE_INCLUSIVE
  T_RANGE_EXCLUSIVE
  T_ELLIPSIS
  T_TEST
  T_LOOP
  T_GUARD
  T_FOR
  T_IN
  T_IS
  NT_mbti
  NT_imports
  NT_package_import
  NT_sigs
  NT_sig_
  NT_const_sig
  NT_value_sig
  NT_method_prefix
  NT_func_sig_no_attr
  NT_func_sig
  NT_impl_method_sig
  NT_trait_method_sig
  NT_type_sig
  NT_impl_sig
  NT_trait_sig
  NT_alias_sig
  NT_enum_constructor
  NT_constructor_param
  NT_record_decl_field
  NT_type_param_with_constraints
  NT_type_params_with_constraints
  NT_type_param_no_constraints
  NT_type_params_no_constraints
  NT_optional_type_params_no_constraints
  NT_type_constraint
  NT_type_decl_name_with_params
  NT_simple_type
  NT_type_
  NT_return_type
  NT_error_type
  NT_optional_type_arguments
  NT_parameter
  NT_trait_method_parameter
  NT_constant
  NT_pub_attr
  NT_qualified_uident
  NT_qualified_uident_
  NT_uident
  NT_lident
  NT_label
  NT_post_label
  NT_separated_nonempty_list_SEMI_package_import_
  NT_loption_type_params_with_constraints_
  NT_separated_list_COMMA_parameter_
  NT_nonempty_list_ATTRIBUTE_
  NT_list_ATTRIBUTE_
  NT_separated_list_COMMA_trait_method_parameter_
  NT_boption_eq_underscore_
  NT_separated_list_SEMI_enum_constructor_
  NT_separated_list_SEMI_record_decl_field_
  NT_separated_nonempty_list_PLUS_qualified_uident_
  NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___
  NT_separated_nonempty_list_SEMI_trait_method_sig_
  NT_separated_nonempty_list_COMMA_constructor_param_
  NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__
  NT_option_eq_tag_
  NT_boption_MUTABLE_
  NT_separated_nonempty_list_PLUS_type_constraint_
  NT_separated_list_COMMA_type_param_with_constraints_
  NT_separated_list_COMMA_type_param_no_constraints_
  NT_separated_nonempty_list_COMMA_type__
  NT_separated_nonempty_list_PLUS_error_type_
  NT_separated_nonempty_list_COMMA_parameter_
  NT_separated_nonempty_list_COMMA_trait_method_parameter_
  NT_separated_nonempty_list_SEMI_enum_constructor_
  NT_separated_nonempty_list_SEMI_record_decl_field_
  NT_separated_nonempty_list_COMMA_type_param_with_constraints_
  NT_separated_nonempty_list_COMMA_type_param_no_constraints_
  EOI
}

// Workaround for EOI unused warning
fn init {
  match (EOI : YYSymbol) {
    EOI => ()
    _ => ()
  }
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_0(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_Type_(_dollar3)
  YYObj__immut_list_T_Type_({();  Cons(_dollar1, _dollar3) })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_1(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrParam(_dollar1)
  YYObj__immut_list_T_ConstrParam_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 168|    vis "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Abstract} }
fn yy_action_2(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Abstract} })
}

// file:///./mbti_parser.mbty
// 251|    simple_type { $1 }
fn yy_action_4(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj_Type({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 126|    trait_sig { Trait($1) }
fn yy_action_5(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitSig(_dollar1)
  YYObj_Sig({();  Trait(_dollar1) })
}

// file:///./mbti_parser.mbty
// 190|    vis "traitalias" uident "=" qualified_uident { TraitAlias(vis=$1, name=$3, trait_name=$5) }
fn yy_action_6(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj_Name(_dollar3)
  guard _args[3].0 is YYObj_QualifiedName(_dollar5)
  YYObj_AliasSig({();  TraitAlias(vis=_dollar1, name=_dollar3, trait_name=_dollar5) })
}

// file:///./mbti_parser.mbty
// 253|    is_async "(" ")" "->" return_type { TArrow(Nil, $5.0, err=$5.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_7(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_8(_last_pos, _args[0:0])
  guard _args[3].0 is YYObj__Type__ErrorType_(_dollar5)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Nil, _dollar5.0, err=_dollar5.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 171|    vis "type" "!" uident type_ { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(SinglePayload($5))} }
fn yy_action_9(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(SinglePayload(_dollar5))} })
}

// file:///./mbti_parser.mbty
// 283|    post_label ":" type_ { Labelled($1, $3) }
fn yy_action_11(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj_TraitMethodParameter({();  Labelled(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 169|    vis "type" type_decl_name_with_params type_ { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Newtype($4)} }
fn yy_action_12(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[3].0 is YYObj_Type(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Newtype(_dollar4)} })
}

// file:///./stdlib.mbty
// 10|    X { Some($1) }
fn yy_action_14(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  YYObj_Option_Bool_({();  Some(_dollar1) })
}

// file:///./mbti_parser.mbty
// 185|    vis "trait" uident { TraitSig::{vis: $1, name: $3, super_traits: Nil, methods: Nil} }
fn yy_action_15(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj_Name(_dollar3)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: Nil, methods: Nil} })
}

// file:///./mbti_parser.mbty
// 265|    qualified_uident_ { TName($1, Nil, loc=mk_loc($sloc)) }
fn yy_action_16(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TName(_dollar1, Nil, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 301|    FLOAT { Double($1) }
fn yy_action_17(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj_Constant({();  Double(_dollar1) })
}

// file:///./mbti_parser.mbty
// 173|    vis "struct" type_decl_name_with_params "{" separated_list(";", record_decl_field) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Record($5)} }
fn yy_action_18(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[3].0 is YYObj__immut_list_T_FieldDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Record(_dollar5)} })
}

// file:///./mbti_parser.mbty
// 184|    vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) "{" separated_nonempty_list(";", trait_method_sig) "}" { TraitSig::{vis: $1, name: $3, super_traits: $4, methods: $6} }
fn yy_action_19(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar3)
  guard _args[4].0 is YYObj__immut_list_T_QualifiedName_(_dollar4)
  guard _args[6].0 is YYObj__immut_list_T_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: _dollar4, methods: _dollar6} })
}

// file:///./mbti_parser.mbty
// 243|    "(" type_ "," separated_nonempty_list(",", type_) ")" { TTuple(Cons($2, $4), loc=mk_loc($sloc)) }
fn yy_action_20(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Type(_dollar2)
  guard _args[3].0 is YYObj__immut_list_T_Type_(_dollar4)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TTuple(Cons(_dollar2, _dollar4), loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 170|    vis "type" "!" uident { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(NoPayload)} }
fn yy_action_21(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(NoPayload)} })
}

// file:///./mbti_parser.mbty
// 174|    vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Variant($5)} }
fn yy_action_22(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[5].0 is YYObj__immut_list_T_ConstrDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Variant(_dollar5)} })
}

// file:///./mbti_parser.mbty
// 323|    UIDENT { QualifiedName::{pkg: None, name: $1, loc: mk_loc($sloc)} }
fn yy_action_23(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_QualifiedName({();  QualifiedName::{pkg: None, name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 341|    LIDENT { Name::{name: $1, loc: mk_loc($sloc)} }
fn yy_action_24(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  Name::{name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 261|    simple_type "?" error_type { ($1, MaybeError($3)) }
fn yy_action_25(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj__Type__ErrorType_({();  (_dollar1, MaybeError(_dollar3)) })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_26(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_Parameter_(_dollar1)
  YYObj__immut_list_T_Parameter_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 15|    X { $1 }
fn yy_action_27(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar1)
  YYObj__immut_list_T_TypeParamWithConstraints_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 172|    vis "type" "!" uident "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(EnumPayload($6))} }
fn yy_action_28(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[6].0 is YYObj__immut_list_T_ConstrDecl_(_dollar6)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(EnumPayload(_dollar6))} })
}

// file:///./mbti_parser.mbty
// 163|    "=" "_" {}
fn yy_action_29(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Unit({(); })
}

// file:///./mbti_parser.mbty
// 207|    boption("mut") lident ":" type_ { FieldDecl::{name: $2, vis: Default, mut_: $1, type_: $4, loc: mk_loc($sloc)} }
fn yy_action_30(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[0].0 is YYObj_Bool(_dollar1)
  guard _args[3].0 is YYObj_Type(_dollar4)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_FieldDecl({();  FieldDecl::{name: _dollar2, vis: Default, mut_: _dollar1, type_: _dollar4, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_31(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__immut_list_T_Type_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 103|    LIDENT STRING ioption(";") imports sigs EOF { Mbti::{package_name: $2, imports: $4, sigs: $5} }
fn yy_action_32(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_33(_args[1].2, _args[2:2])
  guard _args[1].0 is YYObj_StringLiteral(_dollar2)
  guard _args[2].0 is YYObj__immut_list_T_PackageImport_(_dollar4)
  guard _args[3].0 is YYObj__immut_list_T__Sig__Location__(_dollar5)
  YYObj_Mbti({();  Mbti::{package_name: _dollar2, imports: _dollar4, sigs: _dollar5} })
}

// file:///./mbti_parser.mbty
// 172|    vis "type" "!" uident "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(EnumPayload($6))} }
fn yy_action_34(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__immut_list_T_ConstrDecl_(_dollar6)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(EnumPayload(_dollar6))} })
}

// file:///./mbti_parser.mbty
// 319|    { false }
fn yy_action_8(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Bool({();  false })
}

// file:///./mbti_parser.mbty
// 282|    type_ { Positional($1) }
fn yy_action_35(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj_TraitMethodParameter({();  Positional(_dollar1) })
}

// file:///./mbti_parser.mbty
// 146|    FN loption(type_params_with_constraints) method_prefix lident delimited("(", separated_list(",", parameter), ")") "->" return_type { FuncSig::{type_name: $3, name: $4, type_params: $2, params: $5, return_: $7} }
fn yy_action_36(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_37(_args[3].2, _args[4:7])
  guard _args[2].0 is YYObj_Option_Name_(_dollar3)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[1].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar2)
  guard _sub_action_0_result is YYObj__immut_list_T_Parameter_(_dollar5)
  guard _args[8].0 is YYObj__Type__ErrorType_(_dollar7)
  YYObj_FuncSig({();  FuncSig::{type_name: _dollar3, name: _dollar4, type_params: _dollar2, params: _dollar5, return_: _dollar7} })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_38(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__immut_list_T_Type_({();  Cons(_dollar1, Nil) })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_39(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_FieldDecl_({();  Nil })
}

// file:///./mbti_parser.mbty
// 119|    sig_ ";" sigs { Cons(($1, mk_loc($loc($1))), $3) }
fn yy_action_40(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Sig(_dollar1)
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  guard _args[2].0 is YYObj__immut_list_T__Sig__Location__(_dollar3)
  YYObj__immut_list_T__Sig__Location__({();  Cons((_dollar1, mk_loc((_start_pos_of_item0, _end_pos_of_item0))), _dollar3) })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_41(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_PackageImport(_dollar1)
  YYObj__immut_list_T_PackageImport_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 278|    label "?" ":" type_ { OptionalOption($1, $4) }
fn yy_action_42(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[3].0 is YYObj_Type(_dollar4)
  YYObj_Parameter({();  OptionalOption(_dollar1, _dollar4) })
}

// file:///./mbti_parser.mbty
// 269|    delimited("[", separated_nonempty_list(",", type_), "]") { $1 }
fn yy_action_43(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_44(_last_pos, _args[0:3])
  guard _sub_action_0_result is YYObj__immut_list_T_Type_(_dollar1)
  YYObj__immut_list_T_Type_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 169|    vis "type" type_decl_name_with_params type_ { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Newtype($4)} }
fn yy_action_45(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[4].0 is YYObj_Type(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Newtype(_dollar4)} })
}

// file:///./mbti_parser.mbty
// 159|    lident delimited("(", separated_list(",", trait_method_parameter), ")") "->" return_type boption(eq_underscore) { TraitMethodSig::{name: $1, params: $2, return_: $4, has_default: $5} }
fn yy_action_46(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_47(_args[0].2, _args[1:4])
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _sub_action_0_result is YYObj__immut_list_T_TraitMethodParameter_(_dollar2)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar4)
  guard _args[6].0 is YYObj_Bool(_dollar5)
  YYObj_TraitMethodSig({();  TraitMethodSig::{name: _dollar1, params: _dollar2, return_: _dollar4, has_default: _dollar5} })
}

// file:///./mbti_parser.mbty
// 178|    "impl" type_params_with_constraints qualified_uident "for" type_ { Trait(type_params=$2, trait_name=$3, type_=$5) }
fn yy_action_48(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar2)
  guard _args[2].0 is YYObj_QualifiedName(_dollar3)
  guard _args[4].0 is YYObj_Type(_dollar5)
  YYObj_ImplSig({();  Trait(type_params=_dollar2, trait_name=_dollar3, type_=_dollar5) })
}

// file:///./mbti_parser.mbty
// 167|    vis "extern" "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $4.0, type_params: $4.1, components: Extern} }
fn yy_action_49(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4.0, type_params: _dollar4.1, components: Extern} })
}

// file:///./mbti_parser.mbty
// 129|    value_sig { Value($1) }
fn yy_action_50(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ValueSig(_dollar1)
  YYObj_Sig({();  Value(_dollar1) })
}

// file:///./stdlib.mbty
// 10|    X { Some($1) }
fn yy_action_51(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_Type_(_dollar1)
  YYObj_Option__immut_list_T_Type__({();  Some(_dollar1) })
}

// file:///./mbti_parser.mbty
// 112|    STRING { PackageImport::{name: $1, alias_: None} }
fn yy_action_52(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_PackageImport({();  PackageImport::{name: _dollar1, alias_: None} })
}

// file:///./mbti_parser.mbty
// 314|    "(" LIDENT ")" { Some($2) }
fn yy_action_53(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  YYObj_String_({();  Some(_dollar2) })
}

// file:///./mbti_parser.mbty
// 247|    "_" { TAny(mk_loc($sloc)) }
fn yy_action_54(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TAny(mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_47(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_TraitMethodParameter_(_dollar2)
  YYObj__immut_list_T_TraitMethodParameter_({();  _dollar2 })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_37(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_Parameter_(_dollar2)
  YYObj__immut_list_T_Parameter_({();  _dollar2 })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_55(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_TypeParamNoConstraints_({();  Nil })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_56(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_TypeParamNoConstraints_(_dollar1)
  YYObj__immut_list_T_TypeParamNoConstraints_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 9|    { None }
fn yy_action_57(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Option__immut_list_T_Type__({();  None })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_58(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Parameter(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_Parameter_(_dollar3)
  YYObj__immut_list_T_Parameter_({();  Cons(_dollar1, _dollar3) })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_59(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_ConstrParam_(_dollar2)
  YYObj__immut_list_T_ConstrParam_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 128|    const_sig { Const($1) }
fn yy_action_60(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstSig(_dollar1)
  YYObj_Sig({();  Const(_dollar1) })
}

// file:///./mbti_parser.mbty
// 307|    "priv" { Priv(loc=mk_loc($sloc)) }
fn yy_action_13(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Visibility({();  Priv(loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 9|    { None }
fn yy_action_33(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Option_Bool_({();  None })
}

// file:///./mbti_parser.mbty
// 258|    type_ { ($1, NoErrorType) }
fn yy_action_61(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__Type__ErrorType_({();  (_dollar1, NoErrorType) })
}

// file:///./mbti_parser.mbty
// 172|    vis "type" "!" uident "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(EnumPayload($6))} }
fn yy_action_62(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj__immut_list_T_ConstrDecl_(_dollar6)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(EnumPayload(_dollar6))} })
}

// file:///./mbti_parser.mbty
// 211|    uident { TypeParamWithConstraints::{name: $1, constraints: Nil} }
fn yy_action_63(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_TypeParamWithConstraints({();  TypeParamWithConstraints::{name: _dollar1, constraints: Nil} })
}

// file:///./mbti_parser.mbty
// 173|    vis "struct" type_decl_name_with_params "{" separated_list(";", record_decl_field) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Record($5)} }
fn yy_action_64(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[4].0 is YYObj__immut_list_T_FieldDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Record(_dollar5)} })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_65(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Parameter(_dollar1)
  YYObj__immut_list_T_Parameter_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 291|    CHAR { Char($1) }
fn yy_action_66(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_CharLiteral(_dollar1)
  YYObj_Constant({();  Char(_dollar1) })
}

// file:///./stdlib.mbty
// 5|    X { Some($1) }
fn yy_action_67(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_59(_last_pos, _args[0:3])
  guard _sub_action_0_result is YYObj__immut_list_T_ConstrParam_(_dollar1)
  YYObj_Option__immut_list_T_ConstrParam__({();  Some(_dollar1) })
}

// file:///./mbti_parser.mbty
// 306|    { Default }
fn yy_action_3(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Visibility({();  Default })
}

// file:///./mbti_parser.mbty
// 171|    vis "type" "!" uident type_ { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(SinglePayload($5))} }
fn yy_action_68(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(SinglePayload(_dollar5))} })
}

// file:///./mbti_parser.mbty
// 274|    type_ { Positional($1) }
fn yy_action_69(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj_Parameter({();  Positional(_dollar1) })
}

// file:///./mbti_parser.mbty
// 167|    vis "extern" "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $4.0, type_params: $4.1, components: Extern} }
fn yy_action_70(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4.0, type_params: _dollar4.1, components: Extern} })
}

// file:///./stdlib.mbty
// 20|    X { true }
fn yy_action_71(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_29(_last_pos, _args[0:2])
  YYObj_Bool({();  true })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_72(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_PackageImport(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_PackageImport_(_dollar3)
  YYObj__immut_list_T_PackageImport_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 260|    simple_type "!" separated_nonempty_list("+", error_type) { ($1, ErrorType($3)) }
fn yy_action_73(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_Type_(_dollar3)
  YYObj__Type__ErrorType_({();  (_dollar1, ErrorType(_dollar3)) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_74(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitMethodSig(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_TraitMethodSig_(_dollar3)
  YYObj__immut_list_T_TraitMethodSig_({();  Cons(_dollar1, _dollar3) })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_75(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_TypeParamWithConstraints_({();  Nil })
}

// file:///./mbti_parser.mbty
// 194|    uident option(delimited("(", separated_nonempty_list(",", constructor_param), ")")) option(eq_tag) { ConstrDecl::{name: $1, args: $2, tag: $3, loc: mk_loc($sloc)} }
fn yy_action_76(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[1].0 is YYObj_Option__immut_list_T_ConstrParam__(_dollar2)
  guard _args[2].0 is YYObj_Option__String__Location__(_dollar3)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_ConstrDecl({();  ConstrDecl::{name: _dollar1, args: _dollar2, tag: _dollar3, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_77(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FieldDecl(_dollar1)
  YYObj__immut_list_T_FieldDecl_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 189|    vis "typealias" type_decl_name_with_params "=" type_ { TypeAlias(vis=$1, name=$3.0, type_params=$3.1, type_=$5) }
fn yy_action_78(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[4].0 is YYObj_Type(_dollar5)
  YYObj_AliasSig({();  TypeAlias(vis=_dollar1, name=_dollar3.0, type_params=_dollar3.1, type_=_dollar5) })
}

// file:///./stdlib.mbty
// 5|    X { Some($1) }
fn yy_action_79(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_80(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__String__Location_(_dollar1)
  YYObj_Option__String__Location__({();  Some(_dollar1) })
}

// file:///./mbti_parser.mbty
// 151|    nonempty_list(ATTRIBUTE) func_sig_no_attr { $2 }
fn yy_action_81(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_FuncSig(_dollar2)
  YYObj_FuncSig({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 167|    vis "extern" "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $4.0, type_params: $4.1, components: Extern} }
fn yy_action_82(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[4].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4.0, type_params: _dollar4.1, components: Extern} })
}

// file:///./mbti_parser.mbty
// 150|    func_sig_no_attr { $1 }
fn yy_action_83(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FuncSig(_dollar1)
  YYObj_FuncSig({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 246|    "&" qualified_uident_ { TObject($2) }
fn yy_action_84(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_QualifiedName(_dollar2)
  YYObj_Type({();  TObject(_dollar2) })
}

// file:///./mbti_parser.mbty
// 169|    vis "type" type_decl_name_with_params type_ { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Newtype($4)} }
fn yy_action_85(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[2].0 is YYObj_Type(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Newtype(_dollar4)} })
}

// file:///./mbti_parser.mbty
// 198|    "=" INT { ($2, mk_loc($loc($2))) }
fn yy_action_80(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj__String__Location_({();  (_dollar2, mk_loc((_start_pos_of_item1, _end_pos_of_item1))) })
}

// file:///./mbti_parser.mbty
// 216|    "[" separated_list(",", type_param_with_constraints) "]" { $2 }
fn yy_action_86(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar2)
  YYObj__immut_list_T_TypeParamWithConstraints_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 312|    { None }
fn yy_action_87(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_String_({();  None })
}

// file:///./mbti_parser.mbty
// 333|    UIDENT { Name::{name: $1, loc: mk_loc($sloc)} }
fn yy_action_88(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  Name::{name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 133|    "const" uident ":" type_ "=" constant { ConstSig::{name: $2, type_: $4, value: $6} }
fn yy_action_89(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[3].0 is YYObj_Type(_dollar4)
  guard _args[5].0 is YYObj_Constant(_dollar6)
  YYObj_ConstSig({();  ConstSig::{name: _dollar2, type_: _dollar4, value: _dollar6} })
}

// file:///./stdlib.mbty
// 32|    L X { $2 }
fn yy_action_90(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_QualifiedName_(_dollar2)
  YYObj__immut_list_T_QualifiedName_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 244|    "(" type_ ")" { $2 }
fn yy_action_91(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Type(_dollar2)
  YYObj_Type({();  _dollar2 })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_44(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_Type_(_dollar2)
  YYObj__immut_list_T_Type_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 221|    "_" { Underscore(mk_loc($sloc)) }
fn yy_action_92(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_TypeParamNoConstraints({();  Underscore(mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 345|    POST_LABEL { Name::{name: $1, loc: mk_loc($sloc)} }
fn yy_action_93(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  Name::{name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 185|    vis "trait" uident { TraitSig::{vis: $1, name: $3, super_traits: Nil, methods: Nil} }
fn yy_action_94(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar3)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: Nil, methods: Nil} })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_95(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_ConstrDecl_({();  Nil })
}

// file:///./mbti_parser.mbty
// 125|    alias_sig { Alias($1) }
fn yy_action_96(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_AliasSig(_dollar1)
  YYObj_Sig({();  Alias(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_97(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamWithConstraints(_dollar1)
  YYObj__immut_list_T_TypeParamWithConstraints_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 118|    sig_ { Cons(($1, mk_loc($sloc)), Nil) }
fn yy_action_98(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Sig(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj__immut_list_T__Sig__Location__({();  Cons((_dollar1, mk_loc((_symbol_start_pos, _end_pos))), Nil) })
}

// file:///./mbti_parser.mbty
// 253|    is_async "(" ")" "->" return_type { TArrow(Nil, $5.0, err=$5.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_99(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_100(_last_pos, _args[0:1])
  guard _args[4].0 is YYObj__Type__ErrorType_(_dollar5)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Nil, _dollar5.0, err=_dollar5.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 124|    type_sig { Type($1) }
fn yy_action_101(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeSig(_dollar1)
  YYObj_Sig({();  Type(_dollar1) })
}

// file:///./mbti_parser.mbty
// 254|    is_async "(" type_ ")" "->" return_type { TArrow(Cons($3, Nil), $6.0, err=$6.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_102(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_100(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, Nil), _dollar6.0, err=_dollar6.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_103(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrDecl(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_ConstrDecl_(_dollar3)
  YYObj__immut_list_T_ConstrDecl_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 185|    vis "trait" uident { TraitSig::{vis: $1, name: $3, super_traits: Nil, methods: Nil} }
fn yy_action_104(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar3)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: Nil, methods: Nil} })
}

// file:///./mbti_parser.mbty
// 225|    "[" separated_list(",", type_param_no_constraints) "]" { $2 }
fn yy_action_105(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_TypeParamNoConstraints_(_dollar2)
  YYObj__immut_list_T_TypeParamNoConstraints_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 113|    STRING "as" LIDENT { PackageImport::{name: $1, alias_: Some($3)} }
fn yy_action_106(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  guard _args[2].0 is YYObj_String(_dollar3)
  YYObj_PackageImport({();  PackageImport::{name: _dollar1, alias_: Some(_dollar3)} })
}

// file:///./mbti_parser.mbty
// 337|    LIDENT { Name::{name: $1, loc: mk_loc($sloc)} }
fn yy_action_107(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  Name::{name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_108(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_TraitMethodParameter_(_dollar1)
  YYObj__immut_list_T_TraitMethodParameter_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_109(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_FieldDecl_(_dollar1)
  YYObj__immut_list_T_FieldDecl_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 292|    INT {
// 293|    match $1 {
// 294|      [.. lit, .. "UL"] => UInt64(lit.to_string())
// 295|      [.. lit, 'L'] => Int64(lit.to_string())
// 296|      [.. lit, 'U'] => UInt(lit.to_string())
// 297|      [.. lit, 'N'] => BigInt(lit.to_string())
// 298|      lit => Int(lit)
// 299|    }
// 300|  }
fn yy_action_110(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj_Constant({(); 
    match _dollar1 {
      [.. lit, .. "UL"] => UInt64(lit.to_string())
      [.. lit, 'L'] => Int64(lit.to_string())
      [.. lit, 'U'] => UInt(lit.to_string())
      [.. lit, 'N'] => BigInt(lit.to_string())
      lit => Int(lit)
    }
  })
}

// file:///./mbti_parser.mbty
// 254|    is_async "(" type_ ")" "->" return_type { TArrow(Cons($3, Nil), $6.0, err=$6.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_111(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_8(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _args[4].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, Nil), _dollar6.0, err=_dollar6.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 50|    X nonempty_list(X) { Cons($1, $2) }
fn yy_action_112(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  guard _args[1].0 is YYObj__immut_list_T_String_(_dollar2)
  YYObj__immut_list_T_String_({();  Cons(_dollar1, _dollar2) })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_113(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar1)
  YYObj__immut_list_T_TypeParamWithConstraints_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 49|    X { Cons($1, Nil) }
fn yy_action_114(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj__immut_list_T_String_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 127|    impl_sig { Impl($1) }
fn yy_action_115(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ImplSig(_dollar1)
  YYObj_Sig({();  Impl(_dollar1) })
}

// file:///./mbti_parser.mbty
// 318|    "async" { true }
fn yy_action_100(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Bool({();  true })
}

// file:///./mbti_parser.mbty
// 190|    vis "traitalias" uident "=" qualified_uident { TraitAlias(vis=$1, name=$3, trait_name=$5) }
fn yy_action_116(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar3)
  guard _args[5].0 is YYObj_QualifiedName(_dollar5)
  YYObj_AliasSig({();  TraitAlias(vis=_dollar1, name=_dollar3, trait_name=_dollar5) })
}

// file:///./stdlib.mbty
// 4|    { None }
fn yy_action_117(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Option__String__Location__({();  None })
}

// file:///./mbti_parser.mbty
// 289|    BYTE { Byte($1) }
fn yy_action_118(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_CharLiteral(_dollar1)
  YYObj_Constant({();  Byte(_dollar1) })
}

// file:///./mbti_parser.mbty
// 287|    TRUE { Bool(true) }
fn yy_action_119(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Constant({();  Bool(true) })
}

// file:///./stdlib.mbty
// 4|    { None }
fn yy_action_120(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Option__immut_list_T_ConstrParam__({();  None })
}

// file:///./mbti_parser.mbty
// 117|     { Nil }
fn yy_action_121(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T__Sig__Location__({();  Nil })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_122(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__immut_list_T_QualifiedName_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 189|    vis "typealias" type_decl_name_with_params "=" type_ { TypeAlias(vis=$1, name=$3.0, type_params=$3.1, type_=$5) }
fn yy_action_123(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[5].0 is YYObj_Type(_dollar5)
  YYObj_AliasSig({();  TypeAlias(vis=_dollar1, name=_dollar3.0, type_params=_dollar3.1, type_=_dollar5) })
}

// file:///./mbti_parser.mbty
// 174|    vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Variant($5)} }
fn yy_action_124(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[4].0 is YYObj__immut_list_T_ConstrDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Variant(_dollar5)} })
}

// file:///./mbti_parser.mbty
// 170|    vis "type" "!" uident { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(NoPayload)} }
fn yy_action_125(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(NoPayload)} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_126(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_QualifiedName_(_dollar3)
  YYObj__immut_list_T_QualifiedName_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 230|    type_params_no_constraints { $1 }
fn yy_action_127(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_TypeParamNoConstraints_(_dollar1)
  YYObj__immut_list_T_TypeParamNoConstraints_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_128(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrParam(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_ConstrParam_(_dollar3)
  YYObj__immut_list_T_ConstrParam_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 171|    vis "type" "!" uident type_ { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(SinglePayload($5))} }
fn yy_action_129(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj_Type(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(SinglePayload(_dollar5))} })
}

// file:///./mbti_parser.mbty
// 179|    "impl" qualified_uident "for" type_ { Trait(type_params=Nil, trait_name=$2, type_=$4) }
fn yy_action_130(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_QualifiedName(_dollar2)
  guard _args[3].0 is YYObj_Type(_dollar4)
  YYObj_ImplSig({();  Trait(type_params=Nil, trait_name=_dollar2, type_=_dollar4) })
}

// file:///./mbti_parser.mbty
// 308|    "pub" pub_attr { Pub(attr=$2, loc=mk_loc($sloc)) }
fn yy_action_10(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String_(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Visibility({();  Pub(attr=_dollar2, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 252|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { TArrow(Cons($3, $5.or(Nil)), $8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_131(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_100(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_57(_args[3].2, _args[4:4])
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _sub_action_1_result is YYObj_Option__immut_list_T_Type__(_dollar5)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, _dollar5.or(Nil)), _dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 190|    vis "traitalias" uident "=" qualified_uident { TraitAlias(vis=$1, name=$3, trait_name=$5) }
fn yy_action_132(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar3)
  guard _args[4].0 is YYObj_QualifiedName(_dollar5)
  YYObj_AliasSig({();  TraitAlias(vis=_dollar1, name=_dollar3, trait_name=_dollar5) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_133(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamNoConstraints(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_TypeParamNoConstraints_(_dollar3)
  YYObj__immut_list_T_TypeParamNoConstraints_({();  Cons(_dollar1, _dollar3) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_134(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitMethodParameter(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_TraitMethodParameter_(_dollar3)
  YYObj__immut_list_T_TraitMethodParameter_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 276|    post_label ":" type_ "=" ".." { OptionalDefault($1, $3) }
fn yy_action_135(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj_Parameter({();  OptionalDefault(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 329|    PACKAGE_NAME DOT_UIDENT { QualifiedName::{pkg: Some($1), name: $2, loc: mk_loc($sloc)} }
fn yy_action_136(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  guard _args[1].0 is YYObj_String(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_QualifiedName({();  QualifiedName::{pkg: Some(_dollar1), name: _dollar2, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 252|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { TArrow(Cons($3, $5.or(Nil)), $8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_137(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_100(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_51(_args[3].2, _args[4:5])
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _sub_action_1_result is YYObj_Option__immut_list_T_Type__(_dollar5)
  guard _args[7].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, _dollar5.or(Nil)), _dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 238|    uident optional_type_params_no_constraints { ($1, $2) }
fn yy_action_138(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[1].0 is YYObj__immut_list_T_TypeParamNoConstraints_(_dollar2)
  YYObj__Name___immut_list_T_TypeParamNoConstraints__({();  (_dollar1, _dollar2) })
}

// file:///./mbti_parser.mbty
// 324|    PACKAGE_NAME DOT_UIDENT { QualifiedName::{pkg: Some($1), name: $2, loc: mk_loc($sloc)} }
fn yy_action_139(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  guard _args[1].0 is YYObj_String(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_QualifiedName({();  QualifiedName::{pkg: Some(_dollar1), name: _dollar2, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 168|    vis "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Abstract} }
fn yy_action_140(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Abstract} })
}

// file:///./mbti_parser.mbty
// 259|    simple_type "!" { ($1, DefaultErrorType(mk_loc($loc($2)))) }
fn yy_action_141(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj__Type__ErrorType_({();  (_dollar1, DefaultErrorType(mk_loc((_start_pos_of_item1, _end_pos_of_item1)))) })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_142(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_Parameter_({();  Nil })
}

// file:///./mbti_parser.mbty
// 168|    vis "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Abstract} }
fn yy_action_143(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Abstract} })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_144(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitMethodSig(_dollar1)
  YYObj__immut_list_T_TraitMethodSig_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 242|    simple_type "?" { TOption($1, loc=mk_loc($sloc), question_loc=mk_loc($loc($2))) }
fn yy_action_145(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj_Type({();  TOption(_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos)), question_loc=mk_loc((_start_pos_of_item1, _end_pos_of_item1))) })
}

// file:///./stdlib.mbty
// 14|    { Nil }
fn yy_action_146(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_TypeParamWithConstraints_({();  Nil })
}

// file:///./mbti_parser.mbty
// 202|    boption("mut") type_ { ConstrParam::{mut_: $1, type_: $2, label: None} }
fn yy_action_147(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  guard _args[1].0 is YYObj_Type(_dollar2)
  YYObj_ConstrParam({();  ConstrParam::{mut_: _dollar1, type_: _dollar2, label: None} })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_148(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_ConstrDecl_(_dollar1)
  YYObj__immut_list_T_ConstrDecl_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_149(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_QualifiedName_(_dollar3)
  YYObj__immut_list_T_QualifiedName_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 137|    "let" lident ":" type_ { ValueSig::{name: $2, type_: $4} }
fn yy_action_150(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[3].0 is YYObj_Type(_dollar4)
  YYObj_ValueSig({();  ValueSig::{name: _dollar2, type_: _dollar4} })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_151(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrDecl(_dollar1)
  YYObj__immut_list_T_ConstrDecl_({();  Cons(_dollar1, Nil) })
}

// file:///./stdlib.mbty
// 19|    { false }
fn yy_action_152(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Bool({();  false })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_153(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamNoConstraints(_dollar1)
  YYObj__immut_list_T_TypeParamNoConstraints_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 170|    vis "type" "!" uident { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(NoPayload)} }
fn yy_action_154(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(NoPayload)} })
}

// file:///./mbti_parser.mbty
// 313|    "(" "readonly" ")" { Some("readonly") }
fn yy_action_155(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_String_({();  Some("readonly") })
}

// file:///./mbti_parser.mbty
// 141|    { None }
fn yy_action_156(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Option_Name_({();  None })
}

// file:///./mbti_parser.mbty
// 290|    BYTES { Bytes($1) }
fn yy_action_157(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_Constant({();  Bytes(_dollar1) })
}

// file:///./stdlib.mbty
// 20|    X { true }
fn yy_action_158(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Bool({();  true })
}

// file:///./mbti_parser.mbty
// 245|    qualified_uident_ optional_type_arguments { TName($1, $2, loc=mk_loc($sloc)) }
fn yy_action_159(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  guard _args[1].0 is YYObj__immut_list_T_Type_(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TName(_dollar1, _dollar2, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 184|    vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) "{" separated_nonempty_list(";", trait_method_sig) "}" { TraitSig::{vis: $1, name: $3, super_traits: $4, methods: $6} }
fn yy_action_160(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar3)
  guard _args[3].0 is YYObj__immut_list_T_QualifiedName_(_dollar4)
  guard _args[5].0 is YYObj__immut_list_T_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: _dollar4, methods: _dollar6} })
}

// file:///./mbti_parser.mbty
// 234|    qualified_uident { $1 }
fn yy_action_161(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj_QualifiedName({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 302|    STRING { String($1) }
fn yy_action_162(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_Constant({();  String(_dollar1) })
}

// file:///./mbti_parser.mbty
// 142|    uident "::" { Some($1) }
fn yy_action_163(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_Option_Name_({();  Some(_dollar1) })
}

// file:///./mbti_parser.mbty
// 184|    vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) "{" separated_nonempty_list(";", trait_method_sig) "}" { TraitSig::{vis: $1, name: $3, super_traits: $4, methods: $6} }
fn yy_action_164(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj_Name(_dollar3)
  guard _args[2].0 is YYObj__immut_list_T_QualifiedName_(_dollar4)
  guard _args[4].0 is YYObj__immut_list_T_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: _dollar4, methods: _dollar6} })
}

// file:///./mbti_parser.mbty
// 270|     { Nil }
fn yy_action_165(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_Type_({();  Nil })
}

// file:///./mbti_parser.mbty
// 277|    post_label ":" type_ "=" "_" { Autofill($1, $3) }
fn yy_action_166(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj_Parameter({();  Autofill(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 252|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { TArrow(Cons($3, $5.or(Nil)), $8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_167(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_8(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_51(_args[2].2, _args[3:4])
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _sub_action_1_result is YYObj_Option__immut_list_T_Type__(_dollar5)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, _dollar5.or(Nil)), _dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 189|    vis "typealias" type_decl_name_with_params "=" type_ { TypeAlias(vis=$1, name=$3.0, type_params=$3.1, type_=$5) }
fn yy_action_168(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[3].0 is YYObj_Type(_dollar5)
  YYObj_AliasSig({();  TypeAlias(vis=_dollar1, name=_dollar3.0, type_params=_dollar3.1, type_=_dollar5) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_169(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_Type_(_dollar3)
  YYObj__immut_list_T_Type_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 328|    UIDENT { QualifiedName::{pkg: None, name: $1, loc: mk_loc($sloc)} }
fn yy_action_170(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_QualifiedName({();  QualifiedName::{pkg: None, name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 180|    "impl" uident "::" lident { DefaultImpl(trait_name=$2, method_name=$4) }
fn yy_action_171(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  YYObj_ImplSig({();  DefaultImpl(trait_name=_dollar2, method_name=_dollar4) })
}

// file:///./mbti_parser.mbty
// 107|     { Nil }
fn yy_action_172(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_PackageImport_({();  Nil })
}

// file:///./mbti_parser.mbty
// 174|    vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Variant($5)} }
fn yy_action_173(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[3].0 is YYObj__immut_list_T_ConstrDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Variant(_dollar5)} })
}

// file:///./mbti_parser.mbty
// 252|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { TArrow(Cons($3, $5.or(Nil)), $8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_174(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_8(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_57(_args[2].2, _args[3:3])
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _sub_action_1_result is YYObj_Option__immut_list_T_Type__(_dollar5)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, _dollar5.or(Nil)), _dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 203|    boption("mut") post_label ":" type_ { ConstrParam::{mut_: $1, type_: $4, label: Some($2)} }
fn yy_action_175(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  guard _args[3].0 is YYObj_Type(_dollar4)
  guard _args[1].0 is YYObj_Name(_dollar2)
  YYObj_ConstrParam({();  ConstrParam::{mut_: _dollar1, type_: _dollar4, label: Some(_dollar2)} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_176(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FieldDecl(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_FieldDecl_(_dollar3)
  YYObj__immut_list_T_FieldDecl_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 212|    uident ":" separated_nonempty_list("+", type_constraint) { TypeParamWithConstraints::{name: $1, constraints: $3} }
fn yy_action_177(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_QualifiedName_(_dollar3)
  YYObj_TypeParamWithConstraints({();  TypeParamWithConstraints::{name: _dollar1, constraints: _dollar3} })
}

// file:///./mbti_parser.mbty
// 108|    "import" "(" separated_nonempty_list(";", package_import) ")" ";" { $3 }
fn yy_action_178(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[2].0 is YYObj__immut_list_T_PackageImport_(_dollar3)
  YYObj__immut_list_T_PackageImport_({();  _dollar3 })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_179(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__immut_list_T_QualifiedName_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 275|    post_label ":" type_ { Labelled($1, $3) }
fn yy_action_180(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj_Parameter({();  Labelled(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 123|    func_sig { Func($1) }
fn yy_action_181(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FuncSig(_dollar1)
  YYObj_Sig({();  Func(_dollar1) })
}

// file:///./mbti_parser.mbty
// 229|    { Nil }
fn yy_action_182(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_TypeParamNoConstraints_({();  Nil })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_183(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitMethodParameter(_dollar1)
  YYObj__immut_list_T_TraitMethodParameter_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 173|    vis "struct" type_decl_name_with_params "{" separated_list(";", record_decl_field) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Record($5)} }
fn yy_action_184(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_10(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[5].0 is YYObj__immut_list_T_FieldDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Record(_dollar5)} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_185(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamWithConstraints(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar3)
  YYObj__immut_list_T_TypeParamWithConstraints_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 103|    LIDENT STRING ioption(";") imports sigs EOF { Mbti::{package_name: $2, imports: $4, sigs: $5} }
fn yy_action_186(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_args[1].2, _args[2:3])
  guard _args[1].0 is YYObj_StringLiteral(_dollar2)
  guard _args[3].0 is YYObj__immut_list_T_PackageImport_(_dollar4)
  guard _args[4].0 is YYObj__immut_list_T__Sig__Location__(_dollar5)
  YYObj_Mbti({();  Mbti::{package_name: _dollar2, imports: _dollar4, sigs: _dollar5} })
}

// file:///./mbti_parser.mbty
// 288|    FALSE { Bool(false) }
fn yy_action_187(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Constant({();  Bool(false) })
}

// file:///./stdlib.mbty
// 15|    X { $1 }
fn yy_action_188(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_90(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__immut_list_T_QualifiedName_(_dollar1)
  YYObj__immut_list_T_QualifiedName_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_189(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_TraitMethodParameter_({();  Nil })
}

// file:///./stdlib.mbty
// 14|    { Nil }
fn yy_action_190(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_QualifiedName_({();  Nil })
}

// file:///./mbti_parser.mbty
// 220|    uident { Name($1) }
fn yy_action_191(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_TypeParamNoConstraints({();  Name(_dollar1) })
}

// file:///./stdlib.mbty
// 19|    { false }
fn yy_action_192(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Bool({();  false })
}

fn yy_input(token : Token, _start_pos : Position, _end_pos : Position) -> (YYSymbol, YYObj) {
  match token {
    CHAR(data) => (T_CHAR, YYObj_CharLiteral(data))
    INT(data) => (T_INT, YYObj_String(data))
    BYTE(data) => (T_BYTE, YYObj_CharLiteral(data))
    BYTES(data) => (T_BYTES, YYObj_StringLiteral(data))
    FLOAT(data) => (T_FLOAT, YYObj_String(data))
    STRING(data) => (T_STRING, YYObj_StringLiteral(data))
    MULTILINE_STRING(data) => (T_MULTILINE_STRING, YYObj_String(data))
    MULTILINE_INTERP(data) => (T_MULTILINE_INTERP, YYObj_InterpLiteral(data))
    INTERP(data) => (T_INTERP, YYObj_InterpLiteral(data))
    ATTRIBUTE(data) => (T_ATTRIBUTE, YYObj_String(data))
    LIDENT(data) => (T_LIDENT, YYObj_String(data))
    UIDENT(data) => (T_UIDENT, YYObj_String(data))
    POST_LABEL(data) => (T_POST_LABEL, YYObj_String(data))
    COMMENT(data) => (T_COMMENT, YYObj_Comment(data))
    NEWLINE => (T_NEWLINE, YYObj_Void)
    INFIX1(data) => (T_INFIX1, YYObj_String(data))
    INFIX2(data) => (T_INFIX2, YYObj_String(data))
    INFIX3(data) => (T_INFIX3, YYObj_String(data))
    INFIX4(data) => (T_INFIX4, YYObj_String(data))
    AUGMENTED_ASSIGNMENT(data) => (T_AUGMENTED_ASSIGNMENT, YYObj_String(data))
    EOF => (T_EOF, YYObj_Void)
    FALSE => (T_FALSE, YYObj_Void)
    TRUE => (T_TRUE, YYObj_Void)
    PUB => (T_PUB, YYObj_Void)
    PRIV => (T_PRIV, YYObj_Void)
    READONLY => (T_READONLY, YYObj_Void)
    IMPORT => (T_IMPORT, YYObj_Void)
    EXTERN => (T_EXTERN, YYObj_Void)
    BREAK => (T_BREAK, YYObj_Void)
    CONTINUE => (T_CONTINUE, YYObj_Void)
    STRUCT => (T_STRUCT, YYObj_Void)
    ENUM => (T_ENUM, YYObj_Void)
    TRAIT => (T_TRAIT, YYObj_Void)
    DERIVE => (T_DERIVE, YYObj_Void)
    IMPL => (T_IMPL, YYObj_Void)
    WITH => (T_WITH, YYObj_Void)
    RAISE => (T_RAISE, YYObj_Void)
    THROW => (T_THROW, YYObj_Void)
    TRY => (T_TRY, YYObj_Void)
    CATCH => (T_CATCH, YYObj_Void)
    ASYNC => (T_ASYNC, YYObj_Void)
    TYPEALIAS => (T_TYPEALIAS, YYObj_Void)
    TRAITALIAS => (T_TRAITALIAS, YYObj_Void)
    FNALIAS => (T_FNALIAS, YYObj_Void)
    EQUAL => (T_EQUAL, YYObj_Void)
    LPAREN => (T_LPAREN, YYObj_Void)
    RPAREN => (T_RPAREN, YYObj_Void)
    COMMA => (T_COMMA, YYObj_Void)
    MINUS => (T_MINUS, YYObj_Void)
    QUESTION => (T_QUESTION, YYObj_Void)
    EXCLAMATION => (T_EXCLAMATION, YYObj_Void)
    DOT_LIDENT(data) => (T_DOT_LIDENT, YYObj_String(data))
    DOT_UIDENT(data) => (T_DOT_UIDENT, YYObj_String(data))
    DOT_INT(data) => (T_DOT_INT, YYObj_Int(data))
    DOT_LPAREN => (T_DOT_LPAREN, YYObj_Void)
    COLONCOLON => (T_COLONCOLON, YYObj_Void)
    COLON => (T_COLON, YYObj_Void)
    SEMI(data) => (T_SEMI, YYObj_Bool(data))
    LBRACKET => (T_LBRACKET, YYObj_Void)
    PLUS => (T_PLUS, YYObj_Void)
    RBRACKET => (T_RBRACKET, YYObj_Void)
    UNDERSCORE => (T_UNDERSCORE, YYObj_Void)
    BAR => (T_BAR, YYObj_Void)
    LBRACE => (T_LBRACE, YYObj_Void)
    RBRACE => (T_RBRACE, YYObj_Void)
    AMPERAMPER => (T_AMPERAMPER, YYObj_Void)
    AMPER => (T_AMPER, YYObj_Void)
    CARET => (T_CARET, YYObj_Void)
    BARBAR => (T_BARBAR, YYObj_Void)
    PACKAGE_NAME(data) => (T_PACKAGE_NAME, YYObj_String(data))
    AS => (T_AS, YYObj_Void)
    PIPE => (T_PIPE, YYObj_Void)
    ELSE => (T_ELSE, YYObj_Void)
    FN => (T_FN, YYObj_Void)
    IF => (T_IF, YYObj_Void)
    LET => (T_LET, YYObj_Void)
    CONST => (T_CONST, YYObj_Void)
    MATCH => (T_MATCH, YYObj_Void)
    MUTABLE => (T_MUTABLE, YYObj_Void)
    TYPE => (T_TYPE, YYObj_Void)
    FAT_ARROW => (T_FAT_ARROW, YYObj_Void)
    THIN_ARROW => (T_THIN_ARROW, YYObj_Void)
    WHILE => (T_WHILE, YYObj_Void)
    RETURN => (T_RETURN, YYObj_Void)
    DOTDOT => (T_DOTDOT, YYObj_Void)
    RANGE_INCLUSIVE => (T_RANGE_INCLUSIVE, YYObj_Void)
    RANGE_EXCLUSIVE => (T_RANGE_EXCLUSIVE, YYObj_Void)
    ELLIPSIS => (T_ELLIPSIS, YYObj_Void)
    TEST => (T_TEST, YYObj_Void)
    LOOP => (T_LOOP, YYObj_Void)
    GUARD => (T_GUARD, YYObj_Void)
    FOR => (T_FOR, YYObj_Void)
    IN => (T_IN, YYObj_Void)
    IS => (T_IS, YYObj_Void)
  }
}

// [0, mbti → • LIDENT STRING imports sigs EOF, $]
// [1, mbti → • LIDENT STRING SEMI imports sigs EOF, $]
// [180, mbti_prime → • mbti, $]
fn yy_state_0(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_mbti => Shift(yy_state_1)
    T_LIDENT => Shift(yy_state_2)
    _ => Error
  }
}

// [180, mbti_prime → mbti •, $]
fn yy_state_1(_lookahead : YYSymbol) -> YYDecision {
  Accept
}

// [0, mbti → LIDENT • STRING imports sigs EOF, $]
// [1, mbti → LIDENT • STRING SEMI imports sigs EOF, $]
fn yy_state_2(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_3)
    _ => Error
  }
}

// [0, mbti → LIDENT STRING • imports sigs EOF, $]
// [1, mbti → LIDENT STRING • SEMI imports sigs EOF, $]
// [2, imports → •, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
// [3, imports → • IMPORT LPAREN separated_nonempty_list(SEMI,package_import) RPAREN SEMI, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
fn yy_state_3(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_IMPORT => Shift(yy_state_4)
    T_SEMI => Shift(yy_state_15)
    NT_imports => Shift(yy_state_353)
    T_ATTRIBUTE | T_EOF | T_PUB | T_PRIV | T_EXTERN | T_STRUCT | T_ENUM | T_TRAIT | T_IMPL | T_TYPEALIAS | T_TRAITALIAS | T_FN | T_LET | T_CONST | T_TYPE => Reduce(0, NT_imports, yy_action_172)
    _ => Error
  }
}

// [3, imports → IMPORT • LPAREN separated_nonempty_list(SEMI,package_import) RPAREN SEMI, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
fn yy_state_4(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_5)
    _ => Error
  }
}

// [3, imports → IMPORT LPAREN • separated_nonempty_list(SEMI,package_import) RPAREN SEMI, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
// [4, package_import → • STRING, RPAREN / SEMI]
// [5, package_import → • STRING AS LIDENT, RPAREN / SEMI]
// [126, separated_nonempty_list(SEMI,package_import) → • package_import, RPAREN]
// [127, separated_nonempty_list(SEMI,package_import) → • package_import SEMI separated_nonempty_list(SEMI,package_import), RPAREN]
fn yy_state_5(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_package_import => Shift(yy_state_6)
    T_STRING => Shift(yy_state_9)
    NT_separated_nonempty_list_SEMI_package_import_ => Shift(yy_state_12)
    _ => Error
  }
}

// [126, separated_nonempty_list(SEMI,package_import) → package_import •, RPAREN]
// [127, separated_nonempty_list(SEMI,package_import) → package_import • SEMI separated_nonempty_list(SEMI,package_import), RPAREN]
fn yy_state_6(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_7)
    T_RPAREN => Reduce(1, NT_separated_nonempty_list_SEMI_package_import_, yy_action_41)
    _ => Error
  }
}

// [4, package_import → • STRING, RPAREN / SEMI]
// [5, package_import → • STRING AS LIDENT, RPAREN / SEMI]
// [126, separated_nonempty_list(SEMI,package_import) → • package_import, RPAREN]
// [127, separated_nonempty_list(SEMI,package_import) → • package_import SEMI separated_nonempty_list(SEMI,package_import), RPAREN]
// [127, separated_nonempty_list(SEMI,package_import) → package_import SEMI • separated_nonempty_list(SEMI,package_import), RPAREN]
fn yy_state_7(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_package_import => Shift(yy_state_6)
    NT_separated_nonempty_list_SEMI_package_import_ => Shift(yy_state_8)
    T_STRING => Shift(yy_state_9)
    _ => Error
  }
}

// [127, separated_nonempty_list(SEMI,package_import) → package_import SEMI separated_nonempty_list(SEMI,package_import) •, RPAREN]
fn yy_state_8(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_package_import_, yy_action_72)
}

// [4, package_import → STRING •, RPAREN / SEMI]
// [5, package_import → STRING • AS LIDENT, RPAREN / SEMI]
fn yy_state_9(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_10)
    T_RPAREN | T_SEMI => Reduce(1, NT_package_import, yy_action_52)
    _ => Error
  }
}

// [5, package_import → STRING AS • LIDENT, RPAREN / SEMI]
fn yy_state_10(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_11)
    _ => Error
  }
}

// [5, package_import → STRING AS LIDENT •, RPAREN / SEMI]
fn yy_state_11(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_package_import, yy_action_106)
}

// [3, imports → IMPORT LPAREN separated_nonempty_list(SEMI,package_import) • RPAREN SEMI, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
fn yy_state_12(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_13)
    _ => Error
  }
}

// [3, imports → IMPORT LPAREN separated_nonempty_list(SEMI,package_import) RPAREN • SEMI, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
fn yy_state_13(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_14)
    _ => Error
  }
}

// [3, imports → IMPORT LPAREN separated_nonempty_list(SEMI,package_import) RPAREN SEMI •, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
fn yy_state_14(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_imports, yy_action_178)
}

// [1, mbti → LIDENT STRING SEMI • imports sigs EOF, $]
// [2, imports → •, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
// [3, imports → • IMPORT LPAREN separated_nonempty_list(SEMI,package_import) RPAREN SEMI, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
fn yy_state_15(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_IMPORT => Shift(yy_state_4)
    NT_imports => Shift(yy_state_16)
    T_ATTRIBUTE | T_EOF | T_PUB | T_PRIV | T_EXTERN | T_STRUCT | T_ENUM | T_TRAIT | T_IMPL | T_TYPEALIAS | T_TRAITALIAS | T_FN | T_LET | T_CONST | T_TYPE => Reduce(0, NT_imports, yy_action_172)
    _ => Error
  }
}

// [1, mbti → LIDENT STRING SEMI imports • sigs EOF, $]
// [6, sigs → •, EOF]
// [7, sigs → • sig_, EOF]
// [8, sigs → • sig_ SEMI sigs, EOF]
// [9, sig_ → • func_sig, EOF / SEMI]
// [10, sig_ → • type_sig, EOF / SEMI]
// [11, sig_ → • alias_sig, EOF / SEMI]
// [12, sig_ → • trait_sig, EOF / SEMI]
// [13, sig_ → • impl_sig, EOF / SEMI]
// [14, sig_ → • const_sig, EOF / SEMI]
// [15, sig_ → • value_sig, EOF / SEMI]
// [16, const_sig → • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [17, value_sig → • LET lident COLON type_, EOF / SEMI]
// [20, func_sig_no_attr → • FN loption(type_params_with_constraints) method_prefix lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [21, func_sig → • func_sig_no_attr, EOF / SEMI]
// [22, func_sig → • nonempty_list(ATTRIBUTE) func_sig_no_attr, EOF / SEMI]
// [25, type_sig → • EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [26, type_sig → • PRIV EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [27, type_sig → • PUB pub_attr EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [28, type_sig → • TYPE type_decl_name_with_params, EOF / SEMI]
// [29, type_sig → • PRIV TYPE type_decl_name_with_params, EOF / SEMI]
// [30, type_sig → • PUB pub_attr TYPE type_decl_name_with_params, EOF / SEMI]
// [31, type_sig → • TYPE type_decl_name_with_params type_, EOF / SEMI]
// [32, type_sig → • PRIV TYPE type_decl_name_with_params type_, EOF / SEMI]
// [33, type_sig → • PUB pub_attr TYPE type_decl_name_with_params type_, EOF / SEMI]
// [34, type_sig → • TYPE EXCLAMATION uident, EOF / SEMI]
// [35, type_sig → • PRIV TYPE EXCLAMATION uident, EOF / SEMI]
// [36, type_sig → • PUB pub_attr TYPE EXCLAMATION uident, EOF / SEMI]
// [37, type_sig → • TYPE EXCLAMATION uident type_, EOF / SEMI]
// [38, type_sig → • PRIV TYPE EXCLAMATION uident type_, EOF / SEMI]
// [39, type_sig → • PUB pub_attr TYPE EXCLAMATION uident type_, EOF / SEMI]
// [40, type_sig → • TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [41, type_sig → • PRIV TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [42, type_sig → • PUB pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [43, type_sig → • STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [44, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [45, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [46, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [47, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [48, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [49, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [50, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [51, impl_sig → • IMPL uident COLONCOLON lident, EOF / SEMI]
// [52, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [53, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [54, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [55, trait_sig → • TRAIT uident, EOF / SEMI]
// [56, trait_sig → • PRIV TRAIT uident, EOF / SEMI]
// [57, trait_sig → • PUB pub_attr TRAIT uident, EOF / SEMI]
// [58, alias_sig → • TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [59, alias_sig → • PRIV TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [60, alias_sig → • PUB pub_attr TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [61, alias_sig → • TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [62, alias_sig → • PRIV TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [63, alias_sig → • PUB pub_attr TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [132, nonempty_list(ATTRIBUTE) → • ATTRIBUTE, FN]
// [133, nonempty_list(ATTRIBUTE) → • ATTRIBUTE nonempty_list(ATTRIBUTE), FN]
fn yy_state_16(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_17)
    T_TRAITALIAS => Shift(yy_state_19)
    T_TYPEALIAS => Shift(yy_state_27)
    T_TRAIT => Shift(yy_state_102)
    T_IMPL => Shift(yy_state_135)
    T_ENUM => Shift(yy_state_161)
    T_STRUCT => Shift(yy_state_187)
    T_TYPE => Shift(yy_state_200)
    T_PUB => Shift(yy_state_209)
    T_PRIV => Shift(yy_state_252)
    T_EXTERN => Shift(yy_state_289)
    NT_nonempty_list_ATTRIBUTE_ => Shift(yy_state_292)
    T_FN => Shift(yy_state_294)
    NT_func_sig_no_attr => Shift(yy_state_322)
    T_LET => Shift(yy_state_323)
    T_CONST => Shift(yy_state_327)
    NT_value_sig => Shift(yy_state_341)
    NT_const_sig => Shift(yy_state_342)
    NT_impl_sig => Shift(yy_state_343)
    NT_trait_sig => Shift(yy_state_344)
    NT_alias_sig => Shift(yy_state_345)
    NT_type_sig => Shift(yy_state_346)
    NT_func_sig => Shift(yy_state_347)
    NT_sig_ => Shift(yy_state_348)
    NT_sigs => Shift(yy_state_351)
    T_EOF => Reduce(0, NT_sigs, yy_action_121)
    _ => Error
  }
}

// [132, nonempty_list(ATTRIBUTE) → • ATTRIBUTE, FN]
// [132, nonempty_list(ATTRIBUTE) → ATTRIBUTE •, FN]
// [133, nonempty_list(ATTRIBUTE) → • ATTRIBUTE nonempty_list(ATTRIBUTE), FN]
// [133, nonempty_list(ATTRIBUTE) → ATTRIBUTE • nonempty_list(ATTRIBUTE), FN]
fn yy_state_17(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_17)
    NT_nonempty_list_ATTRIBUTE_ => Shift(yy_state_18)
    T_FN => Reduce(1, NT_nonempty_list_ATTRIBUTE_, yy_action_114)
    _ => Error
  }
}

// [133, nonempty_list(ATTRIBUTE) → ATTRIBUTE nonempty_list(ATTRIBUTE) •, FN]
fn yy_state_18(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_nonempty_list_ATTRIBUTE_, yy_action_112)
}

// [61, alias_sig → TRAITALIAS • uident EQUAL qualified_uident, EOF / SEMI]
// [122, uident → • UIDENT, EQUAL]
fn yy_state_19(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_21)
    _ => Error
  }
}

// [122, uident → UIDENT •, UIDENT / EOF / ASYNC / EQUAL / LPAREN / COMMA / COLONCOLON / COLON / SEMI / LBRACKET / RBRACKET / UNDERSCORE / LBRACE / RBRACE / AMPER / PACKAGE_NAME]
fn yy_state_20(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_uident, yy_action_88)
}

// [61, alias_sig → TRAITALIAS uident • EQUAL qualified_uident, EOF / SEMI]
fn yy_state_21(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_22)
    _ => Error
  }
}

// [61, alias_sig → TRAITALIAS uident EQUAL • qualified_uident, EOF / SEMI]
// [118, qualified_uident → • UIDENT, EOF / SEMI]
// [119, qualified_uident → • PACKAGE_NAME DOT_UIDENT, EOF / SEMI]
fn yy_state_22(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_qualified_uident => Shift(yy_state_26)
    _ => Error
  }
}

// [119, qualified_uident → PACKAGE_NAME • DOT_UIDENT, EOF / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]
fn yy_state_23(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOT_UIDENT => Shift(yy_state_24)
    _ => Error
  }
}

// [119, qualified_uident → PACKAGE_NAME DOT_UIDENT •, EOF / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]
fn yy_state_24(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_qualified_uident, yy_action_139)
}

// [118, qualified_uident → UIDENT •, EOF / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]
fn yy_state_25(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_qualified_uident, yy_action_23)
}

// [61, alias_sig → TRAITALIAS uident EQUAL qualified_uident •, EOF / SEMI]
fn yy_state_26(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_alias_sig, yy_action_6)
}

// [58, alias_sig → TYPEALIAS • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, EQUAL]
// [122, uident → • UIDENT, EQUAL / LBRACKET]
fn yy_state_27(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_40)
    _ => Error
  }
}

// [73, type_params_no_constraints → • LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
// [74, optional_type_params_no_constraints → •, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
// [75, optional_type_params_no_constraints → • type_params_no_constraints, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
// [77, type_decl_name_with_params → uident • optional_type_params_no_constraints, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_28(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_optional_type_params_no_constraints => Shift(yy_state_29)
    NT_type_params_no_constraints => Shift(yy_state_30)
    T_LBRACKET => Shift(yy_state_31)
    T_UIDENT | T_EOF | T_ASYNC | T_EQUAL | T_LPAREN | T_SEMI | T_UNDERSCORE | T_LBRACE | T_AMPER | T_PACKAGE_NAME => Reduce(0, NT_optional_type_params_no_constraints, yy_action_182)
    _ => Error
  }
}

// [77, type_decl_name_with_params → uident optional_type_params_no_constraints •, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_29(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_type_decl_name_with_params, yy_action_138)
}

// [75, optional_type_params_no_constraints → type_params_no_constraints •, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_30(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_optional_type_params_no_constraints, yy_action_127)
}

// [71, type_param_no_constraints → • uident, COMMA / RBRACKET]
// [72, type_param_no_constraints → • UNDERSCORE, COMMA / RBRACKET]
// [73, type_params_no_constraints → LBRACKET • separated_list(COMMA,type_param_no_constraints) RBRACKET, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
// [122, uident → • UIDENT, COMMA / RBRACKET]
// [162, separated_list(COMMA,type_param_no_constraints) → •, RBRACKET]
// [163, separated_list(COMMA,type_param_no_constraints) → • separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
// [178, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints, RBRACKET]
// [179, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
fn yy_state_31(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_type_param_no_constraints => Shift(yy_state_32)
    T_UNDERSCORE => Shift(yy_state_35)
    NT_uident => Shift(yy_state_36)
    NT_separated_nonempty_list_COMMA_type_param_no_constraints_ => Shift(yy_state_37)
    NT_separated_list_COMMA_type_param_no_constraints_ => Shift(yy_state_38)
    T_RBRACKET => Reduce(0, NT_separated_list_COMMA_type_param_no_constraints_, yy_action_55)
    _ => Error
  }
}

// [178, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints •, RBRACKET]
// [179, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints • COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
fn yy_state_32(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_33)
    T_RBRACKET => Reduce(1, NT_separated_nonempty_list_COMMA_type_param_no_constraints_, yy_action_153)
    _ => Error
  }
}

// [71, type_param_no_constraints → • uident, COMMA / RBRACKET]
// [72, type_param_no_constraints → • UNDERSCORE, COMMA / RBRACKET]
// [122, uident → • UIDENT, COMMA / RBRACKET]
// [178, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints, RBRACKET]
// [179, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
// [179, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints COMMA • separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
fn yy_state_33(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_type_param_no_constraints => Shift(yy_state_32)
    NT_separated_nonempty_list_COMMA_type_param_no_constraints_ => Shift(yy_state_34)
    T_UNDERSCORE => Shift(yy_state_35)
    NT_uident => Shift(yy_state_36)
    _ => Error
  }
}

// [179, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints) •, RBRACKET]
fn yy_state_34(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_type_param_no_constraints_, yy_action_133)
}

// [72, type_param_no_constraints → UNDERSCORE •, COMMA / RBRACKET]
fn yy_state_35(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_param_no_constraints, yy_action_92)
}

// [71, type_param_no_constraints → uident •, COMMA / RBRACKET]
fn yy_state_36(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_param_no_constraints, yy_action_191)
}

// [163, separated_list(COMMA,type_param_no_constraints) → separated_nonempty_list(COMMA,type_param_no_constraints) •, RBRACKET]
fn yy_state_37(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_type_param_no_constraints_, yy_action_56)
}

// [73, type_params_no_constraints → LBRACKET separated_list(COMMA,type_param_no_constraints) • RBRACKET, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_38(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_39)
    _ => Error
  }
}

// [73, type_params_no_constraints → LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET •, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_39(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_params_no_constraints, yy_action_105)
}

// [58, alias_sig → TYPEALIAS type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_40(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_41)
    _ => Error
  }
}

// [58, alias_sig → TYPEALIAS type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_41(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_101)
    _ => Error
  }
}

// [121, qualified_uident_ → PACKAGE_NAME • DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / PLUS / RBRACKET / RBRACE]
fn yy_state_42(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOT_UIDENT => Shift(yy_state_43)
    _ => Error
  }
}

// [121, qualified_uident_ → PACKAGE_NAME DOT_UIDENT •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / PLUS / RBRACKET / RBRACE]
fn yy_state_43(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_qualified_uident_, yy_action_136)
}

// [120, qualified_uident_ → UIDENT •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / PLUS / RBRACKET / RBRACE]
fn yy_state_44(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_qualified_uident_, yy_action_170)
}

// [85, type_ → ASYNC • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → ASYNC • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → ASYNC • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → ASYNC • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_45(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_46)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [85, type_ → ASYNC LPAREN • type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → ASYNC LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → ASYNC LPAREN • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → ASYNC LPAREN • type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_46(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_RPAREN => Shift(yy_state_47)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_89)
    _ => Error
  }
}

// [89, type_ → ASYNC LPAREN RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_47(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_48)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, simple_type → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, simple_type → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → ASYNC LPAREN RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [94, return_type → • simple_type EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [95, return_type → • simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [96, return_type → • simple_type QUESTION error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_48(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    NT_type_ => Shift(yy_state_49)
    NT_return_type => Shift(yy_state_50)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_63)
    _ => Error
  }
}

// [93, return_type → type_ •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_49(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_return_type, yy_action_61)
}

// [89, type_ → ASYNC LPAREN RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_50(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_, yy_action_99)
}

// [83, simple_type → UNDERSCORE •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_51(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_simple_type, yy_action_54)
}

// [82, simple_type → AMPER • qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_52(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    NT_qualified_uident_ => Shift(yy_state_53)
    _ => Error
  }
}

// [82, simple_type → AMPER qualified_uident_ •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_53(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_simple_type, yy_action_84)
}

// [81, simple_type → qualified_uident_ • optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [98, optional_type_arguments → • LBRACKET separated_nonempty_list(COMMA,type_) RBRACKET, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [99, optional_type_arguments → •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_54(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_55)
    NT_optional_type_arguments => Shift(yy_state_88)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_EXCLAMATION | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(0, NT_optional_type_arguments, yy_action_165)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, COMMA / QUESTION / RBRACKET]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, COMMA / QUESTION / RBRACKET]
// [80, simple_type → • LPAREN type_ RPAREN, COMMA / QUESTION / RBRACKET]
// [81, simple_type → • qualified_uident_ optional_type_arguments, COMMA / QUESTION / RBRACKET]
// [82, simple_type → • AMPER qualified_uident_, COMMA / QUESTION / RBRACKET]
// [83, simple_type → • UNDERSCORE, COMMA / QUESTION / RBRACKET]
// [84, type_ → • simple_type, COMMA / RBRACKET]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [98, optional_type_arguments → LBRACKET • separated_nonempty_list(COMMA,type_) RBRACKET, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, COMMA / QUESTION / LBRACKET / RBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, COMMA / QUESTION / LBRACKET / RBRACKET]
// [164, separated_nonempty_list(COMMA,type_) → • type_, RBRACKET]
// [165, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RBRACKET]
fn yy_state_55(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    NT_type_ => Shift(yy_state_56)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_86)
    _ => Error
  }
}

// [164, separated_nonempty_list(COMMA,type_) → type_ •, RPAREN / RBRACKET]
// [165, separated_nonempty_list(COMMA,type_) → type_ • COMMA separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]
fn yy_state_56(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_57)
    T_RPAREN | T_RBRACKET => Reduce(1, NT_separated_nonempty_list_COMMA_type__, yy_action_31)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION / RBRACKET]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION / RBRACKET]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION / RBRACKET]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION / RBRACKET]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION / RBRACKET]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION / RBRACKET]
// [84, type_ → • simple_type, RPAREN / COMMA / RBRACKET]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET / RBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET / RBRACKET]
// [164, separated_nonempty_list(COMMA,type_) → • type_, RPAREN / RBRACKET]
// [165, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]
// [165, separated_nonempty_list(COMMA,type_) → type_ COMMA • separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]
fn yy_state_57(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    NT_type_ => Shift(yy_state_56)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_58)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    _ => Error
  }
}

// [165, separated_nonempty_list(COMMA,type_) → type_ COMMA separated_nonempty_list(COMMA,type_) •, RPAREN / RBRACKET]
fn yy_state_58(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_type__, yy_action_0)
}

// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, simple_type → LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → LPAREN • type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → LPAREN • type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → LPAREN • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → LPAREN • type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_59(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    T_RPAREN => Shift(yy_state_60)
    NT_type_ => Shift(yy_state_72)
    NT_simple_type => Shift(yy_state_84)
    _ => Error
  }
}

// [90, type_ → LPAREN RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_60(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_61)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, simple_type → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, simple_type → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [90, type_ → LPAREN RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [94, return_type → • simple_type EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [95, return_type → • simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [96, return_type → • simple_type QUESTION error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_61(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    NT_type_ => Shift(yy_state_49)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_return_type => Shift(yy_state_62)
    NT_simple_type => Shift(yy_state_63)
    _ => Error
  }
}

// [90, type_ → LPAREN RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_62(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_, yy_action_7)
}

// [78, simple_type → simple_type • QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → simple_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [94, return_type → simple_type • EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [95, return_type → simple_type • EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [96, return_type → simple_type • QUESTION error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_63(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXCLAMATION => Shift(yy_state_64)
    T_QUESTION => Shift(yy_state_70)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(1, NT_type_, yy_action_4)
    _ => Error
  }
}

// [94, return_type → simple_type EXCLAMATION •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [95, return_type → simple_type EXCLAMATION • separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [97, error_type → • qualified_uident_, EOF / EQUAL / RPAREN / COMMA / SEMI / PLUS / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / SEMI / PLUS / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / SEMI / PLUS / RBRACKET / RBRACE]
// [166, separated_nonempty_list(PLUS,error_type) → • error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [167, separated_nonempty_list(PLUS,error_type) → • error_type PLUS separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_64(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    NT_error_type => Shift(yy_state_65)
    NT_qualified_uident_ => Shift(yy_state_68)
    NT_separated_nonempty_list_PLUS_error_type_ => Shift(yy_state_69)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(2, NT_return_type, yy_action_141)
    _ => Error
  }
}

// [166, separated_nonempty_list(PLUS,error_type) → error_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [167, separated_nonempty_list(PLUS,error_type) → error_type • PLUS separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_65(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS => Shift(yy_state_66)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(1, NT_separated_nonempty_list_PLUS_error_type_, yy_action_38)
    _ => Error
  }
}

// [97, error_type → • qualified_uident_, EOF / EQUAL / RPAREN / COMMA / SEMI / PLUS / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / SEMI / PLUS / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / SEMI / PLUS / RBRACKET / RBRACE]
// [166, separated_nonempty_list(PLUS,error_type) → • error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [167, separated_nonempty_list(PLUS,error_type) → • error_type PLUS separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [167, separated_nonempty_list(PLUS,error_type) → error_type PLUS • separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_66(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    NT_error_type => Shift(yy_state_65)
    NT_separated_nonempty_list_PLUS_error_type_ => Shift(yy_state_67)
    NT_qualified_uident_ => Shift(yy_state_68)
    _ => Error
  }
}

// [167, separated_nonempty_list(PLUS,error_type) → error_type PLUS separated_nonempty_list(PLUS,error_type) •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_67(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_PLUS_error_type_, yy_action_169)
}

// [97, error_type → qualified_uident_ •, EOF / EQUAL / RPAREN / COMMA / SEMI / PLUS / RBRACKET / RBRACE]
fn yy_state_68(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_error_type, yy_action_16)
}

// [95, return_type → simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type) •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_69(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_return_type, yy_action_73)
}

// [78, simple_type → simple_type QUESTION •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [96, return_type → simple_type QUESTION • error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [97, error_type → • qualified_uident_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_70(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    NT_qualified_uident_ => Shift(yy_state_68)
    NT_error_type => Shift(yy_state_71)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_EXCLAMATION | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(2, NT_simple_type, yy_action_145)
    _ => Error
  }
}

// [96, return_type → simple_type QUESTION error_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_71(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_return_type, yy_action_25)
}

// [79, simple_type → LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → LPAREN type_ • RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → LPAREN type_ • COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → LPAREN type_ • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_72(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_73)
    T_COMMA => Shift(yy_state_76)
    _ => Error
  }
}

// [80, simple_type → LPAREN type_ RPAREN •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [92, type_ → LPAREN type_ RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_73(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_74)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_EXCLAMATION | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(3, NT_simple_type, yy_action_91)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, simple_type → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, simple_type → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → LPAREN type_ RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [94, return_type → • simple_type EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [95, return_type → • simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [96, return_type → • simple_type QUESTION error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_74(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    NT_type_ => Shift(yy_state_49)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_63)
    NT_return_type => Shift(yy_state_75)
    _ => Error
  }
}

// [92, type_ → LPAREN type_ RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_75(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_, yy_action_111)
}

// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, simple_type → LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → LPAREN type_ COMMA • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [164, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [165, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
fn yy_state_76(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    NT_type_ => Shift(yy_state_56)
    T_LPAREN => Shift(yy_state_59)
    T_RPAREN => Shift(yy_state_77)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_80)
    NT_simple_type => Shift(yy_state_84)
    _ => Error
  }
}

// [87, type_ → LPAREN type_ COMMA RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_77(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_78)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, simple_type → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, simple_type → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → LPAREN type_ COMMA RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [94, return_type → • simple_type EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [95, return_type → • simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [96, return_type → • simple_type QUESTION error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_78(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    NT_type_ => Shift(yy_state_49)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_63)
    NT_return_type => Shift(yy_state_79)
    _ => Error
  }
}

// [87, type_ → LPAREN type_ COMMA RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_79(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_, yy_action_174)
}

// [79, simple_type → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_80(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_81)
    _ => Error
  }
}

// [79, simple_type → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_81(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_82)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_EXCLAMATION | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(5, NT_simple_type, yy_action_20)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, simple_type → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, simple_type → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [94, return_type → • simple_type EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [95, return_type → • simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [96, return_type → • simple_type QUESTION error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_82(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    NT_type_ => Shift(yy_state_49)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_63)
    NT_return_type => Shift(yy_state_83)
    _ => Error
  }
}

// [88, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_83(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_, yy_action_167)
}

// [78, simple_type → simple_type • QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE]
// [84, type_ → simple_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_84(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_85)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(1, NT_type_, yy_action_4)
    _ => Error
  }
}

// [78, simple_type → simple_type QUESTION •, EOF / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE]
fn yy_state_85(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_simple_type, yy_action_145)
}

// [98, optional_type_arguments → LBRACKET separated_nonempty_list(COMMA,type_) • RBRACKET, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_86(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_87)
    _ => Error
  }
}

// [98, optional_type_arguments → LBRACKET separated_nonempty_list(COMMA,type_) RBRACKET •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_87(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_optional_type_arguments, yy_action_43)
}

// [81, simple_type → qualified_uident_ optional_type_arguments •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_88(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_simple_type, yy_action_159)
}

// [85, type_ → ASYNC LPAREN type_ • COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → ASYNC LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → ASYNC LPAREN type_ • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_89(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_90)
    T_COMMA => Shift(yy_state_93)
    _ => Error
  }
}

// [91, type_ → ASYNC LPAREN type_ RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_90(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_91)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, simple_type → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, simple_type → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → ASYNC LPAREN type_ RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [94, return_type → • simple_type EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [95, return_type → • simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [96, return_type → • simple_type QUESTION error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_91(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    NT_type_ => Shift(yy_state_49)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_63)
    NT_return_type => Shift(yy_state_92)
    _ => Error
  }
}

// [91, type_ → ASYNC LPAREN type_ RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_92(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_, yy_action_102)
}

// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [85, type_ → ASYNC LPAREN type_ COMMA • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → ASYNC LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [164, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [165, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
fn yy_state_93(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    NT_type_ => Shift(yy_state_56)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_94)
    T_RPAREN => Shift(yy_state_98)
    _ => Error
  }
}

// [86, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_94(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_95)
    _ => Error
  }
}

// [86, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_95(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_96)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, simple_type → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, simple_type → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [94, return_type → • simple_type EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [95, return_type → • simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [96, return_type → • simple_type QUESTION error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_96(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    NT_type_ => Shift(yy_state_49)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_63)
    NT_return_type => Shift(yy_state_97)
    _ => Error
  }
}

// [86, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_97(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_, yy_action_137)
}

// [85, type_ → ASYNC LPAREN type_ COMMA RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_98(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_99)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, simple_type → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, simple_type → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [85, type_ → ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [94, return_type → • simple_type EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [95, return_type → • simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [96, return_type → • simple_type QUESTION error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [120, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_99(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    NT_type_ => Shift(yy_state_49)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_63)
    NT_return_type => Shift(yy_state_100)
    _ => Error
  }
}

// [85, type_ → ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_100(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_, yy_action_131)
}

// [58, alias_sig → TYPEALIAS type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
fn yy_state_101(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_alias_sig, yy_action_168)
}

// [52, trait_sig → TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [55, trait_sig → TRAIT • uident, EOF / SEMI]
// [122, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_102(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_103)
    _ => Error
  }
}

// [52, trait_sig → TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [55, trait_sig → TRAIT uident •, EOF / SEMI]
// [146, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, LBRACE]
// [147, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_103(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_104)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_109)
    T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_190)
    T_EOF | T_SEMI => Reduce(2, NT_trait_sig, yy_action_15)
    _ => Error
  }
}

// [118, qualified_uident → • UIDENT, PLUS / LBRACE]
// [119, qualified_uident → • PACKAGE_NAME DOT_UIDENT, PLUS / LBRACE]
// [144, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident, LBRACE]
// [145, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident), LBRACE]
// [147, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → COLON • separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_104(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_separated_nonempty_list_PLUS_qualified_uident_ => Shift(yy_state_105)
    NT_qualified_uident => Shift(yy_state_106)
    _ => Error
  }
}

// [147, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → COLON separated_nonempty_list(PLUS,qualified_uident) •, LBRACE]
fn yy_state_105(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_188)
}

// [144, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident •, LBRACE]
// [145, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident • PLUS separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_106(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS => Shift(yy_state_107)
    T_LBRACE => Reduce(1, NT_separated_nonempty_list_PLUS_qualified_uident_, yy_action_179)
    _ => Error
  }
}

// [118, qualified_uident → • UIDENT, PLUS / LBRACE]
// [119, qualified_uident → • PACKAGE_NAME DOT_UIDENT, PLUS / LBRACE]
// [144, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident, LBRACE]
// [145, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident), LBRACE]
// [145, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident PLUS • separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_107(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_qualified_uident => Shift(yy_state_106)
    NT_separated_nonempty_list_PLUS_qualified_uident_ => Shift(yy_state_108)
    _ => Error
  }
}

// [145, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident) •, LBRACE]
fn yy_state_108(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_PLUS_qualified_uident_, yy_action_149)
}

// [52, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_109(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_110)
    _ => Error
  }
}

// [24, trait_method_sig → • lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [52, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE • separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [123, lident → • LIDENT, LPAREN]
// [148, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [149, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_110(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_trait_method_sig => Shift(yy_state_111)
    T_LIDENT => Shift(yy_state_114)
    NT_lident => Shift(yy_state_115)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_133)
    _ => Error
  }
}

// [148, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig •, RBRACE]
// [149, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig • SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_111(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_112)
    T_RBRACE => Reduce(1, NT_separated_nonempty_list_SEMI_trait_method_sig_, yy_action_144)
    _ => Error
  }
}

// [24, trait_method_sig → • lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [123, lident → • LIDENT, LPAREN]
// [148, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [149, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
// [149, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig SEMI • separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_112(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_trait_method_sig => Shift(yy_state_111)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_113)
    T_LIDENT => Shift(yy_state_114)
    NT_lident => Shift(yy_state_115)
    _ => Error
  }
}

// [149, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig) •, RBRACE]
fn yy_state_113(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_trait_method_sig_, yy_action_74)
}

// [123, lident → LIDENT •, EOF / LPAREN / COLON / SEMI]
fn yy_state_114(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_lident, yy_action_107)
}

// [24, trait_method_sig → lident • LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_115(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_116)
    _ => Error
  }
}

// [24, trait_method_sig → lident LPAREN • separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [105, trait_method_parameter → • type_, RPAREN / COMMA]
// [106, trait_method_parameter → • post_label COLON type_, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [125, post_label → • POST_LABEL, COLON]
// [136, separated_list(COMMA,trait_method_parameter) → •, RPAREN]
// [137, separated_list(COMMA,trait_method_parameter) → • separated_nonempty_list(COMMA,trait_method_parameter), RPAREN]
// [170, separated_nonempty_list(COMMA,trait_method_parameter) → • trait_method_parameter, RPAREN]
// [171, separated_nonempty_list(COMMA,trait_method_parameter) → • trait_method_parameter COMMA separated_nonempty_list(COMMA,trait_method_parameter), RPAREN]
fn yy_state_116(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_trait_method_parameter => Shift(yy_state_117)
    T_POST_LABEL => Shift(yy_state_120)
    NT_post_label => Shift(yy_state_121)
    NT_type_ => Shift(yy_state_124)
    NT_separated_nonempty_list_COMMA_trait_method_parameter_ => Shift(yy_state_125)
    NT_separated_list_COMMA_trait_method_parameter_ => Shift(yy_state_126)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_trait_method_parameter_, yy_action_189)
    _ => Error
  }
}

// [170, separated_nonempty_list(COMMA,trait_method_parameter) → trait_method_parameter •, RPAREN]
// [171, separated_nonempty_list(COMMA,trait_method_parameter) → trait_method_parameter • COMMA separated_nonempty_list(COMMA,trait_method_parameter), RPAREN]
fn yy_state_117(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_118)
    T_RPAREN => Reduce(1, NT_separated_nonempty_list_COMMA_trait_method_parameter_, yy_action_183)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [105, trait_method_parameter → • type_, RPAREN / COMMA]
// [106, trait_method_parameter → • post_label COLON type_, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [125, post_label → • POST_LABEL, COLON]
// [170, separated_nonempty_list(COMMA,trait_method_parameter) → • trait_method_parameter, RPAREN]
// [171, separated_nonempty_list(COMMA,trait_method_parameter) → • trait_method_parameter COMMA separated_nonempty_list(COMMA,trait_method_parameter), RPAREN]
// [171, separated_nonempty_list(COMMA,trait_method_parameter) → trait_method_parameter COMMA • separated_nonempty_list(COMMA,trait_method_parameter), RPAREN]
fn yy_state_118(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_trait_method_parameter => Shift(yy_state_117)
    NT_separated_nonempty_list_COMMA_trait_method_parameter_ => Shift(yy_state_119)
    T_POST_LABEL => Shift(yy_state_120)
    NT_post_label => Shift(yy_state_121)
    NT_type_ => Shift(yy_state_124)
    _ => Error
  }
}

// [171, separated_nonempty_list(COMMA,trait_method_parameter) → trait_method_parameter COMMA separated_nonempty_list(COMMA,trait_method_parameter) •, RPAREN]
fn yy_state_119(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_trait_method_parameter_, yy_action_134)
}

// [125, post_label → POST_LABEL •, COLON]
fn yy_state_120(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_post_label, yy_action_93)
}

// [106, trait_method_parameter → post_label • COLON type_, RPAREN / COMMA]
fn yy_state_121(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_122)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [106, trait_method_parameter → post_label COLON • type_, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_122(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_123)
    _ => Error
  }
}

// [106, trait_method_parameter → post_label COLON type_ •, RPAREN / COMMA]
fn yy_state_123(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_trait_method_parameter, yy_action_11)
}

// [105, trait_method_parameter → type_ •, RPAREN / COMMA]
fn yy_state_124(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_trait_method_parameter, yy_action_35)
}

// [137, separated_list(COMMA,trait_method_parameter) → separated_nonempty_list(COMMA,trait_method_parameter) •, RPAREN]
fn yy_state_125(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_trait_method_parameter_, yy_action_108)
}

// [24, trait_method_sig → lident LPAREN separated_list(COMMA,trait_method_parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_126(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_127)
    _ => Error
  }
}

// [24, trait_method_sig → lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_127(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_128)
    _ => Error
  }
}

// [24, trait_method_sig → lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [78, simple_type → • simple_type QUESTION, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [82, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [83, simple_type → • UNDERSCORE, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [84, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [93, return_type → • type_, EQUAL / SEMI / RBRACE]
// [94, return_type → • simple_type EXCLAMATION, EQUAL / SEMI / RBRACE]
// [95, return_type → • simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type), EQUAL / SEMI / RBRACE]
// [96, return_type → • simple_type QUESTION error_type, EQUAL / SEMI / RBRACE]
// [120, qualified_uident_ → • UIDENT, EQUAL / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACE]
fn yy_state_128(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    NT_type_ => Shift(yy_state_49)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_63)
    NT_return_type => Shift(yy_state_129)
    _ => Error
  }
}

// [24, trait_method_sig → lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [138, boption(eq_underscore) → •, SEMI / RBRACE]
// [139, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]
fn yy_state_129(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_130)
    NT_boption_eq_underscore_ => Shift(yy_state_132)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_152)
    _ => Error
  }
}

// [139, boption(eq_underscore) → EQUAL • UNDERSCORE, SEMI / RBRACE]
fn yy_state_130(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UNDERSCORE => Shift(yy_state_131)
    _ => Error
  }
}

// [139, boption(eq_underscore) → EQUAL UNDERSCORE •, SEMI / RBRACE]
fn yy_state_131(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_boption_eq_underscore_, yy_action_71)
}

// [24, trait_method_sig → lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]
fn yy_state_132(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_trait_method_sig, yy_action_46)
}

// [52, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) • RBRACE, EOF / SEMI]
fn yy_state_133(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_134)
    _ => Error
  }
}

// [52, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE •, EOF / SEMI]
fn yy_state_134(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_trait_sig, yy_action_164)
}

// [49, impl_sig → IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [50, impl_sig → IMPL • qualified_uident FOR type_, EOF / SEMI]
// [51, impl_sig → IMPL • uident COLONCOLON lident, EOF / SEMI]
// [70, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [118, qualified_uident → • UIDENT, FOR]
// [119, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
// [122, uident → • UIDENT, COLONCOLON]
fn yy_state_135(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_136)
    T_LBRACKET => Shift(yy_state_137)
    NT_uident => Shift(yy_state_151)
    NT_qualified_uident => Shift(yy_state_154)
    NT_type_params_with_constraints => Shift(yy_state_157)
    _ => Error
  }
}

// [118, qualified_uident → UIDENT •, FOR]
// [122, uident → UIDENT •, COLONCOLON]
fn yy_state_136(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Reduce(1, NT_qualified_uident, yy_action_23)
    T_COLONCOLON => Reduce(1, NT_uident, yy_action_88)
    _ => Error
  }
}

// [68, type_param_with_constraints → • uident, COMMA / RBRACKET]
// [69, type_param_with_constraints → • uident COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [70, type_params_with_constraints → LBRACKET • separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / PACKAGE_NAME]
// [122, uident → • UIDENT, COMMA / COLON / RBRACKET]
// [160, separated_list(COMMA,type_param_with_constraints) → •, RBRACKET]
// [161, separated_list(COMMA,type_param_with_constraints) → • separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
// [176, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints, RBRACKET]
// [177, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
fn yy_state_137(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_type_param_with_constraints => Shift(yy_state_138)
    NT_uident => Shift(yy_state_141)
    NT_separated_nonempty_list_COMMA_type_param_with_constraints_ => Shift(yy_state_148)
    NT_separated_list_COMMA_type_param_with_constraints_ => Shift(yy_state_149)
    T_RBRACKET => Reduce(0, NT_separated_list_COMMA_type_param_with_constraints_, yy_action_75)
    _ => Error
  }
}

// [176, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints •, RBRACKET]
// [177, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints • COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
fn yy_state_138(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_139)
    T_RBRACKET => Reduce(1, NT_separated_nonempty_list_COMMA_type_param_with_constraints_, yy_action_97)
    _ => Error
  }
}

// [68, type_param_with_constraints → • uident, COMMA / RBRACKET]
// [69, type_param_with_constraints → • uident COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [122, uident → • UIDENT, COMMA / COLON / RBRACKET]
// [176, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints, RBRACKET]
// [177, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
// [177, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints COMMA • separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
fn yy_state_139(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_type_param_with_constraints => Shift(yy_state_138)
    NT_separated_nonempty_list_COMMA_type_param_with_constraints_ => Shift(yy_state_140)
    NT_uident => Shift(yy_state_141)
    _ => Error
  }
}

// [177, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints) •, RBRACKET]
fn yy_state_140(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_type_param_with_constraints_, yy_action_185)
}

// [68, type_param_with_constraints → uident •, COMMA / RBRACKET]
// [69, type_param_with_constraints → uident • COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_141(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_142)
    T_COMMA | T_RBRACKET => Reduce(1, NT_type_param_with_constraints, yy_action_63)
    _ => Error
  }
}

// [69, type_param_with_constraints → uident COLON • separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [76, type_constraint → • qualified_uident, COMMA / PLUS / RBRACKET]
// [118, qualified_uident → • UIDENT, COMMA / PLUS / RBRACKET]
// [119, qualified_uident → • PACKAGE_NAME DOT_UIDENT, COMMA / PLUS / RBRACKET]
// [158, separated_nonempty_list(PLUS,type_constraint) → • type_constraint, COMMA / RBRACKET]
// [159, separated_nonempty_list(PLUS,type_constraint) → • type_constraint PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_142(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_type_constraint => Shift(yy_state_143)
    NT_qualified_uident => Shift(yy_state_146)
    NT_separated_nonempty_list_PLUS_type_constraint_ => Shift(yy_state_147)
    _ => Error
  }
}

// [158, separated_nonempty_list(PLUS,type_constraint) → type_constraint •, COMMA / RBRACKET]
// [159, separated_nonempty_list(PLUS,type_constraint) → type_constraint • PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_143(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS => Shift(yy_state_144)
    T_COMMA | T_RBRACKET => Reduce(1, NT_separated_nonempty_list_PLUS_type_constraint_, yy_action_122)
    _ => Error
  }
}

// [76, type_constraint → • qualified_uident, COMMA / PLUS / RBRACKET]
// [118, qualified_uident → • UIDENT, COMMA / PLUS / RBRACKET]
// [119, qualified_uident → • PACKAGE_NAME DOT_UIDENT, COMMA / PLUS / RBRACKET]
// [158, separated_nonempty_list(PLUS,type_constraint) → • type_constraint, COMMA / RBRACKET]
// [159, separated_nonempty_list(PLUS,type_constraint) → • type_constraint PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [159, separated_nonempty_list(PLUS,type_constraint) → type_constraint PLUS • separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_144(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_type_constraint => Shift(yy_state_143)
    NT_separated_nonempty_list_PLUS_type_constraint_ => Shift(yy_state_145)
    NT_qualified_uident => Shift(yy_state_146)
    _ => Error
  }
}

// [159, separated_nonempty_list(PLUS,type_constraint) → type_constraint PLUS separated_nonempty_list(PLUS,type_constraint) •, COMMA / RBRACKET]
fn yy_state_145(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_PLUS_type_constraint_, yy_action_126)
}

// [76, type_constraint → qualified_uident •, COMMA / PLUS / RBRACKET]
fn yy_state_146(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_constraint, yy_action_161)
}

// [69, type_param_with_constraints → uident COLON separated_nonempty_list(PLUS,type_constraint) •, COMMA / RBRACKET]
fn yy_state_147(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_param_with_constraints, yy_action_177)
}

// [161, separated_list(COMMA,type_param_with_constraints) → separated_nonempty_list(COMMA,type_param_with_constraints) •, RBRACKET]
fn yy_state_148(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_type_param_with_constraints_, yy_action_113)
}

// [70, type_params_with_constraints → LBRACKET separated_list(COMMA,type_param_with_constraints) • RBRACKET, LIDENT / UIDENT / PACKAGE_NAME]
fn yy_state_149(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_150)
    _ => Error
  }
}

// [70, type_params_with_constraints → LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET •, LIDENT / UIDENT / PACKAGE_NAME]
fn yy_state_150(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_params_with_constraints, yy_action_86)
}

// [51, impl_sig → IMPL uident • COLONCOLON lident, EOF / SEMI]
fn yy_state_151(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_152)
    _ => Error
  }
}

// [51, impl_sig → IMPL uident COLONCOLON • lident, EOF / SEMI]
// [123, lident → • LIDENT, EOF / SEMI]
fn yy_state_152(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_114)
    NT_lident => Shift(yy_state_153)
    _ => Error
  }
}

// [51, impl_sig → IMPL uident COLONCOLON lident •, EOF / SEMI]
fn yy_state_153(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_impl_sig, yy_action_171)
}

// [50, impl_sig → IMPL qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_154(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_155)
    _ => Error
  }
}

// [50, impl_sig → IMPL qualified_uident FOR • type_, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_155(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_156)
    _ => Error
  }
}

// [50, impl_sig → IMPL qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_156(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_impl_sig, yy_action_130)
}

// [49, impl_sig → IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [118, qualified_uident → • UIDENT, FOR]
// [119, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_157(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_qualified_uident => Shift(yy_state_158)
    _ => Error
  }
}

// [49, impl_sig → IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_158(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_159)
    _ => Error
  }
}

// [49, impl_sig → IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_159(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_160)
    _ => Error
  }
}

// [49, impl_sig → IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_160(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_impl_sig, yy_action_48)
}

// [46, type_sig → ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [122, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_161(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_162)
    _ => Error
  }
}

// [46, type_sig → ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
fn yy_state_162(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_163)
    _ => Error
  }
}

// [46, type_sig → ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [64, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [122, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [140, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [141, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [173, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_163(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_164)
    NT_uident => Shift(yy_state_167)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_184)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_185)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_95)
    _ => Error
  }
}

// [172, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor •, RBRACE]
// [173, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor • SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_164(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_165)
    T_RBRACE => Reduce(1, NT_separated_nonempty_list_SEMI_enum_constructor_, yy_action_151)
    _ => Error
  }
}

// [64, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [122, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [173, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [173, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor SEMI • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_165(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_164)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_166)
    NT_uident => Shift(yy_state_167)
    _ => Error
  }
}

// [173, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor) •, RBRACE]
fn yy_state_166(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_enum_constructor_, yy_action_103)
}

// [64, enum_constructor → uident • option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [152, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → •, EQUAL / SEMI / RBRACE]
// [153, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → • LPAREN separated_nonempty_list(COMMA,constructor_param) RPAREN, EQUAL / SEMI / RBRACE]
fn yy_state_167(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_168)
    NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__ => Shift(yy_state_180)
    T_EQUAL | T_SEMI | T_RBRACE => Reduce(0, NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__, yy_action_120)
    _ => Error
  }
}

// [65, constructor_param → • boption(MUTABLE) type_, RPAREN / COMMA]
// [66, constructor_param → • boption(MUTABLE) post_label COLON type_, RPAREN / COMMA]
// [150, separated_nonempty_list(COMMA,constructor_param) → • constructor_param, RPAREN]
// [151, separated_nonempty_list(COMMA,constructor_param) → • constructor_param COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [153, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN • separated_nonempty_list(COMMA,constructor_param) RPAREN, EQUAL / SEMI / RBRACE]
// [156, boption(MUTABLE) → •, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
// [157, boption(MUTABLE) → • MUTABLE, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_168(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_169)
    NT_separated_nonempty_list_COMMA_constructor_param_ => Shift(yy_state_170)
    NT_constructor_param => Shift(yy_state_172)
    NT_boption_MUTABLE_ => Shift(yy_state_175)
    T_UIDENT | T_POST_LABEL | T_ASYNC | T_LPAREN | T_UNDERSCORE | T_AMPER | T_PACKAGE_NAME => Reduce(0, NT_boption_MUTABLE_, yy_action_192)
    _ => Error
  }
}

// [157, boption(MUTABLE) → MUTABLE •, LIDENT / UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_169(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_boption_MUTABLE_, yy_action_158)
}

// [153, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN separated_nonempty_list(COMMA,constructor_param) • RPAREN, EQUAL / SEMI / RBRACE]
fn yy_state_170(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_171)
    _ => Error
  }
}

// [153, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN separated_nonempty_list(COMMA,constructor_param) RPAREN •, EQUAL / SEMI / RBRACE]
fn yy_state_171(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__, yy_action_67)
}

// [150, separated_nonempty_list(COMMA,constructor_param) → constructor_param •, RPAREN]
// [151, separated_nonempty_list(COMMA,constructor_param) → constructor_param • COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
fn yy_state_172(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_173)
    T_RPAREN => Reduce(1, NT_separated_nonempty_list_COMMA_constructor_param_, yy_action_1)
    _ => Error
  }
}

// [65, constructor_param → • boption(MUTABLE) type_, RPAREN / COMMA]
// [66, constructor_param → • boption(MUTABLE) post_label COLON type_, RPAREN / COMMA]
// [150, separated_nonempty_list(COMMA,constructor_param) → • constructor_param, RPAREN]
// [151, separated_nonempty_list(COMMA,constructor_param) → • constructor_param COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [151, separated_nonempty_list(COMMA,constructor_param) → constructor_param COMMA • separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [156, boption(MUTABLE) → •, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
// [157, boption(MUTABLE) → • MUTABLE, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_173(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_169)
    NT_constructor_param => Shift(yy_state_172)
    NT_separated_nonempty_list_COMMA_constructor_param_ => Shift(yy_state_174)
    NT_boption_MUTABLE_ => Shift(yy_state_175)
    T_UIDENT | T_POST_LABEL | T_ASYNC | T_LPAREN | T_UNDERSCORE | T_AMPER | T_PACKAGE_NAME => Reduce(0, NT_boption_MUTABLE_, yy_action_192)
    _ => Error
  }
}

// [151, separated_nonempty_list(COMMA,constructor_param) → constructor_param COMMA separated_nonempty_list(COMMA,constructor_param) •, RPAREN]
fn yy_state_174(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_constructor_param_, yy_action_128)
}

// [65, constructor_param → boption(MUTABLE) • type_, RPAREN / COMMA]
// [66, constructor_param → boption(MUTABLE) • post_label COLON type_, RPAREN / COMMA]
// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [125, post_label → • POST_LABEL, COLON]
fn yy_state_175(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    T_POST_LABEL => Shift(yy_state_120)
    NT_post_label => Shift(yy_state_176)
    NT_type_ => Shift(yy_state_179)
    _ => Error
  }
}

// [66, constructor_param → boption(MUTABLE) post_label • COLON type_, RPAREN / COMMA]
fn yy_state_176(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_177)
    _ => Error
  }
}

// [66, constructor_param → boption(MUTABLE) post_label COLON • type_, RPAREN / COMMA]
// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_177(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_178)
    _ => Error
  }
}

// [66, constructor_param → boption(MUTABLE) post_label COLON type_ •, RPAREN / COMMA]
fn yy_state_178(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_constructor_param, yy_action_175)
}

// [65, constructor_param → boption(MUTABLE) type_ •, RPAREN / COMMA]
fn yy_state_179(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_constructor_param, yy_action_147)
}

// [64, enum_constructor → uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) • option(eq_tag), SEMI / RBRACE]
// [154, option(eq_tag) → •, SEMI / RBRACE]
// [155, option(eq_tag) → • EQUAL INT, SEMI / RBRACE]
fn yy_state_180(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_181)
    NT_option_eq_tag_ => Shift(yy_state_183)
    T_SEMI | T_RBRACE => Reduce(0, NT_option_eq_tag_, yy_action_117)
    _ => Error
  }
}

// [155, option(eq_tag) → EQUAL • INT, SEMI / RBRACE]
fn yy_state_181(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_INT => Shift(yy_state_182)
    _ => Error
  }
}

// [155, option(eq_tag) → EQUAL INT •, SEMI / RBRACE]
fn yy_state_182(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_option_eq_tag_, yy_action_79)
}

// [64, enum_constructor → uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag) •, SEMI / RBRACE]
fn yy_state_183(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_enum_constructor, yy_action_76)
}

// [141, separated_list(SEMI,enum_constructor) → separated_nonempty_list(SEMI,enum_constructor) •, RBRACE]
fn yy_state_184(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_SEMI_enum_constructor_, yy_action_148)
}

// [46, type_sig → ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_185(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_186)
    _ => Error
  }
}

// [46, type_sig → ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_186(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_173)
}

// [43, type_sig → STRUCT • type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [122, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_187(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_188)
    _ => Error
  }
}

// [43, type_sig → STRUCT type_decl_name_with_params • LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
fn yy_state_188(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_189)
    _ => Error
  }
}

// [43, type_sig → STRUCT type_decl_name_with_params LBRACE • separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [67, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [142, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [143, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [156, boption(MUTABLE) → •, LIDENT]
// [157, boption(MUTABLE) → • MUTABLE, LIDENT]
// [174, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [175, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_189(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_169)
    NT_record_decl_field => Shift(yy_state_190)
    NT_boption_MUTABLE_ => Shift(yy_state_193)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_197)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_198)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_39)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_192)
    _ => Error
  }
}

// [174, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field •, RBRACE]
// [175, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field • SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_190(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_191)
    T_RBRACE => Reduce(1, NT_separated_nonempty_list_SEMI_record_decl_field_, yy_action_77)
    _ => Error
  }
}

// [67, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [156, boption(MUTABLE) → •, LIDENT]
// [157, boption(MUTABLE) → • MUTABLE, LIDENT]
// [174, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [175, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [175, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field SEMI • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_191(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_169)
    NT_record_decl_field => Shift(yy_state_190)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_192)
    NT_boption_MUTABLE_ => Shift(yy_state_193)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_192)
    _ => Error
  }
}

// [175, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field) •, RBRACE]
fn yy_state_192(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_record_decl_field_, yy_action_176)
}

// [67, record_decl_field → boption(MUTABLE) • lident COLON type_, SEMI / RBRACE]
// [123, lident → • LIDENT, COLON]
fn yy_state_193(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_114)
    NT_lident => Shift(yy_state_194)
    _ => Error
  }
}

// [67, record_decl_field → boption(MUTABLE) lident • COLON type_, SEMI / RBRACE]
fn yy_state_194(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_195)
    _ => Error
  }
}

// [67, record_decl_field → boption(MUTABLE) lident COLON • type_, SEMI / RBRACE]
// [78, simple_type → • simple_type QUESTION, QUESTION / SEMI / RBRACE]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, QUESTION / SEMI / RBRACE]
// [80, simple_type → • LPAREN type_ RPAREN, QUESTION / SEMI / RBRACE]
// [81, simple_type → • qualified_uident_ optional_type_arguments, QUESTION / SEMI / RBRACE]
// [82, simple_type → • AMPER qualified_uident_, QUESTION / SEMI / RBRACE]
// [83, simple_type → • UNDERSCORE, QUESTION / SEMI / RBRACE]
// [84, type_ → • simple_type, SEMI / RBRACE]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [120, qualified_uident_ → • UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]
fn yy_state_195(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_196)
    _ => Error
  }
}

// [67, record_decl_field → boption(MUTABLE) lident COLON type_ •, SEMI / RBRACE]
fn yy_state_196(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_record_decl_field, yy_action_30)
}

// [143, separated_list(SEMI,record_decl_field) → separated_nonempty_list(SEMI,record_decl_field) •, RBRACE]
fn yy_state_197(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_SEMI_record_decl_field_, yy_action_109)
}

// [43, type_sig → STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) • RBRACE, EOF / SEMI]
fn yy_state_198(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_199)
    _ => Error
  }
}

// [43, type_sig → STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE •, EOF / SEMI]
fn yy_state_199(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_18)
}

// [28, type_sig → TYPE • type_decl_name_with_params, EOF / SEMI]
// [31, type_sig → TYPE • type_decl_name_with_params type_, EOF / SEMI]
// [34, type_sig → TYPE • EXCLAMATION uident, EOF / SEMI]
// [37, type_sig → TYPE • EXCLAMATION uident type_, EOF / SEMI]
// [40, type_sig → TYPE • EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / AMPER / PACKAGE_NAME]
// [122, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / LBRACKET / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_200(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    T_EXCLAMATION => Shift(yy_state_201)
    NT_type_decl_name_with_params => Shift(yy_state_207)
    _ => Error
  }
}

// [34, type_sig → TYPE EXCLAMATION • uident, EOF / SEMI]
// [37, type_sig → TYPE EXCLAMATION • uident type_, EOF / SEMI]
// [40, type_sig → TYPE EXCLAMATION • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [122, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_201(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_202)
    _ => Error
  }
}

// [34, type_sig → TYPE EXCLAMATION uident •, EOF / SEMI]
// [37, type_sig → TYPE EXCLAMATION uident • type_, EOF / SEMI]
// [40, type_sig → TYPE EXCLAMATION uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_202(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    T_LBRACE => Shift(yy_state_203)
    NT_type_ => Shift(yy_state_206)
    T_EOF | T_SEMI => Reduce(3, NT_type_sig, yy_action_154)
    _ => Error
  }
}

// [40, type_sig → TYPE EXCLAMATION uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [64, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [122, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [140, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [141, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [173, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_203(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_164)
    NT_uident => Shift(yy_state_167)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_184)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_204)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_95)
    _ => Error
  }
}

// [40, type_sig → TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_204(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_205)
    _ => Error
  }
}

// [40, type_sig → TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_205(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_34)
}

// [37, type_sig → TYPE EXCLAMATION uident type_ •, EOF / SEMI]
fn yy_state_206(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_129)
}

// [28, type_sig → TYPE type_decl_name_with_params •, EOF / SEMI]
// [31, type_sig → TYPE type_decl_name_with_params • type_, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_207(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_208)
    T_EOF | T_SEMI => Reduce(2, NT_type_sig, yy_action_2)
    _ => Error
  }
}

// [31, type_sig → TYPE type_decl_name_with_params type_ •, EOF / SEMI]
fn yy_state_208(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_sig, yy_action_85)
}

// [27, type_sig → PUB • pub_attr EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [30, type_sig → PUB • pub_attr TYPE type_decl_name_with_params, EOF / SEMI]
// [33, type_sig → PUB • pub_attr TYPE type_decl_name_with_params type_, EOF / SEMI]
// [36, type_sig → PUB • pub_attr TYPE EXCLAMATION uident, EOF / SEMI]
// [39, type_sig → PUB • pub_attr TYPE EXCLAMATION uident type_, EOF / SEMI]
// [42, type_sig → PUB • pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [45, type_sig → PUB • pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [48, type_sig → PUB • pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [54, trait_sig → PUB • pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [57, trait_sig → PUB • pub_attr TRAIT uident, EOF / SEMI]
// [60, alias_sig → PUB • pub_attr TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [63, alias_sig → PUB • pub_attr TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [115, pub_attr → •, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
// [116, pub_attr → • LPAREN READONLY RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
// [117, pub_attr → • LPAREN LIDENT RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_209(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_210)
    NT_pub_attr => Shift(yy_state_215)
    T_EXTERN | T_STRUCT | T_ENUM | T_TRAIT | T_TYPEALIAS | T_TRAITALIAS | T_TYPE => Reduce(0, NT_pub_attr, yy_action_87)
    _ => Error
  }
}

// [116, pub_attr → LPAREN • READONLY RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
// [117, pub_attr → LPAREN • LIDENT RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_210(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_211)
    T_READONLY => Shift(yy_state_213)
    _ => Error
  }
}

// [117, pub_attr → LPAREN LIDENT • RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_211(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_212)
    _ => Error
  }
}

// [117, pub_attr → LPAREN LIDENT RPAREN •, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_212(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_pub_attr, yy_action_53)
}

// [116, pub_attr → LPAREN READONLY • RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_213(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_214)
    _ => Error
  }
}

// [116, pub_attr → LPAREN READONLY RPAREN •, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_214(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_pub_attr, yy_action_155)
}

// [27, type_sig → PUB pub_attr • EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [30, type_sig → PUB pub_attr • TYPE type_decl_name_with_params, EOF / SEMI]
// [33, type_sig → PUB pub_attr • TYPE type_decl_name_with_params type_, EOF / SEMI]
// [36, type_sig → PUB pub_attr • TYPE EXCLAMATION uident, EOF / SEMI]
// [39, type_sig → PUB pub_attr • TYPE EXCLAMATION uident type_, EOF / SEMI]
// [42, type_sig → PUB pub_attr • TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [45, type_sig → PUB pub_attr • STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [48, type_sig → PUB pub_attr • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [54, trait_sig → PUB pub_attr • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [57, trait_sig → PUB pub_attr • TRAIT uident, EOF / SEMI]
// [60, alias_sig → PUB pub_attr • TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [63, alias_sig → PUB pub_attr • TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
fn yy_state_215(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAITALIAS => Shift(yy_state_216)
    T_TYPEALIAS => Shift(yy_state_220)
    T_TRAIT => Shift(yy_state_224)
    T_ENUM => Shift(yy_state_230)
    T_STRUCT => Shift(yy_state_235)
    T_TYPE => Shift(yy_state_240)
    T_EXTERN => Shift(yy_state_249)
    _ => Error
  }
}

// [63, alias_sig → PUB pub_attr TRAITALIAS • uident EQUAL qualified_uident, EOF / SEMI]
// [122, uident → • UIDENT, EQUAL]
fn yy_state_216(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_217)
    _ => Error
  }
}

// [63, alias_sig → PUB pub_attr TRAITALIAS uident • EQUAL qualified_uident, EOF / SEMI]
fn yy_state_217(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_218)
    _ => Error
  }
}

// [63, alias_sig → PUB pub_attr TRAITALIAS uident EQUAL • qualified_uident, EOF / SEMI]
// [118, qualified_uident → • UIDENT, EOF / SEMI]
// [119, qualified_uident → • PACKAGE_NAME DOT_UIDENT, EOF / SEMI]
fn yy_state_218(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_qualified_uident => Shift(yy_state_219)
    _ => Error
  }
}

// [63, alias_sig → PUB pub_attr TRAITALIAS uident EQUAL qualified_uident •, EOF / SEMI]
fn yy_state_219(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_116)
}

// [60, alias_sig → PUB pub_attr TYPEALIAS • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, EQUAL]
// [122, uident → • UIDENT, EQUAL / LBRACKET]
fn yy_state_220(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_221)
    _ => Error
  }
}

// [60, alias_sig → PUB pub_attr TYPEALIAS type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_221(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_222)
    _ => Error
  }
}

// [60, alias_sig → PUB pub_attr TYPEALIAS type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_222(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_223)
    _ => Error
  }
}

// [60, alias_sig → PUB pub_attr TYPEALIAS type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
fn yy_state_223(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_123)
}

// [54, trait_sig → PUB pub_attr TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [57, trait_sig → PUB pub_attr TRAIT • uident, EOF / SEMI]
// [122, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_224(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_225)
    _ => Error
  }
}

// [54, trait_sig → PUB pub_attr TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [57, trait_sig → PUB pub_attr TRAIT uident •, EOF / SEMI]
// [146, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, LBRACE]
// [147, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_225(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_104)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_226)
    T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_190)
    T_EOF | T_SEMI => Reduce(4, NT_trait_sig, yy_action_104)
    _ => Error
  }
}

// [54, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_226(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_227)
    _ => Error
  }
}

// [24, trait_method_sig → • lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [54, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE • separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [123, lident → • LIDENT, LPAREN]
// [148, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [149, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_227(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_trait_method_sig => Shift(yy_state_111)
    T_LIDENT => Shift(yy_state_114)
    NT_lident => Shift(yy_state_115)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_228)
    _ => Error
  }
}

// [54, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) • RBRACE, EOF / SEMI]
fn yy_state_228(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_229)
    _ => Error
  }
}

// [54, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE •, EOF / SEMI]
fn yy_state_229(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_trait_sig, yy_action_19)
}

// [48, type_sig → PUB pub_attr ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [122, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_230(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_231)
    _ => Error
  }
}

// [48, type_sig → PUB pub_attr ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
fn yy_state_231(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_232)
    _ => Error
  }
}

// [48, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [64, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [122, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [140, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [141, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [173, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_232(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_164)
    NT_uident => Shift(yy_state_167)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_184)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_233)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_95)
    _ => Error
  }
}

// [48, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_233(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_234)
    _ => Error
  }
}

// [48, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_234(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_22)
}

// [45, type_sig → PUB pub_attr STRUCT • type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [122, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_235(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_236)
    _ => Error
  }
}

// [45, type_sig → PUB pub_attr STRUCT type_decl_name_with_params • LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
fn yy_state_236(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_237)
    _ => Error
  }
}

// [45, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE • separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [67, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [142, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [143, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [156, boption(MUTABLE) → •, LIDENT]
// [157, boption(MUTABLE) → • MUTABLE, LIDENT]
// [174, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [175, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_237(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_169)
    NT_record_decl_field => Shift(yy_state_190)
    NT_boption_MUTABLE_ => Shift(yy_state_193)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_197)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_238)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_39)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_192)
    _ => Error
  }
}

// [45, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) • RBRACE, EOF / SEMI]
fn yy_state_238(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_239)
    _ => Error
  }
}

// [45, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE •, EOF / SEMI]
fn yy_state_239(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_184)
}

// [30, type_sig → PUB pub_attr TYPE • type_decl_name_with_params, EOF / SEMI]
// [33, type_sig → PUB pub_attr TYPE • type_decl_name_with_params type_, EOF / SEMI]
// [36, type_sig → PUB pub_attr TYPE • EXCLAMATION uident, EOF / SEMI]
// [39, type_sig → PUB pub_attr TYPE • EXCLAMATION uident type_, EOF / SEMI]
// [42, type_sig → PUB pub_attr TYPE • EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / AMPER / PACKAGE_NAME]
// [122, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / LBRACKET / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_240(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    T_EXCLAMATION => Shift(yy_state_241)
    NT_type_decl_name_with_params => Shift(yy_state_247)
    _ => Error
  }
}

// [36, type_sig → PUB pub_attr TYPE EXCLAMATION • uident, EOF / SEMI]
// [39, type_sig → PUB pub_attr TYPE EXCLAMATION • uident type_, EOF / SEMI]
// [42, type_sig → PUB pub_attr TYPE EXCLAMATION • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [122, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_241(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_242)
    _ => Error
  }
}

// [36, type_sig → PUB pub_attr TYPE EXCLAMATION uident •, EOF / SEMI]
// [39, type_sig → PUB pub_attr TYPE EXCLAMATION uident • type_, EOF / SEMI]
// [42, type_sig → PUB pub_attr TYPE EXCLAMATION uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_242(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    T_LBRACE => Shift(yy_state_243)
    NT_type_ => Shift(yy_state_246)
    T_EOF | T_SEMI => Reduce(5, NT_type_sig, yy_action_21)
    _ => Error
  }
}

// [42, type_sig → PUB pub_attr TYPE EXCLAMATION uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [64, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [122, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [140, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [141, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [173, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_243(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_164)
    NT_uident => Shift(yy_state_167)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_184)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_244)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_95)
    _ => Error
  }
}

// [42, type_sig → PUB pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_244(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_245)
    _ => Error
  }
}

// [42, type_sig → PUB pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_245(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_28)
}

// [39, type_sig → PUB pub_attr TYPE EXCLAMATION uident type_ •, EOF / SEMI]
fn yy_state_246(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_9)
}

// [30, type_sig → PUB pub_attr TYPE type_decl_name_with_params •, EOF / SEMI]
// [33, type_sig → PUB pub_attr TYPE type_decl_name_with_params • type_, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_247(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_248)
    T_EOF | T_SEMI => Reduce(4, NT_type_sig, yy_action_140)
    _ => Error
  }
}

// [33, type_sig → PUB pub_attr TYPE type_decl_name_with_params type_ •, EOF / SEMI]
fn yy_state_248(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_45)
}

// [27, type_sig → PUB pub_attr EXTERN • TYPE type_decl_name_with_params, EOF / SEMI]
fn yy_state_249(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TYPE => Shift(yy_state_250)
    _ => Error
  }
}

// [27, type_sig → PUB pub_attr EXTERN TYPE • type_decl_name_with_params, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / SEMI]
// [122, uident → • UIDENT, EOF / SEMI / LBRACKET]
fn yy_state_250(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_251)
    _ => Error
  }
}

// [27, type_sig → PUB pub_attr EXTERN TYPE type_decl_name_with_params •, EOF / SEMI]
fn yy_state_251(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_82)
}

// [26, type_sig → PRIV • EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [29, type_sig → PRIV • TYPE type_decl_name_with_params, EOF / SEMI]
// [32, type_sig → PRIV • TYPE type_decl_name_with_params type_, EOF / SEMI]
// [35, type_sig → PRIV • TYPE EXCLAMATION uident, EOF / SEMI]
// [38, type_sig → PRIV • TYPE EXCLAMATION uident type_, EOF / SEMI]
// [41, type_sig → PRIV • TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [44, type_sig → PRIV • STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [47, type_sig → PRIV • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [53, trait_sig → PRIV • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [56, trait_sig → PRIV • TRAIT uident, EOF / SEMI]
// [59, alias_sig → PRIV • TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [62, alias_sig → PRIV • TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
fn yy_state_252(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAITALIAS => Shift(yy_state_253)
    T_TYPEALIAS => Shift(yy_state_257)
    T_TRAIT => Shift(yy_state_261)
    T_ENUM => Shift(yy_state_267)
    T_STRUCT => Shift(yy_state_272)
    T_TYPE => Shift(yy_state_277)
    T_EXTERN => Shift(yy_state_286)
    _ => Error
  }
}

// [62, alias_sig → PRIV TRAITALIAS • uident EQUAL qualified_uident, EOF / SEMI]
// [122, uident → • UIDENT, EQUAL]
fn yy_state_253(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_254)
    _ => Error
  }
}

// [62, alias_sig → PRIV TRAITALIAS uident • EQUAL qualified_uident, EOF / SEMI]
fn yy_state_254(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_255)
    _ => Error
  }
}

// [62, alias_sig → PRIV TRAITALIAS uident EQUAL • qualified_uident, EOF / SEMI]
// [118, qualified_uident → • UIDENT, EOF / SEMI]
// [119, qualified_uident → • PACKAGE_NAME DOT_UIDENT, EOF / SEMI]
fn yy_state_255(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_qualified_uident => Shift(yy_state_256)
    _ => Error
  }
}

// [62, alias_sig → PRIV TRAITALIAS uident EQUAL qualified_uident •, EOF / SEMI]
fn yy_state_256(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_132)
}

// [59, alias_sig → PRIV TYPEALIAS • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, EQUAL]
// [122, uident → • UIDENT, EQUAL / LBRACKET]
fn yy_state_257(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_258)
    _ => Error
  }
}

// [59, alias_sig → PRIV TYPEALIAS type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_258(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_259)
    _ => Error
  }
}

// [59, alias_sig → PRIV TYPEALIAS type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_259(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_260)
    _ => Error
  }
}

// [59, alias_sig → PRIV TYPEALIAS type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
fn yy_state_260(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_78)
}

// [53, trait_sig → PRIV TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [56, trait_sig → PRIV TRAIT • uident, EOF / SEMI]
// [122, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_261(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_262)
    _ => Error
  }
}

// [53, trait_sig → PRIV TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [56, trait_sig → PRIV TRAIT uident •, EOF / SEMI]
// [146, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, LBRACE]
// [147, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_262(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_104)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_263)
    T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_190)
    T_EOF | T_SEMI => Reduce(3, NT_trait_sig, yy_action_94)
    _ => Error
  }
}

// [53, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_263(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_264)
    _ => Error
  }
}

// [24, trait_method_sig → • lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [53, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE • separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [123, lident → • LIDENT, LPAREN]
// [148, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [149, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_264(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_trait_method_sig => Shift(yy_state_111)
    T_LIDENT => Shift(yy_state_114)
    NT_lident => Shift(yy_state_115)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_265)
    _ => Error
  }
}

// [53, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) • RBRACE, EOF / SEMI]
fn yy_state_265(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_266)
    _ => Error
  }
}

// [53, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE •, EOF / SEMI]
fn yy_state_266(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_trait_sig, yy_action_160)
}

// [47, type_sig → PRIV ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [122, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_267(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_268)
    _ => Error
  }
}

// [47, type_sig → PRIV ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
fn yy_state_268(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_269)
    _ => Error
  }
}

// [47, type_sig → PRIV ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [64, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [122, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [140, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [141, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [173, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_269(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_164)
    NT_uident => Shift(yy_state_167)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_184)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_270)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_95)
    _ => Error
  }
}

// [47, type_sig → PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_270(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_271)
    _ => Error
  }
}

// [47, type_sig → PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_271(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_124)
}

// [44, type_sig → PRIV STRUCT • type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [122, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_272(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_273)
    _ => Error
  }
}

// [44, type_sig → PRIV STRUCT type_decl_name_with_params • LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
fn yy_state_273(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_274)
    _ => Error
  }
}

// [44, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE • separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [67, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [142, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [143, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [156, boption(MUTABLE) → •, LIDENT]
// [157, boption(MUTABLE) → • MUTABLE, LIDENT]
// [174, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [175, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_274(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_169)
    NT_record_decl_field => Shift(yy_state_190)
    NT_boption_MUTABLE_ => Shift(yy_state_193)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_197)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_275)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_39)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_192)
    _ => Error
  }
}

// [44, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) • RBRACE, EOF / SEMI]
fn yy_state_275(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_276)
    _ => Error
  }
}

// [44, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE •, EOF / SEMI]
fn yy_state_276(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_64)
}

// [29, type_sig → PRIV TYPE • type_decl_name_with_params, EOF / SEMI]
// [32, type_sig → PRIV TYPE • type_decl_name_with_params type_, EOF / SEMI]
// [35, type_sig → PRIV TYPE • EXCLAMATION uident, EOF / SEMI]
// [38, type_sig → PRIV TYPE • EXCLAMATION uident type_, EOF / SEMI]
// [41, type_sig → PRIV TYPE • EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / AMPER / PACKAGE_NAME]
// [122, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / LBRACKET / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_277(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    T_EXCLAMATION => Shift(yy_state_278)
    NT_type_decl_name_with_params => Shift(yy_state_284)
    _ => Error
  }
}

// [35, type_sig → PRIV TYPE EXCLAMATION • uident, EOF / SEMI]
// [38, type_sig → PRIV TYPE EXCLAMATION • uident type_, EOF / SEMI]
// [41, type_sig → PRIV TYPE EXCLAMATION • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [122, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_278(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_279)
    _ => Error
  }
}

// [35, type_sig → PRIV TYPE EXCLAMATION uident •, EOF / SEMI]
// [38, type_sig → PRIV TYPE EXCLAMATION uident • type_, EOF / SEMI]
// [41, type_sig → PRIV TYPE EXCLAMATION uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_279(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    T_LBRACE => Shift(yy_state_280)
    NT_type_ => Shift(yy_state_283)
    T_EOF | T_SEMI => Reduce(4, NT_type_sig, yy_action_125)
    _ => Error
  }
}

// [41, type_sig → PRIV TYPE EXCLAMATION uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [64, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [122, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [140, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [141, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [173, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_280(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_164)
    NT_uident => Shift(yy_state_167)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_184)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_281)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_95)
    _ => Error
  }
}

// [41, type_sig → PRIV TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_281(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_282)
    _ => Error
  }
}

// [41, type_sig → PRIV TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_282(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_62)
}

// [38, type_sig → PRIV TYPE EXCLAMATION uident type_ •, EOF / SEMI]
fn yy_state_283(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_68)
}

// [29, type_sig → PRIV TYPE type_decl_name_with_params •, EOF / SEMI]
// [32, type_sig → PRIV TYPE type_decl_name_with_params • type_, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_284(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_285)
    T_EOF | T_SEMI => Reduce(3, NT_type_sig, yy_action_143)
    _ => Error
  }
}

// [32, type_sig → PRIV TYPE type_decl_name_with_params type_ •, EOF / SEMI]
fn yy_state_285(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_12)
}

// [26, type_sig → PRIV EXTERN • TYPE type_decl_name_with_params, EOF / SEMI]
fn yy_state_286(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TYPE => Shift(yy_state_287)
    _ => Error
  }
}

// [26, type_sig → PRIV EXTERN TYPE • type_decl_name_with_params, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / SEMI]
// [122, uident → • UIDENT, EOF / SEMI / LBRACKET]
fn yy_state_287(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_288)
    _ => Error
  }
}

// [26, type_sig → PRIV EXTERN TYPE type_decl_name_with_params •, EOF / SEMI]
fn yy_state_288(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_49)
}

// [25, type_sig → EXTERN • TYPE type_decl_name_with_params, EOF / SEMI]
fn yy_state_289(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TYPE => Shift(yy_state_290)
    _ => Error
  }
}

// [25, type_sig → EXTERN TYPE • type_decl_name_with_params, EOF / SEMI]
// [77, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / SEMI]
// [122, uident → • UIDENT, EOF / SEMI / LBRACKET]
fn yy_state_290(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_291)
    _ => Error
  }
}

// [25, type_sig → EXTERN TYPE type_decl_name_with_params •, EOF / SEMI]
fn yy_state_291(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_sig, yy_action_70)
}

// [20, func_sig_no_attr → • FN loption(type_params_with_constraints) method_prefix lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [22, func_sig → nonempty_list(ATTRIBUTE) • func_sig_no_attr, EOF / SEMI]
fn yy_state_292(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_func_sig_no_attr => Shift(yy_state_293)
    T_FN => Shift(yy_state_294)
    _ => Error
  }
}

// [22, func_sig → nonempty_list(ATTRIBUTE) func_sig_no_attr •, EOF / SEMI]
fn yy_state_293(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_func_sig, yy_action_81)
}

// [20, func_sig_no_attr → FN • loption(type_params_with_constraints) method_prefix lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [70, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT]
// [128, loption(type_params_with_constraints) → •, LIDENT / UIDENT]
// [129, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT]
fn yy_state_294(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_137)
    NT_type_params_with_constraints => Shift(yy_state_295)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_296)
    T_LIDENT | T_UIDENT => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_146)
    _ => Error
  }
}

// [129, loption(type_params_with_constraints) → type_params_with_constraints •, LIDENT / UIDENT]
fn yy_state_295(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_loption_type_params_with_constraints_, yy_action_27)
}

// [18, method_prefix → •, LIDENT]
// [19, method_prefix → • uident COLONCOLON, LIDENT]
// [20, func_sig_no_attr → FN loption(type_params_with_constraints) • method_prefix lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [122, uident → • UIDENT, COLONCOLON]
fn yy_state_296(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_method_prefix => Shift(yy_state_297)
    NT_uident => Shift(yy_state_320)
    T_LIDENT => Reduce(0, NT_method_prefix, yy_action_156)
    _ => Error
  }
}

// [20, func_sig_no_attr → FN loption(type_params_with_constraints) method_prefix • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [123, lident → • LIDENT, LPAREN]
fn yy_state_297(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_114)
    NT_lident => Shift(yy_state_298)
    _ => Error
  }
}

// [20, func_sig_no_attr → FN loption(type_params_with_constraints) method_prefix lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_298(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_299)
    _ => Error
  }
}

// [20, func_sig_no_attr → FN loption(type_params_with_constraints) method_prefix lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [100, parameter → • type_, RPAREN / COMMA]
// [101, parameter → • post_label COLON type_, RPAREN / COMMA]
// [102, parameter → • post_label COLON type_ EQUAL DOTDOT, RPAREN / COMMA]
// [103, parameter → • post_label COLON type_ EQUAL UNDERSCORE, RPAREN / COMMA]
// [104, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [124, label → • LIDENT, QUESTION]
// [125, post_label → • POST_LABEL, COLON]
// [130, separated_list(COMMA,parameter) → •, RPAREN]
// [131, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [168, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [169, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_299(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    T_POST_LABEL => Shift(yy_state_120)
    NT_parameter => Shift(yy_state_300)
    T_LIDENT => Shift(yy_state_303)
    NT_label => Shift(yy_state_304)
    NT_post_label => Shift(yy_state_308)
    NT_type_ => Shift(yy_state_314)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_315)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_316)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_142)
    _ => Error
  }
}

// [168, separated_nonempty_list(COMMA,parameter) → parameter •, RPAREN]
// [169, separated_nonempty_list(COMMA,parameter) → parameter • COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_300(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_301)
    T_RPAREN => Reduce(1, NT_separated_nonempty_list_COMMA_parameter_, yy_action_65)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [100, parameter → • type_, RPAREN / COMMA]
// [101, parameter → • post_label COLON type_, RPAREN / COMMA]
// [102, parameter → • post_label COLON type_ EQUAL DOTDOT, RPAREN / COMMA]
// [103, parameter → • post_label COLON type_ EQUAL UNDERSCORE, RPAREN / COMMA]
// [104, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [124, label → • LIDENT, QUESTION]
// [125, post_label → • POST_LABEL, COLON]
// [168, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [169, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
// [169, separated_nonempty_list(COMMA,parameter) → parameter COMMA • separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_301(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    T_POST_LABEL => Shift(yy_state_120)
    NT_parameter => Shift(yy_state_300)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_302)
    T_LIDENT => Shift(yy_state_303)
    NT_label => Shift(yy_state_304)
    NT_post_label => Shift(yy_state_308)
    NT_type_ => Shift(yy_state_314)
    _ => Error
  }
}

// [169, separated_nonempty_list(COMMA,parameter) → parameter COMMA separated_nonempty_list(COMMA,parameter) •, RPAREN]
fn yy_state_302(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_parameter_, yy_action_58)
}

// [124, label → LIDENT •, QUESTION]
fn yy_state_303(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_label, yy_action_24)
}

// [104, parameter → label • QUESTION COLON type_, RPAREN / COMMA]
fn yy_state_304(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_305)
    _ => Error
  }
}

// [104, parameter → label QUESTION • COLON type_, RPAREN / COMMA]
fn yy_state_305(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_306)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [104, parameter → label QUESTION COLON • type_, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_306(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_307)
    _ => Error
  }
}

// [104, parameter → label QUESTION COLON type_ •, RPAREN / COMMA]
fn yy_state_307(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_parameter, yy_action_42)
}

// [101, parameter → post_label • COLON type_, RPAREN / COMMA]
// [102, parameter → post_label • COLON type_ EQUAL DOTDOT, RPAREN / COMMA]
// [103, parameter → post_label • COLON type_ EQUAL UNDERSCORE, RPAREN / COMMA]
fn yy_state_308(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_309)
    _ => Error
  }
}

// [78, simple_type → • simple_type QUESTION, EQUAL / RPAREN / COMMA / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / RPAREN / COMMA / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, EQUAL / RPAREN / COMMA / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / RPAREN / COMMA / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, EQUAL / RPAREN / COMMA / QUESTION]
// [83, simple_type → • UNDERSCORE, EQUAL / RPAREN / COMMA / QUESTION]
// [84, type_ → • simple_type, EQUAL / RPAREN / COMMA]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA]
// [101, parameter → post_label COLON • type_, RPAREN / COMMA]
// [102, parameter → post_label COLON • type_ EQUAL DOTDOT, RPAREN / COMMA]
// [103, parameter → post_label COLON • type_ EQUAL UNDERSCORE, RPAREN / COMMA]
// [120, qualified_uident_ → • UIDENT, EQUAL / RPAREN / COMMA / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_309(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_310)
    _ => Error
  }
}

// [101, parameter → post_label COLON type_ •, RPAREN / COMMA]
// [102, parameter → post_label COLON type_ • EQUAL DOTDOT, RPAREN / COMMA]
// [103, parameter → post_label COLON type_ • EQUAL UNDERSCORE, RPAREN / COMMA]
fn yy_state_310(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_311)
    T_RPAREN | T_COMMA => Reduce(3, NT_parameter, yy_action_180)
    _ => Error
  }
}

// [102, parameter → post_label COLON type_ EQUAL • DOTDOT, RPAREN / COMMA]
// [103, parameter → post_label COLON type_ EQUAL • UNDERSCORE, RPAREN / COMMA]
fn yy_state_311(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UNDERSCORE => Shift(yy_state_312)
    T_DOTDOT => Shift(yy_state_313)
    _ => Error
  }
}

// [103, parameter → post_label COLON type_ EQUAL UNDERSCORE •, RPAREN / COMMA]
fn yy_state_312(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_parameter, yy_action_166)
}

// [102, parameter → post_label COLON type_ EQUAL DOTDOT •, RPAREN / COMMA]
fn yy_state_313(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_parameter, yy_action_135)
}

// [100, parameter → type_ •, RPAREN / COMMA]
fn yy_state_314(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_parameter, yy_action_69)
}

// [131, separated_list(COMMA,parameter) → separated_nonempty_list(COMMA,parameter) •, RPAREN]
fn yy_state_315(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_parameter_, yy_action_26)
}

// [20, func_sig_no_attr → FN loption(type_params_with_constraints) method_prefix lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_316(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_317)
    _ => Error
  }
}

// [20, func_sig_no_attr → FN loption(type_params_with_constraints) method_prefix lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_317(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_318)
    _ => Error
  }
}

// [20, func_sig_no_attr → FN loption(type_params_with_constraints) method_prefix lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / EXCLAMATION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / EXCLAMATION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / EXCLAMATION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / EXCLAMATION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / EXCLAMATION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / EXCLAMATION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [93, return_type → • type_, EOF / SEMI]
// [94, return_type → • simple_type EXCLAMATION, EOF / SEMI]
// [95, return_type → • simple_type EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / SEMI]
// [96, return_type → • simple_type QUESTION error_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / EXCLAMATION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / EXCLAMATION / SEMI / LBRACKET]
fn yy_state_318(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    NT_type_ => Shift(yy_state_49)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_63)
    NT_return_type => Shift(yy_state_319)
    _ => Error
  }
}

// [20, func_sig_no_attr → FN loption(type_params_with_constraints) method_prefix lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_319(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_func_sig_no_attr, yy_action_36)
}

// [19, method_prefix → uident • COLONCOLON, LIDENT]
fn yy_state_320(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_321)
    _ => Error
  }
}

// [19, method_prefix → uident COLONCOLON •, LIDENT]
fn yy_state_321(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_method_prefix, yy_action_163)
}

// [21, func_sig → func_sig_no_attr •, EOF / SEMI]
fn yy_state_322(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_func_sig, yy_action_83)
}

// [17, value_sig → LET • lident COLON type_, EOF / SEMI]
// [123, lident → • LIDENT, COLON]
fn yy_state_323(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_114)
    NT_lident => Shift(yy_state_324)
    _ => Error
  }
}

// [17, value_sig → LET lident • COLON type_, EOF / SEMI]
fn yy_state_324(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_325)
    _ => Error
  }
}

// [17, value_sig → LET lident COLON • type_, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [80, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [82, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [83, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [84, type_ → • simple_type, EOF / SEMI]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_325(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_326)
    _ => Error
  }
}

// [17, value_sig → LET lident COLON type_ •, EOF / SEMI]
fn yy_state_326(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_value_sig, yy_action_150)
}

// [16, const_sig → CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [122, uident → • UIDENT, COLON]
fn yy_state_327(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_328)
    _ => Error
  }
}

// [16, const_sig → CONST uident • COLON type_ EQUAL constant, EOF / SEMI]
fn yy_state_328(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_329)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [78, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [79, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [80, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [81, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [82, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [83, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [84, type_ → • simple_type, EQUAL]
// [85, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [86, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [87, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [88, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [89, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [90, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [91, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [92, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [120, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [121, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]
fn yy_state_329(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_ASYNC => Shift(yy_state_45)
    T_UNDERSCORE => Shift(yy_state_51)
    T_AMPER => Shift(yy_state_52)
    NT_qualified_uident_ => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_59)
    NT_simple_type => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_330)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON type_ • EQUAL constant, EOF / SEMI]
fn yy_state_330(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_331)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [107, constant → • TRUE, EOF / SEMI]
// [108, constant → • FALSE, EOF / SEMI]
// [109, constant → • BYTE, EOF / SEMI]
// [110, constant → • BYTES, EOF / SEMI]
// [111, constant → • CHAR, EOF / SEMI]
// [112, constant → • INT, EOF / SEMI]
// [113, constant → • FLOAT, EOF / SEMI]
// [114, constant → • STRING, EOF / SEMI]
fn yy_state_331(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_332)
    T_FLOAT => Shift(yy_state_333)
    T_INT => Shift(yy_state_334)
    T_CHAR => Shift(yy_state_335)
    T_BYTES => Shift(yy_state_336)
    T_BYTE => Shift(yy_state_337)
    T_FALSE => Shift(yy_state_338)
    T_TRUE => Shift(yy_state_339)
    NT_constant => Shift(yy_state_340)
    _ => Error
  }
}

// [114, constant → STRING •, EOF / SEMI]
fn yy_state_332(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_162)
}

// [113, constant → FLOAT •, EOF / SEMI]
fn yy_state_333(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_17)
}

// [112, constant → INT •, EOF / SEMI]
fn yy_state_334(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_110)
}

// [111, constant → CHAR •, EOF / SEMI]
fn yy_state_335(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_66)
}

// [110, constant → BYTES •, EOF / SEMI]
fn yy_state_336(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_157)
}

// [109, constant → BYTE •, EOF / SEMI]
fn yy_state_337(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_118)
}

// [108, constant → FALSE •, EOF / SEMI]
fn yy_state_338(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_187)
}

// [107, constant → TRUE •, EOF / SEMI]
fn yy_state_339(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_119)
}

// [16, const_sig → CONST uident COLON type_ EQUAL constant •, EOF / SEMI]
fn yy_state_340(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_const_sig, yy_action_89)
}

// [15, sig_ → value_sig •, EOF / SEMI]
fn yy_state_341(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_50)
}

// [14, sig_ → const_sig •, EOF / SEMI]
fn yy_state_342(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_60)
}

// [13, sig_ → impl_sig •, EOF / SEMI]
fn yy_state_343(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_115)
}

// [12, sig_ → trait_sig •, EOF / SEMI]
fn yy_state_344(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_5)
}

// [11, sig_ → alias_sig •, EOF / SEMI]
fn yy_state_345(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_96)
}

// [10, sig_ → type_sig •, EOF / SEMI]
fn yy_state_346(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_101)
}

// [9, sig_ → func_sig •, EOF / SEMI]
fn yy_state_347(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_181)
}

// [7, sigs → sig_ •, EOF]
// [8, sigs → sig_ • SEMI sigs, EOF]
fn yy_state_348(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_349)
    T_EOF => Reduce(1, NT_sigs, yy_action_98)
    _ => Error
  }
}

// [6, sigs → •, EOF]
// [7, sigs → • sig_, EOF]
// [8, sigs → • sig_ SEMI sigs, EOF]
// [8, sigs → sig_ SEMI • sigs, EOF]
// [9, sig_ → • func_sig, EOF / SEMI]
// [10, sig_ → • type_sig, EOF / SEMI]
// [11, sig_ → • alias_sig, EOF / SEMI]
// [12, sig_ → • trait_sig, EOF / SEMI]
// [13, sig_ → • impl_sig, EOF / SEMI]
// [14, sig_ → • const_sig, EOF / SEMI]
// [15, sig_ → • value_sig, EOF / SEMI]
// [16, const_sig → • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [17, value_sig → • LET lident COLON type_, EOF / SEMI]
// [20, func_sig_no_attr → • FN loption(type_params_with_constraints) method_prefix lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [21, func_sig → • func_sig_no_attr, EOF / SEMI]
// [22, func_sig → • nonempty_list(ATTRIBUTE) func_sig_no_attr, EOF / SEMI]
// [25, type_sig → • EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [26, type_sig → • PRIV EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [27, type_sig → • PUB pub_attr EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [28, type_sig → • TYPE type_decl_name_with_params, EOF / SEMI]
// [29, type_sig → • PRIV TYPE type_decl_name_with_params, EOF / SEMI]
// [30, type_sig → • PUB pub_attr TYPE type_decl_name_with_params, EOF / SEMI]
// [31, type_sig → • TYPE type_decl_name_with_params type_, EOF / SEMI]
// [32, type_sig → • PRIV TYPE type_decl_name_with_params type_, EOF / SEMI]
// [33, type_sig → • PUB pub_attr TYPE type_decl_name_with_params type_, EOF / SEMI]
// [34, type_sig → • TYPE EXCLAMATION uident, EOF / SEMI]
// [35, type_sig → • PRIV TYPE EXCLAMATION uident, EOF / SEMI]
// [36, type_sig → • PUB pub_attr TYPE EXCLAMATION uident, EOF / SEMI]
// [37, type_sig → • TYPE EXCLAMATION uident type_, EOF / SEMI]
// [38, type_sig → • PRIV TYPE EXCLAMATION uident type_, EOF / SEMI]
// [39, type_sig → • PUB pub_attr TYPE EXCLAMATION uident type_, EOF / SEMI]
// [40, type_sig → • TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [41, type_sig → • PRIV TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [42, type_sig → • PUB pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [43, type_sig → • STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [44, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [45, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [46, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [47, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [48, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [49, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [50, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [51, impl_sig → • IMPL uident COLONCOLON lident, EOF / SEMI]
// [52, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [53, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [54, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [55, trait_sig → • TRAIT uident, EOF / SEMI]
// [56, trait_sig → • PRIV TRAIT uident, EOF / SEMI]
// [57, trait_sig → • PUB pub_attr TRAIT uident, EOF / SEMI]
// [58, alias_sig → • TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [59, alias_sig → • PRIV TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [60, alias_sig → • PUB pub_attr TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [61, alias_sig → • TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [62, alias_sig → • PRIV TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [63, alias_sig → • PUB pub_attr TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [132, nonempty_list(ATTRIBUTE) → • ATTRIBUTE, FN]
// [133, nonempty_list(ATTRIBUTE) → • ATTRIBUTE nonempty_list(ATTRIBUTE), FN]
fn yy_state_349(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_17)
    T_TRAITALIAS => Shift(yy_state_19)
    T_TYPEALIAS => Shift(yy_state_27)
    T_TRAIT => Shift(yy_state_102)
    T_IMPL => Shift(yy_state_135)
    T_ENUM => Shift(yy_state_161)
    T_STRUCT => Shift(yy_state_187)
    T_TYPE => Shift(yy_state_200)
    T_PUB => Shift(yy_state_209)
    T_PRIV => Shift(yy_state_252)
    T_EXTERN => Shift(yy_state_289)
    NT_nonempty_list_ATTRIBUTE_ => Shift(yy_state_292)
    T_FN => Shift(yy_state_294)
    NT_func_sig_no_attr => Shift(yy_state_322)
    T_LET => Shift(yy_state_323)
    T_CONST => Shift(yy_state_327)
    NT_value_sig => Shift(yy_state_341)
    NT_const_sig => Shift(yy_state_342)
    NT_impl_sig => Shift(yy_state_343)
    NT_trait_sig => Shift(yy_state_344)
    NT_alias_sig => Shift(yy_state_345)
    NT_type_sig => Shift(yy_state_346)
    NT_func_sig => Shift(yy_state_347)
    NT_sig_ => Shift(yy_state_348)
    NT_sigs => Shift(yy_state_350)
    T_EOF => Reduce(0, NT_sigs, yy_action_121)
    _ => Error
  }
}

// [8, sigs → sig_ SEMI sigs •, EOF]
fn yy_state_350(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_sigs, yy_action_40)
}

// [1, mbti → LIDENT STRING SEMI imports sigs • EOF, $]
fn yy_state_351(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EOF => Shift(yy_state_352)
    _ => Error
  }
}

// [1, mbti → LIDENT STRING SEMI imports sigs EOF •, $]
fn yy_state_352(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_mbti, yy_action_186)
}

// [0, mbti → LIDENT STRING imports • sigs EOF, $]
// [6, sigs → •, EOF]
// [7, sigs → • sig_, EOF]
// [8, sigs → • sig_ SEMI sigs, EOF]
// [9, sig_ → • func_sig, EOF / SEMI]
// [10, sig_ → • type_sig, EOF / SEMI]
// [11, sig_ → • alias_sig, EOF / SEMI]
// [12, sig_ → • trait_sig, EOF / SEMI]
// [13, sig_ → • impl_sig, EOF / SEMI]
// [14, sig_ → • const_sig, EOF / SEMI]
// [15, sig_ → • value_sig, EOF / SEMI]
// [16, const_sig → • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [17, value_sig → • LET lident COLON type_, EOF / SEMI]
// [20, func_sig_no_attr → • FN loption(type_params_with_constraints) method_prefix lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [21, func_sig → • func_sig_no_attr, EOF / SEMI]
// [22, func_sig → • nonempty_list(ATTRIBUTE) func_sig_no_attr, EOF / SEMI]
// [25, type_sig → • EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [26, type_sig → • PRIV EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [27, type_sig → • PUB pub_attr EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [28, type_sig → • TYPE type_decl_name_with_params, EOF / SEMI]
// [29, type_sig → • PRIV TYPE type_decl_name_with_params, EOF / SEMI]
// [30, type_sig → • PUB pub_attr TYPE type_decl_name_with_params, EOF / SEMI]
// [31, type_sig → • TYPE type_decl_name_with_params type_, EOF / SEMI]
// [32, type_sig → • PRIV TYPE type_decl_name_with_params type_, EOF / SEMI]
// [33, type_sig → • PUB pub_attr TYPE type_decl_name_with_params type_, EOF / SEMI]
// [34, type_sig → • TYPE EXCLAMATION uident, EOF / SEMI]
// [35, type_sig → • PRIV TYPE EXCLAMATION uident, EOF / SEMI]
// [36, type_sig → • PUB pub_attr TYPE EXCLAMATION uident, EOF / SEMI]
// [37, type_sig → • TYPE EXCLAMATION uident type_, EOF / SEMI]
// [38, type_sig → • PRIV TYPE EXCLAMATION uident type_, EOF / SEMI]
// [39, type_sig → • PUB pub_attr TYPE EXCLAMATION uident type_, EOF / SEMI]
// [40, type_sig → • TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [41, type_sig → • PRIV TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [42, type_sig → • PUB pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [43, type_sig → • STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [44, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [45, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [46, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [47, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [48, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [49, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [50, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [51, impl_sig → • IMPL uident COLONCOLON lident, EOF / SEMI]
// [52, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [53, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [54, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [55, trait_sig → • TRAIT uident, EOF / SEMI]
// [56, trait_sig → • PRIV TRAIT uident, EOF / SEMI]
// [57, trait_sig → • PUB pub_attr TRAIT uident, EOF / SEMI]
// [58, alias_sig → • TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [59, alias_sig → • PRIV TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [60, alias_sig → • PUB pub_attr TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [61, alias_sig → • TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [62, alias_sig → • PRIV TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [63, alias_sig → • PUB pub_attr TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [132, nonempty_list(ATTRIBUTE) → • ATTRIBUTE, FN]
// [133, nonempty_list(ATTRIBUTE) → • ATTRIBUTE nonempty_list(ATTRIBUTE), FN]
fn yy_state_353(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_17)
    T_TRAITALIAS => Shift(yy_state_19)
    T_TYPEALIAS => Shift(yy_state_27)
    T_TRAIT => Shift(yy_state_102)
    T_IMPL => Shift(yy_state_135)
    T_ENUM => Shift(yy_state_161)
    T_STRUCT => Shift(yy_state_187)
    T_TYPE => Shift(yy_state_200)
    T_PUB => Shift(yy_state_209)
    T_PRIV => Shift(yy_state_252)
    T_EXTERN => Shift(yy_state_289)
    NT_nonempty_list_ATTRIBUTE_ => Shift(yy_state_292)
    T_FN => Shift(yy_state_294)
    NT_func_sig_no_attr => Shift(yy_state_322)
    T_LET => Shift(yy_state_323)
    T_CONST => Shift(yy_state_327)
    NT_value_sig => Shift(yy_state_341)
    NT_const_sig => Shift(yy_state_342)
    NT_impl_sig => Shift(yy_state_343)
    NT_trait_sig => Shift(yy_state_344)
    NT_alias_sig => Shift(yy_state_345)
    NT_type_sig => Shift(yy_state_346)
    NT_func_sig => Shift(yy_state_347)
    NT_sig_ => Shift(yy_state_348)
    NT_sigs => Shift(yy_state_354)
    T_EOF => Reduce(0, NT_sigs, yy_action_121)
    _ => Error
  }
}

// [0, mbti → LIDENT STRING imports sigs • EOF, $]
fn yy_state_354(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EOF => Shift(yy_state_355)
    _ => Error
  }
}

// [0, mbti → LIDENT STRING imports sigs EOF •, $]
fn yy_state_355(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_mbti, yy_action_32)
}

fn yy_parse[T](
  tokens : Array[(Token, Position, Position)],
  start : YYState,
  return_ : (YYObj) -> T,
  initial_pos? : Position,
) -> T!ParseError {
  let mut cursor = 0
  let mut state_stack : @immut/list.T[YYState] = Cons(start, Nil)
  let data_stack : Array[(YYObj, Position, Position)] = []
  let mut last_pos = initial_pos.or(tokens[0].1)
  let mut state = start
  let mut lookahead : Option[(YYSymbol, (YYObj, Position, Position), Token?)] = None
  let mut last_shifted_state_stack = state_stack
  while true {
    let decision = match state(EOI) {
      ReduceNoLookahead(_) | Accept as t => t
      _ => {
        match lookahead {
          Some(la) => state(la.0)
          None => {
            if cursor < tokens.length() {
              let (token, start_pos, end_pos) = tokens[cursor]
              cursor += 1
              let (symbol, data) = yy_input(token, start_pos, end_pos)
              lookahead = Some((symbol, (data, start_pos, end_pos), Some(token)))
              state(symbol)
            } else {
              lookahead = Some((EOI, (YYObj_Void, last_pos, last_pos), None))
              state(EOI)
            }
          }
        }
      }
    }
    match decision {
      Accept => return return_(data_stack.unsafe_pop().0)
      Shift(next_state) => {
        guard lookahead is Some(la)
        data_stack.push(la.1)
        state_stack = Cons(next_state, state_stack)
        last_shifted_state_stack = state_stack
        state = next_state
        last_pos = la.1.2
        lookahead = None
      }
      Reduce(count, symbol, action)
      | ReduceNoLookahead(count, symbol, action) => {
        loop (count, symbol, action) {
          _ => {
            let args = data_stack[data_stack.length() - count:]
            let data = action(last_pos, args)
            let (start_pos, end_pos) = if args.length() == 0 {
              (last_pos, last_pos)
            } else {
              (args[0].1, args[args.length() - 1].2)
            }
            for i in 0..<count {
              ignore(data_stack.unsafe_pop())
              state_stack = state_stack.tail()
            }
            state = state_stack.unsafe_head()
            data_stack.push((data, start_pos, end_pos))
            match state(symbol) {
              Accept => return return_(data_stack.unsafe_pop().0)
              Shift(next_state) => {
                state_stack = Cons(next_state, state_stack)
                state = next_state
              }
              Reduce(count, symbol, action)
              | ReduceNoLookahead(count, symbol, action) => continue (count, symbol, action)
              _ => panic()
            }
          }
        }
      }
      Error => {
        let (_, (_, start_pos, end_pos), token) = lookahead.unwrap()
        error!(last_shifted_state_stack, token, (start_pos, end_pos))
      }
    }
  }
  panic()
}

fn error(stack : @immut/list.T[YYState], token : Token?, loc : (Position, Position)) -> Unit!ParseError {
  let expected = []
  fn try_add(symbol : YYSymbol, kind : TokenKind) {
    fn go(stack : @immut/list.T[YYState]) {
      match stack {
        Nil => ()
        Cons(state, _) => {
          match state(symbol) {
            Accept | Shift(_) => expected.push(kind)
            Reduce(count, symbol, _) | ReduceNoLookahead(count, symbol, _) => {
              fn inner_go(stack : @immut/list.T[YYState], count, symbol) {
                let stack = stack.drop(count)
                guard stack is Cons(state, _)
                match state(symbol) {
                  Shift(state) => go(Cons(state, stack))
                  Reduce(count, symbol, _) | ReduceNoLookahead(count, symbol, _) => inner_go(stack, count, symbol)
                  _ => panic()
                }
              }
              inner_go(stack, count, symbol)
            }
            Error => ()
          }
        }
      }
    }
    go(stack)
  }
  for term in ([(T_CHAR, TK_CHAR), (T_INT, TK_INT), (T_BYTE, TK_BYTE), (T_BYTES, TK_BYTES), (T_FLOAT, TK_FLOAT), (T_STRING, TK_STRING), (T_MULTILINE_STRING, TK_MULTILINE_STRING), (T_MULTILINE_INTERP, TK_MULTILINE_INTERP), (T_INTERP, TK_INTERP), (T_ATTRIBUTE, TK_ATTRIBUTE), (T_LIDENT, TK_LIDENT), (T_UIDENT, TK_UIDENT), (T_POST_LABEL, TK_POST_LABEL), (T_COMMENT, TK_COMMENT), (T_NEWLINE, TK_NEWLINE), (T_INFIX1, TK_INFIX1), (T_INFIX2, TK_INFIX2), (T_INFIX3, TK_INFIX3), (T_INFIX4, TK_INFIX4), (T_AUGMENTED_ASSIGNMENT, TK_AUGMENTED_ASSIGNMENT), (T_EOF, TK_EOF), (T_FALSE, TK_FALSE), (T_TRUE, TK_TRUE), (T_PUB, TK_PUB), (T_PRIV, TK_PRIV), (T_READONLY, TK_READONLY), (T_IMPORT, TK_IMPORT), (T_EXTERN, TK_EXTERN), (T_BREAK, TK_BREAK), (T_CONTINUE, TK_CONTINUE), (T_STRUCT, TK_STRUCT), (T_ENUM, TK_ENUM), (T_TRAIT, TK_TRAIT), (T_DERIVE, TK_DERIVE), (T_IMPL, TK_IMPL), (T_WITH, TK_WITH), (T_RAISE, TK_RAISE), (T_THROW, TK_THROW), (T_TRY, TK_TRY), (T_CATCH, TK_CATCH), (T_ASYNC, TK_ASYNC), (T_TYPEALIAS, TK_TYPEALIAS), (T_TRAITALIAS, TK_TRAITALIAS), (T_FNALIAS, TK_FNALIAS), (T_EQUAL, TK_EQUAL), (T_LPAREN, TK_LPAREN), (T_RPAREN, TK_RPAREN), (T_COMMA, TK_COMMA), (T_MINUS, TK_MINUS), (T_QUESTION, TK_QUESTION), (T_EXCLAMATION, TK_EXCLAMATION), (T_DOT_LIDENT, TK_DOT_LIDENT), (T_DOT_UIDENT, TK_DOT_UIDENT), (T_DOT_INT, TK_DOT_INT), (T_DOT_LPAREN, TK_DOT_LPAREN), (T_COLONCOLON, TK_COLONCOLON), (T_COLON, TK_COLON), (T_SEMI, TK_SEMI), (T_LBRACKET, TK_LBRACKET), (T_PLUS, TK_PLUS), (T_RBRACKET, TK_RBRACKET), (T_UNDERSCORE, TK_UNDERSCORE), (T_BAR, TK_BAR), (T_LBRACE, TK_LBRACE), (T_RBRACE, TK_RBRACE), (T_AMPERAMPER, TK_AMPERAMPER), (T_AMPER, TK_AMPER), (T_CARET, TK_CARET), (T_BARBAR, TK_BARBAR), (T_PACKAGE_NAME, TK_PACKAGE_NAME), (T_AS, TK_AS), (T_PIPE, TK_PIPE), (T_ELSE, TK_ELSE), (T_FN, TK_FN), (T_IF, TK_IF), (T_LET, TK_LET), (T_CONST, TK_CONST), (T_MATCH, TK_MATCH), (T_MUTABLE, TK_MUTABLE), (T_TYPE, TK_TYPE), (T_FAT_ARROW, TK_FAT_ARROW), (T_THIN_ARROW, TK_THIN_ARROW), (T_WHILE, TK_WHILE), (T_RETURN, TK_RETURN), (T_DOTDOT, TK_DOTDOT), (T_RANGE_INCLUSIVE, TK_RANGE_INCLUSIVE), (T_RANGE_EXCLUSIVE, TK_RANGE_EXCLUSIVE), (T_ELLIPSIS, TK_ELLIPSIS), (T_TEST, TK_TEST), (T_LOOP, TK_LOOP), (T_GUARD, TK_GUARD), (T_FOR, TK_FOR), (T_IN, TK_IN), (T_IS, TK_IS)] : Array[(YYSymbol, TokenKind)]) {
    try_add(term.0, term.1)
  }
  match token {
    None => raise UnexpectedEndOfInput(loc.1, expected)
    Some(token) => raise UnexpectedToken(token, loc, expected)
  }
}

pub fn mbti(tokens : Array[(Token, Position, Position)], initial_pos? : Position) -> Mbti!ParseError {
  yy_parse!(
    tokens,
    yy_state_0,
    fn {
      YYObj_Mbti(result) => result
      _ => panic()
    },
    initial_pos?,
  )
}

fn _get_symbol_start_pos(args : ArrayView[(YYObj, Position, Position)], last_pos : Position) -> Position {
  if args.length() == 0 {
    last_pos
  } else {
    for i = 0; i < args.length(); i = i + 1 {
      let (_, start_pos, end_pos) = args[i]
      if start_pos == end_pos {
        continue
      }
      return start_pos
    }
    args[args.length() - 1].2
  }
}
