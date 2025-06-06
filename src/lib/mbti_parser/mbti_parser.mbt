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

priv type! YYObj__immut_list_T_FuncSig_ @immut/list.T[FuncSig]

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
  NT_separated_nonempty_list_SEMI_impl_method_sig_
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

// file:///./mbti_parser.mbty
// 167|    vis "type" type_decl_name_with_params type_ { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Newtype($4)} }
fn yy_action_1(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[3].0 is YYObj_Type(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Newtype(_dollar4)} })
}

// file:///./mbti_parser.mbty
// 254|    type_ "!" { ($1, DefaultErrorType(mk_loc($loc($2)))) }
fn yy_action_3(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj__Type__ErrorType_({();  (_dollar1, DefaultErrorType(mk_loc((_start_pos_of_item1, _end_pos_of_item1)))) })
}

// file:///./mbti_parser.mbty
// 132|    value_sig { Value($1) }
fn yy_action_4(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ValueSig(_dollar1)
  YYObj_Sig({();  Value(_dollar1) })
}

// file:///./mbti_parser.mbty
// 243|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { TArrow(Cons($3, $5.or(Nil)), $8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_5(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_6(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_7(_args[3].2, _args[4:5])
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _sub_action_1_result is YYObj_Option__immut_list_T_Type__(_dollar5)
  guard _args[7].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, _dollar5.or(Nil)), _dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 20|    X { true }
fn yy_action_8(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_9(_last_pos, _args[0:2])
  YYObj_Bool({();  true })
}

// file:///./mbti_parser.mbty
// 171|    vis "struct" type_decl_name_with_params "{" separated_list(";", record_decl_field) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Record($5)} }
fn yy_action_10(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[3].0 is YYObj__immut_list_T_FieldDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Record(_dollar5)} })
}

// file:///./mbti_parser.mbty
// 168|    vis "type" "!" uident { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(NoPayload)} }
fn yy_action_12(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(NoPayload)} })
}

// file:///./mbti_parser.mbty
// 286|    INT {
// 287|    match $1 {
// 288|      [.. lit, .. "UL"] => UInt64(lit.to_string())
// 289|      [.. lit, 'L'] => Int64(lit.to_string())
// 290|      [.. lit, 'U'] => UInt(lit.to_string())
// 291|      [.. lit, 'N'] => BigInt(lit.to_string())
// 292|      lit => Int(lit)
// 293|    }
// 294|  }
fn yy_action_14(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
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

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_15(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_Parameter_(_dollar1)
  YYObj__immut_list_T_Parameter_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 188|    vis "typealias" type_decl_name_with_params "=" type_ { TypeAlias(vis=$1, name=$3.0, type_params=$3.1, type_=$5) }
fn yy_action_16(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[4].0 is YYObj_Type(_dollar5)
  YYObj_AliasSig({();  TypeAlias(vis=_dollar1, name=_dollar3.0, type_params=_dollar3.1, type_=_dollar5) })
}

// file:///./mbti_parser.mbty
// 271|    post_label ":" type_ "=" "_" { Autofill($1, $3) }
fn yy_action_17(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj_Parameter({();  Autofill(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 307|    "(" "readonly" ")" { Some("readonly") }
fn yy_action_18(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_String_({();  Some("readonly") })
}

// file:///./mbti_parser.mbty
// 172|    vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Variant($5)} }
fn yy_action_19(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[4].0 is YYObj__immut_list_T_ConstrDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Variant(_dollar5)} })
}

// file:///./mbti_parser.mbty
// 188|    vis "typealias" type_decl_name_with_params "=" type_ { TypeAlias(vis=$1, name=$3.0, type_params=$3.1, type_=$5) }
fn yy_action_20(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[3].0 is YYObj_Type(_dollar5)
  YYObj_AliasSig({();  TypeAlias(vis=_dollar1, name=_dollar3.0, type_params=_dollar3.1, type_=_dollar5) })
}

// file:///./mbti_parser.mbty
// 170|    vis "type" "!" uident "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(EnumPayload($6))} }
fn yy_action_21(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__immut_list_T_ConstrDecl_(_dollar6)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(EnumPayload(_dollar6))} })
}

// file:///./mbti_parser.mbty
// 184|    vis "trait" uident { TraitSig::{vis: $1, name: $3, super_traits: Nil, methods: Nil} }
fn yy_action_22(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj_Name(_dollar3)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: Nil, methods: Nil} })
}

// file:///./mbti_parser.mbty
// 335|    LIDENT { Name::{name: $1, loc: mk_loc($sloc)} }
fn yy_action_23(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  Name::{name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./stdlib.mbty
// 14|    { Nil }
fn yy_action_24(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_TypeParamWithConstraints_({();  Nil })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_25(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_TraitMethodParameter_({();  Nil })
}

// file:///./mbti_parser.mbty
// 270|    post_label ":" type_ "=" ".." { OptionalDefault($1, $3) }
fn yy_action_26(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj_Parameter({();  OptionalDefault(_dollar1, _dollar3) })
}

// file:///./stdlib.mbty
// 44|    { Nil }
fn yy_action_27(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_String_({();  Nil })
}

// file:///./mbti_parser.mbty
// 172|    vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Variant($5)} }
fn yy_action_28(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[3].0 is YYObj__immut_list_T_ConstrDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Variant(_dollar5)} })
}

// file:///./mbti_parser.mbty
// 219|    uident { Name($1) }
fn yy_action_29(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_TypeParamNoConstraints({();  Name(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_30(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__immut_list_T_Type_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 157|    lident delimited("(", separated_list(",", trait_method_parameter), ")") "->" return_type boption(eq_underscore) { TraitMethodSig::{name: $1, params: $2, return_: $4, has_default: $5} }
fn yy_action_31(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_32(_args[0].2, _args[1:4])
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _sub_action_0_result is YYObj__immut_list_T_TraitMethodParameter_(_dollar2)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar4)
  guard _args[6].0 is YYObj_Bool(_dollar5)
  YYObj_TraitMethodSig({();  TraitMethodSig::{name: _dollar1, params: _dollar2, return_: _dollar4, has_default: _dollar5} })
}

// file:///./mbti_parser.mbty
// 171|    vis "struct" type_decl_name_with_params "{" separated_list(";", record_decl_field) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Record($5)} }
fn yy_action_33(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[4].0 is YYObj__immut_list_T_FieldDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Record(_dollar5)} })
}

// file:///./mbti_parser.mbty
// 331|    LIDENT { Name::{name: $1, loc: mk_loc($sloc)} }
fn yy_action_34(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  Name::{name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 285|    CHAR { Char($1) }
fn yy_action_35(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_CharLiteral(_dollar1)
  YYObj_Constant({();  Char(_dollar1) })
}

// file:///./mbti_parser.mbty
// 165|    vis "extern" "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $4.0, type_params: $4.1, components: Extern} }
fn yy_action_36(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4.0, type_params: _dollar4.1, components: Extern} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_37(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FuncSig(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_FuncSig_(_dollar3)
  YYObj__immut_list_T_FuncSig_({();  Cons(_dollar1, _dollar3) })
}

// file:///./stdlib.mbty
// 32|    L X { $2 }
fn yy_action_38(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_QualifiedName_(_dollar2)
  YYObj__immut_list_T_QualifiedName_({();  _dollar2 })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_39(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Parameter(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_Parameter_(_dollar3)
  YYObj__immut_list_T_Parameter_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 128|    alias_sig { Alias($1) }
fn yy_action_40(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_AliasSig(_dollar1)
  YYObj_Sig({();  Alias(_dollar1) })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_41(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_Parameter_(_dollar2)
  YYObj__immut_list_T_Parameter_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 284|    BYTES { Bytes($1) }
fn yy_action_42(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_Constant({();  Bytes(_dollar1) })
}

// file:///./stdlib.mbty
// 5|    X { Some($1) }
fn yy_action_43(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_44(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__String__Location_(_dollar1)
  YYObj_Option__String__Location__({();  Some(_dollar1) })
}

// file:///./mbti_parser.mbty
// 169|    vis "type" "!" uident type_ { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(SinglePayload($5))} }
fn yy_action_45(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj_Type(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(SinglePayload(_dollar5))} })
}

// file:///./mbti_parser.mbty
// 166|    vis "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Abstract} }
fn yy_action_46(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Abstract} })
}

// file:///./mbti_parser.mbty
// 166|    vis "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Abstract} }
fn yy_action_47(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Abstract} })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_48(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Parameter(_dollar1)
  YYObj__immut_list_T_Parameter_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 259|    qualified_uident_ { TName($1, Nil, loc=mk_loc($sloc)) }
fn yy_action_49(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TName(_dollar1, Nil, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 121|    sig_ { Cons(($1, mk_loc($sloc)), Nil) }
fn yy_action_50(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Sig(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj__immut_list_T__Sig__Location__({();  Cons((_dollar1, mk_loc((_symbol_start_pos, _end_pos))), Nil) })
}

// file:///./mbti_parser.mbty
// 323|    PACKAGE_NAME DOT_UIDENT { QualifiedName::{pkg: Some($1), name: $2, loc: mk_loc($sloc)} }
fn yy_action_51(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  guard _args[1].0 is YYObj_String(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_QualifiedName({();  QualifiedName::{pkg: Some(_dollar1), name: _dollar2, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 253|    type_ %prec prec_type { ($1, NoErrorType) }
fn yy_action_52(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__Type__ErrorType_({();  (_dollar1, NoErrorType) })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_53(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_TraitMethodParameter_(_dollar1)
  YYObj__immut_list_T_TraitMethodParameter_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_54(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_FieldDecl_(_dollar1)
  YYObj__immut_list_T_FieldDecl_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 9|    { None }
fn yy_action_55(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Option__immut_list_T_Type__({();  None })
}

// file:///./mbti_parser.mbty
// 176|    "impl" type_params_with_constraints qualified_uident "for" type_ { Trait(type_params=$2, trait_name=$3, type_=$5) }
fn yy_action_56(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar2)
  guard _args[2].0 is YYObj_QualifiedName(_dollar3)
  guard _args[4].0 is YYObj_Type(_dollar5)
  YYObj_ImplSig({();  Trait(type_params=_dollar2, trait_name=_dollar3, type_=_dollar5) })
}

// file:///./stdlib.mbty
// 20|    X { true }
fn yy_action_57(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Bool({();  true })
}

// file:///./stdlib.mbty
// 10|    X { Some($1) }
fn yy_action_7(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_Type_(_dollar1)
  YYObj_Option__immut_list_T_Type__({();  Some(_dollar1) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_58(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrDecl(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_ConstrDecl_(_dollar3)
  YYObj__immut_list_T_ConstrDecl_({();  Cons(_dollar1, _dollar3) })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_59(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_Type_(_dollar2)
  YYObj__immut_list_T_Type_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 255|    type_ "!" separated_nonempty_list("+", error_type) { ($1, ErrorType($3)) }
fn yy_action_60(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_Type_(_dollar3)
  YYObj__Type__ErrorType_({();  (_dollar1, ErrorType(_dollar3)) })
}

// file:///./mbti_parser.mbty
// 140|    "let" lident ":" type_ { ValueSig::{name: $2, type_: $4} }
fn yy_action_61(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[3].0 is YYObj_Type(_dollar4)
  YYObj_ValueSig({();  ValueSig::{name: _dollar2, type_: _dollar4} })
}

// file:///./mbti_parser.mbty
// 313|    { false }
fn yy_action_62(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Bool({();  false })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_63(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_Parameter_({();  Nil })
}

// file:///./mbti_parser.mbty
// 264|     { Nil }
fn yy_action_64(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_Type_({();  Nil })
}

// file:///./mbti_parser.mbty
// 171|    vis "struct" type_decl_name_with_params "{" separated_list(";", record_decl_field) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Record($5)} }
fn yy_action_65(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[5].0 is YYObj__immut_list_T_FieldDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Record(_dollar5)} })
}

// file:///./mbti_parser.mbty
// 220|    "_" { Underscore(mk_loc($sloc)) }
fn yy_action_66(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_TypeParamNoConstraints({();  Underscore(mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 172|    vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Variant($5)} }
fn yy_action_67(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[5].0 is YYObj__immut_list_T_ConstrDecl_(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Variant(_dollar5)} })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_68(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrDecl(_dollar1)
  YYObj__immut_list_T_ConstrDecl_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 302|    "pub" pub_attr { Pub(attr=$2, loc=mk_loc($sloc)) }
fn yy_action_13(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String_(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Visibility({();  Pub(attr=_dollar2, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 312|    "async" { true }
fn yy_action_6(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Bool({();  true })
}

// file:///./mbti_parser.mbty
// 170|    vis "type" "!" uident "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(EnumPayload($6))} }
fn yy_action_69(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[6].0 is YYObj__immut_list_T_ConstrDecl_(_dollar6)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(EnumPayload(_dollar6))} })
}

// file:///./mbti_parser.mbty
// 277|    post_label ":" type_ { Labelled($1, $3) }
fn yy_action_70(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj_TraitMethodParameter({();  Labelled(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 166|    vis "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Abstract} }
fn yy_action_71(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Abstract} })
}

// file:///./stdlib.mbty
// 49|    X { Cons($1, Nil) }
fn yy_action_72(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj__immut_list_T_String_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 269|    post_label ":" type_ { Labelled($1, $3) }
fn yy_action_73(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj_Parameter({();  Labelled(_dollar1, _dollar3) })
}

// file:///./stdlib.mbty
// 4|    { None }
fn yy_action_74(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Option__immut_list_T_ConstrParam__({();  None })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_75(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__immut_list_T_Type_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 178|    "impl" uident "{" separated_nonempty_list(";", impl_method_sig) "}" { Methods(type_name=$2, methods=$4) }
fn yy_action_76(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[3].0 is YYObj__immut_list_T_FuncSig_(_dollar4)
  YYObj_ImplSig({();  Methods(type_name=_dollar2, methods=_dollar4) })
}

// file:///./mbti_parser.mbty
// 206|    boption("mut") lident ":" type_ { FieldDecl::{name: $2, vis: Default, mut_: $1, type_: $4, loc: mk_loc($sloc)} }
fn yy_action_77(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[0].0 is YYObj_Bool(_dollar1)
  guard _args[3].0 is YYObj_Type(_dollar4)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_FieldDecl({();  FieldDecl::{name: _dollar2, vis: Default, mut_: _dollar1, type_: _dollar4, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_78(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamNoConstraints(_dollar1)
  YYObj__immut_list_T_TypeParamNoConstraints_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 197|    "=" INT { ($2, mk_loc($loc($2))) }
fn yy_action_44(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj__String__Location_({();  (_dollar2, mk_loc((_start_pos_of_item1, _end_pos_of_item1))) })
}

// file:///./mbti_parser.mbty
// 189|    vis "traitalias" uident "=" qualified_uident { TraitAlias(vis=$1, name=$3, trait_name=$5) }
fn yy_action_79(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar3)
  guard _args[5].0 is YYObj_QualifiedName(_dollar5)
  YYObj_AliasSig({();  TraitAlias(vis=_dollar1, name=_dollar3, trait_name=_dollar5) })
}

// file:///./mbti_parser.mbty
// 167|    vis "type" type_decl_name_with_params type_ { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Newtype($4)} }
fn yy_action_80(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[2].0 is YYObj_Type(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Newtype(_dollar4)} })
}

// file:///./mbti_parser.mbty
// 165|    vis "extern" "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $4.0, type_params: $4.1, components: Extern} }
fn yy_action_81(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[4].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4.0, type_params: _dollar4.1, components: Extern} })
}

// file:///./mbti_parser.mbty
// 249|    "_" { TAny(mk_loc($sloc)) }
fn yy_action_82(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TAny(mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 168|    vis "type" "!" uident { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(NoPayload)} }
fn yy_action_83(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(NoPayload)} })
}

// file:///./mbti_parser.mbty
// 210|    uident { TypeParamWithConstraints::{name: $1, constraints: Nil} }
fn yy_action_84(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_TypeParamWithConstraints({();  TypeParamWithConstraints::{name: _dollar1, constraints: Nil} })
}

// file:///./mbti_parser.mbty
// 318|    PACKAGE_NAME DOT_UIDENT { QualifiedName::{pkg: Some($1), name: $2, loc: mk_loc($sloc)} }
fn yy_action_85(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  guard _args[1].0 is YYObj_String(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_QualifiedName({();  QualifiedName::{pkg: Some(_dollar1), name: _dollar2, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 228|    { Nil }
fn yy_action_86(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_TypeParamNoConstraints_({();  Nil })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_87(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_PackageImport(_dollar1)
  YYObj__immut_list_T_PackageImport_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 148|    func_sig_no_attr { $1 }
fn yy_action_88(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FuncSig(_dollar1)
  YYObj_FuncSig({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 202|    boption("mut") post_label ":" type_ { ConstrParam::{mut_: $1, type_: $4, label: Some($2)} }
fn yy_action_89(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  guard _args[3].0 is YYObj_Type(_dollar4)
  guard _args[1].0 is YYObj_Name(_dollar2)
  YYObj_ConstrParam({();  ConstrParam::{mut_: _dollar1, type_: _dollar4, label: Some(_dollar2)} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_90(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitMethodSig(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_TraitMethodSig_(_dollar3)
  YYObj__immut_list_T_TraitMethodSig_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 247|    qualified_uident_ optional_type_arguments { TName($1, $2, loc=mk_loc($sloc)) }
fn yy_action_91(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  guard _args[1].0 is YYObj__immut_list_T_Type_(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TName(_dollar1, _dollar2, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_92(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitMethodParameter(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_TraitMethodParameter_(_dollar3)
  YYObj__immut_list_T_TraitMethodParameter_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 233|    qualified_uident { $1 }
fn yy_action_93(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj_QualifiedName({();  _dollar1 })
}

// file:///./stdlib.mbty
// 15|    X { $1 }
fn yy_action_94(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar1)
  YYObj__immut_list_T_TypeParamWithConstraints_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 188|    vis "typealias" type_decl_name_with_params "=" type_ { TypeAlias(vis=$1, name=$3.0, type_params=$3.1, type_=$5) }
fn yy_action_95(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[5].0 is YYObj_Type(_dollar5)
  YYObj_AliasSig({();  TypeAlias(vis=_dollar1, name=_dollar3.0, type_params=_dollar3.1, type_=_dollar5) })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_96(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__immut_list_T_QualifiedName_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 131|    const_sig { Const($1) }
fn yy_action_97(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstSig(_dollar1)
  YYObj_Sig({();  Const(_dollar1) })
}

// file:///./mbti_parser.mbty
// 106|    LIDENT STRING ioption(";") imports sigs EOF { Mbti::{package_name: $2, imports: $4, sigs: $5} }
fn yy_action_98(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_99(_args[1].2, _args[2:2])
  guard _args[1].0 is YYObj_StringLiteral(_dollar2)
  guard _args[2].0 is YYObj__immut_list_T_PackageImport_(_dollar4)
  guard _args[3].0 is YYObj__immut_list_T__Sig__Location__(_dollar5)
  YYObj_Mbti({();  Mbti::{package_name: _dollar2, imports: _dollar4, sigs: _dollar5} })
}

// file:///./mbti_parser.mbty
// 127|    type_sig { Type($1) }
fn yy_action_100(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeSig(_dollar1)
  YYObj_Sig({();  Type(_dollar1) })
}

// file:///./mbti_parser.mbty
// 263|    delimited("[", separated_nonempty_list(",", type_), "]") { $1 }
fn yy_action_101(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_59(_last_pos, _args[0:3])
  guard _sub_action_0_result is YYObj__immut_list_T_Type_(_dollar1)
  YYObj__immut_list_T_Type_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 130|    impl_sig { Impl($1) }
fn yy_action_102(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ImplSig(_dollar1)
  YYObj_Sig({();  Impl(_dollar1) })
}

// file:///./mbti_parser.mbty
// 177|    "impl" qualified_uident "for" type_ { Trait(type_params=Nil, trait_name=$2, type_=$4) }
fn yy_action_103(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_QualifiedName(_dollar2)
  guard _args[3].0 is YYObj_Type(_dollar4)
  YYObj_ImplSig({();  Trait(type_params=Nil, trait_name=_dollar2, type_=_dollar4) })
}

// file:///./stdlib.mbty
// 15|    X { $1 }
fn yy_action_104(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_38(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__immut_list_T_QualifiedName_(_dollar1)
  YYObj__immut_list_T_QualifiedName_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_105(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamWithConstraints(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar3)
  YYObj__immut_list_T_TypeParamWithConstraints_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 189|    vis "traitalias" uident "=" qualified_uident { TraitAlias(vis=$1, name=$3, trait_name=$5) }
fn yy_action_106(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj_Name(_dollar3)
  guard _args[3].0 is YYObj_QualifiedName(_dollar5)
  YYObj_AliasSig({();  TraitAlias(vis=_dollar1, name=_dollar3, trait_name=_dollar5) })
}

// file:///./mbti_parser.mbty
// 237|    uident optional_type_params_no_constraints { ($1, $2) }
fn yy_action_107(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[1].0 is YYObj__immut_list_T_TypeParamNoConstraints_(_dollar2)
  YYObj__Name___immut_list_T_TypeParamNoConstraints__({();  (_dollar1, _dollar2) })
}

// file:///./mbti_parser.mbty
// 122|    sig_ ";" sigs { Cons(($1, mk_loc($loc($1))), $3) }
fn yy_action_108(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Sig(_dollar1)
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  guard _args[2].0 is YYObj__immut_list_T__Sig__Location__(_dollar3)
  YYObj__immut_list_T__Sig__Location__({();  Cons((_dollar1, mk_loc((_start_pos_of_item0, _end_pos_of_item0))), _dollar3) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_109(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_PackageImport(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_PackageImport_(_dollar3)
  YYObj__immut_list_T_PackageImport_({();  Cons(_dollar1, _dollar3) })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_110(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_FieldDecl_({();  Nil })
}

// file:///./stdlib.mbty
// 14|    { Nil }
fn yy_action_111(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_QualifiedName_({();  Nil })
}

// file:///./mbti_parser.mbty
// 306|    { None }
fn yy_action_112(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_String_({();  None })
}

// file:///./mbti_parser.mbty
// 115|    STRING { PackageImport::{name: $1, alias_: None} }
fn yy_action_113(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_PackageImport({();  PackageImport::{name: _dollar1, alias_: None} })
}

// file:///./stdlib.mbty
// 5|    X { Some($1) }
fn yy_action_114(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_115(_last_pos, _args[0:3])
  guard _sub_action_0_result is YYObj__immut_list_T_ConstrParam_(_dollar1)
  YYObj_Option__immut_list_T_ConstrParam__({();  Some(_dollar1) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_116(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_Type_(_dollar3)
  YYObj__immut_list_T_Type_({();  Cons(_dollar1, _dollar3) })
}

// file:///./stdlib.mbty
// 19|    { false }
fn yy_action_117(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Bool({();  false })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_32(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_TraitMethodParameter_(_dollar2)
  YYObj__immut_list_T_TraitMethodParameter_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 136|    "const" uident ":" type_ "=" constant { ConstSig::{name: $2, type_: $4, value: $6} }
fn yy_action_118(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[3].0 is YYObj_Type(_dollar4)
  guard _args[5].0 is YYObj_Constant(_dollar6)
  YYObj_ConstSig({();  ConstSig::{name: _dollar2, type_: _dollar4, value: _dollar6} })
}

// file:///./mbti_parser.mbty
// 327|    UIDENT { Name::{name: $1, loc: mk_loc($sloc)} }
fn yy_action_119(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  Name::{name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 243|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { TArrow(Cons($3, $5.or(Nil)), $8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_120(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_62(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_7(_args[2].2, _args[3:4])
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _sub_action_1_result is YYObj_Option__immut_list_T_Type__(_dollar5)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, _dollar5.or(Nil)), _dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 300|    { Default }
fn yy_action_11(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Visibility({();  Default })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_121(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__immut_list_T_QualifiedName_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 295|    FLOAT { Double($1) }
fn yy_action_122(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj_Constant({();  Double(_dollar1) })
}

// file:///./stdlib.mbty
// 19|    { false }
fn yy_action_123(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Bool({();  false })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_124(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_TypeParamNoConstraints_({();  Nil })
}

// file:///./mbti_parser.mbty
// 126|    func_sig { Func($1) }
fn yy_action_125(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FuncSig(_dollar1)
  YYObj_Sig({();  Func(_dollar1) })
}

// file:///./mbti_parser.mbty
// 243|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { TArrow(Cons($3, $5.or(Nil)), $8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_126(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_6(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_55(_args[3].2, _args[4:4])
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _sub_action_1_result is YYObj_Option__immut_list_T_Type__(_dollar5)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, _dollar5.or(Nil)), _dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 183|    vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) "{" separated_nonempty_list(";", trait_method_sig) "}" { TraitSig::{vis: $1, name: $3, super_traits: $4, methods: $6} }
fn yy_action_127(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar3)
  guard _args[4].0 is YYObj__immut_list_T_QualifiedName_(_dollar4)
  guard _args[6].0 is YYObj__immut_list_T_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: _dollar4, methods: _dollar6} })
}

// file:///./mbti_parser.mbty
// 169|    vis "type" "!" uident type_ { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(SinglePayload($5))} }
fn yy_action_128(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(SinglePayload(_dollar5))} })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_129(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_TypeParamNoConstraints_(_dollar1)
  YYObj__immut_list_T_TypeParamNoConstraints_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 242|    "(" type_ "," separated_nonempty_list(",", type_) ")" { TTuple(Cons($2, $4), loc=mk_loc($sloc)) }
fn yy_action_130(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Type(_dollar2)
  guard _args[3].0 is YYObj__immut_list_T_Type_(_dollar4)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TTuple(Cons(_dollar2, _dollar4), loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 4|    { None }
fn yy_action_131(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Option__String__Location__({();  None })
}

// file:///./mbti_parser.mbty
// 170|    vis "type" "!" uident "{" separated_list(";", enum_constructor) "}" { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(EnumPayload($6))} }
fn yy_action_132(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj__immut_list_T_ConstrDecl_(_dollar6)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(EnumPayload(_dollar6))} })
}

// file:///./mbti_parser.mbty
// 201|    boption("mut") type_ { ConstrParam::{mut_: $1, type_: $2, label: None} }
fn yy_action_133(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  guard _args[1].0 is YYObj_Type(_dollar2)
  YYObj_ConstrParam({();  ConstrParam::{mut_: _dollar1, type_: _dollar2, label: None} })
}

// file:///./mbti_parser.mbty
// 317|    UIDENT { QualifiedName::{pkg: None, name: $1, loc: mk_loc($sloc)} }
fn yy_action_134(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_QualifiedName({();  QualifiedName::{pkg: None, name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 244|    is_async "(" ")" "->" return_type { TArrow(Nil, $5.0, err=$5.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_135(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_6(_last_pos, _args[0:1])
  guard _args[4].0 is YYObj__Type__ErrorType_(_dollar5)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Nil, _dollar5.0, err=_dollar5.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 243|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { TArrow(Cons($3, $5.or(Nil)), $8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_136(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_62(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_55(_args[2].2, _args[3:3])
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _sub_action_1_result is YYObj_Option__immut_list_T_Type__(_dollar5)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, _dollar5.or(Nil)), _dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 153|    list(ATTRIBUTE) lident loption(type_params_with_constraints) delimited("(", separated_list(",", parameter), ")") "->" return_type { FuncSig::{name: $2, type_params: $3, params: $4, return_: $6} }
fn yy_action_137(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_41(_args[2].2, _args[3:6])
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[2].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar3)
  guard _sub_action_0_result is YYObj__immut_list_T_Parameter_(_dollar4)
  guard _args[7].0 is YYObj__Type__ErrorType_(_dollar6)
  YYObj_FuncSig({();  FuncSig::{name: _dollar2, type_params: _dollar3, params: _dollar4, return_: _dollar6} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_138(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FieldDecl(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_FieldDecl_(_dollar3)
  YYObj__immut_list_T_FieldDecl_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 282|    FALSE { Bool(false) }
fn yy_action_139(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Constant({();  Bool(false) })
}

// file:///./stdlib.mbty
// 10|    X { Some($1) }
fn yy_action_140(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  YYObj_Option_Bool_({();  Some(_dollar1) })
}

// file:///./mbti_parser.mbty
// 244|    is_async "(" ")" "->" return_type { TArrow(Nil, $5.0, err=$5.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_141(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_62(_last_pos, _args[0:0])
  guard _args[3].0 is YYObj__Type__ErrorType_(_dollar5)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Nil, _dollar5.0, err=_dollar5.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 339|    POST_LABEL { Name::{name: $1, loc: mk_loc($sloc)} }
fn yy_action_142(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  Name::{name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 168|    vis "type" "!" uident { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(NoPayload)} }
fn yy_action_143(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(NoPayload)} })
}

// file:///./stdlib.mbty
// 45|    X list(X) { Cons($1, Nil) }
fn yy_action_144(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj__immut_list_T_String_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 268|    type_ { Positional($1) }
fn yy_action_145(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj_Parameter({();  Positional(_dollar1) })
}

// file:///./mbti_parser.mbty
// 167|    vis "type" type_decl_name_with_params type_ { TypeSig::{vis: $1, name: $3.0, type_params: $3.1, components: Newtype($4)} }
fn yy_action_146(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar3)
  guard _args[4].0 is YYObj_Type(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar3.0, type_params: _dollar3.1, components: Newtype(_dollar4)} })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_115(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_ConstrParam_(_dollar2)
  YYObj__immut_list_T_ConstrParam_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 245|    "(" type_ ")" { $2 }
fn yy_action_147(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Type(_dollar2)
  YYObj_Type({();  _dollar2 })
}

// file:///./stdlib.mbty
// 50|    X nonempty_list(X) { Cons($1, $2) }
fn yy_action_148(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  guard _args[1].0 is YYObj__immut_list_T_String_(_dollar2)
  YYObj__immut_list_T_String_({();  Cons(_dollar1, _dollar2) })
}

// file:///./mbti_parser.mbty
// 211|    uident ":" separated_nonempty_list("+", type_constraint) { TypeParamWithConstraints::{name: $1, constraints: $3} }
fn yy_action_149(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_QualifiedName_(_dollar3)
  YYObj_TypeParamWithConstraints({();  TypeParamWithConstraints::{name: _dollar1, constraints: _dollar3} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_150(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_QualifiedName_(_dollar3)
  YYObj__immut_list_T_QualifiedName_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 111|    "import" "(" separated_nonempty_list(";", package_import) ")" ";" { $3 }
fn yy_action_151(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[2].0 is YYObj__immut_list_T_PackageImport_(_dollar3)
  YYObj__immut_list_T_PackageImport_({();  _dollar3 })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_152(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitMethodSig(_dollar1)
  YYObj__immut_list_T_TraitMethodSig_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 179|    "impl" uident "::" lident { DefaultImpl(trait_name=$2, method_name=$4) }
fn yy_action_153(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  YYObj_ImplSig({();  DefaultImpl(trait_name=_dollar2, method_name=_dollar4) })
}

// file:///./mbti_parser.mbty
// 149|    nonempty_list(ATTRIBUTE) func_sig_no_attr { $2 }
fn yy_action_154(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_FuncSig(_dollar2)
  YYObj_FuncSig({();  _dollar2 })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_155(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamWithConstraints(_dollar1)
  YYObj__immut_list_T_TypeParamWithConstraints_({();  Cons(_dollar1, Nil) })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_156(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FuncSig(_dollar1)
  YYObj__immut_list_T_FuncSig_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 169|    vis "type" "!" uident type_ { TypeSig::{vis: $1, name: $4, type_params: Nil, components: Error(SinglePayload($5))} }
fn yy_action_157(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar5)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4, type_params: Nil, components: Error(SinglePayload(_dollar5))} })
}

// file:///./mbti_parser.mbty
// 144|    FN lident loption(type_params_with_constraints) delimited("(", separated_list(",", parameter), ")") "->" return_type { FuncSig::{name: $2, type_params: $3, params: $4, return_: $6} }
fn yy_action_158(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_41(_args[2].2, _args[3:6])
  guard _args[1].0 is YYObj_Name(_dollar2)
  guard _args[2].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar3)
  guard _sub_action_0_result is YYObj__immut_list_T_Parameter_(_dollar4)
  guard _args[7].0 is YYObj__Type__ErrorType_(_dollar6)
  YYObj_FuncSig({();  FuncSig::{name: _dollar2, type_params: _dollar3, params: _dollar4, return_: _dollar6} })
}

// file:///./mbti_parser.mbty
// 322|    UIDENT { QualifiedName::{pkg: None, name: $1, loc: mk_loc($sloc)} }
fn yy_action_159(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_QualifiedName({();  QualifiedName::{pkg: None, name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 301|    "priv" { Priv(loc=mk_loc($sloc)) }
fn yy_action_2(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Visibility({();  Priv(loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_160(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar1)
  YYObj__immut_list_T_TypeParamWithConstraints_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 248|    "&" qualified_uident_ { TObject($2) }
fn yy_action_161(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_QualifiedName(_dollar2)
  YYObj_Type({();  TObject(_dollar2) })
}

// file:///./mbti_parser.mbty
// 184|    vis "trait" uident { TraitSig::{vis: $1, name: $3, super_traits: Nil, methods: Nil} }
fn yy_action_162(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_13(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar3)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: Nil, methods: Nil} })
}

// file:///./mbti_parser.mbty
// 308|    "(" LIDENT ")" { Some($2) }
fn yy_action_163(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  YYObj_String_({();  Some(_dollar2) })
}

// file:///./mbti_parser.mbty
// 184|    vis "trait" uident { TraitSig::{vis: $1, name: $3, super_traits: Nil, methods: Nil} }
fn yy_action_164(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar3)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: Nil, methods: Nil} })
}

// file:///./mbti_parser.mbty
// 276|    type_ { Positional($1) }
fn yy_action_165(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj_TraitMethodParameter({();  Positional(_dollar1) })
}

// file:///./mbti_parser.mbty
// 283|    BYTE { Byte($1) }
fn yy_action_166(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_CharLiteral(_dollar1)
  YYObj_Constant({();  Byte(_dollar1) })
}

// file:///./mbti_parser.mbty
// 246|    is_async "(" type_ ")" "->" return_type { TArrow(Cons($3, Nil), $6.0, err=$6.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_167(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_62(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _args[4].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, Nil), _dollar6.0, err=_dollar6.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 189|    vis "traitalias" uident "=" qualified_uident { TraitAlias(vis=$1, name=$3, trait_name=$5) }
fn yy_action_168(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar3)
  guard _args[4].0 is YYObj_QualifiedName(_dollar5)
  YYObj_AliasSig({();  TraitAlias(vis=_dollar1, name=_dollar3, trait_name=_dollar5) })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_169(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitMethodParameter(_dollar1)
  YYObj__immut_list_T_TraitMethodParameter_({();  Cons(_dollar1, Nil) })
}

// file:///./stdlib.mbty
// 9|    { None }
fn yy_action_99(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Option_Bool_({();  None })
}

// file:///./mbti_parser.mbty
// 116|    STRING "as" LIDENT { PackageImport::{name: $1, alias_: Some($3)} }
fn yy_action_170(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  guard _args[2].0 is YYObj_String(_dollar3)
  YYObj_PackageImport({();  PackageImport::{name: _dollar1, alias_: Some(_dollar3)} })
}

// file:///./mbti_parser.mbty
// 183|    vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) "{" separated_nonempty_list(";", trait_method_sig) "}" { TraitSig::{vis: $1, name: $3, super_traits: $4, methods: $6} }
fn yy_action_171(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_2(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar3)
  guard _args[3].0 is YYObj__immut_list_T_QualifiedName_(_dollar4)
  guard _args[5].0 is YYObj__immut_list_T_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: _dollar4, methods: _dollar6} })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_172(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_ConstrDecl_({();  Nil })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_173(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrParam(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_ConstrParam_(_dollar3)
  YYObj__immut_list_T_ConstrParam_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 193|    uident option(delimited("(", separated_nonempty_list(",", constructor_param), ")")) option(eq_tag) { ConstrDecl::{name: $1, args: $2, tag: $3, loc: mk_loc($sloc)} }
fn yy_action_174(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[1].0 is YYObj_Option__immut_list_T_ConstrParam__(_dollar2)
  guard _args[2].0 is YYObj_Option__String__Location__(_dollar3)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_ConstrDecl({();  ConstrDecl::{name: _dollar1, args: _dollar2, tag: _dollar3, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 246|    is_async "(" type_ ")" "->" return_type { TArrow(Cons($3, Nil), $6.0, err=$6.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_175(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_6(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _sub_action_0_result is YYObj_Bool(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  TArrow(Cons(_dollar3, Nil), _dollar6.0, err=_dollar6.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_176(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamNoConstraints(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_TypeParamNoConstraints_(_dollar3)
  YYObj__immut_list_T_TypeParamNoConstraints_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 215|    "[" separated_list(",", type_param_with_constraints) "]" { $2 }
fn yy_action_177(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_TypeParamWithConstraints_(_dollar2)
  YYObj__immut_list_T_TypeParamWithConstraints_({();  _dollar2 })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { Cons($1, $3) }
fn yy_action_178(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  guard _args[2].0 is YYObj__immut_list_T_QualifiedName_(_dollar3)
  YYObj__immut_list_T_QualifiedName_({();  Cons(_dollar1, _dollar3) })
}

// file:///./mbti_parser.mbty
// 224|    "[" separated_list(",", type_param_no_constraints) "]" { $2 }
fn yy_action_179(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[1].0 is YYObj__immut_list_T_TypeParamNoConstraints_(_dollar2)
  YYObj__immut_list_T_TypeParamNoConstraints_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 120|     { Nil }
fn yy_action_180(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T__Sig__Location__({();  Nil })
}

// file:///./mbti_parser.mbty
// 281|    TRUE { Bool(true) }
fn yy_action_181(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Constant({();  Bool(true) })
}

// file:///./mbti_parser.mbty
// 129|    trait_sig { Trait($1) }
fn yy_action_182(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitSig(_dollar1)
  YYObj_Sig({();  Trait(_dollar1) })
}

// file:///./mbti_parser.mbty
// 161|    "=" "_" {}
fn yy_action_9(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj_Unit({(); })
}

// file:///./mbti_parser.mbty
// 241|    type_ "?" { TOption($1, loc=mk_loc($sloc), question_loc=mk_loc($loc($2))) }
fn yy_action_183(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj_Type({();  TOption(_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos)), question_loc=mk_loc((_start_pos_of_item1, _end_pos_of_item1))) })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_184(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FieldDecl(_dollar1)
  YYObj__immut_list_T_FieldDecl_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 110|     { Nil }
fn yy_action_185(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_PackageImport_({();  Nil })
}

// file:///./mbti_parser.mbty
// 183|    vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) "{" separated_nonempty_list(";", trait_method_sig) "}" { TraitSig::{vis: $1, name: $3, super_traits: $4, methods: $6} }
fn yy_action_186(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[1].0 is YYObj_Name(_dollar3)
  guard _args[2].0 is YYObj__immut_list_T_QualifiedName_(_dollar4)
  guard _args[4].0 is YYObj__immut_list_T_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({();  TraitSig::{vis: _dollar1, name: _dollar3, super_traits: _dollar4, methods: _dollar6} })
}

// file:///./mbti_parser.mbty
// 272|    label "?" ":" type_ { OptionalOption($1, $4) }
fn yy_action_187(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[3].0 is YYObj_Type(_dollar4)
  YYObj_Parameter({();  OptionalOption(_dollar1, _dollar4) })
}

// file:///./mbti_parser.mbty
// 106|    LIDENT STRING ioption(";") imports sigs EOF { Mbti::{package_name: $2, imports: $4, sigs: $5} }
fn yy_action_188(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_140(_args[1].2, _args[2:3])
  guard _args[1].0 is YYObj_StringLiteral(_dollar2)
  guard _args[3].0 is YYObj__immut_list_T_PackageImport_(_dollar4)
  guard _args[4].0 is YYObj__immut_list_T__Sig__Location__(_dollar5)
  YYObj_Mbti({();  Mbti::{package_name: _dollar2, imports: _dollar4, sigs: _dollar5} })
}

// file:///./mbti_parser.mbty
// 165|    vis "extern" "type" type_decl_name_with_params { TypeSig::{vis: $1, name: $4.0, type_params: $4.1, components: Extern} }
fn yy_action_189(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_11(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj_Visibility(_dollar1)
  guard _args[2].0 is YYObj__Name___immut_list_T_TypeParamNoConstraints__(_dollar4)
  YYObj_TypeSig({();  TypeSig::{vis: _dollar1, name: _dollar4.0, type_params: _dollar4.1, components: Extern} })
}

// file:///./mbti_parser.mbty
// 296|    STRING { String($1) }
fn yy_action_190(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_Constant({();  String(_dollar1) })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_191(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_ConstrDecl_(_dollar1)
  YYObj__immut_list_T_ConstrDecl_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 59|    X { Cons($1, Nil) }
fn yy_action_192(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrParam(_dollar1)
  YYObj__immut_list_T_ConstrParam_({();  Cons(_dollar1, Nil) })
}

// file:///./mbti_parser.mbty
// 229|    type_params_no_constraints { $1 }
fn yy_action_193(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  guard _args[0].0 is YYObj__immut_list_T_TypeParamNoConstraints_(_dollar1)
  YYObj__immut_list_T_TypeParamNoConstraints_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 54|    { Nil }
fn yy_action_194(_last_pos : Position, _args : ArrayView[(YYObj, Position, Position)]) -> YYObj {
  YYObj__immut_list_T_TypeParamWithConstraints_({();  Nil })
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
// [179, mbti_prime → • mbti, $]
fn yy_state_0(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_mbti => Shift(yy_state_1)
    T_LIDENT => Shift(yy_state_2)
    _ => Error
  }
}

// [179, mbti_prime → mbti •, $]
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
    NT_imports => Shift(yy_state_362)
    T_ATTRIBUTE | T_EOF | T_PUB | T_PRIV | T_EXTERN | T_STRUCT | T_ENUM | T_TRAIT | T_IMPL | T_TYPEALIAS | T_TRAITALIAS | T_FN | T_LET | T_CONST | T_TYPE => Reduce(0, NT_imports, yy_action_185)
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
// [123, separated_nonempty_list(SEMI,package_import) → • package_import, RPAREN]
// [124, separated_nonempty_list(SEMI,package_import) → • package_import SEMI separated_nonempty_list(SEMI,package_import), RPAREN]
fn yy_state_5(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_package_import => Shift(yy_state_6)
    T_STRING => Shift(yy_state_9)
    NT_separated_nonempty_list_SEMI_package_import_ => Shift(yy_state_12)
    _ => Error
  }
}

// [123, separated_nonempty_list(SEMI,package_import) → package_import •, RPAREN]
// [124, separated_nonempty_list(SEMI,package_import) → package_import • SEMI separated_nonempty_list(SEMI,package_import), RPAREN]
fn yy_state_6(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_7)
    T_RPAREN => Reduce(1, NT_separated_nonempty_list_SEMI_package_import_, yy_action_87)
    _ => Error
  }
}

// [4, package_import → • STRING, RPAREN / SEMI]
// [5, package_import → • STRING AS LIDENT, RPAREN / SEMI]
// [123, separated_nonempty_list(SEMI,package_import) → • package_import, RPAREN]
// [124, separated_nonempty_list(SEMI,package_import) → • package_import SEMI separated_nonempty_list(SEMI,package_import), RPAREN]
// [124, separated_nonempty_list(SEMI,package_import) → package_import SEMI • separated_nonempty_list(SEMI,package_import), RPAREN]
fn yy_state_7(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_package_import => Shift(yy_state_6)
    NT_separated_nonempty_list_SEMI_package_import_ => Shift(yy_state_8)
    T_STRING => Shift(yy_state_9)
    _ => Error
  }
}

// [124, separated_nonempty_list(SEMI,package_import) → package_import SEMI separated_nonempty_list(SEMI,package_import) •, RPAREN]
fn yy_state_8(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_package_import_, yy_action_109)
}

// [4, package_import → STRING •, RPAREN / SEMI]
// [5, package_import → STRING • AS LIDENT, RPAREN / SEMI]
fn yy_state_9(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_10)
    T_RPAREN | T_SEMI => Reduce(1, NT_package_import, yy_action_113)
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
  ReduceNoLookahead(3, NT_package_import, yy_action_170)
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
  ReduceNoLookahead(5, NT_imports, yy_action_151)
}

// [1, mbti → LIDENT STRING SEMI • imports sigs EOF, $]
// [2, imports → •, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
// [3, imports → • IMPORT LPAREN separated_nonempty_list(SEMI,package_import) RPAREN SEMI, ATTRIBUTE / EOF / PUB / PRIV / EXTERN / STRUCT / ENUM / TRAIT / IMPL / TYPEALIAS / TRAITALIAS / FN / LET / CONST / TYPE]
fn yy_state_15(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_IMPORT => Shift(yy_state_4)
    NT_imports => Shift(yy_state_16)
    T_ATTRIBUTE | T_EOF | T_PUB | T_PRIV | T_EXTERN | T_STRUCT | T_ENUM | T_TRAIT | T_IMPL | T_TYPEALIAS | T_TRAITALIAS | T_FN | T_LET | T_CONST | T_TYPE => Reduce(0, NT_imports, yy_action_185)
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
// [18, func_sig_no_attr → • FN lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [19, func_sig → • func_sig_no_attr, EOF / SEMI]
// [20, func_sig → • nonempty_list(ATTRIBUTE) func_sig_no_attr, EOF / SEMI]
// [23, type_sig → • EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [24, type_sig → • PRIV EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [25, type_sig → • PUB pub_attr EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [26, type_sig → • TYPE type_decl_name_with_params, EOF / SEMI]
// [27, type_sig → • PRIV TYPE type_decl_name_with_params, EOF / SEMI]
// [28, type_sig → • PUB pub_attr TYPE type_decl_name_with_params, EOF / SEMI]
// [29, type_sig → • TYPE type_decl_name_with_params type_, EOF / SEMI]
// [30, type_sig → • PRIV TYPE type_decl_name_with_params type_, EOF / SEMI]
// [31, type_sig → • PUB pub_attr TYPE type_decl_name_with_params type_, EOF / SEMI]
// [32, type_sig → • TYPE EXCLAMATION uident, EOF / SEMI]
// [33, type_sig → • PRIV TYPE EXCLAMATION uident, EOF / SEMI]
// [34, type_sig → • PUB pub_attr TYPE EXCLAMATION uident, EOF / SEMI]
// [35, type_sig → • TYPE EXCLAMATION uident type_, EOF / SEMI]
// [36, type_sig → • PRIV TYPE EXCLAMATION uident type_, EOF / SEMI]
// [37, type_sig → • PUB pub_attr TYPE EXCLAMATION uident type_, EOF / SEMI]
// [38, type_sig → • TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [39, type_sig → • PRIV TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [40, type_sig → • PUB pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [41, type_sig → • STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [42, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [43, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [44, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [45, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [46, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [47, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [48, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [49, impl_sig → • IMPL uident LBRACE separated_nonempty_list(SEMI,impl_method_sig) RBRACE, EOF / SEMI]
// [50, impl_sig → • IMPL uident COLONCOLON lident, EOF / SEMI]
// [51, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [52, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [53, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [54, trait_sig → • TRAIT uident, EOF / SEMI]
// [55, trait_sig → • PRIV TRAIT uident, EOF / SEMI]
// [56, trait_sig → • PUB pub_attr TRAIT uident, EOF / SEMI]
// [57, alias_sig → • TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [58, alias_sig → • PRIV TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [59, alias_sig → • PUB pub_attr TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [60, alias_sig → • TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [61, alias_sig → • PRIV TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [62, alias_sig → • PUB pub_attr TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [129, nonempty_list(ATTRIBUTE) → • ATTRIBUTE, FN]
// [130, nonempty_list(ATTRIBUTE) → • ATTRIBUTE nonempty_list(ATTRIBUTE), FN]
fn yy_state_16(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_17)
    T_TRAITALIAS => Shift(yy_state_19)
    T_TYPEALIAS => Shift(yy_state_27)
    T_TRAIT => Shift(yy_state_98)
    T_IMPL => Shift(yy_state_131)
    T_ENUM => Shift(yy_state_190)
    T_STRUCT => Shift(yy_state_216)
    T_TYPE => Shift(yy_state_229)
    T_PUB => Shift(yy_state_238)
    T_PRIV => Shift(yy_state_281)
    T_EXTERN => Shift(yy_state_318)
    NT_nonempty_list_ATTRIBUTE_ => Shift(yy_state_321)
    T_FN => Shift(yy_state_323)
    NT_func_sig_no_attr => Shift(yy_state_331)
    T_LET => Shift(yy_state_332)
    T_CONST => Shift(yy_state_336)
    NT_value_sig => Shift(yy_state_350)
    NT_const_sig => Shift(yy_state_351)
    NT_impl_sig => Shift(yy_state_352)
    NT_trait_sig => Shift(yy_state_353)
    NT_alias_sig => Shift(yy_state_354)
    NT_type_sig => Shift(yy_state_355)
    NT_func_sig => Shift(yy_state_356)
    NT_sig_ => Shift(yy_state_357)
    NT_sigs => Shift(yy_state_360)
    T_EOF => Reduce(0, NT_sigs, yy_action_180)
    _ => Error
  }
}

// [129, nonempty_list(ATTRIBUTE) → • ATTRIBUTE, FN]
// [129, nonempty_list(ATTRIBUTE) → ATTRIBUTE •, FN]
// [130, nonempty_list(ATTRIBUTE) → • ATTRIBUTE nonempty_list(ATTRIBUTE), FN]
// [130, nonempty_list(ATTRIBUTE) → ATTRIBUTE • nonempty_list(ATTRIBUTE), FN]
fn yy_state_17(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_17)
    NT_nonempty_list_ATTRIBUTE_ => Shift(yy_state_18)
    T_FN => Reduce(1, NT_nonempty_list_ATTRIBUTE_, yy_action_72)
    _ => Error
  }
}

// [130, nonempty_list(ATTRIBUTE) → ATTRIBUTE nonempty_list(ATTRIBUTE) •, FN]
fn yy_state_18(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_nonempty_list_ATTRIBUTE_, yy_action_148)
}

// [60, alias_sig → TRAITALIAS • uident EQUAL qualified_uident, EOF / SEMI]
// [119, uident → • UIDENT, EQUAL]
fn yy_state_19(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_21)
    _ => Error
  }
}

// [119, uident → UIDENT •, UIDENT / EOF / ASYNC / EQUAL / LPAREN / COMMA / COLON / SEMI / LBRACKET / RBRACKET / UNDERSCORE / LBRACE / RBRACE / AMPER / PACKAGE_NAME]
fn yy_state_20(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_uident, yy_action_119)
}

// [60, alias_sig → TRAITALIAS uident • EQUAL qualified_uident, EOF / SEMI]
fn yy_state_21(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_22)
    _ => Error
  }
}

// [60, alias_sig → TRAITALIAS uident EQUAL • qualified_uident, EOF / SEMI]
// [115, qualified_uident → • UIDENT, EOF / SEMI]
// [116, qualified_uident → • PACKAGE_NAME DOT_UIDENT, EOF / SEMI]
fn yy_state_22(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_qualified_uident => Shift(yy_state_26)
    _ => Error
  }
}

// [116, qualified_uident → PACKAGE_NAME • DOT_UIDENT, EOF / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]
fn yy_state_23(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOT_UIDENT => Shift(yy_state_24)
    _ => Error
  }
}

// [116, qualified_uident → PACKAGE_NAME DOT_UIDENT •, EOF / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]
fn yy_state_24(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_qualified_uident, yy_action_85)
}

// [115, qualified_uident → UIDENT •, EOF / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]
fn yy_state_25(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_qualified_uident, yy_action_134)
}

// [60, alias_sig → TRAITALIAS uident EQUAL qualified_uident •, EOF / SEMI]
fn yy_state_26(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_alias_sig, yy_action_106)
}

// [57, alias_sig → TYPEALIAS • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, EQUAL]
// [119, uident → • UIDENT, EQUAL / LBRACKET]
fn yy_state_27(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_40)
    _ => Error
  }
}

// [72, type_params_no_constraints → • LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
// [73, optional_type_params_no_constraints → •, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
// [74, optional_type_params_no_constraints → • type_params_no_constraints, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
// [76, type_decl_name_with_params → uident • optional_type_params_no_constraints, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_28(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_optional_type_params_no_constraints => Shift(yy_state_29)
    NT_type_params_no_constraints => Shift(yy_state_30)
    T_LBRACKET => Shift(yy_state_31)
    T_UIDENT | T_EOF | T_ASYNC | T_EQUAL | T_LPAREN | T_SEMI | T_UNDERSCORE | T_LBRACE | T_AMPER | T_PACKAGE_NAME => Reduce(0, NT_optional_type_params_no_constraints, yy_action_86)
    _ => Error
  }
}

// [76, type_decl_name_with_params → uident optional_type_params_no_constraints •, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_29(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_type_decl_name_with_params, yy_action_107)
}

// [74, optional_type_params_no_constraints → type_params_no_constraints •, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_30(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_optional_type_params_no_constraints, yy_action_193)
}

// [70, type_param_no_constraints → • uident, COMMA / RBRACKET]
// [71, type_param_no_constraints → • UNDERSCORE, COMMA / RBRACKET]
// [72, type_params_no_constraints → LBRACKET • separated_list(COMMA,type_param_no_constraints) RBRACKET, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
// [119, uident → • UIDENT, COMMA / RBRACKET]
// [161, separated_list(COMMA,type_param_no_constraints) → •, RBRACKET]
// [162, separated_list(COMMA,type_param_no_constraints) → • separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
// [177, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints, RBRACKET]
// [178, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
fn yy_state_31(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_type_param_no_constraints => Shift(yy_state_32)
    T_UNDERSCORE => Shift(yy_state_35)
    NT_uident => Shift(yy_state_36)
    NT_separated_nonempty_list_COMMA_type_param_no_constraints_ => Shift(yy_state_37)
    NT_separated_list_COMMA_type_param_no_constraints_ => Shift(yy_state_38)
    T_RBRACKET => Reduce(0, NT_separated_list_COMMA_type_param_no_constraints_, yy_action_124)
    _ => Error
  }
}

// [177, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints •, RBRACKET]
// [178, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints • COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
fn yy_state_32(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_33)
    T_RBRACKET => Reduce(1, NT_separated_nonempty_list_COMMA_type_param_no_constraints_, yy_action_78)
    _ => Error
  }
}

// [70, type_param_no_constraints → • uident, COMMA / RBRACKET]
// [71, type_param_no_constraints → • UNDERSCORE, COMMA / RBRACKET]
// [119, uident → • UIDENT, COMMA / RBRACKET]
// [177, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints, RBRACKET]
// [178, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
// [178, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints COMMA • separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
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

// [178, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints) •, RBRACKET]
fn yy_state_34(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_type_param_no_constraints_, yy_action_176)
}

// [71, type_param_no_constraints → UNDERSCORE •, COMMA / RBRACKET]
fn yy_state_35(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_param_no_constraints, yy_action_66)
}

// [70, type_param_no_constraints → uident •, COMMA / RBRACKET]
fn yy_state_36(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_param_no_constraints, yy_action_29)
}

// [162, separated_list(COMMA,type_param_no_constraints) → separated_nonempty_list(COMMA,type_param_no_constraints) •, RBRACKET]
fn yy_state_37(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_type_param_no_constraints_, yy_action_129)
}

// [72, type_params_no_constraints → LBRACKET separated_list(COMMA,type_param_no_constraints) • RBRACKET, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_38(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_39)
    _ => Error
  }
}

// [72, type_params_no_constraints → LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET •, UIDENT / EOF / ASYNC / EQUAL / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_39(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_params_no_constraints, yy_action_179)
}

// [57, alias_sig → TYPEALIAS type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_40(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_41)
    _ => Error
  }
}

// [57, alias_sig → TYPEALIAS type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_41(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_97)
    _ => Error
  }
}

// [118, qualified_uident_ → PACKAGE_NAME • DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / PLUS / RBRACKET / RBRACE]
fn yy_state_42(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOT_UIDENT => Shift(yy_state_43)
    _ => Error
  }
}

// [118, qualified_uident_ → PACKAGE_NAME DOT_UIDENT •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / PLUS / RBRACKET / RBRACE]
fn yy_state_43(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_qualified_uident_, yy_action_51)
}

// [117, qualified_uident_ → UIDENT •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / PLUS / RBRACKET / RBRACE]
fn yy_state_44(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_qualified_uident_, yy_action_159)
}

// [90, type_ → UNDERSCORE •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_45(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_, yy_action_82)
}

// [89, type_ → AMPER • qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_46(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    NT_qualified_uident_ => Shift(yy_state_47)
    _ => Error
  }
}

// [89, type_ → AMPER qualified_uident_ •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_47(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_type_, yy_action_161)
}

// [88, type_ → qualified_uident_ • optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [95, optional_type_arguments → • LBRACKET separated_nonempty_list(COMMA,type_) RBRACKET, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [96, optional_type_arguments → •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_48(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_49)
    NT_optional_type_arguments => Shift(yy_state_96)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_EXCLAMATION | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(0, NT_optional_type_arguments, yy_action_64)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, COMMA / QUESTION / RBRACKET]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, COMMA / QUESTION / RBRACKET]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, COMMA / QUESTION / RBRACKET]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, COMMA / QUESTION / RBRACKET]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, COMMA / QUESTION / RBRACKET]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, COMMA / QUESTION / RBRACKET]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, COMMA / QUESTION / RBRACKET]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, COMMA / QUESTION / RBRACKET]
// [85, type_ → • LPAREN type_ RPAREN, COMMA / QUESTION / RBRACKET]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, COMMA / QUESTION / RBRACKET]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, COMMA / QUESTION / RBRACKET]
// [88, type_ → • qualified_uident_ optional_type_arguments, COMMA / QUESTION / RBRACKET]
// [89, type_ → • AMPER qualified_uident_, COMMA / QUESTION / RBRACKET]
// [90, type_ → • UNDERSCORE, COMMA / QUESTION / RBRACKET]
// [95, optional_type_arguments → LBRACKET • separated_nonempty_list(COMMA,type_) RBRACKET, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, COMMA / QUESTION / LBRACKET / RBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, COMMA / QUESTION / LBRACKET / RBRACKET]
// [163, separated_nonempty_list(COMMA,type_) → • type_, RBRACKET]
// [164, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RBRACKET]
fn yy_state_49(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_50)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_81)
    _ => Error
  }
}

// [95, optional_type_arguments → LBRACKET separated_nonempty_list(COMMA,type_) • RBRACKET, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_50(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_51)
    _ => Error
  }
}

// [95, optional_type_arguments → LBRACKET separated_nonempty_list(COMMA,type_) RBRACKET •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_51(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_optional_type_arguments, yy_action_101)
}

// [79, type_ → ASYNC • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → ASYNC • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → ASYNC • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → ASYNC • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_52(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_53)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [79, type_ → ASYNC LPAREN • type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → ASYNC LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → ASYNC LPAREN • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [86, type_ → ASYNC LPAREN • type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_53(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_RPAREN => Shift(yy_state_54)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_84)
    _ => Error
  }
}

// [83, type_ → ASYNC LPAREN RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_54(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_55)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → ASYNC LPAREN RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [89, type_ → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [90, type_ → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [91, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [92, return_type → • type_ EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_55(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    NT_return_type => Shift(yy_state_56)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_61)
    _ => Error
  }
}

// [83, type_ → ASYNC LPAREN RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_56(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_, yy_action_135)
}

// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [78, type_ → LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → LPAREN • type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → LPAREN • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [85, type_ → LPAREN • type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → LPAREN • type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_57(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    T_RPAREN => Shift(yy_state_58)
    NT_type_ => Shift(yy_state_69)
    _ => Error
  }
}

// [84, type_ → LPAREN RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_58(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_59)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → LPAREN RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [89, type_ → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [90, type_ → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [91, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [92, return_type → • type_ EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_59(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_return_type => Shift(yy_state_60)
    NT_type_ => Shift(yy_state_61)
    _ => Error
  }
}

// [84, type_ → LPAREN RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_60(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_, yy_action_141)
}

// [77, type_ → type_ • QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [91, return_type → type_ •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [92, return_type → type_ • EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [93, return_type → type_ • EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_61(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXCLAMATION => Shift(yy_state_62)
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(1, NT_return_type, yy_action_52)
    _ => Error
  }
}

// [92, return_type → type_ EXCLAMATION •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [93, return_type → type_ EXCLAMATION • separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [94, error_type → • qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / PLUS / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / PLUS / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / PLUS / RBRACKET / RBRACE]
// [165, separated_nonempty_list(PLUS,error_type) → • error_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [166, separated_nonempty_list(PLUS,error_type) → • error_type PLUS separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_62(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    NT_error_type => Shift(yy_state_63)
    NT_qualified_uident_ => Shift(yy_state_66)
    NT_separated_nonempty_list_PLUS_error_type_ => Shift(yy_state_67)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_EXCLAMATION | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(2, NT_return_type, yy_action_3)
    _ => Error
  }
}

// [165, separated_nonempty_list(PLUS,error_type) → error_type •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [166, separated_nonempty_list(PLUS,error_type) → error_type • PLUS separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_63(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS => Shift(yy_state_64)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_EXCLAMATION | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(1, NT_separated_nonempty_list_PLUS_error_type_, yy_action_30)
    _ => Error
  }
}

// [94, error_type → • qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / PLUS / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / PLUS / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / PLUS / RBRACKET / RBRACE]
// [165, separated_nonempty_list(PLUS,error_type) → • error_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [166, separated_nonempty_list(PLUS,error_type) → • error_type PLUS separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [166, separated_nonempty_list(PLUS,error_type) → error_type PLUS • separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_64(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    NT_error_type => Shift(yy_state_63)
    NT_separated_nonempty_list_PLUS_error_type_ => Shift(yy_state_65)
    NT_qualified_uident_ => Shift(yy_state_66)
    _ => Error
  }
}

// [166, separated_nonempty_list(PLUS,error_type) → error_type PLUS separated_nonempty_list(PLUS,error_type) •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_65(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_PLUS_error_type_, yy_action_0)
}

// [94, error_type → qualified_uident_ •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / PLUS / RBRACKET / RBRACE]
fn yy_state_66(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_error_type, yy_action_49)
}

// [93, return_type → type_ EXCLAMATION separated_nonempty_list(PLUS,error_type) •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_67(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_return_type, yy_action_60)
}

// [77, type_ → type_ QUESTION •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_68(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_type_, yy_action_183)
}

// [77, type_ → type_ • QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → LPAREN type_ • COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [85, type_ → LPAREN type_ • RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → LPAREN type_ • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_69(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_RPAREN => Shift(yy_state_70)
    T_COMMA => Shift(yy_state_73)
    _ => Error
  }
}

// [85, type_ → LPAREN type_ RPAREN •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → LPAREN type_ RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_70(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_71)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_EXCLAMATION | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(3, NT_type_, yy_action_147)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → LPAREN type_ RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [89, type_ → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [90, type_ → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [91, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [92, return_type → • type_ EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_71(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_61)
    NT_return_type => Shift(yy_state_72)
    _ => Error
  }
}

// [87, type_ → LPAREN type_ RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_72(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_, yy_action_167)
}

// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [78, type_ → LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → LPAREN type_ COMMA • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [163, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [164, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
fn yy_state_73(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    T_RPAREN => Shift(yy_state_74)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_77)
    NT_type_ => Shift(yy_state_81)
    _ => Error
  }
}

// [81, type_ → LPAREN type_ COMMA RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_74(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_75)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → LPAREN type_ COMMA RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [89, type_ → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [90, type_ → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [91, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [92, return_type → • type_ EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_75(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_61)
    NT_return_type => Shift(yy_state_76)
    _ => Error
  }
}

// [81, type_ → LPAREN type_ COMMA RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_76(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_, yy_action_136)
}

// [78, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_77(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_78)
    _ => Error
  }
}

// [78, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_78(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_79)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_EXCLAMATION | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(5, NT_type_, yy_action_130)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [89, type_ → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [90, type_ → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [91, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [92, return_type → • type_ EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_79(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_61)
    NT_return_type => Shift(yy_state_80)
    _ => Error
  }
}

// [82, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_80(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_, yy_action_120)
}

// [77, type_ → type_ • QUESTION, RPAREN / COMMA / QUESTION / RBRACKET]
// [163, separated_nonempty_list(COMMA,type_) → type_ •, RPAREN / RBRACKET]
// [164, separated_nonempty_list(COMMA,type_) → type_ • COMMA separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]
fn yy_state_81(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_COMMA => Shift(yy_state_82)
    T_RPAREN | T_RBRACKET => Reduce(1, NT_separated_nonempty_list_COMMA_type__, yy_action_75)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION / RBRACKET]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION / RBRACKET]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION / RBRACKET]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION / RBRACKET]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION / RBRACKET]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION / RBRACKET]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION / RBRACKET]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION / RBRACKET]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION / RBRACKET]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION / RBRACKET]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION / RBRACKET]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION / RBRACKET]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION / RBRACKET]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION / RBRACKET]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET / RBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET / RBRACKET]
// [163, separated_nonempty_list(COMMA,type_) → • type_, RPAREN / RBRACKET]
// [164, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]
// [164, separated_nonempty_list(COMMA,type_) → type_ COMMA • separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]
fn yy_state_82(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_81)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_83)
    _ => Error
  }
}

// [164, separated_nonempty_list(COMMA,type_) → type_ COMMA separated_nonempty_list(COMMA,type_) •, RPAREN / RBRACKET]
fn yy_state_83(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_type__, yy_action_116)
}

// [77, type_ → type_ • QUESTION, RPAREN / COMMA / QUESTION]
// [79, type_ → ASYNC LPAREN type_ • COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → ASYNC LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → ASYNC LPAREN type_ • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_84(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_RPAREN => Shift(yy_state_85)
    T_COMMA => Shift(yy_state_88)
    _ => Error
  }
}

// [86, type_ → ASYNC LPAREN type_ RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_85(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_86)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → ASYNC LPAREN type_ RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [89, type_ → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [90, type_ → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [91, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [92, return_type → • type_ EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_86(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_61)
    NT_return_type => Shift(yy_state_87)
    _ => Error
  }
}

// [86, type_ → ASYNC LPAREN type_ RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_87(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_, yy_action_175)
}

// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [79, type_ → ASYNC LPAREN type_ COMMA • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → ASYNC LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [163, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [164, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
fn yy_state_88(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_81)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_89)
    T_RPAREN => Shift(yy_state_93)
    _ => Error
  }
}

// [80, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_89(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_90)
    _ => Error
  }
}

// [80, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_90(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_91)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [89, type_ → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [90, type_ → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [91, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [92, return_type → • type_ EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_91(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_61)
    NT_return_type => Shift(yy_state_92)
    _ => Error
  }
}

// [80, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_92(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_, yy_action_5)
}

// [79, type_ → ASYNC LPAREN type_ COMMA RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_93(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_94)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [79, type_ → ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [89, type_ → • AMPER qualified_uident_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [90, type_ → • UNDERSCORE, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [91, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [92, return_type → • type_ EXCLAMATION, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
// [117, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACKET / RBRACE]
fn yy_state_94(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_61)
    NT_return_type => Shift(yy_state_95)
    _ => Error
  }
}

// [79, type_ → ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_95(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_, yy_action_126)
}

// [88, type_ → qualified_uident_ optional_type_arguments •, EOF / EQUAL / RPAREN / COMMA / QUESTION / EXCLAMATION / SEMI / RBRACKET / RBRACE]
fn yy_state_96(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_type_, yy_action_91)
}

// [57, alias_sig → TYPEALIAS type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_97(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(4, NT_alias_sig, yy_action_20)
    _ => Error
  }
}

// [51, trait_sig → TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [54, trait_sig → TRAIT • uident, EOF / SEMI]
// [119, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_98(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_99)
    _ => Error
  }
}

// [51, trait_sig → TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [54, trait_sig → TRAIT uident •, EOF / SEMI]
// [145, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, LBRACE]
// [146, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_99(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_100)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_105)
    T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_111)
    T_EOF | T_SEMI => Reduce(2, NT_trait_sig, yy_action_22)
    _ => Error
  }
}

// [115, qualified_uident → • UIDENT, PLUS / LBRACE]
// [116, qualified_uident → • PACKAGE_NAME DOT_UIDENT, PLUS / LBRACE]
// [143, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident, LBRACE]
// [144, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident), LBRACE]
// [146, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → COLON • separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_100(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_separated_nonempty_list_PLUS_qualified_uident_ => Shift(yy_state_101)
    NT_qualified_uident => Shift(yy_state_102)
    _ => Error
  }
}

// [146, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → COLON separated_nonempty_list(PLUS,qualified_uident) •, LBRACE]
fn yy_state_101(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_104)
}

// [143, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident •, LBRACE]
// [144, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident • PLUS separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_102(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS => Shift(yy_state_103)
    T_LBRACE => Reduce(1, NT_separated_nonempty_list_PLUS_qualified_uident_, yy_action_96)
    _ => Error
  }
}

// [115, qualified_uident → • UIDENT, PLUS / LBRACE]
// [116, qualified_uident → • PACKAGE_NAME DOT_UIDENT, PLUS / LBRACE]
// [143, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident, LBRACE]
// [144, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident), LBRACE]
// [144, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident PLUS • separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_103(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_qualified_uident => Shift(yy_state_102)
    NT_separated_nonempty_list_PLUS_qualified_uident_ => Shift(yy_state_104)
    _ => Error
  }
}

// [144, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident) •, LBRACE]
fn yy_state_104(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_PLUS_qualified_uident_, yy_action_150)
}

// [51, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_105(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_106)
    _ => Error
  }
}

// [22, trait_method_sig → • lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [51, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE • separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [120, lident → • LIDENT, LPAREN]
// [147, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [148, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_106(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_trait_method_sig => Shift(yy_state_107)
    T_LIDENT => Shift(yy_state_110)
    NT_lident => Shift(yy_state_111)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_129)
    _ => Error
  }
}

// [147, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig •, RBRACE]
// [148, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig • SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_107(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_108)
    T_RBRACE => Reduce(1, NT_separated_nonempty_list_SEMI_trait_method_sig_, yy_action_152)
    _ => Error
  }
}

// [22, trait_method_sig → • lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [120, lident → • LIDENT, LPAREN]
// [147, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [148, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
// [148, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig SEMI • separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_108(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_trait_method_sig => Shift(yy_state_107)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_109)
    T_LIDENT => Shift(yy_state_110)
    NT_lident => Shift(yy_state_111)
    _ => Error
  }
}

// [148, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig) •, RBRACE]
fn yy_state_109(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_trait_method_sig_, yy_action_90)
}

// [120, lident → LIDENT •, EOF / LPAREN / COLON / SEMI / LBRACKET]
fn yy_state_110(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_lident, yy_action_34)
}

// [22, trait_method_sig → lident • LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_111(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_112)
    _ => Error
  }
}

// [22, trait_method_sig → lident LPAREN • separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [102, trait_method_parameter → • type_, RPAREN / COMMA]
// [103, trait_method_parameter → • post_label COLON type_, RPAREN / COMMA]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [122, post_label → • POST_LABEL, COLON]
// [133, separated_list(COMMA,trait_method_parameter) → •, RPAREN]
// [134, separated_list(COMMA,trait_method_parameter) → • separated_nonempty_list(COMMA,trait_method_parameter), RPAREN]
// [169, separated_nonempty_list(COMMA,trait_method_parameter) → • trait_method_parameter, RPAREN]
// [170, separated_nonempty_list(COMMA,trait_method_parameter) → • trait_method_parameter COMMA separated_nonempty_list(COMMA,trait_method_parameter), RPAREN]
fn yy_state_112(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_trait_method_parameter => Shift(yy_state_113)
    T_POST_LABEL => Shift(yy_state_116)
    NT_post_label => Shift(yy_state_117)
    NT_type_ => Shift(yy_state_120)
    NT_separated_nonempty_list_COMMA_trait_method_parameter_ => Shift(yy_state_121)
    NT_separated_list_COMMA_trait_method_parameter_ => Shift(yy_state_122)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_trait_method_parameter_, yy_action_25)
    _ => Error
  }
}

// [169, separated_nonempty_list(COMMA,trait_method_parameter) → trait_method_parameter •, RPAREN]
// [170, separated_nonempty_list(COMMA,trait_method_parameter) → trait_method_parameter • COMMA separated_nonempty_list(COMMA,trait_method_parameter), RPAREN]
fn yy_state_113(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_114)
    T_RPAREN => Reduce(1, NT_separated_nonempty_list_COMMA_trait_method_parameter_, yy_action_169)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [102, trait_method_parameter → • type_, RPAREN / COMMA]
// [103, trait_method_parameter → • post_label COLON type_, RPAREN / COMMA]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [122, post_label → • POST_LABEL, COLON]
// [169, separated_nonempty_list(COMMA,trait_method_parameter) → • trait_method_parameter, RPAREN]
// [170, separated_nonempty_list(COMMA,trait_method_parameter) → • trait_method_parameter COMMA separated_nonempty_list(COMMA,trait_method_parameter), RPAREN]
// [170, separated_nonempty_list(COMMA,trait_method_parameter) → trait_method_parameter COMMA • separated_nonempty_list(COMMA,trait_method_parameter), RPAREN]
fn yy_state_114(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_trait_method_parameter => Shift(yy_state_113)
    NT_separated_nonempty_list_COMMA_trait_method_parameter_ => Shift(yy_state_115)
    T_POST_LABEL => Shift(yy_state_116)
    NT_post_label => Shift(yy_state_117)
    NT_type_ => Shift(yy_state_120)
    _ => Error
  }
}

// [170, separated_nonempty_list(COMMA,trait_method_parameter) → trait_method_parameter COMMA separated_nonempty_list(COMMA,trait_method_parameter) •, RPAREN]
fn yy_state_115(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_trait_method_parameter_, yy_action_92)
}

// [122, post_label → POST_LABEL •, COLON]
fn yy_state_116(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_post_label, yy_action_142)
}

// [103, trait_method_parameter → post_label • COLON type_, RPAREN / COMMA]
fn yy_state_117(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_118)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [103, trait_method_parameter → post_label COLON • type_, RPAREN / COMMA]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_118(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_119)
    _ => Error
  }
}

// [77, type_ → type_ • QUESTION, RPAREN / COMMA / QUESTION]
// [103, trait_method_parameter → post_label COLON type_ •, RPAREN / COMMA]
fn yy_state_119(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_RPAREN | T_COMMA => Reduce(3, NT_trait_method_parameter, yy_action_70)
    _ => Error
  }
}

// [77, type_ → type_ • QUESTION, RPAREN / COMMA / QUESTION]
// [102, trait_method_parameter → type_ •, RPAREN / COMMA]
fn yy_state_120(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_RPAREN | T_COMMA => Reduce(1, NT_trait_method_parameter, yy_action_165)
    _ => Error
  }
}

// [134, separated_list(COMMA,trait_method_parameter) → separated_nonempty_list(COMMA,trait_method_parameter) •, RPAREN]
fn yy_state_121(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_trait_method_parameter_, yy_action_53)
}

// [22, trait_method_sig → lident LPAREN separated_list(COMMA,trait_method_parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_122(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_123)
    _ => Error
  }
}

// [22, trait_method_sig → lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_123(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_124)
    _ => Error
  }
}

// [22, trait_method_sig → lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [77, type_ → • type_ QUESTION, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [89, type_ → • AMPER qualified_uident_, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [90, type_ → • UNDERSCORE, EQUAL / QUESTION / EXCLAMATION / SEMI / RBRACE]
// [91, return_type → • type_, EQUAL / SEMI / RBRACE]
// [92, return_type → • type_ EXCLAMATION, EQUAL / SEMI / RBRACE]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), EQUAL / SEMI / RBRACE]
// [117, qualified_uident_ → • UIDENT, EQUAL / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACE]
fn yy_state_124(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_61)
    NT_return_type => Shift(yy_state_125)
    _ => Error
  }
}

// [22, trait_method_sig → lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [135, boption(eq_underscore) → •, SEMI / RBRACE]
// [136, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]
fn yy_state_125(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_126)
    NT_boption_eq_underscore_ => Shift(yy_state_128)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_117)
    _ => Error
  }
}

// [136, boption(eq_underscore) → EQUAL • UNDERSCORE, SEMI / RBRACE]
fn yy_state_126(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UNDERSCORE => Shift(yy_state_127)
    _ => Error
  }
}

// [136, boption(eq_underscore) → EQUAL UNDERSCORE •, SEMI / RBRACE]
fn yy_state_127(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_boption_eq_underscore_, yy_action_8)
}

// [22, trait_method_sig → lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]
fn yy_state_128(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_trait_method_sig, yy_action_31)
}

// [51, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) • RBRACE, EOF / SEMI]
fn yy_state_129(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_130)
    _ => Error
  }
}

// [51, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE •, EOF / SEMI]
fn yy_state_130(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_trait_sig, yy_action_186)
}

// [47, impl_sig → IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [48, impl_sig → IMPL • qualified_uident FOR type_, EOF / SEMI]
// [49, impl_sig → IMPL • uident LBRACE separated_nonempty_list(SEMI,impl_method_sig) RBRACE, EOF / SEMI]
// [50, impl_sig → IMPL • uident COLONCOLON lident, EOF / SEMI]
// [69, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [115, qualified_uident → • UIDENT, FOR]
// [116, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
// [119, uident → • UIDENT, COLONCOLON / LBRACE]
fn yy_state_131(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_132)
    T_LBRACKET => Shift(yy_state_133)
    NT_uident => Shift(yy_state_147)
    NT_qualified_uident => Shift(yy_state_183)
    NT_type_params_with_constraints => Shift(yy_state_186)
    _ => Error
  }
}

// [115, qualified_uident → UIDENT •, FOR]
// [119, uident → UIDENT •, COLONCOLON / LBRACE]
fn yy_state_132(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Reduce(1, NT_qualified_uident, yy_action_134)
    T_COLONCOLON | T_LBRACE => Reduce(1, NT_uident, yy_action_119)
    _ => Error
  }
}

// [67, type_param_with_constraints → • uident, COMMA / RBRACKET]
// [68, type_param_with_constraints → • uident COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [69, type_params_with_constraints → LBRACKET • separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / LPAREN / PACKAGE_NAME]
// [119, uident → • UIDENT, COMMA / COLON / RBRACKET]
// [159, separated_list(COMMA,type_param_with_constraints) → •, RBRACKET]
// [160, separated_list(COMMA,type_param_with_constraints) → • separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
// [175, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints, RBRACKET]
// [176, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
fn yy_state_133(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_type_param_with_constraints => Shift(yy_state_134)
    NT_uident => Shift(yy_state_137)
    NT_separated_nonempty_list_COMMA_type_param_with_constraints_ => Shift(yy_state_144)
    NT_separated_list_COMMA_type_param_with_constraints_ => Shift(yy_state_145)
    T_RBRACKET => Reduce(0, NT_separated_list_COMMA_type_param_with_constraints_, yy_action_194)
    _ => Error
  }
}

// [175, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints •, RBRACKET]
// [176, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints • COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
fn yy_state_134(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_135)
    T_RBRACKET => Reduce(1, NT_separated_nonempty_list_COMMA_type_param_with_constraints_, yy_action_155)
    _ => Error
  }
}

// [67, type_param_with_constraints → • uident, COMMA / RBRACKET]
// [68, type_param_with_constraints → • uident COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [119, uident → • UIDENT, COMMA / COLON / RBRACKET]
// [175, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints, RBRACKET]
// [176, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
// [176, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints COMMA • separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
fn yy_state_135(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_type_param_with_constraints => Shift(yy_state_134)
    NT_separated_nonempty_list_COMMA_type_param_with_constraints_ => Shift(yy_state_136)
    NT_uident => Shift(yy_state_137)
    _ => Error
  }
}

// [176, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints) •, RBRACKET]
fn yy_state_136(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_type_param_with_constraints_, yy_action_105)
}

// [67, type_param_with_constraints → uident •, COMMA / RBRACKET]
// [68, type_param_with_constraints → uident • COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_137(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_138)
    T_COMMA | T_RBRACKET => Reduce(1, NT_type_param_with_constraints, yy_action_84)
    _ => Error
  }
}

// [68, type_param_with_constraints → uident COLON • separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [75, type_constraint → • qualified_uident, COMMA / PLUS / RBRACKET]
// [115, qualified_uident → • UIDENT, COMMA / PLUS / RBRACKET]
// [116, qualified_uident → • PACKAGE_NAME DOT_UIDENT, COMMA / PLUS / RBRACKET]
// [157, separated_nonempty_list(PLUS,type_constraint) → • type_constraint, COMMA / RBRACKET]
// [158, separated_nonempty_list(PLUS,type_constraint) → • type_constraint PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_138(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_type_constraint => Shift(yy_state_139)
    NT_qualified_uident => Shift(yy_state_142)
    NT_separated_nonempty_list_PLUS_type_constraint_ => Shift(yy_state_143)
    _ => Error
  }
}

// [157, separated_nonempty_list(PLUS,type_constraint) → type_constraint •, COMMA / RBRACKET]
// [158, separated_nonempty_list(PLUS,type_constraint) → type_constraint • PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_139(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS => Shift(yy_state_140)
    T_COMMA | T_RBRACKET => Reduce(1, NT_separated_nonempty_list_PLUS_type_constraint_, yy_action_121)
    _ => Error
  }
}

// [75, type_constraint → • qualified_uident, COMMA / PLUS / RBRACKET]
// [115, qualified_uident → • UIDENT, COMMA / PLUS / RBRACKET]
// [116, qualified_uident → • PACKAGE_NAME DOT_UIDENT, COMMA / PLUS / RBRACKET]
// [157, separated_nonempty_list(PLUS,type_constraint) → • type_constraint, COMMA / RBRACKET]
// [158, separated_nonempty_list(PLUS,type_constraint) → • type_constraint PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [158, separated_nonempty_list(PLUS,type_constraint) → type_constraint PLUS • separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_140(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_type_constraint => Shift(yy_state_139)
    NT_separated_nonempty_list_PLUS_type_constraint_ => Shift(yy_state_141)
    NT_qualified_uident => Shift(yy_state_142)
    _ => Error
  }
}

// [158, separated_nonempty_list(PLUS,type_constraint) → type_constraint PLUS separated_nonempty_list(PLUS,type_constraint) •, COMMA / RBRACKET]
fn yy_state_141(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_PLUS_type_constraint_, yy_action_178)
}

// [75, type_constraint → qualified_uident •, COMMA / PLUS / RBRACKET]
fn yy_state_142(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_constraint, yy_action_93)
}

// [68, type_param_with_constraints → uident COLON separated_nonempty_list(PLUS,type_constraint) •, COMMA / RBRACKET]
fn yy_state_143(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_param_with_constraints, yy_action_149)
}

// [160, separated_list(COMMA,type_param_with_constraints) → separated_nonempty_list(COMMA,type_param_with_constraints) •, RBRACKET]
fn yy_state_144(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_type_param_with_constraints_, yy_action_160)
}

// [69, type_params_with_constraints → LBRACKET separated_list(COMMA,type_param_with_constraints) • RBRACKET, UIDENT / LPAREN / PACKAGE_NAME]
fn yy_state_145(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_146)
    _ => Error
  }
}

// [69, type_params_with_constraints → LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET •, UIDENT / LPAREN / PACKAGE_NAME]
fn yy_state_146(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_params_with_constraints, yy_action_177)
}

// [49, impl_sig → IMPL uident • LBRACE separated_nonempty_list(SEMI,impl_method_sig) RBRACE, EOF / SEMI]
// [50, impl_sig → IMPL uident • COLONCOLON lident, EOF / SEMI]
fn yy_state_147(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_148)
    T_LBRACE => Shift(yy_state_150)
    _ => Error
  }
}

// [50, impl_sig → IMPL uident COLONCOLON • lident, EOF / SEMI]
// [120, lident → • LIDENT, EOF / SEMI]
fn yy_state_148(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_110)
    NT_lident => Shift(yy_state_149)
    _ => Error
  }
}

// [50, impl_sig → IMPL uident COLONCOLON lident •, EOF / SEMI]
fn yy_state_149(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_impl_sig, yy_action_153)
}

// [21, impl_method_sig → • list(ATTRIBUTE) lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [49, impl_sig → IMPL uident LBRACE • separated_nonempty_list(SEMI,impl_method_sig) RBRACE, EOF / SEMI]
// [131, list(ATTRIBUTE) → •, LIDENT]
// [132, list(ATTRIBUTE) → • ATTRIBUTE list(ATTRIBUTE), LIDENT]
// [141, separated_nonempty_list(SEMI,impl_method_sig) → • impl_method_sig, RBRACE]
// [142, separated_nonempty_list(SEMI,impl_method_sig) → • impl_method_sig SEMI separated_nonempty_list(SEMI,impl_method_sig), RBRACE]
fn yy_state_150(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_impl_method_sig => Shift(yy_state_151)
    T_ATTRIBUTE => Shift(yy_state_154)
    NT_list_ATTRIBUTE_ => Shift(yy_state_156)
    NT_separated_nonempty_list_SEMI_impl_method_sig_ => Shift(yy_state_181)
    T_LIDENT => Reduce(0, NT_list_ATTRIBUTE_, yy_action_27)
    _ => Error
  }
}

// [141, separated_nonempty_list(SEMI,impl_method_sig) → impl_method_sig •, RBRACE]
// [142, separated_nonempty_list(SEMI,impl_method_sig) → impl_method_sig • SEMI separated_nonempty_list(SEMI,impl_method_sig), RBRACE]
fn yy_state_151(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_152)
    T_RBRACE => Reduce(1, NT_separated_nonempty_list_SEMI_impl_method_sig_, yy_action_156)
    _ => Error
  }
}

// [21, impl_method_sig → • list(ATTRIBUTE) lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [131, list(ATTRIBUTE) → •, LIDENT]
// [132, list(ATTRIBUTE) → • ATTRIBUTE list(ATTRIBUTE), LIDENT]
// [141, separated_nonempty_list(SEMI,impl_method_sig) → • impl_method_sig, RBRACE]
// [142, separated_nonempty_list(SEMI,impl_method_sig) → • impl_method_sig SEMI separated_nonempty_list(SEMI,impl_method_sig), RBRACE]
// [142, separated_nonempty_list(SEMI,impl_method_sig) → impl_method_sig SEMI • separated_nonempty_list(SEMI,impl_method_sig), RBRACE]
fn yy_state_152(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_impl_method_sig => Shift(yy_state_151)
    NT_separated_nonempty_list_SEMI_impl_method_sig_ => Shift(yy_state_153)
    T_ATTRIBUTE => Shift(yy_state_154)
    NT_list_ATTRIBUTE_ => Shift(yy_state_156)
    T_LIDENT => Reduce(0, NT_list_ATTRIBUTE_, yy_action_27)
    _ => Error
  }
}

// [142, separated_nonempty_list(SEMI,impl_method_sig) → impl_method_sig SEMI separated_nonempty_list(SEMI,impl_method_sig) •, RBRACE]
fn yy_state_153(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_impl_method_sig_, yy_action_37)
}

// [131, list(ATTRIBUTE) → •, LIDENT]
// [132, list(ATTRIBUTE) → • ATTRIBUTE list(ATTRIBUTE), LIDENT]
// [132, list(ATTRIBUTE) → ATTRIBUTE • list(ATTRIBUTE), LIDENT]
fn yy_state_154(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_154)
    NT_list_ATTRIBUTE_ => Shift(yy_state_155)
    T_LIDENT => Reduce(0, NT_list_ATTRIBUTE_, yy_action_27)
    _ => Error
  }
}

// [132, list(ATTRIBUTE) → ATTRIBUTE list(ATTRIBUTE) •, LIDENT]
fn yy_state_155(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_list_ATTRIBUTE_, yy_action_144)
}

// [21, impl_method_sig → list(ATTRIBUTE) • lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [120, lident → • LIDENT, LPAREN / LBRACKET]
fn yy_state_156(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_110)
    NT_lident => Shift(yy_state_157)
    _ => Error
  }
}

// [21, impl_method_sig → list(ATTRIBUTE) lident • loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [69, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LPAREN]
// [125, loption(type_params_with_constraints) → •, LPAREN]
// [126, loption(type_params_with_constraints) → • type_params_with_constraints, LPAREN]
fn yy_state_157(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_133)
    NT_type_params_with_constraints => Shift(yy_state_158)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_159)
    T_LPAREN => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_24)
    _ => Error
  }
}

// [126, loption(type_params_with_constraints) → type_params_with_constraints •, LPAREN]
fn yy_state_158(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_loption_type_params_with_constraints_, yy_action_94)
}

// [21, impl_method_sig → list(ATTRIBUTE) lident loption(type_params_with_constraints) • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
fn yy_state_159(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_160)
    _ => Error
  }
}

// [21, impl_method_sig → list(ATTRIBUTE) lident loption(type_params_with_constraints) LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [97, parameter → • type_, RPAREN / COMMA]
// [98, parameter → • post_label COLON type_, RPAREN / COMMA]
// [99, parameter → • post_label COLON type_ EQUAL DOTDOT, RPAREN / COMMA]
// [100, parameter → • post_label COLON type_ EQUAL UNDERSCORE, RPAREN / COMMA]
// [101, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, label → • LIDENT, QUESTION]
// [122, post_label → • POST_LABEL, COLON]
// [127, separated_list(COMMA,parameter) → •, RPAREN]
// [128, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [167, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [168, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_160(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    T_POST_LABEL => Shift(yy_state_116)
    NT_parameter => Shift(yy_state_161)
    T_LIDENT => Shift(yy_state_164)
    NT_label => Shift(yy_state_165)
    NT_post_label => Shift(yy_state_169)
    NT_type_ => Shift(yy_state_175)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_176)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_177)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_63)
    _ => Error
  }
}

// [167, separated_nonempty_list(COMMA,parameter) → parameter •, RPAREN]
// [168, separated_nonempty_list(COMMA,parameter) → parameter • COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_161(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_162)
    T_RPAREN => Reduce(1, NT_separated_nonempty_list_COMMA_parameter_, yy_action_48)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [97, parameter → • type_, RPAREN / COMMA]
// [98, parameter → • post_label COLON type_, RPAREN / COMMA]
// [99, parameter → • post_label COLON type_ EQUAL DOTDOT, RPAREN / COMMA]
// [100, parameter → • post_label COLON type_ EQUAL UNDERSCORE, RPAREN / COMMA]
// [101, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, label → • LIDENT, QUESTION]
// [122, post_label → • POST_LABEL, COLON]
// [167, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [168, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
// [168, separated_nonempty_list(COMMA,parameter) → parameter COMMA • separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_162(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    T_POST_LABEL => Shift(yy_state_116)
    NT_parameter => Shift(yy_state_161)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_163)
    T_LIDENT => Shift(yy_state_164)
    NT_label => Shift(yy_state_165)
    NT_post_label => Shift(yy_state_169)
    NT_type_ => Shift(yy_state_175)
    _ => Error
  }
}

// [168, separated_nonempty_list(COMMA,parameter) → parameter COMMA separated_nonempty_list(COMMA,parameter) •, RPAREN]
fn yy_state_163(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_parameter_, yy_action_39)
}

// [121, label → LIDENT •, QUESTION]
fn yy_state_164(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_label, yy_action_23)
}

// [101, parameter → label • QUESTION COLON type_, RPAREN / COMMA]
fn yy_state_165(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_166)
    _ => Error
  }
}

// [101, parameter → label QUESTION • COLON type_, RPAREN / COMMA]
fn yy_state_166(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_167)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [101, parameter → label QUESTION COLON • type_, RPAREN / COMMA]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_167(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_168)
    _ => Error
  }
}

// [77, type_ → type_ • QUESTION, RPAREN / COMMA / QUESTION]
// [101, parameter → label QUESTION COLON type_ •, RPAREN / COMMA]
fn yy_state_168(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_RPAREN | T_COMMA => Reduce(4, NT_parameter, yy_action_187)
    _ => Error
  }
}

// [98, parameter → post_label • COLON type_, RPAREN / COMMA]
// [99, parameter → post_label • COLON type_ EQUAL DOTDOT, RPAREN / COMMA]
// [100, parameter → post_label • COLON type_ EQUAL UNDERSCORE, RPAREN / COMMA]
fn yy_state_169(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_170)
    _ => Error
  }
}

// [77, type_ → • type_ QUESTION, EQUAL / RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, EQUAL / RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, EQUAL / RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, EQUAL / RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, EQUAL / RPAREN / COMMA / QUESTION]
// [98, parameter → post_label COLON • type_, RPAREN / COMMA]
// [99, parameter → post_label COLON • type_ EQUAL DOTDOT, RPAREN / COMMA]
// [100, parameter → post_label COLON • type_ EQUAL UNDERSCORE, RPAREN / COMMA]
// [117, qualified_uident_ → • UIDENT, EQUAL / RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_170(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_171)
    _ => Error
  }
}

// [77, type_ → type_ • QUESTION, EQUAL / RPAREN / COMMA / QUESTION]
// [98, parameter → post_label COLON type_ •, RPAREN / COMMA]
// [99, parameter → post_label COLON type_ • EQUAL DOTDOT, RPAREN / COMMA]
// [100, parameter → post_label COLON type_ • EQUAL UNDERSCORE, RPAREN / COMMA]
fn yy_state_171(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EQUAL => Shift(yy_state_172)
    T_RPAREN | T_COMMA => Reduce(3, NT_parameter, yy_action_73)
    _ => Error
  }
}

// [99, parameter → post_label COLON type_ EQUAL • DOTDOT, RPAREN / COMMA]
// [100, parameter → post_label COLON type_ EQUAL • UNDERSCORE, RPAREN / COMMA]
fn yy_state_172(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UNDERSCORE => Shift(yy_state_173)
    T_DOTDOT => Shift(yy_state_174)
    _ => Error
  }
}

// [100, parameter → post_label COLON type_ EQUAL UNDERSCORE •, RPAREN / COMMA]
fn yy_state_173(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_parameter, yy_action_17)
}

// [99, parameter → post_label COLON type_ EQUAL DOTDOT •, RPAREN / COMMA]
fn yy_state_174(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_parameter, yy_action_26)
}

// [77, type_ → type_ • QUESTION, RPAREN / COMMA / QUESTION]
// [97, parameter → type_ •, RPAREN / COMMA]
fn yy_state_175(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_RPAREN | T_COMMA => Reduce(1, NT_parameter, yy_action_145)
    _ => Error
  }
}

// [128, separated_list(COMMA,parameter) → separated_nonempty_list(COMMA,parameter) •, RPAREN]
fn yy_state_176(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_parameter_, yy_action_15)
}

// [21, impl_method_sig → list(ATTRIBUTE) lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, SEMI / RBRACE]
fn yy_state_177(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_178)
    _ => Error
  }
}

// [21, impl_method_sig → list(ATTRIBUTE) lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, SEMI / RBRACE]
fn yy_state_178(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_179)
    _ => Error
  }
}

// [21, impl_method_sig → list(ATTRIBUTE) lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, SEMI / RBRACE]
// [77, type_ → • type_ QUESTION, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [89, type_ → • AMPER qualified_uident_, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [90, type_ → • UNDERSCORE, QUESTION / EXCLAMATION / SEMI / RBRACE]
// [91, return_type → • type_, SEMI / RBRACE]
// [92, return_type → • type_ EXCLAMATION, SEMI / RBRACE]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), SEMI / RBRACE]
// [117, qualified_uident_ → • UIDENT, QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, QUESTION / EXCLAMATION / SEMI / LBRACKET / RBRACE]
fn yy_state_179(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_61)
    NT_return_type => Shift(yy_state_180)
    _ => Error
  }
}

// [21, impl_method_sig → list(ATTRIBUTE) lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, SEMI / RBRACE]
fn yy_state_180(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_impl_method_sig, yy_action_137)
}

// [49, impl_sig → IMPL uident LBRACE separated_nonempty_list(SEMI,impl_method_sig) • RBRACE, EOF / SEMI]
fn yy_state_181(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_182)
    _ => Error
  }
}

// [49, impl_sig → IMPL uident LBRACE separated_nonempty_list(SEMI,impl_method_sig) RBRACE •, EOF / SEMI]
fn yy_state_182(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_impl_sig, yy_action_76)
}

// [48, impl_sig → IMPL qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_183(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_184)
    _ => Error
  }
}

// [48, impl_sig → IMPL qualified_uident FOR • type_, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_184(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_185)
    _ => Error
  }
}

// [48, impl_sig → IMPL qualified_uident FOR type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_185(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(4, NT_impl_sig, yy_action_103)
    _ => Error
  }
}

// [47, impl_sig → IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [115, qualified_uident → • UIDENT, FOR]
// [116, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_186(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_qualified_uident => Shift(yy_state_187)
    _ => Error
  }
}

// [47, impl_sig → IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_187(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_188)
    _ => Error
  }
}

// [47, impl_sig → IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_188(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_189)
    _ => Error
  }
}

// [47, impl_sig → IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_189(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(5, NT_impl_sig, yy_action_56)
    _ => Error
  }
}

// [44, type_sig → ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [119, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_190(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_191)
    _ => Error
  }
}

// [44, type_sig → ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
fn yy_state_191(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_192)
    _ => Error
  }
}

// [44, type_sig → ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [63, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [119, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [137, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [138, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [171, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_192(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_193)
    NT_uident => Shift(yy_state_196)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_213)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_214)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_172)
    _ => Error
  }
}

// [171, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor •, RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor • SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_193(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_194)
    T_RBRACE => Reduce(1, NT_separated_nonempty_list_SEMI_enum_constructor_, yy_action_68)
    _ => Error
  }
}

// [63, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [119, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [171, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor SEMI • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_194(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_193)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_195)
    NT_uident => Shift(yy_state_196)
    _ => Error
  }
}

// [172, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor) •, RBRACE]
fn yy_state_195(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_enum_constructor_, yy_action_58)
}

// [63, enum_constructor → uident • option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [151, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → •, EQUAL / SEMI / RBRACE]
// [152, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → • LPAREN separated_nonempty_list(COMMA,constructor_param) RPAREN, EQUAL / SEMI / RBRACE]
fn yy_state_196(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_197)
    NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__ => Shift(yy_state_209)
    T_EQUAL | T_SEMI | T_RBRACE => Reduce(0, NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__, yy_action_74)
    _ => Error
  }
}

// [64, constructor_param → • boption(MUTABLE) type_, RPAREN / COMMA]
// [65, constructor_param → • boption(MUTABLE) post_label COLON type_, RPAREN / COMMA]
// [149, separated_nonempty_list(COMMA,constructor_param) → • constructor_param, RPAREN]
// [150, separated_nonempty_list(COMMA,constructor_param) → • constructor_param COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [152, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN • separated_nonempty_list(COMMA,constructor_param) RPAREN, EQUAL / SEMI / RBRACE]
// [155, boption(MUTABLE) → •, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
// [156, boption(MUTABLE) → • MUTABLE, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_197(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_198)
    NT_separated_nonempty_list_COMMA_constructor_param_ => Shift(yy_state_199)
    NT_constructor_param => Shift(yy_state_201)
    NT_boption_MUTABLE_ => Shift(yy_state_204)
    T_UIDENT | T_POST_LABEL | T_ASYNC | T_LPAREN | T_UNDERSCORE | T_AMPER | T_PACKAGE_NAME => Reduce(0, NT_boption_MUTABLE_, yy_action_123)
    _ => Error
  }
}

// [156, boption(MUTABLE) → MUTABLE •, LIDENT / UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_198(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_boption_MUTABLE_, yy_action_57)
}

// [152, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN separated_nonempty_list(COMMA,constructor_param) • RPAREN, EQUAL / SEMI / RBRACE]
fn yy_state_199(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_200)
    _ => Error
  }
}

// [152, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN separated_nonempty_list(COMMA,constructor_param) RPAREN •, EQUAL / SEMI / RBRACE]
fn yy_state_200(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__, yy_action_114)
}

// [149, separated_nonempty_list(COMMA,constructor_param) → constructor_param •, RPAREN]
// [150, separated_nonempty_list(COMMA,constructor_param) → constructor_param • COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
fn yy_state_201(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_202)
    T_RPAREN => Reduce(1, NT_separated_nonempty_list_COMMA_constructor_param_, yy_action_192)
    _ => Error
  }
}

// [64, constructor_param → • boption(MUTABLE) type_, RPAREN / COMMA]
// [65, constructor_param → • boption(MUTABLE) post_label COLON type_, RPAREN / COMMA]
// [149, separated_nonempty_list(COMMA,constructor_param) → • constructor_param, RPAREN]
// [150, separated_nonempty_list(COMMA,constructor_param) → • constructor_param COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [150, separated_nonempty_list(COMMA,constructor_param) → constructor_param COMMA • separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [155, boption(MUTABLE) → •, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
// [156, boption(MUTABLE) → • MUTABLE, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_202(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_198)
    NT_constructor_param => Shift(yy_state_201)
    NT_separated_nonempty_list_COMMA_constructor_param_ => Shift(yy_state_203)
    NT_boption_MUTABLE_ => Shift(yy_state_204)
    T_UIDENT | T_POST_LABEL | T_ASYNC | T_LPAREN | T_UNDERSCORE | T_AMPER | T_PACKAGE_NAME => Reduce(0, NT_boption_MUTABLE_, yy_action_123)
    _ => Error
  }
}

// [150, separated_nonempty_list(COMMA,constructor_param) → constructor_param COMMA separated_nonempty_list(COMMA,constructor_param) •, RPAREN]
fn yy_state_203(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_constructor_param_, yy_action_173)
}

// [64, constructor_param → boption(MUTABLE) • type_, RPAREN / COMMA]
// [65, constructor_param → boption(MUTABLE) • post_label COLON type_, RPAREN / COMMA]
// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [122, post_label → • POST_LABEL, COLON]
fn yy_state_204(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    T_POST_LABEL => Shift(yy_state_116)
    NT_post_label => Shift(yy_state_205)
    NT_type_ => Shift(yy_state_208)
    _ => Error
  }
}

// [65, constructor_param → boption(MUTABLE) post_label • COLON type_, RPAREN / COMMA]
fn yy_state_205(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_206)
    _ => Error
  }
}

// [65, constructor_param → boption(MUTABLE) post_label COLON • type_, RPAREN / COMMA]
// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_206(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_207)
    _ => Error
  }
}

// [65, constructor_param → boption(MUTABLE) post_label COLON type_ •, RPAREN / COMMA]
// [77, type_ → type_ • QUESTION, RPAREN / COMMA / QUESTION]
fn yy_state_207(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_RPAREN | T_COMMA => Reduce(4, NT_constructor_param, yy_action_89)
    _ => Error
  }
}

// [64, constructor_param → boption(MUTABLE) type_ •, RPAREN / COMMA]
// [77, type_ → type_ • QUESTION, RPAREN / COMMA / QUESTION]
fn yy_state_208(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_RPAREN | T_COMMA => Reduce(2, NT_constructor_param, yy_action_133)
    _ => Error
  }
}

// [63, enum_constructor → uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) • option(eq_tag), SEMI / RBRACE]
// [153, option(eq_tag) → •, SEMI / RBRACE]
// [154, option(eq_tag) → • EQUAL INT, SEMI / RBRACE]
fn yy_state_209(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_210)
    NT_option_eq_tag_ => Shift(yy_state_212)
    T_SEMI | T_RBRACE => Reduce(0, NT_option_eq_tag_, yy_action_131)
    _ => Error
  }
}

// [154, option(eq_tag) → EQUAL • INT, SEMI / RBRACE]
fn yy_state_210(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_INT => Shift(yy_state_211)
    _ => Error
  }
}

// [154, option(eq_tag) → EQUAL INT •, SEMI / RBRACE]
fn yy_state_211(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_option_eq_tag_, yy_action_43)
}

// [63, enum_constructor → uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag) •, SEMI / RBRACE]
fn yy_state_212(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_enum_constructor, yy_action_174)
}

// [138, separated_list(SEMI,enum_constructor) → separated_nonempty_list(SEMI,enum_constructor) •, RBRACE]
fn yy_state_213(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_SEMI_enum_constructor_, yy_action_191)
}

// [44, type_sig → ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_214(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_215)
    _ => Error
  }
}

// [44, type_sig → ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_215(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_28)
}

// [41, type_sig → STRUCT • type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [119, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_216(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_217)
    _ => Error
  }
}

// [41, type_sig → STRUCT type_decl_name_with_params • LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
fn yy_state_217(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_218)
    _ => Error
  }
}

// [41, type_sig → STRUCT type_decl_name_with_params LBRACE • separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [66, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [139, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [140, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [155, boption(MUTABLE) → •, LIDENT]
// [156, boption(MUTABLE) → • MUTABLE, LIDENT]
// [173, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [174, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_218(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_198)
    NT_record_decl_field => Shift(yy_state_219)
    NT_boption_MUTABLE_ => Shift(yy_state_222)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_226)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_227)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_110)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_123)
    _ => Error
  }
}

// [173, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field •, RBRACE]
// [174, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field • SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_219(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_220)
    T_RBRACE => Reduce(1, NT_separated_nonempty_list_SEMI_record_decl_field_, yy_action_184)
    _ => Error
  }
}

// [66, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [155, boption(MUTABLE) → •, LIDENT]
// [156, boption(MUTABLE) → • MUTABLE, LIDENT]
// [173, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [174, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [174, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field SEMI • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_220(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_198)
    NT_record_decl_field => Shift(yy_state_219)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_221)
    NT_boption_MUTABLE_ => Shift(yy_state_222)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_123)
    _ => Error
  }
}

// [174, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field) •, RBRACE]
fn yy_state_221(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_record_decl_field_, yy_action_138)
}

// [66, record_decl_field → boption(MUTABLE) • lident COLON type_, SEMI / RBRACE]
// [120, lident → • LIDENT, COLON]
fn yy_state_222(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_110)
    NT_lident => Shift(yy_state_223)
    _ => Error
  }
}

// [66, record_decl_field → boption(MUTABLE) lident • COLON type_, SEMI / RBRACE]
fn yy_state_223(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_224)
    _ => Error
  }
}

// [66, record_decl_field → boption(MUTABLE) lident COLON • type_, SEMI / RBRACE]
// [77, type_ → • type_ QUESTION, QUESTION / SEMI / RBRACE]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, QUESTION / SEMI / RBRACE]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, QUESTION / SEMI / RBRACE]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, QUESTION / SEMI / RBRACE]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, QUESTION / SEMI / RBRACE]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, QUESTION / SEMI / RBRACE]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, QUESTION / SEMI / RBRACE]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, QUESTION / SEMI / RBRACE]
// [85, type_ → • LPAREN type_ RPAREN, QUESTION / SEMI / RBRACE]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, QUESTION / SEMI / RBRACE]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, QUESTION / SEMI / RBRACE]
// [88, type_ → • qualified_uident_ optional_type_arguments, QUESTION / SEMI / RBRACE]
// [89, type_ → • AMPER qualified_uident_, QUESTION / SEMI / RBRACE]
// [90, type_ → • UNDERSCORE, QUESTION / SEMI / RBRACE]
// [117, qualified_uident_ → • UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]
fn yy_state_224(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_225)
    _ => Error
  }
}

// [66, record_decl_field → boption(MUTABLE) lident COLON type_ •, SEMI / RBRACE]
// [77, type_ → type_ • QUESTION, QUESTION / SEMI / RBRACE]
fn yy_state_225(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_SEMI | T_RBRACE => Reduce(4, NT_record_decl_field, yy_action_77)
    _ => Error
  }
}

// [140, separated_list(SEMI,record_decl_field) → separated_nonempty_list(SEMI,record_decl_field) •, RBRACE]
fn yy_state_226(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_SEMI_record_decl_field_, yy_action_54)
}

// [41, type_sig → STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) • RBRACE, EOF / SEMI]
fn yy_state_227(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_228)
    _ => Error
  }
}

// [41, type_sig → STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE •, EOF / SEMI]
fn yy_state_228(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_10)
}

// [26, type_sig → TYPE • type_decl_name_with_params, EOF / SEMI]
// [29, type_sig → TYPE • type_decl_name_with_params type_, EOF / SEMI]
// [32, type_sig → TYPE • EXCLAMATION uident, EOF / SEMI]
// [35, type_sig → TYPE • EXCLAMATION uident type_, EOF / SEMI]
// [38, type_sig → TYPE • EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / AMPER / PACKAGE_NAME]
// [119, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / LBRACKET / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_229(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    T_EXCLAMATION => Shift(yy_state_230)
    NT_type_decl_name_with_params => Shift(yy_state_236)
    _ => Error
  }
}

// [32, type_sig → TYPE EXCLAMATION • uident, EOF / SEMI]
// [35, type_sig → TYPE EXCLAMATION • uident type_, EOF / SEMI]
// [38, type_sig → TYPE EXCLAMATION • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [119, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_230(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_231)
    _ => Error
  }
}

// [32, type_sig → TYPE EXCLAMATION uident •, EOF / SEMI]
// [35, type_sig → TYPE EXCLAMATION uident • type_, EOF / SEMI]
// [38, type_sig → TYPE EXCLAMATION uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_231(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    T_LBRACE => Shift(yy_state_232)
    NT_type_ => Shift(yy_state_235)
    T_EOF | T_SEMI => Reduce(3, NT_type_sig, yy_action_143)
    _ => Error
  }
}

// [38, type_sig → TYPE EXCLAMATION uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [63, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [119, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [137, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [138, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [171, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_232(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_193)
    NT_uident => Shift(yy_state_196)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_213)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_233)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_172)
    _ => Error
  }
}

// [38, type_sig → TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_233(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_234)
    _ => Error
  }
}

// [38, type_sig → TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_234(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_21)
}

// [35, type_sig → TYPE EXCLAMATION uident type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_235(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(4, NT_type_sig, yy_action_45)
    _ => Error
  }
}

// [26, type_sig → TYPE type_decl_name_with_params •, EOF / SEMI]
// [29, type_sig → TYPE type_decl_name_with_params • type_, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_236(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_237)
    T_EOF | T_SEMI => Reduce(2, NT_type_sig, yy_action_46)
    _ => Error
  }
}

// [29, type_sig → TYPE type_decl_name_with_params type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_237(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(3, NT_type_sig, yy_action_80)
    _ => Error
  }
}

// [25, type_sig → PUB • pub_attr EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [28, type_sig → PUB • pub_attr TYPE type_decl_name_with_params, EOF / SEMI]
// [31, type_sig → PUB • pub_attr TYPE type_decl_name_with_params type_, EOF / SEMI]
// [34, type_sig → PUB • pub_attr TYPE EXCLAMATION uident, EOF / SEMI]
// [37, type_sig → PUB • pub_attr TYPE EXCLAMATION uident type_, EOF / SEMI]
// [40, type_sig → PUB • pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [43, type_sig → PUB • pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [46, type_sig → PUB • pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [53, trait_sig → PUB • pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [56, trait_sig → PUB • pub_attr TRAIT uident, EOF / SEMI]
// [59, alias_sig → PUB • pub_attr TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [62, alias_sig → PUB • pub_attr TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [112, pub_attr → •, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
// [113, pub_attr → • LPAREN READONLY RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
// [114, pub_attr → • LPAREN LIDENT RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_238(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_239)
    NT_pub_attr => Shift(yy_state_244)
    T_EXTERN | T_STRUCT | T_ENUM | T_TRAIT | T_TYPEALIAS | T_TRAITALIAS | T_TYPE => Reduce(0, NT_pub_attr, yy_action_112)
    _ => Error
  }
}

// [113, pub_attr → LPAREN • READONLY RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
// [114, pub_attr → LPAREN • LIDENT RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_239(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_240)
    T_READONLY => Shift(yy_state_242)
    _ => Error
  }
}

// [114, pub_attr → LPAREN LIDENT • RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_240(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_241)
    _ => Error
  }
}

// [114, pub_attr → LPAREN LIDENT RPAREN •, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_241(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_pub_attr, yy_action_163)
}

// [113, pub_attr → LPAREN READONLY • RPAREN, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_242(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_243)
    _ => Error
  }
}

// [113, pub_attr → LPAREN READONLY RPAREN •, EXTERN / STRUCT / ENUM / TRAIT / TYPEALIAS / TRAITALIAS / TYPE]
fn yy_state_243(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_pub_attr, yy_action_18)
}

// [25, type_sig → PUB pub_attr • EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [28, type_sig → PUB pub_attr • TYPE type_decl_name_with_params, EOF / SEMI]
// [31, type_sig → PUB pub_attr • TYPE type_decl_name_with_params type_, EOF / SEMI]
// [34, type_sig → PUB pub_attr • TYPE EXCLAMATION uident, EOF / SEMI]
// [37, type_sig → PUB pub_attr • TYPE EXCLAMATION uident type_, EOF / SEMI]
// [40, type_sig → PUB pub_attr • TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [43, type_sig → PUB pub_attr • STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [46, type_sig → PUB pub_attr • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [53, trait_sig → PUB pub_attr • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [56, trait_sig → PUB pub_attr • TRAIT uident, EOF / SEMI]
// [59, alias_sig → PUB pub_attr • TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [62, alias_sig → PUB pub_attr • TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
fn yy_state_244(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAITALIAS => Shift(yy_state_245)
    T_TYPEALIAS => Shift(yy_state_249)
    T_TRAIT => Shift(yy_state_253)
    T_ENUM => Shift(yy_state_259)
    T_STRUCT => Shift(yy_state_264)
    T_TYPE => Shift(yy_state_269)
    T_EXTERN => Shift(yy_state_278)
    _ => Error
  }
}

// [62, alias_sig → PUB pub_attr TRAITALIAS • uident EQUAL qualified_uident, EOF / SEMI]
// [119, uident → • UIDENT, EQUAL]
fn yy_state_245(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_246)
    _ => Error
  }
}

// [62, alias_sig → PUB pub_attr TRAITALIAS uident • EQUAL qualified_uident, EOF / SEMI]
fn yy_state_246(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_247)
    _ => Error
  }
}

// [62, alias_sig → PUB pub_attr TRAITALIAS uident EQUAL • qualified_uident, EOF / SEMI]
// [115, qualified_uident → • UIDENT, EOF / SEMI]
// [116, qualified_uident → • PACKAGE_NAME DOT_UIDENT, EOF / SEMI]
fn yy_state_247(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_qualified_uident => Shift(yy_state_248)
    _ => Error
  }
}

// [62, alias_sig → PUB pub_attr TRAITALIAS uident EQUAL qualified_uident •, EOF / SEMI]
fn yy_state_248(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_79)
}

// [59, alias_sig → PUB pub_attr TYPEALIAS • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, EQUAL]
// [119, uident → • UIDENT, EQUAL / LBRACKET]
fn yy_state_249(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_250)
    _ => Error
  }
}

// [59, alias_sig → PUB pub_attr TYPEALIAS type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_250(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_251)
    _ => Error
  }
}

// [59, alias_sig → PUB pub_attr TYPEALIAS type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_251(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_252)
    _ => Error
  }
}

// [59, alias_sig → PUB pub_attr TYPEALIAS type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_252(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(6, NT_alias_sig, yy_action_95)
    _ => Error
  }
}

// [53, trait_sig → PUB pub_attr TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [56, trait_sig → PUB pub_attr TRAIT • uident, EOF / SEMI]
// [119, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_253(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_254)
    _ => Error
  }
}

// [53, trait_sig → PUB pub_attr TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [56, trait_sig → PUB pub_attr TRAIT uident •, EOF / SEMI]
// [145, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, LBRACE]
// [146, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_254(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_100)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_255)
    T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_111)
    T_EOF | T_SEMI => Reduce(4, NT_trait_sig, yy_action_162)
    _ => Error
  }
}

// [53, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_255(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_256)
    _ => Error
  }
}

// [22, trait_method_sig → • lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [53, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE • separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [120, lident → • LIDENT, LPAREN]
// [147, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [148, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_256(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_trait_method_sig => Shift(yy_state_107)
    T_LIDENT => Shift(yy_state_110)
    NT_lident => Shift(yy_state_111)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_257)
    _ => Error
  }
}

// [53, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) • RBRACE, EOF / SEMI]
fn yy_state_257(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_258)
    _ => Error
  }
}

// [53, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE •, EOF / SEMI]
fn yy_state_258(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_trait_sig, yy_action_127)
}

// [46, type_sig → PUB pub_attr ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [119, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_259(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_260)
    _ => Error
  }
}

// [46, type_sig → PUB pub_attr ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
fn yy_state_260(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_261)
    _ => Error
  }
}

// [46, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [63, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [119, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [137, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [138, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [171, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_261(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_193)
    NT_uident => Shift(yy_state_196)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_213)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_262)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_172)
    _ => Error
  }
}

// [46, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_262(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_263)
    _ => Error
  }
}

// [46, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_263(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_67)
}

// [43, type_sig → PUB pub_attr STRUCT • type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [119, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_264(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_265)
    _ => Error
  }
}

// [43, type_sig → PUB pub_attr STRUCT type_decl_name_with_params • LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
fn yy_state_265(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_266)
    _ => Error
  }
}

// [43, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE • separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [66, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [139, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [140, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [155, boption(MUTABLE) → •, LIDENT]
// [156, boption(MUTABLE) → • MUTABLE, LIDENT]
// [173, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [174, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_266(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_198)
    NT_record_decl_field => Shift(yy_state_219)
    NT_boption_MUTABLE_ => Shift(yy_state_222)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_226)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_267)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_110)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_123)
    _ => Error
  }
}

// [43, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) • RBRACE, EOF / SEMI]
fn yy_state_267(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_268)
    _ => Error
  }
}

// [43, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE •, EOF / SEMI]
fn yy_state_268(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_65)
}

// [28, type_sig → PUB pub_attr TYPE • type_decl_name_with_params, EOF / SEMI]
// [31, type_sig → PUB pub_attr TYPE • type_decl_name_with_params type_, EOF / SEMI]
// [34, type_sig → PUB pub_attr TYPE • EXCLAMATION uident, EOF / SEMI]
// [37, type_sig → PUB pub_attr TYPE • EXCLAMATION uident type_, EOF / SEMI]
// [40, type_sig → PUB pub_attr TYPE • EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / AMPER / PACKAGE_NAME]
// [119, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / LBRACKET / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_269(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    T_EXCLAMATION => Shift(yy_state_270)
    NT_type_decl_name_with_params => Shift(yy_state_276)
    _ => Error
  }
}

// [34, type_sig → PUB pub_attr TYPE EXCLAMATION • uident, EOF / SEMI]
// [37, type_sig → PUB pub_attr TYPE EXCLAMATION • uident type_, EOF / SEMI]
// [40, type_sig → PUB pub_attr TYPE EXCLAMATION • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [119, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_270(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_271)
    _ => Error
  }
}

// [34, type_sig → PUB pub_attr TYPE EXCLAMATION uident •, EOF / SEMI]
// [37, type_sig → PUB pub_attr TYPE EXCLAMATION uident • type_, EOF / SEMI]
// [40, type_sig → PUB pub_attr TYPE EXCLAMATION uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_271(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    T_LBRACE => Shift(yy_state_272)
    NT_type_ => Shift(yy_state_275)
    T_EOF | T_SEMI => Reduce(5, NT_type_sig, yy_action_12)
    _ => Error
  }
}

// [40, type_sig → PUB pub_attr TYPE EXCLAMATION uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [63, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [119, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [137, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [138, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [171, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_272(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_193)
    NT_uident => Shift(yy_state_196)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_213)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_273)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_172)
    _ => Error
  }
}

// [40, type_sig → PUB pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_273(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_274)
    _ => Error
  }
}

// [40, type_sig → PUB pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_274(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_69)
}

// [37, type_sig → PUB pub_attr TYPE EXCLAMATION uident type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_275(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(6, NT_type_sig, yy_action_128)
    _ => Error
  }
}

// [28, type_sig → PUB pub_attr TYPE type_decl_name_with_params •, EOF / SEMI]
// [31, type_sig → PUB pub_attr TYPE type_decl_name_with_params • type_, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_276(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_277)
    T_EOF | T_SEMI => Reduce(4, NT_type_sig, yy_action_47)
    _ => Error
  }
}

// [31, type_sig → PUB pub_attr TYPE type_decl_name_with_params type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_277(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(5, NT_type_sig, yy_action_146)
    _ => Error
  }
}

// [25, type_sig → PUB pub_attr EXTERN • TYPE type_decl_name_with_params, EOF / SEMI]
fn yy_state_278(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TYPE => Shift(yy_state_279)
    _ => Error
  }
}

// [25, type_sig → PUB pub_attr EXTERN TYPE • type_decl_name_with_params, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / SEMI]
// [119, uident → • UIDENT, EOF / SEMI / LBRACKET]
fn yy_state_279(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_280)
    _ => Error
  }
}

// [25, type_sig → PUB pub_attr EXTERN TYPE type_decl_name_with_params •, EOF / SEMI]
fn yy_state_280(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_81)
}

// [24, type_sig → PRIV • EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [27, type_sig → PRIV • TYPE type_decl_name_with_params, EOF / SEMI]
// [30, type_sig → PRIV • TYPE type_decl_name_with_params type_, EOF / SEMI]
// [33, type_sig → PRIV • TYPE EXCLAMATION uident, EOF / SEMI]
// [36, type_sig → PRIV • TYPE EXCLAMATION uident type_, EOF / SEMI]
// [39, type_sig → PRIV • TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [42, type_sig → PRIV • STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [45, type_sig → PRIV • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [52, trait_sig → PRIV • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [55, trait_sig → PRIV • TRAIT uident, EOF / SEMI]
// [58, alias_sig → PRIV • TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [61, alias_sig → PRIV • TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
fn yy_state_281(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAITALIAS => Shift(yy_state_282)
    T_TYPEALIAS => Shift(yy_state_286)
    T_TRAIT => Shift(yy_state_290)
    T_ENUM => Shift(yy_state_296)
    T_STRUCT => Shift(yy_state_301)
    T_TYPE => Shift(yy_state_306)
    T_EXTERN => Shift(yy_state_315)
    _ => Error
  }
}

// [61, alias_sig → PRIV TRAITALIAS • uident EQUAL qualified_uident, EOF / SEMI]
// [119, uident → • UIDENT, EQUAL]
fn yy_state_282(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_283)
    _ => Error
  }
}

// [61, alias_sig → PRIV TRAITALIAS uident • EQUAL qualified_uident, EOF / SEMI]
fn yy_state_283(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_284)
    _ => Error
  }
}

// [61, alias_sig → PRIV TRAITALIAS uident EQUAL • qualified_uident, EOF / SEMI]
// [115, qualified_uident → • UIDENT, EOF / SEMI]
// [116, qualified_uident → • PACKAGE_NAME DOT_UIDENT, EOF / SEMI]
fn yy_state_284(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_23)
    T_UIDENT => Shift(yy_state_25)
    NT_qualified_uident => Shift(yy_state_285)
    _ => Error
  }
}

// [61, alias_sig → PRIV TRAITALIAS uident EQUAL qualified_uident •, EOF / SEMI]
fn yy_state_285(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_168)
}

// [58, alias_sig → PRIV TYPEALIAS • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, EQUAL]
// [119, uident → • UIDENT, EQUAL / LBRACKET]
fn yy_state_286(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_287)
    _ => Error
  }
}

// [58, alias_sig → PRIV TYPEALIAS type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_287(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_288)
    _ => Error
  }
}

// [58, alias_sig → PRIV TYPEALIAS type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_288(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_289)
    _ => Error
  }
}

// [58, alias_sig → PRIV TYPEALIAS type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_289(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(5, NT_alias_sig, yy_action_16)
    _ => Error
  }
}

// [52, trait_sig → PRIV TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [55, trait_sig → PRIV TRAIT • uident, EOF / SEMI]
// [119, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_290(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_291)
    _ => Error
  }
}

// [52, trait_sig → PRIV TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [55, trait_sig → PRIV TRAIT uident •, EOF / SEMI]
// [145, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, LBRACE]
// [146, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), LBRACE]
fn yy_state_291(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_100)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_292)
    T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_111)
    T_EOF | T_SEMI => Reduce(3, NT_trait_sig, yy_action_164)
    _ => Error
  }
}

// [52, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_292(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_293)
    _ => Error
  }
}

// [22, trait_method_sig → • lident LPAREN separated_list(COMMA,trait_method_parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [52, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE • separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [120, lident → • LIDENT, LPAREN]
// [147, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [148, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_293(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_trait_method_sig => Shift(yy_state_107)
    T_LIDENT => Shift(yy_state_110)
    NT_lident => Shift(yy_state_111)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_294)
    _ => Error
  }
}

// [52, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) • RBRACE, EOF / SEMI]
fn yy_state_294(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_295)
    _ => Error
  }
}

// [52, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE •, EOF / SEMI]
fn yy_state_295(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_trait_sig, yy_action_171)
}

// [45, type_sig → PRIV ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [119, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_296(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_297)
    _ => Error
  }
}

// [45, type_sig → PRIV ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
fn yy_state_297(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_298)
    _ => Error
  }
}

// [45, type_sig → PRIV ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [63, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [119, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [137, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [138, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [171, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_298(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_193)
    NT_uident => Shift(yy_state_196)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_213)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_299)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_172)
    _ => Error
  }
}

// [45, type_sig → PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_299(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_300)
    _ => Error
  }
}

// [45, type_sig → PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_300(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_19)
}

// [42, type_sig → PRIV STRUCT • type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [119, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_301(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_302)
    _ => Error
  }
}

// [42, type_sig → PRIV STRUCT type_decl_name_with_params • LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
fn yy_state_302(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_303)
    _ => Error
  }
}

// [42, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE • separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [66, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [139, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [140, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [155, boption(MUTABLE) → •, LIDENT]
// [156, boption(MUTABLE) → • MUTABLE, LIDENT]
// [173, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [174, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_303(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_198)
    NT_record_decl_field => Shift(yy_state_219)
    NT_boption_MUTABLE_ => Shift(yy_state_222)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_226)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_304)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_110)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_123)
    _ => Error
  }
}

// [42, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) • RBRACE, EOF / SEMI]
fn yy_state_304(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_305)
    _ => Error
  }
}

// [42, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE •, EOF / SEMI]
fn yy_state_305(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_33)
}

// [27, type_sig → PRIV TYPE • type_decl_name_with_params, EOF / SEMI]
// [30, type_sig → PRIV TYPE • type_decl_name_with_params type_, EOF / SEMI]
// [33, type_sig → PRIV TYPE • EXCLAMATION uident, EOF / SEMI]
// [36, type_sig → PRIV TYPE • EXCLAMATION uident type_, EOF / SEMI]
// [39, type_sig → PRIV TYPE • EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / AMPER / PACKAGE_NAME]
// [119, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / LBRACKET / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_306(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    T_EXCLAMATION => Shift(yy_state_307)
    NT_type_decl_name_with_params => Shift(yy_state_313)
    _ => Error
  }
}

// [33, type_sig → PRIV TYPE EXCLAMATION • uident, EOF / SEMI]
// [36, type_sig → PRIV TYPE EXCLAMATION • uident type_, EOF / SEMI]
// [39, type_sig → PRIV TYPE EXCLAMATION • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [119, uident → • UIDENT, UIDENT / EOF / ASYNC / LPAREN / SEMI / UNDERSCORE / LBRACE / AMPER / PACKAGE_NAME]
fn yy_state_307(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_308)
    _ => Error
  }
}

// [33, type_sig → PRIV TYPE EXCLAMATION uident •, EOF / SEMI]
// [36, type_sig → PRIV TYPE EXCLAMATION uident • type_, EOF / SEMI]
// [39, type_sig → PRIV TYPE EXCLAMATION uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_308(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    T_LBRACE => Shift(yy_state_309)
    NT_type_ => Shift(yy_state_312)
    T_EOF | T_SEMI => Reduce(4, NT_type_sig, yy_action_83)
    _ => Error
  }
}

// [39, type_sig → PRIV TYPE EXCLAMATION uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [63, enum_constructor → • uident option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [119, uident → • UIDENT, EQUAL / LPAREN / SEMI / RBRACE]
// [137, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [138, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [171, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [172, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_309(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_enum_constructor => Shift(yy_state_193)
    NT_uident => Shift(yy_state_196)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_213)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_310)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_172)
    _ => Error
  }
}

// [39, type_sig → PRIV TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE, EOF / SEMI]
fn yy_state_310(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_311)
    _ => Error
  }
}

// [39, type_sig → PRIV TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE •, EOF / SEMI]
fn yy_state_311(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_132)
}

// [36, type_sig → PRIV TYPE EXCLAMATION uident type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_312(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(5, NT_type_sig, yy_action_157)
    _ => Error
  }
}

// [27, type_sig → PRIV TYPE type_decl_name_with_params •, EOF / SEMI]
// [30, type_sig → PRIV TYPE type_decl_name_with_params • type_, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_313(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_314)
    T_EOF | T_SEMI => Reduce(3, NT_type_sig, yy_action_71)
    _ => Error
  }
}

// [30, type_sig → PRIV TYPE type_decl_name_with_params type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_314(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(4, NT_type_sig, yy_action_1)
    _ => Error
  }
}

// [24, type_sig → PRIV EXTERN • TYPE type_decl_name_with_params, EOF / SEMI]
fn yy_state_315(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TYPE => Shift(yy_state_316)
    _ => Error
  }
}

// [24, type_sig → PRIV EXTERN TYPE • type_decl_name_with_params, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / SEMI]
// [119, uident → • UIDENT, EOF / SEMI / LBRACKET]
fn yy_state_316(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_317)
    _ => Error
  }
}

// [24, type_sig → PRIV EXTERN TYPE type_decl_name_with_params •, EOF / SEMI]
fn yy_state_317(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_36)
}

// [23, type_sig → EXTERN • TYPE type_decl_name_with_params, EOF / SEMI]
fn yy_state_318(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TYPE => Shift(yy_state_319)
    _ => Error
  }
}

// [23, type_sig → EXTERN TYPE • type_decl_name_with_params, EOF / SEMI]
// [76, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / SEMI]
// [119, uident → • UIDENT, EOF / SEMI / LBRACKET]
fn yy_state_319(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_28)
    NT_type_decl_name_with_params => Shift(yy_state_320)
    _ => Error
  }
}

// [23, type_sig → EXTERN TYPE type_decl_name_with_params •, EOF / SEMI]
fn yy_state_320(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_sig, yy_action_189)
}

// [18, func_sig_no_attr → • FN lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [20, func_sig → nonempty_list(ATTRIBUTE) • func_sig_no_attr, EOF / SEMI]
fn yy_state_321(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_func_sig_no_attr => Shift(yy_state_322)
    T_FN => Shift(yy_state_323)
    _ => Error
  }
}

// [20, func_sig → nonempty_list(ATTRIBUTE) func_sig_no_attr •, EOF / SEMI]
fn yy_state_322(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_func_sig, yy_action_154)
}

// [18, func_sig_no_attr → FN • lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [120, lident → • LIDENT, LPAREN / LBRACKET]
fn yy_state_323(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_110)
    NT_lident => Shift(yy_state_324)
    _ => Error
  }
}

// [18, func_sig_no_attr → FN lident • loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [69, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LPAREN]
// [125, loption(type_params_with_constraints) → •, LPAREN]
// [126, loption(type_params_with_constraints) → • type_params_with_constraints, LPAREN]
fn yy_state_324(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_133)
    NT_type_params_with_constraints => Shift(yy_state_158)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_325)
    T_LPAREN => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_24)
    _ => Error
  }
}

// [18, func_sig_no_attr → FN lident loption(type_params_with_constraints) • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_325(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_326)
    _ => Error
  }
}

// [18, func_sig_no_attr → FN lident loption(type_params_with_constraints) LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [77, type_ → • type_ QUESTION, RPAREN / COMMA / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [89, type_ → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [90, type_ → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [97, parameter → • type_, RPAREN / COMMA]
// [98, parameter → • post_label COLON type_, RPAREN / COMMA]
// [99, parameter → • post_label COLON type_ EQUAL DOTDOT, RPAREN / COMMA]
// [100, parameter → • post_label COLON type_ EQUAL UNDERSCORE, RPAREN / COMMA]
// [101, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [117, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [121, label → • LIDENT, QUESTION]
// [122, post_label → • POST_LABEL, COLON]
// [127, separated_list(COMMA,parameter) → •, RPAREN]
// [128, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [167, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [168, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_326(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    T_POST_LABEL => Shift(yy_state_116)
    NT_parameter => Shift(yy_state_161)
    T_LIDENT => Shift(yy_state_164)
    NT_label => Shift(yy_state_165)
    NT_post_label => Shift(yy_state_169)
    NT_type_ => Shift(yy_state_175)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_176)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_327)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_63)
    _ => Error
  }
}

// [18, func_sig_no_attr → FN lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_327(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_328)
    _ => Error
  }
}

// [18, func_sig_no_attr → FN lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_328(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_329)
    _ => Error
  }
}

// [18, func_sig_no_attr → FN lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / EXCLAMATION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / EXCLAMATION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / EXCLAMATION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / EXCLAMATION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / EXCLAMATION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / EXCLAMATION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / EXCLAMATION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / EXCLAMATION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / EXCLAMATION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / EXCLAMATION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / EXCLAMATION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / EXCLAMATION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / EXCLAMATION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / EXCLAMATION / SEMI]
// [91, return_type → • type_, EOF / SEMI]
// [92, return_type → • type_ EXCLAMATION, EOF / SEMI]
// [93, return_type → • type_ EXCLAMATION separated_nonempty_list(PLUS,error_type), EOF / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / EXCLAMATION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / EXCLAMATION / SEMI / LBRACKET]
fn yy_state_329(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_61)
    NT_return_type => Shift(yy_state_330)
    _ => Error
  }
}

// [18, func_sig_no_attr → FN lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_330(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_func_sig_no_attr, yy_action_158)
}

// [19, func_sig → func_sig_no_attr •, EOF / SEMI]
fn yy_state_331(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_func_sig, yy_action_88)
}

// [17, value_sig → LET • lident COLON type_, EOF / SEMI]
// [120, lident → • LIDENT, COLON]
fn yy_state_332(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_110)
    NT_lident => Shift(yy_state_333)
    _ => Error
  }
}

// [17, value_sig → LET lident • COLON type_, EOF / SEMI]
fn yy_state_333(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_334)
    _ => Error
  }
}

// [17, value_sig → LET lident COLON • type_, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EOF / QUESTION / SEMI]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [85, type_ → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / QUESTION / SEMI]
// [88, type_ → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [89, type_ → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [90, type_ → • UNDERSCORE, EOF / QUESTION / SEMI]
// [117, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_334(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_335)
    _ => Error
  }
}

// [17, value_sig → LET lident COLON type_ •, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EOF / QUESTION / SEMI]
fn yy_state_335(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EOF | T_SEMI => Reduce(4, NT_value_sig, yy_action_61)
    _ => Error
  }
}

// [16, const_sig → CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [119, uident → • UIDENT, COLON]
fn yy_state_336(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_20)
    NT_uident => Shift(yy_state_337)
    _ => Error
  }
}

// [16, const_sig → CONST uident • COLON type_ EQUAL constant, EOF / SEMI]
fn yy_state_337(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_338)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [77, type_ → • type_ QUESTION, EQUAL / QUESTION]
// [78, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [79, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / QUESTION]
// [80, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / QUESTION]
// [81, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / QUESTION]
// [82, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / QUESTION]
// [83, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / QUESTION]
// [84, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / QUESTION]
// [85, type_ → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [86, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / QUESTION]
// [87, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / QUESTION]
// [88, type_ → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [89, type_ → • AMPER qualified_uident_, EQUAL / QUESTION]
// [90, type_ → • UNDERSCORE, EQUAL / QUESTION]
// [117, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [118, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]
fn yy_state_338(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_42)
    T_UIDENT => Shift(yy_state_44)
    T_UNDERSCORE => Shift(yy_state_45)
    T_AMPER => Shift(yy_state_46)
    NT_qualified_uident_ => Shift(yy_state_48)
    T_ASYNC => Shift(yy_state_52)
    T_LPAREN => Shift(yy_state_57)
    NT_type_ => Shift(yy_state_339)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON type_ • EQUAL constant, EOF / SEMI]
// [77, type_ → type_ • QUESTION, EQUAL / QUESTION]
fn yy_state_339(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_68)
    T_EQUAL => Shift(yy_state_340)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [104, constant → • TRUE, EOF / SEMI]
// [105, constant → • FALSE, EOF / SEMI]
// [106, constant → • BYTE, EOF / SEMI]
// [107, constant → • BYTES, EOF / SEMI]
// [108, constant → • CHAR, EOF / SEMI]
// [109, constant → • INT, EOF / SEMI]
// [110, constant → • FLOAT, EOF / SEMI]
// [111, constant → • STRING, EOF / SEMI]
fn yy_state_340(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_341)
    T_FLOAT => Shift(yy_state_342)
    T_INT => Shift(yy_state_343)
    T_CHAR => Shift(yy_state_344)
    T_BYTES => Shift(yy_state_345)
    T_BYTE => Shift(yy_state_346)
    T_FALSE => Shift(yy_state_347)
    T_TRUE => Shift(yy_state_348)
    NT_constant => Shift(yy_state_349)
    _ => Error
  }
}

// [111, constant → STRING •, EOF / SEMI]
fn yy_state_341(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_190)
}

// [110, constant → FLOAT •, EOF / SEMI]
fn yy_state_342(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_122)
}

// [109, constant → INT •, EOF / SEMI]
fn yy_state_343(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_14)
}

// [108, constant → CHAR •, EOF / SEMI]
fn yy_state_344(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_35)
}

// [107, constant → BYTES •, EOF / SEMI]
fn yy_state_345(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_42)
}

// [106, constant → BYTE •, EOF / SEMI]
fn yy_state_346(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_166)
}

// [105, constant → FALSE •, EOF / SEMI]
fn yy_state_347(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_139)
}

// [104, constant → TRUE •, EOF / SEMI]
fn yy_state_348(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_181)
}

// [16, const_sig → CONST uident COLON type_ EQUAL constant •, EOF / SEMI]
fn yy_state_349(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_const_sig, yy_action_118)
}

// [15, sig_ → value_sig •, EOF / SEMI]
fn yy_state_350(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_4)
}

// [14, sig_ → const_sig •, EOF / SEMI]
fn yy_state_351(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_97)
}

// [13, sig_ → impl_sig •, EOF / SEMI]
fn yy_state_352(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_102)
}

// [12, sig_ → trait_sig •, EOF / SEMI]
fn yy_state_353(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_182)
}

// [11, sig_ → alias_sig •, EOF / SEMI]
fn yy_state_354(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_40)
}

// [10, sig_ → type_sig •, EOF / SEMI]
fn yy_state_355(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_100)
}

// [9, sig_ → func_sig •, EOF / SEMI]
fn yy_state_356(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_125)
}

// [7, sigs → sig_ •, EOF]
// [8, sigs → sig_ • SEMI sigs, EOF]
fn yy_state_357(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_358)
    T_EOF => Reduce(1, NT_sigs, yy_action_50)
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
// [18, func_sig_no_attr → • FN lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [19, func_sig → • func_sig_no_attr, EOF / SEMI]
// [20, func_sig → • nonempty_list(ATTRIBUTE) func_sig_no_attr, EOF / SEMI]
// [23, type_sig → • EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [24, type_sig → • PRIV EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [25, type_sig → • PUB pub_attr EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [26, type_sig → • TYPE type_decl_name_with_params, EOF / SEMI]
// [27, type_sig → • PRIV TYPE type_decl_name_with_params, EOF / SEMI]
// [28, type_sig → • PUB pub_attr TYPE type_decl_name_with_params, EOF / SEMI]
// [29, type_sig → • TYPE type_decl_name_with_params type_, EOF / SEMI]
// [30, type_sig → • PRIV TYPE type_decl_name_with_params type_, EOF / SEMI]
// [31, type_sig → • PUB pub_attr TYPE type_decl_name_with_params type_, EOF / SEMI]
// [32, type_sig → • TYPE EXCLAMATION uident, EOF / SEMI]
// [33, type_sig → • PRIV TYPE EXCLAMATION uident, EOF / SEMI]
// [34, type_sig → • PUB pub_attr TYPE EXCLAMATION uident, EOF / SEMI]
// [35, type_sig → • TYPE EXCLAMATION uident type_, EOF / SEMI]
// [36, type_sig → • PRIV TYPE EXCLAMATION uident type_, EOF / SEMI]
// [37, type_sig → • PUB pub_attr TYPE EXCLAMATION uident type_, EOF / SEMI]
// [38, type_sig → • TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [39, type_sig → • PRIV TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [40, type_sig → • PUB pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [41, type_sig → • STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [42, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [43, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [44, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [45, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [46, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [47, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [48, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [49, impl_sig → • IMPL uident LBRACE separated_nonempty_list(SEMI,impl_method_sig) RBRACE, EOF / SEMI]
// [50, impl_sig → • IMPL uident COLONCOLON lident, EOF / SEMI]
// [51, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [52, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [53, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [54, trait_sig → • TRAIT uident, EOF / SEMI]
// [55, trait_sig → • PRIV TRAIT uident, EOF / SEMI]
// [56, trait_sig → • PUB pub_attr TRAIT uident, EOF / SEMI]
// [57, alias_sig → • TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [58, alias_sig → • PRIV TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [59, alias_sig → • PUB pub_attr TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [60, alias_sig → • TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [61, alias_sig → • PRIV TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [62, alias_sig → • PUB pub_attr TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [129, nonempty_list(ATTRIBUTE) → • ATTRIBUTE, FN]
// [130, nonempty_list(ATTRIBUTE) → • ATTRIBUTE nonempty_list(ATTRIBUTE), FN]
fn yy_state_358(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_17)
    T_TRAITALIAS => Shift(yy_state_19)
    T_TYPEALIAS => Shift(yy_state_27)
    T_TRAIT => Shift(yy_state_98)
    T_IMPL => Shift(yy_state_131)
    T_ENUM => Shift(yy_state_190)
    T_STRUCT => Shift(yy_state_216)
    T_TYPE => Shift(yy_state_229)
    T_PUB => Shift(yy_state_238)
    T_PRIV => Shift(yy_state_281)
    T_EXTERN => Shift(yy_state_318)
    NT_nonempty_list_ATTRIBUTE_ => Shift(yy_state_321)
    T_FN => Shift(yy_state_323)
    NT_func_sig_no_attr => Shift(yy_state_331)
    T_LET => Shift(yy_state_332)
    T_CONST => Shift(yy_state_336)
    NT_value_sig => Shift(yy_state_350)
    NT_const_sig => Shift(yy_state_351)
    NT_impl_sig => Shift(yy_state_352)
    NT_trait_sig => Shift(yy_state_353)
    NT_alias_sig => Shift(yy_state_354)
    NT_type_sig => Shift(yy_state_355)
    NT_func_sig => Shift(yy_state_356)
    NT_sig_ => Shift(yy_state_357)
    NT_sigs => Shift(yy_state_359)
    T_EOF => Reduce(0, NT_sigs, yy_action_180)
    _ => Error
  }
}

// [8, sigs → sig_ SEMI sigs •, EOF]
fn yy_state_359(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_sigs, yy_action_108)
}

// [1, mbti → LIDENT STRING SEMI imports sigs • EOF, $]
fn yy_state_360(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EOF => Shift(yy_state_361)
    _ => Error
  }
}

// [1, mbti → LIDENT STRING SEMI imports sigs EOF •, $]
fn yy_state_361(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_mbti, yy_action_188)
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
// [18, func_sig_no_attr → • FN lident loption(type_params_with_constraints) LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [19, func_sig → • func_sig_no_attr, EOF / SEMI]
// [20, func_sig → • nonempty_list(ATTRIBUTE) func_sig_no_attr, EOF / SEMI]
// [23, type_sig → • EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [24, type_sig → • PRIV EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [25, type_sig → • PUB pub_attr EXTERN TYPE type_decl_name_with_params, EOF / SEMI]
// [26, type_sig → • TYPE type_decl_name_with_params, EOF / SEMI]
// [27, type_sig → • PRIV TYPE type_decl_name_with_params, EOF / SEMI]
// [28, type_sig → • PUB pub_attr TYPE type_decl_name_with_params, EOF / SEMI]
// [29, type_sig → • TYPE type_decl_name_with_params type_, EOF / SEMI]
// [30, type_sig → • PRIV TYPE type_decl_name_with_params type_, EOF / SEMI]
// [31, type_sig → • PUB pub_attr TYPE type_decl_name_with_params type_, EOF / SEMI]
// [32, type_sig → • TYPE EXCLAMATION uident, EOF / SEMI]
// [33, type_sig → • PRIV TYPE EXCLAMATION uident, EOF / SEMI]
// [34, type_sig → • PUB pub_attr TYPE EXCLAMATION uident, EOF / SEMI]
// [35, type_sig → • TYPE EXCLAMATION uident type_, EOF / SEMI]
// [36, type_sig → • PRIV TYPE EXCLAMATION uident type_, EOF / SEMI]
// [37, type_sig → • PUB pub_attr TYPE EXCLAMATION uident type_, EOF / SEMI]
// [38, type_sig → • TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [39, type_sig → • PRIV TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [40, type_sig → • PUB pub_attr TYPE EXCLAMATION uident LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [41, type_sig → • STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [42, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [43, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE separated_list(SEMI,record_decl_field) RBRACE, EOF / SEMI]
// [44, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [45, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [46, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE, EOF / SEMI]
// [47, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [48, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [49, impl_sig → • IMPL uident LBRACE separated_nonempty_list(SEMI,impl_method_sig) RBRACE, EOF / SEMI]
// [50, impl_sig → • IMPL uident COLONCOLON lident, EOF / SEMI]
// [51, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [52, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [53, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) LBRACE separated_nonempty_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [54, trait_sig → • TRAIT uident, EOF / SEMI]
// [55, trait_sig → • PRIV TRAIT uident, EOF / SEMI]
// [56, trait_sig → • PUB pub_attr TRAIT uident, EOF / SEMI]
// [57, alias_sig → • TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [58, alias_sig → • PRIV TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [59, alias_sig → • PUB pub_attr TYPEALIAS type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [60, alias_sig → • TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [61, alias_sig → • PRIV TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [62, alias_sig → • PUB pub_attr TRAITALIAS uident EQUAL qualified_uident, EOF / SEMI]
// [129, nonempty_list(ATTRIBUTE) → • ATTRIBUTE, FN]
// [130, nonempty_list(ATTRIBUTE) → • ATTRIBUTE nonempty_list(ATTRIBUTE), FN]
fn yy_state_362(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_17)
    T_TRAITALIAS => Shift(yy_state_19)
    T_TYPEALIAS => Shift(yy_state_27)
    T_TRAIT => Shift(yy_state_98)
    T_IMPL => Shift(yy_state_131)
    T_ENUM => Shift(yy_state_190)
    T_STRUCT => Shift(yy_state_216)
    T_TYPE => Shift(yy_state_229)
    T_PUB => Shift(yy_state_238)
    T_PRIV => Shift(yy_state_281)
    T_EXTERN => Shift(yy_state_318)
    NT_nonempty_list_ATTRIBUTE_ => Shift(yy_state_321)
    T_FN => Shift(yy_state_323)
    NT_func_sig_no_attr => Shift(yy_state_331)
    T_LET => Shift(yy_state_332)
    T_CONST => Shift(yy_state_336)
    NT_value_sig => Shift(yy_state_350)
    NT_const_sig => Shift(yy_state_351)
    NT_impl_sig => Shift(yy_state_352)
    NT_trait_sig => Shift(yy_state_353)
    NT_alias_sig => Shift(yy_state_354)
    NT_type_sig => Shift(yy_state_355)
    NT_func_sig => Shift(yy_state_356)
    NT_sig_ => Shift(yy_state_357)
    NT_sigs => Shift(yy_state_363)
    T_EOF => Reduce(0, NT_sigs, yy_action_180)
    _ => Error
  }
}

// [0, mbti → LIDENT STRING imports sigs • EOF, $]
fn yy_state_363(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EOF => Shift(yy_state_364)
    _ => Error
  }
}

// [0, mbti → LIDENT STRING imports sigs EOF •, $]
fn yy_state_364(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_mbti, yy_action_98)
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
