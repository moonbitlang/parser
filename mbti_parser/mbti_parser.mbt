///|
pub suberror ParseError {
  UnexpectedToken(Token, (@basic.Position, @basic.Position), Array[TokenKind])
  UnexpectedEndOfInput(@basic.Position, Array[TokenKind])
} derive(Debug)

///|
type YYObj = Error

///|
priv suberror YYObj_Void

///|
priv suberror YYObj_Int {
  YYObj_Int(Int)
}

///|
priv suberror YYObj_Sig {
  YYObj_Sig(Sig)
}

///|
priv suberror YYObj_Bool {
  YYObj_Bool(Bool)
}

///|
priv suberror YYObj_Mbti {
  YYObj_Mbti(Mbti)
}

///|
priv suberror YYObj_Name {
  YYObj_Name(Name)
}

///|
priv suberror YYObj_Type {
  YYObj_Type(Type)
}

///|
priv suberror YYObj_Unit {
  YYObj_Unit(Unit)
}

///|
priv suberror YYObj_Label {
  YYObj_Label(Label)
}

///|
priv suberror YYObj_Option_Bool_ {
  YYObj_Option_Bool_(Bool?)
}

///|
priv suberror YYObj_Option_Unit_ {
  YYObj_Option_Unit_(Unit?)
}

///|
priv suberror YYObj_Option__list_List_Type__ {
  YYObj_Option__list_List_Type__(@list.List[Type]?)
}

///|
priv suberror YYObj_Option__list_List_ConstrParam__ {
  YYObj_Option__list_List_ConstrParam__(@list.List[ConstrParam]?)
}

///|
priv suberror YYObj_Option__String__Location__ {
  YYObj_Option__String__Location__((String, Location)?)
}

///|
priv suberror YYObj_String {
  YYObj_String(String)
}

///|
priv suberror YYObj_Comment {
  YYObj_Comment(Comment)
}

///|
priv suberror YYObj_FuncSig {
  YYObj_FuncSig(FuncSig)
}

///|
priv suberror YYObj_ImplSig {
  YYObj_ImplSig(ImplSig)
}

///|
priv suberror YYObj_TypeSig {
  YYObj_TypeSig(TypeSig)
}

///|
priv suberror YYObj_AliasSig {
  YYObj_AliasSig(AliasSig)
}

///|
priv suberror YYObj_ConstSig {
  YYObj_ConstSig(ConstSig)
}

///|
priv suberror YYObj_Constant {
  YYObj_Constant(Constant)
}

///|
priv suberror YYObj_ConstrId {
  YYObj_ConstrId(ConstrId)
}

///|
priv suberror YYObj_TraitSig {
  YYObj_TraitSig(TraitSig)
}

///|
priv suberror YYObj_ValueSig {
  YYObj_ValueSig(ValueSig)
}

///|
priv suberror YYObj_Attribute {
  YYObj_Attribute(Attribute)
}

///|
priv suberror YYObj_FieldDecl {
  YYObj_FieldDecl(FieldDecl)
}

///|
priv suberror YYObj_Parameter {
  YYObj_Parameter(Parameter)
}

///|
priv suberror YYObj_ConstrDecl {
  YYObj_ConstrDecl(ConstrDecl)
}

///|
priv suberror YYObj_Visibility {
  YYObj_Visibility(Visibility)
}

///|
priv suberror YYObj_CharLiteral {
  YYObj_CharLiteral(CharLiteral)
}

///|
priv suberror YYObj_ConstrParam {
  YYObj_ConstrParam(ConstrParam)
}

///|
priv suberror YYObj_InterpLiteral {
  YYObj_InterpLiteral(InterpLiteral)
}

///|
priv suberror YYObj_PackageImport {
  YYObj_PackageImport(PackageImport)
}

///|
priv suberror YYObj_QualifiedName {
  YYObj_QualifiedName(QualifiedName)
}

///|
priv suberror YYObj_StringLiteral {
  YYObj_StringLiteral(StringLiteral)
}

///|
priv suberror YYObj_MethodSelfType {
  YYObj_MethodSelfType(MethodSelfType)
}

///|
priv suberror YYObj_TraitMethodSig {
  YYObj_TraitMethodSig(TraitMethodSig)
}

///|
priv suberror YYObj_TypeDefinition {
  YYObj_TypeDefinition(TypeDefinition)
}

///|
priv suberror YYObj_TypeParamNoConstraints {
  YYObj_TypeParamNoConstraints(TypeParamNoConstraints)
}

///|
priv suberror YYObj_TypeParamWithConstraints {
  YYObj_TypeParamWithConstraints(TypeParamWithConstraints)
}

///|
priv suberror YYObj__list_List_Type_ {
  YYObj__list_List_Type_(@list.List[Type])
}

///|
priv suberror YYObj__list_List_Attribute_ {
  YYObj__list_List_Attribute_(@list.List[Attribute])
}

///|
priv suberror YYObj__list_List_FieldDecl_ {
  YYObj__list_List_FieldDecl_(@list.List[FieldDecl])
}

///|
priv suberror YYObj__list_List_Parameter_ {
  YYObj__list_List_Parameter_(@list.List[Parameter])
}

///|
priv suberror YYObj__list_List_ConstrDecl_ {
  YYObj__list_List_ConstrDecl_(@list.List[ConstrDecl])
}

///|
priv suberror YYObj__list_List_ConstrParam_ {
  YYObj__list_List_ConstrParam_(@list.List[ConstrParam])
}

///|
priv suberror YYObj__list_List_PackageImport_ {
  YYObj__list_List_PackageImport_(@list.List[PackageImport])
}

///|
priv suberror YYObj__list_List_QualifiedName_ {
  YYObj__list_List_QualifiedName_(@list.List[QualifiedName])
}

///|
priv suberror YYObj__list_List_TraitMethodSig_ {
  YYObj__list_List_TraitMethodSig_(@list.List[TraitMethodSig])
}

///|
priv suberror YYObj__list_List_TypeParamNoConstraints_ {
  YYObj__list_List_TypeParamNoConstraints_(@list.List[TypeParamNoConstraints])
}

///|
priv suberror YYObj__list_List_TypeParamWithConstraints_ {
  YYObj__list_List_TypeParamWithConstraints_(
    @list.List[TypeParamWithConstraints]
  )
}

///|
priv suberror YYObj__list_List__Sig__Location__ {
  YYObj__list_List__Sig__Location__(@list.List[(Sig, Location)])
}

///|
priv suberror YYObj_Name_ {
  YYObj_Name_(Name?)
}

///|
priv suberror YYObj_String_ {
  YYObj_String_(String?)
}

///|
priv suberror YYObj_Location_ {
  YYObj_Location_(Location?)
}

///|
priv suberror YYObj__Name___list_List_TypeParamNoConstraints__ {
  YYObj__Name___list_List_TypeParamNoConstraints__(
    (Name, @list.List[TypeParamNoConstraints])
  )
}

///|
priv suberror YYObj__Type__ErrorType_ {
  YYObj__Type__ErrorType_((Type, ErrorType))
}

///|
priv suberror YYObj__String__Location_ {
  YYObj__String__Location_((String, Location))
}

///|
priv suberror YYObj__MethodSelfType___Name_ {
  YYObj__MethodSelfType___Name_((MethodSelfType?, Name))
}

///|
priv suberror YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_ {
  YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(
    (Name, @list.List[TypeParamNoConstraints], QualifiedName)
  )
}

///|
priv suberror YYObj__String__Option_String___String_ {
  YYObj__String__Option_String___String_((String, String?, String))
}

///|
type YYState = (YYSymbol) -> YYDecision

///|
type YYAction = (
  @basic.Position,
  ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj

///|
priv enum YYDecision {
  Accept
  Shift(YYState)
  Reduce(Int, YYSymbol, YYAction)
  ReduceNoLookahead(Int, YYSymbol, YYAction)
  Error
}

///|
priv enum YYSymbol {
  T_CHAR
  T_INT
  T_BYTE
  T_BYTES
  T_FLOAT
  T_DOUBLE
  T_STRING
  T_MULTILINE_STRING
  T_MULTILINE_INTERP
  T_INTERP
  T_REGEX_LITERAL
  T_REGEX_INTERP
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
  T_PLUS_EQUAL
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
  T_EQ_TILDE
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
  T_PIPE_LEFT
  T_LT_PLUS
  T_ELSE
  T_FN
  T_IF
  T_LET
  T_PROOF_ASSERT
  T_PROOF_LET
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
  T_RANGE_LT_INCLUSIVE
  T_RANGE_EXCLUSIVE
  T_RANGE_INCLUSIVE_REV
  T_RANGE_EXCLUSIVE_REV
  T_ELLIPSIS
  T_TEST
  T_LOOP
  T_GUARD
  T_DEFER
  T_FOR
  T_IN
  T_IS
  T_SUBERROR
  T_AND
  T_FORALL
  T_EXISTS
  T_IMPLIES
  T_LETREC
  T_USING
  T_DECLARE
  T_NORAISE
  T_NOBREAK
  T_WHERE
  T_ENUMVIEW
  T_EXTENUM
  T_TRY_QUESTION
  T_TRY_EXCLAMATION
  T_LEXMATCH
  T_LEXMATCH_QUESTION
  T_PACKAGE
  NT_mbti
  NT_imports
  NT_package_import
  NT_sigs
  NT_sig_
  NT_const_sig
  NT_value_sig
  NT_method_self_type_coloncolon
  NT_luident
  NT_func_name
  NT_method_prefix
  NT_func_sig
  NT_trait_method_sig
  NT_type_sig
  NT_optional_derive
  NT_impl_sig
  NT_trait_sig
  NT_trait_body
  NT_using_binder
  NT_alias_sig
  NT_enum_constructor
  NT_constructor_param
  NT_record_decl_field
  NT_record_decl_body
  NT_type_param_with_constraints
  NT_type_params_with_constraints
  NT_type_param_no_constraints
  NT_type_params_no_constraints
  NT_optional_type_params_no_constraints
  NT_type_constraint
  NT_type_decl_name_with_params
  NT_extensible_enum_extension_target
  NT_simple_type
  NT_type_
  NT_return_type
  NT_error_type
  NT_optional_type_arguments
  NT_parameter
  NT_constant
  NT_pub_attr
  NT_qualified_uident
  NT_qualified_uident_
  NT_uident
  NT_lident
  NT_label
  NT_post_label
  NT_non_empty_list_commas_package_import_
  NT_loption_type_params_with_constraints_
  NT_separated_list_COMMA_parameter_
  NT_boption_eq_underscore_
  NT_separated_list_SEMI_enum_constructor_
  NT_separated_list_COMMA_type__
  NT_non_empty_list_commas_qualified_uident_
  NT_separated_nonempty_list_PLUS_qualified_uident_
  NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___
  NT_separated_list_SEMI_trait_method_sig_
  NT_separated_nonempty_list_COMMA_constructor_param_
  NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__
  NT_option_eq_tag_
  NT_boption_MUTABLE_
  NT_separated_list_SEMI_record_decl_field_
  NT_separated_nonempty_list_PLUS_type_constraint_
  NT_separated_list_COMMA_type_param_with_constraints_
  NT_separated_list_COMMA_type_param_no_constraints_
  NT_separated_nonempty_list_COMMA_type__
  NT_nonempty_list_attribute_
  NT_non_empty_list_commas_rev_package_import_
  NT_option_COMMA_
  NT_separated_nonempty_list_COMMA_parameter_
  NT_separated_nonempty_list_SEMI_enum_constructor_
  NT_non_empty_list_commas_rev_qualified_uident_
  NT_separated_nonempty_list_SEMI_trait_method_sig_
  NT_separated_nonempty_list_SEMI_record_decl_field_
  NT_separated_nonempty_list_COMMA_type_param_with_constraints_
  NT_separated_nonempty_list_COMMA_type_param_no_constraints_
  EOI
}

// Workaround for EOI unused warning

///|
fn init {
  match (EOI : YYSymbol) {
    EOI => ()
    _ => ()
  }
}

// file:///./mbti_parser.mbty
// 143|    PACKAGE STRING ioption(";") imports sigs EOF { Mbti::{package_name: $2, imports: $4, sigs: $5, loc : mk_loc($sloc), base_pos: mk_base_pos($loc($1).0)} }

///|
fn yy_action_0(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_1(_args[1].2, _args[2:2])
  guard _args[1].0 is YYObj_StringLiteral(_dollar2)
  guard _args[2].0 is YYObj__list_List_PackageImport_(_dollar4)
  guard _args[3].0 is YYObj__list_List__Sig__Location__(_dollar5)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  YYObj_Mbti(
    {
      ()
      Mbti::{
        package_name: _dollar2,
        imports: _dollar4,
        sigs: _dollar5,
        loc: mk_loc((_symbol_start_pos, _end_pos)),
        base_pos: mk_base_pos((_start_pos_of_item0, _end_pos_of_item0).0),
      }
    },
  )
}

// file:///./stdlib.mbty
// 9|    { None }

///|
fn yy_action_1(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Option_Bool_(
    {
      ()
      None
    },
  )
}

// file:///./mbti_parser.mbty
// 143|    PACKAGE STRING ioption(";") imports sigs EOF { Mbti::{package_name: $2, imports: $4, sigs: $5, loc : mk_loc($sloc), base_pos: mk_base_pos($loc($1).0)} }

///|
fn yy_action_2(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_3(_args[1].2, _args[2:3])
  guard _args[1].0 is YYObj_StringLiteral(_dollar2)
  guard _args[3].0 is YYObj__list_List_PackageImport_(_dollar4)
  guard _args[4].0 is YYObj__list_List__Sig__Location__(_dollar5)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  YYObj_Mbti(
    {
      ()
      Mbti::{
        package_name: _dollar2,
        imports: _dollar4,
        sigs: _dollar5,
        loc: mk_loc((_symbol_start_pos, _end_pos)),
        base_pos: mk_base_pos((_start_pos_of_item0, _end_pos_of_item0).0),
      }
    },
  )
}

// file:///./stdlib.mbty
// 10|    X { Some($1) }

///|
fn yy_action_3(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  YYObj_Option_Bool_(
    {
      ()
      Some(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 159|    sig_ ";" sigs { $3.add(($1, mk_loc($loc($1)))) }

///|
fn yy_action_4(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List__Sig__Location__(_dollar3)
  guard _args[0].0 is YYObj_Sig(_dollar1)
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  YYObj__list_List__Sig__Location__(
    {
      ()
      _dollar3.add((_dollar1, mk_loc((_start_pos_of_item0, _end_pos_of_item0))))
    },
  )
}

// file:///./mbti_parser.mbty
// 158|    sig_ { @list.singleton(($1, mk_loc($sloc))) }

///|
fn yy_action_5(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Sig(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj__list_List__Sig__Location__(
    {
      ()
      @list.singleton((_dollar1, mk_loc((_symbol_start_pos, _end_pos))))
    },
  )
}

// file:///./mbti_parser.mbty
// 163|    func_sig { Func($1) }

///|
fn yy_action_6(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_FuncSig(_dollar1)
  YYObj_Sig(
    {
      ()
      Func(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 164|    type_sig { Type($1) }

///|
fn yy_action_7(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_TypeSig(_dollar1)
  YYObj_Sig(
    {
      ()
      Type(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 165|    alias_sig { Alias($1) }

///|
fn yy_action_8(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_AliasSig(_dollar1)
  YYObj_Sig(
    {
      ()
      Alias(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 166|    trait_sig { Trait($1) }

///|
fn yy_action_9(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_TraitSig(_dollar1)
  YYObj_Sig(
    {
      ()
      Trait(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 167|    impl_sig { Impl($1) }

///|
fn yy_action_10(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_ImplSig(_dollar1)
  YYObj_Sig(
    {
      ()
      Impl(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 168|    const_sig { Const($1) }

///|
fn yy_action_11(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_ConstSig(_dollar1)
  YYObj_Sig(
    {
      ()
      Const(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 169|    value_sig { Value($1) }

///|
fn yy_action_12(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_ValueSig(_dollar1)
  YYObj_Sig(
    {
      ()
      Value(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 173|    attributes vis "const" uident ":" type_ "=" constant { ConstSig::{name: $4, type_: $6, value: $8, attrs: $1} }

///|
fn yy_action_13(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj_Type(_dollar6)
  guard _args[5].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig(
    {
      ()
      ConstSig::{
        name: _dollar4,
        type_: _dollar6,
        value: _dollar8,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 173|    attributes vis "const" uident ":" type_ "=" constant { ConstSig::{name: $4, type_: $6, value: $8, attrs: $1} }

///|
fn yy_action_16(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  guard _args[6].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig(
    {
      ()
      ConstSig::{
        name: _dollar4,
        type_: _dollar6,
        value: _dollar8,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 177|    attributes vis "let" lident ":" type_ { ValueSig::{ attrs: $1, name: $4, type_: $6 } }

///|
fn yy_action_18(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig(
    {
      ()
      ValueSig::{ attrs: _dollar1, name: _dollar4, type_: _dollar6 }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_19(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  let _sub_action_2_result = yy_action_20(_args[0].2, _args[1:2])
  let _sub_action_3_result = yy_action_21(_args[4].2, _args[5:8])
  guard _args[4].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[3].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[9].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_22(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  let _sub_action_2_result = yy_action_23(_args[0].2, _args[1:1])
  let _sub_action_3_result = yy_action_21(_args[3].2, _args[4:7])
  guard _args[3].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[8].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 223|    attributes vis "type" type_decl_name_with_params optional_derive {
// 224|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5} 
// 225|  }

///|
fn yy_action_24(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Abstract,
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 281|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 282|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6) 
// 283|  }

///|
fn yy_action_25(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig(
    {
      ()
      TypeAlias(
        attrs=_dollar1,
        vis=_dollar2,
        name=_dollar4.0,
        type_params=_dollar4.1,
        type_=_dollar6,
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 226|    attributes vis "suberror" uident optional_derive {
// 227|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 228|  }

///|
fn yy_action_26(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(NoPayload),
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 229|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 230|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 231|  }

///|
fn yy_action_27(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(EnumPayload(_dollar6)),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 233|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 234|  }

///|
fn yy_action_28(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: _dollar6,
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 236|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 237|  }

///|
fn yy_action_29(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: TupleStruct(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 240|  }

///|
fn yy_action_30(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Variant(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 242|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 243|  }

///|
fn yy_action_31(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: ExtensibleEnum(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 245|    let (name, type_params, target) = $4
// 246|    TypeSig::{attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 247|  }

///|
fn yy_action_32(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0
    is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig(
    {
      ()
      let (name, type_params, target) = _dollar4
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name,
        type_params,
        components: ExtendEnum(target~, constructors=_dollar7),
        derives: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 256|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 257|    ImplSig::{type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 258|  }

///|
fn yy_action_33(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[3].0 is YYObj_QualifiedName(_dollar5)
  guard _args[5].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: _dollar4,
        trait_name: _dollar5,
        type_: _dollar7,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 259|    attributes vis "impl" qualified_uident "for" type_ { 
// 260|    ImplSig::{type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 261|  }

///|
fn yy_action_34(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_QualifiedName(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: @list.empty(),
        trait_name: _dollar4,
        type_: _dollar6,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body { 
// 266|    TraitSig::{ attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6} 
// 267|  }

///|
fn yy_action_35(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[4].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig(
    {
      ()
      TraitSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        super_traits: _dollar5,
        methods: _dollar6,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 284|    attributes vis "fnalias" uident "::" lident {
// 285|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 286|  }

///|
fn yy_action_36(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar6)
  guard _args[2].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig(
    {
      ()
      FuncAlias(attrs=_dollar1, name=_dollar6, type_name=_dollar4)
    },
  )
}

// file:///./mbti_parser.mbty
// 287|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 288|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 289|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 290|  }

///|
fn yy_action_37(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[2].1
  let _end_pos_of_item3 = _args[2].2
  guard _args[5].0 is YYObj_Name(_dollar7)
  guard _args[6].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Type, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 291|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 292|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 293|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 294|  }

///|
fn yy_action_38(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[2].1
  let _end_pos_of_item3 = _args[2].2
  guard _args[5].0 is YYObj_Name(_dollar7)
  guard _args[6].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Trait, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 173|    attributes vis "const" uident ":" type_ "=" constant { ConstSig::{name: $4, type_: $6, value: $8, attrs: $1} }

///|
fn yy_action_39(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  guard _args[7].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig(
    {
      ()
      ConstSig::{
        name: _dollar4,
        type_: _dollar6,
        value: _dollar8,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 177|    attributes vis "let" lident ":" type_ { ValueSig::{ attrs: $1, name: $4, type_: $6 } }

///|
fn yy_action_41(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig(
    {
      ()
      ValueSig::{ attrs: _dollar1, name: _dollar4, type_: _dollar6 }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_42(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  let _sub_action_2_result = yy_action_20(_args[1].2, _args[2:3])
  let _sub_action_3_result = yy_action_21(_args[5].2, _args[6:9])
  guard _args[5].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[4].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[10].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_43(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  let _sub_action_2_result = yy_action_23(_args[1].2, _args[2:2])
  let _sub_action_3_result = yy_action_21(_args[4].2, _args[5:8])
  guard _args[4].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[3].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[9].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 223|    attributes vis "type" type_decl_name_with_params optional_derive {
// 224|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5} 
// 225|  }

///|
fn yy_action_44(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Abstract,
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 281|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 282|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6) 
// 283|  }

///|
fn yy_action_45(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig(
    {
      ()
      TypeAlias(
        attrs=_dollar1,
        vis=_dollar2,
        name=_dollar4.0,
        type_params=_dollar4.1,
        type_=_dollar6,
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 226|    attributes vis "suberror" uident optional_derive {
// 227|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 228|  }

///|
fn yy_action_46(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(NoPayload),
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 229|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 230|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 231|  }

///|
fn yy_action_47(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(EnumPayload(_dollar6)),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 233|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 234|  }

///|
fn yy_action_48(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: _dollar6,
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 236|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 237|  }

///|
fn yy_action_49(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: TupleStruct(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 240|  }

///|
fn yy_action_50(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Variant(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 242|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 243|  }

///|
fn yy_action_51(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: ExtensibleEnum(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 245|    let (name, type_params, target) = $4
// 246|    TypeSig::{attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 247|  }

///|
fn yy_action_52(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0
    is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[6].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig(
    {
      ()
      let (name, type_params, target) = _dollar4
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name,
        type_params,
        components: ExtendEnum(target~, constructors=_dollar7),
        derives: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 256|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 257|    ImplSig::{type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 258|  }

///|
fn yy_action_53(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[4].0 is YYObj_QualifiedName(_dollar5)
  guard _args[6].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: _dollar4,
        trait_name: _dollar5,
        type_: _dollar7,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 259|    attributes vis "impl" qualified_uident "for" type_ { 
// 260|    ImplSig::{type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 261|  }

///|
fn yy_action_54(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj_QualifiedName(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: @list.empty(),
        trait_name: _dollar4,
        type_: _dollar6,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body { 
// 266|    TraitSig::{ attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6} 
// 267|  }

///|
fn yy_action_55(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[5].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig(
    {
      ()
      TraitSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        super_traits: _dollar5,
        methods: _dollar6,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 284|    attributes vis "fnalias" uident "::" lident {
// 285|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 286|  }

///|
fn yy_action_56(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[5].0 is YYObj_Name(_dollar6)
  guard _args[3].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig(
    {
      ()
      FuncAlias(attrs=_dollar1, name=_dollar6, type_name=_dollar4)
    },
  )
}

// file:///./mbti_parser.mbty
// 287|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 288|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 289|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 290|  }

///|
fn yy_action_57(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[3].1
  let _end_pos_of_item3 = _args[3].2
  guard _args[6].0 is YYObj_Name(_dollar7)
  guard _args[7].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Type, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 291|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 292|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 293|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 294|  }

///|
fn yy_action_58(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[3].1
  let _end_pos_of_item3 = _args[3].2
  guard _args[6].0 is YYObj_Name(_dollar7)
  guard _args[7].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Trait, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 173|    attributes vis "const" uident ":" type_ "=" constant { ConstSig::{name: $4, type_: $6, value: $8, attrs: $1} }

///|
fn yy_action_59(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  guard _args[6].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig(
    {
      ()
      ConstSig::{
        name: _dollar4,
        type_: _dollar6,
        value: _dollar8,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 173|    attributes vis "const" uident ":" type_ "=" constant { ConstSig::{name: $4, type_: $6, value: $8, attrs: $1} }

///|
fn yy_action_61(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  guard _args[7].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig(
    {
      ()
      ConstSig::{
        name: _dollar4,
        type_: _dollar6,
        value: _dollar8,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 177|    attributes vis "let" lident ":" type_ { ValueSig::{ attrs: $1, name: $4, type_: $6 } }

///|
fn yy_action_62(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig(
    {
      ()
      ValueSig::{ attrs: _dollar1, name: _dollar4, type_: _dollar6 }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_63(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  let _sub_action_2_result = yy_action_20(_args[1].2, _args[2:3])
  let _sub_action_3_result = yy_action_21(_args[5].2, _args[6:9])
  guard _args[5].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[4].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[10].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_64(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  let _sub_action_2_result = yy_action_23(_args[1].2, _args[2:2])
  let _sub_action_3_result = yy_action_21(_args[4].2, _args[5:8])
  guard _args[4].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[3].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[9].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 223|    attributes vis "type" type_decl_name_with_params optional_derive {
// 224|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5} 
// 225|  }

///|
fn yy_action_65(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Abstract,
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 281|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 282|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6) 
// 283|  }

///|
fn yy_action_66(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig(
    {
      ()
      TypeAlias(
        attrs=_dollar1,
        vis=_dollar2,
        name=_dollar4.0,
        type_params=_dollar4.1,
        type_=_dollar6,
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 226|    attributes vis "suberror" uident optional_derive {
// 227|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 228|  }

///|
fn yy_action_67(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(NoPayload),
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 229|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 230|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 231|  }

///|
fn yy_action_68(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(EnumPayload(_dollar6)),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 233|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 234|  }

///|
fn yy_action_69(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: _dollar6,
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 236|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 237|  }

///|
fn yy_action_70(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: TupleStruct(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 240|  }

///|
fn yy_action_71(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Variant(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 242|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 243|  }

///|
fn yy_action_72(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: ExtensibleEnum(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 245|    let (name, type_params, target) = $4
// 246|    TypeSig::{attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 247|  }

///|
fn yy_action_73(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0
    is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[6].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig(
    {
      ()
      let (name, type_params, target) = _dollar4
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name,
        type_params,
        components: ExtendEnum(target~, constructors=_dollar7),
        derives: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 256|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 257|    ImplSig::{type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 258|  }

///|
fn yy_action_74(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[4].0 is YYObj_QualifiedName(_dollar5)
  guard _args[6].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: _dollar4,
        trait_name: _dollar5,
        type_: _dollar7,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 259|    attributes vis "impl" qualified_uident "for" type_ { 
// 260|    ImplSig::{type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 261|  }

///|
fn yy_action_75(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj_QualifiedName(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: @list.empty(),
        trait_name: _dollar4,
        type_: _dollar6,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body { 
// 266|    TraitSig::{ attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6} 
// 267|  }

///|
fn yy_action_76(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[5].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig(
    {
      ()
      TraitSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        super_traits: _dollar5,
        methods: _dollar6,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 284|    attributes vis "fnalias" uident "::" lident {
// 285|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 286|  }

///|
fn yy_action_77(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[5].0 is YYObj_Name(_dollar6)
  guard _args[3].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig(
    {
      ()
      FuncAlias(attrs=_dollar1, name=_dollar6, type_name=_dollar4)
    },
  )
}

// file:///./mbti_parser.mbty
// 287|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 288|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 289|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 290|  }

///|
fn yy_action_78(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[3].1
  let _end_pos_of_item3 = _args[3].2
  guard _args[6].0 is YYObj_Name(_dollar7)
  guard _args[7].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Type, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 291|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 292|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 293|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 294|  }

///|
fn yy_action_79(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[3].1
  let _end_pos_of_item3 = _args[3].2
  guard _args[6].0 is YYObj_Name(_dollar7)
  guard _args[7].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Trait, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 426|    "priv" { Priv(loc=mk_loc($sloc)) }

///|
fn yy_action_17(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Visibility(
    {
      ()
      Priv(loc=mk_loc((_symbol_start_pos, _end_pos)))
    },
  )
}

// file:///./mbti_parser.mbty
// 173|    attributes vis "const" uident ":" type_ "=" constant { ConstSig::{name: $4, type_: $6, value: $8, attrs: $1} }

///|
fn yy_action_80(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[6].0 is YYObj_Type(_dollar6)
  guard _args[8].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig(
    {
      ()
      ConstSig::{
        name: _dollar4,
        type_: _dollar6,
        value: _dollar8,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 410|    TRUE { Bool(true) }

///|
fn yy_action_81(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Constant(
    {
      ()
      Bool(true)
    },
  )
}

// file:///./mbti_parser.mbty
// 411|    FALSE { Bool(false) }

///|
fn yy_action_82(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Constant(
    {
      ()
      Bool(false)
    },
  )
}

// file:///./mbti_parser.mbty
// 412|    BYTE { Byte($1) }

///|
fn yy_action_83(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_CharLiteral(_dollar1)
  YYObj_Constant(
    {
      ()
      Byte(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 413|    BYTES { Bytes($1) }

///|
fn yy_action_84(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_Constant(
    {
      ()
      Bytes(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 414|    CHAR { Char($1) }

///|
fn yy_action_85(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_CharLiteral(_dollar1)
  YYObj_Constant(
    {
      ()
      Char(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 415|    INT { make_int_literal($1) }

///|
fn yy_action_86(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj_Constant(
    {
      ()
      make_int_literal(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 416|    "-" INT { make_int_literal("-" + $2) }

///|
fn yy_action_87(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  YYObj_Constant(
    {
      ()
      make_int_literal("-" + _dollar2)
    },
  )
}

// file:///./mbti_parser.mbty
// 418|    "-" FLOAT { make_float_literal("-" + $2) }

///|
fn yy_action_88(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  YYObj_Constant(
    {
      ()
      make_float_literal("-" + _dollar2)
    },
  )
}

// file:///./mbti_parser.mbty
// 420|    "-" DOUBLE { Double("-" + $2) }

///|
fn yy_action_89(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  YYObj_Constant(
    {
      ()
      Double("-" + _dollar2)
    },
  )
}

// file:///./mbti_parser.mbty
// 417|    FLOAT { make_float_literal($1) }

///|
fn yy_action_90(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj_Constant(
    {
      ()
      make_float_literal(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 419|    DOUBLE { Double($1) }

///|
fn yy_action_91(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj_Constant(
    {
      ()
      Double(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 421|    STRING { String($1) }

///|
fn yy_action_92(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_Constant(
    {
      ()
      String(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 177|    attributes vis "let" lident ":" type_ { ValueSig::{ attrs: $1, name: $4, type_: $6 } }

///|
fn yy_action_93(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[6].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig(
    {
      ()
      ValueSig::{ attrs: _dollar1, name: _dollar4, type_: _dollar6 }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_94(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  let _sub_action_2_result = yy_action_20(_args[2].2, _args[3:4])
  let _sub_action_3_result = yy_action_21(_args[6].2, _args[7:10])
  guard _args[6].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[5].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[11].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_95(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  let _sub_action_2_result = yy_action_23(_args[2].2, _args[3:3])
  let _sub_action_3_result = yy_action_21(_args[5].2, _args[6:9])
  guard _args[5].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[4].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[10].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 223|    attributes vis "type" type_decl_name_with_params optional_derive {
// 224|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5} 
// 225|  }

///|
fn yy_action_96(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Abstract,
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 281|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 282|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6) 
// 283|  }

///|
fn yy_action_97(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[6].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig(
    {
      ()
      TypeAlias(
        attrs=_dollar1,
        vis=_dollar2,
        name=_dollar4.0,
        type_params=_dollar4.1,
        type_=_dollar6,
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 226|    attributes vis "suberror" uident optional_derive {
// 227|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 228|  }

///|
fn yy_action_98(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(NoPayload),
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 229|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 230|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 231|  }

///|
fn yy_action_99(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[6].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(EnumPayload(_dollar6)),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 233|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 234|  }

///|
fn yy_action_100(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[6].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: _dollar6,
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 236|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 237|  }

///|
fn yy_action_101(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[6].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: TupleStruct(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 240|  }

///|
fn yy_action_102(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[6].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Variant(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 242|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 243|  }

///|
fn yy_action_103(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[6].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: ExtensibleEnum(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 245|    let (name, type_params, target) = $4
// 246|    TypeSig::{attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 247|  }

///|
fn yy_action_104(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0
    is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[7].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[9].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig(
    {
      ()
      let (name, type_params, target) = _dollar4
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name,
        type_params,
        components: ExtendEnum(target~, constructors=_dollar7),
        derives: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 256|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 257|    ImplSig::{type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 258|  }

///|
fn yy_action_105(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[5].0 is YYObj_QualifiedName(_dollar5)
  guard _args[7].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: _dollar4,
        trait_name: _dollar5,
        type_: _dollar7,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 259|    attributes vis "impl" qualified_uident "for" type_ { 
// 260|    ImplSig::{type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 261|  }

///|
fn yy_action_106(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj_QualifiedName(_dollar4)
  guard _args[6].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: @list.empty(),
        trait_name: _dollar4,
        type_: _dollar6,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body { 
// 266|    TraitSig::{ attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6} 
// 267|  }

///|
fn yy_action_107(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[6].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig(
    {
      ()
      TraitSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        super_traits: _dollar5,
        methods: _dollar6,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 284|    attributes vis "fnalias" uident "::" lident {
// 285|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 286|  }

///|
fn yy_action_108(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[6].0 is YYObj_Name(_dollar6)
  guard _args[4].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig(
    {
      ()
      FuncAlias(attrs=_dollar1, name=_dollar6, type_name=_dollar4)
    },
  )
}

// file:///./mbti_parser.mbty
// 287|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 288|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 289|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 290|  }

///|
fn yy_action_109(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[4].1
  let _end_pos_of_item3 = _args[4].2
  guard _args[7].0 is YYObj_Name(_dollar7)
  guard _args[8].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Type, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 291|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 292|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 293|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 294|  }

///|
fn yy_action_110(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[4].1
  let _end_pos_of_item3 = _args[4].2
  guard _args[7].0 is YYObj_Name(_dollar7)
  guard _args[8].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Trait, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 427|    "pub" pub_attr { Pub(attr=$2, loc=mk_loc($sloc)) }

///|
fn yy_action_40(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_String_(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Visibility(
    {
      ()
      Pub(attr=_dollar2, loc=mk_loc((_symbol_start_pos, _end_pos)))
    },
  )
}

// file:///./mbti_parser.mbty
// 432|    "(" "readonly" ")" { Some("readonly") }

///|
fn yy_action_111(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_String_(
    {
      ()
      Some("readonly")
    },
  )
}

// file:///./mbti_parser.mbty
// 433|    "(" LIDENT ")" { Some($2) }

///|
fn yy_action_112(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  YYObj_String_(
    {
      ()
      Some(_dollar2)
    },
  )
}

// file:///./mbti_parser.mbty
// 431|    { None }

///|
fn yy_action_113(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_String_(
    {
      ()
      None
    },
  )
}

// file:///./mbti_parser.mbty
// 177|    attributes vis "let" lident ":" type_ { ValueSig::{ attrs: $1, name: $4, type_: $6 } }

///|
fn yy_action_114(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig(
    {
      ()
      ValueSig::{ attrs: _dollar1, name: _dollar4, type_: _dollar6 }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_115(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  let _sub_action_2_result = yy_action_20(_args[0].2, _args[1:2])
  let _sub_action_3_result = yy_action_21(_args[4].2, _args[5:8])
  guard _args[4].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[3].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[9].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_116(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  let _sub_action_2_result = yy_action_23(_args[0].2, _args[1:1])
  let _sub_action_3_result = yy_action_21(_args[3].2, _args[4:7])
  guard _args[3].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[8].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 223|    attributes vis "type" type_decl_name_with_params optional_derive {
// 224|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5} 
// 225|  }

///|
fn yy_action_117(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Abstract,
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 281|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 282|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6) 
// 283|  }

///|
fn yy_action_118(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig(
    {
      ()
      TypeAlias(
        attrs=_dollar1,
        vis=_dollar2,
        name=_dollar4.0,
        type_params=_dollar4.1,
        type_=_dollar6,
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 226|    attributes vis "suberror" uident optional_derive {
// 227|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 228|  }

///|
fn yy_action_119(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(NoPayload),
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 229|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 230|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 231|  }

///|
fn yy_action_120(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(EnumPayload(_dollar6)),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 233|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 234|  }

///|
fn yy_action_121(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: _dollar6,
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 236|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 237|  }

///|
fn yy_action_122(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: TupleStruct(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 240|  }

///|
fn yy_action_123(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Variant(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 242|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 243|  }

///|
fn yy_action_124(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: ExtensibleEnum(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 245|    let (name, type_params, target) = $4
// 246|    TypeSig::{attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 247|  }

///|
fn yy_action_125(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0
    is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig(
    {
      ()
      let (name, type_params, target) = _dollar4
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name,
        type_params,
        components: ExtendEnum(target~, constructors=_dollar7),
        derives: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 256|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 257|    ImplSig::{type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 258|  }

///|
fn yy_action_126(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[3].0 is YYObj_QualifiedName(_dollar5)
  guard _args[5].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: _dollar4,
        trait_name: _dollar5,
        type_: _dollar7,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 259|    attributes vis "impl" qualified_uident "for" type_ { 
// 260|    ImplSig::{type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 261|  }

///|
fn yy_action_127(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj_QualifiedName(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: @list.empty(),
        trait_name: _dollar4,
        type_: _dollar6,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body { 
// 266|    TraitSig::{ attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6} 
// 267|  }

///|
fn yy_action_128(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[4].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig(
    {
      ()
      TraitSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        super_traits: _dollar5,
        methods: _dollar6,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 284|    attributes vis "fnalias" uident "::" lident {
// 285|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 286|  }

///|
fn yy_action_129(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar6)
  guard _args[2].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig(
    {
      ()
      FuncAlias(attrs=_dollar1, name=_dollar6, type_name=_dollar4)
    },
  )
}

// file:///./mbti_parser.mbty
// 287|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 288|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 289|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 290|  }

///|
fn yy_action_130(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[2].1
  let _end_pos_of_item3 = _args[2].2
  guard _args[5].0 is YYObj_Name(_dollar7)
  guard _args[6].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Type, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 291|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 292|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 293|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 294|  }

///|
fn yy_action_131(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[2].1
  let _end_pos_of_item3 = _args[2].2
  guard _args[5].0 is YYObj_Name(_dollar7)
  guard _args[6].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Trait, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 177|    attributes vis "let" lident ":" type_ { ValueSig::{ attrs: $1, name: $4, type_: $6 } }

///|
fn yy_action_132(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[1].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig(
    {
      ()
      ValueSig::{ attrs: _dollar1, name: _dollar4, type_: _dollar6 }
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_133(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  let _sub_action_2_result = yy_action_20(_last_pos, _args[0:1])
  let _sub_action_3_result = yy_action_21(_args[3].2, _args[4:7])
  guard _args[3].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[8].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 181|   UIDENT "::" {
// 182|  MethodSelfType::{ name : $1, is_object : false, loc : mk_loc($loc($1)) }
// 183| }

///|
fn yy_action_134(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  YYObj_MethodSelfType(
    {
      ()
      MethodSelfType::{
        name: _dollar1,
        is_object: false,
        loc: mk_loc((_start_pos_of_item0, _end_pos_of_item0)),
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 184|   "&" UIDENT "::" {
// 185|  MethodSelfType::{ name : $2, is_object : true, loc : mk_loc($loc($2)) }
// 186| }

///|
fn yy_action_135(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj_MethodSelfType(
    {
      ()
      MethodSelfType::{
        name: _dollar2,
        is_object: true,
        loc: mk_loc((_start_pos_of_item1, _end_pos_of_item1)),
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 195|    luident { (None, $1) }

///|
fn yy_action_136(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj__MethodSelfType___Name_(
    {
      ()
      (None, _dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 190|    lident { $1 }

///|
fn yy_action_137(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_Name(
    {
      ()
      _dollar1
    },
  )
}

// file:///./mbti_parser.mbty
// 191|    uident { $1 }

///|
fn yy_action_138(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_Name(
    {
      ()
      _dollar1
    },
  )
}

// file:///./mbti_parser.mbty
// 196|    method_self_type_coloncolon luident { (Some($1), $2) }

///|
fn yy_action_139(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_MethodSelfType(_dollar1)
  guard _args[1].0 is YYObj_Name(_dollar2)
  YYObj__MethodSelfType___Name_(
    {
      ()
      (Some(_dollar1), _dollar2)
    },
  )
}

// file:///./mbti_parser.mbty
// 206|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 207|    let (type_name, name) = $6
// 208|    FuncSig::{ attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 209|  }

///|
fn yy_action_140(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  let _sub_action_2_result = yy_action_23(_last_pos, _args[0:0])
  let _sub_action_3_result = yy_action_21(_args[2].2, _args[3:6])
  guard _args[2].0 is YYObj__MethodSelfType___Name_(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_2_result is YYObj_Location_(_dollar3)
  guard _args[1].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar5)
  guard _sub_action_3_result is YYObj__list_List_Parameter_(_dollar7)
  guard _args[7].0 is YYObj__Type__ErrorType_(_dollar9)
  YYObj_FuncSig(
    {
      ()
      let (type_name, name) = _dollar6
      FuncSig::{
        attrs: _dollar1,
        is_async: _dollar3,
        type_name,
        name,
        type_params: _dollar5,
        params: _dollar7,
        return_: _dollar9,
      }
    },
  )
}

// file:///./stdlib.mbty
// 15|    X { $1 }

///|
fn yy_action_141(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar1)
  YYObj__list_List_TypeParamWithConstraints_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./stdlib.mbty
// 14|    { @list.empty() }

///|
fn yy_action_142(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_TypeParamWithConstraints_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 223|    attributes vis "type" type_decl_name_with_params optional_derive {
// 224|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5} 
// 225|  }

///|
fn yy_action_143(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Abstract,
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 281|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 282|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6) 
// 283|  }

///|
fn yy_action_144(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig(
    {
      ()
      TypeAlias(
        attrs=_dollar1,
        vis=_dollar2,
        name=_dollar4.0,
        type_params=_dollar4.1,
        type_=_dollar6,
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 226|    attributes vis "suberror" uident optional_derive {
// 227|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 228|  }

///|
fn yy_action_145(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj_Name(_dollar4)
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(NoPayload),
        derives: _dollar5,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 229|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 230|    TypeSig::{attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 231|  }

///|
fn yy_action_146(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        type_params: @list.empty(),
        components: Error(EnumPayload(_dollar6)),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 233|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 234|  }

///|
fn yy_action_147(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: _dollar6,
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 319|    separated_list(";", record_decl_field) {
// 320|    Record(fields = $1, constr = None)
// 321|  }

///|
fn yy_action_148(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_FieldDecl_(_dollar1)
  YYObj_TypeDefinition(
    {
      ()
      Record(fields=_dollar1, constr=None)
    },
  )
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }

///|
fn yy_action_149(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_FieldDecl_(_dollar1)
  YYObj__list_List_FieldDecl_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./mbti_parser.mbty
// 313|    attributes boption("mut") LIDENT ":" type_ { 
// 314|    FieldDecl::{ name: FieldName::{ label: $3, loc : mk_loc($loc($3)) }, vis: Default, mut_: $2, ty: $5, loc: mk_loc($sloc), attrs: $1, doc: DocString::empty() } 
// 315|  }

///|
fn yy_action_150(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_String(_dollar3)
  let _start_pos_of_item2 = _args[1].1
  let _end_pos_of_item2 = _args[1].2
  guard _args[0].0 is YYObj_Bool(_dollar2)
  guard _args[3].0 is YYObj_Type(_dollar5)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_FieldDecl(
    {
      ()
      FieldDecl::{
        name: FieldName::{
          label: _dollar3,
          loc: mk_loc((_start_pos_of_item2, _end_pos_of_item2)),
        },
        vis: Default,
        mut_: _dollar2,
        ty: _dollar5,
        loc: mk_loc((_symbol_start_pos, _end_pos)),
        attrs: _dollar1,
        doc: DocString::empty(),
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 313|    attributes boption("mut") LIDENT ":" type_ { 
// 314|    FieldDecl::{ name: FieldName::{ label: $3, loc : mk_loc($loc($3)) }, vis: Default, mut_: $2, ty: $5, loc: mk_loc($sloc), attrs: $1, doc: DocString::empty() } 
// 315|  }

///|
fn yy_action_151(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_String(_dollar3)
  let _start_pos_of_item2 = _args[2].1
  let _end_pos_of_item2 = _args[2].2
  guard _args[1].0 is YYObj_Bool(_dollar2)
  guard _args[4].0 is YYObj_Type(_dollar5)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_FieldDecl(
    {
      ()
      FieldDecl::{
        name: FieldName::{
          label: _dollar3,
          loc: mk_loc((_start_pos_of_item2, _end_pos_of_item2)),
        },
        vis: Default,
        mut_: _dollar2,
        ty: _dollar5,
        loc: mk_loc((_symbol_start_pos, _end_pos)),
        attrs: _dollar1,
        doc: DocString::empty(),
      }
    },
  )
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }

///|
fn yy_action_152(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_FieldDecl_(_dollar3)
  guard _args[0].0 is YYObj_FieldDecl(_dollar1)
  YYObj__list_List_FieldDecl_(
    {
      ()
      _dollar3.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }

///|
fn yy_action_153(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_FieldDecl(_dollar1)
  YYObj__list_List_FieldDecl_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }

///|
fn yy_action_154(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_FieldDecl_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 236|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 237|  }

///|
fn yy_action_155(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: TupleStruct(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }

///|
fn yy_action_156(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_Type_(_dollar1)
  YYObj__list_List_Type_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }

///|
fn yy_action_157(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_Type_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 240|  }

///|
fn yy_action_158(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: Variant(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 242|    TypeSig::{attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 243|  }

///|
fn yy_action_159(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig(
    {
      ()
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4.0,
        type_params: _dollar4.1,
        components: ExtensibleEnum(_dollar6),
        derives: _dollar8,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 245|    let (name, type_params, target) = $4
// 246|    TypeSig::{attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 247|  }

///|
fn yy_action_160(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0
    is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig(
    {
      ()
      let (name, type_params, target) = _dollar4
      TypeSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name,
        type_params,
        components: ExtendEnum(target~, constructors=_dollar7),
        derives: _dollar9,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 252|    "derive" "(" non_empty_list_commas(qualified_uident) ")" { $3 }

///|
fn yy_action_161(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar3)
  YYObj__list_List_QualifiedName_(
    {
      ()
      _dollar3
    },
  )
}

// file:///./mbti_parser.mbty
// 139|    non_empty_list_commas_rev(X) option(",") { $1.rev() }

///|
fn yy_action_162(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_QualifiedName_(_dollar1)
  YYObj__list_List_QualifiedName_(
    {
      ()
      _dollar1.rev()
    },
  )
}

// file:///./mbti_parser.mbty
// 135|    non_empty_list_commas_rev(X) "," X { $1.add($3) }

///|
fn yy_action_163(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_QualifiedName_(_dollar1)
  guard _args[2].0 is YYObj_QualifiedName(_dollar3)
  YYObj__list_List_QualifiedName_(
    {
      ()
      _dollar1.add(_dollar3)
    },
  )
}

// file:///./mbti_parser.mbty
// 134|    X { @list.singleton($1) }

///|
fn yy_action_164(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__list_List_QualifiedName_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 251|    { @list.empty() }

///|
fn yy_action_165(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_QualifiedName_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }

///|
fn yy_action_166(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_ConstrDecl_(_dollar1)
  YYObj__list_List_ConstrDecl_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./mbti_parser.mbty
// 298|    attributes UIDENT option(delimited("(", separated_nonempty_list(",", constructor_param), ")")) option(eq_tag) { 
// 299|    ConstrDecl::{name: ConstrName::{ name : $2, loc : mk_loc($loc($2)) }, args: $3, tag: $4, loc: mk_loc($sloc), attrs: $1, doc: DocString::empty()} 
// 300|  }

///|
fn yy_action_167(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  guard _args[0].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[0].1
  let _end_pos_of_item1 = _args[0].2
  guard _args[1].0 is YYObj_Option__list_List_ConstrParam__(_dollar3)
  guard _args[2].0 is YYObj_Option__String__Location__(_dollar4)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstrDecl(
    {
      ()
      ConstrDecl::{
        name: ConstrName::{
          name: _dollar2,
          loc: mk_loc((_start_pos_of_item1, _end_pos_of_item1)),
        },
        args: _dollar3,
        tag: _dollar4,
        loc: mk_loc((_symbol_start_pos, _end_pos)),
        attrs: _dollar1,
        doc: DocString::empty(),
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 298|    attributes UIDENT option(delimited("(", separated_nonempty_list(",", constructor_param), ")")) option(eq_tag) { 
// 299|    ConstrDecl::{name: ConstrName::{ name : $2, loc : mk_loc($loc($2)) }, args: $3, tag: $4, loc: mk_loc($sloc), attrs: $1, doc: DocString::empty()} 
// 300|  }

///|
fn yy_action_168(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  guard _args[1].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  guard _args[2].0 is YYObj_Option__list_List_ConstrParam__(_dollar3)
  guard _args[3].0 is YYObj_Option__String__Location__(_dollar4)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstrDecl(
    {
      ()
      ConstrDecl::{
        name: ConstrName::{
          name: _dollar2,
          loc: mk_loc((_start_pos_of_item1, _end_pos_of_item1)),
        },
        args: _dollar3,
        tag: _dollar4,
        loc: mk_loc((_symbol_start_pos, _end_pos)),
        attrs: _dollar1,
        doc: DocString::empty(),
      }
    },
  )
}

// file:///./stdlib.mbty
// 5|    X { Some($1) }

///|
fn yy_action_169(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_170(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__String__Location_(_dollar1)
  YYObj_Option__String__Location__(
    {
      ()
      Some(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 304|    "=" INT { ($2, mk_loc($loc($2))) }

///|
fn yy_action_170(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj__String__Location_(
    {
      ()
      (_dollar2, mk_loc((_start_pos_of_item1, _end_pos_of_item1)))
    },
  )
}

// file:///./stdlib.mbty
// 4|    { None }

///|
fn yy_action_171(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Option__String__Location__(
    {
      ()
      None
    },
  )
}

// file:///./mbti_parser.mbty
// 308|    boption("mut") type_ { ConstrParam::{mut_: $1, ty: $2, label: None} }

///|
fn yy_action_172(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  guard _args[1].0 is YYObj_Type(_dollar2)
  YYObj_ConstrParam(
    {
      ()
      ConstrParam::{ mut_: _dollar1, ty: _dollar2, label: None }
    },
  )
}

// file:///./mbti_parser.mbty
// 309|    boption("mut") post_label ":" type_ { ConstrParam::{mut_: $1, ty: $4, label: Some($2)} }

///|
fn yy_action_173(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  guard _args[3].0 is YYObj_Type(_dollar4)
  guard _args[1].0 is YYObj_Label(_dollar2)
  YYObj_ConstrParam(
    {
      ()
      ConstrParam::{ mut_: _dollar1, ty: _dollar4, label: Some(_dollar2) }
    },
  )
}

// file:///./mbti_parser.mbty
// 464|    POST_LABEL { Label::{name: $1, loc: Location::trim_last_char(mk_loc($sloc))} }

///|
fn yy_action_174(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Label(
    {
      ()
      Label::{
        name: _dollar1,
        loc: Location::trim_last_char(mk_loc((_symbol_start_pos, _end_pos))),
      }
    },
  )
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }

///|
fn yy_action_175(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_ConstrParam_(_dollar3)
  guard _args[0].0 is YYObj_ConstrParam(_dollar1)
  YYObj__list_List_ConstrParam_(
    {
      ()
      _dollar3.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }

///|
fn yy_action_176(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_ConstrParam(_dollar1)
  YYObj__list_List_ConstrParam_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 5|    X { Some($1) }

///|
fn yy_action_177(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_178(_last_pos, _args[0:3])
  guard _sub_action_0_result is YYObj__list_List_ConstrParam_(_dollar1)
  YYObj_Option__list_List_ConstrParam__(
    {
      ()
      Some(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }

///|
fn yy_action_178(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj__list_List_ConstrParam_(_dollar2)
  YYObj__list_List_ConstrParam_(
    {
      ()
      _dollar2
    },
  )
}

// file:///./stdlib.mbty
// 20|    X { true }

///|
fn yy_action_179(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Bool(
    {
      ()
      true
    },
  )
}

// file:///./stdlib.mbty
// 19|    { false }

///|
fn yy_action_180(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Bool(
    {
      ()
      false
    },
  )
}

// file:///./stdlib.mbty
// 4|    { None }

///|
fn yy_action_181(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Option__list_List_ConstrParam__(
    {
      ()
      None
    },
  )
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }

///|
fn yy_action_182(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_ConstrDecl_(_dollar3)
  guard _args[0].0 is YYObj_ConstrDecl(_dollar1)
  YYObj__list_List_ConstrDecl_(
    {
      ()
      _dollar3.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }

///|
fn yy_action_183(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_ConstrDecl(_dollar1)
  YYObj__list_List_ConstrDecl_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }

///|
fn yy_action_184(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_ConstrDecl_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 352|    uident optional_type_params_no_constraints { ($1, $2) }

///|
fn yy_action_185(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[1].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar2)
  YYObj__Name___list_List_TypeParamNoConstraints__(
    {
      ()
      (_dollar1, _dollar2)
    },
  )
}

// file:///./mbti_parser.mbty
// 356|    uident optional_type_params_no_constraints {
// 357|    let target = QualifiedName::{ name: Ident(name = $1.name), loc: $1.loc }
// 358|    ($1, $2, target)
// 359|  }

///|
fn yy_action_186(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[1].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar2)
  YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(
    {
      ()
      let target = QualifiedName::{
        name: Ident(name=_dollar1.name),
        loc: _dollar1.loc,
      }
      (_dollar1, _dollar2, target)
    },
  )
}

// file:///./mbti_parser.mbty
// 339|    "[" separated_list(",", type_param_no_constraints) "]" { $2 }

///|
fn yy_action_187(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar2)
  YYObj__list_List_TypeParamNoConstraints_(
    {
      ()
      _dollar2
    },
  )
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }

///|
fn yy_action_188(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar1)
  YYObj__list_List_TypeParamNoConstraints_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./mbti_parser.mbty
// 334|    uident { Name($1) }

///|
fn yy_action_189(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_TypeParamNoConstraints(
    {
      ()
      Name(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 335|    "_" { Underscore(mk_loc($sloc)) }

///|
fn yy_action_190(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_TypeParamNoConstraints(
    {
      ()
      Underscore(mk_loc((_symbol_start_pos, _end_pos)))
    },
  )
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }

///|
fn yy_action_191(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar3)
  guard _args[0].0 is YYObj_TypeParamNoConstraints(_dollar1)
  YYObj__list_List_TypeParamNoConstraints_(
    {
      ()
      _dollar3.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }

///|
fn yy_action_192(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamNoConstraints(_dollar1)
  YYObj__list_List_TypeParamNoConstraints_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }

///|
fn yy_action_193(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_TypeParamNoConstraints_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 344|    type_params_no_constraints { $1 }

///|
fn yy_action_194(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar1)
  YYObj__list_List_TypeParamNoConstraints_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./mbti_parser.mbty
// 360|    PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints {
// 361|    let name = Name::{ name: $2, loc: mk_loc($loc($2)) }
// 362|    let target = QualifiedName::{ name: Dot(pkg = $1, id = $2), loc: mk_loc($sloc) }
// 363|    (name, $3, target)
// 364|  }

///|
fn yy_action_195(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  guard _args[2].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar3)
  YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(
    {
      ()
      let name = Name::{
        name: _dollar2,
        loc: mk_loc((_start_pos_of_item1, _end_pos_of_item1)),
      }
      let target = QualifiedName::{
        name: Dot(pkg=_dollar1, id=_dollar2),
        loc: mk_loc((_symbol_start_pos, _end_pos)),
      }
      (name, _dollar3, target)
    },
  )
}

// file:///./mbti_parser.mbty
// 343|    { @list.empty() }

///|
fn yy_action_196(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_TypeParamNoConstraints_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 256|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 257|    ImplSig::{type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 258|  }

///|
fn yy_action_197(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[2].0 is YYObj_QualifiedName(_dollar5)
  guard _args[4].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: _dollar4,
        trait_name: _dollar5,
        type_: _dollar7,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 259|    attributes vis "impl" qualified_uident "for" type_ { 
// 260|    ImplSig::{type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 261|  }

///|
fn yy_action_198(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_QualifiedName(_dollar4)
  guard _args[3].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig(
    {
      ()
      ImplSig::{
        type_params: @list.empty(),
        trait_name: _dollar4,
        type_: _dollar6,
        attrs: _dollar1,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 330|    "[" separated_list(",", type_param_with_constraints) "]" { $2 }

///|
fn yy_action_199(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar2)
  YYObj__list_List_TypeParamWithConstraints_(
    {
      ()
      _dollar2
    },
  )
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }

///|
fn yy_action_200(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar1)
  YYObj__list_List_TypeParamWithConstraints_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./mbti_parser.mbty
// 326|    uident ":" separated_nonempty_list("+", type_constraint) { TypeParamWithConstraints::{name: $1, constraints: $3} }

///|
fn yy_action_201(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar3)
  YYObj_TypeParamWithConstraints(
    {
      ()
      TypeParamWithConstraints::{ name: _dollar1, constraints: _dollar3 }
    },
  )
}

// file:///./mbti_parser.mbty
// 348|    qualified_uident { $1 }

///|
fn yy_action_202(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj_QualifiedName(
    {
      ()
      _dollar1
    },
  )
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }

///|
fn yy_action_203(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar3)
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__list_List_QualifiedName_(
    {
      ()
      _dollar3.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }

///|
fn yy_action_204(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__list_List_QualifiedName_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 325|    uident { TypeParamWithConstraints::{name: $1, constraints: @list.empty()} }

///|
fn yy_action_205(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_TypeParamWithConstraints(
    {
      ()
      TypeParamWithConstraints::{ name: _dollar1, constraints: @list.empty() }
    },
  )
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }

///|
fn yy_action_206(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar3)
  guard _args[0].0 is YYObj_TypeParamWithConstraints(_dollar1)
  YYObj__list_List_TypeParamWithConstraints_(
    {
      ()
      _dollar3.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }

///|
fn yy_action_207(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamWithConstraints(_dollar1)
  YYObj__list_List_TypeParamWithConstraints_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }

///|
fn yy_action_208(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_TypeParamWithConstraints_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body { 
// 266|    TraitSig::{ attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6} 
// 267|  }

///|
fn yy_action_209(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj_Name(_dollar4)
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[3].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig(
    {
      ()
      TraitSig::{
        attrs: _dollar1,
        vis: _dollar2,
        name: _dollar4,
        super_traits: _dollar5,
        methods: _dollar6,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 272|    "{" separated_list(";", trait_method_sig) "}" { $2 }

///|
fn yy_action_210(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj__list_List_TraitMethodSig_(_dollar2)
  YYObj__list_List_TraitMethodSig_(
    {
      ()
      _dollar2
    },
  )
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }

///|
fn yy_action_211(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_TraitMethodSig_(_dollar1)
  YYObj__list_List_TraitMethodSig_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./mbti_parser.mbty
// 213|    attributes is_async lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) { 
// 214|    TraitMethodSig::{ attrs : $1, is_async: $2, name: $3, params: $4, return_: $6, has_default: $7} 
// 215|  }

///|
fn yy_action_212(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_20(_last_pos, _args[0:1])
  let _sub_action_2_result = yy_action_21(_args[1].2, _args[2:5])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[1].0 is YYObj_Name(_dollar3)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar4)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _args[7].0 is YYObj_Bool(_dollar7)
  YYObj_TraitMethodSig(
    {
      ()
      TraitMethodSig::{
        attrs: _dollar1,
        is_async: _dollar2,
        name: _dollar3,
        params: _dollar4,
        return_: _dollar6,
        has_default: _dollar7,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 213|    attributes is_async lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) { 
// 214|    TraitMethodSig::{ attrs : $1, is_async: $2, name: $3, params: $4, return_: $6, has_default: $7} 
// 215|  }

///|
fn yy_action_213(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_23(_last_pos, _args[0:0])
  let _sub_action_2_result = yy_action_21(_args[0].2, _args[1:4])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[0].0 is YYObj_Name(_dollar3)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar4)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _args[6].0 is YYObj_Bool(_dollar7)
  YYObj_TraitMethodSig(
    {
      ()
      TraitMethodSig::{
        attrs: _dollar1,
        is_async: _dollar2,
        name: _dollar3,
        params: _dollar4,
        return_: _dollar6,
        has_default: _dollar7,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 213|    attributes is_async lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) { 
// 214|    TraitMethodSig::{ attrs : $1, is_async: $2, name: $3, params: $4, return_: $6, has_default: $7} 
// 215|  }

///|
fn yy_action_214(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_20(_args[0].2, _args[1:2])
  let _sub_action_2_result = yy_action_21(_args[2].2, _args[3:6])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar3)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar4)
  guard _args[7].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _args[8].0 is YYObj_Bool(_dollar7)
  YYObj_TraitMethodSig(
    {
      ()
      TraitMethodSig::{
        attrs: _dollar1,
        is_async: _dollar2,
        name: _dollar3,
        params: _dollar4,
        return_: _dollar6,
        has_default: _dollar7,
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 213|    attributes is_async lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) { 
// 214|    TraitMethodSig::{ attrs : $1, is_async: $2, name: $3, params: $4, return_: $6, has_default: $7} 
// 215|  }

///|
fn yy_action_215(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_23(_args[0].2, _args[1:1])
  let _sub_action_2_result = yy_action_21(_args[1].2, _args[2:5])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[1].0 is YYObj_Name(_dollar3)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar4)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _args[7].0 is YYObj_Bool(_dollar7)
  YYObj_TraitMethodSig(
    {
      ()
      TraitMethodSig::{
        attrs: _dollar1,
        is_async: _dollar2,
        name: _dollar3,
        params: _dollar4,
        return_: _dollar6,
        has_default: _dollar7,
      }
    },
  )
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }

///|
fn yy_action_21(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj__list_List_Parameter_(_dollar2)
  YYObj__list_List_Parameter_(
    {
      ()
      _dollar2
    },
  )
}

// file:///./mbti_parser.mbty
// 469|    nonempty_list(attribute) { $1 }

///|
fn yy_action_60(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_Attribute_(_dollar1)
  YYObj__list_List_Attribute_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./stdlib.mbty
// 20|    X { true }

///|
fn yy_action_216(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_217(_last_pos, _args[0:2])
  YYObj_Bool(
    {
      ()
      true
    },
  )
}

// file:///./mbti_parser.mbty
// 219|    "=" "_" {}

///|
fn yy_action_217(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Unit(())
}

// file:///./stdlib.mbty
// 19|    { false }

///|
fn yy_action_218(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Bool(
    {
      ()
      false
    },
  )
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }

///|
fn yy_action_219(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_Parameter_(_dollar1)
  YYObj__list_List_Parameter_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./mbti_parser.mbty
// 404|    type_ { DiscardPositional(ty = Some($1), loc = @syntax.no_location) }

///|
fn yy_action_220(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj_Parameter(
    {
      ()
      DiscardPositional(ty=Some(_dollar1), loc=@syntax.no_location)
    },
  )
}

// file:///./mbti_parser.mbty
// 405|    POST_LABEL ":" type_ { Labelled(binder = { name : $1, loc : mk_loc($loc($1)) }, ty = Some($3)) }

///|
fn yy_action_221(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj_Parameter(
    {
      ()
      Labelled(
        binder={
          name: _dollar1,
          loc: mk_loc((_start_pos_of_item0, _end_pos_of_item0)),
        },
        ty=Some(_dollar3),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 378|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { Arrow(args=$5.unwrap_or(@list.empty()).add($3), res=$8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }

///|
fn yy_action_222(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_20(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_223(_args[3].2, _args[4:4])
  guard _sub_action_1_result is YYObj_Option__list_List_Type__(_dollar5)
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Arrow(
        args=_dollar5.unwrap_or(@list.empty()).add(_dollar3),
        res=_dollar8.0,
        err=_dollar8.1,
        is_async=_dollar1,
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 378|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { Arrow(args=$5.unwrap_or(@list.empty()).add($3), res=$8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }

///|
fn yy_action_224(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_20(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_225(_args[3].2, _args[4:5])
  guard _sub_action_1_result is YYObj_Option__list_List_Type__(_dollar5)
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _args[7].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Arrow(
        args=_dollar5.unwrap_or(@list.empty()).add(_dollar3),
        res=_dollar8.0,
        err=_dollar8.1,
        is_async=_dollar1,
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 380|    is_async "(" type_ ")" "->" return_type { Arrow(args=@list.singleton($3), res=$6.0, err=$6.1, is_async=$1, loc=mk_loc($sloc)) }

///|
fn yy_action_226(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_20(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Arrow(
        args=@list.singleton(_dollar3),
        res=_dollar6.0,
        err=_dollar6.1,
        is_async=_dollar1,
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 371|    qualified_uident_ optional_type_arguments { Name(constr_id=$1, tys=$2, loc=mk_loc($sloc)) }

///|
fn yy_action_227(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_ConstrId(_dollar1)
  guard _args[1].0 is YYObj__list_List_Type_(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Name(
        constr_id=_dollar1,
        tys=_dollar2,
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 399|    delimited("[", separated_nonempty_list(",", type_), "]") { $1 }

///|
fn yy_action_228(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_229(_last_pos, _args[0:3])
  guard _sub_action_0_result is YYObj__list_List_Type_(_dollar1)
  YYObj__list_List_Type_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }

///|
fn yy_action_229(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj__list_List_Type_(_dollar2)
  YYObj__list_List_Type_(
    {
      ()
      _dollar2
    },
  )
}

// file:///./mbti_parser.mbty
// 378|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { Arrow(args=$5.unwrap_or(@list.empty()).add($3), res=$8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }

///|
fn yy_action_230(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_23(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_225(_args[2].2, _args[3:4])
  guard _sub_action_1_result is YYObj_Option__list_List_Type__(_dollar5)
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Arrow(
        args=_dollar5.unwrap_or(@list.empty()).add(_dollar3),
        res=_dollar8.0,
        err=_dollar8.1,
        is_async=_dollar1,
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./stdlib.mbty
// 10|    X { Some($1) }

///|
fn yy_action_225(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_Type_(_dollar1)
  YYObj_Option__list_List_Type__(
    {
      ()
      Some(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 369|    "(" type_ "," separated_nonempty_list(",", type_) ")" { Tuple(tys=$4.add($2), loc=mk_loc($sloc)) }

///|
fn yy_action_231(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[3].0 is YYObj__list_List_Type_(_dollar4)
  guard _args[1].0 is YYObj_Type(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Tuple(
        tys=_dollar4.add(_dollar2),
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 378|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { Arrow(args=$5.unwrap_or(@list.empty()).add($3), res=$8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }

///|
fn yy_action_232(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_23(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_223(_args[2].2, _args[3:3])
  guard _sub_action_1_result is YYObj_Option__list_List_Type__(_dollar5)
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Arrow(
        args=_dollar5.unwrap_or(@list.empty()).add(_dollar3),
        res=_dollar8.0,
        err=_dollar8.1,
        is_async=_dollar1,
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./stdlib.mbty
// 9|    { None }

///|
fn yy_action_223(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Option__list_List_Type__(
    {
      ()
      None
    },
  )
}

// file:///./mbti_parser.mbty
// 380|    is_async "(" type_ ")" "->" return_type { Arrow(args=@list.singleton($3), res=$6.0, err=$6.1, is_async=$1, loc=mk_loc($sloc)) }

///|
fn yy_action_233(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_23(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _args[4].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Arrow(
        args=@list.singleton(_dollar3),
        res=_dollar6.0,
        err=_dollar6.1,
        is_async=_dollar1,
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 370|    "(" type_ ")" { $2 }

///|
fn yy_action_234(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_Type(_dollar2)
  YYObj_Type(
    {
      ()
      _dollar2
    },
  )
}

// file:///./mbti_parser.mbty
// 368|    simple_type "?" { Option(ty=$1, loc=mk_loc($sloc), question_loc=mk_loc($loc($2))) }

///|
fn yy_action_235(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj_Type(
    {
      ()
      Option(
        ty=_dollar1,
        loc=mk_loc((_symbol_start_pos, _end_pos)),
        question_loc=mk_loc((_start_pos_of_item1, _end_pos_of_item1)),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 385|    simple_type "noraise" { ($1, Noraise(loc=mk_loc($sloc))) }

///|
fn yy_action_236(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj__Type__ErrorType_(
    {
      ()
      (_dollar1, Noraise(loc=mk_loc((_symbol_start_pos, _end_pos))))
    },
  )
}

// file:///./mbti_parser.mbty
// 387|    simple_type "raise" error_type { ($1, ErrorType(ty = $3)) }

///|
fn yy_action_237(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj__Type__ErrorType_(
    {
      ()
      (_dollar1, ErrorType(ty=_dollar3))
    },
  )
}

// file:///./mbti_parser.mbty
// 388|    simple_type "raise" "?" {
// 389|    let fake_error : Type = Name(constr_id={ id : QualifiedIdent::Ident(name="Error"), loc :@syntax.no_location }, tys=@list.empty(), loc=@syntax.no_location)
// 390|    ($1, MaybeError(ty=fake_error)) 
// 391|  }

///|
fn yy_action_238(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__Type__ErrorType_(
    {
      ()
      let fake_error : Type = Name(
        constr_id={
          id: QualifiedIdent::Ident(name="Error"),
          loc: @syntax.no_location,
        },
        tys=@list.empty(),
        loc=@syntax.no_location,
      )
      (_dollar1, MaybeError(ty=fake_error))
    },
  )
}

// file:///./mbti_parser.mbty
// 395|    qualified_uident_ { Name(constr_id=$1, tys=@list.empty(), loc=mk_loc($sloc)) }

///|
fn yy_action_239(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_ConstrId(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Name(
        constr_id=_dollar1,
        tys=@list.empty(),
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 386|    simple_type "raise" { ($1, DefaultErrorType(loc=mk_loc($loc($2)))) }

///|
fn yy_action_240(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj__Type__ErrorType_(
    {
      ()
      (
        _dollar1,
        DefaultErrorType(loc=mk_loc((_start_pos_of_item1, _end_pos_of_item1))),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 377|    simple_type { $1 }

///|
fn yy_action_241(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj_Type(
    {
      ()
      _dollar1
    },
  )
}

// file:///./mbti_parser.mbty
// 379|    is_async "(" ")" "->" return_type { Arrow(args=@list.empty(), res=$5.0, err=$5.1, is_async=$1, loc=mk_loc($sloc)) }

///|
fn yy_action_242(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_23(_last_pos, _args[0:0])
  guard _args[3].0 is YYObj__Type__ErrorType_(_dollar5)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Arrow(
        args=@list.empty(),
        res=_dollar5.0,
        err=_dollar5.1,
        is_async=_dollar1,
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 438|    { None }

///|
fn yy_action_23(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Location_(
    {
      ()
      None
    },
  )
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }

///|
fn yy_action_243(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_Type_(_dollar3)
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__list_List_Type_(
    {
      ()
      _dollar3.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }

///|
fn yy_action_244(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__list_List_Type_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 400|     { @list.empty() }

///|
fn yy_action_245(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_Type_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 372|    "&" qualified_uident_ { Object($2) }

///|
fn yy_action_246(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_ConstrId(_dollar2)
  YYObj_Type(
    {
      ()
      Object(_dollar2)
    },
  )
}

// file:///./mbti_parser.mbty
// 373|    "_" { Any(loc=mk_loc($sloc)) }

///|
fn yy_action_247(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Any(loc=mk_loc((_symbol_start_pos, _end_pos)))
    },
  )
}

// file:///./mbti_parser.mbty
// 379|    is_async "(" ")" "->" return_type { Arrow(args=@list.empty(), res=$5.0, err=$5.1, is_async=$1, loc=mk_loc($sloc)) }

///|
fn yy_action_248(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_20(_last_pos, _args[0:1])
  guard _args[4].0 is YYObj__Type__ErrorType_(_dollar5)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Type(
    {
      ()
      Arrow(
        args=@list.empty(),
        res=_dollar5.0,
        err=_dollar5.1,
        is_async=_dollar1,
        loc=mk_loc((_symbol_start_pos, _end_pos)),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 437|    "async" { Some(mk_loc($loc($1))) }

///|
fn yy_action_20(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  YYObj_Location_(
    {
      ()
      Some(mk_loc((_start_pos_of_item0, _end_pos_of_item0)))
    },
  )
}

// file:///./mbti_parser.mbty
// 384|    type_ { ($1, NoErrorType) }

///|
fn yy_action_249(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__Type__ErrorType_(
    {
      ()
      (_dollar1, NoErrorType)
    },
  )
}

// file:///./mbti_parser.mbty
// 406|    label "?" ":" type_ { QuestionOptional(binder = { name : $1.name, loc : $1.loc }, ty = Some($4)) }

///|
fn yy_action_250(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Label(_dollar1)
  guard _args[3].0 is YYObj_Type(_dollar4)
  YYObj_Parameter(
    {
      ()
      QuestionOptional(
        binder={ name: _dollar1.name, loc: _dollar1.loc },
        ty=Some(_dollar4),
      )
    },
  )
}

// file:///./mbti_parser.mbty
// 447|    UIDENT { ConstrId::{ id: Ident(name=$1), loc: mk_loc($sloc)} }

///|
fn yy_action_251(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_ConstrId(
    {
      ()
      ConstrId::{
        id: Ident(name=_dollar1),
        loc: mk_loc((_symbol_start_pos, _end_pos)),
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 448|    PACKAGE_NAME DOT_UIDENT { ConstrId::{ id: Dot(pkg=$1, id=$2), loc: mk_loc($sloc)} }

///|
fn yy_action_252(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  guard _args[1].0 is YYObj_String(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_ConstrId(
    {
      ()
      ConstrId::{
        id: Dot(pkg=_dollar1, id=_dollar2),
        loc: mk_loc((_symbol_start_pos, _end_pos)),
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 460|    LIDENT { Label::{name: $1, loc: mk_loc($sloc)} }

///|
fn yy_action_253(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Label(
    {
      ()
      Label::{ name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos)) }
    },
  )
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }

///|
fn yy_action_254(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_Parameter_(_dollar3)
  guard _args[0].0 is YYObj_Parameter(_dollar1)
  YYObj__list_List_Parameter_(
    {
      ()
      _dollar3.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }

///|
fn yy_action_255(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_Parameter(_dollar1)
  YYObj__list_List_Parameter_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }

///|
fn yy_action_256(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_Parameter_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }

///|
fn yy_action_257(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_TraitMethodSig_(_dollar3)
  guard _args[0].0 is YYObj_TraitMethodSig(_dollar1)
  YYObj__list_List_TraitMethodSig_(
    {
      ()
      _dollar3.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }

///|
fn yy_action_258(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_TraitMethodSig(_dollar1)
  YYObj__list_List_TraitMethodSig_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }

///|
fn yy_action_259(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_TraitMethodSig_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 271|    { @list.empty() }

///|
fn yy_action_260(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_TraitMethodSig_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 442|    UIDENT { QualifiedName::{ name: Ident(name=$1), loc: mk_loc($sloc)} }

///|
fn yy_action_261(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_QualifiedName(
    {
      ()
      QualifiedName::{
        name: Ident(name=_dollar1),
        loc: mk_loc((_symbol_start_pos, _end_pos)),
      }
    },
  )
}

// file:///./mbti_parser.mbty
// 443|    PACKAGE_NAME DOT_UIDENT { QualifiedName::{ name: Dot(pkg=$1, id=$2), loc: mk_loc($sloc)} }

///|
fn yy_action_262(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  guard _args[1].0 is YYObj_String(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_QualifiedName(
    {
      ()
      QualifiedName::{
        name: Dot(pkg=_dollar1, id=_dollar2),
        loc: mk_loc((_symbol_start_pos, _end_pos)),
      }
    },
  )
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }

///|
fn yy_action_263(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar3)
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__list_List_QualifiedName_(
    {
      ()
      _dollar3.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }

///|
fn yy_action_264(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__list_List_QualifiedName_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 15|    X { $1 }

///|
fn yy_action_265(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_266(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_QualifiedName_(_dollar1)
  YYObj__list_List_QualifiedName_(
    {
      ()
      _dollar1
    },
  )
}

// file:///./stdlib.mbty
// 32|    L X { $2 }

///|
fn yy_action_266(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj__list_List_QualifiedName_(_dollar2)
  YYObj__list_List_QualifiedName_(
    {
      ()
      _dollar2
    },
  )
}

// file:///./stdlib.mbty
// 14|    { @list.empty() }

///|
fn yy_action_267(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_QualifiedName_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 284|    attributes vis "fnalias" uident "::" lident {
// 285|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 286|  }

///|
fn yy_action_268(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar6)
  guard _args[1].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig(
    {
      ()
      FuncAlias(attrs=_dollar1, name=_dollar6, type_name=_dollar4)
    },
  )
}

// file:///./mbti_parser.mbty
// 456|    LIDENT { Name::{name: $1, loc: mk_loc($sloc)} }

///|
fn yy_action_269(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Name(
    {
      ()
      Name::{ name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos)) }
    },
  )
}

// file:///./mbti_parser.mbty
// 287|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 288|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 289|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 290|  }

///|
fn yy_action_270(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[1].1
  let _end_pos_of_item3 = _args[1].2
  guard _args[4].0 is YYObj_Name(_dollar7)
  guard _args[5].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Type, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 277|    "as" uident { Some($2) }

///|
fn yy_action_271(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  YYObj_Name_(
    {
      ()
      Some(_dollar2)
    },
  )
}

// file:///./mbti_parser.mbty
// 291|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 292|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 293|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 294|  }

///|
fn yy_action_272(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[1].1
  let _end_pos_of_item3 = _args[1].2
  guard _args[4].0 is YYObj_Name(_dollar7)
  guard _args[5].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig(
    {
      ()
      let pkg = Name::{
        name: _dollar4,
        loc: mk_loc((_start_pos_of_item3, _end_pos_of_item3)),
      }
      Using(pkg~, target=_dollar7, name=_dollar8, kind=Trait, attrs=_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 425|    { Default }

///|
fn yy_action_15(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Visibility(
    {
      ()
      Default
    },
  )
}

// file:///./mbti_parser.mbty
// 468|     {  @list.empty() }

///|
fn yy_action_14(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_Attribute_(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 276|    { None }

///|
fn yy_action_273(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Name_(
    {
      ()
      None
    },
  )
}

// file:///./mbti_parser.mbty
// 452|    UIDENT { Name::{name: $1, loc: mk_loc($sloc)} }

///|
fn yy_action_274(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  YYObj_Name(
    {
      ()
      Name::{ name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos)) }
    },
  )
}

// file:///./stdlib.mbty
// 50|    X nonempty_list(X) { $2.add($1) }

///|
fn yy_action_275(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_276(_last_pos, _args[0:1])
  guard _args[1].0 is YYObj__list_List_Attribute_(_dollar2)
  guard _sub_action_0_result is YYObj_Attribute(_dollar1)
  YYObj__list_List_Attribute_(
    {
      ()
      _dollar2.add(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 49|    X { @list.singleton($1) }

///|
fn yy_action_277(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _sub_action_0_result = yy_action_276(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Attribute(_dollar1)
  YYObj__list_List_Attribute_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 473|    ATTRIBUTE { Attribute::new(loc=mk_loc($sloc), $1) }

///|
fn yy_action_276(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 {
    _last_pos
  } else {
    _args[_args.length() - 1].2
  }
  guard _args[0].0 is YYObj__String__Option_String___String_(_dollar1)
  YYObj_Attribute(
    {
      ()
      Attribute::new(loc=mk_loc((_symbol_start_pos, _end_pos)), _dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 157|     { @list.empty() }

///|
fn yy_action_278(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List__Sig__Location__(
    {
      ()
      @list.empty()
    },
  )
}

// file:///./mbti_parser.mbty
// 148|    "import" "{" non_empty_list_commas(package_import) "}" ";" { $3 }

///|
fn yy_action_279(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[2].0 is YYObj__list_List_PackageImport_(_dollar3)
  YYObj__list_List_PackageImport_(
    {
      ()
      _dollar3
    },
  )
}

// file:///./mbti_parser.mbty
// 139|    non_empty_list_commas_rev(X) option(",") { $1.rev() }

///|
fn yy_action_280(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_PackageImport_(_dollar1)
  YYObj__list_List_PackageImport_(
    {
      ()
      _dollar1.rev()
    },
  )
}

// file:///./mbti_parser.mbty
// 153|    STRING "as" PACKAGE_NAME { PackageImport::{name: $1, alias_: Some($3)} }

///|
fn yy_action_281(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  guard _args[2].0 is YYObj_String(_dollar3)
  YYObj_PackageImport(
    {
      ()
      PackageImport::{ name: _dollar1, alias_: Some(_dollar3) }
    },
  )
}

// file:///./mbti_parser.mbty
// 152|    STRING { PackageImport::{name: $1, alias_: None} }

///|
fn yy_action_282(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_PackageImport(
    {
      ()
      PackageImport::{ name: _dollar1, alias_: None }
    },
  )
}

// file:///./mbti_parser.mbty
// 135|    non_empty_list_commas_rev(X) "," X { $1.add($3) }

///|
fn yy_action_283(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj__list_List_PackageImport_(_dollar1)
  guard _args[2].0 is YYObj_PackageImport(_dollar3)
  YYObj__list_List_PackageImport_(
    {
      ()
      _dollar1.add(_dollar3)
    },
  )
}

// file:///./stdlib.mbty
// 5|    X { Some($1) }

///|
fn yy_action_284(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  let _dollar1 = ()
  YYObj_Option_Unit_(
    {
      ()
      Some(_dollar1)
    },
  )
}

// file:///./stdlib.mbty
// 4|    { None }

///|
fn yy_action_285(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj_Option_Unit_(
    {
      ()
      None
    },
  )
}

// file:///./mbti_parser.mbty
// 134|    X { @list.singleton($1) }

///|
fn yy_action_286(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  guard _args[0].0 is YYObj_PackageImport(_dollar1)
  YYObj__list_List_PackageImport_(
    {
      ()
      @list.singleton(_dollar1)
    },
  )
}

// file:///./mbti_parser.mbty
// 147|     { @list.empty() }

///|
fn yy_action_287(
  _last_pos : @basic.Position,
  _args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
) -> YYObj {
  YYObj__list_List_PackageImport_(
    {
      ()
      @list.empty()
    },
  )
}

///|
fn yy_input(
  token : Token,
  _start_pos : @basic.Position,
  _end_pos : @basic.Position,
) -> (YYSymbol, YYObj) {
  match token {
    CHAR(data) => (T_CHAR, YYObj_CharLiteral(data))
    INT(data) => (T_INT, YYObj_String(data))
    BYTE(data) => (T_BYTE, YYObj_CharLiteral(data))
    BYTES(data) => (T_BYTES, YYObj_StringLiteral(data))
    FLOAT(data) => (T_FLOAT, YYObj_String(data))
    DOUBLE(data) => (T_DOUBLE, YYObj_String(data))
    STRING(data) => (T_STRING, YYObj_StringLiteral(data))
    MULTILINE_STRING(data) => (T_MULTILINE_STRING, YYObj_String(data))
    MULTILINE_INTERP(data) => (T_MULTILINE_INTERP, YYObj_InterpLiteral(data))
    INTERP(data) => (T_INTERP, YYObj_InterpLiteral(data))
    REGEX_LITERAL(data) => (T_REGEX_LITERAL, YYObj_String(data))
    REGEX_INTERP(data) => (T_REGEX_INTERP, YYObj_InterpLiteral(data))
    ATTRIBUTE(data) =>
      (T_ATTRIBUTE, YYObj__String__Option_String___String_(data))
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
    PLUS_EQUAL => (T_PLUS_EQUAL, YYObj_Void)
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
    EQ_TILDE => (T_EQ_TILDE, YYObj_Void)
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
    PIPE_LEFT => (T_PIPE_LEFT, YYObj_Void)
    LT_PLUS => (T_LT_PLUS, YYObj_Void)
    ELSE => (T_ELSE, YYObj_Void)
    FN => (T_FN, YYObj_Void)
    IF => (T_IF, YYObj_Void)
    LET => (T_LET, YYObj_Void)
    PROOF_ASSERT => (T_PROOF_ASSERT, YYObj_Void)
    PROOF_LET => (T_PROOF_LET, YYObj_Void)
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
    RANGE_LT_INCLUSIVE => (T_RANGE_LT_INCLUSIVE, YYObj_Void)
    RANGE_EXCLUSIVE => (T_RANGE_EXCLUSIVE, YYObj_Void)
    RANGE_INCLUSIVE_REV => (T_RANGE_INCLUSIVE_REV, YYObj_Void)
    RANGE_EXCLUSIVE_REV => (T_RANGE_EXCLUSIVE_REV, YYObj_Void)
    ELLIPSIS => (T_ELLIPSIS, YYObj_Void)
    TEST => (T_TEST, YYObj_Void)
    LOOP => (T_LOOP, YYObj_Void)
    GUARD => (T_GUARD, YYObj_Void)
    DEFER => (T_DEFER, YYObj_Void)
    FOR => (T_FOR, YYObj_Void)
    IN => (T_IN, YYObj_Void)
    IS => (T_IS, YYObj_Void)
    SUBERROR => (T_SUBERROR, YYObj_Void)
    AND => (T_AND, YYObj_Void)
    FORALL => (T_FORALL, YYObj_Void)
    EXISTS => (T_EXISTS, YYObj_Void)
    IMPLIES => (T_IMPLIES, YYObj_Void)
    LETREC => (T_LETREC, YYObj_Void)
    USING => (T_USING, YYObj_Void)
    DECLARE => (T_DECLARE, YYObj_Void)
    NORAISE => (T_NORAISE, YYObj_Void)
    NOBREAK => (T_NOBREAK, YYObj_Void)
    WHERE => (T_WHERE, YYObj_Void)
    ENUMVIEW => (T_ENUMVIEW, YYObj_Void)
    EXTENUM => (T_EXTENUM, YYObj_Void)
    TRY_QUESTION => (T_TRY_QUESTION, YYObj_Void)
    TRY_EXCLAMATION => (T_TRY_EXCLAMATION, YYObj_Void)
    LEXMATCH => (T_LEXMATCH, YYObj_Void)
    LEXMATCH_QUESTION => (T_LEXMATCH_QUESTION, YYObj_Void)
    PACKAGE => (T_PACKAGE, YYObj_Void)
  }
}

// [0, mbti → • PACKAGE STRING imports sigs EOF, $]
// [1, mbti → • PACKAGE STRING SEMI imports sigs EOF, $]
// [272, mbti_prime → • mbti, $]

///|
fn yy_state_0(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_mbti => Shift(yy_state_1)
    T_PACKAGE => Shift(yy_state_2)
    _ => Error
  }
}

// [272, mbti_prime → mbti •, $]

///|
fn yy_state_1(_lookahead : YYSymbol) -> YYDecision {
  Accept
}

// [0, mbti → PACKAGE • STRING imports sigs EOF, $]
// [1, mbti → PACKAGE • STRING SEMI imports sigs EOF, $]

///|
fn yy_state_2(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_3)
    _ => Error
  }
}

// [0, mbti → PACKAGE STRING • imports sigs EOF, $]
// [1, mbti → PACKAGE STRING • SEMI imports sigs EOF, $]
// [2, imports → •, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [3, imports → • IMPORT LBRACE non_empty_list_commas(package_import) RBRACE SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_3(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_IMPORT => Shift(yy_state_4)
    T_SEMI => Shift(yy_state_17)
    NT_imports => Shift(yy_state_835)
    T_ATTRIBUTE
    | T_EOF
    | T_PUB
    | T_PRIV
    | T_STRUCT
    | T_ENUM
    | T_TRAIT
    | T_IMPL
    | T_ASYNC
    | T_FNALIAS
    | T_FN
    | T_LET
    | T_CONST
    | T_TYPE
    | T_SUBERROR
    | T_USING
    | T_EXTENUM => Reduce(0, NT_imports, yy_action_287)
    _ => Error
  }
}

// [3, imports → IMPORT • LBRACE non_empty_list_commas(package_import) RBRACE SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_4(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_5)
    _ => Error
  }
}

// [3, imports → IMPORT LBRACE • non_empty_list_commas(package_import) RBRACE SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [4, package_import → • STRING, COMMA / RBRACE]
// [5, package_import → • STRING AS PACKAGE_NAME, COMMA / RBRACE]
// [216, non_empty_list_commas(package_import) → • non_empty_list_commas_rev(package_import) option(COMMA), RBRACE]
// [254, non_empty_list_commas_rev(package_import) → • package_import, COMMA / RBRACE]
// [255, non_empty_list_commas_rev(package_import) → • non_empty_list_commas_rev(package_import) COMMA package_import, COMMA / RBRACE]

///|
fn yy_state_5(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_package_import => Shift(yy_state_6)
    NT_non_empty_list_commas_rev_package_import_ => Shift(yy_state_7)
    T_STRING => Shift(yy_state_10)
    NT_non_empty_list_commas_package_import_ => Shift(yy_state_14)
    _ => Error
  }
}

// [254, non_empty_list_commas_rev(package_import) → package_import •, COMMA / RBRACE]

///|
fn yy_state_6(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    1,
    NT_non_empty_list_commas_rev_package_import_,
    yy_action_286,
  )
}

// [216, non_empty_list_commas(package_import) → non_empty_list_commas_rev(package_import) • option(COMMA), RBRACE]
// [255, non_empty_list_commas_rev(package_import) → non_empty_list_commas_rev(package_import) • COMMA package_import, COMMA / RBRACE]
// [256, option(COMMA) → •, RBRACE]
// [257, option(COMMA) → • COMMA, RBRACE]

///|
fn yy_state_7(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_8)
    NT_option_COMMA_ => Shift(yy_state_13)
    T_RBRACE => Reduce(0, NT_option_COMMA_, yy_action_285)
    _ => Error
  }
}

// [4, package_import → • STRING, COMMA / RBRACE]
// [5, package_import → • STRING AS PACKAGE_NAME, COMMA / RBRACE]
// [255, non_empty_list_commas_rev(package_import) → non_empty_list_commas_rev(package_import) COMMA • package_import, COMMA / RBRACE]
// [257, option(COMMA) → COMMA •, RBRACE]

///|
fn yy_state_8(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_package_import => Shift(yy_state_9)
    T_STRING => Shift(yy_state_10)
    T_RBRACE => Reduce(1, NT_option_COMMA_, yy_action_284)
    _ => Error
  }
}

// [255, non_empty_list_commas_rev(package_import) → non_empty_list_commas_rev(package_import) COMMA package_import •, COMMA / RBRACE]

///|
fn yy_state_9(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_non_empty_list_commas_rev_package_import_,
    yy_action_283,
  )
}

// [4, package_import → STRING •, COMMA / RBRACE]
// [5, package_import → STRING • AS PACKAGE_NAME, COMMA / RBRACE]

///|
fn yy_state_10(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_11)
    T_COMMA | T_RBRACE => Reduce(1, NT_package_import, yy_action_282)
    _ => Error
  }
}

// [5, package_import → STRING AS • PACKAGE_NAME, COMMA / RBRACE]

///|
fn yy_state_11(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_12)
    _ => Error
  }
}

// [5, package_import → STRING AS PACKAGE_NAME •, COMMA / RBRACE]

///|
fn yy_state_12(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_package_import, yy_action_281)
}

// [216, non_empty_list_commas(package_import) → non_empty_list_commas_rev(package_import) option(COMMA) •, RBRACE]

///|
fn yy_state_13(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_non_empty_list_commas_package_import_, yy_action_280)
}

// [3, imports → IMPORT LBRACE non_empty_list_commas(package_import) • RBRACE SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_14(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_15)
    _ => Error
  }
}

// [3, imports → IMPORT LBRACE non_empty_list_commas(package_import) RBRACE • SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_15(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_16)
    _ => Error
  }
}

// [3, imports → IMPORT LBRACE non_empty_list_commas(package_import) RBRACE SEMI •, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_16(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_imports, yy_action_279)
}

// [1, mbti → PACKAGE STRING SEMI • imports sigs EOF, $]
// [2, imports → •, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [3, imports → • IMPORT LBRACE non_empty_list_commas(package_import) RBRACE SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_17(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_IMPORT => Shift(yy_state_4)
    NT_imports => Shift(yy_state_18)
    T_ATTRIBUTE
    | T_EOF
    | T_PUB
    | T_PRIV
    | T_STRUCT
    | T_ENUM
    | T_TRAIT
    | T_IMPL
    | T_ASYNC
    | T_FNALIAS
    | T_FN
    | T_LET
    | T_CONST
    | T_TYPE
    | T_SUBERROR
    | T_USING
    | T_EXTENUM => Reduce(0, NT_imports, yy_action_287)
    _ => Error
  }
}

// [1, mbti → PACKAGE STRING SEMI imports • sigs EOF, $]
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
// [17, const_sig → • PRIV CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [18, const_sig → • PUB pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [19, const_sig → • nonempty_list(attribute) CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [20, const_sig → • nonempty_list(attribute) PRIV CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [21, const_sig → • nonempty_list(attribute) PUB pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [22, value_sig → • LET lident COLON type_, EOF / SEMI]
// [23, value_sig → • PRIV LET lident COLON type_, EOF / SEMI]
// [24, value_sig → • PUB pub_attr LET lident COLON type_, EOF / SEMI]
// [25, value_sig → • nonempty_list(attribute) LET lident COLON type_, EOF / SEMI]
// [26, value_sig → • nonempty_list(attribute) PRIV LET lident COLON type_, EOF / SEMI]
// [27, value_sig → • nonempty_list(attribute) PUB pub_attr LET lident COLON type_, EOF / SEMI]
// [36, func_sig → • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [37, func_sig → • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [38, func_sig → • PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [39, func_sig → • PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [40, func_sig → • PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [41, func_sig → • PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [42, func_sig → • nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [43, func_sig → • nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [44, func_sig → • nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [45, func_sig → • nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [46, func_sig → • nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [47, func_sig → • nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [52, type_sig → • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [53, type_sig → • PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [54, type_sig → • PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [55, type_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [56, type_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [57, type_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [58, type_sig → • SUBERROR uident optional_derive, EOF / SEMI]
// [59, type_sig → • PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [60, type_sig → • PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [61, type_sig → • nonempty_list(attribute) SUBERROR uident optional_derive, EOF / SEMI]
// [62, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [63, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [64, type_sig → • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [65, type_sig → • PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [66, type_sig → • PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [67, type_sig → • nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [68, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [69, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [70, type_sig → • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [71, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [72, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [73, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [74, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [75, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [76, type_sig → • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [77, type_sig → • PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [78, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [79, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [80, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [81, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [82, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [83, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [84, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [85, type_sig → • nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [86, type_sig → • nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [87, type_sig → • nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [88, type_sig → • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [89, type_sig → • PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [90, type_sig → • PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [91, type_sig → • nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [92, type_sig → • nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [93, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [94, type_sig → • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [95, type_sig → • PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → • PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → • nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [98, type_sig → • nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [102, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [103, impl_sig → • PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [104, impl_sig → • PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [105, impl_sig → • nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [106, impl_sig → • nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [107, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [108, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [109, impl_sig → • PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [110, impl_sig → • PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [111, impl_sig → • nonempty_list(attribute) IMPL qualified_uident FOR type_, EOF / SEMI]
// [112, impl_sig → • nonempty_list(attribute) PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [114, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [115, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [116, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [117, trait_sig → • nonempty_list(attribute) TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [118, trait_sig → • nonempty_list(attribute) PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [119, trait_sig → • nonempty_list(attribute) PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [124, alias_sig → • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [125, alias_sig → • PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [126, alias_sig → • PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [127, alias_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [128, alias_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [129, alias_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [130, alias_sig → • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [131, alias_sig → • PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [132, alias_sig → • PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [133, alias_sig → • nonempty_list(attribute) FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [134, alias_sig → • nonempty_list(attribute) PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [135, alias_sig → • nonempty_list(attribute) PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [136, alias_sig → • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [137, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [138, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [139, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [140, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [141, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [142, alias_sig → • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [143, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [252, nonempty_list(attribute) → • ATTRIBUTE, PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_18(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_USING => Shift(yy_state_21)
    T_FNALIAS => Shift(yy_state_35)
    T_TRAIT => Shift(yy_state_40)
    T_IMPL => Shift(yy_state_161)
    T_EXTENUM => Shift(yy_state_183)
    T_ENUM => Shift(yy_state_245)
    T_STRUCT => Shift(yy_state_253)
    T_SUBERROR => Shift(yy_state_278)
    T_TYPE => Shift(yy_state_285)
    T_FN => Shift(yy_state_290)
    T_ASYNC => Shift(yy_state_309)
    T_LET => Shift(yy_state_318)
    NT_nonempty_list_attribute_ => Shift(yy_state_322)
    T_PUB => Shift(yy_state_626)
    T_PRIV => Shift(yy_state_722)
    T_CONST => Shift(yy_state_817)
    NT_value_sig => Shift(yy_state_823)
    NT_const_sig => Shift(yy_state_824)
    NT_impl_sig => Shift(yy_state_825)
    NT_trait_sig => Shift(yy_state_826)
    NT_alias_sig => Shift(yy_state_827)
    NT_type_sig => Shift(yy_state_828)
    NT_func_sig => Shift(yy_state_829)
    NT_sig_ => Shift(yy_state_830)
    NT_sigs => Shift(yy_state_833)
    T_EOF => Reduce(0, NT_sigs, yy_action_278)
    _ => Error
  }
}

// [252, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / UIDENT / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / MUTABLE / TYPE / SUBERROR / USING / EXTENUM]
// [252, nonempty_list(attribute) → ATTRIBUTE •, LIDENT / UIDENT / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / MUTABLE / TYPE / SUBERROR / USING / EXTENUM]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / UIDENT / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / MUTABLE / TYPE / SUBERROR / USING / EXTENUM]
// [253, nonempty_list(attribute) → ATTRIBUTE • nonempty_list(attribute), LIDENT / UIDENT / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / MUTABLE / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_19(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_nonempty_list_attribute_ => Shift(yy_state_20)
    T_LIDENT
    | T_UIDENT
    | T_PUB
    | T_PRIV
    | T_STRUCT
    | T_ENUM
    | T_TRAIT
    | T_IMPL
    | T_ASYNC
    | T_FNALIAS
    | T_FN
    | T_LET
    | T_CONST
    | T_MUTABLE
    | T_TYPE
    | T_SUBERROR
    | T_USING
    | T_EXTENUM => Reduce(1, NT_nonempty_list_attribute_, yy_action_277)
    _ => Error
  }
}

// [253, nonempty_list(attribute) → ATTRIBUTE nonempty_list(attribute) •, LIDENT / UIDENT / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / MUTABLE / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_20(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_nonempty_list_attribute_, yy_action_275)
}

// [136, alias_sig → USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [142, alias_sig → USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_21(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_22)
    _ => Error
  }
}

// [136, alias_sig → USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [142, alias_sig → USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_22(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_23)
    _ => Error
  }
}

// [136, alias_sig → USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [142, alias_sig → USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_23(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_24)
    T_TYPE => Shift(yy_state_31)
    _ => Error
  }
}

// [142, alias_sig → USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_24(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_26)
    _ => Error
  }
}

// [212, uident → UIDENT •, PLUS_EQUAL / EOF / DERIVE / EQUAL / LPAREN / COMMA / COLONCOLON / COLON / SEMI / LBRACKET / RBRACKET / LBRACE / RBRACE / AS]

///|
fn yy_state_25(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_uident, yy_action_274)
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [142, alias_sig → USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_26(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_using_binder => Shift(yy_state_27)
    T_AS => Shift(yy_state_29)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [142, alias_sig → USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_27(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_28)
    _ => Error
  }
}

// [142, alias_sig → USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_28(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_alias_sig, yy_action_272)
}

// [123, using_binder → AS • uident, RBRACE]
// [212, uident → • UIDENT, RBRACE]

///|
fn yy_state_29(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_30)
    _ => Error
  }
}

// [123, using_binder → AS uident •, RBRACE]

///|
fn yy_state_30(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_using_binder, yy_action_271)
}

// [136, alias_sig → USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_31(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_32)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [136, alias_sig → USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_32(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_33)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [136, alias_sig → USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_33(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_34)
    _ => Error
  }
}

// [136, alias_sig → USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_34(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_alias_sig, yy_action_270)
}

// [130, alias_sig → FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [212, uident → • UIDENT, COLONCOLON]

///|
fn yy_state_35(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_36)
    _ => Error
  }
}

// [130, alias_sig → FNALIAS uident • COLONCOLON lident, EOF / SEMI]

///|
fn yy_state_36(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_37)
    _ => Error
  }
}

// [130, alias_sig → FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [213, lident → • LIDENT, EOF / SEMI]

///|
fn yy_state_37(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_39)
    _ => Error
  }
}

// [213, lident → LIDENT •, EOF / LPAREN / COLON / SEMI]

///|
fn yy_state_38(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_lident, yy_action_269)
}

// [130, alias_sig → FNALIAS uident COLONCOLON lident •, EOF / SEMI]

///|
fn yy_state_39(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_alias_sig, yy_action_268)
}

// [114, trait_sig → TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [212, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]

///|
fn yy_state_40(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_41)
    _ => Error
  }
}

// [114, trait_sig → TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [230, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [231, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]

///|
fn yy_state_41(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_42)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ =>
      Shift(yy_state_50)
    T_EOF | T_SEMI | T_LBRACE =>
      Reduce(
        0,
        NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___,
        yy_action_267,
      )
    _ => Error
  }
}

// [208, qualified_uident → • UIDENT, EOF / SEMI / PLUS / LBRACE]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, EOF / SEMI / PLUS / LBRACE]
// [228, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident, EOF / SEMI / LBRACE]
// [229, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
// [231, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → COLON • separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]

///|
fn yy_state_42(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_separated_nonempty_list_PLUS_qualified_uident_ => Shift(yy_state_43)
    NT_qualified_uident => Shift(yy_state_44)
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    _ => Error
  }
}

// [231, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → COLON separated_nonempty_list(PLUS,qualified_uident) •, EOF / SEMI / LBRACE]

///|
fn yy_state_43(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    2,
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___,
    yy_action_265,
  )
}

// [228, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident •, EOF / SEMI / LBRACE]
// [229, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident • PLUS separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]

///|
fn yy_state_44(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS => Shift(yy_state_45)
    T_EOF | T_SEMI | T_LBRACE =>
      Reduce(
        1,
        NT_separated_nonempty_list_PLUS_qualified_uident_,
        yy_action_264,
      )
    _ => Error
  }
}

// [208, qualified_uident → • UIDENT, EOF / SEMI / PLUS / LBRACE]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, EOF / SEMI / PLUS / LBRACE]
// [228, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident, EOF / SEMI / LBRACE]
// [229, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
// [229, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident PLUS • separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]

///|
fn yy_state_45(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_qualified_uident => Shift(yy_state_44)
    NT_separated_nonempty_list_PLUS_qualified_uident_ => Shift(yy_state_46)
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    _ => Error
  }
}

// [229, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident) •, EOF / SEMI / LBRACE]

///|
fn yy_state_46(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_separated_nonempty_list_PLUS_qualified_uident_,
    yy_action_263,
  )
}

// [209, qualified_uident → PACKAGE_NAME • DOT_UIDENT, EOF / RPAREN / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]

///|
fn yy_state_47(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOT_UIDENT => Shift(yy_state_48)
    _ => Error
  }
}

// [209, qualified_uident → PACKAGE_NAME DOT_UIDENT •, EOF / RPAREN / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]

///|
fn yy_state_48(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_qualified_uident, yy_action_262)
}

// [208, qualified_uident → UIDENT •, EOF / RPAREN / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]

///|
fn yy_state_49(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_qualified_uident, yy_action_261)
}

// [114, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [120, trait_body → •, EOF / SEMI]
// [121, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]

///|
fn yy_state_50(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_51)
    NT_trait_body => Shift(yy_state_160)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_260)
    _ => Error
  }
}

// [48, trait_method_sig → • ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [49, trait_method_sig → • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [50, trait_method_sig → • nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [51, trait_method_sig → • nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [121, trait_body → LBRACE • separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [213, lident → • LIDENT, LPAREN]
// [232, separated_list(SEMI,trait_method_sig) → •, RBRACE]
// [233, separated_list(SEMI,trait_method_sig) → • separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / ASYNC]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / ASYNC]
// [264, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [265, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]

///|
fn yy_state_51(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_LIDENT => Shift(yy_state_38)
    NT_trait_method_sig => Shift(yy_state_52)
    NT_nonempty_list_attribute_ => Shift(yy_state_55)
    NT_lident => Shift(yy_state_142)
    T_ASYNC => Shift(yy_state_149)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_157)
    NT_separated_list_SEMI_trait_method_sig_ => Shift(yy_state_158)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_trait_method_sig_, yy_action_259)
    _ => Error
  }
}

// [264, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig •, RBRACE]
// [265, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig • SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]

///|
fn yy_state_52(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_53)
    T_RBRACE =>
      Reduce(
        1,
        NT_separated_nonempty_list_SEMI_trait_method_sig_,
        yy_action_258,
      )
    _ => Error
  }
}

// [48, trait_method_sig → • ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [49, trait_method_sig → • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [50, trait_method_sig → • nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [51, trait_method_sig → • nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [213, lident → • LIDENT, LPAREN]
// [252, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / ASYNC]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / ASYNC]
// [264, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [265, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
// [265, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig SEMI • separated_nonempty_list(SEMI,trait_method_sig), RBRACE]

///|
fn yy_state_53(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_LIDENT => Shift(yy_state_38)
    NT_trait_method_sig => Shift(yy_state_52)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_54)
    NT_nonempty_list_attribute_ => Shift(yy_state_55)
    NT_lident => Shift(yy_state_142)
    T_ASYNC => Shift(yy_state_149)
    _ => Error
  }
}

// [265, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig) •, RBRACE]

///|
fn yy_state_54(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_separated_nonempty_list_SEMI_trait_method_sig_,
    yy_action_257,
  )
}

// [50, trait_method_sig → nonempty_list(attribute) • ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [51, trait_method_sig → nonempty_list(attribute) • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_55(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_56)
    T_ASYNC => Shift(yy_state_134)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_56(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_57)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_57(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_127)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [258, separated_nonempty_list(COMMA,parameter) → parameter •, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → parameter • COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_58(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_59)
    T_RPAREN =>
      Reduce(1, NT_separated_nonempty_list_COMMA_parameter_, yy_action_255)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → parameter COMMA • separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_59(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_60)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    _ => Error
  }
}

// [259, separated_nonempty_list(COMMA,parameter) → parameter COMMA separated_nonempty_list(COMMA,parameter) •, RPAREN]

///|
fn yy_state_60(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_separated_nonempty_list_COMMA_parameter_,
    yy_action_254,
  )
}

// [214, label → LIDENT •, QUESTION]

///|
fn yy_state_61(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_label, yy_action_253)
}

// [211, qualified_uident_ → PACKAGE_NAME • DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_62(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOT_UIDENT => Shift(yy_state_63)
    _ => Error
  }
}

// [211, qualified_uident_ → PACKAGE_NAME DOT_UIDENT •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_63(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_qualified_uident_, yy_action_252)
}

// [210, qualified_uident_ → UIDENT •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_64(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_qualified_uident_, yy_action_251)
}

// [192, parameter → label • QUESTION COLON type_, RPAREN / COMMA]

///|
fn yy_state_65(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_66)
    _ => Error
  }
}

// [192, parameter → label QUESTION • COLON type_, RPAREN / COMMA]

///|
fn yy_state_66(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_67)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [192, parameter → label QUESTION COLON • type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]

///|
fn yy_state_67(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_type_ => Shift(yy_state_68)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    _ => Error
  }
}

// [192, parameter → label QUESTION COLON type_ •, RPAREN / COMMA]

///|
fn yy_state_68(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_parameter, yy_action_250)
}

// [174, type_ → ASYNC • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → ASYNC • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → ASYNC • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → ASYNC • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_69(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_70)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [174, type_ → ASYNC LPAREN • type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → ASYNC LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → ASYNC LPAREN • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → ASYNC LPAREN • type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]

///|
fn yy_state_70(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_RPAREN => Shift(yy_state_71)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_110)
    _ => Error
  }
}

// [178, type_ → ASYNC LPAREN RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_71(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_72)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → ASYNC LPAREN RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_72(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    NT_return_type => Shift(yy_state_74)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    _ => Error
  }
}

// [182, return_type → type_ •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_73(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_return_type, yy_action_249)
}

// [178, type_ → ASYNC LPAREN RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_74(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_, yy_action_248)
}

// [172, simple_type → UNDERSCORE •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_75(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_simple_type, yy_action_247)
}

// [171, simple_type → AMPER • qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_76(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_qualified_uident_ => Shift(yy_state_77)
    _ => Error
  }
}

// [171, simple_type → AMPER qualified_uident_ •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_77(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_simple_type, yy_action_246)
}

// [170, simple_type → qualified_uident_ • optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [188, optional_type_arguments → • LBRACKET separated_nonempty_list(COMMA,type_) RBRACKET, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [189, optional_type_arguments → •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_78(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_79)
    NT_optional_type_arguments => Shift(yy_state_109)
    T_EOF
    | T_RAISE
    | T_EQUAL
    | T_RPAREN
    | T_COMMA
    | T_QUESTION
    | T_SEMI
    | T_RBRACKET
    | T_RBRACE
    | T_NORAISE => Reduce(0, NT_optional_type_arguments, yy_action_245)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, COMMA / QUESTION / RBRACKET]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, COMMA / QUESTION / RBRACKET]
// [169, simple_type → • LPAREN type_ RPAREN, COMMA / QUESTION / RBRACKET]
// [170, simple_type → • qualified_uident_ optional_type_arguments, COMMA / QUESTION / RBRACKET]
// [171, simple_type → • AMPER qualified_uident_, COMMA / QUESTION / RBRACKET]
// [172, simple_type → • UNDERSCORE, COMMA / QUESTION / RBRACKET]
// [173, type_ → • simple_type, COMMA / RBRACKET]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [188, optional_type_arguments → LBRACKET • separated_nonempty_list(COMMA,type_) RBRACKET, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [210, qualified_uident_ → • UIDENT, COMMA / QUESTION / LBRACKET / RBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, COMMA / QUESTION / LBRACKET / RBRACKET]
// [250, separated_nonempty_list(COMMA,type_) → • type_, RBRACKET]
// [251, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RBRACKET]

///|
fn yy_state_79(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    NT_type_ => Shift(yy_state_80)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_107)
    _ => Error
  }
}

// [250, separated_nonempty_list(COMMA,type_) → type_ •, RPAREN / RBRACKET]
// [251, separated_nonempty_list(COMMA,type_) → type_ • COMMA separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]

///|
fn yy_state_80(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_81)
    T_RPAREN | T_RBRACKET =>
      Reduce(1, NT_separated_nonempty_list_COMMA_type__, yy_action_244)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION / RBRACKET]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION / RBRACKET]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION / RBRACKET]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION / RBRACKET]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION / RBRACKET]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION / RBRACKET]
// [173, type_ → • simple_type, RPAREN / COMMA / RBRACKET]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET / RBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET / RBRACKET]
// [250, separated_nonempty_list(COMMA,type_) → • type_, RPAREN / RBRACKET]
// [251, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]
// [251, separated_nonempty_list(COMMA,type_) → type_ COMMA • separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]

///|
fn yy_state_81(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    NT_type_ => Shift(yy_state_80)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_82)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    _ => Error
  }
}

// [251, separated_nonempty_list(COMMA,type_) → type_ COMMA separated_nonempty_list(COMMA,type_) •, RPAREN / RBRACKET]

///|
fn yy_state_82(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_type__, yy_action_243)
}

// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [168, simple_type → LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → LPAREN • type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → LPAREN • type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → LPAREN • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → LPAREN • type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]

///|
fn yy_state_83(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    T_RPAREN => Shift(yy_state_84)
    NT_type_ => Shift(yy_state_94)
    NT_simple_type => Shift(yy_state_106)
    _ => Error
  }
}

// [179, type_ → LPAREN RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_84(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_85)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → LPAREN RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_85(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_return_type => Shift(yy_state_86)
    NT_simple_type => Shift(yy_state_87)
    _ => Error
  }
}

// [179, type_ → LPAREN RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_86(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_, yy_action_242)
}

// [167, simple_type → simple_type • QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, type_ → simple_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, return_type → simple_type • NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, return_type → simple_type • RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, return_type → simple_type • RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → simple_type • RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_87(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RAISE => Shift(yy_state_88)
    T_NORAISE => Shift(yy_state_92)
    T_QUESTION => Shift(yy_state_93)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE =>
      Reduce(1, NT_type_, yy_action_241)
    _ => Error
  }
}

// [184, return_type → simple_type RAISE •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, return_type → simple_type RAISE • error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → simple_type RAISE • QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [187, error_type → • qualified_uident_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [210, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_88(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_qualified_uident_ => Shift(yy_state_89)
    T_QUESTION => Shift(yy_state_90)
    NT_error_type => Shift(yy_state_91)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE =>
      Reduce(2, NT_return_type, yy_action_240)
    _ => Error
  }
}

// [187, error_type → qualified_uident_ •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_89(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_error_type, yy_action_239)
}

// [186, return_type → simple_type RAISE QUESTION •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_90(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_return_type, yy_action_238)
}

// [185, return_type → simple_type RAISE error_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_91(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_return_type, yy_action_237)
}

// [183, return_type → simple_type NORAISE •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_92(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_return_type, yy_action_236)
}

// [167, simple_type → simple_type QUESTION •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_93(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_simple_type, yy_action_235)
}

// [168, simple_type → LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → LPAREN type_ • RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [176, type_ → LPAREN type_ • COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → LPAREN type_ • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_94(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_95)
    T_COMMA => Shift(yy_state_98)
    _ => Error
  }
}

// [169, simple_type → LPAREN type_ RPAREN •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [181, type_ → LPAREN type_ RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_95(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_96)
    T_EOF
    | T_RAISE
    | T_EQUAL
    | T_RPAREN
    | T_COMMA
    | T_QUESTION
    | T_SEMI
    | T_RBRACKET
    | T_RBRACE
    | T_NORAISE => Reduce(3, NT_simple_type, yy_action_234)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → LPAREN type_ RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_96(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_97)
    _ => Error
  }
}

// [181, type_ → LPAREN type_ RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_97(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_, yy_action_233)
}

// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [168, simple_type → LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → LPAREN type_ COMMA • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [250, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [251, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]

///|
fn yy_state_98(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    NT_type_ => Shift(yy_state_80)
    T_LPAREN => Shift(yy_state_83)
    T_RPAREN => Shift(yy_state_99)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_102)
    NT_simple_type => Shift(yy_state_106)
    _ => Error
  }
}

// [176, type_ → LPAREN type_ COMMA RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_99(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_100)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → LPAREN type_ COMMA RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_100(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_101)
    _ => Error
  }
}

// [176, type_ → LPAREN type_ COMMA RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_101(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_, yy_action_232)
}

// [168, simple_type → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_102(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_103)
    _ => Error
  }
}

// [168, simple_type → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_103(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_104)
    T_EOF
    | T_RAISE
    | T_EQUAL
    | T_RPAREN
    | T_COMMA
    | T_QUESTION
    | T_SEMI
    | T_RBRACKET
    | T_RBRACE
    | T_NORAISE => Reduce(5, NT_simple_type, yy_action_231)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_104(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_105)
    _ => Error
  }
}

// [177, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_105(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_, yy_action_230)
}

// [167, simple_type → simple_type • QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE]
// [173, type_ → simple_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_106(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_93)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE =>
      Reduce(1, NT_type_, yy_action_241)
    _ => Error
  }
}

// [188, optional_type_arguments → LBRACKET separated_nonempty_list(COMMA,type_) • RBRACKET, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_107(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_108)
    _ => Error
  }
}

// [188, optional_type_arguments → LBRACKET separated_nonempty_list(COMMA,type_) RBRACKET •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_108(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_optional_type_arguments, yy_action_228)
}

// [170, simple_type → qualified_uident_ optional_type_arguments •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_109(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_simple_type, yy_action_227)
}

// [174, type_ → ASYNC LPAREN type_ • COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → ASYNC LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → ASYNC LPAREN type_ • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_110(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_111)
    T_COMMA => Shift(yy_state_114)
    _ => Error
  }
}

// [180, type_ → ASYNC LPAREN type_ RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_111(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_112)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → ASYNC LPAREN type_ RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_112(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_113)
    _ => Error
  }
}

// [180, type_ → ASYNC LPAREN type_ RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_113(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_, yy_action_226)
}

// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [174, type_ → ASYNC LPAREN type_ COMMA • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → ASYNC LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [250, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [251, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]

///|
fn yy_state_114(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    NT_type_ => Shift(yy_state_80)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_115)
    T_RPAREN => Shift(yy_state_119)
    _ => Error
  }
}

// [175, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_115(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_116)
    _ => Error
  }
}

// [175, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_116(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_117)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_117(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_118)
    _ => Error
  }
}

// [175, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_118(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_, yy_action_224)
}

// [174, type_ → ASYNC LPAREN type_ COMMA RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_119(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_120)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [174, type_ → ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]

///|
fn yy_state_120(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_121)
    _ => Error
  }
}

// [174, type_ → ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]

///|
fn yy_state_121(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_, yy_action_222)
}

// [191, parameter → POST_LABEL • COLON type_, RPAREN / COMMA]

///|
fn yy_state_122(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_123)
    _ => Error
  }
}

// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [191, parameter → POST_LABEL COLON • type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]

///|
fn yy_state_123(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_124)
    _ => Error
  }
}

// [191, parameter → POST_LABEL COLON type_ •, RPAREN / COMMA]

///|
fn yy_state_124(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_parameter, yy_action_221)
}

// [190, parameter → type_ •, RPAREN / COMMA]

///|
fn yy_state_125(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_parameter, yy_action_220)
}

// [220, separated_list(COMMA,parameter) → separated_nonempty_list(COMMA,parameter) •, RPAREN]

///|
fn yy_state_126(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_parameter_, yy_action_219)
}

// [51, trait_method_sig → nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_127(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_128)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_128(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_129)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [167, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, return_type → • type_, EQUAL / SEMI / RBRACE]
// [183, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [184, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [210, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]

///|
fn yy_state_129(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_130)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [221, boption(eq_underscore) → •, SEMI / RBRACE]
// [222, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]

///|
fn yy_state_130(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_133)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_218)
    _ => Error
  }
}

// [222, boption(eq_underscore) → EQUAL • UNDERSCORE, SEMI / RBRACE]

///|
fn yy_state_131(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UNDERSCORE => Shift(yy_state_132)
    _ => Error
  }
}

// [222, boption(eq_underscore) → EQUAL UNDERSCORE •, SEMI / RBRACE]

///|
fn yy_state_132(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_boption_eq_underscore_, yy_action_216)
}

// [51, trait_method_sig → nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]

///|
fn yy_state_133(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_trait_method_sig, yy_action_215)
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_134(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_135)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_135(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_136)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_136(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_137)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_137(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_138)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_138(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_139)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [167, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, return_type → • type_, EQUAL / SEMI / RBRACE]
// [183, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [184, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [210, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]

///|
fn yy_state_139(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_140)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [221, boption(eq_underscore) → •, SEMI / RBRACE]
// [222, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]

///|
fn yy_state_140(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_141)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_218)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]

///|
fn yy_state_141(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_trait_method_sig, yy_action_214)
}

// [49, trait_method_sig → lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_142(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_143)
    _ => Error
  }
}

// [49, trait_method_sig → lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_143(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_144)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [49, trait_method_sig → lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_144(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_145)
    _ => Error
  }
}

// [49, trait_method_sig → lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_145(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_146)
    _ => Error
  }
}

// [49, trait_method_sig → lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [167, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, return_type → • type_, EQUAL / SEMI / RBRACE]
// [183, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [184, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [210, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]

///|
fn yy_state_146(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_147)
    _ => Error
  }
}

// [49, trait_method_sig → lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [221, boption(eq_underscore) → •, SEMI / RBRACE]
// [222, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]

///|
fn yy_state_147(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_148)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_218)
    _ => Error
  }
}

// [49, trait_method_sig → lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]

///|
fn yy_state_148(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_trait_method_sig, yy_action_213)
}

// [48, trait_method_sig → ASYNC • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_149(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_150)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_150(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_151)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_151(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_152)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_152(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_153)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]

///|
fn yy_state_153(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_154)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [167, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, return_type → • type_, EQUAL / SEMI / RBRACE]
// [183, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [184, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [185, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [210, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]

///|
fn yy_state_154(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_155)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [221, boption(eq_underscore) → •, SEMI / RBRACE]
// [222, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]

///|
fn yy_state_155(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_156)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_218)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]

///|
fn yy_state_156(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_trait_method_sig, yy_action_212)
}

// [233, separated_list(SEMI,trait_method_sig) → separated_nonempty_list(SEMI,trait_method_sig) •, RBRACE]

///|
fn yy_state_157(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_SEMI_trait_method_sig_, yy_action_211)
}

// [121, trait_body → LBRACE separated_list(SEMI,trait_method_sig) • RBRACE, EOF / SEMI]

///|
fn yy_state_158(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_159)
    _ => Error
  }
}

// [121, trait_body → LBRACE separated_list(SEMI,trait_method_sig) RBRACE •, EOF / SEMI]

///|
fn yy_state_159(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_trait_body, yy_action_210)
}

// [114, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]

///|
fn yy_state_160(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_trait_sig, yy_action_209)
}

// [102, impl_sig → IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [108, impl_sig → IMPL • qualified_uident FOR type_, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_161(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    T_LBRACKET => Shift(yy_state_162)
    NT_qualified_uident => Shift(yy_state_176)
    NT_type_params_with_constraints => Shift(yy_state_179)
    _ => Error
  }
}

// [155, type_param_with_constraints → • uident, COMMA / RBRACKET]
// [156, type_param_with_constraints → • uident COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [157, type_params_with_constraints → LBRACKET • separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / PACKAGE_NAME]
// [212, uident → • UIDENT, COMMA / COLON / RBRACKET]
// [246, separated_list(COMMA,type_param_with_constraints) → •, RBRACKET]
// [247, separated_list(COMMA,type_param_with_constraints) → • separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
// [268, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints, RBRACKET]
// [269, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]

///|
fn yy_state_162(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_type_param_with_constraints => Shift(yy_state_163)
    NT_uident => Shift(yy_state_166)
    NT_separated_nonempty_list_COMMA_type_param_with_constraints_ =>
      Shift(yy_state_173)
    NT_separated_list_COMMA_type_param_with_constraints_ => Shift(yy_state_174)
    T_RBRACKET =>
      Reduce(
        0,
        NT_separated_list_COMMA_type_param_with_constraints_,
        yy_action_208,
      )
    _ => Error
  }
}

// [268, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints •, RBRACKET]
// [269, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints • COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]

///|
fn yy_state_163(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_164)
    T_RBRACKET =>
      Reduce(
        1,
        NT_separated_nonempty_list_COMMA_type_param_with_constraints_,
        yy_action_207,
      )
    _ => Error
  }
}

// [155, type_param_with_constraints → • uident, COMMA / RBRACKET]
// [156, type_param_with_constraints → • uident COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [212, uident → • UIDENT, COMMA / COLON / RBRACKET]
// [268, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints, RBRACKET]
// [269, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
// [269, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints COMMA • separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]

///|
fn yy_state_164(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_type_param_with_constraints => Shift(yy_state_163)
    NT_separated_nonempty_list_COMMA_type_param_with_constraints_ =>
      Shift(yy_state_165)
    NT_uident => Shift(yy_state_166)
    _ => Error
  }
}

// [269, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints) •, RBRACKET]

///|
fn yy_state_165(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_separated_nonempty_list_COMMA_type_param_with_constraints_,
    yy_action_206,
  )
}

// [155, type_param_with_constraints → uident •, COMMA / RBRACKET]
// [156, type_param_with_constraints → uident • COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]

///|
fn yy_state_166(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_167)
    T_COMMA | T_RBRACKET =>
      Reduce(1, NT_type_param_with_constraints, yy_action_205)
    _ => Error
  }
}

// [156, type_param_with_constraints → uident COLON • separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [163, type_constraint → • qualified_uident, COMMA / PLUS / RBRACKET]
// [208, qualified_uident → • UIDENT, COMMA / PLUS / RBRACKET]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, COMMA / PLUS / RBRACKET]
// [244, separated_nonempty_list(PLUS,type_constraint) → • type_constraint, COMMA / RBRACKET]
// [245, separated_nonempty_list(PLUS,type_constraint) → • type_constraint PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]

///|
fn yy_state_167(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    NT_type_constraint => Shift(yy_state_168)
    NT_qualified_uident => Shift(yy_state_171)
    NT_separated_nonempty_list_PLUS_type_constraint_ => Shift(yy_state_172)
    _ => Error
  }
}

// [244, separated_nonempty_list(PLUS,type_constraint) → type_constraint •, COMMA / RBRACKET]
// [245, separated_nonempty_list(PLUS,type_constraint) → type_constraint • PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]

///|
fn yy_state_168(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS => Shift(yy_state_169)
    T_COMMA | T_RBRACKET =>
      Reduce(1, NT_separated_nonempty_list_PLUS_type_constraint_, yy_action_204)
    _ => Error
  }
}

// [163, type_constraint → • qualified_uident, COMMA / PLUS / RBRACKET]
// [208, qualified_uident → • UIDENT, COMMA / PLUS / RBRACKET]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, COMMA / PLUS / RBRACKET]
// [244, separated_nonempty_list(PLUS,type_constraint) → • type_constraint, COMMA / RBRACKET]
// [245, separated_nonempty_list(PLUS,type_constraint) → • type_constraint PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [245, separated_nonempty_list(PLUS,type_constraint) → type_constraint PLUS • separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]

///|
fn yy_state_169(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    NT_type_constraint => Shift(yy_state_168)
    NT_separated_nonempty_list_PLUS_type_constraint_ => Shift(yy_state_170)
    NT_qualified_uident => Shift(yy_state_171)
    _ => Error
  }
}

// [245, separated_nonempty_list(PLUS,type_constraint) → type_constraint PLUS separated_nonempty_list(PLUS,type_constraint) •, COMMA / RBRACKET]

///|
fn yy_state_170(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_separated_nonempty_list_PLUS_type_constraint_,
    yy_action_203,
  )
}

// [163, type_constraint → qualified_uident •, COMMA / PLUS / RBRACKET]

///|
fn yy_state_171(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_constraint, yy_action_202)
}

// [156, type_param_with_constraints → uident COLON separated_nonempty_list(PLUS,type_constraint) •, COMMA / RBRACKET]

///|
fn yy_state_172(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_param_with_constraints, yy_action_201)
}

// [247, separated_list(COMMA,type_param_with_constraints) → separated_nonempty_list(COMMA,type_param_with_constraints) •, RBRACKET]

///|
fn yy_state_173(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    1,
    NT_separated_list_COMMA_type_param_with_constraints_,
    yy_action_200,
  )
}

// [157, type_params_with_constraints → LBRACKET separated_list(COMMA,type_param_with_constraints) • RBRACKET, LIDENT / UIDENT / AMPER / PACKAGE_NAME]

///|
fn yy_state_174(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_175)
    _ => Error
  }
}

// [157, type_params_with_constraints → LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET •, LIDENT / UIDENT / AMPER / PACKAGE_NAME]

///|
fn yy_state_175(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_params_with_constraints, yy_action_199)
}

// [108, impl_sig → IMPL qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_176(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_177)
    _ => Error
  }
}

// [108, impl_sig → IMPL qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_177(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_178)
    _ => Error
  }
}

// [108, impl_sig → IMPL qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_178(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_impl_sig, yy_action_198)
}

// [102, impl_sig → IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_179(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    NT_qualified_uident => Shift(yy_state_180)
    _ => Error
  }
}

// [102, impl_sig → IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_180(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_181)
    _ => Error
  }
}

// [102, impl_sig → IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_181(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_182)
    _ => Error
  }
}

// [102, impl_sig → IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_182(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_impl_sig, yy_action_197)
}

// [88, type_sig → EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [94, type_sig → EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [165, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [166, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [212, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]

///|
fn yy_state_183(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_184)
    NT_uident => Shift(yy_state_197)
    NT_extensible_enum_extension_target => Shift(yy_state_199)
    NT_type_decl_name_with_params => Shift(yy_state_240)
    _ => Error
  }
}

// [166, extensible_enum_extension_target → PACKAGE_NAME • DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]

///|
fn yy_state_184(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOT_UIDENT => Shift(yy_state_185)
    _ => Error
  }
}

// [160, type_params_no_constraints → • LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET, PLUS_EQUAL]
// [161, optional_type_params_no_constraints → •, PLUS_EQUAL]
// [162, optional_type_params_no_constraints → • type_params_no_constraints, PLUS_EQUAL]
// [166, extensible_enum_extension_target → PACKAGE_NAME DOT_UIDENT • optional_type_params_no_constraints, PLUS_EQUAL]

///|
fn yy_state_185(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_optional_type_params_no_constraints => Shift(yy_state_186)
    NT_type_params_no_constraints => Shift(yy_state_187)
    T_LBRACKET => Shift(yy_state_188)
    T_PLUS_EQUAL =>
      Reduce(0, NT_optional_type_params_no_constraints, yy_action_196)
    _ => Error
  }
}

// [166, extensible_enum_extension_target → PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints •, PLUS_EQUAL]

///|
fn yy_state_186(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_extensible_enum_extension_target, yy_action_195)
}

// [162, optional_type_params_no_constraints → type_params_no_constraints •, PLUS_EQUAL / EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]

///|
fn yy_state_187(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_optional_type_params_no_constraints, yy_action_194)
}

// [158, type_param_no_constraints → • uident, COMMA / RBRACKET]
// [159, type_param_no_constraints → • UNDERSCORE, COMMA / RBRACKET]
// [160, type_params_no_constraints → LBRACKET • separated_list(COMMA,type_param_no_constraints) RBRACKET, PLUS_EQUAL / EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
// [212, uident → • UIDENT, COMMA / RBRACKET]
// [248, separated_list(COMMA,type_param_no_constraints) → •, RBRACKET]
// [249, separated_list(COMMA,type_param_no_constraints) → • separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
// [270, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints, RBRACKET]
// [271, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]

///|
fn yy_state_188(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_type_param_no_constraints => Shift(yy_state_189)
    T_UNDERSCORE => Shift(yy_state_192)
    NT_uident => Shift(yy_state_193)
    NT_separated_nonempty_list_COMMA_type_param_no_constraints_ =>
      Shift(yy_state_194)
    NT_separated_list_COMMA_type_param_no_constraints_ => Shift(yy_state_195)
    T_RBRACKET =>
      Reduce(
        0,
        NT_separated_list_COMMA_type_param_no_constraints_,
        yy_action_193,
      )
    _ => Error
  }
}

// [270, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints •, RBRACKET]
// [271, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints • COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]

///|
fn yy_state_189(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_190)
    T_RBRACKET =>
      Reduce(
        1,
        NT_separated_nonempty_list_COMMA_type_param_no_constraints_,
        yy_action_192,
      )
    _ => Error
  }
}

// [158, type_param_no_constraints → • uident, COMMA / RBRACKET]
// [159, type_param_no_constraints → • UNDERSCORE, COMMA / RBRACKET]
// [212, uident → • UIDENT, COMMA / RBRACKET]
// [270, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints, RBRACKET]
// [271, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
// [271, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints COMMA • separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]

///|
fn yy_state_190(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_type_param_no_constraints => Shift(yy_state_189)
    NT_separated_nonempty_list_COMMA_type_param_no_constraints_ =>
      Shift(yy_state_191)
    T_UNDERSCORE => Shift(yy_state_192)
    NT_uident => Shift(yy_state_193)
    _ => Error
  }
}

// [271, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints) •, RBRACKET]

///|
fn yy_state_191(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_separated_nonempty_list_COMMA_type_param_no_constraints_,
    yy_action_191,
  )
}

// [159, type_param_no_constraints → UNDERSCORE •, COMMA / RBRACKET]

///|
fn yy_state_192(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_param_no_constraints, yy_action_190)
}

// [158, type_param_no_constraints → uident •, COMMA / RBRACKET]

///|
fn yy_state_193(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_param_no_constraints, yy_action_189)
}

// [249, separated_list(COMMA,type_param_no_constraints) → separated_nonempty_list(COMMA,type_param_no_constraints) •, RBRACKET]

///|
fn yy_state_194(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    1,
    NT_separated_list_COMMA_type_param_no_constraints_,
    yy_action_188,
  )
}

// [160, type_params_no_constraints → LBRACKET separated_list(COMMA,type_param_no_constraints) • RBRACKET, PLUS_EQUAL / EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]

///|
fn yy_state_195(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_196)
    _ => Error
  }
}

// [160, type_params_no_constraints → LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET •, PLUS_EQUAL / EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]

///|
fn yy_state_196(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_params_no_constraints, yy_action_187)
}

// [160, type_params_no_constraints → • LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET, PLUS_EQUAL / LBRACE]
// [161, optional_type_params_no_constraints → •, PLUS_EQUAL / LBRACE]
// [162, optional_type_params_no_constraints → • type_params_no_constraints, PLUS_EQUAL / LBRACE]
// [164, type_decl_name_with_params → uident • optional_type_params_no_constraints, LBRACE]
// [165, extensible_enum_extension_target → uident • optional_type_params_no_constraints, PLUS_EQUAL]

///|
fn yy_state_197(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_no_constraints => Shift(yy_state_187)
    T_LBRACKET => Shift(yy_state_188)
    NT_optional_type_params_no_constraints => Shift(yy_state_198)
    T_PLUS_EQUAL | T_LBRACE =>
      Reduce(0, NT_optional_type_params_no_constraints, yy_action_196)
    _ => Error
  }
}

// [164, type_decl_name_with_params → uident optional_type_params_no_constraints •, LBRACE]
// [165, extensible_enum_extension_target → uident optional_type_params_no_constraints •, PLUS_EQUAL]

///|
fn yy_state_198(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Reduce(2, NT_type_decl_name_with_params, yy_action_185)
    T_PLUS_EQUAL =>
      Reduce(2, NT_extensible_enum_extension_target, yy_action_186)
    _ => Error
  }
}

// [94, type_sig → EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_199(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_200)
    _ => Error
  }
}

// [94, type_sig → EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_200(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_201)
    _ => Error
  }
}

// [94, type_sig → EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_201(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_228)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [260, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor •, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor • SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_202(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_203)
    T_RBRACE =>
      Reduce(
        1,
        NT_separated_nonempty_list_SEMI_enum_constructor_,
        yy_action_183,
      )
    _ => Error
  }
}

// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor SEMI • separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_203(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_204)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    _ => Error
  }
}

// [261, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor) •, RBRACE]

///|
fn yy_state_204(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_separated_nonempty_list_SEMI_enum_constructor_,
    yy_action_182,
  )
}

// [149, enum_constructor → nonempty_list(attribute) • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]

///|
fn yy_state_205(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_206)
    _ => Error
  }
}

// [149, enum_constructor → nonempty_list(attribute) UIDENT • option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [236, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → •, EQUAL / SEMI / RBRACE]
// [237, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → • LPAREN separated_nonempty_list(COMMA,constructor_param) RPAREN, EQUAL / SEMI / RBRACE]

///|
fn yy_state_206(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_207)
    NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__ =>
      Shift(yy_state_220)
    T_EQUAL | T_SEMI | T_RBRACE =>
      Reduce(
        0,
        NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__,
        yy_action_181,
      )
    _ => Error
  }
}

// [150, constructor_param → • boption(MUTABLE) type_, RPAREN / COMMA]
// [151, constructor_param → • boption(MUTABLE) post_label COLON type_, RPAREN / COMMA]
// [234, separated_nonempty_list(COMMA,constructor_param) → • constructor_param, RPAREN]
// [235, separated_nonempty_list(COMMA,constructor_param) → • constructor_param COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [237, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN • separated_nonempty_list(COMMA,constructor_param) RPAREN, EQUAL / SEMI / RBRACE]
// [240, boption(MUTABLE) → •, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
// [241, boption(MUTABLE) → • MUTABLE, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]

///|
fn yy_state_207(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_208)
    NT_separated_nonempty_list_COMMA_constructor_param_ => Shift(yy_state_209)
    NT_constructor_param => Shift(yy_state_211)
    NT_boption_MUTABLE_ => Shift(yy_state_214)
    T_UIDENT
    | T_POST_LABEL
    | T_ASYNC
    | T_LPAREN
    | T_UNDERSCORE
    | T_AMPER
    | T_PACKAGE_NAME => Reduce(0, NT_boption_MUTABLE_, yy_action_180)
    _ => Error
  }
}

// [241, boption(MUTABLE) → MUTABLE •, LIDENT / UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]

///|
fn yy_state_208(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_boption_MUTABLE_, yy_action_179)
}

// [237, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN separated_nonempty_list(COMMA,constructor_param) • RPAREN, EQUAL / SEMI / RBRACE]

///|
fn yy_state_209(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_210)
    _ => Error
  }
}

// [237, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN separated_nonempty_list(COMMA,constructor_param) RPAREN •, EQUAL / SEMI / RBRACE]

///|
fn yy_state_210(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__,
    yy_action_177,
  )
}

// [234, separated_nonempty_list(COMMA,constructor_param) → constructor_param •, RPAREN]
// [235, separated_nonempty_list(COMMA,constructor_param) → constructor_param • COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]

///|
fn yy_state_211(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_212)
    T_RPAREN =>
      Reduce(
        1,
        NT_separated_nonempty_list_COMMA_constructor_param_,
        yy_action_176,
      )
    _ => Error
  }
}

// [150, constructor_param → • boption(MUTABLE) type_, RPAREN / COMMA]
// [151, constructor_param → • boption(MUTABLE) post_label COLON type_, RPAREN / COMMA]
// [234, separated_nonempty_list(COMMA,constructor_param) → • constructor_param, RPAREN]
// [235, separated_nonempty_list(COMMA,constructor_param) → • constructor_param COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [235, separated_nonempty_list(COMMA,constructor_param) → constructor_param COMMA • separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [240, boption(MUTABLE) → •, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
// [241, boption(MUTABLE) → • MUTABLE, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]

///|
fn yy_state_212(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_208)
    NT_constructor_param => Shift(yy_state_211)
    NT_separated_nonempty_list_COMMA_constructor_param_ => Shift(yy_state_213)
    NT_boption_MUTABLE_ => Shift(yy_state_214)
    T_UIDENT
    | T_POST_LABEL
    | T_ASYNC
    | T_LPAREN
    | T_UNDERSCORE
    | T_AMPER
    | T_PACKAGE_NAME => Reduce(0, NT_boption_MUTABLE_, yy_action_180)
    _ => Error
  }
}

// [235, separated_nonempty_list(COMMA,constructor_param) → constructor_param COMMA separated_nonempty_list(COMMA,constructor_param) •, RPAREN]

///|
fn yy_state_213(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_separated_nonempty_list_COMMA_constructor_param_,
    yy_action_175,
  )
}

// [150, constructor_param → boption(MUTABLE) • type_, RPAREN / COMMA]
// [151, constructor_param → boption(MUTABLE) • post_label COLON type_, RPAREN / COMMA]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, post_label → • POST_LABEL, COLON]

///|
fn yy_state_214(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_215)
    NT_post_label => Shift(yy_state_216)
    NT_type_ => Shift(yy_state_219)
    _ => Error
  }
}

// [215, post_label → POST_LABEL •, COLON]

///|
fn yy_state_215(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_post_label, yy_action_174)
}

// [151, constructor_param → boption(MUTABLE) post_label • COLON type_, RPAREN / COMMA]

///|
fn yy_state_216(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_217)
    _ => Error
  }
}

// [151, constructor_param → boption(MUTABLE) post_label COLON • type_, RPAREN / COMMA]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]

///|
fn yy_state_217(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_218)
    _ => Error
  }
}

// [151, constructor_param → boption(MUTABLE) post_label COLON type_ •, RPAREN / COMMA]

///|
fn yy_state_218(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_constructor_param, yy_action_173)
}

// [150, constructor_param → boption(MUTABLE) type_ •, RPAREN / COMMA]

///|
fn yy_state_219(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_constructor_param, yy_action_172)
}

// [149, enum_constructor → nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) • option(eq_tag), SEMI / RBRACE]
// [238, option(eq_tag) → •, SEMI / RBRACE]
// [239, option(eq_tag) → • EQUAL INT, SEMI / RBRACE]

///|
fn yy_state_220(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_221)
    NT_option_eq_tag_ => Shift(yy_state_223)
    T_SEMI | T_RBRACE => Reduce(0, NT_option_eq_tag_, yy_action_171)
    _ => Error
  }
}

// [239, option(eq_tag) → EQUAL • INT, SEMI / RBRACE]

///|
fn yy_state_221(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_INT => Shift(yy_state_222)
    _ => Error
  }
}

// [239, option(eq_tag) → EQUAL INT •, SEMI / RBRACE]

///|
fn yy_state_222(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_option_eq_tag_, yy_action_169)
}

// [149, enum_constructor → nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag) •, SEMI / RBRACE]

///|
fn yy_state_223(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_enum_constructor, yy_action_168)
}

// [148, enum_constructor → UIDENT • option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [236, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → •, EQUAL / SEMI / RBRACE]
// [237, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → • LPAREN separated_nonempty_list(COMMA,constructor_param) RPAREN, EQUAL / SEMI / RBRACE]

///|
fn yy_state_224(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_207)
    NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__ =>
      Shift(yy_state_225)
    T_EQUAL | T_SEMI | T_RBRACE =>
      Reduce(
        0,
        NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__,
        yy_action_181,
      )
    _ => Error
  }
}

// [148, enum_constructor → UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) • option(eq_tag), SEMI / RBRACE]
// [238, option(eq_tag) → •, SEMI / RBRACE]
// [239, option(eq_tag) → • EQUAL INT, SEMI / RBRACE]

///|
fn yy_state_225(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_221)
    NT_option_eq_tag_ => Shift(yy_state_226)
    T_SEMI | T_RBRACE => Reduce(0, NT_option_eq_tag_, yy_action_171)
    _ => Error
  }
}

// [148, enum_constructor → UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag) •, SEMI / RBRACE]

///|
fn yy_state_226(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_enum_constructor, yy_action_167)
}

// [224, separated_list(SEMI,enum_constructor) → separated_nonempty_list(SEMI,enum_constructor) •, RBRACE]

///|
fn yy_state_227(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_SEMI_enum_constructor_, yy_action_166)
}

// [94, type_sig → EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_228(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_229)
    _ => Error
  }
}

// [94, type_sig → EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_229(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_239)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [101, optional_derive → DERIVE • LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_230(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_231)
    _ => Error
  }
}

// [101, optional_derive → DERIVE LPAREN • non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [208, qualified_uident → • UIDENT, RPAREN / COMMA]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA]
// [227, non_empty_list_commas(qualified_uident) → • non_empty_list_commas_rev(qualified_uident) option(COMMA), RPAREN]
// [262, non_empty_list_commas_rev(qualified_uident) → • qualified_uident, RPAREN / COMMA]
// [263, non_empty_list_commas_rev(qualified_uident) → • non_empty_list_commas_rev(qualified_uident) COMMA qualified_uident, RPAREN / COMMA]

///|
fn yy_state_231(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    NT_qualified_uident => Shift(yy_state_232)
    NT_non_empty_list_commas_rev_qualified_uident_ => Shift(yy_state_233)
    NT_non_empty_list_commas_qualified_uident_ => Shift(yy_state_237)
    _ => Error
  }
}

// [262, non_empty_list_commas_rev(qualified_uident) → qualified_uident •, RPAREN / COMMA]

///|
fn yy_state_232(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    1,
    NT_non_empty_list_commas_rev_qualified_uident_,
    yy_action_164,
  )
}

// [227, non_empty_list_commas(qualified_uident) → non_empty_list_commas_rev(qualified_uident) • option(COMMA), RPAREN]
// [256, option(COMMA) → •, RPAREN]
// [257, option(COMMA) → • COMMA, RPAREN]
// [263, non_empty_list_commas_rev(qualified_uident) → non_empty_list_commas_rev(qualified_uident) • COMMA qualified_uident, RPAREN / COMMA]

///|
fn yy_state_233(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_234)
    NT_option_COMMA_ => Shift(yy_state_236)
    T_RPAREN => Reduce(0, NT_option_COMMA_, yy_action_285)
    _ => Error
  }
}

// [208, qualified_uident → • UIDENT, RPAREN / COMMA]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA]
// [257, option(COMMA) → COMMA •, RPAREN]
// [263, non_empty_list_commas_rev(qualified_uident) → non_empty_list_commas_rev(qualified_uident) COMMA • qualified_uident, RPAREN / COMMA]

///|
fn yy_state_234(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    NT_qualified_uident => Shift(yy_state_235)
    T_RPAREN => Reduce(1, NT_option_COMMA_, yy_action_284)
    _ => Error
  }
}

// [263, non_empty_list_commas_rev(qualified_uident) → non_empty_list_commas_rev(qualified_uident) COMMA qualified_uident •, RPAREN / COMMA]

///|
fn yy_state_235(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_non_empty_list_commas_rev_qualified_uident_,
    yy_action_163,
  )
}

// [227, non_empty_list_commas(qualified_uident) → non_empty_list_commas_rev(qualified_uident) option(COMMA) •, RPAREN]

///|
fn yy_state_236(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    2,
    NT_non_empty_list_commas_qualified_uident_,
    yy_action_162,
  )
}

// [101, optional_derive → DERIVE LPAREN non_empty_list_commas(qualified_uident) • RPAREN, EOF / SEMI]

///|
fn yy_state_237(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_238)
    _ => Error
  }
}

// [101, optional_derive → DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN •, EOF / SEMI]

///|
fn yy_state_238(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_optional_derive, yy_action_161)
}

// [94, type_sig → EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_239(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_160)
}

// [88, type_sig → EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_240(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_241)
    _ => Error
  }
}

// [88, type_sig → EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_241(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_242)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [88, type_sig → EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_242(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_243)
    _ => Error
  }
}

// [88, type_sig → EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_243(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_244)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [88, type_sig → EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_244(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_159)
}

// [82, type_sig → ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [212, uident → • UIDENT, LBRACKET / LBRACE]

///|
fn yy_state_245(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_248)
    _ => Error
  }
}

// [160, type_params_no_constraints → • LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET, EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
// [161, optional_type_params_no_constraints → •, EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
// [162, optional_type_params_no_constraints → • type_params_no_constraints, EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
// [164, type_decl_name_with_params → uident • optional_type_params_no_constraints, EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]

///|
fn yy_state_246(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_no_constraints => Shift(yy_state_187)
    T_LBRACKET => Shift(yy_state_188)
    NT_optional_type_params_no_constraints => Shift(yy_state_247)
    T_EOF | T_DERIVE | T_EQUAL | T_LPAREN | T_SEMI | T_LBRACE =>
      Reduce(0, NT_optional_type_params_no_constraints, yy_action_196)
    _ => Error
  }
}

// [164, type_decl_name_with_params → uident optional_type_params_no_constraints •, EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]

///|
fn yy_state_247(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_type_decl_name_with_params, yy_action_185)
}

// [82, type_sig → ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_248(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_249)
    _ => Error
  }
}

// [82, type_sig → ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_249(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_250)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [82, type_sig → ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_250(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_251)
    _ => Error
  }
}

// [82, type_sig → ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_251(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_252)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [82, type_sig → ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_252(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_158)
}

// [70, type_sig → STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [76, type_sig → STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [212, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]

///|
fn yy_state_253(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_254)
    _ => Error
  }
}

// [70, type_sig → STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [76, type_sig → STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_254(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_255)
    T_LBRACE => Shift(yy_state_260)
    _ => Error
  }
}

// [76, type_sig → STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [225, separated_list(COMMA,type_) → •, RPAREN]
// [226, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [250, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [251, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]

///|
fn yy_state_255(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    NT_type_ => Shift(yy_state_80)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_256)
    NT_separated_list_COMMA_type__ => Shift(yy_state_257)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_157)
    _ => Error
  }
}

// [226, separated_list(COMMA,type_) → separated_nonempty_list(COMMA,type_) •, RPAREN]

///|
fn yy_state_256(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_type__, yy_action_156)
}

// [76, type_sig → STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_257(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_258)
    _ => Error
  }
}

// [76, type_sig → STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_258(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_259)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [76, type_sig → STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]

///|
fn yy_state_259(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_155)
}

// [70, type_sig → STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [152, record_decl_field → • boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [153, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [154, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [240, boption(MUTABLE) → •, LIDENT]
// [241, boption(MUTABLE) → • MUTABLE, LIDENT]
// [242, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [243, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE]
// [266, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [267, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]

///|
fn yy_state_260(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_208)
    NT_record_decl_field => Shift(yy_state_261)
    NT_nonempty_list_attribute_ => Shift(yy_state_264)
    NT_boption_MUTABLE_ => Shift(yy_state_269)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_273)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_274)
    NT_record_decl_body => Shift(yy_state_275)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_180)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_154)
    _ => Error
  }
}

// [266, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field •, RBRACE]
// [267, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field • SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]

///|
fn yy_state_261(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_262)
    T_RBRACE =>
      Reduce(
        1,
        NT_separated_nonempty_list_SEMI_record_decl_field_,
        yy_action_153,
      )
    _ => Error
  }
}

// [152, record_decl_field → • boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [153, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [240, boption(MUTABLE) → •, LIDENT]
// [241, boption(MUTABLE) → • MUTABLE, LIDENT]
// [252, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE]
// [266, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [267, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [267, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field SEMI • separated_nonempty_list(SEMI,record_decl_field), RBRACE]

///|
fn yy_state_262(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_208)
    NT_record_decl_field => Shift(yy_state_261)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_263)
    NT_nonempty_list_attribute_ => Shift(yy_state_264)
    NT_boption_MUTABLE_ => Shift(yy_state_269)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_180)
    _ => Error
  }
}

// [267, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field) •, RBRACE]

///|
fn yy_state_263(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(
    3,
    NT_separated_nonempty_list_SEMI_record_decl_field_,
    yy_action_152,
  )
}

// [153, record_decl_field → nonempty_list(attribute) • boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [240, boption(MUTABLE) → •, LIDENT]
// [241, boption(MUTABLE) → • MUTABLE, LIDENT]

///|
fn yy_state_264(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_208)
    NT_boption_MUTABLE_ => Shift(yy_state_265)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_180)
    _ => Error
  }
}

// [153, record_decl_field → nonempty_list(attribute) boption(MUTABLE) • LIDENT COLON type_, SEMI / RBRACE]

///|
fn yy_state_265(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_266)
    _ => Error
  }
}

// [153, record_decl_field → nonempty_list(attribute) boption(MUTABLE) LIDENT • COLON type_, SEMI / RBRACE]

///|
fn yy_state_266(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_267)
    _ => Error
  }
}

// [153, record_decl_field → nonempty_list(attribute) boption(MUTABLE) LIDENT COLON • type_, SEMI / RBRACE]
// [167, simple_type → • simple_type QUESTION, QUESTION / SEMI / RBRACE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, QUESTION / SEMI / RBRACE]
// [169, simple_type → • LPAREN type_ RPAREN, QUESTION / SEMI / RBRACE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, QUESTION / SEMI / RBRACE]
// [171, simple_type → • AMPER qualified_uident_, QUESTION / SEMI / RBRACE]
// [172, simple_type → • UNDERSCORE, QUESTION / SEMI / RBRACE]
// [173, type_ → • simple_type, SEMI / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [210, qualified_uident_ → • UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]

///|
fn yy_state_267(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_268)
    _ => Error
  }
}

// [153, record_decl_field → nonempty_list(attribute) boption(MUTABLE) LIDENT COLON type_ •, SEMI / RBRACE]

///|
fn yy_state_268(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_record_decl_field, yy_action_151)
}

// [152, record_decl_field → boption(MUTABLE) • LIDENT COLON type_, SEMI / RBRACE]

///|
fn yy_state_269(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_270)
    _ => Error
  }
}

// [152, record_decl_field → boption(MUTABLE) LIDENT • COLON type_, SEMI / RBRACE]

///|
fn yy_state_270(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_271)
    _ => Error
  }
}

// [152, record_decl_field → boption(MUTABLE) LIDENT COLON • type_, SEMI / RBRACE]
// [167, simple_type → • simple_type QUESTION, QUESTION / SEMI / RBRACE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, QUESTION / SEMI / RBRACE]
// [169, simple_type → • LPAREN type_ RPAREN, QUESTION / SEMI / RBRACE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, QUESTION / SEMI / RBRACE]
// [171, simple_type → • AMPER qualified_uident_, QUESTION / SEMI / RBRACE]
// [172, simple_type → • UNDERSCORE, QUESTION / SEMI / RBRACE]
// [173, type_ → • simple_type, SEMI / RBRACE]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [210, qualified_uident_ → • UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]

///|
fn yy_state_271(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_272)
    _ => Error
  }
}

// [152, record_decl_field → boption(MUTABLE) LIDENT COLON type_ •, SEMI / RBRACE]

///|
fn yy_state_272(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_record_decl_field, yy_action_150)
}

// [243, separated_list(SEMI,record_decl_field) → separated_nonempty_list(SEMI,record_decl_field) •, RBRACE]

///|
fn yy_state_273(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_SEMI_record_decl_field_, yy_action_149)
}

// [154, record_decl_body → separated_list(SEMI,record_decl_field) •, RBRACE]

///|
fn yy_state_274(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_record_decl_body, yy_action_148)
}

// [70, type_sig → STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_275(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_276)
    _ => Error
  }
}

// [70, type_sig → STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_276(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_277)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [70, type_sig → STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_277(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_147)
}

// [58, type_sig → SUBERROR • uident optional_derive, EOF / SEMI]
// [64, type_sig → SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]

///|
fn yy_state_278(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_279)
    _ => Error
  }
}

// [58, type_sig → SUBERROR uident • optional_derive, EOF / SEMI]
// [64, type_sig → SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_279(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_LBRACE => Shift(yy_state_280)
    NT_optional_derive => Shift(yy_state_284)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [64, type_sig → SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_280(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_281)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [64, type_sig → SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_281(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_282)
    _ => Error
  }
}

// [64, type_sig → SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_282(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_283)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [64, type_sig → SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_283(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_146)
}

// [58, type_sig → SUBERROR uident optional_derive •, EOF / SEMI]

///|
fn yy_state_284(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_sig, yy_action_145)
}

// [52, type_sig → TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [124, alias_sig → TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]

///|
fn yy_state_285(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_286)
    _ => Error
  }
}

// [52, type_sig → TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [124, alias_sig → TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]

///|
fn yy_state_286(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_EQUAL => Shift(yy_state_287)
    NT_optional_derive => Shift(yy_state_289)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [124, alias_sig → TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_287(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_288)
    _ => Error
  }
}

// [124, alias_sig → TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]

///|
fn yy_state_288(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_alias_sig, yy_action_144)
}

// [52, type_sig → TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]

///|
fn yy_state_289(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_sig, yy_action_143)
}

// [37, func_sig → FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_290(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_292)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [218, loption(type_params_with_constraints) → type_params_with_constraints •, LIDENT / UIDENT / AMPER]

///|
fn yy_state_291(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_loption_type_params_with_constraints_, yy_action_141)
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [37, func_sig → FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_292(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_func_name => Shift(yy_state_293)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_293(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_294)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_294(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_295)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_295(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_296)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_296(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_297)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_297(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_298)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_298(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_func_sig, yy_action_140)
}

// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [33, func_name → method_self_type_coloncolon • luident, LPAREN]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_299(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_LIDENT => Shift(yy_state_38)
    NT_luident => Shift(yy_state_300)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    _ => Error
  }
}

// [33, func_name → method_self_type_coloncolon luident •, LPAREN]

///|
fn yy_state_300(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_func_name, yy_action_139)
}

// [31, luident → uident •, LPAREN]

///|
fn yy_state_301(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_luident, yy_action_138)
}

// [30, luident → lident •, LPAREN]

///|
fn yy_state_302(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_luident, yy_action_137)
}

// [32, func_name → luident •, LPAREN]

///|
fn yy_state_303(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_func_name, yy_action_136)
}

// [29, method_self_type_coloncolon → AMPER • UIDENT COLONCOLON, LIDENT / UIDENT]

///|
fn yy_state_304(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_305)
    _ => Error
  }
}

// [29, method_self_type_coloncolon → AMPER UIDENT • COLONCOLON, LIDENT / UIDENT]

///|
fn yy_state_305(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_306)
    _ => Error
  }
}

// [29, method_self_type_coloncolon → AMPER UIDENT COLONCOLON •, LIDENT / UIDENT]

///|
fn yy_state_306(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_method_self_type_coloncolon, yy_action_135)
}

// [28, method_self_type_coloncolon → UIDENT • COLONCOLON, LIDENT / UIDENT]
// [212, uident → UIDENT •, LPAREN]

///|
fn yy_state_307(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_308)
    T_LPAREN => Reduce(1, NT_uident, yy_action_274)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → UIDENT COLONCOLON •, LIDENT / UIDENT]

///|
fn yy_state_308(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_method_self_type_coloncolon, yy_action_134)
}

// [36, func_sig → ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_309(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_310)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_310(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_311)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [36, func_sig → ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_311(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_312)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_312(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_313)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_313(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_314)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_314(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_315)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_315(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_316)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_316(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_317)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_317(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_func_sig, yy_action_133)
}

// [22, value_sig → LET • lident COLON type_, EOF / SEMI]
// [213, lident → • LIDENT, COLON]

///|
fn yy_state_318(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_319)
    _ => Error
  }
}

// [22, value_sig → LET lident • COLON type_, EOF / SEMI]

///|
fn yy_state_319(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_320)
    _ => Error
  }
}

// [22, value_sig → LET lident COLON • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_320(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_321)
    _ => Error
  }
}

// [22, value_sig → LET lident COLON type_ •, EOF / SEMI]

///|
fn yy_state_321(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_value_sig, yy_action_132)
}

// [19, const_sig → nonempty_list(attribute) • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [20, const_sig → nonempty_list(attribute) • PRIV CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [21, const_sig → nonempty_list(attribute) • PUB pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [25, value_sig → nonempty_list(attribute) • LET lident COLON type_, EOF / SEMI]
// [26, value_sig → nonempty_list(attribute) • PRIV LET lident COLON type_, EOF / SEMI]
// [27, value_sig → nonempty_list(attribute) • PUB pub_attr LET lident COLON type_, EOF / SEMI]
// [42, func_sig → nonempty_list(attribute) • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [43, func_sig → nonempty_list(attribute) • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [44, func_sig → nonempty_list(attribute) • PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [45, func_sig → nonempty_list(attribute) • PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [46, func_sig → nonempty_list(attribute) • PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [47, func_sig → nonempty_list(attribute) • PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [55, type_sig → nonempty_list(attribute) • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [56, type_sig → nonempty_list(attribute) • PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [57, type_sig → nonempty_list(attribute) • PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [61, type_sig → nonempty_list(attribute) • SUBERROR uident optional_derive, EOF / SEMI]
// [62, type_sig → nonempty_list(attribute) • PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [63, type_sig → nonempty_list(attribute) • PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [67, type_sig → nonempty_list(attribute) • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [68, type_sig → nonempty_list(attribute) • PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [69, type_sig → nonempty_list(attribute) • PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [73, type_sig → nonempty_list(attribute) • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [74, type_sig → nonempty_list(attribute) • PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [75, type_sig → nonempty_list(attribute) • PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [79, type_sig → nonempty_list(attribute) • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [80, type_sig → nonempty_list(attribute) • PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [81, type_sig → nonempty_list(attribute) • PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [85, type_sig → nonempty_list(attribute) • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [86, type_sig → nonempty_list(attribute) • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [87, type_sig → nonempty_list(attribute) • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [91, type_sig → nonempty_list(attribute) • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [92, type_sig → nonempty_list(attribute) • PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [93, type_sig → nonempty_list(attribute) • PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → nonempty_list(attribute) • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [98, type_sig → nonempty_list(attribute) • PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → nonempty_list(attribute) • PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [105, impl_sig → nonempty_list(attribute) • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [106, impl_sig → nonempty_list(attribute) • PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [107, impl_sig → nonempty_list(attribute) • PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [111, impl_sig → nonempty_list(attribute) • IMPL qualified_uident FOR type_, EOF / SEMI]
// [112, impl_sig → nonempty_list(attribute) • PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → nonempty_list(attribute) • PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [117, trait_sig → nonempty_list(attribute) • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [118, trait_sig → nonempty_list(attribute) • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [119, trait_sig → nonempty_list(attribute) • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [127, alias_sig → nonempty_list(attribute) • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [128, alias_sig → nonempty_list(attribute) • PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [129, alias_sig → nonempty_list(attribute) • PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [133, alias_sig → nonempty_list(attribute) • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [134, alias_sig → nonempty_list(attribute) • PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [135, alias_sig → nonempty_list(attribute) • PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [139, alias_sig → nonempty_list(attribute) • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [140, alias_sig → nonempty_list(attribute) • PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [141, alias_sig → nonempty_list(attribute) • PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → nonempty_list(attribute) • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → nonempty_list(attribute) • PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → nonempty_list(attribute) • PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_322(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_USING => Shift(yy_state_323)
    T_FNALIAS => Shift(yy_state_334)
    T_TRAIT => Shift(yy_state_338)
    T_IMPL => Shift(yy_state_342)
    T_EXTENUM => Shift(yy_state_350)
    T_ENUM => Shift(yy_state_362)
    T_STRUCT => Shift(yy_state_368)
    T_SUBERROR => Shift(yy_state_378)
    T_TYPE => Shift(yy_state_385)
    T_FN => Shift(yy_state_390)
    T_ASYNC => Shift(yy_state_398)
    T_LET => Shift(yy_state_407)
    T_PUB => Shift(yy_state_411)
    T_PRIV => Shift(yy_state_525)
    T_CONST => Shift(yy_state_620)
    _ => Error
  }
}

// [139, alias_sig → nonempty_list(attribute) USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → nonempty_list(attribute) USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_323(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_324)
    _ => Error
  }
}

// [139, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_324(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_325)
    _ => Error
  }
}

// [139, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_325(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_326)
    T_TYPE => Shift(yy_state_330)
    _ => Error
  }
}

// [145, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_326(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_327)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [145, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_327(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_328)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [145, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_328(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_329)
    _ => Error
  }
}

// [145, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_329(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_alias_sig, yy_action_131)
}

// [139, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_330(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_331)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [139, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_331(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_332)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [139, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_332(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_333)
    _ => Error
  }
}

// [139, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_333(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_alias_sig, yy_action_130)
}

// [133, alias_sig → nonempty_list(attribute) FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [212, uident → • UIDENT, COLONCOLON]

///|
fn yy_state_334(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_335)
    _ => Error
  }
}

// [133, alias_sig → nonempty_list(attribute) FNALIAS uident • COLONCOLON lident, EOF / SEMI]

///|
fn yy_state_335(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_336)
    _ => Error
  }
}

// [133, alias_sig → nonempty_list(attribute) FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [213, lident → • LIDENT, EOF / SEMI]

///|
fn yy_state_336(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_337)
    _ => Error
  }
}

// [133, alias_sig → nonempty_list(attribute) FNALIAS uident COLONCOLON lident •, EOF / SEMI]

///|
fn yy_state_337(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_129)
}

// [117, trait_sig → nonempty_list(attribute) TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [212, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]

///|
fn yy_state_338(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_339)
    _ => Error
  }
}

// [117, trait_sig → nonempty_list(attribute) TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [230, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [231, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]

///|
fn yy_state_339(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_42)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ =>
      Shift(yy_state_340)
    T_EOF | T_SEMI | T_LBRACE =>
      Reduce(
        0,
        NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___,
        yy_action_267,
      )
    _ => Error
  }
}

// [117, trait_sig → nonempty_list(attribute) TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [120, trait_body → •, EOF / SEMI]
// [121, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]

///|
fn yy_state_340(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_51)
    NT_trait_body => Shift(yy_state_341)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_260)
    _ => Error
  }
}

// [117, trait_sig → nonempty_list(attribute) TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]

///|
fn yy_state_341(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_trait_sig, yy_action_128)
}

// [105, impl_sig → nonempty_list(attribute) IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [111, impl_sig → nonempty_list(attribute) IMPL • qualified_uident FOR type_, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_342(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    T_LBRACKET => Shift(yy_state_162)
    NT_qualified_uident => Shift(yy_state_343)
    NT_type_params_with_constraints => Shift(yy_state_346)
    _ => Error
  }
}

// [111, impl_sig → nonempty_list(attribute) IMPL qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_343(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_344)
    _ => Error
  }
}

// [111, impl_sig → nonempty_list(attribute) IMPL qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_344(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_345)
    _ => Error
  }
}

// [111, impl_sig → nonempty_list(attribute) IMPL qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_345(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_impl_sig, yy_action_127)
}

// [105, impl_sig → nonempty_list(attribute) IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_346(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    NT_qualified_uident => Shift(yy_state_347)
    _ => Error
  }
}

// [105, impl_sig → nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_347(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_348)
    _ => Error
  }
}

// [105, impl_sig → nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_348(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_349)
    _ => Error
  }
}

// [105, impl_sig → nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_349(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_impl_sig, yy_action_126)
}

// [91, type_sig → nonempty_list(attribute) EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → nonempty_list(attribute) EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [165, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [166, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [212, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]

///|
fn yy_state_350(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_184)
    NT_uident => Shift(yy_state_197)
    NT_extensible_enum_extension_target => Shift(yy_state_351)
    NT_type_decl_name_with_params => Shift(yy_state_357)
    _ => Error
  }
}

// [97, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_351(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_352)
    _ => Error
  }
}

// [97, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_352(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_353)
    _ => Error
  }
}

// [97, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_353(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_354)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [97, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_354(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_355)
    _ => Error
  }
}

// [97, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_355(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_356)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [97, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_356(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_125)
}

// [91, type_sig → nonempty_list(attribute) EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_357(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_358)
    _ => Error
  }
}

// [91, type_sig → nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_358(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_359)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [91, type_sig → nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_359(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_360)
    _ => Error
  }
}

// [91, type_sig → nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_360(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_361)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [91, type_sig → nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_361(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_124)
}

// [85, type_sig → nonempty_list(attribute) ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [212, uident → • UIDENT, LBRACKET / LBRACE]

///|
fn yy_state_362(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_363)
    _ => Error
  }
}

// [85, type_sig → nonempty_list(attribute) ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_363(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_364)
    _ => Error
  }
}

// [85, type_sig → nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_364(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_365)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [85, type_sig → nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_365(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_366)
    _ => Error
  }
}

// [85, type_sig → nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_366(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_367)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [85, type_sig → nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_367(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_123)
}

// [73, type_sig → nonempty_list(attribute) STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [79, type_sig → nonempty_list(attribute) STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [212, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]

///|
fn yy_state_368(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_369)
    _ => Error
  }
}

// [73, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [79, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_369(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_370)
    T_LBRACE => Shift(yy_state_374)
    _ => Error
  }
}

// [79, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [225, separated_list(COMMA,type_) → •, RPAREN]
// [226, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [250, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [251, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]

///|
fn yy_state_370(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    NT_type_ => Shift(yy_state_80)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_256)
    NT_separated_list_COMMA_type__ => Shift(yy_state_371)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_157)
    _ => Error
  }
}

// [79, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_371(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_372)
    _ => Error
  }
}

// [79, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_372(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_373)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [79, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]

///|
fn yy_state_373(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_122)
}

// [73, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [152, record_decl_field → • boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [153, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [154, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [240, boption(MUTABLE) → •, LIDENT]
// [241, boption(MUTABLE) → • MUTABLE, LIDENT]
// [242, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [243, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE]
// [266, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [267, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]

///|
fn yy_state_374(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_208)
    NT_record_decl_field => Shift(yy_state_261)
    NT_nonempty_list_attribute_ => Shift(yy_state_264)
    NT_boption_MUTABLE_ => Shift(yy_state_269)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_273)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_274)
    NT_record_decl_body => Shift(yy_state_375)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_180)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_154)
    _ => Error
  }
}

// [73, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_375(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_376)
    _ => Error
  }
}

// [73, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_376(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_377)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [73, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_377(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_121)
}

// [61, type_sig → nonempty_list(attribute) SUBERROR • uident optional_derive, EOF / SEMI]
// [67, type_sig → nonempty_list(attribute) SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]

///|
fn yy_state_378(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_379)
    _ => Error
  }
}

// [61, type_sig → nonempty_list(attribute) SUBERROR uident • optional_derive, EOF / SEMI]
// [67, type_sig → nonempty_list(attribute) SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_379(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_LBRACE => Shift(yy_state_380)
    NT_optional_derive => Shift(yy_state_384)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [67, type_sig → nonempty_list(attribute) SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_380(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_381)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [67, type_sig → nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_381(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_382)
    _ => Error
  }
}

// [67, type_sig → nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_382(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_383)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [67, type_sig → nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_383(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_120)
}

// [61, type_sig → nonempty_list(attribute) SUBERROR uident optional_derive •, EOF / SEMI]

///|
fn yy_state_384(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_119)
}

// [55, type_sig → nonempty_list(attribute) TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [127, alias_sig → nonempty_list(attribute) TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]

///|
fn yy_state_385(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_386)
    _ => Error
  }
}

// [55, type_sig → nonempty_list(attribute) TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [127, alias_sig → nonempty_list(attribute) TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]

///|
fn yy_state_386(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_EQUAL => Shift(yy_state_387)
    NT_optional_derive => Shift(yy_state_389)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [127, alias_sig → nonempty_list(attribute) TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_387(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_388)
    _ => Error
  }
}

// [127, alias_sig → nonempty_list(attribute) TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]

///|
fn yy_state_388(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_118)
}

// [55, type_sig → nonempty_list(attribute) TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]

///|
fn yy_state_389(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_117)
}

// [43, func_sig → nonempty_list(attribute) FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_390(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_391)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_391(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_392)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_392(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_393)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_393(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_394)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_394(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_395)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_395(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_396)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_396(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_397)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_397(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_func_sig, yy_action_116)
}

// [42, func_sig → nonempty_list(attribute) ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_398(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_399)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_399(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_400)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_400(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_401)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_401(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_402)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_402(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_403)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_403(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_404)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_404(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_405)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_405(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_406)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_406(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_func_sig, yy_action_115)
}

// [25, value_sig → nonempty_list(attribute) LET • lident COLON type_, EOF / SEMI]
// [213, lident → • LIDENT, COLON]

///|
fn yy_state_407(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_408)
    _ => Error
  }
}

// [25, value_sig → nonempty_list(attribute) LET lident • COLON type_, EOF / SEMI]

///|
fn yy_state_408(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_409)
    _ => Error
  }
}

// [25, value_sig → nonempty_list(attribute) LET lident COLON • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_409(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_410)
    _ => Error
  }
}

// [25, value_sig → nonempty_list(attribute) LET lident COLON type_ •, EOF / SEMI]

///|
fn yy_state_410(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_value_sig, yy_action_114)
}

// [21, const_sig → nonempty_list(attribute) PUB • pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [27, value_sig → nonempty_list(attribute) PUB • pub_attr LET lident COLON type_, EOF / SEMI]
// [46, func_sig → nonempty_list(attribute) PUB • pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [47, func_sig → nonempty_list(attribute) PUB • pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [57, type_sig → nonempty_list(attribute) PUB • pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [63, type_sig → nonempty_list(attribute) PUB • pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [69, type_sig → nonempty_list(attribute) PUB • pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [75, type_sig → nonempty_list(attribute) PUB • pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [81, type_sig → nonempty_list(attribute) PUB • pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [87, type_sig → nonempty_list(attribute) PUB • pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [93, type_sig → nonempty_list(attribute) PUB • pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → nonempty_list(attribute) PUB • pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [107, impl_sig → nonempty_list(attribute) PUB • pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → nonempty_list(attribute) PUB • pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [119, trait_sig → nonempty_list(attribute) PUB • pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [129, alias_sig → nonempty_list(attribute) PUB • pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [135, alias_sig → nonempty_list(attribute) PUB • pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [141, alias_sig → nonempty_list(attribute) PUB • pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → nonempty_list(attribute) PUB • pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [205, pub_attr → •, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [206, pub_attr → • LPAREN READONLY RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [207, pub_attr → • LPAREN LIDENT RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_411(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_412)
    NT_pub_attr => Shift(yy_state_417)
    T_STRUCT
    | T_ENUM
    | T_TRAIT
    | T_IMPL
    | T_ASYNC
    | T_FNALIAS
    | T_FN
    | T_LET
    | T_CONST
    | T_TYPE
    | T_SUBERROR
    | T_USING
    | T_EXTENUM => Reduce(0, NT_pub_attr, yy_action_113)
    _ => Error
  }
}

// [206, pub_attr → LPAREN • READONLY RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [207, pub_attr → LPAREN • LIDENT RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_412(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_413)
    T_READONLY => Shift(yy_state_415)
    _ => Error
  }
}

// [207, pub_attr → LPAREN LIDENT • RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_413(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_414)
    _ => Error
  }
}

// [207, pub_attr → LPAREN LIDENT RPAREN •, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_414(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_pub_attr, yy_action_112)
}

// [206, pub_attr → LPAREN READONLY • RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_415(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_416)
    _ => Error
  }
}

// [206, pub_attr → LPAREN READONLY RPAREN •, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_416(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_pub_attr, yy_action_111)
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [27, value_sig → nonempty_list(attribute) PUB pub_attr • LET lident COLON type_, EOF / SEMI]
// [46, func_sig → nonempty_list(attribute) PUB pub_attr • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [47, func_sig → nonempty_list(attribute) PUB pub_attr • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [57, type_sig → nonempty_list(attribute) PUB pub_attr • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [63, type_sig → nonempty_list(attribute) PUB pub_attr • SUBERROR uident optional_derive, EOF / SEMI]
// [69, type_sig → nonempty_list(attribute) PUB pub_attr • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [75, type_sig → nonempty_list(attribute) PUB pub_attr • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [81, type_sig → nonempty_list(attribute) PUB pub_attr • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [87, type_sig → nonempty_list(attribute) PUB pub_attr • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [93, type_sig → nonempty_list(attribute) PUB pub_attr • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → nonempty_list(attribute) PUB pub_attr • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [107, impl_sig → nonempty_list(attribute) PUB pub_attr • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → nonempty_list(attribute) PUB pub_attr • IMPL qualified_uident FOR type_, EOF / SEMI]
// [119, trait_sig → nonempty_list(attribute) PUB pub_attr • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [129, alias_sig → nonempty_list(attribute) PUB pub_attr • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [135, alias_sig → nonempty_list(attribute) PUB pub_attr • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [141, alias_sig → nonempty_list(attribute) PUB pub_attr • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → nonempty_list(attribute) PUB pub_attr • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_417(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_USING => Shift(yy_state_418)
    T_FNALIAS => Shift(yy_state_429)
    T_TRAIT => Shift(yy_state_433)
    T_IMPL => Shift(yy_state_437)
    T_EXTENUM => Shift(yy_state_445)
    T_ENUM => Shift(yy_state_457)
    T_STRUCT => Shift(yy_state_463)
    T_SUBERROR => Shift(yy_state_473)
    T_TYPE => Shift(yy_state_480)
    T_FN => Shift(yy_state_485)
    T_ASYNC => Shift(yy_state_493)
    T_LET => Shift(yy_state_502)
    T_CONST => Shift(yy_state_506)
    _ => Error
  }
}

// [141, alias_sig → nonempty_list(attribute) PUB pub_attr USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → nonempty_list(attribute) PUB pub_attr USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_418(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_419)
    _ => Error
  }
}

// [141, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_419(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_420)
    _ => Error
  }
}

// [141, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_420(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_421)
    T_TYPE => Shift(yy_state_425)
    _ => Error
  }
}

// [147, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_421(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_422)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [147, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_422(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_423)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [147, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_423(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_424)
    _ => Error
  }
}

// [147, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_424(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_alias_sig, yy_action_110)
}

// [141, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_425(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_426)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [141, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_426(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_427)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [141, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_427(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_428)
    _ => Error
  }
}

// [141, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_428(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_alias_sig, yy_action_109)
}

// [135, alias_sig → nonempty_list(attribute) PUB pub_attr FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [212, uident → • UIDENT, COLONCOLON]

///|
fn yy_state_429(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_430)
    _ => Error
  }
}

// [135, alias_sig → nonempty_list(attribute) PUB pub_attr FNALIAS uident • COLONCOLON lident, EOF / SEMI]

///|
fn yy_state_430(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_431)
    _ => Error
  }
}

// [135, alias_sig → nonempty_list(attribute) PUB pub_attr FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [213, lident → • LIDENT, EOF / SEMI]

///|
fn yy_state_431(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_432)
    _ => Error
  }
}

// [135, alias_sig → nonempty_list(attribute) PUB pub_attr FNALIAS uident COLONCOLON lident •, EOF / SEMI]

///|
fn yy_state_432(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_alias_sig, yy_action_108)
}

// [119, trait_sig → nonempty_list(attribute) PUB pub_attr TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [212, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]

///|
fn yy_state_433(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_434)
    _ => Error
  }
}

// [119, trait_sig → nonempty_list(attribute) PUB pub_attr TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [230, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [231, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]

///|
fn yy_state_434(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_42)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ =>
      Shift(yy_state_435)
    T_EOF | T_SEMI | T_LBRACE =>
      Reduce(
        0,
        NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___,
        yy_action_267,
      )
    _ => Error
  }
}

// [119, trait_sig → nonempty_list(attribute) PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [120, trait_body → •, EOF / SEMI]
// [121, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]

///|
fn yy_state_435(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_51)
    NT_trait_body => Shift(yy_state_436)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_260)
    _ => Error
  }
}

// [119, trait_sig → nonempty_list(attribute) PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]

///|
fn yy_state_436(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_trait_sig, yy_action_107)
}

// [107, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL • qualified_uident FOR type_, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_437(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    T_LBRACKET => Shift(yy_state_162)
    NT_qualified_uident => Shift(yy_state_438)
    NT_type_params_with_constraints => Shift(yy_state_441)
    _ => Error
  }
}

// [113, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_438(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_439)
    _ => Error
  }
}

// [113, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_439(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_440)
    _ => Error
  }
}

// [113, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_440(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_impl_sig, yy_action_106)
}

// [107, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_441(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    NT_qualified_uident => Shift(yy_state_442)
    _ => Error
  }
}

// [107, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_442(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_443)
    _ => Error
  }
}

// [107, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_443(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_444)
    _ => Error
  }
}

// [107, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_444(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_impl_sig, yy_action_105)
}

// [93, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [165, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [166, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [212, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]

///|
fn yy_state_445(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_184)
    NT_uident => Shift(yy_state_197)
    NT_extensible_enum_extension_target => Shift(yy_state_446)
    NT_type_decl_name_with_params => Shift(yy_state_452)
    _ => Error
  }
}

// [99, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_446(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_447)
    _ => Error
  }
}

// [99, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_447(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_448)
    _ => Error
  }
}

// [99, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_448(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_449)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [99, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_449(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_450)
    _ => Error
  }
}

// [99, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_450(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_451)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [99, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_451(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_type_sig, yy_action_104)
}

// [93, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_452(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_453)
    _ => Error
  }
}

// [93, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_453(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_454)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [93, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_454(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_455)
    _ => Error
  }
}

// [93, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_455(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_456)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [93, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_456(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_103)
}

// [87, type_sig → nonempty_list(attribute) PUB pub_attr ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [212, uident → • UIDENT, LBRACKET / LBRACE]

///|
fn yy_state_457(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_458)
    _ => Error
  }
}

// [87, type_sig → nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_458(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_459)
    _ => Error
  }
}

// [87, type_sig → nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_459(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_460)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [87, type_sig → nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_460(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_461)
    _ => Error
  }
}

// [87, type_sig → nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_461(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_462)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [87, type_sig → nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_462(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_102)
}

// [75, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [81, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [212, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]

///|
fn yy_state_463(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_464)
    _ => Error
  }
}

// [75, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [81, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_464(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_465)
    T_LBRACE => Shift(yy_state_469)
    _ => Error
  }
}

// [81, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [225, separated_list(COMMA,type_) → •, RPAREN]
// [226, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [250, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [251, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]

///|
fn yy_state_465(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    NT_type_ => Shift(yy_state_80)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_256)
    NT_separated_list_COMMA_type__ => Shift(yy_state_466)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_157)
    _ => Error
  }
}

// [81, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_466(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_467)
    _ => Error
  }
}

// [81, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_467(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_468)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [81, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]

///|
fn yy_state_468(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_101)
}

// [75, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [152, record_decl_field → • boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [153, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [154, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [240, boption(MUTABLE) → •, LIDENT]
// [241, boption(MUTABLE) → • MUTABLE, LIDENT]
// [242, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [243, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE]
// [266, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [267, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]

///|
fn yy_state_469(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_208)
    NT_record_decl_field => Shift(yy_state_261)
    NT_nonempty_list_attribute_ => Shift(yy_state_264)
    NT_boption_MUTABLE_ => Shift(yy_state_269)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_273)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_274)
    NT_record_decl_body => Shift(yy_state_470)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_180)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_154)
    _ => Error
  }
}

// [75, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_470(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_471)
    _ => Error
  }
}

// [75, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_471(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_472)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [75, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_472(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_100)
}

// [63, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR • uident optional_derive, EOF / SEMI]
// [69, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]

///|
fn yy_state_473(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_474)
    _ => Error
  }
}

// [63, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident • optional_derive, EOF / SEMI]
// [69, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_474(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_LBRACE => Shift(yy_state_475)
    NT_optional_derive => Shift(yy_state_479)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [69, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_475(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_476)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [69, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_476(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_477)
    _ => Error
  }
}

// [69, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_477(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_478)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [69, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_478(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_99)
}

// [63, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident optional_derive •, EOF / SEMI]

///|
fn yy_state_479(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_98)
}

// [57, type_sig → nonempty_list(attribute) PUB pub_attr TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [129, alias_sig → nonempty_list(attribute) PUB pub_attr TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]

///|
fn yy_state_480(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_481)
    _ => Error
  }
}

// [57, type_sig → nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [129, alias_sig → nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]

///|
fn yy_state_481(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_EQUAL => Shift(yy_state_482)
    NT_optional_derive => Shift(yy_state_484)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [129, alias_sig → nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_482(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_483)
    _ => Error
  }
}

// [129, alias_sig → nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]

///|
fn yy_state_483(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_alias_sig, yy_action_97)
}

// [57, type_sig → nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]

///|
fn yy_state_484(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_96)
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_485(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_486)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_486(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_487)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_487(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_488)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_488(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_489)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_489(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_490)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_490(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_491)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_491(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_492)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_492(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(11, NT_func_sig, yy_action_95)
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_493(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_494)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_494(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_495)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_495(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_496)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_496(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_497)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_497(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_498)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_498(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_499)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_499(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_500)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_500(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_501)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_501(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(12, NT_func_sig, yy_action_94)
}

// [27, value_sig → nonempty_list(attribute) PUB pub_attr LET • lident COLON type_, EOF / SEMI]
// [213, lident → • LIDENT, COLON]

///|
fn yy_state_502(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_503)
    _ => Error
  }
}

// [27, value_sig → nonempty_list(attribute) PUB pub_attr LET lident • COLON type_, EOF / SEMI]

///|
fn yy_state_503(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_504)
    _ => Error
  }
}

// [27, value_sig → nonempty_list(attribute) PUB pub_attr LET lident COLON • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_504(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_505)
    _ => Error
  }
}

// [27, value_sig → nonempty_list(attribute) PUB pub_attr LET lident COLON type_ •, EOF / SEMI]

///|
fn yy_state_505(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_value_sig, yy_action_93)
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [212, uident → • UIDENT, COLON]

///|
fn yy_state_506(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_507)
    _ => Error
  }
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST uident • COLON type_ EQUAL constant, EOF / SEMI]

///|
fn yy_state_507(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_508)
    _ => Error
  }
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [172, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [173, type_ → • simple_type, EQUAL]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [210, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]

///|
fn yy_state_508(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_509)
    _ => Error
  }
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST uident COLON type_ • EQUAL constant, EOF / SEMI]

///|
fn yy_state_509(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_510)
    _ => Error
  }
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [193, constant → • TRUE, EOF / SEMI]
// [194, constant → • FALSE, EOF / SEMI]
// [195, constant → • BYTE, EOF / SEMI]
// [196, constant → • BYTES, EOF / SEMI]
// [197, constant → • CHAR, EOF / SEMI]
// [198, constant → • INT, EOF / SEMI]
// [199, constant → • MINUS INT, EOF / SEMI]
// [200, constant → • FLOAT, EOF / SEMI]
// [201, constant → • MINUS FLOAT, EOF / SEMI]
// [202, constant → • DOUBLE, EOF / SEMI]
// [203, constant → • MINUS DOUBLE, EOF / SEMI]
// [204, constant → • STRING, EOF / SEMI]

///|
fn yy_state_510(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_511)
    T_DOUBLE => Shift(yy_state_512)
    T_FLOAT => Shift(yy_state_513)
    T_MINUS => Shift(yy_state_514)
    T_INT => Shift(yy_state_518)
    T_CHAR => Shift(yy_state_519)
    T_BYTES => Shift(yy_state_520)
    T_BYTE => Shift(yy_state_521)
    T_FALSE => Shift(yy_state_522)
    T_TRUE => Shift(yy_state_523)
    NT_constant => Shift(yy_state_524)
    _ => Error
  }
}

// [204, constant → STRING •, EOF / SEMI]

///|
fn yy_state_511(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_92)
}

// [202, constant → DOUBLE •, EOF / SEMI]

///|
fn yy_state_512(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_91)
}

// [200, constant → FLOAT •, EOF / SEMI]

///|
fn yy_state_513(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_90)
}

// [199, constant → MINUS • INT, EOF / SEMI]
// [201, constant → MINUS • FLOAT, EOF / SEMI]
// [203, constant → MINUS • DOUBLE, EOF / SEMI]

///|
fn yy_state_514(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOUBLE => Shift(yy_state_515)
    T_FLOAT => Shift(yy_state_516)
    T_INT => Shift(yy_state_517)
    _ => Error
  }
}

// [203, constant → MINUS DOUBLE •, EOF / SEMI]

///|
fn yy_state_515(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_constant, yy_action_89)
}

// [201, constant → MINUS FLOAT •, EOF / SEMI]

///|
fn yy_state_516(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_constant, yy_action_88)
}

// [199, constant → MINUS INT •, EOF / SEMI]

///|
fn yy_state_517(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_constant, yy_action_87)
}

// [198, constant → INT •, EOF / SEMI]

///|
fn yy_state_518(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_86)
}

// [197, constant → CHAR •, EOF / SEMI]

///|
fn yy_state_519(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_85)
}

// [196, constant → BYTES •, EOF / SEMI]

///|
fn yy_state_520(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_84)
}

// [195, constant → BYTE •, EOF / SEMI]

///|
fn yy_state_521(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_83)
}

// [194, constant → FALSE •, EOF / SEMI]

///|
fn yy_state_522(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_82)
}

// [193, constant → TRUE •, EOF / SEMI]

///|
fn yy_state_523(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_81)
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST uident COLON type_ EQUAL constant •, EOF / SEMI]

///|
fn yy_state_524(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_const_sig, yy_action_80)
}

// [20, const_sig → nonempty_list(attribute) PRIV • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [26, value_sig → nonempty_list(attribute) PRIV • LET lident COLON type_, EOF / SEMI]
// [44, func_sig → nonempty_list(attribute) PRIV • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [45, func_sig → nonempty_list(attribute) PRIV • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [56, type_sig → nonempty_list(attribute) PRIV • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [62, type_sig → nonempty_list(attribute) PRIV • SUBERROR uident optional_derive, EOF / SEMI]
// [68, type_sig → nonempty_list(attribute) PRIV • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [74, type_sig → nonempty_list(attribute) PRIV • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [80, type_sig → nonempty_list(attribute) PRIV • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [86, type_sig → nonempty_list(attribute) PRIV • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [92, type_sig → nonempty_list(attribute) PRIV • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [98, type_sig → nonempty_list(attribute) PRIV • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [106, impl_sig → nonempty_list(attribute) PRIV • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [112, impl_sig → nonempty_list(attribute) PRIV • IMPL qualified_uident FOR type_, EOF / SEMI]
// [118, trait_sig → nonempty_list(attribute) PRIV • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [128, alias_sig → nonempty_list(attribute) PRIV • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [134, alias_sig → nonempty_list(attribute) PRIV • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [140, alias_sig → nonempty_list(attribute) PRIV • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → nonempty_list(attribute) PRIV • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_525(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_USING => Shift(yy_state_526)
    T_FNALIAS => Shift(yy_state_537)
    T_TRAIT => Shift(yy_state_541)
    T_IMPL => Shift(yy_state_545)
    T_EXTENUM => Shift(yy_state_553)
    T_ENUM => Shift(yy_state_565)
    T_STRUCT => Shift(yy_state_571)
    T_SUBERROR => Shift(yy_state_581)
    T_TYPE => Shift(yy_state_588)
    T_FN => Shift(yy_state_593)
    T_ASYNC => Shift(yy_state_601)
    T_LET => Shift(yy_state_610)
    T_CONST => Shift(yy_state_614)
    _ => Error
  }
}

// [140, alias_sig → nonempty_list(attribute) PRIV USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → nonempty_list(attribute) PRIV USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_526(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_527)
    _ => Error
  }
}

// [140, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_527(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_528)
    _ => Error
  }
}

// [140, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_528(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_529)
    T_TYPE => Shift(yy_state_533)
    _ => Error
  }
}

// [146, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_529(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_530)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [146, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_530(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_531)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [146, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_531(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_532)
    _ => Error
  }
}

// [146, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_532(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_alias_sig, yy_action_79)
}

// [140, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_533(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_534)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [140, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_534(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_535)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [140, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_535(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_536)
    _ => Error
  }
}

// [140, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_536(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_alias_sig, yy_action_78)
}

// [134, alias_sig → nonempty_list(attribute) PRIV FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [212, uident → • UIDENT, COLONCOLON]

///|
fn yy_state_537(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_538)
    _ => Error
  }
}

// [134, alias_sig → nonempty_list(attribute) PRIV FNALIAS uident • COLONCOLON lident, EOF / SEMI]

///|
fn yy_state_538(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_539)
    _ => Error
  }
}

// [134, alias_sig → nonempty_list(attribute) PRIV FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [213, lident → • LIDENT, EOF / SEMI]

///|
fn yy_state_539(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_540)
    _ => Error
  }
}

// [134, alias_sig → nonempty_list(attribute) PRIV FNALIAS uident COLONCOLON lident •, EOF / SEMI]

///|
fn yy_state_540(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_77)
}

// [118, trait_sig → nonempty_list(attribute) PRIV TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [212, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]

///|
fn yy_state_541(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_542)
    _ => Error
  }
}

// [118, trait_sig → nonempty_list(attribute) PRIV TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [230, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [231, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]

///|
fn yy_state_542(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_42)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ =>
      Shift(yy_state_543)
    T_EOF | T_SEMI | T_LBRACE =>
      Reduce(
        0,
        NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___,
        yy_action_267,
      )
    _ => Error
  }
}

// [118, trait_sig → nonempty_list(attribute) PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [120, trait_body → •, EOF / SEMI]
// [121, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]

///|
fn yy_state_543(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_51)
    NT_trait_body => Shift(yy_state_544)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_260)
    _ => Error
  }
}

// [118, trait_sig → nonempty_list(attribute) PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]

///|
fn yy_state_544(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_trait_sig, yy_action_76)
}

// [106, impl_sig → nonempty_list(attribute) PRIV IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [112, impl_sig → nonempty_list(attribute) PRIV IMPL • qualified_uident FOR type_, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_545(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    T_LBRACKET => Shift(yy_state_162)
    NT_qualified_uident => Shift(yy_state_546)
    NT_type_params_with_constraints => Shift(yy_state_549)
    _ => Error
  }
}

// [112, impl_sig → nonempty_list(attribute) PRIV IMPL qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_546(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_547)
    _ => Error
  }
}

// [112, impl_sig → nonempty_list(attribute) PRIV IMPL qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_547(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_548)
    _ => Error
  }
}

// [112, impl_sig → nonempty_list(attribute) PRIV IMPL qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_548(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_impl_sig, yy_action_75)
}

// [106, impl_sig → nonempty_list(attribute) PRIV IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_549(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    NT_qualified_uident => Shift(yy_state_550)
    _ => Error
  }
}

// [106, impl_sig → nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_550(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_551)
    _ => Error
  }
}

// [106, impl_sig → nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_551(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_552)
    _ => Error
  }
}

// [106, impl_sig → nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_552(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_impl_sig, yy_action_74)
}

// [92, type_sig → nonempty_list(attribute) PRIV EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [98, type_sig → nonempty_list(attribute) PRIV EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [165, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [166, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [212, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]

///|
fn yy_state_553(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_184)
    NT_uident => Shift(yy_state_197)
    NT_extensible_enum_extension_target => Shift(yy_state_554)
    NT_type_decl_name_with_params => Shift(yy_state_560)
    _ => Error
  }
}

// [98, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_554(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_555)
    _ => Error
  }
}

// [98, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_555(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_556)
    _ => Error
  }
}

// [98, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_556(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_557)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [98, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_557(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_558)
    _ => Error
  }
}

// [98, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_558(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_559)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [98, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_559(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_73)
}

// [92, type_sig → nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_560(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_561)
    _ => Error
  }
}

// [92, type_sig → nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_561(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_562)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [92, type_sig → nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_562(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_563)
    _ => Error
  }
}

// [92, type_sig → nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_563(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_564)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [92, type_sig → nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_564(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_72)
}

// [86, type_sig → nonempty_list(attribute) PRIV ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [212, uident → • UIDENT, LBRACKET / LBRACE]

///|
fn yy_state_565(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_566)
    _ => Error
  }
}

// [86, type_sig → nonempty_list(attribute) PRIV ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_566(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_567)
    _ => Error
  }
}

// [86, type_sig → nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_567(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_568)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [86, type_sig → nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_568(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_569)
    _ => Error
  }
}

// [86, type_sig → nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_569(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_570)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [86, type_sig → nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_570(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_71)
}

// [74, type_sig → nonempty_list(attribute) PRIV STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [80, type_sig → nonempty_list(attribute) PRIV STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [212, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]

///|
fn yy_state_571(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_572)
    _ => Error
  }
}

// [74, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [80, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_572(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_573)
    T_LBRACE => Shift(yy_state_577)
    _ => Error
  }
}

// [80, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [225, separated_list(COMMA,type_) → •, RPAREN]
// [226, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [250, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [251, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]

///|
fn yy_state_573(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    NT_type_ => Shift(yy_state_80)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_256)
    NT_separated_list_COMMA_type__ => Shift(yy_state_574)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_157)
    _ => Error
  }
}

// [80, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_574(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_575)
    _ => Error
  }
}

// [80, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_575(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_576)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [80, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]

///|
fn yy_state_576(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_70)
}

// [74, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [152, record_decl_field → • boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [153, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [154, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [240, boption(MUTABLE) → •, LIDENT]
// [241, boption(MUTABLE) → • MUTABLE, LIDENT]
// [242, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [243, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE]
// [266, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [267, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]

///|
fn yy_state_577(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_208)
    NT_record_decl_field => Shift(yy_state_261)
    NT_nonempty_list_attribute_ => Shift(yy_state_264)
    NT_boption_MUTABLE_ => Shift(yy_state_269)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_273)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_274)
    NT_record_decl_body => Shift(yy_state_578)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_180)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_154)
    _ => Error
  }
}

// [74, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_578(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_579)
    _ => Error
  }
}

// [74, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_579(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_580)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [74, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_580(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_69)
}

// [62, type_sig → nonempty_list(attribute) PRIV SUBERROR • uident optional_derive, EOF / SEMI]
// [68, type_sig → nonempty_list(attribute) PRIV SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]

///|
fn yy_state_581(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_582)
    _ => Error
  }
}

// [62, type_sig → nonempty_list(attribute) PRIV SUBERROR uident • optional_derive, EOF / SEMI]
// [68, type_sig → nonempty_list(attribute) PRIV SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_582(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_LBRACE => Shift(yy_state_583)
    NT_optional_derive => Shift(yy_state_587)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [68, type_sig → nonempty_list(attribute) PRIV SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_583(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_584)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [68, type_sig → nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_584(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_585)
    _ => Error
  }
}

// [68, type_sig → nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_585(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_586)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [68, type_sig → nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_586(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_68)
}

// [62, type_sig → nonempty_list(attribute) PRIV SUBERROR uident optional_derive •, EOF / SEMI]

///|
fn yy_state_587(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_67)
}

// [56, type_sig → nonempty_list(attribute) PRIV TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [128, alias_sig → nonempty_list(attribute) PRIV TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]

///|
fn yy_state_588(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_589)
    _ => Error
  }
}

// [56, type_sig → nonempty_list(attribute) PRIV TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [128, alias_sig → nonempty_list(attribute) PRIV TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]

///|
fn yy_state_589(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_EQUAL => Shift(yy_state_590)
    NT_optional_derive => Shift(yy_state_592)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [128, alias_sig → nonempty_list(attribute) PRIV TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_590(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_591)
    _ => Error
  }
}

// [128, alias_sig → nonempty_list(attribute) PRIV TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]

///|
fn yy_state_591(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_66)
}

// [56, type_sig → nonempty_list(attribute) PRIV TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]

///|
fn yy_state_592(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_65)
}

// [45, func_sig → nonempty_list(attribute) PRIV FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_593(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_594)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_594(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_595)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_595(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_596)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_596(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_597)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_597(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_598)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_598(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_599)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_599(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_600)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_600(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_func_sig, yy_action_64)
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_601(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_602)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_602(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_603)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_603(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_604)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_604(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_605)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_605(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_606)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_606(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_607)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_607(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_608)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_608(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_609)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_609(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(11, NT_func_sig, yy_action_63)
}

// [26, value_sig → nonempty_list(attribute) PRIV LET • lident COLON type_, EOF / SEMI]
// [213, lident → • LIDENT, COLON]

///|
fn yy_state_610(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_611)
    _ => Error
  }
}

// [26, value_sig → nonempty_list(attribute) PRIV LET lident • COLON type_, EOF / SEMI]

///|
fn yy_state_611(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_612)
    _ => Error
  }
}

// [26, value_sig → nonempty_list(attribute) PRIV LET lident COLON • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_612(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_613)
    _ => Error
  }
}

// [26, value_sig → nonempty_list(attribute) PRIV LET lident COLON type_ •, EOF / SEMI]

///|
fn yy_state_613(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_value_sig, yy_action_62)
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [212, uident → • UIDENT, COLON]

///|
fn yy_state_614(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_615)
    _ => Error
  }
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST uident • COLON type_ EQUAL constant, EOF / SEMI]

///|
fn yy_state_615(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_616)
    _ => Error
  }
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [172, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [173, type_ → • simple_type, EQUAL]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [210, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]

///|
fn yy_state_616(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_617)
    _ => Error
  }
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST uident COLON type_ • EQUAL constant, EOF / SEMI]

///|
fn yy_state_617(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_618)
    _ => Error
  }
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [193, constant → • TRUE, EOF / SEMI]
// [194, constant → • FALSE, EOF / SEMI]
// [195, constant → • BYTE, EOF / SEMI]
// [196, constant → • BYTES, EOF / SEMI]
// [197, constant → • CHAR, EOF / SEMI]
// [198, constant → • INT, EOF / SEMI]
// [199, constant → • MINUS INT, EOF / SEMI]
// [200, constant → • FLOAT, EOF / SEMI]
// [201, constant → • MINUS FLOAT, EOF / SEMI]
// [202, constant → • DOUBLE, EOF / SEMI]
// [203, constant → • MINUS DOUBLE, EOF / SEMI]
// [204, constant → • STRING, EOF / SEMI]

///|
fn yy_state_618(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_511)
    T_DOUBLE => Shift(yy_state_512)
    T_FLOAT => Shift(yy_state_513)
    T_MINUS => Shift(yy_state_514)
    T_INT => Shift(yy_state_518)
    T_CHAR => Shift(yy_state_519)
    T_BYTES => Shift(yy_state_520)
    T_BYTE => Shift(yy_state_521)
    T_FALSE => Shift(yy_state_522)
    T_TRUE => Shift(yy_state_523)
    NT_constant => Shift(yy_state_619)
    _ => Error
  }
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST uident COLON type_ EQUAL constant •, EOF / SEMI]

///|
fn yy_state_619(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_const_sig, yy_action_61)
}

// [19, const_sig → nonempty_list(attribute) CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [212, uident → • UIDENT, COLON]

///|
fn yy_state_620(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_621)
    _ => Error
  }
}

// [19, const_sig → nonempty_list(attribute) CONST uident • COLON type_ EQUAL constant, EOF / SEMI]

///|
fn yy_state_621(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_622)
    _ => Error
  }
}

// [19, const_sig → nonempty_list(attribute) CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [172, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [173, type_ → • simple_type, EQUAL]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [210, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]

///|
fn yy_state_622(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_623)
    _ => Error
  }
}

// [19, const_sig → nonempty_list(attribute) CONST uident COLON type_ • EQUAL constant, EOF / SEMI]

///|
fn yy_state_623(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_624)
    _ => Error
  }
}

// [19, const_sig → nonempty_list(attribute) CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [193, constant → • TRUE, EOF / SEMI]
// [194, constant → • FALSE, EOF / SEMI]
// [195, constant → • BYTE, EOF / SEMI]
// [196, constant → • BYTES, EOF / SEMI]
// [197, constant → • CHAR, EOF / SEMI]
// [198, constant → • INT, EOF / SEMI]
// [199, constant → • MINUS INT, EOF / SEMI]
// [200, constant → • FLOAT, EOF / SEMI]
// [201, constant → • MINUS FLOAT, EOF / SEMI]
// [202, constant → • DOUBLE, EOF / SEMI]
// [203, constant → • MINUS DOUBLE, EOF / SEMI]
// [204, constant → • STRING, EOF / SEMI]

///|
fn yy_state_624(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_511)
    T_DOUBLE => Shift(yy_state_512)
    T_FLOAT => Shift(yy_state_513)
    T_MINUS => Shift(yy_state_514)
    T_INT => Shift(yy_state_518)
    T_CHAR => Shift(yy_state_519)
    T_BYTES => Shift(yy_state_520)
    T_BYTE => Shift(yy_state_521)
    T_FALSE => Shift(yy_state_522)
    T_TRUE => Shift(yy_state_523)
    NT_constant => Shift(yy_state_625)
    _ => Error
  }
}

// [19, const_sig → nonempty_list(attribute) CONST uident COLON type_ EQUAL constant •, EOF / SEMI]

///|
fn yy_state_625(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_const_sig, yy_action_59)
}

// [18, const_sig → PUB • pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [24, value_sig → PUB • pub_attr LET lident COLON type_, EOF / SEMI]
// [40, func_sig → PUB • pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [41, func_sig → PUB • pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [54, type_sig → PUB • pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [60, type_sig → PUB • pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [66, type_sig → PUB • pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [72, type_sig → PUB • pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [78, type_sig → PUB • pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [84, type_sig → PUB • pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [90, type_sig → PUB • pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → PUB • pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [104, impl_sig → PUB • pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [110, impl_sig → PUB • pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [116, trait_sig → PUB • pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [126, alias_sig → PUB • pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [132, alias_sig → PUB • pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [138, alias_sig → PUB • pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → PUB • pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [205, pub_attr → •, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [206, pub_attr → • LPAREN READONLY RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [207, pub_attr → • LPAREN LIDENT RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_626(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_412)
    NT_pub_attr => Shift(yy_state_627)
    T_STRUCT
    | T_ENUM
    | T_TRAIT
    | T_IMPL
    | T_ASYNC
    | T_FNALIAS
    | T_FN
    | T_LET
    | T_CONST
    | T_TYPE
    | T_SUBERROR
    | T_USING
    | T_EXTENUM => Reduce(0, NT_pub_attr, yy_action_113)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [24, value_sig → PUB pub_attr • LET lident COLON type_, EOF / SEMI]
// [40, func_sig → PUB pub_attr • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [41, func_sig → PUB pub_attr • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [54, type_sig → PUB pub_attr • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [60, type_sig → PUB pub_attr • SUBERROR uident optional_derive, EOF / SEMI]
// [66, type_sig → PUB pub_attr • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [72, type_sig → PUB pub_attr • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [78, type_sig → PUB pub_attr • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [84, type_sig → PUB pub_attr • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [90, type_sig → PUB pub_attr • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → PUB pub_attr • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [104, impl_sig → PUB pub_attr • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [110, impl_sig → PUB pub_attr • IMPL qualified_uident FOR type_, EOF / SEMI]
// [116, trait_sig → PUB pub_attr • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [126, alias_sig → PUB pub_attr • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [132, alias_sig → PUB pub_attr • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [138, alias_sig → PUB pub_attr • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → PUB pub_attr • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_627(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_USING => Shift(yy_state_628)
    T_FNALIAS => Shift(yy_state_639)
    T_TRAIT => Shift(yy_state_643)
    T_IMPL => Shift(yy_state_647)
    T_EXTENUM => Shift(yy_state_655)
    T_ENUM => Shift(yy_state_667)
    T_STRUCT => Shift(yy_state_673)
    T_SUBERROR => Shift(yy_state_683)
    T_TYPE => Shift(yy_state_690)
    T_FN => Shift(yy_state_695)
    T_ASYNC => Shift(yy_state_703)
    T_LET => Shift(yy_state_712)
    T_CONST => Shift(yy_state_716)
    _ => Error
  }
}

// [138, alias_sig → PUB pub_attr USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → PUB pub_attr USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_628(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_629)
    _ => Error
  }
}

// [138, alias_sig → PUB pub_attr USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → PUB pub_attr USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_629(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_630)
    _ => Error
  }
}

// [138, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_630(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_631)
    T_TYPE => Shift(yy_state_635)
    _ => Error
  }
}

// [144, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_631(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_632)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [144, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_632(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_633)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [144, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_633(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_634)
    _ => Error
  }
}

// [144, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_634(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_alias_sig, yy_action_58)
}

// [138, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_635(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_636)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [138, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_636(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_637)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [138, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_637(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_638)
    _ => Error
  }
}

// [138, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_638(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_alias_sig, yy_action_57)
}

// [132, alias_sig → PUB pub_attr FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [212, uident → • UIDENT, COLONCOLON]

///|
fn yy_state_639(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_640)
    _ => Error
  }
}

// [132, alias_sig → PUB pub_attr FNALIAS uident • COLONCOLON lident, EOF / SEMI]

///|
fn yy_state_640(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_641)
    _ => Error
  }
}

// [132, alias_sig → PUB pub_attr FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [213, lident → • LIDENT, EOF / SEMI]

///|
fn yy_state_641(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_642)
    _ => Error
  }
}

// [132, alias_sig → PUB pub_attr FNALIAS uident COLONCOLON lident •, EOF / SEMI]

///|
fn yy_state_642(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_56)
}

// [116, trait_sig → PUB pub_attr TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [212, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]

///|
fn yy_state_643(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_644)
    _ => Error
  }
}

// [116, trait_sig → PUB pub_attr TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [230, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [231, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]

///|
fn yy_state_644(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_42)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ =>
      Shift(yy_state_645)
    T_EOF | T_SEMI | T_LBRACE =>
      Reduce(
        0,
        NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___,
        yy_action_267,
      )
    _ => Error
  }
}

// [116, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [120, trait_body → •, EOF / SEMI]
// [121, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]

///|
fn yy_state_645(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_51)
    NT_trait_body => Shift(yy_state_646)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_260)
    _ => Error
  }
}

// [116, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]

///|
fn yy_state_646(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_trait_sig, yy_action_55)
}

// [104, impl_sig → PUB pub_attr IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [110, impl_sig → PUB pub_attr IMPL • qualified_uident FOR type_, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_647(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    T_LBRACKET => Shift(yy_state_162)
    NT_qualified_uident => Shift(yy_state_648)
    NT_type_params_with_constraints => Shift(yy_state_651)
    _ => Error
  }
}

// [110, impl_sig → PUB pub_attr IMPL qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_648(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_649)
    _ => Error
  }
}

// [110, impl_sig → PUB pub_attr IMPL qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_649(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_650)
    _ => Error
  }
}

// [110, impl_sig → PUB pub_attr IMPL qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_650(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_impl_sig, yy_action_54)
}

// [104, impl_sig → PUB pub_attr IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_651(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    NT_qualified_uident => Shift(yy_state_652)
    _ => Error
  }
}

// [104, impl_sig → PUB pub_attr IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_652(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_653)
    _ => Error
  }
}

// [104, impl_sig → PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_653(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_654)
    _ => Error
  }
}

// [104, impl_sig → PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_654(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_impl_sig, yy_action_53)
}

// [90, type_sig → PUB pub_attr EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → PUB pub_attr EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [165, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [166, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [212, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]

///|
fn yy_state_655(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_184)
    NT_uident => Shift(yy_state_197)
    NT_extensible_enum_extension_target => Shift(yy_state_656)
    NT_type_decl_name_with_params => Shift(yy_state_662)
    _ => Error
  }
}

// [96, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_656(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_657)
    _ => Error
  }
}

// [96, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_657(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_658)
    _ => Error
  }
}

// [96, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_658(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_659)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [96, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_659(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_660)
    _ => Error
  }
}

// [96, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_660(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_661)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [96, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_661(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_52)
}

// [90, type_sig → PUB pub_attr EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_662(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_663)
    _ => Error
  }
}

// [90, type_sig → PUB pub_attr EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_663(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_664)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [90, type_sig → PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_664(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_665)
    _ => Error
  }
}

// [90, type_sig → PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_665(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_666)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [90, type_sig → PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_666(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_51)
}

// [84, type_sig → PUB pub_attr ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [212, uident → • UIDENT, LBRACKET / LBRACE]

///|
fn yy_state_667(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_668)
    _ => Error
  }
}

// [84, type_sig → PUB pub_attr ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_668(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_669)
    _ => Error
  }
}

// [84, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_669(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_670)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [84, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_670(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_671)
    _ => Error
  }
}

// [84, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_671(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_672)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [84, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_672(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_50)
}

// [72, type_sig → PUB pub_attr STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [78, type_sig → PUB pub_attr STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [212, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]

///|
fn yy_state_673(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_674)
    _ => Error
  }
}

// [72, type_sig → PUB pub_attr STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [78, type_sig → PUB pub_attr STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_674(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_675)
    T_LBRACE => Shift(yy_state_679)
    _ => Error
  }
}

// [78, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [225, separated_list(COMMA,type_) → •, RPAREN]
// [226, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [250, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [251, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]

///|
fn yy_state_675(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    NT_type_ => Shift(yy_state_80)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_256)
    NT_separated_list_COMMA_type__ => Shift(yy_state_676)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_157)
    _ => Error
  }
}

// [78, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_676(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_677)
    _ => Error
  }
}

// [78, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_677(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_678)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [78, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]

///|
fn yy_state_678(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_49)
}

// [72, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [152, record_decl_field → • boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [153, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [154, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [240, boption(MUTABLE) → •, LIDENT]
// [241, boption(MUTABLE) → • MUTABLE, LIDENT]
// [242, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [243, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE]
// [266, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [267, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]

///|
fn yy_state_679(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_208)
    NT_record_decl_field => Shift(yy_state_261)
    NT_nonempty_list_attribute_ => Shift(yy_state_264)
    NT_boption_MUTABLE_ => Shift(yy_state_269)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_273)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_274)
    NT_record_decl_body => Shift(yy_state_680)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_180)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_154)
    _ => Error
  }
}

// [72, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_680(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_681)
    _ => Error
  }
}

// [72, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_681(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_682)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [72, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_682(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_48)
}

// [60, type_sig → PUB pub_attr SUBERROR • uident optional_derive, EOF / SEMI]
// [66, type_sig → PUB pub_attr SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]

///|
fn yy_state_683(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_684)
    _ => Error
  }
}

// [60, type_sig → PUB pub_attr SUBERROR uident • optional_derive, EOF / SEMI]
// [66, type_sig → PUB pub_attr SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_684(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_LBRACE => Shift(yy_state_685)
    NT_optional_derive => Shift(yy_state_689)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [66, type_sig → PUB pub_attr SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_685(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_686)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [66, type_sig → PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_686(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_687)
    _ => Error
  }
}

// [66, type_sig → PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_687(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_688)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [66, type_sig → PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_688(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_47)
}

// [60, type_sig → PUB pub_attr SUBERROR uident optional_derive •, EOF / SEMI]

///|
fn yy_state_689(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_46)
}

// [54, type_sig → PUB pub_attr TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [126, alias_sig → PUB pub_attr TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]

///|
fn yy_state_690(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_691)
    _ => Error
  }
}

// [54, type_sig → PUB pub_attr TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [126, alias_sig → PUB pub_attr TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]

///|
fn yy_state_691(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_EQUAL => Shift(yy_state_692)
    NT_optional_derive => Shift(yy_state_694)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [126, alias_sig → PUB pub_attr TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_692(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_693)
    _ => Error
  }
}

// [126, alias_sig → PUB pub_attr TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]

///|
fn yy_state_693(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_45)
}

// [54, type_sig → PUB pub_attr TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]

///|
fn yy_state_694(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_44)
}

// [41, func_sig → PUB pub_attr FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_695(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_696)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_696(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_697)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_697(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_698)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_698(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_699)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_699(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_700)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_700(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_701)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_701(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_702)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_702(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_func_sig, yy_action_43)
}

// [40, func_sig → PUB pub_attr ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_703(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_704)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_704(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_705)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_705(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_706)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_706(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_707)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_707(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_708)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_708(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_709)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_709(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_710)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_710(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_711)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_711(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(11, NT_func_sig, yy_action_42)
}

// [24, value_sig → PUB pub_attr LET • lident COLON type_, EOF / SEMI]
// [213, lident → • LIDENT, COLON]

///|
fn yy_state_712(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_713)
    _ => Error
  }
}

// [24, value_sig → PUB pub_attr LET lident • COLON type_, EOF / SEMI]

///|
fn yy_state_713(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_714)
    _ => Error
  }
}

// [24, value_sig → PUB pub_attr LET lident COLON • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_714(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_715)
    _ => Error
  }
}

// [24, value_sig → PUB pub_attr LET lident COLON type_ •, EOF / SEMI]

///|
fn yy_state_715(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_value_sig, yy_action_41)
}

// [18, const_sig → PUB pub_attr CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [212, uident → • UIDENT, COLON]

///|
fn yy_state_716(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_717)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr CONST uident • COLON type_ EQUAL constant, EOF / SEMI]

///|
fn yy_state_717(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_718)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [172, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [173, type_ → • simple_type, EQUAL]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [210, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]

///|
fn yy_state_718(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_719)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr CONST uident COLON type_ • EQUAL constant, EOF / SEMI]

///|
fn yy_state_719(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_720)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [193, constant → • TRUE, EOF / SEMI]
// [194, constant → • FALSE, EOF / SEMI]
// [195, constant → • BYTE, EOF / SEMI]
// [196, constant → • BYTES, EOF / SEMI]
// [197, constant → • CHAR, EOF / SEMI]
// [198, constant → • INT, EOF / SEMI]
// [199, constant → • MINUS INT, EOF / SEMI]
// [200, constant → • FLOAT, EOF / SEMI]
// [201, constant → • MINUS FLOAT, EOF / SEMI]
// [202, constant → • DOUBLE, EOF / SEMI]
// [203, constant → • MINUS DOUBLE, EOF / SEMI]
// [204, constant → • STRING, EOF / SEMI]

///|
fn yy_state_720(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_511)
    T_DOUBLE => Shift(yy_state_512)
    T_FLOAT => Shift(yy_state_513)
    T_MINUS => Shift(yy_state_514)
    T_INT => Shift(yy_state_518)
    T_CHAR => Shift(yy_state_519)
    T_BYTES => Shift(yy_state_520)
    T_BYTE => Shift(yy_state_521)
    T_FALSE => Shift(yy_state_522)
    T_TRUE => Shift(yy_state_523)
    NT_constant => Shift(yy_state_721)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr CONST uident COLON type_ EQUAL constant •, EOF / SEMI]

///|
fn yy_state_721(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_const_sig, yy_action_39)
}

// [17, const_sig → PRIV • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [23, value_sig → PRIV • LET lident COLON type_, EOF / SEMI]
// [38, func_sig → PRIV • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [39, func_sig → PRIV • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [53, type_sig → PRIV • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [59, type_sig → PRIV • SUBERROR uident optional_derive, EOF / SEMI]
// [65, type_sig → PRIV • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [71, type_sig → PRIV • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [77, type_sig → PRIV • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [83, type_sig → PRIV • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [89, type_sig → PRIV • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [95, type_sig → PRIV • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [103, impl_sig → PRIV • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [109, impl_sig → PRIV • IMPL qualified_uident FOR type_, EOF / SEMI]
// [115, trait_sig → PRIV • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [125, alias_sig → PRIV • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [131, alias_sig → PRIV • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [137, alias_sig → PRIV • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [143, alias_sig → PRIV • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_722(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_USING => Shift(yy_state_723)
    T_FNALIAS => Shift(yy_state_734)
    T_TRAIT => Shift(yy_state_738)
    T_IMPL => Shift(yy_state_742)
    T_EXTENUM => Shift(yy_state_750)
    T_ENUM => Shift(yy_state_762)
    T_STRUCT => Shift(yy_state_768)
    T_SUBERROR => Shift(yy_state_778)
    T_TYPE => Shift(yy_state_785)
    T_FN => Shift(yy_state_790)
    T_ASYNC => Shift(yy_state_798)
    T_LET => Shift(yy_state_807)
    T_CONST => Shift(yy_state_811)
    _ => Error
  }
}

// [137, alias_sig → PRIV USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [143, alias_sig → PRIV USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_723(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_724)
    _ => Error
  }
}

// [137, alias_sig → PRIV USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [143, alias_sig → PRIV USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_724(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_725)
    _ => Error
  }
}

// [137, alias_sig → PRIV USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [143, alias_sig → PRIV USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_725(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_726)
    T_TYPE => Shift(yy_state_730)
    _ => Error
  }
}

// [143, alias_sig → PRIV USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_726(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_727)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [143, alias_sig → PRIV USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_727(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_728)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [143, alias_sig → PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_728(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_729)
    _ => Error
  }
}

// [143, alias_sig → PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_729(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_alias_sig, yy_action_38)
}

// [137, alias_sig → PRIV USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [212, uident → • UIDENT, RBRACE / AS]

///|
fn yy_state_730(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_731)
    _ => Error
  }
}

// [122, using_binder → •, RBRACE]
// [123, using_binder → • AS uident, RBRACE]
// [137, alias_sig → PRIV USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]

///|
fn yy_state_731(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_732)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_273)
    _ => Error
  }
}

// [137, alias_sig → PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]

///|
fn yy_state_732(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_733)
    _ => Error
  }
}

// [137, alias_sig → PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]

///|
fn yy_state_733(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_alias_sig, yy_action_37)
}

// [131, alias_sig → PRIV FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [212, uident → • UIDENT, COLONCOLON]

///|
fn yy_state_734(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_735)
    _ => Error
  }
}

// [131, alias_sig → PRIV FNALIAS uident • COLONCOLON lident, EOF / SEMI]

///|
fn yy_state_735(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_736)
    _ => Error
  }
}

// [131, alias_sig → PRIV FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [213, lident → • LIDENT, EOF / SEMI]

///|
fn yy_state_736(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_737)
    _ => Error
  }
}

// [131, alias_sig → PRIV FNALIAS uident COLONCOLON lident •, EOF / SEMI]

///|
fn yy_state_737(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_36)
}

// [115, trait_sig → PRIV TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [212, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]

///|
fn yy_state_738(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_739)
    _ => Error
  }
}

// [115, trait_sig → PRIV TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [230, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [231, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]

///|
fn yy_state_739(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_42)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ =>
      Shift(yy_state_740)
    T_EOF | T_SEMI | T_LBRACE =>
      Reduce(
        0,
        NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___,
        yy_action_267,
      )
    _ => Error
  }
}

// [115, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [120, trait_body → •, EOF / SEMI]
// [121, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]

///|
fn yy_state_740(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_51)
    NT_trait_body => Shift(yy_state_741)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_260)
    _ => Error
  }
}

// [115, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]

///|
fn yy_state_741(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_trait_sig, yy_action_35)
}

// [103, impl_sig → PRIV IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [109, impl_sig → PRIV IMPL • qualified_uident FOR type_, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_742(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    T_LBRACKET => Shift(yy_state_162)
    NT_qualified_uident => Shift(yy_state_743)
    NT_type_params_with_constraints => Shift(yy_state_746)
    _ => Error
  }
}

// [109, impl_sig → PRIV IMPL qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_743(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_744)
    _ => Error
  }
}

// [109, impl_sig → PRIV IMPL qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_744(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_745)
    _ => Error
  }
}

// [109, impl_sig → PRIV IMPL qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_745(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_impl_sig, yy_action_34)
}

// [103, impl_sig → PRIV IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [208, qualified_uident → • UIDENT, FOR]
// [209, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]

///|
fn yy_state_746(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_47)
    T_UIDENT => Shift(yy_state_49)
    NT_qualified_uident => Shift(yy_state_747)
    _ => Error
  }
}

// [103, impl_sig → PRIV IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]

///|
fn yy_state_747(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_748)
    _ => Error
  }
}

// [103, impl_sig → PRIV IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_748(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_749)
    _ => Error
  }
}

// [103, impl_sig → PRIV IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]

///|
fn yy_state_749(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_impl_sig, yy_action_33)
}

// [89, type_sig → PRIV EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [95, type_sig → PRIV EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [165, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [166, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [212, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]

///|
fn yy_state_750(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_184)
    NT_uident => Shift(yy_state_197)
    NT_extensible_enum_extension_target => Shift(yy_state_751)
    NT_type_decl_name_with_params => Shift(yy_state_757)
    _ => Error
  }
}

// [95, type_sig → PRIV EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_751(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_752)
    _ => Error
  }
}

// [95, type_sig → PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_752(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_753)
    _ => Error
  }
}

// [95, type_sig → PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_753(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_754)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [95, type_sig → PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_754(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_755)
    _ => Error
  }
}

// [95, type_sig → PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_755(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_756)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [95, type_sig → PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_756(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_32)
}

// [89, type_sig → PRIV EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_757(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_758)
    _ => Error
  }
}

// [89, type_sig → PRIV EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_758(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_759)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [89, type_sig → PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_759(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_760)
    _ => Error
  }
}

// [89, type_sig → PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_760(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_761)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [89, type_sig → PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_761(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_31)
}

// [83, type_sig → PRIV ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [212, uident → • UIDENT, LBRACKET / LBRACE]

///|
fn yy_state_762(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_763)
    _ => Error
  }
}

// [83, type_sig → PRIV ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_763(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_764)
    _ => Error
  }
}

// [83, type_sig → PRIV ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_764(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_765)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [83, type_sig → PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_765(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_766)
    _ => Error
  }
}

// [83, type_sig → PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_766(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_767)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [83, type_sig → PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_767(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_30)
}

// [71, type_sig → PRIV STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [77, type_sig → PRIV STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [212, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]

///|
fn yy_state_768(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_769)
    _ => Error
  }
}

// [71, type_sig → PRIV STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [77, type_sig → PRIV STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_769(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_770)
    T_LBRACE => Shift(yy_state_774)
    _ => Error
  }
}

// [77, type_sig → PRIV STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [225, separated_list(COMMA,type_) → •, RPAREN]
// [226, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [250, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [251, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]

///|
fn yy_state_770(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    NT_type_ => Shift(yy_state_80)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_256)
    NT_separated_list_COMMA_type__ => Shift(yy_state_771)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_157)
    _ => Error
  }
}

// [77, type_sig → PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]

///|
fn yy_state_771(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_772)
    _ => Error
  }
}

// [77, type_sig → PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_772(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_773)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [77, type_sig → PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]

///|
fn yy_state_773(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_29)
}

// [71, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [152, record_decl_field → • boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [153, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) LIDENT COLON type_, SEMI / RBRACE]
// [154, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [240, boption(MUTABLE) → •, LIDENT]
// [241, boption(MUTABLE) → • MUTABLE, LIDENT]
// [242, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [243, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE]
// [266, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [267, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]

///|
fn yy_state_774(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_208)
    NT_record_decl_field => Shift(yy_state_261)
    NT_nonempty_list_attribute_ => Shift(yy_state_264)
    NT_boption_MUTABLE_ => Shift(yy_state_269)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_273)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_274)
    NT_record_decl_body => Shift(yy_state_775)
    T_LIDENT => Reduce(0, NT_boption_MUTABLE_, yy_action_180)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_154)
    _ => Error
  }
}

// [71, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_775(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_776)
    _ => Error
  }
}

// [71, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_776(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_777)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [71, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_777(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_28)
}

// [59, type_sig → PRIV SUBERROR • uident optional_derive, EOF / SEMI]
// [65, type_sig → PRIV SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]

///|
fn yy_state_778(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_779)
    _ => Error
  }
}

// [59, type_sig → PRIV SUBERROR uident • optional_derive, EOF / SEMI]
// [65, type_sig → PRIV SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_779(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_LBRACE => Shift(yy_state_780)
    NT_optional_derive => Shift(yy_state_784)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [65, type_sig → PRIV SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [148, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [149, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [223, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [224, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [252, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [260, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [261, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]

///|
fn yy_state_780(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_202)
    NT_nonempty_list_attribute_ => Shift(yy_state_205)
    T_UIDENT => Shift(yy_state_224)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_227)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_781)
    T_RBRACE =>
      Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_184)
    _ => Error
  }
}

// [65, type_sig → PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]

///|
fn yy_state_781(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_782)
    _ => Error
  }
}

// [65, type_sig → PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]

///|
fn yy_state_782(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    NT_optional_derive => Shift(yy_state_783)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [65, type_sig → PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]

///|
fn yy_state_783(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_27)
}

// [59, type_sig → PRIV SUBERROR uident optional_derive •, EOF / SEMI]

///|
fn yy_state_784(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_26)
}

// [53, type_sig → PRIV TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [125, alias_sig → PRIV TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [164, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [212, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]

///|
fn yy_state_785(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_246)
    NT_type_decl_name_with_params => Shift(yy_state_786)
    _ => Error
  }
}

// [53, type_sig → PRIV TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [100, optional_derive → •, EOF / SEMI]
// [101, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [125, alias_sig → PRIV TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]

///|
fn yy_state_786(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_230)
    T_EQUAL => Shift(yy_state_787)
    NT_optional_derive => Shift(yy_state_789)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_165)
    _ => Error
  }
}

// [125, alias_sig → PRIV TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_787(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_788)
    _ => Error
  }
}

// [125, alias_sig → PRIV TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]

///|
fn yy_state_788(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_25)
}

// [53, type_sig → PRIV TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]

///|
fn yy_state_789(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_24)
}

// [39, func_sig → PRIV FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_790(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_791)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [39, func_sig → PRIV FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_791(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_792)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_792(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_793)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_793(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_794)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_794(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_795)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_795(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_796)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_796(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_797)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_797(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_func_sig, yy_action_22)
}

// [38, func_sig → PRIV ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_798(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_799)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [157, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER]
// [217, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER]
// [218, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER]

///|
fn yy_state_799(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_162)
    NT_type_params_with_constraints => Shift(yy_state_291)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_800)
    T_LIDENT | T_UIDENT | T_AMPER =>
      Reduce(0, NT_loption_type_params_with_constraints_, yy_action_142)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [212, uident → • UIDENT, LPAREN]
// [213, lident → • LIDENT, LPAREN]

///|
fn yy_state_800(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_method_self_type_coloncolon => Shift(yy_state_299)
    NT_uident => Shift(yy_state_301)
    NT_lident => Shift(yy_state_302)
    NT_luident => Shift(yy_state_303)
    T_AMPER => Shift(yy_state_304)
    T_UIDENT => Shift(yy_state_307)
    NT_func_name => Shift(yy_state_801)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_801(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_802)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [172, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [173, type_ → • simple_type, RPAREN / COMMA]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [190, parameter → • type_, RPAREN / COMMA]
// [191, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [192, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [210, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [214, label → • LIDENT, QUESTION]
// [219, separated_list(COMMA,parameter) → •, RPAREN]
// [220, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [258, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [259, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]

///|
fn yy_state_802(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_parameter => Shift(yy_state_58)
    T_LIDENT => Shift(yy_state_61)
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_label => Shift(yy_state_65)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_122)
    NT_type_ => Shift(yy_state_125)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_126)
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_803)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_256)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_803(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_804)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]

///|
fn yy_state_804(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_805)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [171, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, return_type → • type_, EOF / SEMI]
// [183, return_type → • simple_type NORAISE, EOF / SEMI]
// [184, return_type → • simple_type RAISE, EOF / SEMI]
// [185, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [186, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]

///|
fn yy_state_805(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    NT_type_ => Shift(yy_state_73)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_87)
    NT_return_type => Shift(yy_state_806)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]

///|
fn yy_state_806(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_func_sig, yy_action_19)
}

// [23, value_sig → PRIV LET • lident COLON type_, EOF / SEMI]
// [213, lident → • LIDENT, COLON]

///|
fn yy_state_807(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LIDENT => Shift(yy_state_38)
    NT_lident => Shift(yy_state_808)
    _ => Error
  }
}

// [23, value_sig → PRIV LET lident • COLON type_, EOF / SEMI]

///|
fn yy_state_808(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_809)
    _ => Error
  }
}

// [23, value_sig → PRIV LET lident COLON • type_, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [169, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [171, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [172, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [173, type_ → • simple_type, EOF / SEMI]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [210, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]

///|
fn yy_state_809(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_810)
    _ => Error
  }
}

// [23, value_sig → PRIV LET lident COLON type_ •, EOF / SEMI]

///|
fn yy_state_810(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_value_sig, yy_action_18)
}

// [17, const_sig → PRIV CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [212, uident → • UIDENT, COLON]

///|
fn yy_state_811(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_812)
    _ => Error
  }
}

// [17, const_sig → PRIV CONST uident • COLON type_ EQUAL constant, EOF / SEMI]

///|
fn yy_state_812(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_813)
    _ => Error
  }
}

// [17, const_sig → PRIV CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [172, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [173, type_ → • simple_type, EQUAL]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [210, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]

///|
fn yy_state_813(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_814)
    _ => Error
  }
}

// [17, const_sig → PRIV CONST uident COLON type_ • EQUAL constant, EOF / SEMI]

///|
fn yy_state_814(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_815)
    _ => Error
  }
}

// [17, const_sig → PRIV CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [193, constant → • TRUE, EOF / SEMI]
// [194, constant → • FALSE, EOF / SEMI]
// [195, constant → • BYTE, EOF / SEMI]
// [196, constant → • BYTES, EOF / SEMI]
// [197, constant → • CHAR, EOF / SEMI]
// [198, constant → • INT, EOF / SEMI]
// [199, constant → • MINUS INT, EOF / SEMI]
// [200, constant → • FLOAT, EOF / SEMI]
// [201, constant → • MINUS FLOAT, EOF / SEMI]
// [202, constant → • DOUBLE, EOF / SEMI]
// [203, constant → • MINUS DOUBLE, EOF / SEMI]
// [204, constant → • STRING, EOF / SEMI]

///|
fn yy_state_815(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_511)
    T_DOUBLE => Shift(yy_state_512)
    T_FLOAT => Shift(yy_state_513)
    T_MINUS => Shift(yy_state_514)
    T_INT => Shift(yy_state_518)
    T_CHAR => Shift(yy_state_519)
    T_BYTES => Shift(yy_state_520)
    T_BYTE => Shift(yy_state_521)
    T_FALSE => Shift(yy_state_522)
    T_TRUE => Shift(yy_state_523)
    NT_constant => Shift(yy_state_816)
    _ => Error
  }
}

// [17, const_sig → PRIV CONST uident COLON type_ EQUAL constant •, EOF / SEMI]

///|
fn yy_state_816(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_const_sig, yy_action_16)
}

// [16, const_sig → CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [212, uident → • UIDENT, COLON]

///|
fn yy_state_817(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_818)
    _ => Error
  }
}

// [16, const_sig → CONST uident • COLON type_ EQUAL constant, EOF / SEMI]

///|
fn yy_state_818(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_819)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [167, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [168, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [169, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [170, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [171, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [172, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [173, type_ → • simple_type, EQUAL]
// [174, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [175, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [176, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [177, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [178, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [210, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [211, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]

///|
fn yy_state_819(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_820)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON type_ • EQUAL constant, EOF / SEMI]

///|
fn yy_state_820(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_821)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [193, constant → • TRUE, EOF / SEMI]
// [194, constant → • FALSE, EOF / SEMI]
// [195, constant → • BYTE, EOF / SEMI]
// [196, constant → • BYTES, EOF / SEMI]
// [197, constant → • CHAR, EOF / SEMI]
// [198, constant → • INT, EOF / SEMI]
// [199, constant → • MINUS INT, EOF / SEMI]
// [200, constant → • FLOAT, EOF / SEMI]
// [201, constant → • MINUS FLOAT, EOF / SEMI]
// [202, constant → • DOUBLE, EOF / SEMI]
// [203, constant → • MINUS DOUBLE, EOF / SEMI]
// [204, constant → • STRING, EOF / SEMI]

///|
fn yy_state_821(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_511)
    T_DOUBLE => Shift(yy_state_512)
    T_FLOAT => Shift(yy_state_513)
    T_MINUS => Shift(yy_state_514)
    T_INT => Shift(yy_state_518)
    T_CHAR => Shift(yy_state_519)
    T_BYTES => Shift(yy_state_520)
    T_BYTE => Shift(yy_state_521)
    T_FALSE => Shift(yy_state_522)
    T_TRUE => Shift(yy_state_523)
    NT_constant => Shift(yy_state_822)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON type_ EQUAL constant •, EOF / SEMI]

///|
fn yy_state_822(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_const_sig, yy_action_13)
}

// [15, sig_ → value_sig •, EOF / SEMI]

///|
fn yy_state_823(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_12)
}

// [14, sig_ → const_sig •, EOF / SEMI]

///|
fn yy_state_824(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_11)
}

// [13, sig_ → impl_sig •, EOF / SEMI]

///|
fn yy_state_825(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_10)
}

// [12, sig_ → trait_sig •, EOF / SEMI]

///|
fn yy_state_826(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_9)
}

// [11, sig_ → alias_sig •, EOF / SEMI]

///|
fn yy_state_827(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_8)
}

// [10, sig_ → type_sig •, EOF / SEMI]

///|
fn yy_state_828(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_7)
}

// [9, sig_ → func_sig •, EOF / SEMI]

///|
fn yy_state_829(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_6)
}

// [7, sigs → sig_ •, EOF]
// [8, sigs → sig_ • SEMI sigs, EOF]

///|
fn yy_state_830(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_831)
    T_EOF => Reduce(1, NT_sigs, yy_action_5)
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
// [17, const_sig → • PRIV CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [18, const_sig → • PUB pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [19, const_sig → • nonempty_list(attribute) CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [20, const_sig → • nonempty_list(attribute) PRIV CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [21, const_sig → • nonempty_list(attribute) PUB pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [22, value_sig → • LET lident COLON type_, EOF / SEMI]
// [23, value_sig → • PRIV LET lident COLON type_, EOF / SEMI]
// [24, value_sig → • PUB pub_attr LET lident COLON type_, EOF / SEMI]
// [25, value_sig → • nonempty_list(attribute) LET lident COLON type_, EOF / SEMI]
// [26, value_sig → • nonempty_list(attribute) PRIV LET lident COLON type_, EOF / SEMI]
// [27, value_sig → • nonempty_list(attribute) PUB pub_attr LET lident COLON type_, EOF / SEMI]
// [36, func_sig → • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [37, func_sig → • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [38, func_sig → • PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [39, func_sig → • PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [40, func_sig → • PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [41, func_sig → • PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [42, func_sig → • nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [43, func_sig → • nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [44, func_sig → • nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [45, func_sig → • nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [46, func_sig → • nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [47, func_sig → • nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [52, type_sig → • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [53, type_sig → • PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [54, type_sig → • PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [55, type_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [56, type_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [57, type_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [58, type_sig → • SUBERROR uident optional_derive, EOF / SEMI]
// [59, type_sig → • PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [60, type_sig → • PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [61, type_sig → • nonempty_list(attribute) SUBERROR uident optional_derive, EOF / SEMI]
// [62, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [63, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [64, type_sig → • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [65, type_sig → • PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [66, type_sig → • PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [67, type_sig → • nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [68, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [69, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [70, type_sig → • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [71, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [72, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [73, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [74, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [75, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [76, type_sig → • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [77, type_sig → • PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [78, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [79, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [80, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [81, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [82, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [83, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [84, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [85, type_sig → • nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [86, type_sig → • nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [87, type_sig → • nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [88, type_sig → • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [89, type_sig → • PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [90, type_sig → • PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [91, type_sig → • nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [92, type_sig → • nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [93, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [94, type_sig → • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [95, type_sig → • PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → • PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → • nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [98, type_sig → • nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [102, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [103, impl_sig → • PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [104, impl_sig → • PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [105, impl_sig → • nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [106, impl_sig → • nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [107, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [108, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [109, impl_sig → • PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [110, impl_sig → • PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [111, impl_sig → • nonempty_list(attribute) IMPL qualified_uident FOR type_, EOF / SEMI]
// [112, impl_sig → • nonempty_list(attribute) PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [114, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [115, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [116, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [117, trait_sig → • nonempty_list(attribute) TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [118, trait_sig → • nonempty_list(attribute) PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [119, trait_sig → • nonempty_list(attribute) PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [124, alias_sig → • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [125, alias_sig → • PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [126, alias_sig → • PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [127, alias_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [128, alias_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [129, alias_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [130, alias_sig → • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [131, alias_sig → • PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [132, alias_sig → • PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [133, alias_sig → • nonempty_list(attribute) FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [134, alias_sig → • nonempty_list(attribute) PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [135, alias_sig → • nonempty_list(attribute) PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [136, alias_sig → • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [137, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [138, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [139, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [140, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [141, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [142, alias_sig → • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [143, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [252, nonempty_list(attribute) → • ATTRIBUTE, PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_831(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_USING => Shift(yy_state_21)
    T_FNALIAS => Shift(yy_state_35)
    T_TRAIT => Shift(yy_state_40)
    T_IMPL => Shift(yy_state_161)
    T_EXTENUM => Shift(yy_state_183)
    T_ENUM => Shift(yy_state_245)
    T_STRUCT => Shift(yy_state_253)
    T_SUBERROR => Shift(yy_state_278)
    T_TYPE => Shift(yy_state_285)
    T_FN => Shift(yy_state_290)
    T_ASYNC => Shift(yy_state_309)
    T_LET => Shift(yy_state_318)
    NT_nonempty_list_attribute_ => Shift(yy_state_322)
    T_PUB => Shift(yy_state_626)
    T_PRIV => Shift(yy_state_722)
    T_CONST => Shift(yy_state_817)
    NT_value_sig => Shift(yy_state_823)
    NT_const_sig => Shift(yy_state_824)
    NT_impl_sig => Shift(yy_state_825)
    NT_trait_sig => Shift(yy_state_826)
    NT_alias_sig => Shift(yy_state_827)
    NT_type_sig => Shift(yy_state_828)
    NT_func_sig => Shift(yy_state_829)
    NT_sig_ => Shift(yy_state_830)
    NT_sigs => Shift(yy_state_832)
    T_EOF => Reduce(0, NT_sigs, yy_action_278)
    _ => Error
  }
}

// [8, sigs → sig_ SEMI sigs •, EOF]

///|
fn yy_state_832(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_sigs, yy_action_4)
}

// [1, mbti → PACKAGE STRING SEMI imports sigs • EOF, $]

///|
fn yy_state_833(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EOF => Shift(yy_state_834)
    _ => Error
  }
}

// [1, mbti → PACKAGE STRING SEMI imports sigs EOF •, $]

///|
fn yy_state_834(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_mbti, yy_action_2)
}

// [0, mbti → PACKAGE STRING imports • sigs EOF, $]
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
// [17, const_sig → • PRIV CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [18, const_sig → • PUB pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [19, const_sig → • nonempty_list(attribute) CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [20, const_sig → • nonempty_list(attribute) PRIV CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [21, const_sig → • nonempty_list(attribute) PUB pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [22, value_sig → • LET lident COLON type_, EOF / SEMI]
// [23, value_sig → • PRIV LET lident COLON type_, EOF / SEMI]
// [24, value_sig → • PUB pub_attr LET lident COLON type_, EOF / SEMI]
// [25, value_sig → • nonempty_list(attribute) LET lident COLON type_, EOF / SEMI]
// [26, value_sig → • nonempty_list(attribute) PRIV LET lident COLON type_, EOF / SEMI]
// [27, value_sig → • nonempty_list(attribute) PUB pub_attr LET lident COLON type_, EOF / SEMI]
// [36, func_sig → • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [37, func_sig → • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [38, func_sig → • PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [39, func_sig → • PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [40, func_sig → • PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [41, func_sig → • PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [42, func_sig → • nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [43, func_sig → • nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [44, func_sig → • nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [45, func_sig → • nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [46, func_sig → • nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [47, func_sig → • nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [52, type_sig → • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [53, type_sig → • PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [54, type_sig → • PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [55, type_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [56, type_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [57, type_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [58, type_sig → • SUBERROR uident optional_derive, EOF / SEMI]
// [59, type_sig → • PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [60, type_sig → • PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [61, type_sig → • nonempty_list(attribute) SUBERROR uident optional_derive, EOF / SEMI]
// [62, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [63, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [64, type_sig → • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [65, type_sig → • PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [66, type_sig → • PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [67, type_sig → • nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [68, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [69, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [70, type_sig → • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [71, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [72, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [73, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [74, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [75, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [76, type_sig → • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [77, type_sig → • PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [78, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [79, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [80, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [81, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [82, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [83, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [84, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [85, type_sig → • nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [86, type_sig → • nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [87, type_sig → • nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [88, type_sig → • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [89, type_sig → • PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [90, type_sig → • PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [91, type_sig → • nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [92, type_sig → • nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [93, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [94, type_sig → • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [95, type_sig → • PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → • PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → • nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [98, type_sig → • nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [102, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [103, impl_sig → • PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [104, impl_sig → • PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [105, impl_sig → • nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [106, impl_sig → • nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [107, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [108, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [109, impl_sig → • PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [110, impl_sig → • PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [111, impl_sig → • nonempty_list(attribute) IMPL qualified_uident FOR type_, EOF / SEMI]
// [112, impl_sig → • nonempty_list(attribute) PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [114, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [115, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [116, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [117, trait_sig → • nonempty_list(attribute) TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [118, trait_sig → • nonempty_list(attribute) PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [119, trait_sig → • nonempty_list(attribute) PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [124, alias_sig → • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [125, alias_sig → • PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [126, alias_sig → • PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [127, alias_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [128, alias_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [129, alias_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [130, alias_sig → • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [131, alias_sig → • PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [132, alias_sig → • PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [133, alias_sig → • nonempty_list(attribute) FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [134, alias_sig → • nonempty_list(attribute) PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [135, alias_sig → • nonempty_list(attribute) PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [136, alias_sig → • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [137, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [138, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [139, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [140, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [141, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [142, alias_sig → • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [143, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [252, nonempty_list(attribute) → • ATTRIBUTE, PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [253, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]

///|
fn yy_state_835(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_USING => Shift(yy_state_21)
    T_FNALIAS => Shift(yy_state_35)
    T_TRAIT => Shift(yy_state_40)
    T_IMPL => Shift(yy_state_161)
    T_EXTENUM => Shift(yy_state_183)
    T_ENUM => Shift(yy_state_245)
    T_STRUCT => Shift(yy_state_253)
    T_SUBERROR => Shift(yy_state_278)
    T_TYPE => Shift(yy_state_285)
    T_FN => Shift(yy_state_290)
    T_ASYNC => Shift(yy_state_309)
    T_LET => Shift(yy_state_318)
    NT_nonempty_list_attribute_ => Shift(yy_state_322)
    T_PUB => Shift(yy_state_626)
    T_PRIV => Shift(yy_state_722)
    T_CONST => Shift(yy_state_817)
    NT_value_sig => Shift(yy_state_823)
    NT_const_sig => Shift(yy_state_824)
    NT_impl_sig => Shift(yy_state_825)
    NT_trait_sig => Shift(yy_state_826)
    NT_alias_sig => Shift(yy_state_827)
    NT_type_sig => Shift(yy_state_828)
    NT_func_sig => Shift(yy_state_829)
    NT_sig_ => Shift(yy_state_830)
    NT_sigs => Shift(yy_state_836)
    T_EOF => Reduce(0, NT_sigs, yy_action_278)
    _ => Error
  }
}

// [0, mbti → PACKAGE STRING imports sigs • EOF, $]

///|
fn yy_state_836(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EOF => Shift(yy_state_837)
    _ => Error
  }
}

// [0, mbti → PACKAGE STRING imports sigs EOF •, $]

///|
fn yy_state_837(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_mbti, yy_action_0)
}

///|
fn[T] yy_parse(
  tokens : Array[(Token, @basic.Position, @basic.Position)],
  start : YYState,
  return_ : (YYObj) -> T,
  initial_pos? : @basic.Position,
) -> T raise ParseError {
  let mut cursor = 0
  let mut state_stack : @list.List[YYState] = @list.cons(start, @list.empty())
  let data_stack : Array[(YYObj, @basic.Position, @basic.Position)] = []
  let mut last_pos = initial_pos.unwrap_or(tokens[0].1)
  let mut state = start
  let mut lookahead : (
    YYSymbol,
    (YYObj, @basic.Position, @basic.Position),
    Token?,
  )? = None
  let mut last_shifted_state_stack = state_stack
  while true {
    let decision = match state(EOI) {
      ReduceNoLookahead(_) | Accept as t => t
      _ =>
        match lookahead {
          Some(la) => state(la.0)
          None =>
            if cursor < tokens.length() {
              let (token, start_pos, end_pos) = tokens[cursor]
              cursor += 1
              let (symbol, data) = yy_input(token, start_pos, end_pos)
              lookahead = Some(
                (symbol, (data, start_pos, end_pos), Some(token)),
              )
              state(symbol)
            } else {
              lookahead = Some((EOI, (YYObj_Void, last_pos, last_pos), None))
              state(EOI)
            }
        }
    }
    match decision {
      Accept => return return_(data_stack.unsafe_pop().0)
      Shift(next_state) => {
        guard lookahead is Some(la)
        data_stack.push(la.1)
        state_stack = @list.cons(next_state, state_stack)
        last_shifted_state_stack = state_stack
        state = next_state
        last_pos = la.1.2
        lookahead = None
      }
      Reduce(count, symbol, action)
      | ReduceNoLookahead(count, symbol, action) =>
        for count = count, symbol = symbol, action = action {
          let args = data_stack[data_stack.length() - count:]
          let data = action(last_pos, args)
          let (start_pos, end_pos) = if args.length() == 0 {
            (last_pos, last_pos)
          } else {
            (args[0].1, args[args.length() - 1].2)
          }
          for _ in 0..<count {
            ignore(data_stack.unsafe_pop())
            state_stack = state_stack.unsafe_tail()
          }
          state = state_stack.unsafe_head()
          data_stack.push((data, start_pos, end_pos))
          match state(symbol) {
            Accept => return return_(data_stack.unsafe_pop().0)
            Shift(next_state) => {
              state_stack = @list.cons(next_state, state_stack)
              state = next_state
              break ()
            }
            Reduce(count, symbol, action)
            | ReduceNoLookahead(count, symbol, action) =>
              continue count, symbol, action
            _ => panic()
          }
        }
      Error => {
        let (_, (_, start_pos, end_pos), token) = lookahead.unwrap()
        error(last_shifted_state_stack, token, (start_pos, end_pos))
      }
    }
  }
  panic()
}

///|
fn error(
  stack : @list.List[YYState],
  token : Token?,
  loc : (@basic.Position, @basic.Position),
) -> Unit raise ParseError {
  let expected = []
  fn try_add(symbol : YYSymbol, kind : TokenKind) {
    fn go(stack : @list.List[YYState]) {
      match stack {
        Empty => ()
        More(state, ..) =>
          match state(symbol) {
            Accept | Shift(_) => expected.push(kind)
            Reduce(count, symbol, _) | ReduceNoLookahead(count, symbol, _) => {
              fn inner_go(stack : @list.List[YYState], count, symbol) {
                let stack = stack.drop(count)
                guard stack is More(state, ..)
                match state(symbol) {
                  Shift(state) => go(@list.cons(state, stack))
                  Reduce(count, symbol, _)
                  | ReduceNoLookahead(count, symbol, _) =>
                    inner_go(stack, count, symbol)
                  _ => panic()
                }
              }
              inner_go(stack, count, symbol)
            }
            Error => ()
          }
      }
    }
    go(stack)
  }
  for
    term in (
      [
        (T_CHAR, TK_CHAR),
        (T_INT, TK_INT),
        (T_BYTE, TK_BYTE),
        (T_BYTES, TK_BYTES),
        (T_FLOAT, TK_FLOAT),
        (T_DOUBLE, TK_DOUBLE),
        (T_STRING, TK_STRING),
        (T_MULTILINE_STRING, TK_MULTILINE_STRING),
        (T_MULTILINE_INTERP, TK_MULTILINE_INTERP),
        (T_INTERP, TK_INTERP),
        (T_REGEX_LITERAL, TK_REGEX_LITERAL),
        (T_REGEX_INTERP, TK_REGEX_INTERP),
        (T_ATTRIBUTE, TK_ATTRIBUTE),
        (T_LIDENT, TK_LIDENT),
        (T_UIDENT, TK_UIDENT),
        (T_POST_LABEL, TK_POST_LABEL),
        (T_COMMENT, TK_COMMENT),
        (T_NEWLINE, TK_NEWLINE),
        (T_INFIX1, TK_INFIX1),
        (T_INFIX2, TK_INFIX2),
        (T_INFIX3, TK_INFIX3),
        (T_INFIX4, TK_INFIX4),
        (T_AUGMENTED_ASSIGNMENT, TK_AUGMENTED_ASSIGNMENT),
        (T_PLUS_EQUAL, TK_PLUS_EQUAL),
        (T_EOF, TK_EOF),
        (T_FALSE, TK_FALSE),
        (T_TRUE, TK_TRUE),
        (T_PUB, TK_PUB),
        (T_PRIV, TK_PRIV),
        (T_READONLY, TK_READONLY),
        (T_IMPORT, TK_IMPORT),
        (T_EXTERN, TK_EXTERN),
        (T_BREAK, TK_BREAK),
        (T_CONTINUE, TK_CONTINUE),
        (T_STRUCT, TK_STRUCT),
        (T_ENUM, TK_ENUM),
        (T_TRAIT, TK_TRAIT),
        (T_DERIVE, TK_DERIVE),
        (T_IMPL, TK_IMPL),
        (T_WITH, TK_WITH),
        (T_RAISE, TK_RAISE),
        (T_THROW, TK_THROW),
        (T_TRY, TK_TRY),
        (T_CATCH, TK_CATCH),
        (T_ASYNC, TK_ASYNC),
        (T_TYPEALIAS, TK_TYPEALIAS),
        (T_TRAITALIAS, TK_TRAITALIAS),
        (T_FNALIAS, TK_FNALIAS),
        (T_EQUAL, TK_EQUAL),
        (T_EQ_TILDE, TK_EQ_TILDE),
        (T_LPAREN, TK_LPAREN),
        (T_RPAREN, TK_RPAREN),
        (T_COMMA, TK_COMMA),
        (T_MINUS, TK_MINUS),
        (T_QUESTION, TK_QUESTION),
        (T_EXCLAMATION, TK_EXCLAMATION),
        (T_DOT_LIDENT, TK_DOT_LIDENT),
        (T_DOT_UIDENT, TK_DOT_UIDENT),
        (T_DOT_INT, TK_DOT_INT),
        (T_DOT_LPAREN, TK_DOT_LPAREN),
        (T_COLONCOLON, TK_COLONCOLON),
        (T_COLON, TK_COLON),
        (T_SEMI, TK_SEMI),
        (T_LBRACKET, TK_LBRACKET),
        (T_PLUS, TK_PLUS),
        (T_RBRACKET, TK_RBRACKET),
        (T_UNDERSCORE, TK_UNDERSCORE),
        (T_BAR, TK_BAR),
        (T_LBRACE, TK_LBRACE),
        (T_RBRACE, TK_RBRACE),
        (T_AMPERAMPER, TK_AMPERAMPER),
        (T_AMPER, TK_AMPER),
        (T_CARET, TK_CARET),
        (T_BARBAR, TK_BARBAR),
        (T_PACKAGE_NAME, TK_PACKAGE_NAME),
        (T_AS, TK_AS),
        (T_PIPE, TK_PIPE),
        (T_PIPE_LEFT, TK_PIPE_LEFT),
        (T_LT_PLUS, TK_LT_PLUS),
        (T_ELSE, TK_ELSE),
        (T_FN, TK_FN),
        (T_IF, TK_IF),
        (T_LET, TK_LET),
        (T_PROOF_ASSERT, TK_PROOF_ASSERT),
        (T_PROOF_LET, TK_PROOF_LET),
        (T_CONST, TK_CONST),
        (T_MATCH, TK_MATCH),
        (T_MUTABLE, TK_MUTABLE),
        (T_TYPE, TK_TYPE),
        (T_FAT_ARROW, TK_FAT_ARROW),
        (T_THIN_ARROW, TK_THIN_ARROW),
        (T_WHILE, TK_WHILE),
        (T_RETURN, TK_RETURN),
        (T_DOTDOT, TK_DOTDOT),
        (T_RANGE_INCLUSIVE, TK_RANGE_INCLUSIVE),
        (T_RANGE_LT_INCLUSIVE, TK_RANGE_LT_INCLUSIVE),
        (T_RANGE_EXCLUSIVE, TK_RANGE_EXCLUSIVE),
        (T_RANGE_INCLUSIVE_REV, TK_RANGE_INCLUSIVE_REV),
        (T_RANGE_EXCLUSIVE_REV, TK_RANGE_EXCLUSIVE_REV),
        (T_ELLIPSIS, TK_ELLIPSIS),
        (T_TEST, TK_TEST),
        (T_LOOP, TK_LOOP),
        (T_GUARD, TK_GUARD),
        (T_DEFER, TK_DEFER),
        (T_FOR, TK_FOR),
        (T_IN, TK_IN),
        (T_IS, TK_IS),
        (T_SUBERROR, TK_SUBERROR),
        (T_AND, TK_AND),
        (T_FORALL, TK_FORALL),
        (T_EXISTS, TK_EXISTS),
        (T_IMPLIES, TK_IMPLIES),
        (T_LETREC, TK_LETREC),
        (T_USING, TK_USING),
        (T_DECLARE, TK_DECLARE),
        (T_NORAISE, TK_NORAISE),
        (T_NOBREAK, TK_NOBREAK),
        (T_WHERE, TK_WHERE),
        (T_ENUMVIEW, TK_ENUMVIEW),
        (T_EXTENUM, TK_EXTENUM),
        (T_TRY_QUESTION, TK_TRY_QUESTION),
        (T_TRY_EXCLAMATION, TK_TRY_EXCLAMATION),
        (T_LEXMATCH, TK_LEXMATCH),
        (T_LEXMATCH_QUESTION, TK_LEXMATCH_QUESTION),
        (T_PACKAGE, TK_PACKAGE),
      ] : Array[(YYSymbol, TokenKind)]) {
    try_add(term.0, term.1)
  }
  match token {
    None => raise UnexpectedEndOfInput(loc.1, expected)
    Some(token) => raise UnexpectedToken(token, loc, expected)
  }
}

///|
pub fn mbti(
  tokens : Array[(Token, @basic.Position, @basic.Position)],
  initial_pos? : @basic.Position,
) -> Mbti raise ParseError {
  yy_parse(
    tokens,
    yy_state_0,
    it => {
      guard it is YYObj_Mbti(result)
      result
    },
    initial_pos?,
  )
}

///|
fn _get_symbol_start_pos(
  args : ArrayView[(YYObj, @basic.Position, @basic.Position)],
  last_pos : @basic.Position,
) -> @basic.Position {
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
