
pub suberror ParseError {
  UnexpectedToken(Token, (@basic.Position, @basic.Position), Array[TokenKind])
  UnexpectedEndOfInput(@basic.Position, Array[TokenKind])
} derive(Debug)

type YYObj = Error

priv suberror YYObj_Void

priv suberror YYObj_Int {
  YYObj_Int(Int)
}

priv suberror YYObj_Sig {
  YYObj_Sig(Sig)
}

priv suberror YYObj_Bool {
  YYObj_Bool(Bool)
}

priv suberror YYObj_Mbti {
  YYObj_Mbti(Mbti)
}

priv suberror YYObj_Name {
  YYObj_Name(Name)
}

priv suberror YYObj_Type {
  YYObj_Type(Type)
}

priv suberror YYObj_Unit {
  YYObj_Unit(Unit)
}

priv suberror YYObj_Label {
  YYObj_Label(Label)
}

priv suberror YYObj_Option_Bool_ {
  YYObj_Option_Bool_(Option[Bool])
}

priv suberror YYObj_Option_Unit_ {
  YYObj_Option_Unit_(Option[Unit])
}

priv suberror YYObj_Option__list_List_Type__ {
  YYObj_Option__list_List_Type__(Option[@list.List[Type]])
}

priv suberror YYObj_Option__list_List_ConstrParam__ {
  YYObj_Option__list_List_ConstrParam__(Option[@list.List[ConstrParam]])
}

priv suberror YYObj_Option__String__Location__ {
  YYObj_Option__String__Location__(Option[(String, Location)])
}

priv suberror YYObj_String {
  YYObj_String(String)
}

priv suberror YYObj_Comment {
  YYObj_Comment(Comment)
}

priv suberror YYObj_FuncSig {
  YYObj_FuncSig(FuncSig)
}

priv suberror YYObj_ImplSig {
  YYObj_ImplSig(ImplSig)
}

priv suberror YYObj_TypeSig {
  YYObj_TypeSig(TypeSig)
}

priv suberror YYObj_AliasSig {
  YYObj_AliasSig(AliasSig)
}

priv suberror YYObj_ConstSig {
  YYObj_ConstSig(ConstSig)
}

priv suberror YYObj_Constant {
  YYObj_Constant(Constant)
}

priv suberror YYObj_ConstrId {
  YYObj_ConstrId(ConstrId)
}

priv suberror YYObj_TraitSig {
  YYObj_TraitSig(TraitSig)
}

priv suberror YYObj_ValueSig {
  YYObj_ValueSig(ValueSig)
}

priv suberror YYObj_Attribute {
  YYObj_Attribute(Attribute)
}

priv suberror YYObj_FieldDecl {
  YYObj_FieldDecl(FieldDecl)
}

priv suberror YYObj_Parameter {
  YYObj_Parameter(Parameter)
}

priv suberror YYObj_ConstrDecl {
  YYObj_ConstrDecl(ConstrDecl)
}

priv suberror YYObj_Visibility {
  YYObj_Visibility(Visibility)
}

priv suberror YYObj_CharLiteral {
  YYObj_CharLiteral(CharLiteral)
}

priv suberror YYObj_ConstrParam {
  YYObj_ConstrParam(ConstrParam)
}

priv suberror YYObj_InterpLiteral {
  YYObj_InterpLiteral(InterpLiteral)
}

priv suberror YYObj_PackageImport {
  YYObj_PackageImport(PackageImport)
}

priv suberror YYObj_QualifiedName {
  YYObj_QualifiedName(QualifiedName)
}

priv suberror YYObj_StringLiteral {
  YYObj_StringLiteral(StringLiteral)
}

priv suberror YYObj_MethodSelfType {
  YYObj_MethodSelfType(MethodSelfType)
}

priv suberror YYObj_TraitMethodSig {
  YYObj_TraitMethodSig(TraitMethodSig)
}

priv suberror YYObj_TypeDefinition {
  YYObj_TypeDefinition(TypeDefinition)
}

priv suberror YYObj_TypeParamNoConstraints {
  YYObj_TypeParamNoConstraints(TypeParamNoConstraints)
}

priv suberror YYObj_TypeParamWithConstraints {
  YYObj_TypeParamWithConstraints(TypeParamWithConstraints)
}

priv suberror YYObj__list_List_Type_ {
  YYObj__list_List_Type_(@list.List[Type])
}

priv suberror YYObj__list_List_Attribute_ {
  YYObj__list_List_Attribute_(@list.List[Attribute])
}

priv suberror YYObj__list_List_FieldDecl_ {
  YYObj__list_List_FieldDecl_(@list.List[FieldDecl])
}

priv suberror YYObj__list_List_Parameter_ {
  YYObj__list_List_Parameter_(@list.List[Parameter])
}

priv suberror YYObj__list_List_ConstrDecl_ {
  YYObj__list_List_ConstrDecl_(@list.List[ConstrDecl])
}

priv suberror YYObj__list_List_ConstrParam_ {
  YYObj__list_List_ConstrParam_(@list.List[ConstrParam])
}

priv suberror YYObj__list_List_PackageImport_ {
  YYObj__list_List_PackageImport_(@list.List[PackageImport])
}

priv suberror YYObj__list_List_QualifiedName_ {
  YYObj__list_List_QualifiedName_(@list.List[QualifiedName])
}

priv suberror YYObj__list_List_TraitMethodSig_ {
  YYObj__list_List_TraitMethodSig_(@list.List[TraitMethodSig])
}

priv suberror YYObj__list_List_TypeParamNoConstraints_ {
  YYObj__list_List_TypeParamNoConstraints_(@list.List[TypeParamNoConstraints])
}

priv suberror YYObj__list_List_TypeParamWithConstraints_ {
  YYObj__list_List_TypeParamWithConstraints_(@list.List[TypeParamWithConstraints])
}

priv suberror YYObj__list_List__Sig__Location__ {
  YYObj__list_List__Sig__Location__(@list.List[(Sig, Location)])
}

priv suberror YYObj_Name_ {
  YYObj_Name_(Name?)
}

priv suberror YYObj_String_ {
  YYObj_String_(String?)
}

priv suberror YYObj_Location_ {
  YYObj_Location_(Location?)
}

priv suberror YYObj__Name___list_List_TypeParamNoConstraints__ {
  YYObj__Name___list_List_TypeParamNoConstraints__((Name, @list.List[TypeParamNoConstraints]))
}

priv suberror YYObj__Type__ErrorType_ {
  YYObj__Type__ErrorType_((Type, ErrorType))
}

priv suberror YYObj__String__Location_ {
  YYObj__String__Location_((String, Location))
}

priv suberror YYObj__MethodSelfType___Name_ {
  YYObj__MethodSelfType___Name_((MethodSelfType?, Name))
}

priv suberror YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_ {
  YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_((Name, @list.List[TypeParamNoConstraints], QualifiedName))
}

priv suberror YYObj__String__Option_String___String_ {
  YYObj__String__Option_String___String_((String, Option[String], String))
}

type YYState = (YYSymbol) -> YYDecision

type YYAction = (@basic.Position, ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj

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
  T_DOUBLE
  T_STRING
  T_MULTILINE_STRING
  T_MULTILINE_INTERP
  T_INTERP
  T_BYTES_INTERP
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
  T_LBRACKET_BAR
  T_PLUS
  T_RBRACKET
  T_BAR_RBRACKET
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
  T_LT_QUESTION
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
  T_LEXSCAN
  T_EXTEND
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
fn init {
  match (EOI : YYSymbol) {
    EOI => ()
    _ => ()
  }
}

// file:///./mbti_parser.mbty
// 149|    PACKAGE STRING ioption(";") imports sigs EOF { {package_name: $2, imports: $4, sigs: $5, loc : mk_loc($sloc), base_pos: mk_base_pos($loc($1).0)} }
fn yy_action_0(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_1(_args[1].2, _args[2:2])
  guard _args[1].0 is YYObj_StringLiteral(_dollar2)
  guard _args[2].0 is YYObj__list_List_PackageImport_(_dollar4)
  guard _args[3].0 is YYObj__list_List__Sig__Location__(_dollar5)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  YYObj_Mbti({();  {package_name: _dollar2, imports: _dollar4, sigs: _dollar5, loc : mk_loc((_symbol_start_pos, _end_pos)), base_pos: mk_base_pos((_start_pos_of_item0, _end_pos_of_item0).0)} })
}

// file:///./stdlib.mbty
// 9|    { None }
fn yy_action_1(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Option_Bool_({();  None })
}

// file:///./mbti_parser.mbty
// 149|    PACKAGE STRING ioption(";") imports sigs EOF { {package_name: $2, imports: $4, sigs: $5, loc : mk_loc($sloc), base_pos: mk_base_pos($loc($1).0)} }
fn yy_action_2(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_3(_args[1].2, _args[2:3])
  guard _args[1].0 is YYObj_StringLiteral(_dollar2)
  guard _args[3].0 is YYObj__list_List_PackageImport_(_dollar4)
  guard _args[4].0 is YYObj__list_List__Sig__Location__(_dollar5)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  YYObj_Mbti({();  {package_name: _dollar2, imports: _dollar4, sigs: _dollar5, loc : mk_loc((_symbol_start_pos, _end_pos)), base_pos: mk_base_pos((_start_pos_of_item0, _end_pos_of_item0).0)} })
}

// file:///./stdlib.mbty
// 10|    X { Some($1) }
fn yy_action_3(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  YYObj_Option_Bool_({();  Some(_dollar1) })
}

// file:///./mbti_parser.mbty
// 165|    sig_ ";" sigs { $3.add(($1, mk_loc($loc($1)))) }
fn yy_action_4(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List__Sig__Location__(_dollar3)
  guard _args[0].0 is YYObj_Sig(_dollar1)
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  YYObj__list_List__Sig__Location__({();  _dollar3.add((_dollar1, mk_loc((_start_pos_of_item0, _end_pos_of_item0)))) })
}

// file:///./mbti_parser.mbty
// 164|    sig_ { @list.singleton(($1, mk_loc($sloc))) }
fn yy_action_5(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Sig(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj__list_List__Sig__Location__({();  @list.singleton((_dollar1, mk_loc((_symbol_start_pos, _end_pos)))) })
}

// file:///./mbti_parser.mbty
// 169|    func_sig { Func($1) }
fn yy_action_6(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FuncSig(_dollar1)
  YYObj_Sig({();  Func(_dollar1) })
}

// file:///./mbti_parser.mbty
// 170|    type_sig { Type($1) }
fn yy_action_7(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeSig(_dollar1)
  YYObj_Sig({();  Type(_dollar1) })
}

// file:///./mbti_parser.mbty
// 171|    alias_sig { Alias($1) }
fn yy_action_8(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_AliasSig(_dollar1)
  YYObj_Sig({();  Alias(_dollar1) })
}

// file:///./mbti_parser.mbty
// 172|    trait_sig { Trait($1) }
fn yy_action_9(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitSig(_dollar1)
  YYObj_Sig({();  Trait(_dollar1) })
}

// file:///./mbti_parser.mbty
// 173|    impl_sig { Impl($1) }
fn yy_action_10(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ImplSig(_dollar1)
  YYObj_Sig({();  Impl(_dollar1) })
}

// file:///./mbti_parser.mbty
// 174|    const_sig { Const($1) }
fn yy_action_11(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstSig(_dollar1)
  YYObj_Sig({();  Const(_dollar1) })
}

// file:///./mbti_parser.mbty
// 175|    value_sig { Value($1) }
fn yy_action_12(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ValueSig(_dollar1)
  YYObj_Sig({();  Value(_dollar1) })
}

// file:///./mbti_parser.mbty
// 179|    attributes vis "const" uident ":" type_ "=" constant { {name: $4, type_: $6, value: $8, attrs: $1} }
fn yy_action_13(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj_Type(_dollar6)
  guard _args[5].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig({();  {name: _dollar4, type_: _dollar6, value: _dollar8, attrs: _dollar1} })
}

// file:///./mbti_parser.mbty
// 179|    attributes vis "const" uident ":" type_ "=" constant { {name: $4, type_: $6, value: $8, attrs: $1} }
fn yy_action_16(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  guard _args[6].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig({();  {name: _dollar4, type_: _dollar6, value: _dollar8, attrs: _dollar1} })
}

// file:///./mbti_parser.mbty
// 183|    attributes vis "let" lident ":" type_ { { attrs: $1, name: $4, type_: $6 } }
fn yy_action_18(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig({();  { attrs: _dollar1, name: _dollar4, type_: _dollar6 } })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_19(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_22(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "type" type_decl_name_with_params optional_derive {
// 233|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5}
// 234|  }
fn yy_action_24(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Abstract, derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 290|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 291|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6)
// 292|  }
fn yy_action_25(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig({(); 
    TypeAlias(attrs = _dollar1, vis=_dollar2, name=_dollar4.0, type_params=_dollar4.1, type_=_dollar6)
  })
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "suberror" uident optional_derive {
// 236|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 237|  }
fn yy_action_26(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(NoPayload), derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 240|  }
fn yy_action_27(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(EnumPayload(_dollar6)), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 242|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 243|  }
fn yy_action_28(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: _dollar6, derives: _dollar8 }
  })
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 245|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 246|  }
fn yy_action_29(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: TupleStruct(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 247|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 248|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 249|  }
fn yy_action_30(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Variant(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 250|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 251|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 252|  }
fn yy_action_31(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: ExtensibleEnum(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 253|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 254|    let (name, type_params, target) = $4
// 255|    {attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 256|  }
fn yy_action_32(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig({(); 
    let (name, type_params, target) = _dollar4
    {attrs : _dollar1, vis: _dollar2, name, type_params, components: ExtendEnum(target~, constructors = _dollar7), derives: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 266|    {type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 267|  }
fn yy_action_33(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[3].0 is YYObj_QualifiedName(_dollar5)
  guard _args[5].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : _dollar4, trait_name : _dollar5, type_ : _dollar7, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 268|    attributes vis "impl" qualified_uident "for" type_ {
// 269|    {type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 270|  }
fn yy_action_34(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_QualifiedName(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : @list.empty(), trait_name : _dollar4, type_ : _dollar6, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 274|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body {
// 275|    { attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6}
// 276|  }
fn yy_action_35(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[4].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({(); 
    { attrs : _dollar1, vis: _dollar2, name: _dollar4, super_traits: _dollar5, methods: _dollar6}
  })
}

// file:///./mbti_parser.mbty
// 293|    attributes vis "fnalias" uident "::" lident {
// 294|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 295|  }
fn yy_action_36(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar6)
  guard _args[2].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig({(); 
    FuncAlias(attrs = _dollar1,name=_dollar6, type_name=_dollar4)
  })
}

// file:///./mbti_parser.mbty
// 296|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 297|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 298|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 299|  }
fn yy_action_37(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[2].1
  let _end_pos_of_item3 = _args[2].2
  guard _args[5].0 is YYObj_Name(_dollar7)
  guard _args[6].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Type, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 300|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 301|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 302|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 303|  }
fn yy_action_38(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_17(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[2].1
  let _end_pos_of_item3 = _args[2].2
  guard _args[5].0 is YYObj_Name(_dollar7)
  guard _args[6].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Trait, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 179|    attributes vis "const" uident ":" type_ "=" constant { {name: $4, type_: $6, value: $8, attrs: $1} }
fn yy_action_39(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  guard _args[7].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig({();  {name: _dollar4, type_: _dollar6, value: _dollar8, attrs: _dollar1} })
}

// file:///./mbti_parser.mbty
// 183|    attributes vis "let" lident ":" type_ { { attrs: $1, name: $4, type_: $6 } }
fn yy_action_41(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig({();  { attrs: _dollar1, name: _dollar4, type_: _dollar6 } })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_42(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_43(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "type" type_decl_name_with_params optional_derive {
// 233|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5}
// 234|  }
fn yy_action_44(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Abstract, derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 290|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 291|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6)
// 292|  }
fn yy_action_45(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig({(); 
    TypeAlias(attrs = _dollar1, vis=_dollar2, name=_dollar4.0, type_params=_dollar4.1, type_=_dollar6)
  })
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "suberror" uident optional_derive {
// 236|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 237|  }
fn yy_action_46(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(NoPayload), derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 240|  }
fn yy_action_47(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(EnumPayload(_dollar6)), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 242|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 243|  }
fn yy_action_48(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: _dollar6, derives: _dollar8 }
  })
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 245|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 246|  }
fn yy_action_49(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: TupleStruct(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 247|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 248|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 249|  }
fn yy_action_50(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Variant(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 250|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 251|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 252|  }
fn yy_action_51(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: ExtensibleEnum(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 253|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 254|    let (name, type_params, target) = $4
// 255|    {attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 256|  }
fn yy_action_52(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[6].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig({(); 
    let (name, type_params, target) = _dollar4
    {attrs : _dollar1, vis: _dollar2, name, type_params, components: ExtendEnum(target~, constructors = _dollar7), derives: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 266|    {type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 267|  }
fn yy_action_53(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[4].0 is YYObj_QualifiedName(_dollar5)
  guard _args[6].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : _dollar4, trait_name : _dollar5, type_ : _dollar7, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 268|    attributes vis "impl" qualified_uident "for" type_ {
// 269|    {type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 270|  }
fn yy_action_54(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj_QualifiedName(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : @list.empty(), trait_name : _dollar4, type_ : _dollar6, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 274|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body {
// 275|    { attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6}
// 276|  }
fn yy_action_55(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[5].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({(); 
    { attrs : _dollar1, vis: _dollar2, name: _dollar4, super_traits: _dollar5, methods: _dollar6}
  })
}

// file:///./mbti_parser.mbty
// 293|    attributes vis "fnalias" uident "::" lident {
// 294|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 295|  }
fn yy_action_56(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[5].0 is YYObj_Name(_dollar6)
  guard _args[3].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig({(); 
    FuncAlias(attrs = _dollar1,name=_dollar6, type_name=_dollar4)
  })
}

// file:///./mbti_parser.mbty
// 296|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 297|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 298|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 299|  }
fn yy_action_57(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[3].1
  let _end_pos_of_item3 = _args[3].2
  guard _args[6].0 is YYObj_Name(_dollar7)
  guard _args[7].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Type, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 300|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 301|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 302|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 303|  }
fn yy_action_58(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_40(_last_pos, _args[0:2])
  guard _args[3].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[3].1
  let _end_pos_of_item3 = _args[3].2
  guard _args[6].0 is YYObj_Name(_dollar7)
  guard _args[7].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Trait, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 179|    attributes vis "const" uident ":" type_ "=" constant { {name: $4, type_: $6, value: $8, attrs: $1} }
fn yy_action_59(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  guard _args[6].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig({();  {name: _dollar4, type_: _dollar6, value: _dollar8, attrs: _dollar1} })
}

// file:///./mbti_parser.mbty
// 179|    attributes vis "const" uident ":" type_ "=" constant { {name: $4, type_: $6, value: $8, attrs: $1} }
fn yy_action_61(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  guard _args[7].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig({();  {name: _dollar4, type_: _dollar6, value: _dollar8, attrs: _dollar1} })
}

// file:///./mbti_parser.mbty
// 183|    attributes vis "let" lident ":" type_ { { attrs: $1, name: $4, type_: $6 } }
fn yy_action_62(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig({();  { attrs: _dollar1, name: _dollar4, type_: _dollar6 } })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_63(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_64(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "type" type_decl_name_with_params optional_derive {
// 233|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5}
// 234|  }
fn yy_action_65(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Abstract, derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 290|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 291|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6)
// 292|  }
fn yy_action_66(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig({(); 
    TypeAlias(attrs = _dollar1, vis=_dollar2, name=_dollar4.0, type_params=_dollar4.1, type_=_dollar6)
  })
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "suberror" uident optional_derive {
// 236|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 237|  }
fn yy_action_67(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(NoPayload), derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 240|  }
fn yy_action_68(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(EnumPayload(_dollar6)), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 242|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 243|  }
fn yy_action_69(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: _dollar6, derives: _dollar8 }
  })
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 245|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 246|  }
fn yy_action_70(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: TupleStruct(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 247|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 248|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 249|  }
fn yy_action_71(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Variant(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 250|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 251|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 252|  }
fn yy_action_72(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: ExtensibleEnum(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 253|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 254|    let (name, type_params, target) = $4
// 255|    {attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 256|  }
fn yy_action_73(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[6].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig({(); 
    let (name, type_params, target) = _dollar4
    {attrs : _dollar1, vis: _dollar2, name, type_params, components: ExtendEnum(target~, constructors = _dollar7), derives: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 266|    {type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 267|  }
fn yy_action_74(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[4].0 is YYObj_QualifiedName(_dollar5)
  guard _args[6].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : _dollar4, trait_name : _dollar5, type_ : _dollar7, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 268|    attributes vis "impl" qualified_uident "for" type_ {
// 269|    {type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 270|  }
fn yy_action_75(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj_QualifiedName(_dollar4)
  guard _args[5].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : @list.empty(), trait_name : _dollar4, type_ : _dollar6, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 274|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body {
// 275|    { attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6}
// 276|  }
fn yy_action_76(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[3].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[5].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({(); 
    { attrs : _dollar1, vis: _dollar2, name: _dollar4, super_traits: _dollar5, methods: _dollar6}
  })
}

// file:///./mbti_parser.mbty
// 293|    attributes vis "fnalias" uident "::" lident {
// 294|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 295|  }
fn yy_action_77(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[5].0 is YYObj_Name(_dollar6)
  guard _args[3].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig({(); 
    FuncAlias(attrs = _dollar1,name=_dollar6, type_name=_dollar4)
  })
}

// file:///./mbti_parser.mbty
// 296|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 297|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 298|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 299|  }
fn yy_action_78(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[3].1
  let _end_pos_of_item3 = _args[3].2
  guard _args[6].0 is YYObj_Name(_dollar7)
  guard _args[7].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Type, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 300|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 301|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 302|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 303|  }
fn yy_action_79(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_17(_args[0].2, _args[1:2])
  guard _args[3].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[3].1
  let _end_pos_of_item3 = _args[3].2
  guard _args[6].0 is YYObj_Name(_dollar7)
  guard _args[7].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Trait, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 435|    "priv" { Priv(loc=mk_loc($sloc)) }
fn yy_action_17(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Visibility({();  Priv(loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 179|    attributes vis "const" uident ":" type_ "=" constant { {name: $4, type_: $6, value: $8, attrs: $1} }
fn yy_action_80(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[6].0 is YYObj_Type(_dollar6)
  guard _args[8].0 is YYObj_Constant(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstSig({();  {name: _dollar4, type_: _dollar6, value: _dollar8, attrs: _dollar1} })
}

// file:///./mbti_parser.mbty
// 419|    TRUE { Bool(true) }
fn yy_action_81(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Constant({();  Bool(true) })
}

// file:///./mbti_parser.mbty
// 420|    FALSE { Bool(false) }
fn yy_action_82(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Constant({();  Bool(false) })
}

// file:///./mbti_parser.mbty
// 421|    BYTE { Byte($1) }
fn yy_action_83(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_CharLiteral(_dollar1)
  YYObj_Constant({();  Byte(_dollar1) })
}

// file:///./mbti_parser.mbty
// 422|    BYTES { Bytes($1) }
fn yy_action_84(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_Constant({();  Bytes(_dollar1) })
}

// file:///./mbti_parser.mbty
// 423|    CHAR { Char($1) }
fn yy_action_85(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_CharLiteral(_dollar1)
  YYObj_Constant({();  Char(_dollar1) })
}

// file:///./mbti_parser.mbty
// 424|    INT { make_int_literal($1) }
fn yy_action_86(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj_Constant({();  make_int_literal(_dollar1) })
}

// file:///./mbti_parser.mbty
// 425|    "-" INT { make_int_literal("-" + $2) }
fn yy_action_87(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  YYObj_Constant({();  make_int_literal("-" + _dollar2) })
}

// file:///./mbti_parser.mbty
// 427|    "-" FLOAT { make_float_literal("-" + $2) }
fn yy_action_88(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  YYObj_Constant({();  make_float_literal("-" + _dollar2) })
}

// file:///./mbti_parser.mbty
// 429|    "-" DOUBLE { Double("-" + $2) }
fn yy_action_89(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  YYObj_Constant({();  Double("-" + _dollar2) })
}

// file:///./mbti_parser.mbty
// 426|    FLOAT { make_float_literal($1) }
fn yy_action_90(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj_Constant({();  make_float_literal(_dollar1) })
}

// file:///./mbti_parser.mbty
// 428|    DOUBLE { Double($1) }
fn yy_action_91(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  YYObj_Constant({();  Double(_dollar1) })
}

// file:///./mbti_parser.mbty
// 430|    STRING { String($1) }
fn yy_action_92(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_Constant({();  String(_dollar1) })
}

// file:///./mbti_parser.mbty
// 183|    attributes vis "let" lident ":" type_ { { attrs: $1, name: $4, type_: $6 } }
fn yy_action_93(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[6].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig({();  { attrs: _dollar1, name: _dollar4, type_: _dollar6 } })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_94(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_95(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "type" type_decl_name_with_params optional_derive {
// 233|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5}
// 234|  }
fn yy_action_96(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Abstract, derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 290|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 291|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6)
// 292|  }
fn yy_action_97(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[6].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig({(); 
    TypeAlias(attrs = _dollar1, vis=_dollar2, name=_dollar4.0, type_params=_dollar4.1, type_=_dollar6)
  })
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "suberror" uident optional_derive {
// 236|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 237|  }
fn yy_action_98(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(NoPayload), derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 240|  }
fn yy_action_99(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[6].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(EnumPayload(_dollar6)), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 242|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 243|  }
fn yy_action_100(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[6].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: _dollar6, derives: _dollar8 }
  })
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 245|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 246|  }
fn yy_action_101(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[6].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: TupleStruct(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 247|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 248|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 249|  }
fn yy_action_102(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[6].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Variant(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 250|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 251|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 252|  }
fn yy_action_103(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[6].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[8].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: ExtensibleEnum(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 253|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 254|    let (name, type_params, target) = $4
// 255|    {attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 256|  }
fn yy_action_104(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[7].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[9].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig({(); 
    let (name, type_params, target) = _dollar4
    {attrs : _dollar1, vis: _dollar2, name, type_params, components: ExtendEnum(target~, constructors = _dollar7), derives: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 266|    {type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 267|  }
fn yy_action_105(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[5].0 is YYObj_QualifiedName(_dollar5)
  guard _args[7].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : _dollar4, trait_name : _dollar5, type_ : _dollar7, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 268|    attributes vis "impl" qualified_uident "for" type_ {
// 269|    {type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 270|  }
fn yy_action_106(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj_QualifiedName(_dollar4)
  guard _args[6].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : @list.empty(), trait_name : _dollar4, type_ : _dollar6, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 274|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body {
// 275|    { attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6}
// 276|  }
fn yy_action_107(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj_Name(_dollar4)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[6].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({(); 
    { attrs : _dollar1, vis: _dollar2, name: _dollar4, super_traits: _dollar5, methods: _dollar6}
  })
}

// file:///./mbti_parser.mbty
// 293|    attributes vis "fnalias" uident "::" lident {
// 294|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 295|  }
fn yy_action_108(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[6].0 is YYObj_Name(_dollar6)
  guard _args[4].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig({(); 
    FuncAlias(attrs = _dollar1,name=_dollar6, type_name=_dollar4)
  })
}

// file:///./mbti_parser.mbty
// 296|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 297|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 298|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 299|  }
fn yy_action_109(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[4].1
  let _end_pos_of_item3 = _args[4].2
  guard _args[7].0 is YYObj_Name(_dollar7)
  guard _args[8].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Type, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 300|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 301|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 302|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 303|  }
fn yy_action_110(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_40(_args[0].2, _args[1:3])
  guard _args[4].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[4].1
  let _end_pos_of_item3 = _args[4].2
  guard _args[7].0 is YYObj_Name(_dollar7)
  guard _args[8].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Trait, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 436|    "pub" pub_attr { Pub(attr=$2, loc=mk_loc($sloc)) }
fn yy_action_40(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String_(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Visibility({();  Pub(attr=_dollar2, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 441|    "(" "readonly" ")" { Some("readonly") }
fn yy_action_111(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_String_({();  Some("readonly") })
}

// file:///./mbti_parser.mbty
// 442|    "(" lident ")" { Some($2.name) }
fn yy_action_112(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  YYObj_String_({();  Some(_dollar2.name) })
}

// file:///./mbti_parser.mbty
// 440|    { None }
fn yy_action_113(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_String_({();  None })
}

// file:///./mbti_parser.mbty
// 183|    attributes vis "let" lident ":" type_ { { attrs: $1, name: $4, type_: $6 } }
fn yy_action_114(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig({();  { attrs: _dollar1, name: _dollar4, type_: _dollar6 } })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_115(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_116(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "type" type_decl_name_with_params optional_derive {
// 233|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5}
// 234|  }
fn yy_action_117(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Abstract, derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 290|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 291|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6)
// 292|  }
fn yy_action_118(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig({(); 
    TypeAlias(attrs = _dollar1, vis=_dollar2, name=_dollar4.0, type_params=_dollar4.1, type_=_dollar6)
  })
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "suberror" uident optional_derive {
// 236|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 237|  }
fn yy_action_119(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(NoPayload), derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 240|  }
fn yy_action_120(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(EnumPayload(_dollar6)), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 242|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 243|  }
fn yy_action_121(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: _dollar6, derives: _dollar8 }
  })
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 245|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 246|  }
fn yy_action_122(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: TupleStruct(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 247|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 248|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 249|  }
fn yy_action_123(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Variant(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 250|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 251|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 252|  }
fn yy_action_124(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: ExtensibleEnum(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 253|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 254|    let (name, type_params, target) = $4
// 255|    {attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 256|  }
fn yy_action_125(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[5].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[7].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig({(); 
    let (name, type_params, target) = _dollar4
    {attrs : _dollar1, vis: _dollar2, name, type_params, components: ExtendEnum(target~, constructors = _dollar7), derives: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 266|    {type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 267|  }
fn yy_action_126(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[3].0 is YYObj_QualifiedName(_dollar5)
  guard _args[5].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : _dollar4, trait_name : _dollar5, type_ : _dollar7, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 268|    attributes vis "impl" qualified_uident "for" type_ {
// 269|    {type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 270|  }
fn yy_action_127(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj_QualifiedName(_dollar4)
  guard _args[4].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : @list.empty(), trait_name : _dollar4, type_ : _dollar6, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 274|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body {
// 275|    { attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6}
// 276|  }
fn yy_action_128(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[4].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({(); 
    { attrs : _dollar1, vis: _dollar2, name: _dollar4, super_traits: _dollar5, methods: _dollar6}
  })
}

// file:///./mbti_parser.mbty
// 293|    attributes vis "fnalias" uident "::" lident {
// 294|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 295|  }
fn yy_action_129(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[4].0 is YYObj_Name(_dollar6)
  guard _args[2].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig({(); 
    FuncAlias(attrs = _dollar1,name=_dollar6, type_name=_dollar4)
  })
}

// file:///./mbti_parser.mbty
// 296|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 297|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 298|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 299|  }
fn yy_action_130(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[2].1
  let _end_pos_of_item3 = _args[2].2
  guard _args[5].0 is YYObj_Name(_dollar7)
  guard _args[6].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Type, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 300|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 301|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 302|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 303|  }
fn yy_action_131(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_15(_args[0].2, _args[1:1])
  guard _args[2].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[2].1
  let _end_pos_of_item3 = _args[2].2
  guard _args[5].0 is YYObj_Name(_dollar7)
  guard _args[6].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Trait, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 183|    attributes vis "let" lident ":" type_ { { attrs: $1, name: $4, type_: $6 } }
fn yy_action_132(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[1].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj_Type(_dollar6)
  YYObj_ValueSig({();  { attrs: _dollar1, name: _dollar4, type_: _dollar6 } })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_133(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 187|   UIDENT "::" {
// 188|  { name : $1, is_object : false, loc : mk_loc($loc($1)) }
// 189| }
fn yy_action_134(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  YYObj_MethodSelfType({(); 
  { name : _dollar1, is_object : false, loc : mk_loc((_start_pos_of_item0, _end_pos_of_item0)) }
 })
}

// file:///./mbti_parser.mbty
// 190|   "&" UIDENT "::" {
// 191|  { name : $2, is_object : true, loc : mk_loc($loc($2)) }
// 192| }
fn yy_action_135(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj_MethodSelfType({(); 
  { name : _dollar2, is_object : true, loc : mk_loc((_start_pos_of_item1, _end_pos_of_item1)) }
 })
}

// file:///./mbti_parser.mbty
// 201|    luident { (None, $1) }
fn yy_action_136(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj__MethodSelfType___Name_({();  (None, _dollar1) })
}

// file:///./mbti_parser.mbty
// 196|    lident { $1 }
fn yy_action_137(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_Name({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 197|    uident { $1 }
fn yy_action_138(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_Name({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 202|    method_self_type_coloncolon luident { (Some($1), $2) }
fn yy_action_139(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_MethodSelfType(_dollar1)
  guard _args[1].0 is YYObj_Name(_dollar2)
  YYObj__MethodSelfType___Name_({();  (Some(_dollar1), _dollar2) })
}

// file:///./mbti_parser.mbty
// 212|    attributes vis is_async FN loption(type_params_with_constraints) func_name delimited("(", separated_list(",", parameter), ")") "->" return_type {
// 213|    let (type_name, name) = $6
// 214|    { attrs: $1, is_async : $3, type_name, name, type_params: $5, params: $7, return_: $9}
// 215|  }
fn yy_action_140(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
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
  YYObj_FuncSig({(); 
    let (type_name, name) = _dollar6
    { attrs: _dollar1, is_async : _dollar3, type_name, name, type_params: _dollar5, params: _dollar7, return_: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 232|    attributes vis "type" type_decl_name_with_params optional_derive {
// 233|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Abstract, derives: $5}
// 234|  }
fn yy_action_141(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Abstract, derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 290|    attributes vis "type"  type_decl_name_with_params "=" type_ {
// 291|    TypeAlias(attrs = $1, vis=$2, name=$4.0, type_params=$4.1, type_=$6)
// 292|  }
fn yy_action_142(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj_Type(_dollar6)
  YYObj_AliasSig({(); 
    TypeAlias(attrs = _dollar1, vis=_dollar2, name=_dollar4.0, type_params=_dollar4.1, type_=_dollar6)
  })
}

// file:///./mbti_parser.mbty
// 235|    attributes vis "suberror" uident optional_derive {
// 236|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(NoPayload), derives: $5}
// 237|  }
fn yy_action_143(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj_Name(_dollar4)
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar5)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(NoPayload), derives: _dollar5}
  })
}

// file:///./mbti_parser.mbty
// 238|    attributes vis "suberror" uident "{" separated_list(";", enum_constructor) "}" optional_derive {
// 239|    {attrs : $1, vis: $2, name: $4, type_params: @list.empty(), components: Error(EnumPayload($6)), derives: $8}
// 240|  }
fn yy_action_144(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj_Name(_dollar4)
  guard _args[3].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4, type_params: @list.empty(), components: Error(EnumPayload(_dollar6)), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 241|    attributes vis "struct" type_decl_name_with_params "{" record_decl_body "}" optional_derive {
// 242|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: $6, derives: $8 }
// 243|  }
fn yy_action_145(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj_TypeDefinition(_dollar6)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: _dollar6, derives: _dollar8 }
  })
}

// file:///./mbti_parser.mbty
// 328|    separated_list(";", record_decl_field) {
// 329|    Record(fields = $1)
// 330|  }
fn yy_action_146(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_FieldDecl_(_dollar1)
  YYObj_TypeDefinition({(); 
    Record(fields = _dollar1)
  })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_147(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_FieldDecl_(_dollar1)
  YYObj__list_List_FieldDecl_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 322|    attributes boption("mut") lident ":" type_ {
// 323|    { name: { label: $3.name, loc : $3.loc }, vis: Default, mut_: $2, ty: $5, loc: mk_loc($sloc), attrs: $1, doc: DocString::empty() }
// 324|  }
fn yy_action_148(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_Name(_dollar3)
  guard _args[0].0 is YYObj_Bool(_dollar2)
  guard _args[3].0 is YYObj_Type(_dollar5)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_FieldDecl({(); 
    { name: { label: _dollar3.name, loc : _dollar3.loc }, vis: Default, mut_: _dollar2, ty: _dollar5, loc: mk_loc((_symbol_start_pos, _end_pos)), attrs: _dollar1, doc: DocString::empty() }
  })
}

// file:///./mbti_parser.mbty
// 322|    attributes boption("mut") lident ":" type_ {
// 323|    { name: { label: $3.name, loc : $3.loc }, vis: Default, mut_: $2, ty: $5, loc: mk_loc($sloc), attrs: $1, doc: DocString::empty() }
// 324|  }
fn yy_action_149(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_Name(_dollar3)
  guard _args[1].0 is YYObj_Bool(_dollar2)
  guard _args[4].0 is YYObj_Type(_dollar5)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_FieldDecl({(); 
    { name: { label: _dollar3.name, loc : _dollar3.loc }, vis: Default, mut_: _dollar2, ty: _dollar5, loc: mk_loc((_symbol_start_pos, _end_pos)), attrs: _dollar1, doc: DocString::empty() }
  })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }
fn yy_action_150(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_FieldDecl_(_dollar3)
  guard _args[0].0 is YYObj_FieldDecl(_dollar1)
  YYObj__list_List_FieldDecl_({();  _dollar3.add(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }
fn yy_action_151(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_FieldDecl(_dollar1)
  YYObj__list_List_FieldDecl_({();  @list.singleton(_dollar1) })
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }
fn yy_action_152(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_FieldDecl_({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 244|    attributes vis "struct" type_decl_name_with_params "(" separated_list(",", type_) ")" optional_derive {
// 245|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: TupleStruct($6), derives: $8}
// 246|  }
fn yy_action_153(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj__list_List_Type_(_dollar6)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: TupleStruct(_dollar6), derives: _dollar8}
  })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_154(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_Type_(_dollar1)
  YYObj__list_List_Type_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }
fn yy_action_155(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_Type_({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 247|    attributes vis "enum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 248|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: Variant($6), derives: $8}
// 249|  }
fn yy_action_156(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: Variant(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 250|    attributes vis "extenum" type_decl_name_with_params "{" separated_list(";", enum_constructor) "}" optional_derive {
// 251|    {attrs : $1, vis: $2, name: $4.0, type_params: $4.1, components: ExtensibleEnum($6), derives: $8}
// 252|  }
fn yy_action_157(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints__(_dollar4)
  guard _args[3].0 is YYObj__list_List_ConstrDecl_(_dollar6)
  guard _args[5].0 is YYObj__list_List_QualifiedName_(_dollar8)
  YYObj_TypeSig({(); 
    {attrs : _dollar1, vis: _dollar2, name: _dollar4.0, type_params: _dollar4.1, components: ExtensibleEnum(_dollar6), derives: _dollar8}
  })
}

// file:///./mbti_parser.mbty
// 253|    attributes vis "extenum" extensible_enum_extension_target "+=" "{" separated_list(";", enum_constructor) "}" optional_derive {
// 254|    let (name, type_params, target) = $4
// 255|    {attrs : $1, vis: $2, name, type_params, components: ExtendEnum(target~, constructors = $7), derives: $9}
// 256|  }
fn yy_action_158(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_(_dollar4)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[4].0 is YYObj__list_List_ConstrDecl_(_dollar7)
  guard _args[6].0 is YYObj__list_List_QualifiedName_(_dollar9)
  YYObj_TypeSig({(); 
    let (name, type_params, target) = _dollar4
    {attrs : _dollar1, vis: _dollar2, name, type_params, components: ExtendEnum(target~, constructors = _dollar7), derives: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 261|    "derive" "(" non_empty_list_commas(qualified_uident) ")" { $3 }
fn yy_action_159(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar3)
  YYObj__list_List_QualifiedName_({();  _dollar3 })
}

// file:///./mbti_parser.mbty
// 145|    non_empty_list_commas_rev(X) option(",") { $1.rev() }
fn yy_action_160(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_QualifiedName_(_dollar1)
  YYObj__list_List_QualifiedName_({();  _dollar1.rev() })
}

// file:///./mbti_parser.mbty
// 141|    non_empty_list_commas_rev(X) "," X { $1.add($3) }
fn yy_action_161(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_QualifiedName_(_dollar1)
  guard _args[2].0 is YYObj_QualifiedName(_dollar3)
  YYObj__list_List_QualifiedName_({();  _dollar1.add(_dollar3) })
}

// file:///./mbti_parser.mbty
// 140|    X { @list.singleton($1) }
fn yy_action_162(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__list_List_QualifiedName_({();  @list.singleton(_dollar1) })
}

// file:///./mbti_parser.mbty
// 260|    { @list.empty() }
fn yy_action_163(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_QualifiedName_({();  @list.empty() })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_164(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_ConstrDecl_(_dollar1)
  YYObj__list_List_ConstrDecl_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 307|    attributes UIDENT option(delimited("(", separated_nonempty_list(",", constructor_param), ")")) option(eq_tag) {
// 308|    {name: { name : $2, loc : mk_loc($loc($2)) }, args: $3, tag: $4, loc: mk_loc($sloc), attrs: $1, doc: DocString::empty()}
// 309|  }
fn yy_action_165(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  guard _args[0].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[0].1
  let _end_pos_of_item1 = _args[0].2
  guard _args[1].0 is YYObj_Option__list_List_ConstrParam__(_dollar3)
  guard _args[2].0 is YYObj_Option__String__Location__(_dollar4)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstrDecl({(); 
    {name: { name : _dollar2, loc : mk_loc((_start_pos_of_item1, _end_pos_of_item1)) }, args: _dollar3, tag: _dollar4, loc: mk_loc((_symbol_start_pos, _end_pos)), attrs: _dollar1, doc: DocString::empty()}
  })
}

// file:///./mbti_parser.mbty
// 307|    attributes UIDENT option(delimited("(", separated_nonempty_list(",", constructor_param), ")")) option(eq_tag) {
// 308|    {name: { name : $2, loc : mk_loc($loc($2)) }, args: $3, tag: $4, loc: mk_loc($sloc), attrs: $1, doc: DocString::empty()}
// 309|  }
fn yy_action_166(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  guard _args[1].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  guard _args[2].0 is YYObj_Option__list_List_ConstrParam__(_dollar3)
  guard _args[3].0 is YYObj_Option__String__Location__(_dollar4)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ConstrDecl({(); 
    {name: { name : _dollar2, loc : mk_loc((_start_pos_of_item1, _end_pos_of_item1)) }, args: _dollar3, tag: _dollar4, loc: mk_loc((_symbol_start_pos, _end_pos)), attrs: _dollar1, doc: DocString::empty()}
  })
}

// file:///./stdlib.mbty
// 5|    X { Some($1) }
fn yy_action_167(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_168(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__String__Location_(_dollar1)
  YYObj_Option__String__Location__({();  Some(_dollar1) })
}

// file:///./mbti_parser.mbty
// 313|    "=" INT { ($2, mk_loc($loc($2))) }
fn yy_action_168(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj__String__Location_({();  (_dollar2, mk_loc((_start_pos_of_item1, _end_pos_of_item1))) })
}

// file:///./stdlib.mbty
// 4|    { None }
fn yy_action_169(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Option__String__Location__({();  None })
}

// file:///./mbti_parser.mbty
// 317|    boption("mut") type_ { {mut_: $1, ty: $2, label: None} }
fn yy_action_170(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  guard _args[1].0 is YYObj_Type(_dollar2)
  YYObj_ConstrParam({();  {mut_: _dollar1, ty: _dollar2, label: None} })
}

// file:///./mbti_parser.mbty
// 318|    boption("mut") post_label ":" type_ { {mut_: $1, ty: $4, label: Some($2)} }
fn yy_action_171(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Bool(_dollar1)
  guard _args[3].0 is YYObj_Type(_dollar4)
  guard _args[1].0 is YYObj_Label(_dollar2)
  YYObj_ConstrParam({();  {mut_: _dollar1, ty: _dollar4, label: Some(_dollar2)} })
}

// file:///./mbti_parser.mbty
// 474|    POST_LABEL { {name: $1, loc: Location::trim_last_char(mk_loc($sloc))} }
fn yy_action_172(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Label({();  {name: _dollar1, loc: Location::trim_last_char(mk_loc((_symbol_start_pos, _end_pos)))} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }
fn yy_action_173(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_ConstrParam_(_dollar3)
  guard _args[0].0 is YYObj_ConstrParam(_dollar1)
  YYObj__list_List_ConstrParam_({();  _dollar3.add(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }
fn yy_action_174(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrParam(_dollar1)
  YYObj__list_List_ConstrParam_({();  @list.singleton(_dollar1) })
}

// file:///./stdlib.mbty
// 5|    X { Some($1) }
fn yy_action_175(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_176(_last_pos, _args[0:3])
  guard _sub_action_0_result is YYObj__list_List_ConstrParam_(_dollar1)
  YYObj_Option__list_List_ConstrParam__({();  Some(_dollar1) })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_176(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj__list_List_ConstrParam_(_dollar2)
  YYObj__list_List_ConstrParam_({();  _dollar2 })
}

// file:///./stdlib.mbty
// 20|    X { true }
fn yy_action_177(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Bool({();  true })
}

// file:///./stdlib.mbty
// 19|    { false }
fn yy_action_178(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Bool({();  false })
}

// file:///./stdlib.mbty
// 4|    { None }
fn yy_action_179(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Option__list_List_ConstrParam__({();  None })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }
fn yy_action_180(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_ConstrDecl_(_dollar3)
  guard _args[0].0 is YYObj_ConstrDecl(_dollar1)
  YYObj__list_List_ConstrDecl_({();  _dollar3.add(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }
fn yy_action_181(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrDecl(_dollar1)
  YYObj__list_List_ConstrDecl_({();  @list.singleton(_dollar1) })
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }
fn yy_action_182(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_ConstrDecl_({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 361|    uident optional_type_params_no_constraints { ($1, $2) }
fn yy_action_183(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[1].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar2)
  YYObj__Name___list_List_TypeParamNoConstraints__({();  (_dollar1, _dollar2) })
}

// file:///./mbti_parser.mbty
// 365|    uident optional_type_params_no_constraints {
// 366|    let target = QualifiedName::{ name: Ident(name = $1.name), loc: $1.loc }
// 367|    ($1, $2, target)
// 368|  }
fn yy_action_184(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[1].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar2)
  YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_({(); 
    let target = QualifiedName::{ name: Ident(name = _dollar1.name), loc: _dollar1.loc }
    (_dollar1, _dollar2, target)
  })
}

// file:///./mbti_parser.mbty
// 348|    "[" separated_list(",", type_param_no_constraints) "]" { $2 }
fn yy_action_185(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar2)
  YYObj__list_List_TypeParamNoConstraints_({();  _dollar2 })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_186(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar1)
  YYObj__list_List_TypeParamNoConstraints_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 343|    uident { Name($1) }
fn yy_action_187(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_TypeParamNoConstraints({();  Name(_dollar1) })
}

// file:///./mbti_parser.mbty
// 344|    "_" { Underscore(mk_loc($sloc)) }
fn yy_action_188(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_TypeParamNoConstraints({();  Underscore(mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }
fn yy_action_189(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar3)
  guard _args[0].0 is YYObj_TypeParamNoConstraints(_dollar1)
  YYObj__list_List_TypeParamNoConstraints_({();  _dollar3.add(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }
fn yy_action_190(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamNoConstraints(_dollar1)
  YYObj__list_List_TypeParamNoConstraints_({();  @list.singleton(_dollar1) })
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }
fn yy_action_191(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_TypeParamNoConstraints_({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 353|    type_params_no_constraints { $1 }
fn yy_action_192(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar1)
  YYObj__list_List_TypeParamNoConstraints_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 369|    PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints {
// 370|    let name = Name::{ name: $2, loc: mk_loc($loc($2)) }
// 371|    let target = QualifiedName::{ name: Dot(pkg = $1, id = $2), loc: mk_loc($sloc) }
// 372|    (name, $3, target)
// 373|  }
fn yy_action_193(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_String(_dollar2)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  guard _args[2].0 is YYObj__list_List_TypeParamNoConstraints_(_dollar3)
  YYObj__Name___list_List_TypeParamNoConstraints___QualifiedName_({(); 
    let name = Name::{ name: _dollar2, loc: mk_loc((_start_pos_of_item1, _end_pos_of_item1)) }
    let target = QualifiedName::{ name: Dot(pkg = _dollar1, id = _dollar2), loc: mk_loc((_symbol_start_pos, _end_pos)) }
    (name, _dollar3, target)
  })
}

// file:///./mbti_parser.mbty
// 352|    { @list.empty() }
fn yy_action_194(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_TypeParamNoConstraints_({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 265|    attributes vis "impl" type_params_with_constraints qualified_uident "for" type_ {
// 266|    {type_params : $4, trait_name : $5, type_ : $7, attrs : $1}
// 267|  }
fn yy_action_195(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[2].0 is YYObj_QualifiedName(_dollar5)
  guard _args[4].0 is YYObj_Type(_dollar7)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : _dollar4, trait_name : _dollar5, type_ : _dollar7, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 268|    attributes vis "impl" qualified_uident "for" type_ {
// 269|    {type_params : @list.empty(), trait_name : $4, type_ : $6, attrs : $1}
// 270|  }
fn yy_action_196(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_QualifiedName(_dollar4)
  guard _args[3].0 is YYObj_Type(_dollar6)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_ImplSig({(); 
    {type_params : @list.empty(), trait_name : _dollar4, type_ : _dollar6, attrs : _dollar1}
  })
}

// file:///./mbti_parser.mbty
// 274|    attributes vis "trait" uident loption(preceded(":", separated_nonempty_list("+", qualified_uident))) trait_body {
// 275|    { attrs : $1, vis: $2, name: $4, super_traits: $5, methods: $6}
// 276|  }
fn yy_action_197(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Visibility(_dollar2)
  guard _args[1].0 is YYObj_Name(_dollar4)
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar5)
  guard _args[3].0 is YYObj__list_List_TraitMethodSig_(_dollar6)
  YYObj_TraitSig({(); 
    { attrs : _dollar1, vis: _dollar2, name: _dollar4, super_traits: _dollar5, methods: _dollar6}
  })
}

// file:///./mbti_parser.mbty
// 281|    "{" separated_list(";", trait_method_sig) "}" { $2 }
fn yy_action_198(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj__list_List_TraitMethodSig_(_dollar2)
  YYObj__list_List_TraitMethodSig_({();  _dollar2 })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_199(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_TraitMethodSig_(_dollar1)
  YYObj__list_List_TraitMethodSig_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 219|    attributes is_async "fn" loption(type_params_with_constraints) lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) {
// 220|    { attrs : $1, is_async: $2, type_params: $4, name: $5, params: $6, return_: $8, has_default: $9}
// 221|  }
fn yy_action_200(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_20(_last_pos, _args[0:1])
  let _sub_action_2_result = yy_action_21(_args[3].2, _args[4:7])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[3].0 is YYObj_Name(_dollar5)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar6)
  guard _args[8].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _args[9].0 is YYObj_Bool(_dollar9)
  YYObj_TraitMethodSig({(); 
    { attrs : _dollar1, is_async: _dollar2, type_params: _dollar4, name: _dollar5, params: _dollar6, return_: _dollar8, has_default: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 222|    attributes is_async lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) {
// 223|    { attrs : $1, is_async: $2, type_params: @list.empty(), name: $3, params: $4, return_: $6, has_default: $7}
// 224|  }
fn yy_action_201(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_20(_last_pos, _args[0:1])
  let _sub_action_2_result = yy_action_21(_args[1].2, _args[2:5])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[1].0 is YYObj_Name(_dollar3)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar4)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _args[7].0 is YYObj_Bool(_dollar7)
  YYObj_TraitMethodSig({(); 
    { attrs : _dollar1, is_async: _dollar2, type_params: @list.empty(), name: _dollar3, params: _dollar4, return_: _dollar6, has_default: _dollar7}
  })
}

// file:///./mbti_parser.mbty
// 219|    attributes is_async "fn" loption(type_params_with_constraints) lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) {
// 220|    { attrs : $1, is_async: $2, type_params: $4, name: $5, params: $6, return_: $8, has_default: $9}
// 221|  }
fn yy_action_202(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_23(_last_pos, _args[0:0])
  let _sub_action_2_result = yy_action_21(_args[2].2, _args[3:6])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[1].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[2].0 is YYObj_Name(_dollar5)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar6)
  guard _args[7].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _args[8].0 is YYObj_Bool(_dollar9)
  YYObj_TraitMethodSig({(); 
    { attrs : _dollar1, is_async: _dollar2, type_params: _dollar4, name: _dollar5, params: _dollar6, return_: _dollar8, has_default: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 219|    attributes is_async "fn" loption(type_params_with_constraints) lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) {
// 220|    { attrs : $1, is_async: $2, type_params: $4, name: $5, params: $6, return_: $8, has_default: $9}
// 221|  }
fn yy_action_203(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_20(_args[0].2, _args[1:2])
  let _sub_action_2_result = yy_action_21(_args[4].2, _args[5:8])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[3].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[4].0 is YYObj_Name(_dollar5)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar6)
  guard _args[9].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _args[10].0 is YYObj_Bool(_dollar9)
  YYObj_TraitMethodSig({(); 
    { attrs : _dollar1, is_async: _dollar2, type_params: _dollar4, name: _dollar5, params: _dollar6, return_: _dollar8, has_default: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 222|    attributes is_async lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) {
// 223|    { attrs : $1, is_async: $2, type_params: @list.empty(), name: $3, params: $4, return_: $6, has_default: $7}
// 224|  }
fn yy_action_204(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_20(_args[0].2, _args[1:2])
  let _sub_action_2_result = yy_action_21(_args[2].2, _args[3:6])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[2].0 is YYObj_Name(_dollar3)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar4)
  guard _args[7].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _args[8].0 is YYObj_Bool(_dollar7)
  YYObj_TraitMethodSig({(); 
    { attrs : _dollar1, is_async: _dollar2, type_params: @list.empty(), name: _dollar3, params: _dollar4, return_: _dollar6, has_default: _dollar7}
  })
}

// file:///./mbti_parser.mbty
// 219|    attributes is_async "fn" loption(type_params_with_constraints) lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) {
// 220|    { attrs : $1, is_async: $2, type_params: $4, name: $5, params: $6, return_: $8, has_default: $9}
// 221|  }
fn yy_action_205(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_23(_args[0].2, _args[1:1])
  let _sub_action_2_result = yy_action_21(_args[3].2, _args[4:7])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar4)
  guard _args[3].0 is YYObj_Name(_dollar5)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar6)
  guard _args[8].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _args[9].0 is YYObj_Bool(_dollar9)
  YYObj_TraitMethodSig({(); 
    { attrs : _dollar1, is_async: _dollar2, type_params: _dollar4, name: _dollar5, params: _dollar6, return_: _dollar8, has_default: _dollar9}
  })
}

// file:///./mbti_parser.mbty
// 339|    "[" separated_list(",", type_param_with_constraints) "]" { $2 }
fn yy_action_206(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar2)
  YYObj__list_List_TypeParamWithConstraints_({();  _dollar2 })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_207(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar1)
  YYObj__list_List_TypeParamWithConstraints_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 335|    uident ":" separated_nonempty_list("+", type_constraint) { {name: $1, constraints: $3} }
fn yy_action_208(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar3)
  YYObj_TypeParamWithConstraints({();  {name: _dollar1, constraints: _dollar3} })
}

// file:///./mbti_parser.mbty
// 357|    qualified_uident { $1 }
fn yy_action_209(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj_QualifiedName({();  _dollar1 })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }
fn yy_action_210(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar3)
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__list_List_QualifiedName_({();  _dollar3.add(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }
fn yy_action_211(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__list_List_QualifiedName_({();  @list.singleton(_dollar1) })
}

// file:///./mbti_parser.mbty
// 334|    uident { {name: $1, constraints: @list.empty()} }
fn yy_action_212(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_TypeParamWithConstraints({();  {name: _dollar1, constraints: @list.empty()} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }
fn yy_action_213(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar3)
  guard _args[0].0 is YYObj_TypeParamWithConstraints(_dollar1)
  YYObj__list_List_TypeParamWithConstraints_({();  _dollar3.add(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }
fn yy_action_214(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TypeParamWithConstraints(_dollar1)
  YYObj__list_List_TypeParamWithConstraints_({();  @list.singleton(_dollar1) })
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }
fn yy_action_215(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_TypeParamWithConstraints_({();  @list.empty() })
}

// file:///./stdlib.mbty
// 15|    X { $1 }
fn yy_action_216(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_TypeParamWithConstraints_(_dollar1)
  YYObj__list_List_TypeParamWithConstraints_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 14|    { @list.empty() }
fn yy_action_217(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_TypeParamWithConstraints_({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 222|    attributes is_async lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) {
// 223|    { attrs : $1, is_async: $2, type_params: @list.empty(), name: $3, params: $4, return_: $6, has_default: $7}
// 224|  }
fn yy_action_218(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_60(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_23(_args[0].2, _args[1:1])
  let _sub_action_2_result = yy_action_21(_args[1].2, _args[2:5])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[1].0 is YYObj_Name(_dollar3)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar4)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _args[7].0 is YYObj_Bool(_dollar7)
  YYObj_TraitMethodSig({(); 
    { attrs : _dollar1, is_async: _dollar2, type_params: @list.empty(), name: _dollar3, params: _dollar4, return_: _dollar6, has_default: _dollar7}
  })
}

// file:///./mbti_parser.mbty
// 479|    nonempty_list(attribute) { $1 }
fn yy_action_60(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_Attribute_(_dollar1)
  YYObj__list_List_Attribute_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 222|    attributes is_async lident delimited("(", separated_list(",", parameter), ")") "->" return_type boption(eq_underscore) {
// 223|    { attrs : $1, is_async: $2, type_params: @list.empty(), name: $3, params: $4, return_: $6, has_default: $7}
// 224|  }
fn yy_action_219(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_23(_last_pos, _args[0:0])
  let _sub_action_2_result = yy_action_21(_args[0].2, _args[1:4])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _sub_action_1_result is YYObj_Location_(_dollar2)
  guard _args[0].0 is YYObj_Name(_dollar3)
  guard _sub_action_2_result is YYObj__list_List_Parameter_(_dollar4)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _args[6].0 is YYObj_Bool(_dollar7)
  YYObj_TraitMethodSig({(); 
    { attrs : _dollar1, is_async: _dollar2, type_params: @list.empty(), name: _dollar3, params: _dollar4, return_: _dollar6, has_default: _dollar7}
  })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_21(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj__list_List_Parameter_(_dollar2)
  YYObj__list_List_Parameter_({();  _dollar2 })
}

// file:///./stdlib.mbty
// 20|    X { true }
fn yy_action_220(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_221(_last_pos, _args[0:2])
  YYObj_Bool({();  true })
}

// file:///./mbti_parser.mbty
// 228|    "=" "_" {}
fn yy_action_221(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Unit({(); })
}

// file:///./stdlib.mbty
// 19|    { false }
fn yy_action_222(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Bool({();  false })
}

// file:///./stdlib.mbty
// 55|    separated_nonempty_list(SEP, X) { $1 }
fn yy_action_223(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_Parameter_(_dollar1)
  YYObj__list_List_Parameter_({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 413|    type_ { DiscardPositional(ty = Some($1), loc = @syntax.no_location) }
fn yy_action_224(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj_Parameter({();  DiscardPositional(ty = Some(_dollar1), loc = @syntax.no_location) })
}

// file:///./mbti_parser.mbty
// 414|    POST_LABEL ":" type_ { Labelled(binder = { name : $1, loc : mk_loc($loc($1)) }, ty = Some($3)) }
fn yy_action_225(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj_Parameter({();  Labelled(binder = { name : _dollar1, loc : mk_loc((_start_pos_of_item0, _end_pos_of_item0)) }, ty = Some(_dollar3)) })
}

// file:///./mbti_parser.mbty
// 387|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { Arrow(args=$5.unwrap_or(@list.empty()).add($3), res=$8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_226(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_20(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_227(_args[3].2, _args[4:4])
  guard _sub_action_1_result is YYObj_Option__list_List_Type__(_dollar5)
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Arrow(args=_dollar5.unwrap_or(@list.empty()).add(_dollar3), res=_dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 387|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { Arrow(args=$5.unwrap_or(@list.empty()).add($3), res=$8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_228(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_20(_last_pos, _args[0:1])
  let _sub_action_1_result = yy_action_229(_args[3].2, _args[4:5])
  guard _sub_action_1_result is YYObj_Option__list_List_Type__(_dollar5)
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _args[7].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Arrow(args=_dollar5.unwrap_or(@list.empty()).add(_dollar3), res=_dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 389|    is_async "(" type_ ")" "->" return_type { Arrow(args=@list.singleton($3), res=$6.0, err=$6.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_230(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_20(_last_pos, _args[0:1])
  guard _args[2].0 is YYObj_Type(_dollar3)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Arrow(args=@list.singleton(_dollar3), res=_dollar6.0, err=_dollar6.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 380|    qualified_uident_ optional_type_arguments { Name(constr_id=$1, tys=$2, loc=mk_loc($sloc)) }
fn yy_action_231(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrId(_dollar1)
  guard _args[1].0 is YYObj__list_List_Type_(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Name(constr_id=_dollar1, tys=_dollar2, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 408|    delimited("[", separated_nonempty_list(",", type_), "]") { $1 }
fn yy_action_232(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_233(_last_pos, _args[0:3])
  guard _sub_action_0_result is YYObj__list_List_Type_(_dollar1)
  YYObj__list_List_Type_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 40|    L X R { $2 }
fn yy_action_233(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj__list_List_Type_(_dollar2)
  YYObj__list_List_Type_({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 387|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { Arrow(args=$5.unwrap_or(@list.empty()).add($3), res=$8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_234(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_23(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_229(_args[2].2, _args[3:4])
  guard _sub_action_1_result is YYObj_Option__list_List_Type__(_dollar5)
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _args[6].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Arrow(args=_dollar5.unwrap_or(@list.empty()).add(_dollar3), res=_dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 10|    X { Some($1) }
fn yy_action_229(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_Type_(_dollar1)
  YYObj_Option__list_List_Type__({();  Some(_dollar1) })
}

// file:///./mbti_parser.mbty
// 378|    "(" type_ "," separated_nonempty_list(",", type_) ")" { Tuple(tys=$4.add($2), loc=mk_loc($sloc)) }
fn yy_action_235(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[3].0 is YYObj__list_List_Type_(_dollar4)
  guard _args[1].0 is YYObj_Type(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Tuple(tys=_dollar4.add(_dollar2), loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 387|    is_async "(" type_ "," ioption(separated_nonempty_list(",", type_)) ")" "->" return_type { Arrow(args=$5.unwrap_or(@list.empty()).add($3), res=$8.0, err=$8.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_236(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_23(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_227(_args[2].2, _args[3:3])
  guard _sub_action_1_result is YYObj_Option__list_List_Type__(_dollar5)
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _args[5].0 is YYObj__Type__ErrorType_(_dollar8)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Arrow(args=_dollar5.unwrap_or(@list.empty()).add(_dollar3), res=_dollar8.0, err=_dollar8.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./stdlib.mbty
// 9|    { None }
fn yy_action_227(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Option__list_List_Type__({();  None })
}

// file:///./mbti_parser.mbty
// 389|    is_async "(" type_ ")" "->" return_type { Arrow(args=@list.singleton($3), res=$6.0, err=$6.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_237(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_23(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_Type(_dollar3)
  guard _args[4].0 is YYObj__Type__ErrorType_(_dollar6)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Arrow(args=@list.singleton(_dollar3), res=_dollar6.0, err=_dollar6.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 379|    "(" type_ ")" { $2 }
fn yy_action_238(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Type(_dollar2)
  YYObj_Type({();  _dollar2 })
}

// file:///./mbti_parser.mbty
// 377|    simple_type "?" { Option(ty=$1, loc=mk_loc($sloc), question_loc=mk_loc($loc($2))) }
fn yy_action_239(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj_Type({();  Option(ty=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos)), question_loc=mk_loc((_start_pos_of_item1, _end_pos_of_item1))) })
}

// file:///./mbti_parser.mbty
// 394|    simple_type "noraise" { ($1, Noraise(loc=mk_loc($sloc))) }
fn yy_action_240(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj__Type__ErrorType_({();  (_dollar1, Noraise(loc=mk_loc((_symbol_start_pos, _end_pos)))) })
}

// file:///./mbti_parser.mbty
// 396|    simple_type "raise" error_type { ($1, ErrorType(ty = $3)) }
fn yy_action_241(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  guard _args[2].0 is YYObj_Type(_dollar3)
  YYObj__Type__ErrorType_({();  (_dollar1, ErrorType(ty = _dollar3)) })
}

// file:///./mbti_parser.mbty
// 397|    simple_type "raise" "?" {
// 398|    let fake_error : Type = Name(constr_id={ id : Ident(name="Error"), loc :@syntax.no_location }, tys=@list.empty(), loc=@syntax.no_location)
// 399|    ($1, MaybeError(ty=fake_error))
// 400|  }
fn yy_action_242(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__Type__ErrorType_({(); 
    let fake_error : Type = Name(constr_id={ id : Ident(name="Error"), loc :@syntax.no_location }, tys=@list.empty(), loc=@syntax.no_location)
    (_dollar1, MaybeError(ty=fake_error))
  })
}

// file:///./mbti_parser.mbty
// 404|    qualified_uident_ { Name(constr_id=$1, tys=@list.empty(), loc=mk_loc($sloc)) }
fn yy_action_243(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_ConstrId(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Name(constr_id=_dollar1, tys=@list.empty(), loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 395|    simple_type "raise" { ($1, DefaultErrorType(loc=mk_loc($loc($2)))) }
fn yy_action_244(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  let _start_pos_of_item1 = _args[1].1
  let _end_pos_of_item1 = _args[1].2
  YYObj__Type__ErrorType_({();  (_dollar1, DefaultErrorType(loc=mk_loc((_start_pos_of_item1, _end_pos_of_item1)))) })
}

// file:///./mbti_parser.mbty
// 386|    simple_type { $1 }
fn yy_action_245(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj_Type({();  _dollar1 })
}

// file:///./mbti_parser.mbty
// 388|    is_async "(" ")" "->" return_type { Arrow(args=@list.empty(), res=$5.0, err=$5.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_246(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_23(_last_pos, _args[0:0])
  guard _args[3].0 is YYObj__Type__ErrorType_(_dollar5)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Arrow(args=@list.empty(), res=_dollar5.0, err=_dollar5.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 447|    { None }
fn yy_action_23(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Location_({();  None })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }
fn yy_action_247(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_Type_(_dollar3)
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__list_List_Type_({();  _dollar3.add(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }
fn yy_action_248(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__list_List_Type_({();  @list.singleton(_dollar1) })
}

// file:///./mbti_parser.mbty
// 409|     { @list.empty() }
fn yy_action_249(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_Type_({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 381|    "&" qualified_uident_ { Object($2) }
fn yy_action_250(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_ConstrId(_dollar2)
  YYObj_Type({();  Object(_dollar2) })
}

// file:///./mbti_parser.mbty
// 382|    "_" { Any(loc=mk_loc($sloc)) }
fn yy_action_251(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Any(loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 388|    is_async "(" ")" "->" return_type { Arrow(args=@list.empty(), res=$5.0, err=$5.1, is_async=$1, loc=mk_loc($sloc)) }
fn yy_action_252(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_20(_last_pos, _args[0:1])
  guard _args[4].0 is YYObj__Type__ErrorType_(_dollar5)
  guard _sub_action_0_result is YYObj_Location_(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Type({();  Arrow(args=@list.empty(), res=_dollar5.0, err=_dollar5.1, is_async=_dollar1, loc=mk_loc((_symbol_start_pos, _end_pos))) })
}

// file:///./mbti_parser.mbty
// 446|    "async" { Some(mk_loc($loc($1))) }
fn yy_action_20(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _start_pos_of_item0 = _args[0].1
  let _end_pos_of_item0 = _args[0].2
  YYObj_Location_({();  Some(mk_loc((_start_pos_of_item0, _end_pos_of_item0))) })
}

// file:///./mbti_parser.mbty
// 393|    type_ { ($1, NoErrorType) }
fn yy_action_253(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Type(_dollar1)
  YYObj__Type__ErrorType_({();  (_dollar1, NoErrorType) })
}

// file:///./mbti_parser.mbty
// 415|    label "?" ":" type_ { QuestionOptional(binder = { name : $1.name, loc : $1.loc }, ty = Some($4)) }
fn yy_action_254(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Label(_dollar1)
  guard _args[3].0 is YYObj_Type(_dollar4)
  YYObj_Parameter({();  QuestionOptional(binder = { name : _dollar1.name, loc : _dollar1.loc }, ty = Some(_dollar4)) })
}

// file:///./mbti_parser.mbty
// 456|    UIDENT { { id: Ident(name=$1), loc: mk_loc($sloc)} }
fn yy_action_255(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_ConstrId({();  { id: Ident(name=_dollar1), loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 457|    PACKAGE_NAME DOT_UIDENT { { id: Dot(pkg=$1, id=$2), loc: mk_loc($sloc)} }
fn yy_action_256(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  guard _args[1].0 is YYObj_String(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_ConstrId({();  { id: Dot(pkg=_dollar1, id=_dollar2), loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 470|    lident { {name: $1.name, loc: $1.loc} }
fn yy_action_257(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Name(_dollar1)
  YYObj_Label({();  {name: _dollar1.name, loc: _dollar1.loc} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }
fn yy_action_258(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_Parameter_(_dollar3)
  guard _args[0].0 is YYObj_Parameter(_dollar1)
  YYObj__list_List_Parameter_({();  _dollar3.add(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }
fn yy_action_259(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_Parameter(_dollar1)
  YYObj__list_List_Parameter_({();  @list.singleton(_dollar1) })
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }
fn yy_action_260(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_Parameter_({();  @list.empty() })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }
fn yy_action_261(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_TraitMethodSig_(_dollar3)
  guard _args[0].0 is YYObj_TraitMethodSig(_dollar1)
  YYObj__list_List_TraitMethodSig_({();  _dollar3.add(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }
fn yy_action_262(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_TraitMethodSig(_dollar1)
  YYObj__list_List_TraitMethodSig_({();  @list.singleton(_dollar1) })
}

// file:///./stdlib.mbty
// 54|    { @list.empty() }
fn yy_action_263(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_TraitMethodSig_({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 280|    { @list.empty() }
fn yy_action_264(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_TraitMethodSig_({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 451|    UIDENT { { name: Ident(name=$1), loc: mk_loc($sloc)} }
fn yy_action_265(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_QualifiedName({();  { name: Ident(name=_dollar1), loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 452|    PACKAGE_NAME DOT_UIDENT { { name: Dot(pkg=$1, id=$2), loc: mk_loc($sloc)} }
fn yy_action_266(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  guard _args[1].0 is YYObj_String(_dollar2)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_QualifiedName({();  { name: Dot(pkg=_dollar1, id=_dollar2), loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./stdlib.mbty
// 60|    X SEP separated_nonempty_list(SEP, X) { $3.add($1) }
fn yy_action_267(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_QualifiedName_(_dollar3)
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__list_List_QualifiedName_({();  _dollar3.add(_dollar1) })
}

// file:///./stdlib.mbty
// 59|    X { @list.singleton($1) }
fn yy_action_268(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_QualifiedName(_dollar1)
  YYObj__list_List_QualifiedName_({();  @list.singleton(_dollar1) })
}

// file:///./stdlib.mbty
// 15|    X { $1 }
fn yy_action_269(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_270(_last_pos, _args[0:2])
  guard _sub_action_0_result is YYObj__list_List_QualifiedName_(_dollar1)
  YYObj__list_List_QualifiedName_({();  _dollar1 })
}

// file:///./stdlib.mbty
// 32|    L X { $2 }
fn yy_action_270(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj__list_List_QualifiedName_(_dollar2)
  YYObj__list_List_QualifiedName_({();  _dollar2 })
}

// file:///./stdlib.mbty
// 14|    { @list.empty() }
fn yy_action_271(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_QualifiedName_({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 293|    attributes vis "fnalias" uident "::" lident {
// 294|    FuncAlias(attrs = $1,name=$6, type_name=$4)
// 295|  }
fn yy_action_272(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  guard _args[3].0 is YYObj_Name(_dollar6)
  guard _args[1].0 is YYObj_Name(_dollar4)
  YYObj_AliasSig({(); 
    FuncAlias(attrs = _dollar1,name=_dollar6, type_name=_dollar4)
  })
}

// file:///./mbti_parser.mbty
// 465|    LIDENT { {name: $1, loc: mk_loc($sloc)} }
fn yy_action_273(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  {name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 466|    "extend" { {name: "extend", loc: mk_loc($sloc)} }
fn yy_action_274(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  {name: "extend", loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./mbti_parser.mbty
// 296|    attributes vis "using" PACKAGE_NAME "{" "type" uident using_binder "}" {
// 297|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 298|    Using(pkg~, target = $7, name = $8, kind = Type, attrs = $1)
// 299|  }
fn yy_action_275(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[1].1
  let _end_pos_of_item3 = _args[1].2
  guard _args[4].0 is YYObj_Name(_dollar7)
  guard _args[5].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Type, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 286|    "as" uident { Some($2) }
fn yy_action_276(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[1].0 is YYObj_Name(_dollar2)
  YYObj_Name_({();  Some(_dollar2) })
}

// file:///./mbti_parser.mbty
// 300|    attributes vis "using" PACKAGE_NAME "{" "trait" uident using_binder "}" {
// 301|    let pkg = Name::{ name : $4, loc : mk_loc($loc($4)) }
// 302|    Using(pkg~, target = $7, name = $8, kind = Trait, attrs = $1)
// 303|  }
fn yy_action_277(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_14(_last_pos, _args[0:0])
  let _sub_action_1_result = yy_action_15(_last_pos, _args[0:0])
  guard _args[1].0 is YYObj_String(_dollar4)
  let _start_pos_of_item3 = _args[1].1
  let _end_pos_of_item3 = _args[1].2
  guard _args[4].0 is YYObj_Name(_dollar7)
  guard _args[5].0 is YYObj_Name_(_dollar8)
  guard _sub_action_0_result is YYObj__list_List_Attribute_(_dollar1)
  YYObj_AliasSig({(); 
    let pkg = Name::{ name : _dollar4, loc : mk_loc((_start_pos_of_item3, _end_pos_of_item3)) }
    Using(pkg~, target = _dollar7, name = _dollar8, kind = Trait, attrs = _dollar1)
  })
}

// file:///./mbti_parser.mbty
// 434|    { Default }
fn yy_action_15(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Visibility({();  Default })
}

// file:///./mbti_parser.mbty
// 478|     {  @list.empty() }
fn yy_action_14(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_Attribute_({();   @list.empty() })
}

// file:///./mbti_parser.mbty
// 285|    { None }
fn yy_action_278(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Name_({();  None })
}

// file:///./mbti_parser.mbty
// 461|    UIDENT { {name: $1, loc: mk_loc($sloc)} }
fn yy_action_279(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_String(_dollar1)
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  YYObj_Name({();  {name: _dollar1, loc: mk_loc((_symbol_start_pos, _end_pos))} })
}

// file:///./stdlib.mbty
// 50|    X nonempty_list(X) { $2.add($1) }
fn yy_action_280(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_281(_last_pos, _args[0:1])
  guard _args[1].0 is YYObj__list_List_Attribute_(_dollar2)
  guard _sub_action_0_result is YYObj_Attribute(_dollar1)
  YYObj__list_List_Attribute_({();  _dollar2.add(_dollar1) })
}

// file:///./stdlib.mbty
// 49|    X { @list.singleton($1) }
fn yy_action_282(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _sub_action_0_result = yy_action_281(_last_pos, _args[0:1])
  guard _sub_action_0_result is YYObj_Attribute(_dollar1)
  YYObj__list_List_Attribute_({();  @list.singleton(_dollar1) })
}

// file:///./mbti_parser.mbty
// 483|    ATTRIBUTE { Attribute::new(loc=mk_loc($sloc), $1) }
fn yy_action_281(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _symbol_start_pos = _get_symbol_start_pos(_args, _last_pos)
  let _end_pos = if _args.length() == 0 { _last_pos } else { _args[_args.length() - 1].2 }
  guard _args[0].0 is YYObj__String__Option_String___String_(_dollar1)
  YYObj_Attribute({();  Attribute::new(loc=mk_loc((_symbol_start_pos, _end_pos)), _dollar1) })
}

// file:///./mbti_parser.mbty
// 163|     { @list.empty() }
fn yy_action_283(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List__Sig__Location__({();  @list.empty() })
}

// file:///./mbti_parser.mbty
// 154|    "import" "{" non_empty_list_commas(package_import) "}" ";" { $3 }
fn yy_action_284(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[2].0 is YYObj__list_List_PackageImport_(_dollar3)
  YYObj__list_List_PackageImport_({();  _dollar3 })
}

// file:///./mbti_parser.mbty
// 145|    non_empty_list_commas_rev(X) option(",") { $1.rev() }
fn yy_action_285(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_PackageImport_(_dollar1)
  YYObj__list_List_PackageImport_({();  _dollar1.rev() })
}

// file:///./mbti_parser.mbty
// 159|    STRING "as" PACKAGE_NAME { {name: $1, alias_: Some($3)} }
fn yy_action_286(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  guard _args[2].0 is YYObj_String(_dollar3)
  YYObj_PackageImport({();  {name: _dollar1, alias_: Some(_dollar3)} })
}

// file:///./mbti_parser.mbty
// 158|    STRING { {name: $1, alias_: None} }
fn yy_action_287(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_StringLiteral(_dollar1)
  YYObj_PackageImport({();  {name: _dollar1, alias_: None} })
}

// file:///./mbti_parser.mbty
// 141|    non_empty_list_commas_rev(X) "," X { $1.add($3) }
fn yy_action_288(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj__list_List_PackageImport_(_dollar1)
  guard _args[2].0 is YYObj_PackageImport(_dollar3)
  YYObj__list_List_PackageImport_({();  _dollar1.add(_dollar3) })
}

// file:///./stdlib.mbty
// 5|    X { Some($1) }
fn yy_action_289(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  let _dollar1 = ()
  YYObj_Option_Unit_({();  Some(_dollar1) })
}

// file:///./stdlib.mbty
// 4|    { None }
fn yy_action_290(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj_Option_Unit_({();  None })
}

// file:///./mbti_parser.mbty
// 140|    X { @list.singleton($1) }
fn yy_action_291(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  guard _args[0].0 is YYObj_PackageImport(_dollar1)
  YYObj__list_List_PackageImport_({();  @list.singleton(_dollar1) })
}

// file:///./mbti_parser.mbty
// 153|     { @list.empty() }
fn yy_action_292(_last_pos : @basic.Position, _args : ArrayView[(YYObj, @basic.Position, @basic.Position)]) -> YYObj {
  YYObj__list_List_PackageImport_({();  @list.empty() })
}

fn yy_input(token : Token, _start_pos : @basic.Position, _end_pos : @basic.Position) -> (YYSymbol, YYObj) {
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
    BYTES_INTERP(data) => (T_BYTES_INTERP, YYObj_InterpLiteral(data))
    REGEX_LITERAL(data) => (T_REGEX_LITERAL, YYObj_String(data))
    REGEX_INTERP(data) => (T_REGEX_INTERP, YYObj_InterpLiteral(data))
    ATTRIBUTE(data) => (T_ATTRIBUTE, YYObj__String__Option_String___String_(data))
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
    LBRACKET_BAR => (T_LBRACKET_BAR, YYObj_Void)
    PLUS => (T_PLUS, YYObj_Void)
    RBRACKET => (T_RBRACKET, YYObj_Void)
    BAR_RBRACKET => (T_BAR_RBRACKET, YYObj_Void)
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
    LT_QUESTION => (T_LT_QUESTION, YYObj_Void)
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
    LEXSCAN => (T_LEXSCAN, YYObj_Void)
    EXTEND => (T_EXTEND, YYObj_Void)
    PACKAGE => (T_PACKAGE, YYObj_Void)
  }
}

// [0, mbti → • PACKAGE STRING imports sigs EOF, $]
// [1, mbti → • PACKAGE STRING SEMI imports sigs EOF, $]
// [277, mbti_prime → • mbti, $]
fn yy_state_0(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_mbti => Shift(yy_state_1)
    T_PACKAGE => Shift(yy_state_2)
    _ => Error
  }
}

// [277, mbti_prime → mbti •, $]
fn yy_state_1(_lookahead : YYSymbol) -> YYDecision {
  Accept
}

// [0, mbti → PACKAGE • STRING imports sigs EOF, $]
// [1, mbti → PACKAGE • STRING SEMI imports sigs EOF, $]
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
fn yy_state_3(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_IMPORT => Shift(yy_state_4)
    T_SEMI => Shift(yy_state_17)
    NT_imports => Shift(yy_state_872)
    T_ATTRIBUTE | T_EOF | T_PUB | T_PRIV | T_STRUCT | T_ENUM | T_TRAIT | T_IMPL | T_ASYNC | T_FNALIAS | T_FN | T_LET | T_CONST | T_TYPE | T_SUBERROR | T_USING | T_EXTENUM => Reduce(0, NT_imports, yy_action_292)
    _ => Error
  }
}

// [3, imports → IMPORT • LBRACE non_empty_list_commas(package_import) RBRACE SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_4(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_5)
    _ => Error
  }
}

// [3, imports → IMPORT LBRACE • non_empty_list_commas(package_import) RBRACE SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [4, package_import → • STRING, COMMA / RBRACE]
// [5, package_import → • STRING AS PACKAGE_NAME, COMMA / RBRACE]
// [221, non_empty_list_commas(package_import) → • non_empty_list_commas_rev(package_import) option(COMMA), RBRACE]
// [259, non_empty_list_commas_rev(package_import) → • package_import, COMMA / RBRACE]
// [260, non_empty_list_commas_rev(package_import) → • non_empty_list_commas_rev(package_import) COMMA package_import, COMMA / RBRACE]
fn yy_state_5(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_package_import => Shift(yy_state_6)
    NT_non_empty_list_commas_rev_package_import_ => Shift(yy_state_7)
    T_STRING => Shift(yy_state_10)
    NT_non_empty_list_commas_package_import_ => Shift(yy_state_14)
    _ => Error
  }
}

// [259, non_empty_list_commas_rev(package_import) → package_import •, COMMA / RBRACE]
fn yy_state_6(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_non_empty_list_commas_rev_package_import_, yy_action_291)
}

// [221, non_empty_list_commas(package_import) → non_empty_list_commas_rev(package_import) • option(COMMA), RBRACE]
// [260, non_empty_list_commas_rev(package_import) → non_empty_list_commas_rev(package_import) • COMMA package_import, COMMA / RBRACE]
// [261, option(COMMA) → •, RBRACE]
// [262, option(COMMA) → • COMMA, RBRACE]
fn yy_state_7(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_8)
    NT_option_COMMA_ => Shift(yy_state_13)
    T_RBRACE => Reduce(0, NT_option_COMMA_, yy_action_290)
    _ => Error
  }
}

// [4, package_import → • STRING, COMMA / RBRACE]
// [5, package_import → • STRING AS PACKAGE_NAME, COMMA / RBRACE]
// [260, non_empty_list_commas_rev(package_import) → non_empty_list_commas_rev(package_import) COMMA • package_import, COMMA / RBRACE]
// [262, option(COMMA) → COMMA •, RBRACE]
fn yy_state_8(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_package_import => Shift(yy_state_9)
    T_STRING => Shift(yy_state_10)
    T_RBRACE => Reduce(1, NT_option_COMMA_, yy_action_289)
    _ => Error
  }
}

// [260, non_empty_list_commas_rev(package_import) → non_empty_list_commas_rev(package_import) COMMA package_import •, COMMA / RBRACE]
fn yy_state_9(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_non_empty_list_commas_rev_package_import_, yy_action_288)
}

// [4, package_import → STRING •, COMMA / RBRACE]
// [5, package_import → STRING • AS PACKAGE_NAME, COMMA / RBRACE]
fn yy_state_10(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_11)
    T_COMMA | T_RBRACE => Reduce(1, NT_package_import, yy_action_287)
    _ => Error
  }
}

// [5, package_import → STRING AS • PACKAGE_NAME, COMMA / RBRACE]
fn yy_state_11(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_12)
    _ => Error
  }
}

// [5, package_import → STRING AS PACKAGE_NAME •, COMMA / RBRACE]
fn yy_state_12(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_package_import, yy_action_286)
}

// [221, non_empty_list_commas(package_import) → non_empty_list_commas_rev(package_import) option(COMMA) •, RBRACE]
fn yy_state_13(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_non_empty_list_commas_package_import_, yy_action_285)
}

// [3, imports → IMPORT LBRACE non_empty_list_commas(package_import) • RBRACE SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_14(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_15)
    _ => Error
  }
}

// [3, imports → IMPORT LBRACE non_empty_list_commas(package_import) RBRACE • SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_15(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_16)
    _ => Error
  }
}

// [3, imports → IMPORT LBRACE non_empty_list_commas(package_import) RBRACE SEMI •, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_16(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_imports, yy_action_284)
}

// [1, mbti → PACKAGE STRING SEMI • imports sigs EOF, $]
// [2, imports → •, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [3, imports → • IMPORT LBRACE non_empty_list_commas(package_import) RBRACE SEMI, ATTRIBUTE / EOF / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_17(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_IMPORT => Shift(yy_state_4)
    NT_imports => Shift(yy_state_18)
    T_ATTRIBUTE | T_EOF | T_PUB | T_PRIV | T_STRUCT | T_ENUM | T_TRAIT | T_IMPL | T_ASYNC | T_FNALIAS | T_FN | T_LET | T_CONST | T_TYPE | T_SUBERROR | T_USING | T_EXTENUM => Reduce(0, NT_imports, yy_action_292)
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
// [56, type_sig → • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [57, type_sig → • PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [58, type_sig → • PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [59, type_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [60, type_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [61, type_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [62, type_sig → • SUBERROR uident optional_derive, EOF / SEMI]
// [63, type_sig → • PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [64, type_sig → • PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [65, type_sig → • nonempty_list(attribute) SUBERROR uident optional_derive, EOF / SEMI]
// [66, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [67, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [68, type_sig → • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [69, type_sig → • PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [70, type_sig → • PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [71, type_sig → • nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [72, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [73, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [74, type_sig → • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [75, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [76, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [77, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [78, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [79, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [80, type_sig → • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [81, type_sig → • PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [82, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [83, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [84, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [85, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [86, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [87, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [88, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [89, type_sig → • nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [90, type_sig → • nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [91, type_sig → • nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [92, type_sig → • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [93, type_sig → • PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [94, type_sig → • PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [95, type_sig → • nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → • nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [98, type_sig → • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → • PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, type_sig → • PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [101, type_sig → • nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [102, type_sig → • nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [103, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [106, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [107, impl_sig → • PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [108, impl_sig → • PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [109, impl_sig → • nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [110, impl_sig → • nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [111, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [112, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → • PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [114, impl_sig → • PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [115, impl_sig → • nonempty_list(attribute) IMPL qualified_uident FOR type_, EOF / SEMI]
// [116, impl_sig → • nonempty_list(attribute) PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [117, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [118, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [119, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [120, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [121, trait_sig → • nonempty_list(attribute) TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [122, trait_sig → • nonempty_list(attribute) PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [123, trait_sig → • nonempty_list(attribute) PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [128, alias_sig → • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [129, alias_sig → • PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [130, alias_sig → • PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [131, alias_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [132, alias_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [133, alias_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [134, alias_sig → • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [135, alias_sig → • PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [136, alias_sig → • PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [137, alias_sig → • nonempty_list(attribute) FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [138, alias_sig → • nonempty_list(attribute) PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [139, alias_sig → • nonempty_list(attribute) PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [140, alias_sig → • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [141, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [142, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [143, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [148, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [149, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [150, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [151, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [257, nonempty_list(attribute) → • ATTRIBUTE, PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_18(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_USING => Shift(yy_state_21)
    T_FNALIAS => Shift(yy_state_35)
    T_TRAIT => Shift(yy_state_41)
    T_IMPL => Shift(yy_state_213)
    T_EXTENUM => Shift(yy_state_221)
    T_ENUM => Shift(yy_state_283)
    T_STRUCT => Shift(yy_state_291)
    T_SUBERROR => Shift(yy_state_316)
    T_TYPE => Shift(yy_state_323)
    T_FN => Shift(yy_state_328)
    T_ASYNC => Shift(yy_state_346)
    T_LET => Shift(yy_state_355)
    NT_nonempty_list_attribute_ => Shift(yy_state_359)
    T_PUB => Shift(yy_state_663)
    T_PRIV => Shift(yy_state_759)
    T_CONST => Shift(yy_state_854)
    NT_value_sig => Shift(yy_state_860)
    NT_const_sig => Shift(yy_state_861)
    NT_impl_sig => Shift(yy_state_862)
    NT_trait_sig => Shift(yy_state_863)
    NT_alias_sig => Shift(yy_state_864)
    NT_type_sig => Shift(yy_state_865)
    NT_func_sig => Shift(yy_state_866)
    NT_sig_ => Shift(yy_state_867)
    NT_sigs => Shift(yy_state_870)
    T_EOF => Reduce(0, NT_sigs, yy_action_283)
    _ => Error
  }
}

// [257, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / UIDENT / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / MUTABLE / TYPE / SUBERROR / USING / EXTENUM / EXTEND]
// [257, nonempty_list(attribute) → ATTRIBUTE •, LIDENT / UIDENT / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / MUTABLE / TYPE / SUBERROR / USING / EXTENUM / EXTEND]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / UIDENT / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / MUTABLE / TYPE / SUBERROR / USING / EXTENUM / EXTEND]
// [258, nonempty_list(attribute) → ATTRIBUTE • nonempty_list(attribute), LIDENT / UIDENT / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / MUTABLE / TYPE / SUBERROR / USING / EXTENUM / EXTEND]
fn yy_state_19(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_nonempty_list_attribute_ => Shift(yy_state_20)
    T_LIDENT | T_UIDENT | T_PUB | T_PRIV | T_STRUCT | T_ENUM | T_TRAIT | T_IMPL | T_ASYNC | T_FNALIAS | T_FN | T_LET | T_CONST | T_MUTABLE | T_TYPE | T_SUBERROR | T_USING | T_EXTENUM | T_EXTEND => Reduce(1, NT_nonempty_list_attribute_, yy_action_282)
    _ => Error
  }
}

// [258, nonempty_list(attribute) → ATTRIBUTE nonempty_list(attribute) •, LIDENT / UIDENT / PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / MUTABLE / TYPE / SUBERROR / USING / EXTENUM / EXTEND]
fn yy_state_20(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_nonempty_list_attribute_, yy_action_280)
}

// [140, alias_sig → USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_21(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_22)
    _ => Error
  }
}

// [140, alias_sig → USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_22(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_23)
    _ => Error
  }
}

// [140, alias_sig → USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_23(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_24)
    T_TYPE => Shift(yy_state_31)
    _ => Error
  }
}

// [146, alias_sig → USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_24(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_26)
    _ => Error
  }
}

// [216, uident → UIDENT •, PLUS_EQUAL / EOF / DERIVE / EQUAL / LPAREN / COMMA / COLONCOLON / COLON / SEMI / LBRACKET / RBRACKET / LBRACE / RBRACE / AS]
fn yy_state_25(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_uident, yy_action_279)
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [146, alias_sig → USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_26(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_using_binder => Shift(yy_state_27)
    T_AS => Shift(yy_state_29)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [146, alias_sig → USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_27(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_28)
    _ => Error
  }
}

// [146, alias_sig → USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_28(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_alias_sig, yy_action_277)
}

// [127, using_binder → AS • uident, RBRACE]
// [216, uident → • UIDENT, RBRACE]
fn yy_state_29(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_30)
    _ => Error
  }
}

// [127, using_binder → AS uident •, RBRACE]
fn yy_state_30(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_using_binder, yy_action_276)
}

// [140, alias_sig → USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_31(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_32)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [140, alias_sig → USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_32(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_33)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [140, alias_sig → USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_33(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_34)
    _ => Error
  }
}

// [140, alias_sig → USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_34(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_alias_sig, yy_action_275)
}

// [134, alias_sig → FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [216, uident → • UIDENT, COLONCOLON]
fn yy_state_35(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_36)
    _ => Error
  }
}

// [134, alias_sig → FNALIAS uident • COLONCOLON lident, EOF / SEMI]
fn yy_state_36(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_37)
    _ => Error
  }
}

// [134, alias_sig → FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [217, lident → • LIDENT, EOF / SEMI]
// [218, lident → • EXTEND, EOF / SEMI]
fn yy_state_37(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_40)
    _ => Error
  }
}

// [218, lident → EXTEND •, EOF / LPAREN / RPAREN / QUESTION / COLON / SEMI]
fn yy_state_38(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_lident, yy_action_274)
}

// [217, lident → LIDENT •, EOF / LPAREN / RPAREN / QUESTION / COLON / SEMI]
fn yy_state_39(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_lident, yy_action_273)
}

// [134, alias_sig → FNALIAS uident COLONCOLON lident •, EOF / SEMI]
fn yy_state_40(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_alias_sig, yy_action_272)
}

// [118, trait_sig → TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [216, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_41(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_42)
    _ => Error
  }
}

// [118, trait_sig → TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [235, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [236, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
fn yy_state_42(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_43)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_51)
    T_EOF | T_SEMI | T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_271)
    _ => Error
  }
}

// [212, qualified_uident → • UIDENT, EOF / SEMI / PLUS / LBRACE]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, EOF / SEMI / PLUS / LBRACE]
// [233, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident, EOF / SEMI / LBRACE]
// [234, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
// [236, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → COLON • separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
fn yy_state_43(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_separated_nonempty_list_PLUS_qualified_uident_ => Shift(yy_state_44)
    NT_qualified_uident => Shift(yy_state_45)
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    _ => Error
  }
}

// [236, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → COLON separated_nonempty_list(PLUS,qualified_uident) •, EOF / SEMI / LBRACE]
fn yy_state_44(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_269)
}

// [233, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident •, EOF / SEMI / LBRACE]
// [234, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident • PLUS separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
fn yy_state_45(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS => Shift(yy_state_46)
    T_EOF | T_SEMI | T_LBRACE => Reduce(1, NT_separated_nonempty_list_PLUS_qualified_uident_, yy_action_268)
    _ => Error
  }
}

// [212, qualified_uident → • UIDENT, EOF / SEMI / PLUS / LBRACE]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, EOF / SEMI / PLUS / LBRACE]
// [233, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident, EOF / SEMI / LBRACE]
// [234, separated_nonempty_list(PLUS,qualified_uident) → • qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
// [234, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident PLUS • separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
fn yy_state_46(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_qualified_uident => Shift(yy_state_45)
    NT_separated_nonempty_list_PLUS_qualified_uident_ => Shift(yy_state_47)
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    _ => Error
  }
}

// [234, separated_nonempty_list(PLUS,qualified_uident) → qualified_uident PLUS separated_nonempty_list(PLUS,qualified_uident) •, EOF / SEMI / LBRACE]
fn yy_state_47(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_PLUS_qualified_uident_, yy_action_267)
}

// [213, qualified_uident → PACKAGE_NAME • DOT_UIDENT, EOF / RPAREN / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]
fn yy_state_48(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOT_UIDENT => Shift(yy_state_49)
    _ => Error
  }
}

// [213, qualified_uident → PACKAGE_NAME DOT_UIDENT •, EOF / RPAREN / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]
fn yy_state_49(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_qualified_uident, yy_action_266)
}

// [212, qualified_uident → UIDENT •, EOF / RPAREN / COMMA / SEMI / PLUS / RBRACKET / LBRACE / FOR]
fn yy_state_50(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_qualified_uident, yy_action_265)
}

// [118, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [124, trait_body → •, EOF / SEMI]
// [125, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_51(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_52)
    NT_trait_body => Shift(yy_state_212)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_264)
    _ => Error
  }
}

// [48, trait_method_sig → • ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [49, trait_method_sig → • FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [50, trait_method_sig → • nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [51, trait_method_sig → • nonempty_list(attribute) FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [52, trait_method_sig → • ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [53, trait_method_sig → • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [54, trait_method_sig → • nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [55, trait_method_sig → • nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [125, trait_body → LBRACE • separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
// [237, separated_list(SEMI,trait_method_sig) → •, RBRACE]
// [238, separated_list(SEMI,trait_method_sig) → • separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / ASYNC / FN / EXTEND]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / ASYNC / FN / EXTEND]
// [269, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [270, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_52(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_trait_method_sig => Shift(yy_state_53)
    NT_lident => Shift(yy_state_56)
    NT_nonempty_list_attribute_ => Shift(yy_state_134)
    T_FN => Shift(yy_state_183)
    T_ASYNC => Shift(yy_state_192)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_209)
    NT_separated_list_SEMI_trait_method_sig_ => Shift(yy_state_210)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_trait_method_sig_, yy_action_263)
    _ => Error
  }
}

// [269, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig •, RBRACE]
// [270, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig • SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_53(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_54)
    T_RBRACE => Reduce(1, NT_separated_nonempty_list_SEMI_trait_method_sig_, yy_action_262)
    _ => Error
  }
}

// [48, trait_method_sig → • ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [49, trait_method_sig → • FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [50, trait_method_sig → • nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [51, trait_method_sig → • nonempty_list(attribute) FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [52, trait_method_sig → • ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [53, trait_method_sig → • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [54, trait_method_sig → • nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [55, trait_method_sig → • nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
// [257, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / ASYNC / FN / EXTEND]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / ASYNC / FN / EXTEND]
// [269, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig, RBRACE]
// [270, separated_nonempty_list(SEMI,trait_method_sig) → • trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
// [270, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig SEMI • separated_nonempty_list(SEMI,trait_method_sig), RBRACE]
fn yy_state_54(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_trait_method_sig => Shift(yy_state_53)
    NT_separated_nonempty_list_SEMI_trait_method_sig_ => Shift(yy_state_55)
    NT_lident => Shift(yy_state_56)
    NT_nonempty_list_attribute_ => Shift(yy_state_134)
    T_FN => Shift(yy_state_183)
    T_ASYNC => Shift(yy_state_192)
    _ => Error
  }
}

// [270, separated_nonempty_list(SEMI,trait_method_sig) → trait_method_sig SEMI separated_nonempty_list(SEMI,trait_method_sig) •, RBRACE]
fn yy_state_55(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_trait_method_sig_, yy_action_261)
}

// [53, trait_method_sig → lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_56(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_57)
    _ => Error
  }
}

// [53, trait_method_sig → lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_57(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [263, separated_nonempty_list(COMMA,parameter) → parameter •, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → parameter • COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_58(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_59)
    T_RPAREN => Reduce(1, NT_separated_nonempty_list_COMMA_parameter_, yy_action_259)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → parameter COMMA • separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_59(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_separated_nonempty_list_COMMA_parameter_ => Shift(yy_state_60)
    NT_lident => Shift(yy_state_61)
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

// [264, separated_nonempty_list(COMMA,parameter) → parameter COMMA separated_nonempty_list(COMMA,parameter) •, RPAREN]
fn yy_state_60(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_parameter_, yy_action_258)
}

// [219, label → lident •, QUESTION]
fn yy_state_61(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_label, yy_action_257)
}

// [215, qualified_uident_ → PACKAGE_NAME • DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
fn yy_state_62(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOT_UIDENT => Shift(yy_state_63)
    _ => Error
  }
}

// [215, qualified_uident_ → PACKAGE_NAME DOT_UIDENT •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
fn yy_state_63(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_qualified_uident_, yy_action_256)
}

// [214, qualified_uident_ → UIDENT •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
fn yy_state_64(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_qualified_uident_, yy_action_255)
}

// [196, parameter → label • QUESTION COLON type_, RPAREN / COMMA]
fn yy_state_65(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_66)
    _ => Error
  }
}

// [196, parameter → label QUESTION • COLON type_, RPAREN / COMMA]
fn yy_state_66(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_67)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [196, parameter → label QUESTION COLON • type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
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

// [196, parameter → label QUESTION COLON type_ •, RPAREN / COMMA]
fn yy_state_68(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_parameter, yy_action_254)
}

// [178, type_ → ASYNC • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → ASYNC • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → ASYNC • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → ASYNC • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_69(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_70)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → ASYNC LPAREN • type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → ASYNC LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → ASYNC LPAREN • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → ASYNC LPAREN • type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
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

// [182, type_ → ASYNC LPAREN RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_71(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_72)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → ASYNC LPAREN RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [187, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [188, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
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

// [186, return_type → type_ •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_73(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_return_type, yy_action_253)
}

// [182, type_ → ASYNC LPAREN RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_74(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_, yy_action_252)
}

// [176, simple_type → UNDERSCORE •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
fn yy_state_75(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_simple_type, yy_action_251)
}

// [175, simple_type → AMPER • qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
fn yy_state_76(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_qualified_uident_ => Shift(yy_state_77)
    _ => Error
  }
}

// [175, simple_type → AMPER qualified_uident_ •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
fn yy_state_77(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_simple_type, yy_action_250)
}

// [174, simple_type → qualified_uident_ • optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [192, optional_type_arguments → • LBRACKET separated_nonempty_list(COMMA,type_) RBRACKET, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [193, optional_type_arguments → •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
fn yy_state_78(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACKET => Shift(yy_state_79)
    NT_optional_type_arguments => Shift(yy_state_109)
    T_EOF | T_RAISE | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_SEMI | T_RBRACKET | T_RBRACE | T_NORAISE => Reduce(0, NT_optional_type_arguments, yy_action_249)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, COMMA / QUESTION / RBRACKET]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, COMMA / QUESTION / RBRACKET]
// [173, simple_type → • LPAREN type_ RPAREN, COMMA / QUESTION / RBRACKET]
// [174, simple_type → • qualified_uident_ optional_type_arguments, COMMA / QUESTION / RBRACKET]
// [175, simple_type → • AMPER qualified_uident_, COMMA / QUESTION / RBRACKET]
// [176, simple_type → • UNDERSCORE, COMMA / QUESTION / RBRACKET]
// [177, type_ → • simple_type, COMMA / RBRACKET]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, COMMA / RBRACKET]
// [192, optional_type_arguments → LBRACKET • separated_nonempty_list(COMMA,type_) RBRACKET, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [214, qualified_uident_ → • UIDENT, COMMA / QUESTION / LBRACKET / RBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, COMMA / QUESTION / LBRACKET / RBRACKET]
// [255, separated_nonempty_list(COMMA,type_) → • type_, RBRACKET]
// [256, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RBRACKET]
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

// [255, separated_nonempty_list(COMMA,type_) → type_ •, RPAREN / RBRACKET]
// [256, separated_nonempty_list(COMMA,type_) → type_ • COMMA separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]
fn yy_state_80(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_81)
    T_RPAREN | T_RBRACKET => Reduce(1, NT_separated_nonempty_list_COMMA_type__, yy_action_248)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION / RBRACKET]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION / RBRACKET]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION / RBRACKET]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION / RBRACKET]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION / RBRACKET]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION / RBRACKET]
// [177, type_ → • simple_type, RPAREN / COMMA / RBRACKET]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA / RBRACKET]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET / RBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET / RBRACKET]
// [255, separated_nonempty_list(COMMA,type_) → • type_, RPAREN / RBRACKET]
// [256, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]
// [256, separated_nonempty_list(COMMA,type_) → type_ COMMA • separated_nonempty_list(COMMA,type_), RPAREN / RBRACKET]
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

// [256, separated_nonempty_list(COMMA,type_) → type_ COMMA separated_nonempty_list(COMMA,type_) •, RPAREN / RBRACKET]
fn yy_state_82(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_type__, yy_action_247)
}

// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [172, simple_type → LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → LPAREN • type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → LPAREN • type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → LPAREN • type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → LPAREN • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → LPAREN • type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
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

// [183, type_ → LPAREN RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_84(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_85)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, type_ → LPAREN RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [187, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [188, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
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

// [183, type_ → LPAREN RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_86(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_, yy_action_246)
}

// [171, simple_type → simple_type • QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → simple_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [187, return_type → simple_type • NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [188, return_type → simple_type • RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [189, return_type → simple_type • RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [190, return_type → simple_type • RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_87(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RAISE => Shift(yy_state_88)
    T_NORAISE => Shift(yy_state_92)
    T_QUESTION => Shift(yy_state_93)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(1, NT_type_, yy_action_245)
    _ => Error
  }
}

// [188, return_type → simple_type RAISE •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [189, return_type → simple_type RAISE • error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [190, return_type → simple_type RAISE • QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [191, error_type → • qualified_uident_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [214, qualified_uident_ → • UIDENT, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_88(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    NT_qualified_uident_ => Shift(yy_state_89)
    T_QUESTION => Shift(yy_state_90)
    NT_error_type => Shift(yy_state_91)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(2, NT_return_type, yy_action_244)
    _ => Error
  }
}

// [191, error_type → qualified_uident_ •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_89(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_error_type, yy_action_243)
}

// [190, return_type → simple_type RAISE QUESTION •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_90(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_return_type, yy_action_242)
}

// [189, return_type → simple_type RAISE error_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_91(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_return_type, yy_action_241)
}

// [187, return_type → simple_type NORAISE •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_92(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_return_type, yy_action_240)
}

// [171, simple_type → simple_type QUESTION •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
fn yy_state_93(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_simple_type, yy_action_239)
}

// [172, simple_type → LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → LPAREN type_ • RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [180, type_ → LPAREN type_ • COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → LPAREN type_ • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_94(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_95)
    T_COMMA => Shift(yy_state_98)
    _ => Error
  }
}

// [173, simple_type → LPAREN type_ RPAREN •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [185, type_ → LPAREN type_ RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_95(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_96)
    T_EOF | T_RAISE | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_SEMI | T_RBRACKET | T_RBRACE | T_NORAISE => Reduce(3, NT_simple_type, yy_action_238)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → LPAREN type_ RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [187, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [188, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
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

// [185, type_ → LPAREN type_ RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_97(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_, yy_action_237)
}

// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [172, simple_type → LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → LPAREN type_ COMMA • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [255, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [256, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
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

// [180, type_ → LPAREN type_ COMMA RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_99(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_100)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → LPAREN type_ COMMA RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [187, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [188, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
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

// [180, type_ → LPAREN type_ COMMA RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_101(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_, yy_action_236)
}

// [172, simple_type → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [181, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_102(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_103)
    _ => Error
  }
}

// [172, simple_type → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [181, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_103(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_104)
    T_EOF | T_RAISE | T_EQUAL | T_RPAREN | T_COMMA | T_QUESTION | T_SEMI | T_RBRACKET | T_RBRACE | T_NORAISE => Reduce(5, NT_simple_type, yy_action_235)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [187, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [188, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
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

// [181, type_ → LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_105(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_, yy_action_234)
}

// [171, simple_type → simple_type • QUESTION, EOF / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE]
// [177, type_ → simple_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_106(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_QUESTION => Shift(yy_state_93)
    T_EOF | T_EQUAL | T_RPAREN | T_COMMA | T_SEMI | T_RBRACKET | T_RBRACE => Reduce(1, NT_type_, yy_action_245)
    _ => Error
  }
}

// [192, optional_type_arguments → LBRACKET separated_nonempty_list(COMMA,type_) • RBRACKET, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
fn yy_state_107(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_108)
    _ => Error
  }
}

// [192, optional_type_arguments → LBRACKET separated_nonempty_list(COMMA,type_) RBRACKET •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
fn yy_state_108(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_optional_type_arguments, yy_action_232)
}

// [174, simple_type → qualified_uident_ optional_type_arguments •, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
fn yy_state_109(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_simple_type, yy_action_231)
}

// [178, type_ → ASYNC LPAREN type_ • COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → ASYNC LPAREN type_ • COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → ASYNC LPAREN type_ • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_110(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_111)
    T_COMMA => Shift(yy_state_114)
    _ => Error
  }
}

// [184, type_ → ASYNC LPAREN type_ RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_111(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_112)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → ASYNC LPAREN type_ RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [187, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [188, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
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

// [184, type_ → ASYNC LPAREN type_ RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_113(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_, yy_action_230)
}

// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [178, type_ → ASYNC LPAREN type_ COMMA • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → ASYNC LPAREN type_ COMMA • separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [255, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [256, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
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

// [179, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) • RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_115(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_116)
    _ => Error
  }
}

// [179, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_116(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_117)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [187, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [188, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
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

// [179, type_ → ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_118(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_, yy_action_228)
}

// [178, type_ → ASYNC LPAREN type_ COMMA RPAREN • THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_119(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_120)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / RBRACKET / RBRACE / NORAISE]
// [177, type_ → • simple_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [178, type_ → ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW • return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [186, return_type → • type_, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [187, return_type → • simple_type NORAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [188, return_type → • simple_type RAISE, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / EQUAL / RPAREN / COMMA / QUESTION / SEMI / LBRACKET / RBRACKET / RBRACE / NORAISE]
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

// [178, type_ → ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type •, EOF / EQUAL / RPAREN / COMMA / SEMI / RBRACKET / RBRACE]
fn yy_state_121(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_, yy_action_226)
}

// [195, parameter → POST_LABEL • COLON type_, RPAREN / COMMA]
fn yy_state_122(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_123)
    _ => Error
  }
}

// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [195, parameter → POST_LABEL COLON • type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
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

// [195, parameter → POST_LABEL COLON type_ •, RPAREN / COMMA]
fn yy_state_124(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_parameter, yy_action_225)
}

// [194, parameter → type_ •, RPAREN / COMMA]
fn yy_state_125(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_parameter, yy_action_224)
}

// [225, separated_list(COMMA,parameter) → separated_nonempty_list(COMMA,parameter) •, RPAREN]
fn yy_state_126(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_parameter_, yy_action_223)
}

// [53, trait_method_sig → lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_127(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_128)
    _ => Error
  }
}

// [53, trait_method_sig → lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_128(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_129)
    _ => Error
  }
}

// [53, trait_method_sig → lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [177, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • type_, EQUAL / SEMI / RBRACE]
// [187, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [188, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [214, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
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

// [53, trait_method_sig → lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [226, boption(eq_underscore) → •, SEMI / RBRACE]
// [227, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]
fn yy_state_130(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_133)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_222)
    _ => Error
  }
}

// [227, boption(eq_underscore) → EQUAL • UNDERSCORE, SEMI / RBRACE]
fn yy_state_131(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UNDERSCORE => Shift(yy_state_132)
    _ => Error
  }
}

// [227, boption(eq_underscore) → EQUAL UNDERSCORE •, SEMI / RBRACE]
fn yy_state_132(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_boption_eq_underscore_, yy_action_220)
}

// [53, trait_method_sig → lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]
fn yy_state_133(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_trait_method_sig, yy_action_219)
}

// [50, trait_method_sig → nonempty_list(attribute) • ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [51, trait_method_sig → nonempty_list(attribute) • FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [54, trait_method_sig → nonempty_list(attribute) • ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [55, trait_method_sig → nonempty_list(attribute) • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_134(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_135)
    T_FN => Shift(yy_state_142)
    T_ASYNC => Shift(yy_state_166)
    _ => Error
  }
}

// [55, trait_method_sig → nonempty_list(attribute) lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_135(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_136)
    _ => Error
  }
}

// [55, trait_method_sig → nonempty_list(attribute) lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_136(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [55, trait_method_sig → nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_137(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_138)
    _ => Error
  }
}

// [55, trait_method_sig → nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_138(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_139)
    _ => Error
  }
}

// [55, trait_method_sig → nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [177, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • type_, EQUAL / SEMI / RBRACE]
// [187, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [188, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [214, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
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

// [55, trait_method_sig → nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [226, boption(eq_underscore) → •, SEMI / RBRACE]
// [227, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]
fn yy_state_140(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_141)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_222)
    _ => Error
  }
}

// [55, trait_method_sig → nonempty_list(attribute) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]
fn yy_state_141(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_trait_method_sig, yy_action_218)
}

// [51, trait_method_sig → nonempty_list(attribute) FN • loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / EXTEND]
fn yy_state_142(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_158)
    T_LIDENT | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [223, loption(type_params_with_constraints) → type_params_with_constraints •, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_143(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_loption_type_params_with_constraints_, yy_action_216)
}

// [159, type_param_with_constraints → • uident, COMMA / RBRACKET]
// [160, type_param_with_constraints → • uident COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [161, type_params_with_constraints → LBRACKET • separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / PACKAGE_NAME / EXTEND]
// [216, uident → • UIDENT, COMMA / COLON / RBRACKET]
// [251, separated_list(COMMA,type_param_with_constraints) → •, RBRACKET]
// [252, separated_list(COMMA,type_param_with_constraints) → • separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
// [273, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints, RBRACKET]
// [274, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
fn yy_state_144(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_type_param_with_constraints => Shift(yy_state_145)
    NT_uident => Shift(yy_state_148)
    NT_separated_nonempty_list_COMMA_type_param_with_constraints_ => Shift(yy_state_155)
    NT_separated_list_COMMA_type_param_with_constraints_ => Shift(yy_state_156)
    T_RBRACKET => Reduce(0, NT_separated_list_COMMA_type_param_with_constraints_, yy_action_215)
    _ => Error
  }
}

// [273, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints •, RBRACKET]
// [274, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints • COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
fn yy_state_145(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_146)
    T_RBRACKET => Reduce(1, NT_separated_nonempty_list_COMMA_type_param_with_constraints_, yy_action_214)
    _ => Error
  }
}

// [159, type_param_with_constraints → • uident, COMMA / RBRACKET]
// [160, type_param_with_constraints → • uident COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [216, uident → • UIDENT, COMMA / COLON / RBRACKET]
// [273, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints, RBRACKET]
// [274, separated_nonempty_list(COMMA,type_param_with_constraints) → • type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
// [274, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints COMMA • separated_nonempty_list(COMMA,type_param_with_constraints), RBRACKET]
fn yy_state_146(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_type_param_with_constraints => Shift(yy_state_145)
    NT_separated_nonempty_list_COMMA_type_param_with_constraints_ => Shift(yy_state_147)
    NT_uident => Shift(yy_state_148)
    _ => Error
  }
}

// [274, separated_nonempty_list(COMMA,type_param_with_constraints) → type_param_with_constraints COMMA separated_nonempty_list(COMMA,type_param_with_constraints) •, RBRACKET]
fn yy_state_147(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_type_param_with_constraints_, yy_action_213)
}

// [159, type_param_with_constraints → uident •, COMMA / RBRACKET]
// [160, type_param_with_constraints → uident • COLON separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_148(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_149)
    T_COMMA | T_RBRACKET => Reduce(1, NT_type_param_with_constraints, yy_action_212)
    _ => Error
  }
}

// [160, type_param_with_constraints → uident COLON • separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [167, type_constraint → • qualified_uident, COMMA / PLUS / RBRACKET]
// [212, qualified_uident → • UIDENT, COMMA / PLUS / RBRACKET]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, COMMA / PLUS / RBRACKET]
// [249, separated_nonempty_list(PLUS,type_constraint) → • type_constraint, COMMA / RBRACKET]
// [250, separated_nonempty_list(PLUS,type_constraint) → • type_constraint PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_149(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    NT_type_constraint => Shift(yy_state_150)
    NT_qualified_uident => Shift(yy_state_153)
    NT_separated_nonempty_list_PLUS_type_constraint_ => Shift(yy_state_154)
    _ => Error
  }
}

// [249, separated_nonempty_list(PLUS,type_constraint) → type_constraint •, COMMA / RBRACKET]
// [250, separated_nonempty_list(PLUS,type_constraint) → type_constraint • PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_150(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS => Shift(yy_state_151)
    T_COMMA | T_RBRACKET => Reduce(1, NT_separated_nonempty_list_PLUS_type_constraint_, yy_action_211)
    _ => Error
  }
}

// [167, type_constraint → • qualified_uident, COMMA / PLUS / RBRACKET]
// [212, qualified_uident → • UIDENT, COMMA / PLUS / RBRACKET]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, COMMA / PLUS / RBRACKET]
// [249, separated_nonempty_list(PLUS,type_constraint) → • type_constraint, COMMA / RBRACKET]
// [250, separated_nonempty_list(PLUS,type_constraint) → • type_constraint PLUS separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
// [250, separated_nonempty_list(PLUS,type_constraint) → type_constraint PLUS • separated_nonempty_list(PLUS,type_constraint), COMMA / RBRACKET]
fn yy_state_151(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    NT_type_constraint => Shift(yy_state_150)
    NT_separated_nonempty_list_PLUS_type_constraint_ => Shift(yy_state_152)
    NT_qualified_uident => Shift(yy_state_153)
    _ => Error
  }
}

// [250, separated_nonempty_list(PLUS,type_constraint) → type_constraint PLUS separated_nonempty_list(PLUS,type_constraint) •, COMMA / RBRACKET]
fn yy_state_152(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_PLUS_type_constraint_, yy_action_210)
}

// [167, type_constraint → qualified_uident •, COMMA / PLUS / RBRACKET]
fn yy_state_153(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_constraint, yy_action_209)
}

// [160, type_param_with_constraints → uident COLON separated_nonempty_list(PLUS,type_constraint) •, COMMA / RBRACKET]
fn yy_state_154(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_param_with_constraints, yy_action_208)
}

// [252, separated_list(COMMA,type_param_with_constraints) → separated_nonempty_list(COMMA,type_param_with_constraints) •, RBRACKET]
fn yy_state_155(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_type_param_with_constraints_, yy_action_207)
}

// [161, type_params_with_constraints → LBRACKET separated_list(COMMA,type_param_with_constraints) • RBRACKET, LIDENT / UIDENT / AMPER / PACKAGE_NAME / EXTEND]
fn yy_state_156(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_157)
    _ => Error
  }
}

// [161, type_params_with_constraints → LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET •, LIDENT / UIDENT / AMPER / PACKAGE_NAME / EXTEND]
fn yy_state_157(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_params_with_constraints, yy_action_206)
}

// [51, trait_method_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_158(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_159)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_159(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_160)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_160(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_161)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_161(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_162)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_162(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_163)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [177, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • type_, EQUAL / SEMI / RBRACE]
// [187, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [188, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [214, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
fn yy_state_163(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_164)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [226, boption(eq_underscore) → •, SEMI / RBRACE]
// [227, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]
fn yy_state_164(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_165)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_222)
    _ => Error
  }
}

// [51, trait_method_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]
fn yy_state_165(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_trait_method_sig, yy_action_205)
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC • FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [54, trait_method_sig → nonempty_list(attribute) ASYNC • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_166(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_167)
    T_FN => Shift(yy_state_174)
    _ => Error
  }
}

// [54, trait_method_sig → nonempty_list(attribute) ASYNC lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_167(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_168)
    _ => Error
  }
}

// [54, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_168(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_169)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [54, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_169(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_170)
    _ => Error
  }
}

// [54, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_170(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_171)
    _ => Error
  }
}

// [54, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [177, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • type_, EQUAL / SEMI / RBRACE]
// [187, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [188, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [214, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
fn yy_state_171(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_172)
    _ => Error
  }
}

// [54, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [226, boption(eq_underscore) → •, SEMI / RBRACE]
// [227, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]
fn yy_state_172(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_173)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_222)
    _ => Error
  }
}

// [54, trait_method_sig → nonempty_list(attribute) ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]
fn yy_state_173(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_trait_method_sig, yy_action_204)
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC FN • loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / EXTEND]
fn yy_state_174(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_175)
    T_LIDENT | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_175(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_176)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_176(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_177)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_177(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_178)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_178(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_179)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_179(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_180)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [177, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • type_, EQUAL / SEMI / RBRACE]
// [187, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [188, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [214, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
fn yy_state_180(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_181)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [226, boption(eq_underscore) → •, SEMI / RBRACE]
// [227, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]
fn yy_state_181(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_182)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_222)
    _ => Error
  }
}

// [50, trait_method_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]
fn yy_state_182(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(11, NT_trait_method_sig, yy_action_203)
}

// [49, trait_method_sig → FN • loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / EXTEND]
fn yy_state_183(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_184)
    T_LIDENT | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [49, trait_method_sig → FN loption(type_params_with_constraints) • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_184(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_185)
    _ => Error
  }
}

// [49, trait_method_sig → FN loption(type_params_with_constraints) lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_185(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_186)
    _ => Error
  }
}

// [49, trait_method_sig → FN loption(type_params_with_constraints) lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_186(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_187)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [49, trait_method_sig → FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_187(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_188)
    _ => Error
  }
}

// [49, trait_method_sig → FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_188(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_189)
    _ => Error
  }
}

// [49, trait_method_sig → FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [177, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • type_, EQUAL / SEMI / RBRACE]
// [187, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [188, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [214, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
fn yy_state_189(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_190)
    _ => Error
  }
}

// [49, trait_method_sig → FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [226, boption(eq_underscore) → •, SEMI / RBRACE]
// [227, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]
fn yy_state_190(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_191)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_222)
    _ => Error
  }
}

// [49, trait_method_sig → FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]
fn yy_state_191(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_trait_method_sig, yy_action_202)
}

// [48, trait_method_sig → ASYNC • FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [52, trait_method_sig → ASYNC • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_192(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_193)
    T_FN => Shift(yy_state_200)
    _ => Error
  }
}

// [52, trait_method_sig → ASYNC lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_193(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_194)
    _ => Error
  }
}

// [52, trait_method_sig → ASYNC lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_194(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_195)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [52, trait_method_sig → ASYNC lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_195(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_196)
    _ => Error
  }
}

// [52, trait_method_sig → ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_196(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_197)
    _ => Error
  }
}

// [52, trait_method_sig → ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [177, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • type_, EQUAL / SEMI / RBRACE]
// [187, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [188, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [214, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
fn yy_state_197(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_198)
    _ => Error
  }
}

// [52, trait_method_sig → ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [226, boption(eq_underscore) → •, SEMI / RBRACE]
// [227, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]
fn yy_state_198(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_199)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_222)
    _ => Error
  }
}

// [52, trait_method_sig → ASYNC lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]
fn yy_state_199(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_trait_method_sig, yy_action_201)
}

// [48, trait_method_sig → ASYNC FN • loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / EXTEND]
fn yy_state_200(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_201)
    T_LIDENT | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC FN loption(type_params_with_constraints) • lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_201(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_202)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC FN loption(type_params_with_constraints) lident • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_202(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_203)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC FN loption(type_params_with_constraints) lident LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_203(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_204)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_204(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_205)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type boption(eq_underscore), SEMI / RBRACE]
fn yy_state_205(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_206)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type boption(eq_underscore), SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [176, simple_type → • UNDERSCORE, RAISE / EQUAL / QUESTION / SEMI / RBRACE / NORAISE]
// [177, type_ → • simple_type, EQUAL / SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL / SEMI / RBRACE]
// [186, return_type → • type_, EQUAL / SEMI / RBRACE]
// [187, return_type → • simple_type NORAISE, EQUAL / SEMI / RBRACE]
// [188, return_type → • simple_type RAISE, EQUAL / SEMI / RBRACE]
// [189, return_type → • simple_type RAISE error_type, EQUAL / SEMI / RBRACE]
// [190, return_type → • simple_type RAISE QUESTION, EQUAL / SEMI / RBRACE]
// [214, qualified_uident_ → • UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RAISE / EQUAL / QUESTION / SEMI / LBRACKET / RBRACE / NORAISE]
fn yy_state_206(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_207)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type • boption(eq_underscore), SEMI / RBRACE]
// [226, boption(eq_underscore) → •, SEMI / RBRACE]
// [227, boption(eq_underscore) → • EQUAL UNDERSCORE, SEMI / RBRACE]
fn yy_state_207(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_131)
    NT_boption_eq_underscore_ => Shift(yy_state_208)
    T_SEMI | T_RBRACE => Reduce(0, NT_boption_eq_underscore_, yy_action_222)
    _ => Error
  }
}

// [48, trait_method_sig → ASYNC FN loption(type_params_with_constraints) lident LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type boption(eq_underscore) •, SEMI / RBRACE]
fn yy_state_208(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_trait_method_sig, yy_action_200)
}

// [238, separated_list(SEMI,trait_method_sig) → separated_nonempty_list(SEMI,trait_method_sig) •, RBRACE]
fn yy_state_209(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_SEMI_trait_method_sig_, yy_action_199)
}

// [125, trait_body → LBRACE separated_list(SEMI,trait_method_sig) • RBRACE, EOF / SEMI]
fn yy_state_210(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_211)
    _ => Error
  }
}

// [125, trait_body → LBRACE separated_list(SEMI,trait_method_sig) RBRACE •, EOF / SEMI]
fn yy_state_211(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_trait_body, yy_action_198)
}

// [118, trait_sig → TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]
fn yy_state_212(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_trait_sig, yy_action_197)
}

// [106, impl_sig → IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [112, impl_sig → IMPL • qualified_uident FOR type_, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_213(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    T_LBRACKET => Shift(yy_state_144)
    NT_qualified_uident => Shift(yy_state_214)
    NT_type_params_with_constraints => Shift(yy_state_217)
    _ => Error
  }
}

// [112, impl_sig → IMPL qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_214(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_215)
    _ => Error
  }
}

// [112, impl_sig → IMPL qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_215(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_216)
    _ => Error
  }
}

// [112, impl_sig → IMPL qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_216(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_impl_sig, yy_action_196)
}

// [106, impl_sig → IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_217(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    NT_qualified_uident => Shift(yy_state_218)
    _ => Error
  }
}

// [106, impl_sig → IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_218(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_219)
    _ => Error
  }
}

// [106, impl_sig → IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_219(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_220)
    _ => Error
  }
}

// [106, impl_sig → IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_220(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_impl_sig, yy_action_195)
}

// [92, type_sig → EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [98, type_sig → EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [169, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [170, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [216, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]
fn yy_state_221(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_222)
    NT_uident => Shift(yy_state_235)
    NT_extensible_enum_extension_target => Shift(yy_state_237)
    NT_type_decl_name_with_params => Shift(yy_state_278)
    _ => Error
  }
}

// [170, extensible_enum_extension_target → PACKAGE_NAME • DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
fn yy_state_222(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOT_UIDENT => Shift(yy_state_223)
    _ => Error
  }
}

// [164, type_params_no_constraints → • LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET, PLUS_EQUAL]
// [165, optional_type_params_no_constraints → •, PLUS_EQUAL]
// [166, optional_type_params_no_constraints → • type_params_no_constraints, PLUS_EQUAL]
// [170, extensible_enum_extension_target → PACKAGE_NAME DOT_UIDENT • optional_type_params_no_constraints, PLUS_EQUAL]
fn yy_state_223(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_optional_type_params_no_constraints => Shift(yy_state_224)
    NT_type_params_no_constraints => Shift(yy_state_225)
    T_LBRACKET => Shift(yy_state_226)
    T_PLUS_EQUAL => Reduce(0, NT_optional_type_params_no_constraints, yy_action_194)
    _ => Error
  }
}

// [170, extensible_enum_extension_target → PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints •, PLUS_EQUAL]
fn yy_state_224(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_extensible_enum_extension_target, yy_action_193)
}

// [166, optional_type_params_no_constraints → type_params_no_constraints •, PLUS_EQUAL / EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
fn yy_state_225(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_optional_type_params_no_constraints, yy_action_192)
}

// [162, type_param_no_constraints → • uident, COMMA / RBRACKET]
// [163, type_param_no_constraints → • UNDERSCORE, COMMA / RBRACKET]
// [164, type_params_no_constraints → LBRACKET • separated_list(COMMA,type_param_no_constraints) RBRACKET, PLUS_EQUAL / EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
// [216, uident → • UIDENT, COMMA / RBRACKET]
// [253, separated_list(COMMA,type_param_no_constraints) → •, RBRACKET]
// [254, separated_list(COMMA,type_param_no_constraints) → • separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
// [275, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints, RBRACKET]
// [276, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
fn yy_state_226(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_type_param_no_constraints => Shift(yy_state_227)
    T_UNDERSCORE => Shift(yy_state_230)
    NT_uident => Shift(yy_state_231)
    NT_separated_nonempty_list_COMMA_type_param_no_constraints_ => Shift(yy_state_232)
    NT_separated_list_COMMA_type_param_no_constraints_ => Shift(yy_state_233)
    T_RBRACKET => Reduce(0, NT_separated_list_COMMA_type_param_no_constraints_, yy_action_191)
    _ => Error
  }
}

// [275, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints •, RBRACKET]
// [276, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints • COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
fn yy_state_227(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_228)
    T_RBRACKET => Reduce(1, NT_separated_nonempty_list_COMMA_type_param_no_constraints_, yy_action_190)
    _ => Error
  }
}

// [162, type_param_no_constraints → • uident, COMMA / RBRACKET]
// [163, type_param_no_constraints → • UNDERSCORE, COMMA / RBRACKET]
// [216, uident → • UIDENT, COMMA / RBRACKET]
// [275, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints, RBRACKET]
// [276, separated_nonempty_list(COMMA,type_param_no_constraints) → • type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
// [276, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints COMMA • separated_nonempty_list(COMMA,type_param_no_constraints), RBRACKET]
fn yy_state_228(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_type_param_no_constraints => Shift(yy_state_227)
    NT_separated_nonempty_list_COMMA_type_param_no_constraints_ => Shift(yy_state_229)
    T_UNDERSCORE => Shift(yy_state_230)
    NT_uident => Shift(yy_state_231)
    _ => Error
  }
}

// [276, separated_nonempty_list(COMMA,type_param_no_constraints) → type_param_no_constraints COMMA separated_nonempty_list(COMMA,type_param_no_constraints) •, RBRACKET]
fn yy_state_229(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_type_param_no_constraints_, yy_action_189)
}

// [163, type_param_no_constraints → UNDERSCORE •, COMMA / RBRACKET]
fn yy_state_230(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_param_no_constraints, yy_action_188)
}

// [162, type_param_no_constraints → uident •, COMMA / RBRACKET]
fn yy_state_231(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_type_param_no_constraints, yy_action_187)
}

// [254, separated_list(COMMA,type_param_no_constraints) → separated_nonempty_list(COMMA,type_param_no_constraints) •, RBRACKET]
fn yy_state_232(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_type_param_no_constraints_, yy_action_186)
}

// [164, type_params_no_constraints → LBRACKET separated_list(COMMA,type_param_no_constraints) • RBRACKET, PLUS_EQUAL / EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
fn yy_state_233(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACKET => Shift(yy_state_234)
    _ => Error
  }
}

// [164, type_params_no_constraints → LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET •, PLUS_EQUAL / EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
fn yy_state_234(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_params_no_constraints, yy_action_185)
}

// [164, type_params_no_constraints → • LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET, PLUS_EQUAL / LBRACE]
// [165, optional_type_params_no_constraints → •, PLUS_EQUAL / LBRACE]
// [166, optional_type_params_no_constraints → • type_params_no_constraints, PLUS_EQUAL / LBRACE]
// [168, type_decl_name_with_params → uident • optional_type_params_no_constraints, LBRACE]
// [169, extensible_enum_extension_target → uident • optional_type_params_no_constraints, PLUS_EQUAL]
fn yy_state_235(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_no_constraints => Shift(yy_state_225)
    T_LBRACKET => Shift(yy_state_226)
    NT_optional_type_params_no_constraints => Shift(yy_state_236)
    T_PLUS_EQUAL | T_LBRACE => Reduce(0, NT_optional_type_params_no_constraints, yy_action_194)
    _ => Error
  }
}

// [168, type_decl_name_with_params → uident optional_type_params_no_constraints •, LBRACE]
// [169, extensible_enum_extension_target → uident optional_type_params_no_constraints •, PLUS_EQUAL]
fn yy_state_236(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Reduce(2, NT_type_decl_name_with_params, yy_action_183)
    T_PLUS_EQUAL => Reduce(2, NT_extensible_enum_extension_target, yy_action_184)
    _ => Error
  }
}

// [98, type_sig → EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_237(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_238)
    _ => Error
  }
}

// [98, type_sig → EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_238(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_239)
    _ => Error
  }
}

// [98, type_sig → EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_239(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_266)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [265, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor •, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor • SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_240(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_241)
    T_RBRACE => Reduce(1, NT_separated_nonempty_list_SEMI_enum_constructor_, yy_action_181)
    _ => Error
  }
}

// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor SEMI • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_241(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_242)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    _ => Error
  }
}

// [266, separated_nonempty_list(SEMI,enum_constructor) → enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor) •, RBRACE]
fn yy_state_242(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_enum_constructor_, yy_action_180)
}

// [153, enum_constructor → nonempty_list(attribute) • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
fn yy_state_243(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_244)
    _ => Error
  }
}

// [153, enum_constructor → nonempty_list(attribute) UIDENT • option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [241, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → •, EQUAL / SEMI / RBRACE]
// [242, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → • LPAREN separated_nonempty_list(COMMA,constructor_param) RPAREN, EQUAL / SEMI / RBRACE]
fn yy_state_244(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_245)
    NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__ => Shift(yy_state_258)
    T_EQUAL | T_SEMI | T_RBRACE => Reduce(0, NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__, yy_action_179)
    _ => Error
  }
}

// [154, constructor_param → • boption(MUTABLE) type_, RPAREN / COMMA]
// [155, constructor_param → • boption(MUTABLE) post_label COLON type_, RPAREN / COMMA]
// [239, separated_nonempty_list(COMMA,constructor_param) → • constructor_param, RPAREN]
// [240, separated_nonempty_list(COMMA,constructor_param) → • constructor_param COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [242, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN • separated_nonempty_list(COMMA,constructor_param) RPAREN, EQUAL / SEMI / RBRACE]
// [245, boption(MUTABLE) → •, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
// [246, boption(MUTABLE) → • MUTABLE, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_245(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_246)
    NT_separated_nonempty_list_COMMA_constructor_param_ => Shift(yy_state_247)
    NT_constructor_param => Shift(yy_state_249)
    NT_boption_MUTABLE_ => Shift(yy_state_252)
    T_UIDENT | T_POST_LABEL | T_ASYNC | T_LPAREN | T_UNDERSCORE | T_AMPER | T_PACKAGE_NAME => Reduce(0, NT_boption_MUTABLE_, yy_action_178)
    _ => Error
  }
}

// [246, boption(MUTABLE) → MUTABLE •, LIDENT / UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME / EXTEND]
fn yy_state_246(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_boption_MUTABLE_, yy_action_177)
}

// [242, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN separated_nonempty_list(COMMA,constructor_param) • RPAREN, EQUAL / SEMI / RBRACE]
fn yy_state_247(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_248)
    _ => Error
  }
}

// [242, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → LPAREN separated_nonempty_list(COMMA,constructor_param) RPAREN •, EQUAL / SEMI / RBRACE]
fn yy_state_248(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__, yy_action_175)
}

// [239, separated_nonempty_list(COMMA,constructor_param) → constructor_param •, RPAREN]
// [240, separated_nonempty_list(COMMA,constructor_param) → constructor_param • COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
fn yy_state_249(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_250)
    T_RPAREN => Reduce(1, NT_separated_nonempty_list_COMMA_constructor_param_, yy_action_174)
    _ => Error
  }
}

// [154, constructor_param → • boption(MUTABLE) type_, RPAREN / COMMA]
// [155, constructor_param → • boption(MUTABLE) post_label COLON type_, RPAREN / COMMA]
// [239, separated_nonempty_list(COMMA,constructor_param) → • constructor_param, RPAREN]
// [240, separated_nonempty_list(COMMA,constructor_param) → • constructor_param COMMA separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [240, separated_nonempty_list(COMMA,constructor_param) → constructor_param COMMA • separated_nonempty_list(COMMA,constructor_param), RPAREN]
// [245, boption(MUTABLE) → •, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
// [246, boption(MUTABLE) → • MUTABLE, UIDENT / POST_LABEL / ASYNC / LPAREN / UNDERSCORE / AMPER / PACKAGE_NAME]
fn yy_state_250(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_246)
    NT_constructor_param => Shift(yy_state_249)
    NT_separated_nonempty_list_COMMA_constructor_param_ => Shift(yy_state_251)
    NT_boption_MUTABLE_ => Shift(yy_state_252)
    T_UIDENT | T_POST_LABEL | T_ASYNC | T_LPAREN | T_UNDERSCORE | T_AMPER | T_PACKAGE_NAME => Reduce(0, NT_boption_MUTABLE_, yy_action_178)
    _ => Error
  }
}

// [240, separated_nonempty_list(COMMA,constructor_param) → constructor_param COMMA separated_nonempty_list(COMMA,constructor_param) •, RPAREN]
fn yy_state_251(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_COMMA_constructor_param_, yy_action_173)
}

// [154, constructor_param → boption(MUTABLE) • type_, RPAREN / COMMA]
// [155, constructor_param → boption(MUTABLE) • post_label COLON type_, RPAREN / COMMA]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [220, post_label → • POST_LABEL, COLON]
fn yy_state_252(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    T_POST_LABEL => Shift(yy_state_253)
    NT_post_label => Shift(yy_state_254)
    NT_type_ => Shift(yy_state_257)
    _ => Error
  }
}

// [220, post_label → POST_LABEL •, COLON]
fn yy_state_253(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_post_label, yy_action_172)
}

// [155, constructor_param → boption(MUTABLE) post_label • COLON type_, RPAREN / COMMA]
fn yy_state_254(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_255)
    _ => Error
  }
}

// [155, constructor_param → boption(MUTABLE) post_label COLON • type_, RPAREN / COMMA]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
fn yy_state_255(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_256)
    _ => Error
  }
}

// [155, constructor_param → boption(MUTABLE) post_label COLON type_ •, RPAREN / COMMA]
fn yy_state_256(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_constructor_param, yy_action_171)
}

// [154, constructor_param → boption(MUTABLE) type_ •, RPAREN / COMMA]
fn yy_state_257(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_constructor_param, yy_action_170)
}

// [153, enum_constructor → nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) • option(eq_tag), SEMI / RBRACE]
// [243, option(eq_tag) → •, SEMI / RBRACE]
// [244, option(eq_tag) → • EQUAL INT, SEMI / RBRACE]
fn yy_state_258(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_259)
    NT_option_eq_tag_ => Shift(yy_state_261)
    T_SEMI | T_RBRACE => Reduce(0, NT_option_eq_tag_, yy_action_169)
    _ => Error
  }
}

// [244, option(eq_tag) → EQUAL • INT, SEMI / RBRACE]
fn yy_state_259(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_INT => Shift(yy_state_260)
    _ => Error
  }
}

// [244, option(eq_tag) → EQUAL INT •, SEMI / RBRACE]
fn yy_state_260(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_option_eq_tag_, yy_action_167)
}

// [153, enum_constructor → nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag) •, SEMI / RBRACE]
fn yy_state_261(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_enum_constructor, yy_action_166)
}

// [152, enum_constructor → UIDENT • option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [241, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → •, EQUAL / SEMI / RBRACE]
// [242, option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) → • LPAREN separated_nonempty_list(COMMA,constructor_param) RPAREN, EQUAL / SEMI / RBRACE]
fn yy_state_262(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_245)
    NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__ => Shift(yy_state_263)
    T_EQUAL | T_SEMI | T_RBRACE => Reduce(0, NT_option_delimited_LPAREN_separated_nonempty_list_COMMA_constructor_param__RPAREN__, yy_action_179)
    _ => Error
  }
}

// [152, enum_constructor → UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) • option(eq_tag), SEMI / RBRACE]
// [243, option(eq_tag) → •, SEMI / RBRACE]
// [244, option(eq_tag) → • EQUAL INT, SEMI / RBRACE]
fn yy_state_263(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_259)
    NT_option_eq_tag_ => Shift(yy_state_264)
    T_SEMI | T_RBRACE => Reduce(0, NT_option_eq_tag_, yy_action_169)
    _ => Error
  }
}

// [152, enum_constructor → UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag) •, SEMI / RBRACE]
fn yy_state_264(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_enum_constructor, yy_action_165)
}

// [229, separated_list(SEMI,enum_constructor) → separated_nonempty_list(SEMI,enum_constructor) •, RBRACE]
fn yy_state_265(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_SEMI_enum_constructor_, yy_action_164)
}

// [98, type_sig → EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_266(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_267)
    _ => Error
  }
}

// [98, type_sig → EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_267(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_277)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [105, optional_derive → DERIVE • LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_268(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_269)
    _ => Error
  }
}

// [105, optional_derive → DERIVE LPAREN • non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [212, qualified_uident → • UIDENT, RPAREN / COMMA]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA]
// [232, non_empty_list_commas(qualified_uident) → • non_empty_list_commas_rev(qualified_uident) option(COMMA), RPAREN]
// [267, non_empty_list_commas_rev(qualified_uident) → • qualified_uident, RPAREN / COMMA]
// [268, non_empty_list_commas_rev(qualified_uident) → • non_empty_list_commas_rev(qualified_uident) COMMA qualified_uident, RPAREN / COMMA]
fn yy_state_269(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    NT_qualified_uident => Shift(yy_state_270)
    NT_non_empty_list_commas_rev_qualified_uident_ => Shift(yy_state_271)
    NT_non_empty_list_commas_qualified_uident_ => Shift(yy_state_275)
    _ => Error
  }
}

// [267, non_empty_list_commas_rev(qualified_uident) → qualified_uident •, RPAREN / COMMA]
fn yy_state_270(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_non_empty_list_commas_rev_qualified_uident_, yy_action_162)
}

// [232, non_empty_list_commas(qualified_uident) → non_empty_list_commas_rev(qualified_uident) • option(COMMA), RPAREN]
// [261, option(COMMA) → •, RPAREN]
// [262, option(COMMA) → • COMMA, RPAREN]
// [268, non_empty_list_commas_rev(qualified_uident) → non_empty_list_commas_rev(qualified_uident) • COMMA qualified_uident, RPAREN / COMMA]
fn yy_state_271(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COMMA => Shift(yy_state_272)
    NT_option_COMMA_ => Shift(yy_state_274)
    T_RPAREN => Reduce(0, NT_option_COMMA_, yy_action_290)
    _ => Error
  }
}

// [212, qualified_uident → • UIDENT, RPAREN / COMMA]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA]
// [262, option(COMMA) → COMMA •, RPAREN]
// [268, non_empty_list_commas_rev(qualified_uident) → non_empty_list_commas_rev(qualified_uident) COMMA • qualified_uident, RPAREN / COMMA]
fn yy_state_272(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    NT_qualified_uident => Shift(yy_state_273)
    T_RPAREN => Reduce(1, NT_option_COMMA_, yy_action_289)
    _ => Error
  }
}

// [268, non_empty_list_commas_rev(qualified_uident) → non_empty_list_commas_rev(qualified_uident) COMMA qualified_uident •, RPAREN / COMMA]
fn yy_state_273(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_non_empty_list_commas_rev_qualified_uident_, yy_action_161)
}

// [232, non_empty_list_commas(qualified_uident) → non_empty_list_commas_rev(qualified_uident) option(COMMA) •, RPAREN]
fn yy_state_274(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_non_empty_list_commas_qualified_uident_, yy_action_160)
}

// [105, optional_derive → DERIVE LPAREN non_empty_list_commas(qualified_uident) • RPAREN, EOF / SEMI]
fn yy_state_275(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_276)
    _ => Error
  }
}

// [105, optional_derive → DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN •, EOF / SEMI]
fn yy_state_276(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_optional_derive, yy_action_159)
}

// [98, type_sig → EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_277(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_158)
}

// [92, type_sig → EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_278(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_279)
    _ => Error
  }
}

// [92, type_sig → EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_279(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_280)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [92, type_sig → EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_280(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_281)
    _ => Error
  }
}

// [92, type_sig → EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_281(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_282)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [92, type_sig → EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_282(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_157)
}

// [86, type_sig → ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [216, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_283(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_286)
    _ => Error
  }
}

// [164, type_params_no_constraints → • LBRACKET separated_list(COMMA,type_param_no_constraints) RBRACKET, EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
// [165, optional_type_params_no_constraints → •, EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
// [166, optional_type_params_no_constraints → • type_params_no_constraints, EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
// [168, type_decl_name_with_params → uident • optional_type_params_no_constraints, EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
fn yy_state_284(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_no_constraints => Shift(yy_state_225)
    T_LBRACKET => Shift(yy_state_226)
    NT_optional_type_params_no_constraints => Shift(yy_state_285)
    T_EOF | T_DERIVE | T_EQUAL | T_LPAREN | T_SEMI | T_LBRACE => Reduce(0, NT_optional_type_params_no_constraints, yy_action_194)
    _ => Error
  }
}

// [168, type_decl_name_with_params → uident optional_type_params_no_constraints •, EOF / DERIVE / EQUAL / LPAREN / SEMI / LBRACE]
fn yy_state_285(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_type_decl_name_with_params, yy_action_183)
}

// [86, type_sig → ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_286(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_287)
    _ => Error
  }
}

// [86, type_sig → ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_287(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_288)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [86, type_sig → ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_288(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_289)
    _ => Error
  }
}

// [86, type_sig → ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_289(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_290)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [86, type_sig → ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_290(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_156)
}

// [74, type_sig → STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [80, type_sig → STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [216, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]
fn yy_state_291(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_292)
    _ => Error
  }
}

// [74, type_sig → STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [80, type_sig → STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
fn yy_state_292(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_293)
    T_LBRACE => Shift(yy_state_298)
    _ => Error
  }
}

// [80, type_sig → STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [230, separated_list(COMMA,type_) → •, RPAREN]
// [231, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [255, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [256, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
fn yy_state_293(_lookahead : YYSymbol) -> YYDecision {
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
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_294)
    NT_separated_list_COMMA_type__ => Shift(yy_state_295)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_155)
    _ => Error
  }
}

// [231, separated_list(COMMA,type_) → separated_nonempty_list(COMMA,type_) •, RPAREN]
fn yy_state_294(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_COMMA_type__, yy_action_154)
}

// [80, type_sig → STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]
fn yy_state_295(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_296)
    _ => Error
  }
}

// [80, type_sig → STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_296(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_297)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [80, type_sig → STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]
fn yy_state_297(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_153)
}

// [74, type_sig → STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [156, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [157, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [158, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [245, boption(MUTABLE) → •, LIDENT / EXTEND]
// [246, boption(MUTABLE) → • MUTABLE, LIDENT / EXTEND]
// [247, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [248, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE / EXTEND]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE / EXTEND]
// [271, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [272, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_298(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_246)
    NT_record_decl_field => Shift(yy_state_299)
    NT_nonempty_list_attribute_ => Shift(yy_state_302)
    NT_boption_MUTABLE_ => Shift(yy_state_307)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_311)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_312)
    NT_record_decl_body => Shift(yy_state_313)
    T_LIDENT | T_EXTEND => Reduce(0, NT_boption_MUTABLE_, yy_action_178)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_152)
    _ => Error
  }
}

// [271, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field •, RBRACE]
// [272, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field • SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_299(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_300)
    T_RBRACE => Reduce(1, NT_separated_nonempty_list_SEMI_record_decl_field_, yy_action_151)
    _ => Error
  }
}

// [156, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [157, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [245, boption(MUTABLE) → •, LIDENT / EXTEND]
// [246, boption(MUTABLE) → • MUTABLE, LIDENT / EXTEND]
// [257, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE / EXTEND]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE / EXTEND]
// [271, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [272, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [272, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field SEMI • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_300(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_246)
    NT_record_decl_field => Shift(yy_state_299)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_301)
    NT_nonempty_list_attribute_ => Shift(yy_state_302)
    NT_boption_MUTABLE_ => Shift(yy_state_307)
    T_LIDENT | T_EXTEND => Reduce(0, NT_boption_MUTABLE_, yy_action_178)
    _ => Error
  }
}

// [272, separated_nonempty_list(SEMI,record_decl_field) → record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field) •, RBRACE]
fn yy_state_301(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_separated_nonempty_list_SEMI_record_decl_field_, yy_action_150)
}

// [157, record_decl_field → nonempty_list(attribute) • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [245, boption(MUTABLE) → •, LIDENT / EXTEND]
// [246, boption(MUTABLE) → • MUTABLE, LIDENT / EXTEND]
fn yy_state_302(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_MUTABLE => Shift(yy_state_246)
    NT_boption_MUTABLE_ => Shift(yy_state_303)
    T_LIDENT | T_EXTEND => Reduce(0, NT_boption_MUTABLE_, yy_action_178)
    _ => Error
  }
}

// [157, record_decl_field → nonempty_list(attribute) boption(MUTABLE) • lident COLON type_, SEMI / RBRACE]
// [217, lident → • LIDENT, COLON]
// [218, lident → • EXTEND, COLON]
fn yy_state_303(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_304)
    _ => Error
  }
}

// [157, record_decl_field → nonempty_list(attribute) boption(MUTABLE) lident • COLON type_, SEMI / RBRACE]
fn yy_state_304(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_305)
    _ => Error
  }
}

// [157, record_decl_field → nonempty_list(attribute) boption(MUTABLE) lident COLON • type_, SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, QUESTION / SEMI / RBRACE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, QUESTION / SEMI / RBRACE]
// [173, simple_type → • LPAREN type_ RPAREN, QUESTION / SEMI / RBRACE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, QUESTION / SEMI / RBRACE]
// [175, simple_type → • AMPER qualified_uident_, QUESTION / SEMI / RBRACE]
// [176, simple_type → • UNDERSCORE, QUESTION / SEMI / RBRACE]
// [177, type_ → • simple_type, SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [214, qualified_uident_ → • UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]
fn yy_state_305(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_306)
    _ => Error
  }
}

// [157, record_decl_field → nonempty_list(attribute) boption(MUTABLE) lident COLON type_ •, SEMI / RBRACE]
fn yy_state_306(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_record_decl_field, yy_action_149)
}

// [156, record_decl_field → boption(MUTABLE) • lident COLON type_, SEMI / RBRACE]
// [217, lident → • LIDENT, COLON]
// [218, lident → • EXTEND, COLON]
fn yy_state_307(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_308)
    _ => Error
  }
}

// [156, record_decl_field → boption(MUTABLE) lident • COLON type_, SEMI / RBRACE]
fn yy_state_308(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_309)
    _ => Error
  }
}

// [156, record_decl_field → boption(MUTABLE) lident COLON • type_, SEMI / RBRACE]
// [171, simple_type → • simple_type QUESTION, QUESTION / SEMI / RBRACE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, QUESTION / SEMI / RBRACE]
// [173, simple_type → • LPAREN type_ RPAREN, QUESTION / SEMI / RBRACE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, QUESTION / SEMI / RBRACE]
// [175, simple_type → • AMPER qualified_uident_, QUESTION / SEMI / RBRACE]
// [176, simple_type → • UNDERSCORE, QUESTION / SEMI / RBRACE]
// [177, type_ → • simple_type, SEMI / RBRACE]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, SEMI / RBRACE]
// [214, qualified_uident_ → • UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, QUESTION / SEMI / LBRACKET / RBRACE]
fn yy_state_309(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_310)
    _ => Error
  }
}

// [156, record_decl_field → boption(MUTABLE) lident COLON type_ •, SEMI / RBRACE]
fn yy_state_310(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_record_decl_field, yy_action_148)
}

// [248, separated_list(SEMI,record_decl_field) → separated_nonempty_list(SEMI,record_decl_field) •, RBRACE]
fn yy_state_311(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_separated_list_SEMI_record_decl_field_, yy_action_147)
}

// [158, record_decl_body → separated_list(SEMI,record_decl_field) •, RBRACE]
fn yy_state_312(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_record_decl_body, yy_action_146)
}

// [74, type_sig → STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]
fn yy_state_313(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_314)
    _ => Error
  }
}

// [74, type_sig → STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_314(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_315)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [74, type_sig → STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]
fn yy_state_315(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_145)
}

// [62, type_sig → SUBERROR • uident optional_derive, EOF / SEMI]
// [68, type_sig → SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]
fn yy_state_316(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_317)
    _ => Error
  }
}

// [62, type_sig → SUBERROR uident • optional_derive, EOF / SEMI]
// [68, type_sig → SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_317(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_LBRACE => Shift(yy_state_318)
    NT_optional_derive => Shift(yy_state_322)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [68, type_sig → SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_318(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_319)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [68, type_sig → SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_319(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_320)
    _ => Error
  }
}

// [68, type_sig → SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_320(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_321)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [68, type_sig → SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_321(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_144)
}

// [62, type_sig → SUBERROR uident optional_derive •, EOF / SEMI]
fn yy_state_322(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_sig, yy_action_143)
}

// [56, type_sig → TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [128, alias_sig → TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]
fn yy_state_323(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_324)
    _ => Error
  }
}

// [56, type_sig → TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [128, alias_sig → TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_324(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_EQUAL => Shift(yy_state_325)
    NT_optional_derive => Shift(yy_state_327)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [128, alias_sig → TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_325(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_326)
    _ => Error
  }
}

// [128, alias_sig → TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
fn yy_state_326(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_alias_sig, yy_action_142)
}

// [56, type_sig → TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]
fn yy_state_327(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_type_sig, yy_action_141)
}

// [37, func_sig → FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_328(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_329)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [37, func_sig → FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_329(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_func_name => Shift(yy_state_330)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_330(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_331)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_331(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_332)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_332(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_333)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_333(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_334)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_334(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_335)
    _ => Error
  }
}

// [37, func_sig → FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_335(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_func_sig, yy_action_140)
}

// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [33, func_name → method_self_type_coloncolon • luident, LPAREN]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_336(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_luident => Shift(yy_state_337)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    _ => Error
  }
}

// [33, func_name → method_self_type_coloncolon luident •, LPAREN]
fn yy_state_337(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_func_name, yy_action_139)
}

// [31, luident → uident •, LPAREN]
fn yy_state_338(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_luident, yy_action_138)
}

// [30, luident → lident •, LPAREN]
fn yy_state_339(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_luident, yy_action_137)
}

// [32, func_name → luident •, LPAREN]
fn yy_state_340(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_func_name, yy_action_136)
}

// [29, method_self_type_coloncolon → AMPER • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
fn yy_state_341(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_342)
    _ => Error
  }
}

// [29, method_self_type_coloncolon → AMPER UIDENT • COLONCOLON, LIDENT / UIDENT / EXTEND]
fn yy_state_342(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_343)
    _ => Error
  }
}

// [29, method_self_type_coloncolon → AMPER UIDENT COLONCOLON •, LIDENT / UIDENT / EXTEND]
fn yy_state_343(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_method_self_type_coloncolon, yy_action_135)
}

// [28, method_self_type_coloncolon → UIDENT • COLONCOLON, LIDENT / UIDENT / EXTEND]
// [216, uident → UIDENT •, LPAREN]
fn yy_state_344(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_345)
    T_LPAREN => Reduce(1, NT_uident, yy_action_279)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → UIDENT COLONCOLON •, LIDENT / UIDENT / EXTEND]
fn yy_state_345(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_method_self_type_coloncolon, yy_action_134)
}

// [36, func_sig → ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_346(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_347)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_347(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_348)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [36, func_sig → ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_348(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_349)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_349(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_350)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_350(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_351)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_351(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_352)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_352(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_353)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_353(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_354)
    _ => Error
  }
}

// [36, func_sig → ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_354(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_func_sig, yy_action_133)
}

// [22, value_sig → LET • lident COLON type_, EOF / SEMI]
// [217, lident → • LIDENT, COLON]
// [218, lident → • EXTEND, COLON]
fn yy_state_355(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_356)
    _ => Error
  }
}

// [22, value_sig → LET lident • COLON type_, EOF / SEMI]
fn yy_state_356(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_357)
    _ => Error
  }
}

// [22, value_sig → LET lident COLON • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_357(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_358)
    _ => Error
  }
}

// [22, value_sig → LET lident COLON type_ •, EOF / SEMI]
fn yy_state_358(_lookahead : YYSymbol) -> YYDecision {
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
// [59, type_sig → nonempty_list(attribute) • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [60, type_sig → nonempty_list(attribute) • PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [61, type_sig → nonempty_list(attribute) • PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [65, type_sig → nonempty_list(attribute) • SUBERROR uident optional_derive, EOF / SEMI]
// [66, type_sig → nonempty_list(attribute) • PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [67, type_sig → nonempty_list(attribute) • PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [71, type_sig → nonempty_list(attribute) • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [72, type_sig → nonempty_list(attribute) • PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [73, type_sig → nonempty_list(attribute) • PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [77, type_sig → nonempty_list(attribute) • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [78, type_sig → nonempty_list(attribute) • PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [79, type_sig → nonempty_list(attribute) • PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [83, type_sig → nonempty_list(attribute) • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [84, type_sig → nonempty_list(attribute) • PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [85, type_sig → nonempty_list(attribute) • PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [89, type_sig → nonempty_list(attribute) • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [90, type_sig → nonempty_list(attribute) • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [91, type_sig → nonempty_list(attribute) • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [95, type_sig → nonempty_list(attribute) • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → nonempty_list(attribute) • PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → nonempty_list(attribute) • PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [101, type_sig → nonempty_list(attribute) • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [102, type_sig → nonempty_list(attribute) • PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [103, type_sig → nonempty_list(attribute) • PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [109, impl_sig → nonempty_list(attribute) • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [110, impl_sig → nonempty_list(attribute) • PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [111, impl_sig → nonempty_list(attribute) • PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [115, impl_sig → nonempty_list(attribute) • IMPL qualified_uident FOR type_, EOF / SEMI]
// [116, impl_sig → nonempty_list(attribute) • PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [117, impl_sig → nonempty_list(attribute) • PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [121, trait_sig → nonempty_list(attribute) • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [122, trait_sig → nonempty_list(attribute) • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [123, trait_sig → nonempty_list(attribute) • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [131, alias_sig → nonempty_list(attribute) • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [132, alias_sig → nonempty_list(attribute) • PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [133, alias_sig → nonempty_list(attribute) • PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [137, alias_sig → nonempty_list(attribute) • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [138, alias_sig → nonempty_list(attribute) • PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [139, alias_sig → nonempty_list(attribute) • PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [143, alias_sig → nonempty_list(attribute) • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → nonempty_list(attribute) • PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → nonempty_list(attribute) • PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [149, alias_sig → nonempty_list(attribute) • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [150, alias_sig → nonempty_list(attribute) • PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [151, alias_sig → nonempty_list(attribute) • PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_359(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_USING => Shift(yy_state_360)
    T_FNALIAS => Shift(yy_state_371)
    T_TRAIT => Shift(yy_state_375)
    T_IMPL => Shift(yy_state_379)
    T_EXTENUM => Shift(yy_state_387)
    T_ENUM => Shift(yy_state_399)
    T_STRUCT => Shift(yy_state_405)
    T_SUBERROR => Shift(yy_state_415)
    T_TYPE => Shift(yy_state_422)
    T_FN => Shift(yy_state_427)
    T_ASYNC => Shift(yy_state_435)
    T_LET => Shift(yy_state_444)
    T_PUB => Shift(yy_state_448)
    T_PRIV => Shift(yy_state_562)
    T_CONST => Shift(yy_state_657)
    _ => Error
  }
}

// [143, alias_sig → nonempty_list(attribute) USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [149, alias_sig → nonempty_list(attribute) USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_360(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_361)
    _ => Error
  }
}

// [143, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [149, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_361(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_362)
    _ => Error
  }
}

// [143, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [149, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_362(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_363)
    T_TYPE => Shift(yy_state_367)
    _ => Error
  }
}

// [149, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_363(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_364)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [149, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_364(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_365)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [149, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_365(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_366)
    _ => Error
  }
}

// [149, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_366(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_alias_sig, yy_action_131)
}

// [143, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_367(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_368)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [143, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_368(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_369)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [143, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_369(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_370)
    _ => Error
  }
}

// [143, alias_sig → nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_370(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_alias_sig, yy_action_130)
}

// [137, alias_sig → nonempty_list(attribute) FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [216, uident → • UIDENT, COLONCOLON]
fn yy_state_371(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_372)
    _ => Error
  }
}

// [137, alias_sig → nonempty_list(attribute) FNALIAS uident • COLONCOLON lident, EOF / SEMI]
fn yy_state_372(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_373)
    _ => Error
  }
}

// [137, alias_sig → nonempty_list(attribute) FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [217, lident → • LIDENT, EOF / SEMI]
// [218, lident → • EXTEND, EOF / SEMI]
fn yy_state_373(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_374)
    _ => Error
  }
}

// [137, alias_sig → nonempty_list(attribute) FNALIAS uident COLONCOLON lident •, EOF / SEMI]
fn yy_state_374(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_129)
}

// [121, trait_sig → nonempty_list(attribute) TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [216, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_375(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_376)
    _ => Error
  }
}

// [121, trait_sig → nonempty_list(attribute) TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [235, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [236, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
fn yy_state_376(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_43)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_377)
    T_EOF | T_SEMI | T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_271)
    _ => Error
  }
}

// [121, trait_sig → nonempty_list(attribute) TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [124, trait_body → •, EOF / SEMI]
// [125, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_377(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_52)
    NT_trait_body => Shift(yy_state_378)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_264)
    _ => Error
  }
}

// [121, trait_sig → nonempty_list(attribute) TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]
fn yy_state_378(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_trait_sig, yy_action_128)
}

// [109, impl_sig → nonempty_list(attribute) IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [115, impl_sig → nonempty_list(attribute) IMPL • qualified_uident FOR type_, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_379(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    T_LBRACKET => Shift(yy_state_144)
    NT_qualified_uident => Shift(yy_state_380)
    NT_type_params_with_constraints => Shift(yy_state_383)
    _ => Error
  }
}

// [115, impl_sig → nonempty_list(attribute) IMPL qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_380(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_381)
    _ => Error
  }
}

// [115, impl_sig → nonempty_list(attribute) IMPL qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_381(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_382)
    _ => Error
  }
}

// [115, impl_sig → nonempty_list(attribute) IMPL qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_382(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_impl_sig, yy_action_127)
}

// [109, impl_sig → nonempty_list(attribute) IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_383(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    NT_qualified_uident => Shift(yy_state_384)
    _ => Error
  }
}

// [109, impl_sig → nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_384(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_385)
    _ => Error
  }
}

// [109, impl_sig → nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_385(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_386)
    _ => Error
  }
}

// [109, impl_sig → nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_386(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_impl_sig, yy_action_126)
}

// [95, type_sig → nonempty_list(attribute) EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [101, type_sig → nonempty_list(attribute) EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [169, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [170, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [216, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]
fn yy_state_387(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_222)
    NT_uident => Shift(yy_state_235)
    NT_extensible_enum_extension_target => Shift(yy_state_388)
    NT_type_decl_name_with_params => Shift(yy_state_394)
    _ => Error
  }
}

// [101, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_388(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_389)
    _ => Error
  }
}

// [101, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_389(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_390)
    _ => Error
  }
}

// [101, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_390(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_391)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [101, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_391(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_392)
    _ => Error
  }
}

// [101, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_392(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_393)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [101, type_sig → nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_393(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_125)
}

// [95, type_sig → nonempty_list(attribute) EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_394(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_395)
    _ => Error
  }
}

// [95, type_sig → nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_395(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_396)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [95, type_sig → nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_396(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_397)
    _ => Error
  }
}

// [95, type_sig → nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_397(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_398)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [95, type_sig → nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_398(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_124)
}

// [89, type_sig → nonempty_list(attribute) ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [216, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_399(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_400)
    _ => Error
  }
}

// [89, type_sig → nonempty_list(attribute) ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_400(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_401)
    _ => Error
  }
}

// [89, type_sig → nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_401(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_402)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [89, type_sig → nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_402(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_403)
    _ => Error
  }
}

// [89, type_sig → nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_403(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_404)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [89, type_sig → nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_404(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_123)
}

// [77, type_sig → nonempty_list(attribute) STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [83, type_sig → nonempty_list(attribute) STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [216, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]
fn yy_state_405(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_406)
    _ => Error
  }
}

// [77, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [83, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
fn yy_state_406(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_407)
    T_LBRACE => Shift(yy_state_411)
    _ => Error
  }
}

// [83, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [230, separated_list(COMMA,type_) → •, RPAREN]
// [231, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [255, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [256, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
fn yy_state_407(_lookahead : YYSymbol) -> YYDecision {
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
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_294)
    NT_separated_list_COMMA_type__ => Shift(yy_state_408)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_155)
    _ => Error
  }
}

// [83, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]
fn yy_state_408(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_409)
    _ => Error
  }
}

// [83, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_409(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_410)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [83, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]
fn yy_state_410(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_122)
}

// [77, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [156, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [157, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [158, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [245, boption(MUTABLE) → •, LIDENT / EXTEND]
// [246, boption(MUTABLE) → • MUTABLE, LIDENT / EXTEND]
// [247, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [248, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE / EXTEND]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE / EXTEND]
// [271, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [272, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_411(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_246)
    NT_record_decl_field => Shift(yy_state_299)
    NT_nonempty_list_attribute_ => Shift(yy_state_302)
    NT_boption_MUTABLE_ => Shift(yy_state_307)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_311)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_312)
    NT_record_decl_body => Shift(yy_state_412)
    T_LIDENT | T_EXTEND => Reduce(0, NT_boption_MUTABLE_, yy_action_178)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_152)
    _ => Error
  }
}

// [77, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]
fn yy_state_412(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_413)
    _ => Error
  }
}

// [77, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_413(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_414)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [77, type_sig → nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]
fn yy_state_414(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_121)
}

// [65, type_sig → nonempty_list(attribute) SUBERROR • uident optional_derive, EOF / SEMI]
// [71, type_sig → nonempty_list(attribute) SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]
fn yy_state_415(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_416)
    _ => Error
  }
}

// [65, type_sig → nonempty_list(attribute) SUBERROR uident • optional_derive, EOF / SEMI]
// [71, type_sig → nonempty_list(attribute) SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_416(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_LBRACE => Shift(yy_state_417)
    NT_optional_derive => Shift(yy_state_421)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [71, type_sig → nonempty_list(attribute) SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_417(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_418)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [71, type_sig → nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_418(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_419)
    _ => Error
  }
}

// [71, type_sig → nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_419(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_420)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [71, type_sig → nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_420(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_120)
}

// [65, type_sig → nonempty_list(attribute) SUBERROR uident optional_derive •, EOF / SEMI]
fn yy_state_421(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_119)
}

// [59, type_sig → nonempty_list(attribute) TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [131, alias_sig → nonempty_list(attribute) TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]
fn yy_state_422(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_423)
    _ => Error
  }
}

// [59, type_sig → nonempty_list(attribute) TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [131, alias_sig → nonempty_list(attribute) TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_423(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_EQUAL => Shift(yy_state_424)
    NT_optional_derive => Shift(yy_state_426)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [131, alias_sig → nonempty_list(attribute) TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_424(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_425)
    _ => Error
  }
}

// [131, alias_sig → nonempty_list(attribute) TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
fn yy_state_425(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_118)
}

// [59, type_sig → nonempty_list(attribute) TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]
fn yy_state_426(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_117)
}

// [43, func_sig → nonempty_list(attribute) FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_427(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_428)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_428(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_429)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_429(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_430)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_430(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_431)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_431(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_432)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_432(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_433)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_433(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_434)
    _ => Error
  }
}

// [43, func_sig → nonempty_list(attribute) FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_434(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_func_sig, yy_action_116)
}

// [42, func_sig → nonempty_list(attribute) ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_435(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_436)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_436(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_437)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_437(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_438)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_438(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_439)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_439(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_440)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_440(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_441)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_441(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_442)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_442(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_443)
    _ => Error
  }
}

// [42, func_sig → nonempty_list(attribute) ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_443(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_func_sig, yy_action_115)
}

// [25, value_sig → nonempty_list(attribute) LET • lident COLON type_, EOF / SEMI]
// [217, lident → • LIDENT, COLON]
// [218, lident → • EXTEND, COLON]
fn yy_state_444(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_445)
    _ => Error
  }
}

// [25, value_sig → nonempty_list(attribute) LET lident • COLON type_, EOF / SEMI]
fn yy_state_445(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_446)
    _ => Error
  }
}

// [25, value_sig → nonempty_list(attribute) LET lident COLON • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_446(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_447)
    _ => Error
  }
}

// [25, value_sig → nonempty_list(attribute) LET lident COLON type_ •, EOF / SEMI]
fn yy_state_447(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_value_sig, yy_action_114)
}

// [21, const_sig → nonempty_list(attribute) PUB • pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [27, value_sig → nonempty_list(attribute) PUB • pub_attr LET lident COLON type_, EOF / SEMI]
// [46, func_sig → nonempty_list(attribute) PUB • pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [47, func_sig → nonempty_list(attribute) PUB • pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [61, type_sig → nonempty_list(attribute) PUB • pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [67, type_sig → nonempty_list(attribute) PUB • pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [73, type_sig → nonempty_list(attribute) PUB • pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [79, type_sig → nonempty_list(attribute) PUB • pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [85, type_sig → nonempty_list(attribute) PUB • pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [91, type_sig → nonempty_list(attribute) PUB • pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → nonempty_list(attribute) PUB • pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [103, type_sig → nonempty_list(attribute) PUB • pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [111, impl_sig → nonempty_list(attribute) PUB • pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [117, impl_sig → nonempty_list(attribute) PUB • pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [123, trait_sig → nonempty_list(attribute) PUB • pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [133, alias_sig → nonempty_list(attribute) PUB • pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [139, alias_sig → nonempty_list(attribute) PUB • pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [145, alias_sig → nonempty_list(attribute) PUB • pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [151, alias_sig → nonempty_list(attribute) PUB • pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [209, pub_attr → •, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [210, pub_attr → • LPAREN READONLY RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [211, pub_attr → • LPAREN lident RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_448(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_449)
    NT_pub_attr => Shift(yy_state_454)
    T_STRUCT | T_ENUM | T_TRAIT | T_IMPL | T_ASYNC | T_FNALIAS | T_FN | T_LET | T_CONST | T_TYPE | T_SUBERROR | T_USING | T_EXTENUM => Reduce(0, NT_pub_attr, yy_action_113)
    _ => Error
  }
}

// [210, pub_attr → LPAREN • READONLY RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [211, pub_attr → LPAREN • lident RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [217, lident → • LIDENT, RPAREN]
// [218, lident → • EXTEND, RPAREN]
fn yy_state_449(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_450)
    T_READONLY => Shift(yy_state_452)
    _ => Error
  }
}

// [211, pub_attr → LPAREN lident • RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_450(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_451)
    _ => Error
  }
}

// [211, pub_attr → LPAREN lident RPAREN •, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_451(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_pub_attr, yy_action_112)
}

// [210, pub_attr → LPAREN READONLY • RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_452(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_453)
    _ => Error
  }
}

// [210, pub_attr → LPAREN READONLY RPAREN •, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_453(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_pub_attr, yy_action_111)
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [27, value_sig → nonempty_list(attribute) PUB pub_attr • LET lident COLON type_, EOF / SEMI]
// [46, func_sig → nonempty_list(attribute) PUB pub_attr • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [47, func_sig → nonempty_list(attribute) PUB pub_attr • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [61, type_sig → nonempty_list(attribute) PUB pub_attr • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [67, type_sig → nonempty_list(attribute) PUB pub_attr • SUBERROR uident optional_derive, EOF / SEMI]
// [73, type_sig → nonempty_list(attribute) PUB pub_attr • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [79, type_sig → nonempty_list(attribute) PUB pub_attr • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [85, type_sig → nonempty_list(attribute) PUB pub_attr • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [91, type_sig → nonempty_list(attribute) PUB pub_attr • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → nonempty_list(attribute) PUB pub_attr • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [103, type_sig → nonempty_list(attribute) PUB pub_attr • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [111, impl_sig → nonempty_list(attribute) PUB pub_attr • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [117, impl_sig → nonempty_list(attribute) PUB pub_attr • IMPL qualified_uident FOR type_, EOF / SEMI]
// [123, trait_sig → nonempty_list(attribute) PUB pub_attr • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [133, alias_sig → nonempty_list(attribute) PUB pub_attr • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [139, alias_sig → nonempty_list(attribute) PUB pub_attr • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [145, alias_sig → nonempty_list(attribute) PUB pub_attr • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [151, alias_sig → nonempty_list(attribute) PUB pub_attr • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_454(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_USING => Shift(yy_state_455)
    T_FNALIAS => Shift(yy_state_466)
    T_TRAIT => Shift(yy_state_470)
    T_IMPL => Shift(yy_state_474)
    T_EXTENUM => Shift(yy_state_482)
    T_ENUM => Shift(yy_state_494)
    T_STRUCT => Shift(yy_state_500)
    T_SUBERROR => Shift(yy_state_510)
    T_TYPE => Shift(yy_state_517)
    T_FN => Shift(yy_state_522)
    T_ASYNC => Shift(yy_state_530)
    T_LET => Shift(yy_state_539)
    T_CONST => Shift(yy_state_543)
    _ => Error
  }
}

// [145, alias_sig → nonempty_list(attribute) PUB pub_attr USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [151, alias_sig → nonempty_list(attribute) PUB pub_attr USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_455(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_456)
    _ => Error
  }
}

// [145, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [151, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_456(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_457)
    _ => Error
  }
}

// [145, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [151, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_457(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_458)
    T_TYPE => Shift(yy_state_462)
    _ => Error
  }
}

// [151, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_458(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_459)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [151, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_459(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_460)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [151, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_460(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_461)
    _ => Error
  }
}

// [151, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_461(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_alias_sig, yy_action_110)
}

// [145, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_462(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_463)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [145, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_463(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_464)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [145, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_464(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_465)
    _ => Error
  }
}

// [145, alias_sig → nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_465(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_alias_sig, yy_action_109)
}

// [139, alias_sig → nonempty_list(attribute) PUB pub_attr FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [216, uident → • UIDENT, COLONCOLON]
fn yy_state_466(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_467)
    _ => Error
  }
}

// [139, alias_sig → nonempty_list(attribute) PUB pub_attr FNALIAS uident • COLONCOLON lident, EOF / SEMI]
fn yy_state_467(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_468)
    _ => Error
  }
}

// [139, alias_sig → nonempty_list(attribute) PUB pub_attr FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [217, lident → • LIDENT, EOF / SEMI]
// [218, lident → • EXTEND, EOF / SEMI]
fn yy_state_468(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_469)
    _ => Error
  }
}

// [139, alias_sig → nonempty_list(attribute) PUB pub_attr FNALIAS uident COLONCOLON lident •, EOF / SEMI]
fn yy_state_469(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_alias_sig, yy_action_108)
}

// [123, trait_sig → nonempty_list(attribute) PUB pub_attr TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [216, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_470(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_471)
    _ => Error
  }
}

// [123, trait_sig → nonempty_list(attribute) PUB pub_attr TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [235, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [236, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
fn yy_state_471(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_43)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_472)
    T_EOF | T_SEMI | T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_271)
    _ => Error
  }
}

// [123, trait_sig → nonempty_list(attribute) PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [124, trait_body → •, EOF / SEMI]
// [125, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_472(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_52)
    NT_trait_body => Shift(yy_state_473)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_264)
    _ => Error
  }
}

// [123, trait_sig → nonempty_list(attribute) PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]
fn yy_state_473(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_trait_sig, yy_action_107)
}

// [111, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [117, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL • qualified_uident FOR type_, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_474(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    T_LBRACKET => Shift(yy_state_144)
    NT_qualified_uident => Shift(yy_state_475)
    NT_type_params_with_constraints => Shift(yy_state_478)
    _ => Error
  }
}

// [117, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_475(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_476)
    _ => Error
  }
}

// [117, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_476(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_477)
    _ => Error
  }
}

// [117, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_477(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_impl_sig, yy_action_106)
}

// [111, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_478(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    NT_qualified_uident => Shift(yy_state_479)
    _ => Error
  }
}

// [111, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_479(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_480)
    _ => Error
  }
}

// [111, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_480(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_481)
    _ => Error
  }
}

// [111, impl_sig → nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_481(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_impl_sig, yy_action_105)
}

// [97, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [103, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [169, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [170, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [216, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]
fn yy_state_482(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_222)
    NT_uident => Shift(yy_state_235)
    NT_extensible_enum_extension_target => Shift(yy_state_483)
    NT_type_decl_name_with_params => Shift(yy_state_489)
    _ => Error
  }
}

// [103, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_483(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_484)
    _ => Error
  }
}

// [103, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_484(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_485)
    _ => Error
  }
}

// [103, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_485(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_486)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [103, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_486(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_487)
    _ => Error
  }
}

// [103, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_487(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_488)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [103, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_488(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_type_sig, yy_action_104)
}

// [97, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_489(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_490)
    _ => Error
  }
}

// [97, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_490(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_491)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [97, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_491(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_492)
    _ => Error
  }
}

// [97, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_492(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_493)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [97, type_sig → nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_493(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_103)
}

// [91, type_sig → nonempty_list(attribute) PUB pub_attr ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [216, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_494(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_495)
    _ => Error
  }
}

// [91, type_sig → nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_495(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_496)
    _ => Error
  }
}

// [91, type_sig → nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_496(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_497)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [91, type_sig → nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_497(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_498)
    _ => Error
  }
}

// [91, type_sig → nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_498(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_499)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [91, type_sig → nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_499(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_102)
}

// [79, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [85, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [216, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]
fn yy_state_500(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_501)
    _ => Error
  }
}

// [79, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [85, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
fn yy_state_501(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_502)
    T_LBRACE => Shift(yy_state_506)
    _ => Error
  }
}

// [85, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [230, separated_list(COMMA,type_) → •, RPAREN]
// [231, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [255, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [256, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
fn yy_state_502(_lookahead : YYSymbol) -> YYDecision {
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
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_294)
    NT_separated_list_COMMA_type__ => Shift(yy_state_503)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_155)
    _ => Error
  }
}

// [85, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]
fn yy_state_503(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_504)
    _ => Error
  }
}

// [85, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_504(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_505)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [85, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]
fn yy_state_505(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_101)
}

// [79, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [156, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [157, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [158, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [245, boption(MUTABLE) → •, LIDENT / EXTEND]
// [246, boption(MUTABLE) → • MUTABLE, LIDENT / EXTEND]
// [247, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [248, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE / EXTEND]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE / EXTEND]
// [271, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [272, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_506(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_246)
    NT_record_decl_field => Shift(yy_state_299)
    NT_nonempty_list_attribute_ => Shift(yy_state_302)
    NT_boption_MUTABLE_ => Shift(yy_state_307)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_311)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_312)
    NT_record_decl_body => Shift(yy_state_507)
    T_LIDENT | T_EXTEND => Reduce(0, NT_boption_MUTABLE_, yy_action_178)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_152)
    _ => Error
  }
}

// [79, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]
fn yy_state_507(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_508)
    _ => Error
  }
}

// [79, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_508(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_509)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [79, type_sig → nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]
fn yy_state_509(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_100)
}

// [67, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR • uident optional_derive, EOF / SEMI]
// [73, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]
fn yy_state_510(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_511)
    _ => Error
  }
}

// [67, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident • optional_derive, EOF / SEMI]
// [73, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_511(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_LBRACE => Shift(yy_state_512)
    NT_optional_derive => Shift(yy_state_516)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [73, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_512(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_513)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [73, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_513(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_514)
    _ => Error
  }
}

// [73, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_514(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_515)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [73, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_515(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_99)
}

// [67, type_sig → nonempty_list(attribute) PUB pub_attr SUBERROR uident optional_derive •, EOF / SEMI]
fn yy_state_516(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_98)
}

// [61, type_sig → nonempty_list(attribute) PUB pub_attr TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [133, alias_sig → nonempty_list(attribute) PUB pub_attr TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]
fn yy_state_517(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_518)
    _ => Error
  }
}

// [61, type_sig → nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [133, alias_sig → nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_518(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_EQUAL => Shift(yy_state_519)
    NT_optional_derive => Shift(yy_state_521)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [133, alias_sig → nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_519(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_520)
    _ => Error
  }
}

// [133, alias_sig → nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
fn yy_state_520(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_alias_sig, yy_action_97)
}

// [61, type_sig → nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]
fn yy_state_521(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_type_sig, yy_action_96)
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_522(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_523)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_523(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_524)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_524(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_525)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_525(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_526)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_526(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_527)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_527(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_528)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_528(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_529)
    _ => Error
  }
}

// [47, func_sig → nonempty_list(attribute) PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_529(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(11, NT_func_sig, yy_action_95)
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_530(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_531)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_531(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_532)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_532(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_533)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_533(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_534)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_534(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_535)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_535(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_536)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_536(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_537)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_537(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_538)
    _ => Error
  }
}

// [46, func_sig → nonempty_list(attribute) PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_538(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(12, NT_func_sig, yy_action_94)
}

// [27, value_sig → nonempty_list(attribute) PUB pub_attr LET • lident COLON type_, EOF / SEMI]
// [217, lident → • LIDENT, COLON]
// [218, lident → • EXTEND, COLON]
fn yy_state_539(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_540)
    _ => Error
  }
}

// [27, value_sig → nonempty_list(attribute) PUB pub_attr LET lident • COLON type_, EOF / SEMI]
fn yy_state_540(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_541)
    _ => Error
  }
}

// [27, value_sig → nonempty_list(attribute) PUB pub_attr LET lident COLON • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_541(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_542)
    _ => Error
  }
}

// [27, value_sig → nonempty_list(attribute) PUB pub_attr LET lident COLON type_ •, EOF / SEMI]
fn yy_state_542(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_value_sig, yy_action_93)
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [216, uident → • UIDENT, COLON]
fn yy_state_543(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_544)
    _ => Error
  }
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST uident • COLON type_ EQUAL constant, EOF / SEMI]
fn yy_state_544(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_545)
    _ => Error
  }
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [176, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [177, type_ → • simple_type, EQUAL]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [214, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]
fn yy_state_545(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_546)
    _ => Error
  }
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST uident COLON type_ • EQUAL constant, EOF / SEMI]
fn yy_state_546(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_547)
    _ => Error
  }
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [197, constant → • TRUE, EOF / SEMI]
// [198, constant → • FALSE, EOF / SEMI]
// [199, constant → • BYTE, EOF / SEMI]
// [200, constant → • BYTES, EOF / SEMI]
// [201, constant → • CHAR, EOF / SEMI]
// [202, constant → • INT, EOF / SEMI]
// [203, constant → • MINUS INT, EOF / SEMI]
// [204, constant → • FLOAT, EOF / SEMI]
// [205, constant → • MINUS FLOAT, EOF / SEMI]
// [206, constant → • DOUBLE, EOF / SEMI]
// [207, constant → • MINUS DOUBLE, EOF / SEMI]
// [208, constant → • STRING, EOF / SEMI]
fn yy_state_547(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_548)
    T_DOUBLE => Shift(yy_state_549)
    T_FLOAT => Shift(yy_state_550)
    T_MINUS => Shift(yy_state_551)
    T_INT => Shift(yy_state_555)
    T_CHAR => Shift(yy_state_556)
    T_BYTES => Shift(yy_state_557)
    T_BYTE => Shift(yy_state_558)
    T_FALSE => Shift(yy_state_559)
    T_TRUE => Shift(yy_state_560)
    NT_constant => Shift(yy_state_561)
    _ => Error
  }
}

// [208, constant → STRING •, EOF / SEMI]
fn yy_state_548(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_92)
}

// [206, constant → DOUBLE •, EOF / SEMI]
fn yy_state_549(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_91)
}

// [204, constant → FLOAT •, EOF / SEMI]
fn yy_state_550(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_90)
}

// [203, constant → MINUS • INT, EOF / SEMI]
// [205, constant → MINUS • FLOAT, EOF / SEMI]
// [207, constant → MINUS • DOUBLE, EOF / SEMI]
fn yy_state_551(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DOUBLE => Shift(yy_state_552)
    T_FLOAT => Shift(yy_state_553)
    T_INT => Shift(yy_state_554)
    _ => Error
  }
}

// [207, constant → MINUS DOUBLE •, EOF / SEMI]
fn yy_state_552(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_constant, yy_action_89)
}

// [205, constant → MINUS FLOAT •, EOF / SEMI]
fn yy_state_553(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_constant, yy_action_88)
}

// [203, constant → MINUS INT •, EOF / SEMI]
fn yy_state_554(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(2, NT_constant, yy_action_87)
}

// [202, constant → INT •, EOF / SEMI]
fn yy_state_555(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_86)
}

// [201, constant → CHAR •, EOF / SEMI]
fn yy_state_556(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_85)
}

// [200, constant → BYTES •, EOF / SEMI]
fn yy_state_557(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_84)
}

// [199, constant → BYTE •, EOF / SEMI]
fn yy_state_558(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_83)
}

// [198, constant → FALSE •, EOF / SEMI]
fn yy_state_559(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_82)
}

// [197, constant → TRUE •, EOF / SEMI]
fn yy_state_560(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_constant, yy_action_81)
}

// [21, const_sig → nonempty_list(attribute) PUB pub_attr CONST uident COLON type_ EQUAL constant •, EOF / SEMI]
fn yy_state_561(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_const_sig, yy_action_80)
}

// [20, const_sig → nonempty_list(attribute) PRIV • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [26, value_sig → nonempty_list(attribute) PRIV • LET lident COLON type_, EOF / SEMI]
// [44, func_sig → nonempty_list(attribute) PRIV • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [45, func_sig → nonempty_list(attribute) PRIV • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [60, type_sig → nonempty_list(attribute) PRIV • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [66, type_sig → nonempty_list(attribute) PRIV • SUBERROR uident optional_derive, EOF / SEMI]
// [72, type_sig → nonempty_list(attribute) PRIV • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [78, type_sig → nonempty_list(attribute) PRIV • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [84, type_sig → nonempty_list(attribute) PRIV • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [90, type_sig → nonempty_list(attribute) PRIV • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → nonempty_list(attribute) PRIV • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [102, type_sig → nonempty_list(attribute) PRIV • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [110, impl_sig → nonempty_list(attribute) PRIV • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [116, impl_sig → nonempty_list(attribute) PRIV • IMPL qualified_uident FOR type_, EOF / SEMI]
// [122, trait_sig → nonempty_list(attribute) PRIV • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [132, alias_sig → nonempty_list(attribute) PRIV • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [138, alias_sig → nonempty_list(attribute) PRIV • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [144, alias_sig → nonempty_list(attribute) PRIV • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [150, alias_sig → nonempty_list(attribute) PRIV • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_562(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_USING => Shift(yy_state_563)
    T_FNALIAS => Shift(yy_state_574)
    T_TRAIT => Shift(yy_state_578)
    T_IMPL => Shift(yy_state_582)
    T_EXTENUM => Shift(yy_state_590)
    T_ENUM => Shift(yy_state_602)
    T_STRUCT => Shift(yy_state_608)
    T_SUBERROR => Shift(yy_state_618)
    T_TYPE => Shift(yy_state_625)
    T_FN => Shift(yy_state_630)
    T_ASYNC => Shift(yy_state_638)
    T_LET => Shift(yy_state_647)
    T_CONST => Shift(yy_state_651)
    _ => Error
  }
}

// [144, alias_sig → nonempty_list(attribute) PRIV USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [150, alias_sig → nonempty_list(attribute) PRIV USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_563(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_564)
    _ => Error
  }
}

// [144, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [150, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_564(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_565)
    _ => Error
  }
}

// [144, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [150, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_565(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_566)
    T_TYPE => Shift(yy_state_570)
    _ => Error
  }
}

// [150, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_566(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_567)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [150, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_567(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_568)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [150, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_568(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_569)
    _ => Error
  }
}

// [150, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_569(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_alias_sig, yy_action_79)
}

// [144, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_570(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_571)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [144, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_571(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_572)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [144, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_572(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_573)
    _ => Error
  }
}

// [144, alias_sig → nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_573(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_alias_sig, yy_action_78)
}

// [138, alias_sig → nonempty_list(attribute) PRIV FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [216, uident → • UIDENT, COLONCOLON]
fn yy_state_574(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_575)
    _ => Error
  }
}

// [138, alias_sig → nonempty_list(attribute) PRIV FNALIAS uident • COLONCOLON lident, EOF / SEMI]
fn yy_state_575(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_576)
    _ => Error
  }
}

// [138, alias_sig → nonempty_list(attribute) PRIV FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [217, lident → • LIDENT, EOF / SEMI]
// [218, lident → • EXTEND, EOF / SEMI]
fn yy_state_576(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_577)
    _ => Error
  }
}

// [138, alias_sig → nonempty_list(attribute) PRIV FNALIAS uident COLONCOLON lident •, EOF / SEMI]
fn yy_state_577(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_77)
}

// [122, trait_sig → nonempty_list(attribute) PRIV TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [216, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_578(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_579)
    _ => Error
  }
}

// [122, trait_sig → nonempty_list(attribute) PRIV TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [235, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [236, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
fn yy_state_579(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_43)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_580)
    T_EOF | T_SEMI | T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_271)
    _ => Error
  }
}

// [122, trait_sig → nonempty_list(attribute) PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [124, trait_body → •, EOF / SEMI]
// [125, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_580(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_52)
    NT_trait_body => Shift(yy_state_581)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_264)
    _ => Error
  }
}

// [122, trait_sig → nonempty_list(attribute) PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]
fn yy_state_581(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_trait_sig, yy_action_76)
}

// [110, impl_sig → nonempty_list(attribute) PRIV IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [116, impl_sig → nonempty_list(attribute) PRIV IMPL • qualified_uident FOR type_, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_582(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    T_LBRACKET => Shift(yy_state_144)
    NT_qualified_uident => Shift(yy_state_583)
    NT_type_params_with_constraints => Shift(yy_state_586)
    _ => Error
  }
}

// [116, impl_sig → nonempty_list(attribute) PRIV IMPL qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_583(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_584)
    _ => Error
  }
}

// [116, impl_sig → nonempty_list(attribute) PRIV IMPL qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_584(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_585)
    _ => Error
  }
}

// [116, impl_sig → nonempty_list(attribute) PRIV IMPL qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_585(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_impl_sig, yy_action_75)
}

// [110, impl_sig → nonempty_list(attribute) PRIV IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_586(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    NT_qualified_uident => Shift(yy_state_587)
    _ => Error
  }
}

// [110, impl_sig → nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_587(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_588)
    _ => Error
  }
}

// [110, impl_sig → nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_588(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_589)
    _ => Error
  }
}

// [110, impl_sig → nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_589(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_impl_sig, yy_action_74)
}

// [96, type_sig → nonempty_list(attribute) PRIV EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [102, type_sig → nonempty_list(attribute) PRIV EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [169, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [170, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [216, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]
fn yy_state_590(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_222)
    NT_uident => Shift(yy_state_235)
    NT_extensible_enum_extension_target => Shift(yy_state_591)
    NT_type_decl_name_with_params => Shift(yy_state_597)
    _ => Error
  }
}

// [102, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_591(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_592)
    _ => Error
  }
}

// [102, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_592(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_593)
    _ => Error
  }
}

// [102, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_593(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_594)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [102, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_594(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_595)
    _ => Error
  }
}

// [102, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_595(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_596)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [102, type_sig → nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_596(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_73)
}

// [96, type_sig → nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_597(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_598)
    _ => Error
  }
}

// [96, type_sig → nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_598(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_599)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [96, type_sig → nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_599(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_600)
    _ => Error
  }
}

// [96, type_sig → nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_600(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_601)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [96, type_sig → nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_601(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_72)
}

// [90, type_sig → nonempty_list(attribute) PRIV ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [216, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_602(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_603)
    _ => Error
  }
}

// [90, type_sig → nonempty_list(attribute) PRIV ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_603(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_604)
    _ => Error
  }
}

// [90, type_sig → nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_604(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_605)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [90, type_sig → nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_605(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_606)
    _ => Error
  }
}

// [90, type_sig → nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_606(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_607)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [90, type_sig → nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_607(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_71)
}

// [78, type_sig → nonempty_list(attribute) PRIV STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [84, type_sig → nonempty_list(attribute) PRIV STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [216, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]
fn yy_state_608(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_609)
    _ => Error
  }
}

// [78, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [84, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
fn yy_state_609(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_610)
    T_LBRACE => Shift(yy_state_614)
    _ => Error
  }
}

// [84, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [230, separated_list(COMMA,type_) → •, RPAREN]
// [231, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [255, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [256, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
fn yy_state_610(_lookahead : YYSymbol) -> YYDecision {
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
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_294)
    NT_separated_list_COMMA_type__ => Shift(yy_state_611)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_155)
    _ => Error
  }
}

// [84, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]
fn yy_state_611(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_612)
    _ => Error
  }
}

// [84, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_612(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_613)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [84, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]
fn yy_state_613(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_70)
}

// [78, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [156, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [157, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [158, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [245, boption(MUTABLE) → •, LIDENT / EXTEND]
// [246, boption(MUTABLE) → • MUTABLE, LIDENT / EXTEND]
// [247, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [248, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE / EXTEND]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE / EXTEND]
// [271, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [272, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_614(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_246)
    NT_record_decl_field => Shift(yy_state_299)
    NT_nonempty_list_attribute_ => Shift(yy_state_302)
    NT_boption_MUTABLE_ => Shift(yy_state_307)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_311)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_312)
    NT_record_decl_body => Shift(yy_state_615)
    T_LIDENT | T_EXTEND => Reduce(0, NT_boption_MUTABLE_, yy_action_178)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_152)
    _ => Error
  }
}

// [78, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]
fn yy_state_615(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_616)
    _ => Error
  }
}

// [78, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_616(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_617)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [78, type_sig → nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]
fn yy_state_617(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_69)
}

// [66, type_sig → nonempty_list(attribute) PRIV SUBERROR • uident optional_derive, EOF / SEMI]
// [72, type_sig → nonempty_list(attribute) PRIV SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]
fn yy_state_618(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_619)
    _ => Error
  }
}

// [66, type_sig → nonempty_list(attribute) PRIV SUBERROR uident • optional_derive, EOF / SEMI]
// [72, type_sig → nonempty_list(attribute) PRIV SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_619(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_LBRACE => Shift(yy_state_620)
    NT_optional_derive => Shift(yy_state_624)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [72, type_sig → nonempty_list(attribute) PRIV SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_620(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_621)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [72, type_sig → nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_621(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_622)
    _ => Error
  }
}

// [72, type_sig → nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_622(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_623)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [72, type_sig → nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_623(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_68)
}

// [66, type_sig → nonempty_list(attribute) PRIV SUBERROR uident optional_derive •, EOF / SEMI]
fn yy_state_624(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_67)
}

// [60, type_sig → nonempty_list(attribute) PRIV TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [132, alias_sig → nonempty_list(attribute) PRIV TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]
fn yy_state_625(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_626)
    _ => Error
  }
}

// [60, type_sig → nonempty_list(attribute) PRIV TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [132, alias_sig → nonempty_list(attribute) PRIV TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_626(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_EQUAL => Shift(yy_state_627)
    NT_optional_derive => Shift(yy_state_629)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [132, alias_sig → nonempty_list(attribute) PRIV TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_627(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_628)
    _ => Error
  }
}

// [132, alias_sig → nonempty_list(attribute) PRIV TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
fn yy_state_628(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_66)
}

// [60, type_sig → nonempty_list(attribute) PRIV TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]
fn yy_state_629(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_65)
}

// [45, func_sig → nonempty_list(attribute) PRIV FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_630(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_631)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_631(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_632)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_632(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_633)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_633(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_634)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_634(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_635)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_635(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_636)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_636(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_637)
    _ => Error
  }
}

// [45, func_sig → nonempty_list(attribute) PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_637(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_func_sig, yy_action_64)
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_638(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_639)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_639(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_640)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_640(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_641)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_641(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_642)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_642(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_643)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_643(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_644)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_644(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_645)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_645(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_646)
    _ => Error
  }
}

// [44, func_sig → nonempty_list(attribute) PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_646(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(11, NT_func_sig, yy_action_63)
}

// [26, value_sig → nonempty_list(attribute) PRIV LET • lident COLON type_, EOF / SEMI]
// [217, lident → • LIDENT, COLON]
// [218, lident → • EXTEND, COLON]
fn yy_state_647(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_648)
    _ => Error
  }
}

// [26, value_sig → nonempty_list(attribute) PRIV LET lident • COLON type_, EOF / SEMI]
fn yy_state_648(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_649)
    _ => Error
  }
}

// [26, value_sig → nonempty_list(attribute) PRIV LET lident COLON • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
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

// [26, value_sig → nonempty_list(attribute) PRIV LET lident COLON type_ •, EOF / SEMI]
fn yy_state_650(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_value_sig, yy_action_62)
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [216, uident → • UIDENT, COLON]
fn yy_state_651(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_652)
    _ => Error
  }
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST uident • COLON type_ EQUAL constant, EOF / SEMI]
fn yy_state_652(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_653)
    _ => Error
  }
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [176, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [177, type_ → • simple_type, EQUAL]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [214, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]
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

// [20, const_sig → nonempty_list(attribute) PRIV CONST uident COLON type_ • EQUAL constant, EOF / SEMI]
fn yy_state_654(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_655)
    _ => Error
  }
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [197, constant → • TRUE, EOF / SEMI]
// [198, constant → • FALSE, EOF / SEMI]
// [199, constant → • BYTE, EOF / SEMI]
// [200, constant → • BYTES, EOF / SEMI]
// [201, constant → • CHAR, EOF / SEMI]
// [202, constant → • INT, EOF / SEMI]
// [203, constant → • MINUS INT, EOF / SEMI]
// [204, constant → • FLOAT, EOF / SEMI]
// [205, constant → • MINUS FLOAT, EOF / SEMI]
// [206, constant → • DOUBLE, EOF / SEMI]
// [207, constant → • MINUS DOUBLE, EOF / SEMI]
// [208, constant → • STRING, EOF / SEMI]
fn yy_state_655(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_548)
    T_DOUBLE => Shift(yy_state_549)
    T_FLOAT => Shift(yy_state_550)
    T_MINUS => Shift(yy_state_551)
    T_INT => Shift(yy_state_555)
    T_CHAR => Shift(yy_state_556)
    T_BYTES => Shift(yy_state_557)
    T_BYTE => Shift(yy_state_558)
    T_FALSE => Shift(yy_state_559)
    T_TRUE => Shift(yy_state_560)
    NT_constant => Shift(yy_state_656)
    _ => Error
  }
}

// [20, const_sig → nonempty_list(attribute) PRIV CONST uident COLON type_ EQUAL constant •, EOF / SEMI]
fn yy_state_656(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_const_sig, yy_action_61)
}

// [19, const_sig → nonempty_list(attribute) CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [216, uident → • UIDENT, COLON]
fn yy_state_657(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_658)
    _ => Error
  }
}

// [19, const_sig → nonempty_list(attribute) CONST uident • COLON type_ EQUAL constant, EOF / SEMI]
fn yy_state_658(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_659)
    _ => Error
  }
}

// [19, const_sig → nonempty_list(attribute) CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [176, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [177, type_ → • simple_type, EQUAL]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [214, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]
fn yy_state_659(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_660)
    _ => Error
  }
}

// [19, const_sig → nonempty_list(attribute) CONST uident COLON type_ • EQUAL constant, EOF / SEMI]
fn yy_state_660(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_661)
    _ => Error
  }
}

// [19, const_sig → nonempty_list(attribute) CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [197, constant → • TRUE, EOF / SEMI]
// [198, constant → • FALSE, EOF / SEMI]
// [199, constant → • BYTE, EOF / SEMI]
// [200, constant → • BYTES, EOF / SEMI]
// [201, constant → • CHAR, EOF / SEMI]
// [202, constant → • INT, EOF / SEMI]
// [203, constant → • MINUS INT, EOF / SEMI]
// [204, constant → • FLOAT, EOF / SEMI]
// [205, constant → • MINUS FLOAT, EOF / SEMI]
// [206, constant → • DOUBLE, EOF / SEMI]
// [207, constant → • MINUS DOUBLE, EOF / SEMI]
// [208, constant → • STRING, EOF / SEMI]
fn yy_state_661(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_548)
    T_DOUBLE => Shift(yy_state_549)
    T_FLOAT => Shift(yy_state_550)
    T_MINUS => Shift(yy_state_551)
    T_INT => Shift(yy_state_555)
    T_CHAR => Shift(yy_state_556)
    T_BYTES => Shift(yy_state_557)
    T_BYTE => Shift(yy_state_558)
    T_FALSE => Shift(yy_state_559)
    T_TRUE => Shift(yy_state_560)
    NT_constant => Shift(yy_state_662)
    _ => Error
  }
}

// [19, const_sig → nonempty_list(attribute) CONST uident COLON type_ EQUAL constant •, EOF / SEMI]
fn yy_state_662(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_const_sig, yy_action_59)
}

// [18, const_sig → PUB • pub_attr CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [24, value_sig → PUB • pub_attr LET lident COLON type_, EOF / SEMI]
// [40, func_sig → PUB • pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [41, func_sig → PUB • pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [58, type_sig → PUB • pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [64, type_sig → PUB • pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [70, type_sig → PUB • pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [76, type_sig → PUB • pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [82, type_sig → PUB • pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [88, type_sig → PUB • pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [94, type_sig → PUB • pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, type_sig → PUB • pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [108, impl_sig → PUB • pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [114, impl_sig → PUB • pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [120, trait_sig → PUB • pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [130, alias_sig → PUB • pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [136, alias_sig → PUB • pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [142, alias_sig → PUB • pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [148, alias_sig → PUB • pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [209, pub_attr → •, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [210, pub_attr → • LPAREN READONLY RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [211, pub_attr → • LPAREN lident RPAREN, STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_663(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_449)
    NT_pub_attr => Shift(yy_state_664)
    T_STRUCT | T_ENUM | T_TRAIT | T_IMPL | T_ASYNC | T_FNALIAS | T_FN | T_LET | T_CONST | T_TYPE | T_SUBERROR | T_USING | T_EXTENUM => Reduce(0, NT_pub_attr, yy_action_113)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [24, value_sig → PUB pub_attr • LET lident COLON type_, EOF / SEMI]
// [40, func_sig → PUB pub_attr • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [41, func_sig → PUB pub_attr • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [58, type_sig → PUB pub_attr • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [64, type_sig → PUB pub_attr • SUBERROR uident optional_derive, EOF / SEMI]
// [70, type_sig → PUB pub_attr • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [76, type_sig → PUB pub_attr • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [82, type_sig → PUB pub_attr • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [88, type_sig → PUB pub_attr • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [94, type_sig → PUB pub_attr • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, type_sig → PUB pub_attr • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [108, impl_sig → PUB pub_attr • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [114, impl_sig → PUB pub_attr • IMPL qualified_uident FOR type_, EOF / SEMI]
// [120, trait_sig → PUB pub_attr • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [130, alias_sig → PUB pub_attr • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [136, alias_sig → PUB pub_attr • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [142, alias_sig → PUB pub_attr • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [148, alias_sig → PUB pub_attr • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_664(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_USING => Shift(yy_state_665)
    T_FNALIAS => Shift(yy_state_676)
    T_TRAIT => Shift(yy_state_680)
    T_IMPL => Shift(yy_state_684)
    T_EXTENUM => Shift(yy_state_692)
    T_ENUM => Shift(yy_state_704)
    T_STRUCT => Shift(yy_state_710)
    T_SUBERROR => Shift(yy_state_720)
    T_TYPE => Shift(yy_state_727)
    T_FN => Shift(yy_state_732)
    T_ASYNC => Shift(yy_state_740)
    T_LET => Shift(yy_state_749)
    T_CONST => Shift(yy_state_753)
    _ => Error
  }
}

// [142, alias_sig → PUB pub_attr USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [148, alias_sig → PUB pub_attr USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_665(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_666)
    _ => Error
  }
}

// [142, alias_sig → PUB pub_attr USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [148, alias_sig → PUB pub_attr USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_666(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_667)
    _ => Error
  }
}

// [142, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [148, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_667(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_668)
    T_TYPE => Shift(yy_state_672)
    _ => Error
  }
}

// [148, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_668(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_669)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [148, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_669(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_670)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [148, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_670(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_671)
    _ => Error
  }
}

// [148, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_671(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_alias_sig, yy_action_58)
}

// [142, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_672(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_673)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [142, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_673(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_674)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [142, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_674(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_675)
    _ => Error
  }
}

// [142, alias_sig → PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_675(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_alias_sig, yy_action_57)
}

// [136, alias_sig → PUB pub_attr FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [216, uident → • UIDENT, COLONCOLON]
fn yy_state_676(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_677)
    _ => Error
  }
}

// [136, alias_sig → PUB pub_attr FNALIAS uident • COLONCOLON lident, EOF / SEMI]
fn yy_state_677(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_678)
    _ => Error
  }
}

// [136, alias_sig → PUB pub_attr FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [217, lident → • LIDENT, EOF / SEMI]
// [218, lident → • EXTEND, EOF / SEMI]
fn yy_state_678(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_679)
    _ => Error
  }
}

// [136, alias_sig → PUB pub_attr FNALIAS uident COLONCOLON lident •, EOF / SEMI]
fn yy_state_679(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_56)
}

// [120, trait_sig → PUB pub_attr TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [216, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_680(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_681)
    _ => Error
  }
}

// [120, trait_sig → PUB pub_attr TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [235, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [236, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
fn yy_state_681(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_43)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_682)
    T_EOF | T_SEMI | T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_271)
    _ => Error
  }
}

// [120, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [124, trait_body → •, EOF / SEMI]
// [125, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_682(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_52)
    NT_trait_body => Shift(yy_state_683)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_264)
    _ => Error
  }
}

// [120, trait_sig → PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]
fn yy_state_683(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_trait_sig, yy_action_55)
}

// [108, impl_sig → PUB pub_attr IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [114, impl_sig → PUB pub_attr IMPL • qualified_uident FOR type_, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_684(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    T_LBRACKET => Shift(yy_state_144)
    NT_qualified_uident => Shift(yy_state_685)
    NT_type_params_with_constraints => Shift(yy_state_688)
    _ => Error
  }
}

// [114, impl_sig → PUB pub_attr IMPL qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_685(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_686)
    _ => Error
  }
}

// [114, impl_sig → PUB pub_attr IMPL qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_686(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_687)
    _ => Error
  }
}

// [114, impl_sig → PUB pub_attr IMPL qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_687(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_impl_sig, yy_action_54)
}

// [108, impl_sig → PUB pub_attr IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_688(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    NT_qualified_uident => Shift(yy_state_689)
    _ => Error
  }
}

// [108, impl_sig → PUB pub_attr IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_689(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_690)
    _ => Error
  }
}

// [108, impl_sig → PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_690(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_691)
    _ => Error
  }
}

// [108, impl_sig → PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_691(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_impl_sig, yy_action_53)
}

// [94, type_sig → PUB pub_attr EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, type_sig → PUB pub_attr EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [169, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [170, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [216, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]
fn yy_state_692(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_222)
    NT_uident => Shift(yy_state_235)
    NT_extensible_enum_extension_target => Shift(yy_state_693)
    NT_type_decl_name_with_params => Shift(yy_state_699)
    _ => Error
  }
}

// [100, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_693(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_694)
    _ => Error
  }
}

// [100, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_694(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_695)
    _ => Error
  }
}

// [100, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_695(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_696)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [100, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_696(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_697)
    _ => Error
  }
}

// [100, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_697(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_698)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [100, type_sig → PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_698(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_type_sig, yy_action_52)
}

// [94, type_sig → PUB pub_attr EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_699(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_700)
    _ => Error
  }
}

// [94, type_sig → PUB pub_attr EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_700(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_701)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [94, type_sig → PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_701(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_702)
    _ => Error
  }
}

// [94, type_sig → PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_702(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_703)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [94, type_sig → PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_703(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_51)
}

// [88, type_sig → PUB pub_attr ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [216, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_704(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_705)
    _ => Error
  }
}

// [88, type_sig → PUB pub_attr ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_705(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_706)
    _ => Error
  }
}

// [88, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_706(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_707)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [88, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_707(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_708)
    _ => Error
  }
}

// [88, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_708(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_709)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [88, type_sig → PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_709(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_50)
}

// [76, type_sig → PUB pub_attr STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [82, type_sig → PUB pub_attr STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [216, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]
fn yy_state_710(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_711)
    _ => Error
  }
}

// [76, type_sig → PUB pub_attr STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [82, type_sig → PUB pub_attr STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
fn yy_state_711(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_712)
    T_LBRACE => Shift(yy_state_716)
    _ => Error
  }
}

// [82, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [230, separated_list(COMMA,type_) → •, RPAREN]
// [231, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [255, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [256, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
fn yy_state_712(_lookahead : YYSymbol) -> YYDecision {
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
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_294)
    NT_separated_list_COMMA_type__ => Shift(yy_state_713)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_155)
    _ => Error
  }
}

// [82, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]
fn yy_state_713(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_714)
    _ => Error
  }
}

// [82, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_714(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_715)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [82, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]
fn yy_state_715(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_49)
}

// [76, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [156, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [157, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [158, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [245, boption(MUTABLE) → •, LIDENT / EXTEND]
// [246, boption(MUTABLE) → • MUTABLE, LIDENT / EXTEND]
// [247, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [248, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE / EXTEND]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE / EXTEND]
// [271, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [272, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_716(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_246)
    NT_record_decl_field => Shift(yy_state_299)
    NT_nonempty_list_attribute_ => Shift(yy_state_302)
    NT_boption_MUTABLE_ => Shift(yy_state_307)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_311)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_312)
    NT_record_decl_body => Shift(yy_state_717)
    T_LIDENT | T_EXTEND => Reduce(0, NT_boption_MUTABLE_, yy_action_178)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_152)
    _ => Error
  }
}

// [76, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]
fn yy_state_717(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_718)
    _ => Error
  }
}

// [76, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_718(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_719)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [76, type_sig → PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]
fn yy_state_719(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_48)
}

// [64, type_sig → PUB pub_attr SUBERROR • uident optional_derive, EOF / SEMI]
// [70, type_sig → PUB pub_attr SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]
fn yy_state_720(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_721)
    _ => Error
  }
}

// [64, type_sig → PUB pub_attr SUBERROR uident • optional_derive, EOF / SEMI]
// [70, type_sig → PUB pub_attr SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_721(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_LBRACE => Shift(yy_state_722)
    NT_optional_derive => Shift(yy_state_726)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [70, type_sig → PUB pub_attr SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_722(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_723)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [70, type_sig → PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_723(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_724)
    _ => Error
  }
}

// [70, type_sig → PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_724(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_725)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [70, type_sig → PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_725(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_47)
}

// [64, type_sig → PUB pub_attr SUBERROR uident optional_derive •, EOF / SEMI]
fn yy_state_726(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_46)
}

// [58, type_sig → PUB pub_attr TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [130, alias_sig → PUB pub_attr TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]
fn yy_state_727(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_728)
    _ => Error
  }
}

// [58, type_sig → PUB pub_attr TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [130, alias_sig → PUB pub_attr TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_728(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_EQUAL => Shift(yy_state_729)
    NT_optional_derive => Shift(yy_state_731)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [130, alias_sig → PUB pub_attr TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_729(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_730)
    _ => Error
  }
}

// [130, alias_sig → PUB pub_attr TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
fn yy_state_730(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_alias_sig, yy_action_45)
}

// [58, type_sig → PUB pub_attr TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]
fn yy_state_731(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_type_sig, yy_action_44)
}

// [41, func_sig → PUB pub_attr FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_732(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_733)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_733(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_734)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_734(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_735)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_735(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_736)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_736(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_737)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_737(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_738)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_738(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_739)
    _ => Error
  }
}

// [41, func_sig → PUB pub_attr FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_739(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_func_sig, yy_action_43)
}

// [40, func_sig → PUB pub_attr ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_740(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_741)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_741(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_742)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_742(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_743)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_743(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_744)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_744(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_745)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_745(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_746)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_746(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_747)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_747(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_748)
    _ => Error
  }
}

// [40, func_sig → PUB pub_attr ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_748(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(11, NT_func_sig, yy_action_42)
}

// [24, value_sig → PUB pub_attr LET • lident COLON type_, EOF / SEMI]
// [217, lident → • LIDENT, COLON]
// [218, lident → • EXTEND, COLON]
fn yy_state_749(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_750)
    _ => Error
  }
}

// [24, value_sig → PUB pub_attr LET lident • COLON type_, EOF / SEMI]
fn yy_state_750(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_751)
    _ => Error
  }
}

// [24, value_sig → PUB pub_attr LET lident COLON • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_751(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_752)
    _ => Error
  }
}

// [24, value_sig → PUB pub_attr LET lident COLON type_ •, EOF / SEMI]
fn yy_state_752(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_value_sig, yy_action_41)
}

// [18, const_sig → PUB pub_attr CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [216, uident → • UIDENT, COLON]
fn yy_state_753(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_754)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr CONST uident • COLON type_ EQUAL constant, EOF / SEMI]
fn yy_state_754(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_755)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [176, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [177, type_ → • simple_type, EQUAL]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [214, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]
fn yy_state_755(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_756)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr CONST uident COLON type_ • EQUAL constant, EOF / SEMI]
fn yy_state_756(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_757)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [197, constant → • TRUE, EOF / SEMI]
// [198, constant → • FALSE, EOF / SEMI]
// [199, constant → • BYTE, EOF / SEMI]
// [200, constant → • BYTES, EOF / SEMI]
// [201, constant → • CHAR, EOF / SEMI]
// [202, constant → • INT, EOF / SEMI]
// [203, constant → • MINUS INT, EOF / SEMI]
// [204, constant → • FLOAT, EOF / SEMI]
// [205, constant → • MINUS FLOAT, EOF / SEMI]
// [206, constant → • DOUBLE, EOF / SEMI]
// [207, constant → • MINUS DOUBLE, EOF / SEMI]
// [208, constant → • STRING, EOF / SEMI]
fn yy_state_757(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_548)
    T_DOUBLE => Shift(yy_state_549)
    T_FLOAT => Shift(yy_state_550)
    T_MINUS => Shift(yy_state_551)
    T_INT => Shift(yy_state_555)
    T_CHAR => Shift(yy_state_556)
    T_BYTES => Shift(yy_state_557)
    T_BYTE => Shift(yy_state_558)
    T_FALSE => Shift(yy_state_559)
    T_TRUE => Shift(yy_state_560)
    NT_constant => Shift(yy_state_758)
    _ => Error
  }
}

// [18, const_sig → PUB pub_attr CONST uident COLON type_ EQUAL constant •, EOF / SEMI]
fn yy_state_758(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_const_sig, yy_action_39)
}

// [17, const_sig → PRIV • CONST uident COLON type_ EQUAL constant, EOF / SEMI]
// [23, value_sig → PRIV • LET lident COLON type_, EOF / SEMI]
// [38, func_sig → PRIV • ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [39, func_sig → PRIV • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [57, type_sig → PRIV • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [63, type_sig → PRIV • SUBERROR uident optional_derive, EOF / SEMI]
// [69, type_sig → PRIV • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [75, type_sig → PRIV • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [81, type_sig → PRIV • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [87, type_sig → PRIV • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [93, type_sig → PRIV • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → PRIV • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [107, impl_sig → PRIV • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → PRIV • IMPL qualified_uident FOR type_, EOF / SEMI]
// [119, trait_sig → PRIV • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [129, alias_sig → PRIV • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [135, alias_sig → PRIV • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [141, alias_sig → PRIV • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → PRIV • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_759(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_USING => Shift(yy_state_760)
    T_FNALIAS => Shift(yy_state_771)
    T_TRAIT => Shift(yy_state_775)
    T_IMPL => Shift(yy_state_779)
    T_EXTENUM => Shift(yy_state_787)
    T_ENUM => Shift(yy_state_799)
    T_STRUCT => Shift(yy_state_805)
    T_SUBERROR => Shift(yy_state_815)
    T_TYPE => Shift(yy_state_822)
    T_FN => Shift(yy_state_827)
    T_ASYNC => Shift(yy_state_835)
    T_LET => Shift(yy_state_844)
    T_CONST => Shift(yy_state_848)
    _ => Error
  }
}

// [141, alias_sig → PRIV USING • PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → PRIV USING • PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_760(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_761)
    _ => Error
  }
}

// [141, alias_sig → PRIV USING PACKAGE_NAME • LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → PRIV USING PACKAGE_NAME • LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_761(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_762)
    _ => Error
  }
}

// [141, alias_sig → PRIV USING PACKAGE_NAME LBRACE • TYPE uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → PRIV USING PACKAGE_NAME LBRACE • TRAIT uident using_binder RBRACE, EOF / SEMI]
fn yy_state_762(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_TRAIT => Shift(yy_state_763)
    T_TYPE => Shift(yy_state_767)
    _ => Error
  }
}

// [147, alias_sig → PRIV USING PACKAGE_NAME LBRACE TRAIT • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_763(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_764)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [147, alias_sig → PRIV USING PACKAGE_NAME LBRACE TRAIT uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_764(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_765)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [147, alias_sig → PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_765(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_766)
    _ => Error
  }
}

// [147, alias_sig → PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_766(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_alias_sig, yy_action_38)
}

// [141, alias_sig → PRIV USING PACKAGE_NAME LBRACE TYPE • uident using_binder RBRACE, EOF / SEMI]
// [216, uident → • UIDENT, RBRACE / AS]
fn yy_state_767(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_768)
    _ => Error
  }
}

// [126, using_binder → •, RBRACE]
// [127, using_binder → • AS uident, RBRACE]
// [141, alias_sig → PRIV USING PACKAGE_NAME LBRACE TYPE uident • using_binder RBRACE, EOF / SEMI]
fn yy_state_768(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_AS => Shift(yy_state_29)
    NT_using_binder => Shift(yy_state_769)
    T_RBRACE => Reduce(0, NT_using_binder, yy_action_278)
    _ => Error
  }
}

// [141, alias_sig → PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder • RBRACE, EOF / SEMI]
fn yy_state_769(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_770)
    _ => Error
  }
}

// [141, alias_sig → PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE •, EOF / SEMI]
fn yy_state_770(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_alias_sig, yy_action_37)
}

// [135, alias_sig → PRIV FNALIAS • uident COLONCOLON lident, EOF / SEMI]
// [216, uident → • UIDENT, COLONCOLON]
fn yy_state_771(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_772)
    _ => Error
  }
}

// [135, alias_sig → PRIV FNALIAS uident • COLONCOLON lident, EOF / SEMI]
fn yy_state_772(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLONCOLON => Shift(yy_state_773)
    _ => Error
  }
}

// [135, alias_sig → PRIV FNALIAS uident COLONCOLON • lident, EOF / SEMI]
// [217, lident → • LIDENT, EOF / SEMI]
// [218, lident → • EXTEND, EOF / SEMI]
fn yy_state_773(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_774)
    _ => Error
  }
}

// [135, alias_sig → PRIV FNALIAS uident COLONCOLON lident •, EOF / SEMI]
fn yy_state_774(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_36)
}

// [119, trait_sig → PRIV TRAIT • uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [216, uident → • UIDENT, EOF / COLON / SEMI / LBRACE]
fn yy_state_775(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_776)
    _ => Error
  }
}

// [119, trait_sig → PRIV TRAIT uident • loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [235, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → •, EOF / SEMI / LBRACE]
// [236, loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) → • COLON separated_nonempty_list(PLUS,qualified_uident), EOF / SEMI / LBRACE]
fn yy_state_776(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_43)
    NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___ => Shift(yy_state_777)
    T_EOF | T_SEMI | T_LBRACE => Reduce(0, NT_loption_preceded_COLON_separated_nonempty_list_PLUS_qualified_uident___, yy_action_271)
    _ => Error
  }
}

// [119, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) • trait_body, EOF / SEMI]
// [124, trait_body → •, EOF / SEMI]
// [125, trait_body → • LBRACE separated_list(SEMI,trait_method_sig) RBRACE, EOF / SEMI]
fn yy_state_777(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_52)
    NT_trait_body => Shift(yy_state_778)
    T_EOF | T_SEMI => Reduce(0, NT_trait_body, yy_action_264)
    _ => Error
  }
}

// [119, trait_sig → PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body •, EOF / SEMI]
fn yy_state_778(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_trait_sig, yy_action_35)
}

// [107, impl_sig → PRIV IMPL • type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → PRIV IMPL • qualified_uident FOR type_, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, UIDENT / PACKAGE_NAME]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_779(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    T_LBRACKET => Shift(yy_state_144)
    NT_qualified_uident => Shift(yy_state_780)
    NT_type_params_with_constraints => Shift(yy_state_783)
    _ => Error
  }
}

// [113, impl_sig → PRIV IMPL qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_780(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_781)
    _ => Error
  }
}

// [113, impl_sig → PRIV IMPL qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_781(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_782)
    _ => Error
  }
}

// [113, impl_sig → PRIV IMPL qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_782(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_impl_sig, yy_action_34)
}

// [107, impl_sig → PRIV IMPL type_params_with_constraints • qualified_uident FOR type_, EOF / SEMI]
// [212, qualified_uident → • UIDENT, FOR]
// [213, qualified_uident → • PACKAGE_NAME DOT_UIDENT, FOR]
fn yy_state_783(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_48)
    T_UIDENT => Shift(yy_state_50)
    NT_qualified_uident => Shift(yy_state_784)
    _ => Error
  }
}

// [107, impl_sig → PRIV IMPL type_params_with_constraints qualified_uident • FOR type_, EOF / SEMI]
fn yy_state_784(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FOR => Shift(yy_state_785)
    _ => Error
  }
}

// [107, impl_sig → PRIV IMPL type_params_with_constraints qualified_uident FOR • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_785(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_786)
    _ => Error
  }
}

// [107, impl_sig → PRIV IMPL type_params_with_constraints qualified_uident FOR type_ •, EOF / SEMI]
fn yy_state_786(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_impl_sig, yy_action_33)
}

// [93, type_sig → PRIV EXTENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → PRIV EXTENUM • extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [169, extensible_enum_extension_target → • uident optional_type_params_no_constraints, PLUS_EQUAL]
// [170, extensible_enum_extension_target → • PACKAGE_NAME DOT_UIDENT optional_type_params_no_constraints, PLUS_EQUAL]
// [216, uident → • UIDENT, PLUS_EQUAL / LBRACKET / LBRACE]
fn yy_state_787(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    T_PACKAGE_NAME => Shift(yy_state_222)
    NT_uident => Shift(yy_state_235)
    NT_extensible_enum_extension_target => Shift(yy_state_788)
    NT_type_decl_name_with_params => Shift(yy_state_794)
    _ => Error
  }
}

// [99, type_sig → PRIV EXTENUM extensible_enum_extension_target • PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_788(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PLUS_EQUAL => Shift(yy_state_789)
    _ => Error
  }
}

// [99, type_sig → PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_789(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_790)
    _ => Error
  }
}

// [99, type_sig → PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_790(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_791)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [99, type_sig → PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_791(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_792)
    _ => Error
  }
}

// [99, type_sig → PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_792(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_793)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [99, type_sig → PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_793(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(8, NT_type_sig, yy_action_32)
}

// [93, type_sig → PRIV EXTENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_794(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_795)
    _ => Error
  }
}

// [93, type_sig → PRIV EXTENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_795(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_796)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [93, type_sig → PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_796(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_797)
    _ => Error
  }
}

// [93, type_sig → PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_797(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_798)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [93, type_sig → PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_798(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_31)
}

// [87, type_sig → PRIV ENUM • type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LBRACE]
// [216, uident → • UIDENT, LBRACKET / LBRACE]
fn yy_state_799(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_800)
    _ => Error
  }
}

// [87, type_sig → PRIV ENUM type_decl_name_with_params • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
fn yy_state_800(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LBRACE => Shift(yy_state_801)
    _ => Error
  }
}

// [87, type_sig → PRIV ENUM type_decl_name_with_params LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_801(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_802)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [87, type_sig → PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_802(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_803)
    _ => Error
  }
}

// [87, type_sig → PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_803(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_804)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [87, type_sig → PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_804(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_30)
}

// [75, type_sig → PRIV STRUCT • type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [81, type_sig → PRIV STRUCT • type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, LPAREN / LBRACE]
// [216, uident → • UIDENT, LPAREN / LBRACKET / LBRACE]
fn yy_state_805(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_806)
    _ => Error
  }
}

// [75, type_sig → PRIV STRUCT type_decl_name_with_params • LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [81, type_sig → PRIV STRUCT type_decl_name_with_params • LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
fn yy_state_806(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_807)
    T_LBRACE => Shift(yy_state_811)
    _ => Error
  }
}

// [81, type_sig → PRIV STRUCT type_decl_name_with_params LPAREN • separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [230, separated_list(COMMA,type_) → •, RPAREN]
// [231, separated_list(COMMA,type_) → • separated_nonempty_list(COMMA,type_), RPAREN]
// [255, separated_nonempty_list(COMMA,type_) → • type_, RPAREN]
// [256, separated_nonempty_list(COMMA,type_) → • type_ COMMA separated_nonempty_list(COMMA,type_), RPAREN]
fn yy_state_807(_lookahead : YYSymbol) -> YYDecision {
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
    NT_separated_nonempty_list_COMMA_type__ => Shift(yy_state_294)
    NT_separated_list_COMMA_type__ => Shift(yy_state_808)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_type__, yy_action_155)
    _ => Error
  }
}

// [81, type_sig → PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) • RPAREN optional_derive, EOF / SEMI]
fn yy_state_808(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_809)
    _ => Error
  }
}

// [81, type_sig → PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_809(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_810)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [81, type_sig → PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive •, EOF / SEMI]
fn yy_state_810(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_29)
}

// [75, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE • record_decl_body RBRACE optional_derive, EOF / SEMI]
// [156, record_decl_field → • boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [157, record_decl_field → • nonempty_list(attribute) boption(MUTABLE) lident COLON type_, SEMI / RBRACE]
// [158, record_decl_body → • separated_list(SEMI,record_decl_field), RBRACE]
// [245, boption(MUTABLE) → •, LIDENT / EXTEND]
// [246, boption(MUTABLE) → • MUTABLE, LIDENT / EXTEND]
// [247, separated_list(SEMI,record_decl_field) → •, RBRACE]
// [248, separated_list(SEMI,record_decl_field) → • separated_nonempty_list(SEMI,record_decl_field), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, LIDENT / MUTABLE / EXTEND]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), LIDENT / MUTABLE / EXTEND]
// [271, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field, RBRACE]
// [272, separated_nonempty_list(SEMI,record_decl_field) → • record_decl_field SEMI separated_nonempty_list(SEMI,record_decl_field), RBRACE]
fn yy_state_811(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_MUTABLE => Shift(yy_state_246)
    NT_record_decl_field => Shift(yy_state_299)
    NT_nonempty_list_attribute_ => Shift(yy_state_302)
    NT_boption_MUTABLE_ => Shift(yy_state_307)
    NT_separated_nonempty_list_SEMI_record_decl_field_ => Shift(yy_state_311)
    NT_separated_list_SEMI_record_decl_field_ => Shift(yy_state_312)
    NT_record_decl_body => Shift(yy_state_812)
    T_LIDENT | T_EXTEND => Reduce(0, NT_boption_MUTABLE_, yy_action_178)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_record_decl_field_, yy_action_152)
    _ => Error
  }
}

// [75, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body • RBRACE optional_derive, EOF / SEMI]
fn yy_state_812(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_813)
    _ => Error
  }
}

// [75, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_813(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_814)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [75, type_sig → PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive •, EOF / SEMI]
fn yy_state_814(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_28)
}

// [63, type_sig → PRIV SUBERROR • uident optional_derive, EOF / SEMI]
// [69, type_sig → PRIV SUBERROR • uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / SEMI / LBRACE]
fn yy_state_815(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_816)
    _ => Error
  }
}

// [63, type_sig → PRIV SUBERROR uident • optional_derive, EOF / SEMI]
// [69, type_sig → PRIV SUBERROR uident • LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_816(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_LBRACE => Shift(yy_state_817)
    NT_optional_derive => Shift(yy_state_821)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [69, type_sig → PRIV SUBERROR uident LBRACE • separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [152, enum_constructor → • UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [153, enum_constructor → • nonempty_list(attribute) UIDENT option(delimited(LPAREN,separated_nonempty_list(COMMA,constructor_param),RPAREN)) option(eq_tag), SEMI / RBRACE]
// [228, separated_list(SEMI,enum_constructor) → •, RBRACE]
// [229, separated_list(SEMI,enum_constructor) → • separated_nonempty_list(SEMI,enum_constructor), RBRACE]
// [257, nonempty_list(attribute) → • ATTRIBUTE, UIDENT]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), UIDENT]
// [265, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor, RBRACE]
// [266, separated_nonempty_list(SEMI,enum_constructor) → • enum_constructor SEMI separated_nonempty_list(SEMI,enum_constructor), RBRACE]
fn yy_state_817(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    NT_enum_constructor => Shift(yy_state_240)
    NT_nonempty_list_attribute_ => Shift(yy_state_243)
    T_UIDENT => Shift(yy_state_262)
    NT_separated_nonempty_list_SEMI_enum_constructor_ => Shift(yy_state_265)
    NT_separated_list_SEMI_enum_constructor_ => Shift(yy_state_818)
    T_RBRACE => Reduce(0, NT_separated_list_SEMI_enum_constructor_, yy_action_182)
    _ => Error
  }
}

// [69, type_sig → PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) • RBRACE optional_derive, EOF / SEMI]
fn yy_state_818(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RBRACE => Shift(yy_state_819)
    _ => Error
  }
}

// [69, type_sig → PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
fn yy_state_819(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    NT_optional_derive => Shift(yy_state_820)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [69, type_sig → PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive •, EOF / SEMI]
fn yy_state_820(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_type_sig, yy_action_27)
}

// [63, type_sig → PRIV SUBERROR uident optional_derive •, EOF / SEMI]
fn yy_state_821(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_26)
}

// [57, type_sig → PRIV TYPE • type_decl_name_with_params optional_derive, EOF / SEMI]
// [129, alias_sig → PRIV TYPE • type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [168, type_decl_name_with_params → • uident optional_type_params_no_constraints, EOF / DERIVE / EQUAL / SEMI]
// [216, uident → • UIDENT, EOF / DERIVE / EQUAL / SEMI / LBRACKET]
fn yy_state_822(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_284)
    NT_type_decl_name_with_params => Shift(yy_state_823)
    _ => Error
  }
}

// [57, type_sig → PRIV TYPE type_decl_name_with_params • optional_derive, EOF / SEMI]
// [104, optional_derive → •, EOF / SEMI]
// [105, optional_derive → • DERIVE LPAREN non_empty_list_commas(qualified_uident) RPAREN, EOF / SEMI]
// [129, alias_sig → PRIV TYPE type_decl_name_with_params • EQUAL type_, EOF / SEMI]
fn yy_state_823(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_DERIVE => Shift(yy_state_268)
    T_EQUAL => Shift(yy_state_824)
    NT_optional_derive => Shift(yy_state_826)
    T_EOF | T_SEMI => Reduce(0, NT_optional_derive, yy_action_163)
    _ => Error
  }
}

// [129, alias_sig → PRIV TYPE type_decl_name_with_params EQUAL • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_824(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_825)
    _ => Error
  }
}

// [129, alias_sig → PRIV TYPE type_decl_name_with_params EQUAL type_ •, EOF / SEMI]
fn yy_state_825(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_alias_sig, yy_action_25)
}

// [57, type_sig → PRIV TYPE type_decl_name_with_params optional_derive •, EOF / SEMI]
fn yy_state_826(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(4, NT_type_sig, yy_action_24)
}

// [39, func_sig → PRIV FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_827(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_828)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [39, func_sig → PRIV FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_828(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_829)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_829(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_830)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_830(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_831)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_831(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_832)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_832(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_833)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_833(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_834)
    _ => Error
  }
}

// [39, func_sig → PRIV FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_834(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(9, NT_func_sig, yy_action_22)
}

// [38, func_sig → PRIV ASYNC • FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_835(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_FN => Shift(yy_state_836)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN • loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [161, type_params_with_constraints → • LBRACKET separated_list(COMMA,type_param_with_constraints) RBRACKET, LIDENT / UIDENT / AMPER / EXTEND]
// [222, loption(type_params_with_constraints) → •, LIDENT / UIDENT / AMPER / EXTEND]
// [223, loption(type_params_with_constraints) → • type_params_with_constraints, LIDENT / UIDENT / AMPER / EXTEND]
fn yy_state_836(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_type_params_with_constraints => Shift(yy_state_143)
    T_LBRACKET => Shift(yy_state_144)
    NT_loption_type_params_with_constraints_ => Shift(yy_state_837)
    T_LIDENT | T_UIDENT | T_AMPER | T_EXTEND => Reduce(0, NT_loption_type_params_with_constraints_, yy_action_217)
    _ => Error
  }
}

// [28, method_self_type_coloncolon → • UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [29, method_self_type_coloncolon → • AMPER UIDENT COLONCOLON, LIDENT / UIDENT / EXTEND]
// [30, luident → • lident, LPAREN]
// [31, luident → • uident, LPAREN]
// [32, func_name → • luident, LPAREN]
// [33, func_name → • method_self_type_coloncolon luident, LPAREN]
// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) • func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [216, uident → • UIDENT, LPAREN]
// [217, lident → • LIDENT, LPAREN]
// [218, lident → • EXTEND, LPAREN]
fn yy_state_837(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_method_self_type_coloncolon => Shift(yy_state_336)
    NT_uident => Shift(yy_state_338)
    NT_lident => Shift(yy_state_339)
    NT_luident => Shift(yy_state_340)
    T_AMPER => Shift(yy_state_341)
    T_UIDENT => Shift(yy_state_344)
    NT_func_name => Shift(yy_state_838)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name • LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_838(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_LPAREN => Shift(yy_state_839)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN • separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, RPAREN / COMMA / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, RPAREN / COMMA / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, RPAREN / COMMA / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, RPAREN / COMMA / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, RPAREN / COMMA / QUESTION]
// [176, simple_type → • UNDERSCORE, RPAREN / COMMA / QUESTION]
// [177, type_ → • simple_type, RPAREN / COMMA]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, RPAREN / COMMA]
// [194, parameter → • type_, RPAREN / COMMA]
// [195, parameter → • POST_LABEL COLON type_, RPAREN / COMMA]
// [196, parameter → • label QUESTION COLON type_, RPAREN / COMMA]
// [214, qualified_uident_ → • UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, RPAREN / COMMA / QUESTION / LBRACKET]
// [217, lident → • LIDENT, QUESTION]
// [218, lident → • EXTEND, QUESTION]
// [219, label → • lident, QUESTION]
// [224, separated_list(COMMA,parameter) → •, RPAREN]
// [225, separated_list(COMMA,parameter) → • separated_nonempty_list(COMMA,parameter), RPAREN]
// [263, separated_nonempty_list(COMMA,parameter) → • parameter, RPAREN]
// [264, separated_nonempty_list(COMMA,parameter) → • parameter COMMA separated_nonempty_list(COMMA,parameter), RPAREN]
fn yy_state_839(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_parameter => Shift(yy_state_58)
    NT_lident => Shift(yy_state_61)
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
    NT_separated_list_COMMA_parameter_ => Shift(yy_state_840)
    T_RPAREN => Reduce(0, NT_separated_list_COMMA_parameter_, yy_action_260)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) • RPAREN THIN_ARROW return_type, EOF / SEMI]
fn yy_state_840(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_RPAREN => Shift(yy_state_841)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN • THIN_ARROW return_type, EOF / SEMI]
fn yy_state_841(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_THIN_ARROW => Shift(yy_state_842)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW • return_type, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [175, simple_type → • AMPER qualified_uident_, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [176, simple_type → • UNDERSCORE, EOF / RAISE / QUESTION / SEMI / NORAISE]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [186, return_type → • type_, EOF / SEMI]
// [187, return_type → • simple_type NORAISE, EOF / SEMI]
// [188, return_type → • simple_type RAISE, EOF / SEMI]
// [189, return_type → • simple_type RAISE error_type, EOF / SEMI]
// [190, return_type → • simple_type RAISE QUESTION, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / RAISE / QUESTION / SEMI / LBRACKET / NORAISE]
fn yy_state_842(_lookahead : YYSymbol) -> YYDecision {
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
    NT_return_type => Shift(yy_state_843)
    _ => Error
  }
}

// [38, func_sig → PRIV ASYNC FN loption(type_params_with_constraints) func_name LPAREN separated_list(COMMA,parameter) RPAREN THIN_ARROW return_type •, EOF / SEMI]
fn yy_state_843(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(10, NT_func_sig, yy_action_19)
}

// [23, value_sig → PRIV LET • lident COLON type_, EOF / SEMI]
// [217, lident → • LIDENT, COLON]
// [218, lident → • EXTEND, COLON]
fn yy_state_844(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EXTEND => Shift(yy_state_38)
    T_LIDENT => Shift(yy_state_39)
    NT_lident => Shift(yy_state_845)
    _ => Error
  }
}

// [23, value_sig → PRIV LET lident • COLON type_, EOF / SEMI]
fn yy_state_845(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_846)
    _ => Error
  }
}

// [23, value_sig → PRIV LET lident COLON • type_, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EOF / QUESTION / SEMI]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EOF / QUESTION / SEMI]
// [173, simple_type → • LPAREN type_ RPAREN, EOF / QUESTION / SEMI]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EOF / QUESTION / SEMI]
// [175, simple_type → • AMPER qualified_uident_, EOF / QUESTION / SEMI]
// [176, simple_type → • UNDERSCORE, EOF / QUESTION / SEMI]
// [177, type_ → • simple_type, EOF / SEMI]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EOF / SEMI]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EOF / SEMI]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EOF / SEMI]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EOF / SEMI]
// [214, qualified_uident_ → • UIDENT, EOF / QUESTION / SEMI / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EOF / QUESTION / SEMI / LBRACKET]
fn yy_state_846(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_847)
    _ => Error
  }
}

// [23, value_sig → PRIV LET lident COLON type_ •, EOF / SEMI]
fn yy_state_847(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_value_sig, yy_action_18)
}

// [17, const_sig → PRIV CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [216, uident → • UIDENT, COLON]
fn yy_state_848(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_849)
    _ => Error
  }
}

// [17, const_sig → PRIV CONST uident • COLON type_ EQUAL constant, EOF / SEMI]
fn yy_state_849(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_850)
    _ => Error
  }
}

// [17, const_sig → PRIV CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [176, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [177, type_ → • simple_type, EQUAL]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [214, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]
fn yy_state_850(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_851)
    _ => Error
  }
}

// [17, const_sig → PRIV CONST uident COLON type_ • EQUAL constant, EOF / SEMI]
fn yy_state_851(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_852)
    _ => Error
  }
}

// [17, const_sig → PRIV CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [197, constant → • TRUE, EOF / SEMI]
// [198, constant → • FALSE, EOF / SEMI]
// [199, constant → • BYTE, EOF / SEMI]
// [200, constant → • BYTES, EOF / SEMI]
// [201, constant → • CHAR, EOF / SEMI]
// [202, constant → • INT, EOF / SEMI]
// [203, constant → • MINUS INT, EOF / SEMI]
// [204, constant → • FLOAT, EOF / SEMI]
// [205, constant → • MINUS FLOAT, EOF / SEMI]
// [206, constant → • DOUBLE, EOF / SEMI]
// [207, constant → • MINUS DOUBLE, EOF / SEMI]
// [208, constant → • STRING, EOF / SEMI]
fn yy_state_852(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_548)
    T_DOUBLE => Shift(yy_state_549)
    T_FLOAT => Shift(yy_state_550)
    T_MINUS => Shift(yy_state_551)
    T_INT => Shift(yy_state_555)
    T_CHAR => Shift(yy_state_556)
    T_BYTES => Shift(yy_state_557)
    T_BYTE => Shift(yy_state_558)
    T_FALSE => Shift(yy_state_559)
    T_TRUE => Shift(yy_state_560)
    NT_constant => Shift(yy_state_853)
    _ => Error
  }
}

// [17, const_sig → PRIV CONST uident COLON type_ EQUAL constant •, EOF / SEMI]
fn yy_state_853(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(7, NT_const_sig, yy_action_16)
}

// [16, const_sig → CONST • uident COLON type_ EQUAL constant, EOF / SEMI]
// [216, uident → • UIDENT, COLON]
fn yy_state_854(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_UIDENT => Shift(yy_state_25)
    NT_uident => Shift(yy_state_855)
    _ => Error
  }
}

// [16, const_sig → CONST uident • COLON type_ EQUAL constant, EOF / SEMI]
fn yy_state_855(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_COLON => Shift(yy_state_856)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON • type_ EQUAL constant, EOF / SEMI]
// [171, simple_type → • simple_type QUESTION, EQUAL / QUESTION]
// [172, simple_type → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN, EQUAL / QUESTION]
// [173, simple_type → • LPAREN type_ RPAREN, EQUAL / QUESTION]
// [174, simple_type → • qualified_uident_ optional_type_arguments, EQUAL / QUESTION]
// [175, simple_type → • AMPER qualified_uident_, EQUAL / QUESTION]
// [176, simple_type → • UNDERSCORE, EQUAL / QUESTION]
// [177, type_ → • simple_type, EQUAL]
// [178, type_ → • ASYNC LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [179, type_ → • ASYNC LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [180, type_ → • LPAREN type_ COMMA RPAREN THIN_ARROW return_type, EQUAL]
// [181, type_ → • LPAREN type_ COMMA separated_nonempty_list(COMMA,type_) RPAREN THIN_ARROW return_type, EQUAL]
// [182, type_ → • ASYNC LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [183, type_ → • LPAREN RPAREN THIN_ARROW return_type, EQUAL]
// [184, type_ → • ASYNC LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [185, type_ → • LPAREN type_ RPAREN THIN_ARROW return_type, EQUAL]
// [214, qualified_uident_ → • UIDENT, EQUAL / QUESTION / LBRACKET]
// [215, qualified_uident_ → • PACKAGE_NAME DOT_UIDENT, EQUAL / QUESTION / LBRACKET]
fn yy_state_856(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_PACKAGE_NAME => Shift(yy_state_62)
    T_UIDENT => Shift(yy_state_64)
    T_ASYNC => Shift(yy_state_69)
    T_UNDERSCORE => Shift(yy_state_75)
    T_AMPER => Shift(yy_state_76)
    NT_qualified_uident_ => Shift(yy_state_78)
    T_LPAREN => Shift(yy_state_83)
    NT_simple_type => Shift(yy_state_106)
    NT_type_ => Shift(yy_state_857)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON type_ • EQUAL constant, EOF / SEMI]
fn yy_state_857(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EQUAL => Shift(yy_state_858)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON type_ EQUAL • constant, EOF / SEMI]
// [197, constant → • TRUE, EOF / SEMI]
// [198, constant → • FALSE, EOF / SEMI]
// [199, constant → • BYTE, EOF / SEMI]
// [200, constant → • BYTES, EOF / SEMI]
// [201, constant → • CHAR, EOF / SEMI]
// [202, constant → • INT, EOF / SEMI]
// [203, constant → • MINUS INT, EOF / SEMI]
// [204, constant → • FLOAT, EOF / SEMI]
// [205, constant → • MINUS FLOAT, EOF / SEMI]
// [206, constant → • DOUBLE, EOF / SEMI]
// [207, constant → • MINUS DOUBLE, EOF / SEMI]
// [208, constant → • STRING, EOF / SEMI]
fn yy_state_858(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_STRING => Shift(yy_state_548)
    T_DOUBLE => Shift(yy_state_549)
    T_FLOAT => Shift(yy_state_550)
    T_MINUS => Shift(yy_state_551)
    T_INT => Shift(yy_state_555)
    T_CHAR => Shift(yy_state_556)
    T_BYTES => Shift(yy_state_557)
    T_BYTE => Shift(yy_state_558)
    T_FALSE => Shift(yy_state_559)
    T_TRUE => Shift(yy_state_560)
    NT_constant => Shift(yy_state_859)
    _ => Error
  }
}

// [16, const_sig → CONST uident COLON type_ EQUAL constant •, EOF / SEMI]
fn yy_state_859(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(6, NT_const_sig, yy_action_13)
}

// [15, sig_ → value_sig •, EOF / SEMI]
fn yy_state_860(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_12)
}

// [14, sig_ → const_sig •, EOF / SEMI]
fn yy_state_861(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_11)
}

// [13, sig_ → impl_sig •, EOF / SEMI]
fn yy_state_862(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_10)
}

// [12, sig_ → trait_sig •, EOF / SEMI]
fn yy_state_863(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_9)
}

// [11, sig_ → alias_sig •, EOF / SEMI]
fn yy_state_864(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_8)
}

// [10, sig_ → type_sig •, EOF / SEMI]
fn yy_state_865(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_7)
}

// [9, sig_ → func_sig •, EOF / SEMI]
fn yy_state_866(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(1, NT_sig_, yy_action_6)
}

// [7, sigs → sig_ •, EOF]
// [8, sigs → sig_ • SEMI sigs, EOF]
fn yy_state_867(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_SEMI => Shift(yy_state_868)
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
// [56, type_sig → • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [57, type_sig → • PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [58, type_sig → • PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [59, type_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [60, type_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [61, type_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [62, type_sig → • SUBERROR uident optional_derive, EOF / SEMI]
// [63, type_sig → • PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [64, type_sig → • PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [65, type_sig → • nonempty_list(attribute) SUBERROR uident optional_derive, EOF / SEMI]
// [66, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [67, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [68, type_sig → • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [69, type_sig → • PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [70, type_sig → • PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [71, type_sig → • nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [72, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [73, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [74, type_sig → • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [75, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [76, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [77, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [78, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [79, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [80, type_sig → • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [81, type_sig → • PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [82, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [83, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [84, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [85, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [86, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [87, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [88, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [89, type_sig → • nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [90, type_sig → • nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [91, type_sig → • nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [92, type_sig → • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [93, type_sig → • PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [94, type_sig → • PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [95, type_sig → • nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → • nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [98, type_sig → • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → • PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, type_sig → • PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [101, type_sig → • nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [102, type_sig → • nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [103, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [106, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [107, impl_sig → • PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [108, impl_sig → • PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [109, impl_sig → • nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [110, impl_sig → • nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [111, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [112, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → • PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [114, impl_sig → • PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [115, impl_sig → • nonempty_list(attribute) IMPL qualified_uident FOR type_, EOF / SEMI]
// [116, impl_sig → • nonempty_list(attribute) PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [117, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [118, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [119, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [120, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [121, trait_sig → • nonempty_list(attribute) TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [122, trait_sig → • nonempty_list(attribute) PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [123, trait_sig → • nonempty_list(attribute) PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [128, alias_sig → • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [129, alias_sig → • PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [130, alias_sig → • PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [131, alias_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [132, alias_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [133, alias_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [134, alias_sig → • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [135, alias_sig → • PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [136, alias_sig → • PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [137, alias_sig → • nonempty_list(attribute) FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [138, alias_sig → • nonempty_list(attribute) PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [139, alias_sig → • nonempty_list(attribute) PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [140, alias_sig → • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [141, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [142, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [143, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [148, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [149, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [150, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [151, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [257, nonempty_list(attribute) → • ATTRIBUTE, PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_868(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_USING => Shift(yy_state_21)
    T_FNALIAS => Shift(yy_state_35)
    T_TRAIT => Shift(yy_state_41)
    T_IMPL => Shift(yy_state_213)
    T_EXTENUM => Shift(yy_state_221)
    T_ENUM => Shift(yy_state_283)
    T_STRUCT => Shift(yy_state_291)
    T_SUBERROR => Shift(yy_state_316)
    T_TYPE => Shift(yy_state_323)
    T_FN => Shift(yy_state_328)
    T_ASYNC => Shift(yy_state_346)
    T_LET => Shift(yy_state_355)
    NT_nonempty_list_attribute_ => Shift(yy_state_359)
    T_PUB => Shift(yy_state_663)
    T_PRIV => Shift(yy_state_759)
    T_CONST => Shift(yy_state_854)
    NT_value_sig => Shift(yy_state_860)
    NT_const_sig => Shift(yy_state_861)
    NT_impl_sig => Shift(yy_state_862)
    NT_trait_sig => Shift(yy_state_863)
    NT_alias_sig => Shift(yy_state_864)
    NT_type_sig => Shift(yy_state_865)
    NT_func_sig => Shift(yy_state_866)
    NT_sig_ => Shift(yy_state_867)
    NT_sigs => Shift(yy_state_869)
    T_EOF => Reduce(0, NT_sigs, yy_action_283)
    _ => Error
  }
}

// [8, sigs → sig_ SEMI sigs •, EOF]
fn yy_state_869(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(3, NT_sigs, yy_action_4)
}

// [1, mbti → PACKAGE STRING SEMI imports sigs • EOF, $]
fn yy_state_870(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EOF => Shift(yy_state_871)
    _ => Error
  }
}

// [1, mbti → PACKAGE STRING SEMI imports sigs EOF •, $]
fn yy_state_871(_lookahead : YYSymbol) -> YYDecision {
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
// [56, type_sig → • TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [57, type_sig → • PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [58, type_sig → • PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [59, type_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [60, type_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [61, type_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params optional_derive, EOF / SEMI]
// [62, type_sig → • SUBERROR uident optional_derive, EOF / SEMI]
// [63, type_sig → • PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [64, type_sig → • PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [65, type_sig → • nonempty_list(attribute) SUBERROR uident optional_derive, EOF / SEMI]
// [66, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident optional_derive, EOF / SEMI]
// [67, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident optional_derive, EOF / SEMI]
// [68, type_sig → • SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [69, type_sig → • PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [70, type_sig → • PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [71, type_sig → • nonempty_list(attribute) SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [72, type_sig → • nonempty_list(attribute) PRIV SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [73, type_sig → • nonempty_list(attribute) PUB pub_attr SUBERROR uident LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [74, type_sig → • STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [75, type_sig → • PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [76, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [77, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [78, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [79, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LBRACE record_decl_body RBRACE optional_derive, EOF / SEMI]
// [80, type_sig → • STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [81, type_sig → • PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [82, type_sig → • PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [83, type_sig → • nonempty_list(attribute) STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [84, type_sig → • nonempty_list(attribute) PRIV STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [85, type_sig → • nonempty_list(attribute) PUB pub_attr STRUCT type_decl_name_with_params LPAREN separated_list(COMMA,type_) RPAREN optional_derive, EOF / SEMI]
// [86, type_sig → • ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [87, type_sig → • PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [88, type_sig → • PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [89, type_sig → • nonempty_list(attribute) ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [90, type_sig → • nonempty_list(attribute) PRIV ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [91, type_sig → • nonempty_list(attribute) PUB pub_attr ENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [92, type_sig → • EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [93, type_sig → • PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [94, type_sig → • PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [95, type_sig → • nonempty_list(attribute) EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [96, type_sig → • nonempty_list(attribute) PRIV EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [97, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM type_decl_name_with_params LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [98, type_sig → • EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [99, type_sig → • PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [100, type_sig → • PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [101, type_sig → • nonempty_list(attribute) EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [102, type_sig → • nonempty_list(attribute) PRIV EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [103, type_sig → • nonempty_list(attribute) PUB pub_attr EXTENUM extensible_enum_extension_target PLUS_EQUAL LBRACE separated_list(SEMI,enum_constructor) RBRACE optional_derive, EOF / SEMI]
// [106, impl_sig → • IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [107, impl_sig → • PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [108, impl_sig → • PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [109, impl_sig → • nonempty_list(attribute) IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [110, impl_sig → • nonempty_list(attribute) PRIV IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [111, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL type_params_with_constraints qualified_uident FOR type_, EOF / SEMI]
// [112, impl_sig → • IMPL qualified_uident FOR type_, EOF / SEMI]
// [113, impl_sig → • PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [114, impl_sig → • PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [115, impl_sig → • nonempty_list(attribute) IMPL qualified_uident FOR type_, EOF / SEMI]
// [116, impl_sig → • nonempty_list(attribute) PRIV IMPL qualified_uident FOR type_, EOF / SEMI]
// [117, impl_sig → • nonempty_list(attribute) PUB pub_attr IMPL qualified_uident FOR type_, EOF / SEMI]
// [118, trait_sig → • TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [119, trait_sig → • PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [120, trait_sig → • PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [121, trait_sig → • nonempty_list(attribute) TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [122, trait_sig → • nonempty_list(attribute) PRIV TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [123, trait_sig → • nonempty_list(attribute) PUB pub_attr TRAIT uident loption(preceded(COLON,separated_nonempty_list(PLUS,qualified_uident))) trait_body, EOF / SEMI]
// [128, alias_sig → • TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [129, alias_sig → • PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [130, alias_sig → • PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [131, alias_sig → • nonempty_list(attribute) TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [132, alias_sig → • nonempty_list(attribute) PRIV TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [133, alias_sig → • nonempty_list(attribute) PUB pub_attr TYPE type_decl_name_with_params EQUAL type_, EOF / SEMI]
// [134, alias_sig → • FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [135, alias_sig → • PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [136, alias_sig → • PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [137, alias_sig → • nonempty_list(attribute) FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [138, alias_sig → • nonempty_list(attribute) PRIV FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [139, alias_sig → • nonempty_list(attribute) PUB pub_attr FNALIAS uident COLONCOLON lident, EOF / SEMI]
// [140, alias_sig → • USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [141, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [142, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [143, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [144, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [145, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TYPE uident using_binder RBRACE, EOF / SEMI]
// [146, alias_sig → • USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [147, alias_sig → • PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [148, alias_sig → • PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [149, alias_sig → • nonempty_list(attribute) USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [150, alias_sig → • nonempty_list(attribute) PRIV USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [151, alias_sig → • nonempty_list(attribute) PUB pub_attr USING PACKAGE_NAME LBRACE TRAIT uident using_binder RBRACE, EOF / SEMI]
// [257, nonempty_list(attribute) → • ATTRIBUTE, PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
// [258, nonempty_list(attribute) → • ATTRIBUTE nonempty_list(attribute), PUB / PRIV / STRUCT / ENUM / TRAIT / IMPL / ASYNC / FNALIAS / FN / LET / CONST / TYPE / SUBERROR / USING / EXTENUM]
fn yy_state_872(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_ATTRIBUTE => Shift(yy_state_19)
    T_USING => Shift(yy_state_21)
    T_FNALIAS => Shift(yy_state_35)
    T_TRAIT => Shift(yy_state_41)
    T_IMPL => Shift(yy_state_213)
    T_EXTENUM => Shift(yy_state_221)
    T_ENUM => Shift(yy_state_283)
    T_STRUCT => Shift(yy_state_291)
    T_SUBERROR => Shift(yy_state_316)
    T_TYPE => Shift(yy_state_323)
    T_FN => Shift(yy_state_328)
    T_ASYNC => Shift(yy_state_346)
    T_LET => Shift(yy_state_355)
    NT_nonempty_list_attribute_ => Shift(yy_state_359)
    T_PUB => Shift(yy_state_663)
    T_PRIV => Shift(yy_state_759)
    T_CONST => Shift(yy_state_854)
    NT_value_sig => Shift(yy_state_860)
    NT_const_sig => Shift(yy_state_861)
    NT_impl_sig => Shift(yy_state_862)
    NT_trait_sig => Shift(yy_state_863)
    NT_alias_sig => Shift(yy_state_864)
    NT_type_sig => Shift(yy_state_865)
    NT_func_sig => Shift(yy_state_866)
    NT_sig_ => Shift(yy_state_867)
    NT_sigs => Shift(yy_state_873)
    T_EOF => Reduce(0, NT_sigs, yy_action_283)
    _ => Error
  }
}

// [0, mbti → PACKAGE STRING imports sigs • EOF, $]
fn yy_state_873(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    T_EOF => Shift(yy_state_874)
    _ => Error
  }
}

// [0, mbti → PACKAGE STRING imports sigs EOF •, $]
fn yy_state_874(_lookahead : YYSymbol) -> YYDecision {
  ReduceNoLookahead(5, NT_mbti, yy_action_0)
}

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
  let mut lookahead : Option[(YYSymbol, (YYObj, @basic.Position, @basic.Position), Token?)] = None
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
        state_stack = @list.cons(next_state, state_stack)
        last_shifted_state_stack = state_stack
        state = next_state
        last_pos = la.1.2
        lookahead = None
      }
      Reduce(count, symbol, action)
      | ReduceNoLookahead(count, symbol, action) => {
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
            | ReduceNoLookahead(count, symbol, action) => continue count, symbol, action
            _ => panic()
          }
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

fn error(stack : @list.List[YYState], token : Token?, loc : (@basic.Position, @basic.Position)) -> Unit raise ParseError {
  let expected = []
  fn try_add(symbol : YYSymbol, kind : TokenKind) {
    fn go(stack : @list.List[YYState]) {
      match stack {
        Empty => ()
        More(state, ..) => {
          match state(symbol) {
            Accept | Shift(_) => expected.push(kind)
            Reduce(count, symbol, _) | ReduceNoLookahead(count, symbol, _) => {
              fn inner_go(stack : @list.List[YYState], count, symbol) {
                let stack = stack.drop(count)
                guard stack is More(state, ..)
                match state(symbol) {
                  Shift(state) => go(@list.cons(state, stack))
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
  for term in ([(T_CHAR, TK_CHAR), (T_INT, TK_INT), (T_BYTE, TK_BYTE), (T_BYTES, TK_BYTES), (T_FLOAT, TK_FLOAT), (T_DOUBLE, TK_DOUBLE), (T_STRING, TK_STRING), (T_MULTILINE_STRING, TK_MULTILINE_STRING), (T_MULTILINE_INTERP, TK_MULTILINE_INTERP), (T_INTERP, TK_INTERP), (T_BYTES_INTERP, TK_BYTES_INTERP), (T_REGEX_LITERAL, TK_REGEX_LITERAL), (T_REGEX_INTERP, TK_REGEX_INTERP), (T_ATTRIBUTE, TK_ATTRIBUTE), (T_LIDENT, TK_LIDENT), (T_UIDENT, TK_UIDENT), (T_POST_LABEL, TK_POST_LABEL), (T_COMMENT, TK_COMMENT), (T_NEWLINE, TK_NEWLINE), (T_INFIX1, TK_INFIX1), (T_INFIX2, TK_INFIX2), (T_INFIX3, TK_INFIX3), (T_INFIX4, TK_INFIX4), (T_AUGMENTED_ASSIGNMENT, TK_AUGMENTED_ASSIGNMENT), (T_PLUS_EQUAL, TK_PLUS_EQUAL), (T_EOF, TK_EOF), (T_FALSE, TK_FALSE), (T_TRUE, TK_TRUE), (T_PUB, TK_PUB), (T_PRIV, TK_PRIV), (T_READONLY, TK_READONLY), (T_IMPORT, TK_IMPORT), (T_EXTERN, TK_EXTERN), (T_BREAK, TK_BREAK), (T_CONTINUE, TK_CONTINUE), (T_STRUCT, TK_STRUCT), (T_ENUM, TK_ENUM), (T_TRAIT, TK_TRAIT), (T_DERIVE, TK_DERIVE), (T_IMPL, TK_IMPL), (T_WITH, TK_WITH), (T_RAISE, TK_RAISE), (T_THROW, TK_THROW), (T_TRY, TK_TRY), (T_CATCH, TK_CATCH), (T_ASYNC, TK_ASYNC), (T_TYPEALIAS, TK_TYPEALIAS), (T_TRAITALIAS, TK_TRAITALIAS), (T_FNALIAS, TK_FNALIAS), (T_EQUAL, TK_EQUAL), (T_EQ_TILDE, TK_EQ_TILDE), (T_LPAREN, TK_LPAREN), (T_RPAREN, TK_RPAREN), (T_COMMA, TK_COMMA), (T_MINUS, TK_MINUS), (T_QUESTION, TK_QUESTION), (T_EXCLAMATION, TK_EXCLAMATION), (T_DOT_LIDENT, TK_DOT_LIDENT), (T_DOT_UIDENT, TK_DOT_UIDENT), (T_DOT_INT, TK_DOT_INT), (T_DOT_LPAREN, TK_DOT_LPAREN), (T_COLONCOLON, TK_COLONCOLON), (T_COLON, TK_COLON), (T_SEMI, TK_SEMI), (T_LBRACKET, TK_LBRACKET), (T_LBRACKET_BAR, TK_LBRACKET_BAR), (T_PLUS, TK_PLUS), (T_RBRACKET, TK_RBRACKET), (T_BAR_RBRACKET, TK_BAR_RBRACKET), (T_UNDERSCORE, TK_UNDERSCORE), (T_BAR, TK_BAR), (T_LBRACE, TK_LBRACE), (T_RBRACE, TK_RBRACE), (T_AMPERAMPER, TK_AMPERAMPER), (T_AMPER, TK_AMPER), (T_CARET, TK_CARET), (T_BARBAR, TK_BARBAR), (T_PACKAGE_NAME, TK_PACKAGE_NAME), (T_AS, TK_AS), (T_PIPE, TK_PIPE), (T_PIPE_LEFT, TK_PIPE_LEFT), (T_LT_PLUS, TK_LT_PLUS), (T_LT_QUESTION, TK_LT_QUESTION), (T_ELSE, TK_ELSE), (T_FN, TK_FN), (T_IF, TK_IF), (T_LET, TK_LET), (T_PROOF_ASSERT, TK_PROOF_ASSERT), (T_PROOF_LET, TK_PROOF_LET), (T_CONST, TK_CONST), (T_MATCH, TK_MATCH), (T_MUTABLE, TK_MUTABLE), (T_TYPE, TK_TYPE), (T_FAT_ARROW, TK_FAT_ARROW), (T_THIN_ARROW, TK_THIN_ARROW), (T_WHILE, TK_WHILE), (T_RETURN, TK_RETURN), (T_DOTDOT, TK_DOTDOT), (T_RANGE_INCLUSIVE, TK_RANGE_INCLUSIVE), (T_RANGE_LT_INCLUSIVE, TK_RANGE_LT_INCLUSIVE), (T_RANGE_EXCLUSIVE, TK_RANGE_EXCLUSIVE), (T_RANGE_INCLUSIVE_REV, TK_RANGE_INCLUSIVE_REV), (T_RANGE_EXCLUSIVE_REV, TK_RANGE_EXCLUSIVE_REV), (T_ELLIPSIS, TK_ELLIPSIS), (T_TEST, TK_TEST), (T_LOOP, TK_LOOP), (T_GUARD, TK_GUARD), (T_DEFER, TK_DEFER), (T_FOR, TK_FOR), (T_IN, TK_IN), (T_IS, TK_IS), (T_SUBERROR, TK_SUBERROR), (T_AND, TK_AND), (T_FORALL, TK_FORALL), (T_EXISTS, TK_EXISTS), (T_IMPLIES, TK_IMPLIES), (T_LETREC, TK_LETREC), (T_USING, TK_USING), (T_DECLARE, TK_DECLARE), (T_NORAISE, TK_NORAISE), (T_NOBREAK, TK_NOBREAK), (T_WHERE, TK_WHERE), (T_ENUMVIEW, TK_ENUMVIEW), (T_EXTENUM, TK_EXTENUM), (T_TRY_QUESTION, TK_TRY_QUESTION), (T_TRY_EXCLAMATION, TK_TRY_EXCLAMATION), (T_LEXMATCH, TK_LEXMATCH), (T_LEXMATCH_QUESTION, TK_LEXMATCH_QUESTION), (T_LEXSCAN, TK_LEXSCAN), (T_EXTEND, TK_EXTEND), (T_PACKAGE, TK_PACKAGE)] : Array[(YYSymbol, TokenKind)]) {
    try_add(term.0, term.1)
  }
  match token {
    None => raise UnexpectedEndOfInput(loc.1, expected)
    Some(token) => raise UnexpectedToken(token, loc, expected)
  }
}

pub fn mbti(tokens : Array[(Token, @basic.Position, @basic.Position)], initial_pos? : @basic.Position) -> Mbti raise ParseError {
  yy_parse(
    tokens,
    yy_state_0,
    (it) => {
      guard it is YYObj_Mbti(result)
      result
    },
    initial_pos?,
  )
}

fn _get_symbol_start_pos(args : ArrayView[(YYObj, @basic.Position, @basic.Position)], last_pos : @basic.Position) -> @basic.Position {
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
