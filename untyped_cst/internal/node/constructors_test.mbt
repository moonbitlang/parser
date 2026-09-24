///|
fn constructor_test_position() -> @basic.Position {
  @basic.Position::{ fname: "constructors_test.mbt", lnum: 1, bol: 0, cnum: 0, }
}

///|
fn constructor_kind_index(kind : @node.NodeKind) -> Int {
  match kind {
    Impls => 0
    Expression => 1
    Missing => 2
    Error => 3
    Syntax_Token(_) => 4
    Syntax_Unexpected(_) => 5
    Impl_Import => 6
    Impl_Extend => 7
    Impl_Using => 8
    Impl_Function => 9
    Impl_Let => 10
    Impl_Const => 11
    Impl_TypeAbstract => 12
    Impl_TypeAlias => 13
    Impl_RecordStruct => 14
    Impl_TupleStruct => 15
    Impl_EnumVariant => 16
    Impl_ExtensibleEnum => 17
    Impl_ExtendEnum => 18
    Impl_Suberror => 19
    Impl_SuberrorEnum => 20
    Impl_EnumView => 21
    Impl_Trait => 22
    Impl_ImplMethod => 23
    Impl_ImplRelation => 24
    Impl_Test => 25
    Expr_Ident => 26
    Expr_Constant => 27
    Expr_Interp => 28
    Expr_BytesInterp => 29
    Expr_Block => 30
    Expr_Apply => 31
    Expr_DotApply => 32
    Expr_Field => 33
    Expr_ArrayGet => 34
    Expr_ArrayGetSlice => 35
    Expr_Array => 36
    Expr_ArraySpread => 37
    Expr_ArraySet => 38
    Expr_ArrayAugmentedSet => 39
    Expr_ListComprehension => 40
    Expr_Unit => 41
    Expr_Group => 42
    Expr_Tuple => 43
    Expr_Sequence => 44
    Expr_Constraint => 45
    Expr_Record => 46
    Expr_RecordUpdate => 47
    Expr_Map => 48
    Expr_Infix => 49
    Expr_Pipe => 50
    Expr_RevPipe => 51
    Expr_Implies => 52
    Expr_Unary => 53
    Expr_Assign => 54
    Expr_Mutate => 55
    Expr_If => 56
    Expr_Match => 57
    Expr_Try => 58
    Expr_TryOperator => 59
    Expr_For => 60
    Expr_ForEach => 61
    Expr_While => 62
    Expr_Function => 63
    Expr_Method => 64
    Expr_As => 65
    Expr_Is => 66
    Expr_LexMatch => 67
    Expr_LexScan => 68
    Expr_RegexMatch => 69
    Expr_TemplateWriting => 70
    Expr_Let => 71
    Expr_LetMut => 72
    Expr_LetAnd => 73
    Expr_LetFn => 74
    Expr_Return => 75
    Expr_Raise => 76
    Expr_Break => 77
    Expr_Continue => 78
    Expr_LabelledBlock => 79
    Expr_Defer => 80
    Expr_ErrDefer => 81
    Expr_Guard => 82
    Expr_Quantifier => 83
    Expr_Hole => 84
    Expr_Constr => 85
    Expr_MultilineString => 86
    Expr_MissingExpr => 87
    Expr_ProofAssert => 88
    Expr_ProofLet => 89
    Pattern_Var => 90
    Pattern_Any => 91
    Pattern_Constant => 92
    Pattern_Tuple => 93
    Pattern_Array => 94
    Pattern_Rest => 95
    Pattern_Record => 96
    Pattern_Map => 97
    Pattern_Constr => 98
    Pattern_ConstrAny => 99
    Pattern_SpecialConstr => 100
    Pattern_Or => 101
    Pattern_Alias => 102
    Pattern_Range => 103
    Pattern_Constraint => 104
    Pattern_With => 105
    Pattern_MissingPattern => 106
    Pattern_Group => 107
    ArrayPattern_Pattern => 108
    ArrayPattern_StringSpread => 109
    ArrayPattern_BytesSpread => 110
    ArrayPattern_ConstSpread => 111
    ArrayPatterns_Closed => 112
    ArrayPatterns_Open => 113
    Type_Name => 114
    Type_Tuple => 115
    Type_Arrow => 116
    Type_Application => 117
    Type_Option => 118
    Type_Object => 119
    Type_Any => 120
    Type_Group => 121
    Type_MissingType => 122
    ErrorType_Error => 123
    ErrorType_Default => 124
    ErrorType_Noraise => 125
    ErrorType_Maybe => 126
    TypeDesc_Abstract => 127
    TypeDesc_ExtensibleEnum => 128
    TypeDesc_ErrorNoPayload => 129
    TypeDesc_ErrorEnumPayload => 130
    TypeDesc_Variant => 131
    TypeDesc_ExtendEnum => 132
    TypeDesc_Record => 133
    TypeDesc_TupleStruct => 134
    TypeDesc_Alias => 135
    LexPattern_RegexInterp => 136
    RegexPattern_Placeholder => 137
    RegexPattern_Literal => 138
    RegexPattern_Reference => 139
    RegexPattern_Sequence => 140
    RegexPattern_Alternation => 141
    RegexPattern_Alias => 142
    Case_MatchCase => 143
    Case_LexScanCase => 144
    Argument_Positional => 145
    Argument_Labelled => 146
    Argument_LabelledPun => 147
    Argument_LabelledOption => 148
    Argument_LabelledOptionPun => 149
    ArrayElement_Regular => 150
    ArrayElement_Spread => 151
    Parameter_DiscardPositional => 152
    Parameter_Positional => 153
    Parameter_Labelled => 154
    Parameter_Optional => 155
    Parameter_QuestionOptional => 156
    Parameter_TypeDecl => 157
    Parameter_TypeVar => 158
    Parameter_ConstructorPositional => 159
    Parameter_ConstructorLabelled => 160
    Field_Declaration => 161
    Field_Definition => 162
    Field_Pattern => 163
    Field_MapExpression => 164
    Field_MapPattern => 165
    Field_Where => 166
    Field_RecordSpread => 167
    Binding_LetRec => 168
    Binding_For => 169
    Binding_ForEach => 170
    Binding_Regex => 171
    TraitMethod_Declaration => 172
    Name_Identifier => 173
    Name_Package => 174
    LongIdent_Ident => 175
    LongIdent_Dot => 176
    Name_Binder => 177
    Name_Label => 178
    Name_Type => 179
    Name_Constr => 180
    Name_ConstrId => 181
    Name_Var => 182
    Name_Constructor => 183
    Name_Field => 184
    Name_AliasTarget => 185
    Accessor_Label => 186
    Accessor_Index => 187
    Accessor_Newtype => 188
    Name_Test => 189
    Constant_Bool => 190
    Constant_Byte => 191
    Constant_Bytes => 192
    Constant_Char => 193
    Constant_Int => 194
    Constant_Int64 => 195
    Constant_UInt => 196
    Constant_UInt64 => 197
    Constant_Float => 198
    Constant_Double => 199
    Constant_String => 200
    Constant_Regex => 201
    Constant_BigInt => 202
    Operator_Infix => 203
    Operator_Augmented => 204
    Operator_UnaryPlus => 205
    Operator_UnaryMinus => 206
    Operator_UnaryNot => 207
    ListComprehensionKind_For => 208
    ListComprehensionKind_ForEach => 209
    DotDotBinder_None => 210
    DotDotBinder_Underscore => 211
    DotDotBinder_Binder => 212
    DotDotBinder_BinderAs => 213
    ConstructorExtraInfo_None => 214
    ConstructorExtraInfo_Package => 215
    ConstructorExtraInfo_TypeName => 216
    ConstructorExtraInfo_TypeNameWithConstrPackage => 217
    MultilineStringElement_String => 218
    MultilineStringElement_Interp => 219
    LexLiteral_String => 220
    LexLiteral_Regex => 221
    LexScanCasePattern_Pattern => 222
    LexScanCasePattern_Binder => 223
    LexScanCasePattern_Wildcard => 224
    ConstructorTag_Value => 225
    Language_Value => 226
    Stub_ModuleName => 227
    Stub_FunctionName => 228
    Stub_Code => 229
    DeclBody_Expr => 230
    DeclBody_Import => 231
    DeclBody_EmbeddedString => 232
    DeclBody_EmbeddedMultilineString => 233
    DeclBody_None => 234
    Visibility_Default => 235
    Visibility_Pub => 236
    Visibility_Priv => 237
    RangeKind_Inclusive => 238
    RangeKind_Exclusive => 239
    HoleKind_Incomplete => 240
    HoleKind_Todo => 241
    FnKind_Lambda => 242
    FnKind_Arrow => 243
    Group_Brace => 244
    Group_Paren => 245
    TrailingMark_Comma => 246
    TrailingMark_Semi => 247
    TrailingMark_None => 248
    TryOperatorKind_Exclamation => 249
    QuantifierKind_Forall => 250
    QuantifierKind_Exists => 251
    UsingKind_Value => 252
    UsingKind_Type => 253
    UsingKind_Trait => 254
    Openness_Open => 255
    Openness_Closed => 256
    ArrayKind_Array => 257
    ArrayKind_Iter => 258
    Flag_Async => 259
    Flag_Declare => 260
    Flag_Mutable => 261
    Flag_Pun => 262
    Flag_ReturnSelf => 263
    Flag_Conditional => 264
    Flag_HasTry => 265
    Flag_MatchAbsent => 266
    Flag_Faked => 267
    Flag_Augmented => 268
    Flag_Object => 269
    Flag_Exclamation => 270
    Attribute_Simple => 271
    Attribute_Qualified => 272
    Attribute_Name => 273
    Attribute_Qualifier => 274
    Attribute_RawPayload => 275
    Comment_InlineTrailing => 276
    Comment_Ownline => 277
    Comment_Content => 278
    Comment_LeadingBlankLine => 279
    Comment_TrailingBlankLine => 280
    Comment_ConsumedByDocstring => 281
    InterpSegment_Literal => 282
    InterpSegment_Source => 283
    Clause_ReturnType => 284
    Clause_Where => 285
    Clause_ConstructorDecl => 286
    Clause_UsingItem => 287
    Clause_ImportPackage => 288
    Clause_ExtendItem => 289
    Clause_WithBinding => 290
    Clause_DerivingDirective => 291
    List_ArgumentList => 292
    List_ParameterList => 293
    List_TraitMethodParameterList => 294
    List_TypeDeclParameterList => 295
    List_TypeVarBinderList => 296
    List_ConstructorParameterList => 297
    List_TupleStructTypeList => 298
    List_StructFieldList => 299
    List_TraitMethodList => 300
    List_RecordFieldList => 301
    List_MapFieldList => 302
    List_BlockStatementList => 303
    List_ContinueArgumentList => 304
    List_TupleElementList => 305
    List_TupleTypeList => 306
    List_ArrayElementList => 307
    List_TypeArgumentList => 308
    List_TuplePatternList => 309
    List_ConstructorPatternArgumentList => 310
    List_ArrayPatternList => 311
    List_RecordPatternFieldList => 312
    List_MapPatternFieldList => 313
    List_WhereFieldList => 314
    List_ObjectFieldList => 315
    List_ConstructorList => 316
    List_UsingItemList => 317
    List_ImportPackageList => 318
    List_ExtendItemList => 319
    List_DerivingList => 320
    List_MatchCaseList => 321
    List_LexScanCaseList => 322
    List_RegexBindingList => 323
    List_ForBindingList => 324
    List_ForEachBindingList => 325
    List_WithBindingList => 326
    Leaf(_) => 327
    Pattern_ConstrUnknown => 328
  }
}

///|
test "every NodeKind has one typed CstNode constructor" {
  let loc = @node.empty_loc(constructor_test_position())
  let span = @node.empty_span(0)
  let missing = @node.CstNode::missing(loc~, source_span=span)
  let nodes : Array[@node.CstNode] = [
    @node.CstNode::impls(loc~, source_span=span, entries=[]),
    @node.CstNode::expression(loc~, source_span=span, entries=[]),
    @node.CstNode::missing(loc~, source_span=span),
    @node.CstNode::error(loc~, source_span=span, entries=[]),
    @node.CstNode::syntax_token(token_kind=TK_FN, loc~, source_span=span, entries=[]),
    @node.CstNode::syntax_unexpected(
      token_kind=TK_NEWLINE,
      loc~,
      source_span=span,
      entries=[],
    ),
    @node.CstNode::impl_import(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_extend(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_using(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_function(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_let(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_const(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_type_abstract(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_type_alias(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_record_struct(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_tuple_struct(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_enum_variant(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_extensible_enum(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_extend_enum(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_suberror(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_suberror_enum(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_enum_view(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_trait(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_impl_method(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_impl_relation(loc~, source_span=span, entries=[]),
    @node.CstNode::impl_test(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_ident(loc~, source_span=span, name=[]),
    @node.CstNode::expr_constant(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_interp(loc~, source_span=span, token=missing),
    @node.CstNode::expr_bytes_interp(loc~, source_span=span, token=missing),
    @node.CstNode::expr_block(loc~, source_span=span, body=missing),
    @node.CstNode::expr_apply(loc~, source_span=span, func=missing, arg=missing),
    @node.CstNode::expr_dot_apply(loc~, source_span=span, self_=missing, error=[]),
    @node.CstNode::expr_field(
      loc~,
      source_span=span,
      self_=missing,
      token=missing,
      field=missing,
    ),
    @node.CstNode::expr_array_get(
      loc~,
      source_span=span,
      self_=missing,
      open=missing,
      index=missing,
      close=missing,
    ),
    @node.CstNode::expr_array_get_slice(
      loc~,
      source_span=span,
      self_=missing,
      open=missing,
      op=missing,
      close=missing,
    ),
    @node.CstNode::expr_array(
      loc~,
      source_span=span,
      body=missing,
      array_kind=missing,
    ),
    @node.CstNode::expr_array_spread(
      loc~,
      source_span=span,
      body=missing,
      array_kind=missing,
    ),
    @node.CstNode::expr_array_set(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_array_augmented_set(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_list_comprehension(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_unit(
      loc~,
      source_span=span,
      body=missing,
      faked=missing,
    ),
    @node.CstNode::expr_group(
      loc~,
      source_span=span,
      body=missing,
      group=missing,
    ),
    @node.CstNode::expr_tuple(loc~, source_span=span, body=missing),
    @node.CstNode::expr_sequence(loc~, source_span=span, body=missing),
    @node.CstNode::expr_constraint(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_record(loc~, source_span=span, body=missing),
    @node.CstNode::expr_record_update(loc~, source_span=span, body=missing),
    @node.CstNode::expr_map(loc~, source_span=span, body=missing),
    @node.CstNode::expr_infix(
      loc~,
      source_span=span,
      lhs=missing,
      op=missing,
      operator=missing,
      rhs=missing,
    ),
    @node.CstNode::expr_pipe(
      loc~,
      source_span=span,
      lhs=missing,
      op=missing,
      rhs=missing,
    ),
    @node.CstNode::expr_rev_pipe(
      loc~,
      source_span=span,
      lhs=missing,
      op=missing,
      rhs=missing,
    ),
    @node.CstNode::expr_implies(
      loc~,
      source_span=span,
      lhs=missing,
      op=missing,
      operator=missing,
      rhs=missing,
    ),
    @node.CstNode::expr_unary(
      loc~,
      source_span=span,
      op=missing,
      operator=missing,
      expr=missing,
    ),
    @node.CstNode::expr_assign(
      loc~,
      source_span=span,
      name=[],
      op=missing,
      operator=missing,
      rhs=missing,
    ),
    @node.CstNode::expr_mutate(
      loc~,
      source_span=span,
      self_=missing,
      token=missing,
      field=missing,
      op=missing,
      operator=missing,
      rhs=missing,
    ),
    @node.CstNode::expr_if(loc~, source_span=span, entries=[
      Kw(missing),
      Cond(missing),
      Body(missing),
    ]),
    @node.CstNode::expr_match(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_try(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_try_operator(
      loc~,
      source_span=span,
      kw=missing,
      body=missing,
    ),
    @node.CstNode::expr_for(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_for_each(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_while(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_function(loc~, source_span=span, kw=[], error=[]),
    @node.CstNode::expr_method(
      loc~,
      source_span=span,
      type_=missing,
      op=missing,
    ),
    @node.CstNode::expr_as(
      loc~,
      source_span=span,
      expr=missing,
      op=missing,
      type_=missing,
    ),
    @node.CstNode::expr_is(
      loc~,
      source_span=span,
      expr=missing,
      op=missing,
      pattern=missing,
    ),
    @node.CstNode::expr_lex_match(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_lex_scan(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_regex_match(
      loc~,
      source_span=span,
      expr=missing,
      op=missing,
      pattern=missing,
    ),
    @node.CstNode::expr_template_writing(
      loc~,
      source_span=span,
      expr=missing,
      op=missing,
      template=missing,
    ),
    @node.CstNode::expr_let(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_let_mut(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_let_and(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_let_fn(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_return(loc~, source_span=span, kw=missing),
    @node.CstNode::expr_raise(loc~, source_span=span, kw=missing, expr=missing),
    @node.CstNode::expr_break(loc~, source_span=span, kw=missing),
    @node.CstNode::expr_continue(loc~, source_span=span, kw=missing),
    @node.CstNode::expr_labelled_block(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_defer(loc~, source_span=span, kw=missing, expr=missing),
    @node.CstNode::expr_err_defer(
      loc~,
      source_span=span,
      kw=missing,
      expr=missing,
    ),
    @node.CstNode::expr_guard(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_quantifier(
      loc~,
      source_span=span,
      kw=missing,
      name=missing,
      op=missing,
      type_=missing,
      sep=missing,
      body=missing,
    ),
    @node.CstNode::expr_hole(loc~, source_span=span, token=missing),
    @node.CstNode::expr_constr(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_multiline_string(loc~, source_span=span, entries=[]),
    @node.CstNode::expr_missing_expr(loc~, source_span=span, error=[]),
    @node.CstNode::expr_proof_assert(
      loc~,
      source_span=span,
      kw=missing,
      expr=missing,
    ),
    @node.CstNode::expr_proof_let(
      loc~,
      source_span=span,
      kw=missing,
      name=missing,
      op=missing,
      expr=missing,
    ),
    @node.CstNode::pattern_var(loc~, source_span=span, entries=[Name(missing)]),
    @node.CstNode::pattern_any(loc~, source_span=span, name=missing),
    @node.CstNode::pattern_constant(loc~, source_span=span),
    @node.CstNode::pattern_tuple(loc~, source_span=span, body=missing),
    @node.CstNode::pattern_array(loc~, source_span=span, body=missing),
    @node.CstNode::pattern_rest(loc~, source_span=span, op=missing),
    @node.CstNode::pattern_record(loc~, source_span=span, body=missing),
    @node.CstNode::pattern_map(loc~, source_span=span, body=missing),
    @node.CstNode::pattern_constr(loc~, source_span=span, entries=[]),
    @node.CstNode::pattern_constr_any(
      loc~,
      source_span=span,
      type_=missing,
      op=missing,
      wildcard=missing,
    ),
    @node.CstNode::pattern_special_constr(
      loc~,
      source_span=span,
      name=missing,
      params=missing,
    ),
    @node.CstNode::pattern_or(loc~, source_span=span, entries=[]),
    @node.CstNode::pattern_alias(
      loc~,
      source_span=span,
      pattern=missing,
      op=missing,
      name=missing,
    ),
    @node.CstNode::pattern_range(
      loc~,
      source_span=span,
      lhs=missing,
      op=missing,
      rhs=missing,
    ),
    @node.CstNode::pattern_constraint(loc~, source_span=span, entries=[
      Pattern(missing),
      Op(missing),
      Type_(missing),
    ]),
    @node.CstNode::pattern_with(
      loc~,
      source_span=span,
      open=missing,
      pattern=missing,
      op=missing,
      body=missing,
      close=missing,
    ),
    @node.CstNode::pattern_missing_pattern(loc~, source_span=span, entries=[]),
    @node.CstNode::pattern_group(loc~, source_span=span),
    @node.CstNode::array_pattern_pattern(
      loc~,
      source_span=span,
      pattern=missing,
    ),
    @node.CstNode::array_pattern_string_spread(
      loc~,
      source_span=span,
      op=missing,
      str=missing,
      dot_dot_binder=missing,
    ),
    @node.CstNode::array_pattern_bytes_spread(
      loc~,
      source_span=span,
      op=missing,
      bytes=missing,
      dot_dot_binder=missing,
    ),
    @node.CstNode::array_pattern_const_spread(
      loc~,
      source_span=span,
      op=missing,
      const_=missing,
      dot_dot_binder=missing,
    ),
    @node.CstNode::array_patterns_closed(loc~, source_span=span, body=missing),
    @node.CstNode::array_patterns_open(loc~, source_span=span, body=missing),
    @node.CstNode::type_name(loc~, source_span=span, name=[], error=[]),
    @node.CstNode::type_tuple(loc~, source_span=span, body=missing),
    @node.CstNode::type_arrow(loc~, source_span=span, entries=[]),
    @node.CstNode::type_application(loc~, source_span=span, entries=[
      Func(missing),
      Arg(missing),
    ]),
    @node.CstNode::type_option(
      loc~,
      source_span=span,
      type_=missing,
      op=missing,
      operator=missing,
    ),
    @node.CstNode::type_object(loc~, source_span=span),
    @node.CstNode::type_any(loc~, source_span=span, name=missing),
    @node.CstNode::type_group(loc~, source_span=span, entries=[]),
    @node.CstNode::type_missing_type(loc~, source_span=span, error=[]),
    @node.CstNode::error_type_error(
      loc~,
      source_span=span,
      op=missing,
      type_=missing,
    ),
    @node.CstNode::error_type_default(loc~, source_span=span, op=missing),
    @node.CstNode::error_type_noraise(loc~, source_span=span, op=missing),
    @node.CstNode::error_type_maybe(
      loc~,
      source_span=span,
      op=missing,
      type_=missing,
    ),
    @node.CstNode::type_desc_abstract(loc~, source_span=span),
    @node.CstNode::type_desc_extensible_enum(loc~, source_span=span),
    @node.CstNode::type_desc_error_no_payload(loc~, source_span=span),
    @node.CstNode::type_desc_error_enum_payload(loc~, source_span=span),
    @node.CstNode::type_desc_variant(loc~, source_span=span),
    @node.CstNode::type_desc_extend_enum(loc~, source_span=span),
    @node.CstNode::type_desc_record(loc~, source_span=span),
    @node.CstNode::type_desc_tuple_struct(loc~, source_span=span),
    @node.CstNode::type_desc_alias(loc~, source_span=span),
    @node.CstNode::lex_pattern_regex_interp(loc~, source_span=span),
    @node.CstNode::regex_pattern_placeholder(
      loc~,
      source_span=span,
      open=missing,
      pattern=missing,
      error=[],
      close=missing,
    ),
    @node.CstNode::regex_pattern_literal(loc~, source_span=span),
    @node.CstNode::regex_pattern_reference(loc~, source_span=span, name=[]),
    @node.CstNode::regex_pattern_sequence(
      loc~,
      source_span=span,
      lhs=missing,
      op=missing,
      rhs=missing,
    ),
    @node.CstNode::regex_pattern_alternation(
      loc~,
      source_span=span,
      lhs=missing,
      op=missing,
      rhs=missing,
    ),
    @node.CstNode::regex_pattern_alias(
      loc~,
      source_span=span,
      pattern=missing,
      kw=missing,
      name=missing,
    ),
    @node.CstNode::case_match_case(
      loc~,
      source_span=span,
      pattern=missing,
      error=[],
      body=missing,
    ),
    @node.CstNode::case_lex_scan_case(
      loc~,
      source_span=span,
      pattern=missing,
      op=missing,
      body=missing,
    ),
    @node.CstNode::argument_positional(loc~, source_span=span, expr=missing),
    @node.CstNode::argument_labelled(
      loc~,
      source_span=span,
      name=missing,
      op=missing,
    ),
    @node.CstNode::argument_labelled_pun(loc~, source_span=span, name=missing),
    @node.CstNode::argument_labelled_option(
      loc~,
      source_span=span,
      name=missing,
      op=[],
      expr=missing,
    ),
    @node.CstNode::argument_labelled_option_pun(
      loc~,
      source_span=span,
      name=missing,
      op=missing,
    ),
    @node.CstNode::array_element_regular(loc~, source_span=span, expr=missing),
    @node.CstNode::array_element_spread(
      loc~,
      source_span=span,
      op=missing,
      expr=missing,
    ),
    @node.CstNode::parameter_discard_positional(loc~, source_span=span, entries=[]),
    @node.CstNode::parameter_positional(loc~, source_span=span, entries=[]),
    @node.CstNode::parameter_labelled(loc~, source_span=span, entries=[
      Pattern(missing),
    ]),
    @node.CstNode::parameter_optional(loc~, source_span=span, entries=[]),
    @node.CstNode::parameter_question_optional(loc~, source_span=span, entries=[
      Pattern(missing),
    ]),
    @node.CstNode::parameter_type_decl(loc~, source_span=span, name=missing),
    @node.CstNode::parameter_type_var(loc~, source_span=span, entries=[]),
    @node.CstNode::parameter_constructor_positional(
      loc~,
      source_span=span,
      type_=missing,
    ),
    @node.CstNode::parameter_constructor_labelled(
      loc~,
      source_span=span,
      name=missing,
      op=missing,
      type_=missing,
    ),
    @node.CstNode::field_declaration(loc~, source_span=span, entries=[]),
    @node.CstNode::field_definition(loc~, source_span=span, entries=[]),
    @node.CstNode::field_pattern(loc~, source_span=span, entries=[]),
    @node.CstNode::field_map_expression(loc~, source_span=span, entries=[]),
    @node.CstNode::field_map_pattern(loc~, source_span=span, entries=[]),
    @node.CstNode::field_where(
      loc~,
      source_span=span,
      name=missing,
      op=missing,
      expr=missing,
    ),
    @node.CstNode::field_record_spread(
      loc~,
      source_span=span,
      op=missing,
      expr=missing,
    ),
    @node.CstNode::binding_let_rec(loc~, source_span=span, entries=[]),
    @node.CstNode::binding_for(loc~, source_span=span, name=missing, error=[]),
    @node.CstNode::binding_for_each(
      loc~,
      source_span=span,
      name=missing,
      op=missing,
      expr=missing,
    ),
    @node.CstNode::binding_regex(loc~, source_span=span, name=missing),
    @node.CstNode::trait_method_declaration(loc~, source_span=span, entries=[
      Name(missing),
    ]),
    @node.CstNode::name_identifier(value="value", loc~, source_span=span),
    @node.CstNode::name_package(value="value", loc~, source_span=span),
    @node.CstNode::long_ident_ident(loc~, source_span=span, component=missing),
    @node.CstNode::long_ident_dot(loc~, source_span=span, component=[]),
    @node.CstNode::name_binder(loc~, source_span=span, long_ident=missing),
    @node.CstNode::name_label(loc~, source_span=span),
    @node.CstNode::name_type(loc~, source_span=span, long_ident=missing),
    @node.CstNode::name_constr(loc~, source_span=span, long_ident=missing),
    @node.CstNode::name_constr_id(loc~, source_span=span, long_ident=missing),
    @node.CstNode::name_var(loc~, source_span=span, long_ident=missing),
    @node.CstNode::name_constructor(loc~, source_span=span, long_ident=missing),
    @node.CstNode::name_field(loc~, source_span=span, long_ident=missing),
    @node.CstNode::name_alias_target(loc~, source_span=span, long_ident=missing),
    @node.CstNode::accessor_label(value="value", loc~, source_span=span),
    @node.CstNode::accessor_index(value=1, loc~, source_span=span),
    @node.CstNode::accessor_newtype(loc~, source_span=span),
    @node.CstNode::name_test(value="value", loc~, source_span=span),
    @node.CstNode::constant_bool(value=true, loc~, source_span=span),
    @node.CstNode::constant_byte(value="value", loc~, source_span=span),
    @node.CstNode::constant_bytes(value="value", loc~, source_span=span),
    @node.CstNode::constant_char(value="value", loc~, source_span=span),
    @node.CstNode::constant_int(value="value", loc~, source_span=span),
    @node.CstNode::constant_int64(value="value", loc~, source_span=span),
    @node.CstNode::constant_uint(value="value", loc~, source_span=span),
    @node.CstNode::constant_uint64(value="value", loc~, source_span=span),
    @node.CstNode::constant_float(value="value", loc~, source_span=span),
    @node.CstNode::constant_double(value="value", loc~, source_span=span),
    @node.CstNode::constant_string(value="value", loc~, source_span=span),
    @node.CstNode::constant_regex(value="value", loc~, source_span=span),
    @node.CstNode::constant_big_int(value="value", loc~, source_span=span),
    @node.CstNode::operator_infix(value="value", loc~, source_span=span),
    @node.CstNode::operator_augmented(value="value", loc~, source_span=span),
    @node.CstNode::operator_unary_plus(value="value", loc~, source_span=span),
    @node.CstNode::operator_unary_minus(value="value", loc~, source_span=span),
    @node.CstNode::operator_unary_not(value="value", loc~, source_span=span),
    @node.CstNode::list_comprehension_kind_for(loc~, source_span=span),
    @node.CstNode::list_comprehension_kind_for_each(loc~, source_span=span),
    @node.CstNode::dot_dot_binder_none(loc~, source_span=span),
    @node.CstNode::dot_dot_binder_underscore(loc~, source_span=span),
    @node.CstNode::dot_dot_binder_binder(loc~, source_span=span),
    @node.CstNode::dot_dot_binder_binder_as(loc~, source_span=span),
    @node.CstNode::constructor_extra_info_none(loc~, source_span=span),
    @node.CstNode::constructor_extra_info_package(loc~, source_span=span),
    @node.CstNode::constructor_extra_info_type_name(loc~, source_span=span),
    @node.CstNode::constructor_extra_info_type_name_with_constr_package(
      loc~,
      source_span=span,
    ),
    @node.CstNode::multiline_string_element_string(
      value="value",
      loc~,
      source_span=span,
    ),
    @node.CstNode::multiline_string_element_interp(loc~, source_span=span, segment=[]),
    @node.CstNode::lex_literal_string(value="value", loc~, source_span=span),
    @node.CstNode::lex_literal_regex(value="value", loc~, source_span=span),
    @node.CstNode::lex_scan_case_pattern_pattern(loc~, source_span=span),
    @node.CstNode::lex_scan_case_pattern_binder(loc~, source_span=span),
    @node.CstNode::lex_scan_case_pattern_wildcard(loc~, source_span=span),
    @node.CstNode::constructor_tag_value(value="value", loc~, source_span=span),
    @node.CstNode::language_value(value="value", loc~, source_span=span),
    @node.CstNode::stub_module_name(value="value", loc~, source_span=span),
    @node.CstNode::stub_function_name(value="value", loc~, source_span=span),
    @node.CstNode::stub_code(value="value", loc~, source_span=span),
    @node.CstNode::decl_body_expr(loc~, source_span=span),
    @node.CstNode::decl_body_import(loc~, source_span=span),
    @node.CstNode::decl_body_embedded_string(loc~, source_span=span),
    @node.CstNode::decl_body_embedded_multiline_string(loc~, source_span=span),
    @node.CstNode::decl_body_none(loc~, source_span=span),
    @node.CstNode::visibility_default(loc~, source_span=span),
    @node.CstNode::visibility_pub(loc~, source_span=span, entries=[]),
    @node.CstNode::visibility_priv(loc~, source_span=span, kw=missing),
    @node.CstNode::range_kind_inclusive(loc~, source_span=span),
    @node.CstNode::range_kind_exclusive(loc~, source_span=span),
    @node.CstNode::hole_kind_incomplete(loc~, source_span=span),
    @node.CstNode::hole_kind_todo(loc~, source_span=span),
    @node.CstNode::fn_kind_lambda(loc~, source_span=span),
    @node.CstNode::fn_kind_arrow(loc~, source_span=span),
    @node.CstNode::group_brace(loc~, source_span=span),
    @node.CstNode::group_paren(loc~, source_span=span),
    @node.CstNode::trailing_mark_comma(loc~, source_span=span),
    @node.CstNode::trailing_mark_semi(loc~, source_span=span),
    @node.CstNode::trailing_mark_none(loc~, source_span=span),
    @node.CstNode::try_operator_kind_exclamation(loc~, source_span=span),
    @node.CstNode::quantifier_kind_forall(loc~, source_span=span),
    @node.CstNode::quantifier_kind_exists(loc~, source_span=span),
    @node.CstNode::using_kind_value(loc~, source_span=span),
    @node.CstNode::using_kind_type(loc~, source_span=span),
    @node.CstNode::using_kind_trait(loc~, source_span=span),
    @node.CstNode::openness_open(loc~, source_span=span),
    @node.CstNode::openness_closed(loc~, source_span=span),
    @node.CstNode::array_kind_array(loc~, source_span=span),
    @node.CstNode::array_kind_iter(loc~, source_span=span),
    @node.CstNode::flag_async(value=true, loc~, source_span=span),
    @node.CstNode::flag_declare(value=true, loc~, source_span=span),
    @node.CstNode::flag_mutable(value=true, loc~, source_span=span),
    @node.CstNode::flag_pun(value=true, loc~, source_span=span),
    @node.CstNode::flag_return_self(value=true, loc~, source_span=span),
    @node.CstNode::flag_conditional(value=true, loc~, source_span=span),
    @node.CstNode::flag_has_try(value=true, loc~, source_span=span),
    @node.CstNode::flag_match_absent(value=true, loc~, source_span=span),
    @node.CstNode::flag_faked(value=true, loc~, source_span=span),
    @node.CstNode::flag_augmented(value=true, loc~, source_span=span),
    @node.CstNode::flag_object(value=true, loc~, source_span=span),
    @node.CstNode::flag_exclamation(value=true, loc~, source_span=span),
    @node.CstNode::attribute_simple(
      loc~,
      source_span=span,
      name=missing,
      raw_payload=missing,
    ),
    @node.CstNode::attribute_qualified(
      loc~,
      source_span=span,
      qualifier=missing,
      name=missing,
      raw_payload=missing,
    ),
    @node.CstNode::attribute_name(value="value", loc~, source_span=span),
    @node.CstNode::attribute_qualifier(value="value", loc~, source_span=span),
    @node.CstNode::attribute_raw_payload(value="value", loc~, source_span=span),
    @node.CstNode::comment_inline_trailing(
      loc~,
      source_span=span,
      content=missing,
      consumed_by_docstring=missing,
    ),
    @node.CstNode::comment_ownline(
      loc~,
      source_span=span,
      content=missing,
      leading_blank_line=missing,
      trailing_blank_line=missing,
      consumed_by_docstring=missing,
    ),
    @node.CstNode::comment_content(value="value", loc~, source_span=span),
    @node.CstNode::comment_leading_blank_line(
      value=true,
      loc~,
      source_span=span,
    ),
    @node.CstNode::comment_trailing_blank_line(
      value=true,
      loc~,
      source_span=span,
    ),
    @node.CstNode::comment_consumed_by_docstring(
      value=true,
      loc~,
      source_span=span,
    ),
    @node.CstNode::interp_segment_literal(value="value", loc~, source_span=span),
    @node.CstNode::interp_segment_source(
      loc~,
      source_span=span,
      expr=missing,
      payload=missing,
    ),
    @node.CstNode::clause_return_type(loc~, source_span=span, entries=[]),
    @node.CstNode::clause_where(
      loc~,
      source_span=span,
      kw=missing,
      body=missing,
    ),
    @node.CstNode::clause_constructor_decl(loc~, source_span=span, attrs=[]),
    @node.CstNode::clause_using_item(loc~, source_span=span, target=missing),
    @node.CstNode::clause_import_package(loc~, source_span=span, entries=[
      Path(missing),
    ]),
    @node.CstNode::clause_extend_item(loc~, source_span=span, entries=[
      Target(missing),
    ]),
    @node.CstNode::clause_with_binding(loc~, source_span=span, error=[]),
    @node.CstNode::clause_deriving_directive(loc~, source_span=span, name=[]),
    @node.CstNode::list_argument_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_parameter_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_trait_method_parameter_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_type_decl_parameter_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_type_var_binder_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_constructor_parameter_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_tuple_struct_type_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_struct_field_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_trait_method_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_record_field_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_map_field_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_block_statement_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_continue_argument_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_tuple_element_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_tuple_type_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_array_element_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_type_argument_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_tuple_pattern_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_constructor_pattern_argument_list(
      loc~,
      source_span=span,
      entries=[],
    ),
    @node.CstNode::list_array_pattern_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_record_pattern_field_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_map_pattern_field_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_where_field_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_object_field_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_constructor_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_using_item_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_import_package_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_extend_item_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_deriving_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_match_case_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_lex_scan_case_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_regex_binding_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_for_binding_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_for_each_binding_list(loc~, source_span=span, entries=[]),
    @node.CstNode::list_with_binding_list(loc~, source_span=span, entries=[]),
    @node.CstNode::leaf(
      payload=@node.NodePayload::String("payload"),
      loc~,
      source_span=span,
    ),
    @node.CstNode::pattern_constr_unknown(
      loc~,
      source_span=span,
      type_=missing,
      op=missing,
      wildcard=missing,
    ),
  ]
  @test.assert_eq(nodes.length(), 329)
  for i, node in nodes {
    @test.assert_eq(constructor_kind_index(node.kind), i)
  }
}
