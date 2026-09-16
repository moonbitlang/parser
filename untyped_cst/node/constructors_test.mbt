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
    Syntax_Keyword => 4
    Syntax_Delimiter => 5
    Syntax_Separator => 6
    Syntax_Operator => 7
    Syntax_Identifier => 8
    Syntax_Label => 9
    Syntax_Accessor => 10
    Syntax_Literal => 11
    Syntax_Interpolation => 12
    Syntax_Attribute => 13
    Syntax_Comment => 14
    Syntax_Eof => 15
    Syntax_Unexpected => 16
    Impl_Import => 17
    Impl_Extend => 18
    Impl_Using => 19
    Impl_Function => 20
    Impl_Let => 21
    Impl_Const => 22
    Impl_TypeAbstract => 23
    Impl_TypeAlias => 24
    Impl_RecordStruct => 25
    Impl_TupleStruct => 26
    Impl_EnumVariant => 27
    Impl_ExtensibleEnum => 28
    Impl_ExtendEnum => 29
    Impl_Suberror => 30
    Impl_SuberrorEnum => 31
    Impl_EnumView => 32
    Impl_Trait => 33
    Impl_ImplMethod => 34
    Impl_ImplRelation => 35
    Impl_Test => 36
    Expr_Ident => 37
    Expr_Constant => 38
    Expr_Interp => 39
    Expr_BytesInterp => 40
    Expr_Block => 41
    Expr_Apply => 42
    Expr_DotApply => 43
    Expr_Field => 44
    Expr_ArrayGet => 45
    Expr_ArrayGetSlice => 46
    Expr_Array => 47
    Expr_ArraySpread => 48
    Expr_ArraySet => 49
    Expr_ArrayAugmentedSet => 50
    Expr_ListComprehension => 51
    Expr_Unit => 52
    Expr_Group => 53
    Expr_Tuple => 54
    Expr_Sequence => 55
    Expr_Constraint => 56
    Expr_Record => 57
    Expr_RecordUpdate => 58
    Expr_Map => 59
    Expr_Infix => 60
    Expr_Pipe => 61
    Expr_RevPipe => 62
    Expr_Implies => 63
    Expr_Unary => 64
    Expr_Assign => 65
    Expr_Mutate => 66
    Expr_If => 67
    Expr_Match => 68
    Expr_Try => 69
    Expr_TryOperator => 70
    Expr_For => 71
    Expr_ForEach => 72
    Expr_While => 73
    Expr_Function => 74
    Expr_Method => 75
    Expr_As => 76
    Expr_Is => 77
    Expr_LexMatch => 78
    Expr_LexScan => 79
    Expr_RegexMatch => 80
    Expr_TemplateWriting => 81
    Expr_Let => 82
    Expr_LetMut => 83
    Expr_LetAnd => 84
    Expr_LetFn => 85
    Expr_Return => 86
    Expr_Raise => 87
    Expr_Break => 88
    Expr_Continue => 89
    Expr_LabelledBlock => 90
    Expr_Defer => 91
    Expr_ErrDefer => 92
    Expr_Guard => 93
    Expr_Quantifier => 94
    Expr_Hole => 95
    Expr_Constr => 96
    Expr_MultilineString => 97
    Expr_MissingExpr => 98
    Expr_ProofAssert => 99
    Expr_ProofLet => 100
    Pattern_Var => 101
    Pattern_Any => 102
    Pattern_Constant => 103
    Pattern_Tuple => 104
    Pattern_Array => 105
    Pattern_Rest => 106
    Pattern_Record => 107
    Pattern_Map => 108
    Pattern_Constr => 109
    Pattern_ConstrAny => 110
    Pattern_SpecialConstr => 111
    Pattern_Or => 112
    Pattern_Alias => 113
    Pattern_Range => 114
    Pattern_Constraint => 115
    Pattern_With => 116
    Pattern_MissingPattern => 117
    Pattern_Group => 118
    ArrayPattern_Pattern => 119
    ArrayPattern_StringSpread => 120
    ArrayPattern_BytesSpread => 121
    ArrayPattern_ConstSpread => 122
    ArrayPatterns_Closed => 123
    ArrayPatterns_Open => 124
    Type_Name => 125
    Type_Tuple => 126
    Type_Arrow => 127
    Type_Application => 128
    Type_Option => 129
    Type_Object => 130
    Type_Any => 131
    Type_Group => 132
    Type_MissingType => 133
    ErrorType_Error => 134
    ErrorType_Default => 135
    ErrorType_Noraise => 136
    ErrorType_Maybe => 137
    TypeDesc_Abstract => 138
    TypeDesc_ExtensibleEnum => 139
    TypeDesc_ErrorNoPayload => 140
    TypeDesc_ErrorEnumPayload => 141
    TypeDesc_Variant => 142
    TypeDesc_ExtendEnum => 143
    TypeDesc_Record => 144
    TypeDesc_TupleStruct => 145
    TypeDesc_Alias => 146
    LexPattern_RegexInterp => 147
    RegexPattern_Placeholder => 148
    RegexPattern_Literal => 149
    RegexPattern_Reference => 150
    RegexPattern_Sequence => 151
    RegexPattern_Alternation => 152
    RegexPattern_Alias => 153
    Case_MatchCase => 154
    Case_LexScanCase => 155
    Argument_Positional => 156
    Argument_Labelled => 157
    Argument_LabelledPun => 158
    Argument_LabelledOption => 159
    Argument_LabelledOptionPun => 160
    ArrayElement_Regular => 161
    ArrayElement_Spread => 162
    Parameter_DiscardPositional => 163
    Parameter_Positional => 164
    Parameter_Labelled => 165
    Parameter_Optional => 166
    Parameter_QuestionOptional => 167
    Parameter_TypeDecl => 168
    Parameter_TypeVar => 169
    Parameter_ConstructorPositional => 170
    Parameter_ConstructorLabelled => 171
    Field_Declaration => 172
    Field_Definition => 173
    Field_Pattern => 174
    Field_MapExpression => 175
    Field_MapPattern => 176
    Field_Where => 177
    Field_RecordSpread => 178
    Binding_LetRec => 179
    Binding_For => 180
    Binding_ForEach => 181
    Binding_Regex => 182
    TraitMethod_Declaration => 183
    Name_Identifier => 184
    Name_Package => 185
    LongIdent_Ident => 186
    LongIdent_Dot => 187
    Name_Binder => 188
    Name_Label => 189
    Name_Type => 190
    Name_Constr => 191
    Name_ConstrId => 192
    Name_Var => 193
    Name_Constructor => 194
    Name_Field => 195
    Name_AliasTarget => 196
    Accessor_Label => 197
    Accessor_Index => 198
    Accessor_Newtype => 199
    Name_Test => 200
    Constant_Bool => 201
    Constant_Byte => 202
    Constant_Bytes => 203
    Constant_Char => 204
    Constant_Int => 205
    Constant_Int64 => 206
    Constant_UInt => 207
    Constant_UInt64 => 208
    Constant_Float => 209
    Constant_Double => 210
    Constant_String => 211
    Constant_Regex => 212
    Constant_BigInt => 213
    Operator_Infix => 214
    Operator_Augmented => 215
    Operator_UnaryPlus => 216
    Operator_UnaryMinus => 217
    Operator_UnaryNot => 218
    ListComprehensionKind_For => 219
    ListComprehensionKind_ForEach => 220
    DotDotBinder_None => 221
    DotDotBinder_Underscore => 222
    DotDotBinder_Binder => 223
    DotDotBinder_BinderAs => 224
    ConstructorExtraInfo_None => 225
    ConstructorExtraInfo_Package => 226
    ConstructorExtraInfo_TypeName => 227
    ConstructorExtraInfo_TypeNameWithConstrPackage => 228
    MultilineStringElement_String => 229
    MultilineStringElement_Interp => 230
    LexLiteral_String => 231
    LexLiteral_Regex => 232
    LexScanCasePattern_Pattern => 233
    LexScanCasePattern_Binder => 234
    LexScanCasePattern_Wildcard => 235
    ConstructorTag_Value => 236
    Language_Value => 237
    Stub_ModuleName => 238
    Stub_FunctionName => 239
    Stub_Code => 240
    Aggregate_Span => 241
    DeclBody_Expr => 242
    DeclBody_Import => 243
    DeclBody_EmbeddedString => 244
    DeclBody_EmbeddedMultilineString => 245
    DeclBody_None => 246
    Visibility_Default => 247
    Visibility_Pub => 248
    Visibility_Priv => 249
    RangeKind_Inclusive => 250
    RangeKind_Exclusive => 251
    HoleKind_Incomplete => 252
    HoleKind_Todo => 253
    FnKind_Lambda => 254
    FnKind_Arrow => 255
    Group_Brace => 256
    Group_Paren => 257
    TrailingMark_Comma => 258
    TrailingMark_Semi => 259
    TrailingMark_None => 260
    TryOperatorKind_Exclamation => 261
    QuantifierKind_Forall => 262
    QuantifierKind_Exists => 263
    UsingKind_Value => 264
    UsingKind_Type => 265
    UsingKind_Trait => 266
    Openness_Open => 267
    Openness_Closed => 268
    ArrayKind_Array => 269
    ArrayKind_Iter => 270
    Flag_Async => 271
    Flag_Declare => 272
    Flag_Mutable => 273
    Flag_Pun => 274
    Flag_ReturnSelf => 275
    Flag_Conditional => 276
    Flag_HasTry => 277
    Flag_MatchAbsent => 278
    Flag_Faked => 279
    Flag_Augmented => 280
    Flag_Object => 281
    Flag_Exclamation => 282
    Attribute_Simple => 283
    Attribute_Qualified => 284
    Attribute_Name => 285
    Attribute_Qualifier => 286
    Attribute_RawPayload => 287
    Comment_InlineTrailing => 288
    Comment_Ownline => 289
    Comment_Content => 290
    Comment_LeadingBlankLine => 291
    Comment_TrailingBlankLine => 292
    Comment_ConsumedByDocstring => 293
    InterpSegment_Literal => 294
    InterpSegment_Source => 295
    Clause_ReturnType => 296
    Clause_Where => 297
    Clause_ConstructorDecl => 298
    Clause_UsingItem => 299
    Clause_ImportPackage => 300
    Clause_ExtendItem => 301
    Clause_WithBinding => 302
    Clause_DerivingDirective => 303
    List_ArgumentList => 304
    List_ParameterList => 305
    List_TraitMethodParameterList => 306
    List_TypeDeclParameterList => 307
    List_TypeVarBinderList => 308
    List_ConstructorParameterList => 309
    List_TupleStructTypeList => 310
    List_StructFieldList => 311
    List_TraitMethodList => 312
    List_RecordFieldList => 313
    List_MapFieldList => 314
    List_BlockStatementList => 315
    List_ContinueArgumentList => 316
    List_TupleElementList => 317
    List_TupleTypeList => 318
    List_ArrayElementList => 319
    List_TypeArgumentList => 320
    List_TuplePatternList => 321
    List_ConstructorPatternArgumentList => 322
    List_ArrayPatternList => 323
    List_RecordPatternFieldList => 324
    List_MapPatternFieldList => 325
    List_WhereFieldList => 326
    List_ObjectFieldList => 327
    List_ConstructorList => 328
    List_UsingItemList => 329
    List_ImportPackageList => 330
    List_ExtendItemList => 331
    List_DerivingList => 332
    List_MatchCaseList => 333
    List_LexScanCaseList => 334
    List_RegexBindingList => 335
    List_ForBindingList => 336
    List_ForEachBindingList => 337
    List_WithBindingList => 338
    Leaf(_) => 339
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
    @node.CstNode::syntax_keyword(loc~, source_span=span),
    @node.CstNode::syntax_delimiter(loc~, source_span=span),
    @node.CstNode::syntax_separator(loc~, source_span=span),
    @node.CstNode::syntax_operator(loc~, source_span=span),
    @node.CstNode::syntax_identifier(loc~, source_span=span),
    @node.CstNode::syntax_label(loc~, source_span=span),
    @node.CstNode::syntax_accessor(loc~, source_span=span),
    @node.CstNode::syntax_literal(loc~, source_span=span),
    @node.CstNode::syntax_interpolation(loc~, source_span=span, segment=[]),
    @node.CstNode::syntax_attribute(loc~, source_span=span),
    @node.CstNode::syntax_comment(loc~, source_span=span),
    @node.CstNode::syntax_eof(loc~, source_span=span),
    @node.CstNode::syntax_unexpected(loc~, source_span=span),
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
    @node.CstNode::impl_test(loc~, source_span=span, attrs=[], kw=[], error=[]),
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
    @node.CstNode::expr_array_set(
      loc~,
      source_span=span,
      self_=missing,
      open=missing,
      index=missing,
      close=missing,
      index_loc=missing,
      op=missing,
      operator=missing,
      rhs=missing,
    ),
    @node.CstNode::expr_array_augmented_set(
      loc~,
      source_span=span,
      self_=missing,
      open=missing,
      index=missing,
      close=missing,
      index_loc=missing,
      op=missing,
      operator=missing,
      rhs=missing,
    ),
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
    @node.CstNode::expr_constraint(
      loc~,
      source_span=span,
      expr=missing,
      op=missing,
      type_=missing,
    ),
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
    @node.CstNode::expr_labelled_block(
      loc~,
      source_span=span,
      label=missing,
      label_loc=missing,
      op=missing,
      body=missing,
    ),
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
    @node.CstNode::pattern_or(
      loc~,
      source_span=span,
      lhs=missing,
      op=missing,
      rhs=missing,
    ),
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
    @node.CstNode::pattern_missing_pattern(loc~, source_span=span, error=[]),
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
    @node.CstNode::aggregate_span(loc~, source_span=span),
    @node.CstNode::decl_body_expr(loc~, source_span=span),
    @node.CstNode::decl_body_import(loc~, source_span=span),
    @node.CstNode::decl_body_embedded_string(loc~, source_span=span),
    @node.CstNode::decl_body_embedded_multiline_string(loc~, source_span=span),
    @node.CstNode::decl_body_none(loc~, source_span=span),
    @node.CstNode::visibility_default(loc~, source_span=span),
    @node.CstNode::visibility_pub(loc~, source_span=span, kw=missing),
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
    @node.CstNode::clause_import_package(loc~, source_span=span, path=missing),
    @node.CstNode::clause_extend_item(loc~, source_span=span, target=missing),
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
  ]
  @test.assert_eq(nodes.length(), 340)
  for i, node in nodes {
    @test.assert_eq(constructor_kind_index(node.kind), i)
  }
}
