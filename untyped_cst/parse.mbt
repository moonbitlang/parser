///|
fn delimiter_depth_delta(tok : Token) -> Int {
  match tok {
    LPAREN | LBRACKET | LBRACKET_BAR | LBRACE => 1
    RPAREN | RBRACKET | BAR_RBRACKET | RBRACE => -1
    _ => 0
  }
}

///|
fn next_depth(depth : Int, tok : Token) -> Int {
  let depth = depth + delimiter_depth_delta(tok)
  if depth < 0 {
    0
  } else {
    depth
  }
}

///|
fn next_paren_depth(depth : Int, tok : Token) -> Int {
  match tok {
    LPAREN => depth + 1
    RPAREN => if depth > 0 { depth - 1 } else { 0 }
    _ => depth
  }
}

///|
fn is_top_level_boundary(tok : Token, depth : Int) -> Bool {
  if depth == 0 {
    match tok {
      SEMI(_) | EOF => true
      _ => false
    }
  } else {
    false
  }
}

///|
fn top_level_decl_can_start(tok : Token) -> Bool {
  match tok {
    ATTRIBUTE(_)
    | PUB
    | PRIV
    | DECLARE
    | ASYNC
    | FN
    | LET
    | CONST
    | TYPE
    | STRUCT
    | ENUM
    | SUBERROR
    | EXTENUM
    | ENUMVIEW
    | TRAIT
    | IMPL
    | TEST
    | USING
    | IMPORT
    | EXTEND
    | EXTERN => true
    _ => false
  }
}

///|
fn top_level_decl_expected_tokens() -> Array[TokenKind] {
  [
    TK_IMPORT,
    TokenKind::TK_PUB,
    TokenKind::TK_PRIV,
    TokenKind::TK_TYPE,
    TokenKind::TK_SUBERROR,
    TokenKind::TK_ASYNC,
    TokenKind::TK_FN,
    TokenKind::TK_STRUCT,
    TokenKind::TK_ENUM,
    TokenKind::TK_EXTENUM,
    TokenKind::TK_LET,
    TokenKind::TK_CONST,
    TokenKind::TK_EXTERN,
    TokenKind::TK_TEST,
    TokenKind::TK_IMPL,
    TokenKind::TK_TRAIT,
    TokenKind::TK_ENUMVIEW,
    TokenKind::TK_ATTRIBUTE,
    TokenKind::TK_USING,
    TK_EXTEND,
  ]
}

///|
priv enum TopItemKind {
  Invalid
  Import
  Extend
  Using
  Function
  Let
  Const
  Type
  Struct
  Enum
  EnumView
  Trait
  Impl
  Test
}

///|
priv enum DeclPhase {
  Start
  AwaitingName
  AwaitingLetPattern
  AwaitingParameters
  AwaitingEqual
  AwaitingValue
  AwaitingType
  AwaitingBody
  AwaitingImplRelation
  AwaitingEnumViewConstructors
  AwaitingEnumViewFor
  AwaitingEnumViewWith
  AwaitingMethodName
  AwaitingUsingName
  AwaitingExternLanguage
  AwaitingExternFunction
  AwaitingStub
  Complete
}

///|
priv enum DeclExpectation {
  ExpectedTokens(Array[TokenKind])
  ExpectedDescription(String)
  NoExpectation
}

///|
fn decl_name_expectation(kind : TopItemKind) -> Array[TokenKind] {
  match kind {
    TopItemKind::Function =>
      [TokenKind::TK_LIDENT, TokenKind::TK_EXTEND, TokenKind::TK_PACKAGE_NAME]
    TopItemKind::Impl => [TokenKind::TK_UIDENT, TokenKind::TK_PACKAGE_NAME]
    _ => [TokenKind::TK_LIDENT]
  }
}

///|
fn decl_phase_expectation(
  phase : DeclPhase,
  kind : TopItemKind,
) -> DeclExpectation {
  match phase {
    AwaitingName => ExpectedTokens(decl_name_expectation(kind))
    AwaitingLetPattern =>
      ExpectedTokens([TokenKind::TK_LIDENT, TokenKind::TK_EXTEND])
    AwaitingParameters => ExpectedTokens([TokenKind::TK_LPAREN])
    AwaitingEqual => ExpectedTokens([TokenKind::TK_EQUAL])
    AwaitingValue => ExpectedDescription("simple expression")
    AwaitingType => ExpectedDescription("type")
    AwaitingBody | AwaitingEnumViewConstructors =>
      ExpectedTokens([TokenKind::TK_LBRACE])
    AwaitingImplRelation =>
      ExpectedTokens([TokenKind::TK_FOR, TokenKind::TK_WITH])
    AwaitingEnumViewFor => ExpectedTokens([TokenKind::TK_FOR])
    AwaitingEnumViewWith => ExpectedTokens([TokenKind::TK_WITH])
    AwaitingMethodName =>
      ExpectedTokens([TokenKind::TK_LIDENT, TokenKind::TK_EXTEND])
    AwaitingUsingName => ExpectedTokens([TokenKind::TK_PACKAGE_NAME])
    AwaitingExternLanguage | AwaitingStub =>
      ExpectedTokens([TokenKind::TK_STRING])
    AwaitingExternFunction => ExpectedTokens([TokenKind::TK_FN])
    Start | Complete => NoExpectation
  }
}

///|
fn decl_phase_after_name(kind : TopItemKind, is_suberror : Bool) -> DeclPhase {
  match kind {
    TopItemKind::Function => AwaitingBody
    TopItemKind::Const => AwaitingEqual
    TopItemKind::Type => Complete
    TopItemKind::Struct => AwaitingBody
    TopItemKind::Enum => if is_suberror { Complete } else { AwaitingBody }
    TopItemKind::Trait => AwaitingBody
    TopItemKind::Impl => AwaitingImplRelation
    TopItemKind::EnumView => AwaitingEnumViewConstructors
    Import | Extend => Complete
    _ => Complete
  }
}

///|
fn ParserState::report_decl_phase(
  self : ParserState,
  phase : DeclPhase,
  kind : TopItemKind,
) -> Unit {
  match decl_phase_expectation(phase, kind) {
    ExpectedTokens(tokens) => self.report_expected_tokens(tokens)
    ExpectedDescription(description) =>
      self.report_expected_description(description)
    NoExpectation => ()
  }
}

///|
fn ParserState::report_decl_phase_at(
  self : ParserState,
  phase : DeclPhase,
  kind : TopItemKind,
  found : Token,
  loc : Location,
) -> Unit {
  match decl_phase_expectation(phase, kind) {
    ExpectedTokens(tokens) => self.report_expected_tokens_at(found, loc, tokens)
    ExpectedDescription(description) =>
      self.report_expected_description_at(found, loc, description)
    NoExpectation => ()
  }
}

///|
fn decl_keyword_expects_name(tok : Token) -> Bool {
  match tok {
    FN
    | CONST
    | TYPE
    | STRUCT
    | ENUM
    | SUBERROR
    | EXTENUM
    | ENUMVIEW
    | TRAIT
    | IMPL => true
    _ => false
  }
}

///|
fn decl_keyword_kind(tok : Token) -> TopItemKind {
  match tok {
    FN => TopItemKind::Function
    CONST => TopItemKind::Const
    TYPE => TopItemKind::Type
    STRUCT => TopItemKind::Struct
    ENUM | SUBERROR | EXTENUM => TopItemKind::Enum
    ENUMVIEW => TopItemKind::EnumView
    TRAIT => TopItemKind::Trait
    IMPL => TopItemKind::Impl
    _ => TopItemKind::Invalid
  }
}

///|
fn decl_token_label(tok : Token) -> String? {
  match tok {
    ATTRIBUTE(_) => Some("attrs")
    PUB | PRIV => Some("vis")
    DECLARE
    | ASYNC
    | FN
    | LET
    | CONST
    | TYPE
    | STRUCT
    | ENUM
    | SUBERROR
    | EXTENUM
    | ENUMVIEW
    | TRAIT
    | IMPL
    | TEST
    | USING
    | EXTERN => Some("kw")
    SEMI(_) => Some("sep")
    EOF => Some("eof")
    _ => Some("item")
  }
}

///|
fn decl_prefix_token(tok : Token) -> Bool {
  match tok {
    ATTRIBUTE(_) | PUB | PRIV | DECLARE | ASYNC | TEST | EXTERN => true
    _ => false
  }
}

///|
fn parse_visibility_clause(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut is_pub = false
  let mut visibility_kind = Visibility_Default
  match state.peek() {
    Some((PUB, _, _)) => {
      is_pub = true
      visibility_kind = Visibility_Pub
      if state.consume_any() is Some(kw) {
        push_child(children, Some("kw"), kw)
      } else {
        ()
      }
    }
    Some((PRIV, _, _)) => {
      visibility_kind = Visibility_Priv
      if state.consume_any() is Some(kw) {
        push_child(children, Some("kw"), kw)
      } else {
        ()
      }
    }
    _ =>
      push_child(
        children,
        Some("kw"),
        state.missing_token_node(TokenKind::TK_PUB),
      )
  }
  if is_pub && state.current_is(TokenKind::TK_LPAREN) {
    push_child(children, Some("open"), state.expect_token(TokenKind::TK_LPAREN))
    match state.peek() {
      Some((LIDENT(_), _, _)) | Some((READONLY, _, _)) | Some((EXTEND, _, _)) =>
        if state.consume_any() is Some(attr) {
          push_child(children, Some("attr"), attr)
        } else {
          ()
        }
      Some((found, start, end)) => {
        state.report_expected_tokens_at(found, mk_loc(start, end), [
          TokenKind::TK_READONLY,
          TokenKind::TK_LIDENT,
          TokenKind::TK_EXTEND,
        ])
        while !state.current_is(TokenKind::TK_RPAREN) &&
              !state.current_is(TokenKind::TK_EOF) &&
              !(state.peek() is None) {
          if state.consume_any() is Some(bad) {
            push_child(
              children,
              Some("error"),
              error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
            )
          } else {
            ()
          }
        }
        push_child(
          children,
          Some("attr"),
          state.missing_token_node_silent(TokenKind::TK_LIDENT),
        )
      }
      None =>
        push_child(
          children,
          Some("attr"),
          state.missing_token_node_silent(TokenKind::TK_LIDENT),
        )
    }
    push_child(
      children,
      Some("close"),
      state.expect_token(TokenKind::TK_RPAREN),
    )
  }
  state.node_from_children(visibility_kind, children)
}

///|
fn child_label_is_missing(node : CstNode, label : String) -> Bool {
  node
  .first_child_with_label(label)
  .map(child => child.kind is Missing)
  .unwrap_or(false)
}

///|
fn is_name_token(tok : Token) -> Bool {
  match tok {
    LIDENT(_) | UIDENT(_) | PACKAGE_NAME(_) | EXTEND => true
    _ => false
  }
}

///|
fn decl_name_token_is_valid(tok : Token, kind : TopItemKind) -> Bool {
  match kind {
    TopItemKind::Function =>
      tok is (LIDENT(_) | UIDENT(_) | PACKAGE_NAME(_) | EXTEND)
    TopItemKind::Type
    | TopItemKind::Struct
    | TopItemKind::Enum
    | TopItemKind::EnumView
    | TopItemKind::Trait
    | TopItemKind::Impl => tok is (UIDENT(_) | PACKAGE_NAME(_))
    _ => is_name_token(tok)
  }
}

///|
fn parse_parameter_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut is_labelled = false
  let mut has_question = false
  let mut has_default = false
  let pat = match state.peek() {
    Some((EOF, _, _)) => {
      state.report_expected_description("parameter name or label")
      let pos = state.current_position()
      node(
        Pattern_MissingPattern,
        empty_loc(pos),
        empty_span(state.offset_for_position(pos)),
        [],
      )
    }
    Some((POST_LABEL(_), _, _)) => {
      is_labelled = true
      parse_single_token_pattern(state, Pattern_Var)
    }
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      parse_single_token_pattern(state, Pattern_Var)
    Some((UNDERSCORE, _, _)) => parse_single_token_pattern(state, Pattern_Any)
    Some((found, start, end)) => {
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "parameter name or label",
      )
      let invalid : Array[(String?, CstNode)] = []
      while !state.current_is(TokenKind::TK_COMMA) &&
            !state.current_is(TokenKind::TK_RPAREN) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(invalid, Some("error"), bad)
        } else {
          ()
        }
      }
      state.node_from_children(Pattern_MissingPattern, invalid)
    }
    None => state.node_from_children(Pattern_MissingPattern, [])
  }
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  if state.current_is(TokenKind::TK_QUESTION) {
    has_question = true
    if state.consume_any() is Some(question) {
      push_child(children, Some("question"), question)
    } else {
      ()
    }
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let reports_before_type = state.reports.length()
    let ty = parse_type_arrow(state, [
      TokenKind::TK_EQUAL,
      TokenKind::TK_COMMA,
      TokenKind::TK_RPAREN,
      TokenKind::TK_EOF,
    ])
    push_child(
      children,
      Some("type"),
      state.required_type_node(ty, reports_before_type),
    )
    push_children(children, state.consume_trivia())
  }
  if state.current_is(TokenKind::TK_EQUAL) {
    has_default = true
    if !is_labelled && !has_question {
      match state.peek() {
        Some((_, start, end)) =>
          state.report(
            mk_loc(start, end),
            "Only labeled argument can have default value",
          )
        None => ()
      }
    }
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
    let default = parse_expr_until(state, [
      TokenKind::TK_COMMA,
      TokenKind::TK_RPAREN,
      TokenKind::TK_EOF,
    ])
    if !default.children.is_empty() {
      push_child(children, Some("default"), default)
    }
  }
  let kind = if pat.kind is Pattern_Any {
    Parameter_DiscardPositional
  } else if is_labelled {
    if has_default {
      Parameter_Optional
    } else {
      Parameter_Labelled
    }
  } else if has_default {
    Parameter_Optional
  } else if has_question {
    Parameter_QuestionOptional
  } else {
    Parameter_Positional
  }
  state.node_from_children(kind, children)
}

///|
fn parse_parameter_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_parameter_item,
    List_ParameterList,
    sep_kind=TokenKind::TK_COMMA,
    follow_set=[TokenKind::TK_LBRACE],
    item_expected="parameter name or label",
    post_item_expected=[TokenKind::TK_COMMA, TokenKind::TK_RPAREN],
  )
}

///|
fn parse_function_stub_tokens(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> Unit {
  match state.peek() {
    Some((STRING(first_value), _, _)) => {
      let mut first : CstNode? = None
      if state.consume_any() is Some(stub) {
        first = Some(stub)
        push_child(children, Some("stub_token"), stub)
      } else {
        ()
      }
      push_children(children, state.consume_trivia())
      match state.peek() {
        Some((STRING(second_value), _, _)) =>
          if state.consume_any() is Some(stub) {
            push_child(children, Some("stub_token"), stub)
            if first is Some(first) {
              push_child(
                children,
                Some("stub"),
                scalar_node(
                  Stub_ModuleName,
                  first.loc,
                  first.source_span,
                  String(first_value),
                ),
              )
            }
            push_child(
              children,
              Some("stub"),
              scalar_node(
                Stub_FunctionName,
                stub.loc,
                stub.source_span,
                String(second_value),
              ),
            )
            push_child(
              children,
              Some("decl_body_kind"),
              marker_node(DeclBody_Import, stub.loc.end, stub.source_span.1),
            )
          } else {
            ()
          }
        _ =>
          if first is Some(first) {
            push_child(
              children,
              Some("stub"),
              scalar_node(
                Stub_Code,
                first.loc,
                first.source_span,
                String(first_value),
              ),
            )
            push_child(
              children,
              Some("decl_body_kind"),
              marker_node(
                DeclBody_EmbeddedString,
                first.loc.end,
                first.source_span.1,
              ),
            )
          }
      }
    }
    Some((MULTILINE_STRING(_), _, _)) => {
      let mut done = false
      let mut marked_kind = false
      while !done {
        match state.peek() {
          Some((MULTILINE_STRING(value), _, _)) => {
            if state.consume_any() is Some(stub) {
              push_child(children, Some("stub_token"), stub)
              push_child(
                children,
                Some("stub"),
                scalar_node(
                  Stub_Code,
                  stub.loc,
                  stub.source_span,
                  String(value),
                ),
              )
              if !marked_kind {
                marked_kind = true
                push_child(
                  children,
                  Some("decl_body_kind"),
                  marker_node(
                    DeclBody_EmbeddedMultilineString,
                    stub.loc.end,
                    stub.source_span.1,
                  ),
                )
              }
            } else {
              done = true
            }
            if state.next_significant_is_multiline_string() {
              push_children(children, state.consume_trivia())
            }
          }
          _ => done = true
        }
      }
    }
    _ => ()
  }
}

///|
fn decl_accepts_type_params(kind : TopItemKind) -> Bool {
  match kind {
    TopItemKind::Type | TopItemKind::Struct | TopItemKind::Enum => true
    _ => false
  }
}

///|
fn parse_type_parameter_item(
  state : ParserState,
  allow_constraints : Bool,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((UIDENT(_), _, _)) | Some((UNDERSCORE, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ =>
      push_child(
        children,
        Some("name"),
        state.missing_token_node(TokenKind::TK_UIDENT),
      )
  }
  if state.current_is(TokenKind::TK_COLON) {
    if !allow_constraints {
      state.report_expected_tokens([TokenKind::TK_COMMA, TokenKind::TK_RBRACKET])
    }
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let ty = parse_type_until(state, [
      TokenKind::TK_PLUS,
      TokenKind::TK_COMMA,
      TokenKind::TK_RBRACKET,
      TokenKind::TK_EOF,
    ])
    if !ty.children.is_empty() {
      push_child(children, Some("type"), ty)
    }
    while state.current_is(TokenKind::TK_PLUS) {
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let ty = parse_type_until(state, [
        TokenKind::TK_PLUS,
        TokenKind::TK_COMMA,
        TokenKind::TK_RBRACKET,
        TokenKind::TK_EOF,
      ])
      if !ty.children.is_empty() {
        push_child(children, Some("type"), ty)
      }
    }
  }
  state.node_from_children(
    if allow_constraints {
      Parameter_TypeVar
    } else {
      Parameter_TypeDecl
    },
    children,
  )
}

///|
fn parse_type_decl_param_item(state : ParserState) -> CstNode {
  parse_type_parameter_item(state, false)
}

///|
fn parse_fun_type_param_item(state : ParserState) -> CstNode {
  parse_type_parameter_item(state, true)
}

///|
fn parse_type_decl_parameter_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACKET,
    TokenKind::TK_RBRACKET,
    parse_type_decl_param_item,
    List_TypeDeclParameterList,
    sep_kind=TokenKind::TK_COMMA,
    require_nonempty=true,
  )
}

///|
fn parse_fun_type_parameter_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACKET,
    TokenKind::TK_RBRACKET,
    parse_fun_type_param_item,
    List_TypeVarBinderList,
    sep_kind=TokenKind::TK_COMMA,
    require_nonempty=true,
  )
}

///|
fn parse_tuple_struct_type_item(state : ParserState) -> CstNode {
  parse_type_arrow(state, [
    TokenKind::TK_COMMA,
    TokenKind::TK_RPAREN,
    TokenKind::TK_EOF,
  ])
}

///|
fn parse_tuple_struct_type_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_tuple_struct_type_item,
    List_TupleStructTypeList,
    sep_kind=TokenKind::TK_COMMA,
    require_nonempty=true,
    nonempty_expected="type",
  )
}

///|
fn decl_body_uses_balanced_group(kind : TopItemKind) -> Bool {
  match kind {
    TopItemKind::Struct | TopItemKind::Enum | TopItemKind::Trait => true
    _ => false
  }
}

///|
fn decl_body_list_kind(kind : TopItemKind) -> NodeKind {
  match kind {
    TopItemKind::Enum => List_ConstructorList
    TopItemKind::Struct => List_StructFieldList
    TopItemKind::Trait => List_TraitMethodList
    _ => List_BlockStatementList
  }
}

///|
fn parse_balanced_brace_group(
  state : ParserState,
  list_kind : NodeKind,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut depth = 0
  match state.peek() {
    Some((LBRACE, _, _)) =>
      if state.consume_any() is Some(open) {
        push_child(children, Some("open"), open)
        depth = 1
      } else {
        ()
      }
    _ => ()
  }
  while depth > 0 {
    match state.peek() {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) =>
        push_children(children, state.consume_trivia())
      Some((EOF, _, _)) | None => depth = 0
      Some((LBRACE, _, _)) =>
        if state.consume_any() is Some(open) {
          push_child(children, Some("open"), open)
          depth += 1
        } else {
          depth = 0
        }
      Some((RBRACE, _, _)) =>
        if state.consume_any() is Some(close) {
          push_child(children, Some("close"), close)
          depth -= 1
        } else {
          depth = 0
        }
      Some((tok, _, _)) =>
        if state.consume_any() is Some(consumed) {
          push_child(children, token_label(tok), consumed)
        } else {
          depth = 0
        }
    }
  }
  state.node_from_children(list_kind, children)
}

///|
fn parse_struct_member_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_struct_member_item,
    List_StructFieldList,
    sep_kind=TokenKind::TK_SEMI,
    invalid_delims=[TokenKind::TK_COMMA],
  )
}

///|
fn parse_enum_constructor_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_enum_constructor_item,
    List_ConstructorList,
    sep_kind=TokenKind::TK_SEMI,
    invalid_delims=[TokenKind::TK_COMMA],
  )
}

///|
fn parse_trait_member_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_trait_member_item,
    List_TraitMethodList,
    sep_kind=TokenKind::TK_SEMI,
  )
}

///|
fn parse_trait_member_item(state : ParserState) -> CstNode {
  let diagnostics_before = state.reports.length()
  let children : Array[(String?, CstNode)] = []
  let mut saw_params = false
  while state.current_is(TokenKind::TK_ATTRIBUTE) {
    if state.consume_any() is Some(attr) {
      push_child(children, Some("attrs"), attr)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  if state.current_is(TokenKind::TK_ASYNC) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  if state.current_is(TokenKind::TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    if state.current_is(TokenKind::TK_LBRACKET) {
      push_child(
        children,
        Some("quantifier"),
        parse_fun_type_parameter_list(state),
      )
    }
    push_children(children, state.consume_trivia())
  }
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    Some((UIDENT(name), start, end)) => {
      state.report(
        mk_loc(start, end),
        "Unexpected uppercase identifier \{name}, expected lowercase identifier in trait method name.",
      )
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    }
    _ => {
      state.report_expected_tokens([TokenKind::TK_LIDENT, TokenKind::TK_EXTEND])
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TokenKind::TK_LIDENT),
      )
    }
  }
  if state.reports.length() == diagnostics_before &&
    state.current_is(TokenKind::TK_LPAREN) {
    push_child(children, Some("params"), parse_trait_method_param_list(state))
    saw_params = true
  }
  if state.reports.length() == diagnostics_before &&
    state.current_is(TokenKind::TK_THIN_ARROW) {
    let reports_before_return = state.reports.length()
    push_child(children, Some("clause"), parse_func_return_type(state))
    if state.reports.length() > reports_before_return &&
      state.current_is(TokenKind::TK_LPAREN) {
      state.report_expected_tokens([TokenKind::TK_SEMI, TokenKind::TK_RBRACE])
    }
  }
  if state.reports.length() == diagnostics_before &&
    (
      state.current_is(TokenKind::TK_RAISE) ||
      state.current_is(TokenKind::TK_NORAISE) ||
      state.current_is(TokenKind::TK_NOCANCEL)
    ) {
    push_child(children, Some("clause"), parse_func_return_type(state))
  }
  if state.reports.length() == diagnostics_before &&
    state.current_is(TokenKind::TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
    match state.peek() {
      Some((UNDERSCORE, _, _)) =>
        if state.consume_any() is Some(stub) {
          push_child(children, Some("stub"), stub)
        } else {
          ()
        }
      _ => state.report_expected_tokens([TokenKind::TK_UNDERSCORE])
    }
  }
  if state.reports.length() == diagnostics_before && !saw_params {
    state.report_expected_tokens([TokenKind::TK_LPAREN])
  }
  state
  .node_from_children(TraitMethod_Declaration, children)
  .with_structured_name("semantic_name", Name_Binder, ["name"])
  .with_bool_marker(
    "async",
    Flag_Async,
    children
    .iter()
    .any(fn(entry) {
      entry.0 is Some("kw") && entry.1.has_child_kind(Flag_Async)
    }),
  )
  .with_async_location_anchor()
  .with_decl_like_function_location_anchor()
}

///|
fn parse_trait_method_param_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_trait_method_param_item,
    List_TraitMethodParameterList,
    sep_kind=TokenKind::TK_COMMA,
  )
}

///|
fn parse_trait_method_param_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut parameter_kind = Parameter_DiscardPositional
  match state.peek() {
    Some((tok, _, _)) if tok is (LIDENT(_) | POST_LABEL(_) | EXTEND) =>
      match state.peek(offset=1) {
        Some((QUESTION, _, _)) | Some((COLON, _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("pattern"), name)
            parameter_kind = if tok is POST_LABEL(_) {
              Parameter_Labelled
            } else {
              Parameter_Positional
            }
          } else {
            ()
          }
        _ => ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_QUESTION) {
    if parameter_kind is Parameter_Positional {
      parameter_kind = Parameter_QuestionOptional
    }
    if state.consume_any() is Some(question) {
      push_child(children, Some("op"), question)
    } else {
      ()
    }
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
  }
  let reports_before_type = state.reports.length()
  let ty = match state.peek() {
    Some((POST_LABEL(_), _, _))
    | Some((PACKAGE_NAME(_), _, _))
    | Some((UIDENT(_), _, _))
    | Some((LPAREN, _, _))
    | Some((UNDERSCORE, _, _))
    | Some((AMPER, _, _))
    | Some((ASYNC, _, _)) =>
      parse_type_arrow(state, [
        TokenKind::TK_COMMA,
        TokenKind::TK_RPAREN,
        TokenKind::TK_EOF,
      ])
    Some((COMMA | RPAREN | EOF, _, _)) => state.missing_type_node()
    Some((found, start, end)) => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_POST_LABEL,
        TokenKind::TK_PACKAGE_NAME,
        TokenKind::TK_UIDENT,
        TokenKind::TK_LPAREN,
        TokenKind::TK_UNDERSCORE,
        TokenKind::TK_AMPER,
        TokenKind::TK_ASYNC,
      ])
      let invalid : Array[(String?, CstNode)] = []
      while !state.current_is(TokenKind::TK_COMMA) &&
            !state.current_is(TokenKind::TK_RPAREN) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(invalid, Some("error"), bad)
        } else {
          ()
        }
      }
      state.node_from_children(Type_MissingType, invalid)
    }
    None => state.missing_type_node()
  }
  push_child(
    children,
    Some("type"),
    state.required_type_node(ty, reports_before_type),
  )
  state.node_from_children(parameter_kind, children)
}

///|
fn parse_enum_constructor_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  while state.current_is(TokenKind::TK_ATTRIBUTE) {
    if state.consume_any() is Some(attr) {
      push_child(children, Some("attrs"), attr)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  match state.peek() {
    Some((PUB, start, end)) | Some((PRIV, start, end)) => {
      state.report(
        mk_loc(start, end),
        "No individual visibility for enum constructor.",
      )
      if state.consume_any() is Some(vis) {
        push_child(children, Some("vis"), vis)
      } else {
        ()
      }
      push_children(children, state.consume_trivia())
    }
    _ => ()
  }
  match state.peek() {
    Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    Some((found, start, end)) => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_LIDENT,
      ])
      if state.consume_any() is Some(bad) {
        push_child(
          children,
          Some("error"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
    }
    None =>
      push_child(
        children,
        Some("name"),
        state.missing_token_node(TokenKind::TK_LIDENT),
      )
  }
  if state.current_is(TokenKind::TK_LPAREN) {
    push_child(children, Some("params"), parse_constructor_param_list(state))
  }
  if state.current_is(TokenKind::TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
    match state.peek() {
      Some((INT(value), _, _)) =>
        if state.consume_any() is Some(tag) {
          push_child(children, Some("tag"), tag)
          push_child(
            children,
            Some("constructor_tag"),
            scalar_node(
              ConstructorTag_Value,
              tag.loc,
              tag.source_span,
              String(value),
            ),
          )
        } else {
          ()
        }
      _ =>
        push_child(
          children,
          Some("tag"),
          state.missing_token_node(TokenKind::TK_INT),
        )
    }
  }
  state
  .node_from_children(Clause_ConstructorDecl, children)
  .with_structured_name("semantic_name", Name_Constr, ["name"])
}

///|
fn parse_constructor_param_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_constructor_param_item,
    List_ConstructorParameterList,
    sep_kind=TokenKind::TK_COMMA,
    post_item_expected=[TokenKind::TK_COMMA, TokenKind::TK_RPAREN],
    report_post_item_after_error=true,
  )
}

///|
fn parse_constructor_param_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.current_is(TokenKind::TK_MUTABLE) {
    if state.consume_any() is Some(mut_token) {
      push_child(children, Some("mut"), mut_token)
    } else {
      ()
    }
  }
  match (state.peek(), state.peek(offset=1)) {
    (Some((LIDENT(name), start, end)), Some((COLON, _, _))) => {
      state.report(
        mk_loc(start, end),
        "Unexpected lowercase identifier \{name}, expected uppercase identifier in type name.",
      )
      if state.consume_any() is Some(name_node) {
        push_child(children, Some("type"), name_node)
      } else {
        ()
      }
      return state
        .node_from_children(Parameter_ConstructorPositional, children)
        .with_bool_marker(
          "mutable",
          Flag_Mutable,
          children.iter().any(fn(entry) { entry.0 is Some("mut") }),
        )
    }
    _ => ()
  }
  match state.peek() {
    Some((POST_LABEL(_), _, _))
    | Some((LIDENT(_), _, _))
    | Some((EXTEND, _, _)) =>
      match state.peek(offset=1) {
        Some((COLON, _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("name"), name)
          } else {
            ()
          }
        _ => ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
  }
  let reports_before_type = state.reports.length()
  let ty = parse_type_arrow(state, [
    TokenKind::TK_COMMA,
    TokenKind::TK_RPAREN,
    TokenKind::TK_EOF,
  ])
  push_child(
    children,
    Some("type"),
    state.required_type_node(ty, reports_before_type),
  )
  state
  .node_from_children(
    if children.iter().any(fn(entry) { entry.0 is Some("name") }) {
      Parameter_ConstructorLabelled
    } else {
      Parameter_ConstructorPositional
    },
    children,
  )
  .with_bool_marker(
    "mutable",
    Flag_Mutable,
    children.iter().any(fn(entry) { entry.0 is Some("mut") }),
  )
}

///|
fn parse_struct_member_item(state : ParserState) -> CstNode {
  parse_struct_field_item(state)
}

///|
fn parse_struct_field_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut done_prefix = false
  while !done_prefix {
    match state.peek() {
      Some((ATTRIBUTE(_), _, _)) => {
        if state.consume_any() is Some(attr) {
          push_child(children, Some("attrs"), attr)
        } else {
          ()
        }
        push_children(children, state.consume_trivia())
      }
      Some((PUB, _, _)) | Some((PRIV, _, _)) => {
        push_child(children, Some("vis"), parse_visibility_clause(state))
        push_children(children, state.consume_trivia())
      }
      Some((MUTABLE, _, _)) => {
        if state.consume_any() is Some(mut_token) {
          push_child(children, Some("mut"), mut_token)
        } else {
          ()
        }
        push_children(children, state.consume_trivia())
      }
      _ => done_prefix = true
    }
  }
  match state.peek() {
    Some((FN | ASYNC, _, _)) =>
      return parse_invalid_struct_constructor(state, children)
    _ => ()
  }
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    Some((UIDENT(name), start, end)) => {
      state.report(
        mk_loc(start, end),
        "Unexpected uppercase identifier \{name}, expected lowercase identifier in struct field name.",
      )
      if state.consume_any() is Some(name_node) {
        push_child(children, Some("name"), name_node)
      } else {
        ()
      }
    }
    Some((found, start, end)) => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_LIDENT,
        TokenKind::TK_EXTEND,
      ])
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TokenKind::TK_LIDENT),
      )
    }
    None =>
      push_child(
        children,
        Some("name"),
        state.missing_token_node_with_expected(TokenKind::TK_LIDENT, [
          TokenKind::TK_LIDENT,
          TokenKind::TK_EXTEND,
        ]),
      )
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let reports_before_type = state.reports.length()
    let ty = parse_type_arrow(state, [
      TokenKind::TK_NEWLINE,
      TokenKind::TK_SEMI,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ])
    push_child(
      children,
      Some("type"),
      state.required_type_node(ty, reports_before_type),
    )
  }
  let field = state
    .node_from_children(Field_Declaration, children)
    .with_structured_name("semantic_name", Name_Field, ["name"])
    .with_bool_marker(
      "mutable",
      Flag_Mutable,
      children.iter().any(fn(entry) { entry.0 is Some("mut") }),
    )
  if field.first_child_with_label("vis") is None {
    with_marker_child(field, "vis", Visibility_Default)
  } else {
    field
  }
}

///|
fn parse_invalid_struct_constructor(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> CstNode {
  match state.peek() {
    Some((found, start, end)) =>
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_LIDENT,
        TokenKind::TK_EXTEND,
      ])
    None => ()
  }
  if state.current_is(TokenKind::TK_ASYNC) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  if state.current_is(TokenKind::TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
  }
  if state.current_is(TokenKind::TK_LBRACKET) {
    push_child(
      children,
      Some("quantifier"),
      parse_fun_type_parameter_list(state),
    )
  }
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_LPAREN) {
    push_child(children, Some("params"), parse_parameter_list(state))
  }
  if state.current_is(TokenKind::TK_THIN_ARROW) ||
    state.current_is(TokenKind::TK_RAISE) ||
    state.current_is(TokenKind::TK_NORAISE) ||
    state.current_is(TokenKind::TK_NOCANCEL) {
    push_child(children, Some("clause"), parse_func_return_type(state))
  }
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    push_child(
      children,
      Some("body"),
      state.missing_token_node_with_expected(TokenKind::TK_LBRACE, [
        TokenKind::TK_LBRACE,
      ]),
    )
  }
  state
  .node_from_children(Impl_Function, children)
  .with_structured_name("semantic_name", Name_Binder, ["name"])
  .with_bool_marker(
    "async",
    Flag_Async,
    children
    .iter()
    .any(fn(entry) {
      entry.0 is Some("kw") && entry.1.has_child_kind(Flag_Async)
    }),
  )
  .with_bool_marker(
    "declare",
    Flag_Declare,
    children
    .iter()
    .any(fn(entry) {
      entry.0 is Some("kw") && entry.1.has_child_kind(Flag_Declare)
    }),
  )
}

///|
fn parse_impl_method_item(state : ParserState) -> CstNode {
  let diagnostics_before = state.reports.length()
  let children : Array[(String?, CstNode)] = []
  let mut saw_params = false
  let mut saw_body = false
  let mut saw_stub = false
  let mut parsed_return_clause = false
  if state.current_is(TokenKind::TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    if state.current_is(TokenKind::TK_LBRACKET) {
      push_child(
        children,
        Some("quantifier"),
        parse_fun_type_parameter_list(state),
      )
    }
  }
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    Some((UIDENT(name), start, end)) => {
      state.report(
        mk_loc(start, end),
        "Unexpected uppercase identifier \{name}, expected lowercase identifier in name of method.",
      )
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    }
    _ => {
      state.report_expected_tokens([TokenKind::TK_LIDENT, TokenKind::TK_EXTEND])
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TokenKind::TK_LIDENT),
      )
    }
  }
  if state.reports.length() == diagnostics_before &&
    state.current_is(TokenKind::TK_LPAREN) {
    push_child(children, Some("params"), parse_parameter_list(state))
    saw_params = true
  }
  if state.reports.length() == diagnostics_before &&
    (
      state.current_is(TokenKind::TK_THIN_ARROW) ||
      state.current_is(TokenKind::TK_RAISE) ||
      state.current_is(TokenKind::TK_NORAISE) ||
      state.current_is(TokenKind::TK_NOCANCEL)
    ) {
    parsed_return_clause = true
    push_child(children, Some("clause"), parse_func_return_type(state))
  }
  if (state.reports.length() == diagnostics_before || parsed_return_clause) &&
    state.current_is(TokenKind::TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
    saw_body = true
  }
  if state.reports.length() == diagnostics_before &&
    state.current_is(TokenKind::TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
    match state.peek() {
      Some((STRING(_), _, _)) | Some((MULTILINE_STRING(_), _, _)) => {
        saw_stub = true
        parse_function_stub_tokens(state, children)
      }
      _ => state.report_expected_tokens([TokenKind::TK_STRING])
    }
  }
  if state.reports.length() == diagnostics_before {
    if !saw_params {
      state.report_expected_tokens([TokenKind::TK_LPAREN])
    } else if !saw_body && !saw_stub {
      state.report_expected_tokens([TokenKind::TK_LBRACE])
    }
  }
  state
  .node_from_children(Impl_Function, children)
  .with_structured_name("semantic_name", Name_Binder, ["name"])
  .with_bool_marker(
    "async",
    Flag_Async,
    children
    .iter()
    .any(fn(entry) {
      entry.0 is Some("kw") && entry.1.has_child_kind(Flag_Async)
    }),
  )
  .with_bool_marker(
    "declare",
    Flag_Declare,
    children
    .iter()
    .any(fn(entry) {
      entry.0 is Some("kw") && entry.1.has_child_kind(Flag_Declare)
    }),
  )
}

///|
fn parse_using_name_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_using_name_item,
    List_UsingItemList,
    sep_kind=TokenKind::TK_COMMA,
  )
}

///|
fn parse_using_name_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut name_kind = 0
  match state.peek() {
    Some((TYPE, _, _)) => {
      name_kind = 2
      if state.consume_any() is Some(kind) {
        push_child(children, Some("kind"), kind)
      } else {
        ()
      }
    }
    Some((TRAIT, _, _)) => {
      name_kind = 3
      if state.consume_any() is Some(kind) {
        push_child(children, Some("kind"), kind)
      } else {
        ()
      }
    }
    _ => ()
  }
  match state.peek() {
    Some((UIDENT(_), _, _)) => {
      if name_kind == 0 {
        name_kind = 1
      }
      if state.consume_any() is Some(name) {
        push_child(children, Some("target"), name)
      } else {
        ()
      }
    }
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("target"), name)
      } else {
        ()
      }
    _ =>
      push_child(
        children,
        Some("target"),
        state.missing_token_node_with_expected(TokenKind::TK_LIDENT, [
          TokenKind::TK_LIDENT,
          TokenKind::TK_EXTEND,
          TokenKind::TK_UIDENT,
          TokenKind::TK_TYPE,
          TokenKind::TK_TRAIT,
        ]),
      )
  }
  if state.current_is(TokenKind::TK_AS) {
    if state.consume_any() is Some(as_token) {
      push_child(children, Some("kw"), as_token)
    } else {
      ()
    }
    match state.peek() {
      Some((UIDENT(name), start, end)) => {
        if name_kind == 0 {
          state.report(
            mk_loc(start, end),
            "Unexpected uppercase identifier \{name}, expected lowercase identifier in value name.",
          )
        }
        if state.consume_any() is Some(alias_name) {
          push_child(children, Some("name"), alias_name)
        } else {
          ()
        }
      }
      Some((LIDENT(name), start, end)) => {
        if name_kind > 0 {
          let category = match name_kind {
            1 => "constant"
            2 => "type"
            _ => "trait"
          }
          state.report(
            mk_loc(start, end),
            "Unexpected lowercase identifier \{name}, expected uppercase identifier in \{category} name.",
          )
        }
        if state.consume_any() is Some(alias_name) {
          push_child(children, Some("name"), alias_name)
        } else {
          ()
        }
      }
      Some((EXTEND, _, _)) =>
        if state.consume_any() is Some(alias_name) {
          push_child(children, Some("name"), alias_name)
        } else {
          ()
        }
      _ => ()
    }
  }
  let using_kind = match name_kind {
    2 => UsingKind_Type
    3 => UsingKind_Trait
    _ => UsingKind_Value
  }
  with_marker_child(
    state
    .node_from_children(Clause_UsingItem, children)
    .with_structured_name("semantic_name", Name_AliasTarget, ["target", "name"]),
    "using_kind",
    using_kind,
  )
}

///|
fn parse_deriving_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_deriving_item,
    List_DerivingList,
    sep_kind=TokenKind::TK_COMMA,
  )
}

///|
fn parse_deriving_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match (state.peek(), state.peek(offset=1)) {
    (
      Some((PACKAGE_NAME(_), _, _)),
      Some((DOT_UIDENT(_), _, _))
      | Some((DOT_LIDENT(_), _, _)),
    ) => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    }
    (Some((UIDENT(_), _, _)) | Some((LIDENT(_), _, _)), _) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ =>
      push_child(
        children,
        Some("name"),
        state.missing_token_node(TokenKind::TK_UIDENT),
      )
  }
  if state.current_is(TokenKind::TK_LPAREN) {
    push_child(children, Some("arg"), parse_argument_list(state))
  }
  state
  .node_from_children(Clause_DerivingDirective, children)
  .with_structured_name("semantic_name", Name_Type, ["name"])
}

///|
fn top_item_children_have_label(
  children : Array[(String?, CstNode)],
  expected : String,
) -> Bool {
  for i in 0..<children.length() {
    match children.get(i) {
      Some((Some(label), _)) if label == expected => return true
      _ => ()
    }
  }
  false
}

///|
fn top_item_child_with_label_has_kind(
  children : Array[(String?, CstNode)],
  expected_label : String,
  expected_kind : NodeKind,
) -> Bool {
  for i in 0..<children.length() {
    match children.get(i) {
      Some((Some(label), child)) if label == expected_label =>
        return child.kind == expected_kind
      _ => ()
    }
  }
  false
}

///|
fn top_item_children_have_child_or_grandchild_kind(
  children : Array[(String?, CstNode)],
  expected : NodeKind,
) -> Bool {
  for entry in children {
    let (_, child) = entry
    if child.kind == expected || child.has_child_kind(expected) {
      return true
    }
  }
  false
}

///|
fn classify_top_item_node_kind(
  children : Array[(String?, CstNode)],
  parsed_kind : TopItemKind,
  is_suberror : Bool,
  is_extenum : Bool,
  saw_body : Bool,
) -> NodeKind {
  match parsed_kind {
    TopItemKind::Invalid => Error
    Import => Impl_Import
    Extend => Impl_Extend
    TopItemKind::Using => Impl_Using
    TopItemKind::Function => Impl_Function
    TopItemKind::Let => Impl_Let
    TopItemKind::Const => Impl_Const
    TopItemKind::Type =>
      if top_item_children_have_label(children, "type") {
        Impl_TypeAlias
      } else {
        Impl_TypeAbstract
      }
    TopItemKind::Struct =>
      if top_item_child_with_label_has_kind(
          children,
          "body",
          List_TupleStructTypeList,
        ) {
        Impl_TupleStruct
      } else {
        Impl_RecordStruct
      }
    TopItemKind::Enum =>
      if is_suberror {
        if saw_body {
          Impl_SuberrorEnum
        } else {
          Impl_Suberror
        }
      } else if is_extenum {
        if top_item_children_have_child_or_grandchild_kind(
            children,
            Operator_Augmented,
          ) {
          Impl_ExtendEnum
        } else {
          Impl_ExtensibleEnum
        }
      } else {
        Impl_EnumVariant
      }
    TopItemKind::EnumView => Impl_EnumView
    TopItemKind::Trait => Impl_Trait
    TopItemKind::Impl =>
      if top_item_children_have_label(children, "method") {
        Impl_ImplMethod
      } else {
        Impl_ImplRelation
      }
    TopItemKind::Test => Impl_Test
  }
}

///|
fn type_desc_kind_for_impl(kind : NodeKind) -> NodeKind? {
  match kind {
    Impl_TypeAbstract => Some(TypeDesc_Abstract)
    Impl_TypeAlias => Some(TypeDesc_Alias)
    Impl_RecordStruct => Some(TypeDesc_Record)
    Impl_TupleStruct => Some(TypeDesc_TupleStruct)
    Impl_EnumVariant => Some(TypeDesc_Variant)
    Impl_ExtensibleEnum => Some(TypeDesc_ExtensibleEnum)
    Impl_ExtendEnum => Some(TypeDesc_ExtendEnum)
    Impl_Suberror => Some(TypeDesc_ErrorNoPayload)
    Impl_SuberrorEnum => Some(TypeDesc_ErrorEnumPayload)
    _ => None
  }
}

///|
fn decl_body_kind(node : CstNode, is_declare : Bool) -> NodeKind {
  if is_declare {
    return DeclBody_None
  }
  if node.first_child_of_kind(NodeKind::is_decl_body) is Some(marker) {
    return marker.kind
  }
  let stubs = node.children_with_label("stub")
  match stubs.get(0) {
    Some(first) =>
      if first.kind is Stub_ModuleName {
        DeclBody_Import
      } else if stubs.length() > 1 {
        DeclBody_EmbeddedMultilineString
      } else if first.kind is Stub_Code {
        DeclBody_EmbeddedString
      } else {
        DeclBody_Expr
      }
    None => DeclBody_Expr
  }
}

///|
fn annotate_decl_body(source : CstNode, is_declare : Bool) -> CstNode {
  let kind = decl_body_kind(source, is_declare)
  let children : Array[(String?, CstNode)] = []
  for entry in source.children {
    let (_, child) = entry
    if !child.kind.is_decl_body() {
      children.push(entry)
    }
  }
  push_child(
    children,
    Some("decl_body_kind"),
    marker_node(kind, source.loc.end, source.source_span.1),
  )
  node(source.kind, source.loc, source.source_span, children)
}

///|
fn parse_balanced_decl_body(state : ParserState, kind : TopItemKind) -> CstNode {
  let diagnostics_before = state.reports.length()
  let children : Array[(String?, CstNode)] = []
  let mut depth = 0
  let mut paren_depth = 0
  let mut done = false
  let mut phase = DeclPhase::Start
  let mut accepts_params = false
  let mut enumview_has_constructors = false
  let mut recovered_body_without_close = false
  let mut saw_core_keyword = false
  let mut params_complete = true
  let mut saw_body = false
  let mut saw_stub = false
  let mut saw_language = false
  let mut is_declare = false
  let mut is_extern = false
  let mut is_suberror = false
  let mut is_extenum = false
  let mut suberror_had_type_params = false
  let mut invalid_grouped_impl = false
  let mut missing_return_before_impl = false
  let mut reported_missing_body = false
  let mut return_type_had_error = false
  let mut saw_async_prefix = false
  while !done {
    match state.peek() {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) =>
        push_children(children, state.consume_trivia())
      Some((tok, _, _)) if is_top_level_boundary(tok, depth) => done = true
      Some((tok, start, end)) if saw_core_keyword &&
        top_level_decl_can_start(tok) &&
        !(phase is AwaitingName && is_name_token(tok)) => {
        if state.suppress_next_top_level_recovery_report {
          state.suppress_next_top_level_recovery_report = false
        } else if invalid_grouped_impl {
          state.report(mk_loc(start, end), "Unexpected token here.")
        } else if (
            state.reports.length() == diagnostics_before ||
            missing_return_before_impl
          ) &&
          !recovered_body_without_close {
          match decl_phase_expectation(phase, kind) {
            NoExpectation =>
              state.report(mk_loc(start, end), "Missing newline here.")
            _ =>
              state.report_decl_phase_at(phase, kind, tok, mk_loc(start, end))
          }
          if phase is AwaitingBody {
            reported_missing_body = true
          }
        }
        done = true
      }
      Some((tok, start, end)) if phase is AwaitingName && is_name_token(tok) => {
        if kind is TopItemKind::Function &&
          tok is LIDENT(name) &&
          state.peek_nth_significant(1).map(item => item.0) is Some(COLONCOLON) {
          state.report(
            mk_loc(start, end),
            "Unexpected lowercase identifier \{name}, expected uppercase identifier in type name.",
          )
        }
        if !decl_name_token_is_valid(tok, kind) {
          match (kind, tok) {
            (TopItemKind::Struct, LIDENT(name)) =>
              state.report(
                mk_loc(start, end),
                "Unexpected lowercase identifier \{name}, expected uppercase identifier in struct name.",
              )
            _ =>
              state.report(mk_loc(start, end), "expected uppercase type name")
          }
        }
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
        phase = decl_phase_after_name(kind, is_suberror)
        accepts_params = true
      }
      Some((LBRACKET, _, _)) if phase is AwaitingName &&
        kind is TopItemKind::Function =>
        push_child(
          children,
          Some("quantifier"),
          parse_fun_type_parameter_list(state),
        )
      Some((LBRACKET, _, _)) if phase is AwaitingName &&
        (kind is TopItemKind::Impl || kind is TopItemKind::EnumView) =>
        push_child(
          children,
          Some("quantifier"),
          parse_fun_type_parameter_list(state),
        )
      Some((AMPER, _, _)) if phase is AwaitingName &&
        kind is TopItemKind::Function =>
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
          push_child(
            children,
            Some("object"),
            bool_marker_node(Flag_Object, op.loc.start, op.source_span.0, true),
          )
          push_child(
            children,
            Some("object_loc"),
            token_location_node(op.kind, op.loc, op.source_span),
          )
        } else {
          ()
        }
      Some((LPAREN, _, _)) if kind is TopItemKind::Struct => {
        push_child(children, Some("body"), parse_tuple_struct_type_list(state))
        saw_body = true
        accepts_params = false
        phase = Complete
      }
      Some((LPAREN, _, _)) if accepts_params && kind is TopItemKind::Function => {
        let params = parse_parameter_list(state)
        params_complete = !child_label_is_missing(params, "close")
        push_child(children, Some("params"), params)
        accepts_params = false
        phase = if is_extern {
          AwaitingEqual
        } else if is_declare {
          Complete
        } else {
          AwaitingBody
        }
      }
      Some((DOT_UIDENT(_), _, _)) | Some((DOT_LIDENT(_), _, _)) if accepts_params &&
        (
          kind is TopItemKind::Type ||
          kind is TopItemKind::Struct ||
          kind is TopItemKind::Enum ||
          kind is TopItemKind::EnumView ||
          kind is TopItemKind::Impl
        ) => {
        match state.peek() {
          Some((DOT_LIDENT(_), start, end)) =>
            state.report(mk_loc(start, end), "expected uppercase type name")
          _ => ()
        }
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
      }
      Some((DOT_UIDENT(_), _, _)) | Some((DOT_LIDENT(_), _, _)) if accepts_params &&
        kind is TopItemKind::Function =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
      Some((COLONCOLON, _, _)) if accepts_params &&
        kind is TopItemKind::Function => {
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
        } else {
          ()
        }
        match state.peek() {
          Some((LIDENT(_), _, _))
          | Some((UIDENT(_), _, _))
          | Some((EXTEND, _, _)) =>
            if state.consume_any() is Some(method_name) {
              push_child(children, Some("method"), method_name)
            } else {
              ()
            }
          _ =>
            push_child(
              children,
              Some("method"),
              state.missing_token_node(TokenKind::TK_LIDENT),
            )
        }
      }
      Some((LBRACKET, _, _)) if accepts_params && decl_accepts_type_params(kind) => {
        if is_suberror {
          suberror_had_type_params = true
        }
        push_child(
          children,
          Some("params"),
          parse_type_decl_parameter_list(state),
        )
        accepts_params = false
      }
      Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) if phase
        is AwaitingLetPattern => {
        let pat = parse_pattern_until(state, [
          TokenKind::TK_COLON,
          TokenKind::TK_EQUAL,
          TokenKind::TK_SEMI,
          TokenKind::TK_EOF,
        ])
        if !pat.children.is_empty() {
          push_child(children, Some("pattern"), pat)
        } else {
          push_child(
            children,
            Some("pattern"),
            state.missing_token_node_with_expected(TokenKind::TK_LIDENT, [
              TokenKind::TK_LIDENT,
              TokenKind::TK_EXTEND,
            ]),
          )
        }
        phase = AwaitingEqual
      }
      Some((UIDENT(name), start, end)) if phase is AwaitingLetPattern => {
        state.report(
          mk_loc(start, end),
          "Unexpected uppercase identifier \{name}, expected lowercase identifier in name of `let`.",
        )
        let pat = parse_single_token_pattern(state, Pattern_Var)
        push_child(children, Some("pattern"), pat)
        phase = AwaitingEqual
        match state.peek() {
          Some((found, next_start, next_end)) if !(found is EQUAL) => {
            state.report_expected_tokens_at(
              found,
              mk_loc(next_start, next_end),
              [TokenKind::TK_EQUAL],
            )
            let mut recovering = true
            while recovering {
              match state.peek() {
                Some((tok, _, _)) if top_level_decl_can_start(tok) =>
                  recovering = false
                Some((EOF, _, _)) | None => recovering = false
                Some(_) =>
                  if state.consume_any() is Some(bad) {
                    push_child(
                      children,
                      Some("error"),
                      error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
                    )
                  } else {
                    recovering = false
                  }
              }
            }
            phase = Complete
            done = true
          }
          _ => ()
        }
      }
      Some((found, start, end)) if phase is AwaitingLetPattern => {
        state.report_expected_tokens_at(found, mk_loc(start, end), [
          TokenKind::TK_LIDENT,
          TokenKind::TK_EXTEND,
        ])
        push_child(
          children,
          Some("pattern"),
          state.missing_token_node_silent(TokenKind::TK_LIDENT),
        )
        let mut recovering = true
        while recovering {
          match state.peek() {
            Some((tok, _, _)) if top_level_decl_can_start(tok) =>
              recovering = false
            Some((EOF, _, _)) | None => recovering = false
            Some(_) =>
              if state.consume_any() is Some(bad) {
                push_child(
                  children,
                  Some("error"),
                  error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
                )
              } else {
                recovering = false
              }
          }
        }
        phase = Complete
        done = true
      }
      Some((LET, _, _)) => {
        saw_core_keyword = true
        if state.consume_any() is Some(let_token) {
          push_child(children, decl_token_label(LET), let_token)
        } else {
          ()
        }
        phase = AwaitingLetPattern
      }
      Some((TEST, _, _)) if kind is TopItemKind::Test => {
        saw_core_keyword = true
        if state.consume_any() is Some(test_token) {
          push_child(children, decl_token_label(TEST), test_token)
        } else {
          ()
        }
        match state.peek_significant() {
          Some((STRING(value), _, _)) =>
            if state.consume_any() is Some(name) {
              push_child(children, Some("name"), name)
              push_child(
                children,
                Some("test_name"),
                scalar_node(
                  Name_Test,
                  name.loc,
                  name.source_span,
                  String(value),
                ),
              )
            } else {
              ()
            }
          _ => ()
        }
        if state.current_is(TokenKind::TK_LPAREN) {
          push_child(children, Some("params"), parse_parameter_list(state))
        }
        phase = AwaitingBody
      }
      Some((PUB, _, _)) | Some((PRIV, _, _)) =>
        push_child(children, Some("vis"), parse_visibility_clause(state))
      Some((EXTERN, _, _)) => {
        is_extern = true
        phase = AwaitingExternLanguage
        if state.consume_any() is Some(consumed) {
          push_child(children, decl_token_label(EXTERN), consumed)
        } else {
          ()
        }
        push_children(children, state.consume_trivia())
        match state.peek() {
          Some((STRING(value), _, _)) =>
            if state.consume_any() is Some(language) {
              saw_language = true
              phase = AwaitingExternFunction
              push_child(children, Some("language_token"), language)
              push_child(
                children,
                Some("language"),
                scalar_node(
                  Language_Value,
                  language.loc,
                  language.source_span,
                  String(value),
                ),
              )
            } else {
              ()
            }
          _ => state.report_expected_tokens([TokenKind::TK_STRING])
        }
      }
      Some((tok, _, _)) if decl_prefix_token(tok) => {
        if tok is DECLARE {
          is_declare = true
        }
        if tok is ASYNC {
          saw_async_prefix = true
        }
        if state.consume_any() is Some(consumed) {
          push_child(children, decl_token_label(tok), consumed)
        } else {
          ()
        }
      }
      Some((tok, _, _)) if decl_keyword_expects_name(tok) => {
        if is_extern && saw_language && !(tok is FN) {
          match state.peek() {
            Some((found, start, end)) =>
              state.report_expected_tokens_at(found, mk_loc(start, end), [
                TokenKind::TK_FN,
              ])
            None => ()
          }
          while !state.current_is(TokenKind::TK_EOF) && !(state.peek() is None) {
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            } else {
              ()
            }
          }
          done = true
          continue
        }
        saw_core_keyword = true
        if tok is SUBERROR {
          is_suberror = true
        }
        if tok is EXTENUM {
          is_extenum = true
        }
        if state.consume_any() is Some(keyword) {
          push_child(children, decl_token_label(tok), keyword)
        } else {
          ()
        }
        phase = AwaitingName
      }
      Some((USING, _, _)) if kind is TopItemKind::Using => {
        if is_declare {
          match state.peek() {
            Some((found, start, end)) =>
              state.report_expected_tokens_at(
                found,
                mk_loc(start, end),
                top_level_decl_expected_tokens(),
              )
            None => ()
          }
          while !state.current_is(TokenKind::TK_EOF) && !(state.peek() is None) {
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            } else {
              ()
            }
          }
          done = true
          continue
        }
        saw_core_keyword = true
        phase = AwaitingUsingName
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        } else {
          ()
        }
        match state.peek() {
          Some((PACKAGE_NAME(_), _, _))
          | Some((LIDENT(_), _, _))
          | Some((UIDENT(_), _, _)) =>
            if state.consume_any() is Some(name) {
              phase = AwaitingBody
              push_child(children, Some("name"), name)
            } else {
              ()
            }
          _ => ()
        }
      }
      Some((tok, _, _)) if paren_depth > 0 &&
        pattern_can_start(tok) &&
        state.pattern_has_top_level_annotation() => {
        let pat = parse_pattern_until(state, [
          TokenKind::TK_COLON,
          TokenKind::TK_COMMA,
          TokenKind::TK_RPAREN,
          TokenKind::TK_EOF,
        ])
        if !pat.children.is_empty() {
          push_child(children, Some("pattern"), pat)
        }
      }
      Some((COLON, _, _)) if kind is TopItemKind::Trait && depth == 0 => {
        if state.consume_any() is Some(colon) {
          push_child(children, Some("op"), colon)
        } else {
          ()
        }
        let stops = [
          TokenKind::TK_PLUS,
          TokenKind::TK_LBRACE,
          TokenKind::TK_SEMI,
          TokenKind::TK_EOF,
        ]
        let reports_before_type = state.reports.length()
        let ty = parse_type_arrow(state, stops)
        let ty = if !ty.children.is_empty() {
          ty
        } else {
          if state.reports.length() == reports_before_type {
            state.report_expected_tokens([
              TokenKind::TK_UIDENT,
              TokenKind::TK_PACKAGE_NAME,
            ])
          }
          state.missing_type_node()
        }
        push_child(children, Some("type"), ty)
        while state.current_is(TokenKind::TK_PLUS) {
          if state.consume_any() is Some(op) {
            push_child(children, Some("op"), op)
          } else {
            ()
          }
          let ty = parse_type_arrow(state, stops)
          let ty = if !ty.children.is_empty() {
            ty
          } else {
            state.report_expected_tokens([
              TokenKind::TK_UIDENT,
              TokenKind::TK_PACKAGE_NAME,
            ])
            state.missing_type_node()
          }
          push_child(children, Some("type"), ty)
        }
      }
      Some((COLON, start, end)) if kind is TopItemKind::Function && depth == 0 => {
        state.report_expected_tokens_at(COLON, mk_loc(start, end), [
          TokenKind::TK_LBRACE,
        ])
        while !state.current_is(TokenKind::TK_EOF) && !(state.peek() is None) {
          if state.consume_any() is Some(bad) {
            push_child(children, Some("error"), bad)
          } else {
            ()
          }
        }
        saw_body = true
        phase = Complete
        done = true
      }
      Some((COLON, _, _)) => {
        if state.consume_any() is Some(colon) {
          push_child(children, Some("op"), colon)
        } else {
          ()
        }
        let reports_before_type = state.reports.length()
        let ty = parse_type_arrow(state, [
          TokenKind::TK_COMMA,
          TokenKind::TK_RPAREN,
          TokenKind::TK_RBRACKET,
          TokenKind::TK_RBRACE,
          TokenKind::TK_EQUAL,
          TokenKind::TK_SEMI,
          TokenKind::TK_EOF,
        ])
        push_child(
          children,
          Some("type"),
          state.required_type_node(ty, reports_before_type),
        )
      }
      Some((THIN_ARROW, _, _)) if depth == 0 => {
        phase = AwaitingType
        missing_return_before_impl = match state.peek_nth_significant(1) {
          Some((tok, _, _)) => token_starts_impl_for_missing_return_type(tok)
          None => false
        }
        let reports_before_return_type = state.reports.length()
        push_child(children, Some("clause"), parse_func_return_type(state))
        return_type_had_error = state.reports.length() >
          reports_before_return_type
        phase = if is_declare { Complete } else { AwaitingBody }
      }
      Some((RAISE, _, _)) | Some((NORAISE, _, _)) | Some((NOCANCEL, _, _)) if depth ==
        0 => {
        push_child(children, Some("clause"), parse_func_return_type(state))
        phase = if is_declare { Complete } else { AwaitingBody }
      }
      Some((EQUAL, _, _)) if kind is TopItemKind::Type => {
        phase = AwaitingType
        if state.consume_any() is Some(equal) {
          push_child(children, Some("op"), equal)
        } else {
          ()
        }
        let reports_before_type = state.reports.length()
        let ty = parse_type_arrow(state, [TokenKind::TK_SEMI, TokenKind::TK_EOF])
        let ty = state.required_type_node(ty, reports_before_type)
        push_child(children, Some("type"), ty)
        if !(ty.kind is Type_MissingType) {
          phase = Complete
        }
      }
      Some((EQUAL, _, _)) if kind is TopItemKind::Let ||
        kind is TopItemKind::Const => {
        phase = AwaitingValue
        if state.consume_any() is Some(equal) {
          push_child(children, Some("op"), equal)
        } else {
          ()
        }
        let value_started_with_loop = state
          .peek_significant()
          .map(item => item.0)
          is Some(LOOP)
        let expr = parse_expr_until(state, [
          TokenKind::TK_SEMI,
          TokenKind::TK_EOF,
        ])
        if !expr.children.is_empty() {
          push_child(children, Some("expr"), expr)
          phase = Complete
        } else {
          state.report_expected_description("simple expression")
          push_child(children, Some("expr"), state.missing_expr_node())
        }
        if value_started_with_loop &&
          state.current_is(TokenKind::TK_SEMI) &&
          state.peek_nth_significant(1).map(item => item.0) is Some(LPAREN) {
          state.suppress_next_top_level_recovery_report = true
        }
      }
      Some((EQUAL, _, _)) if kind is TopItemKind::Function => {
        phase = AwaitingStub
        if state.consume_any() is Some(equal) {
          push_child(children, Some("op"), equal)
        } else {
          ()
        }
        push_children(children, state.consume_trivia())
        match state.peek() {
          Some((STRING(_), _, _)) | Some((MULTILINE_STRING(_), _, _)) => {
            saw_stub = true
            phase = Complete
            parse_function_stub_tokens(state, children)
          }
          _ => state.report_expected_tokens([TokenKind::TK_STRING])
        }
        done = true
      }
      Some((FOR, _, _)) if kind is TopItemKind::Impl => {
        phase = AwaitingType
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        } else {
          ()
        }
        let reports_before_type = state.reports.length()
        let ty = parse_type_arrow(state, [
          TokenKind::TK_WITH,
          TokenKind::TK_LBRACE,
          TokenKind::TK_SEMI,
          TokenKind::TK_EOF,
        ])
        let ty = state.required_type_node(ty, reports_before_type)
        push_child(children, Some("type"), ty)
        if !(ty.kind is Type_MissingType) {
          phase = Complete
        }
      }
      Some((FOR, _, _)) if kind is TopItemKind::EnumView => {
        phase = AwaitingType
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        } else {
          ()
        }
        let reports_before_type = state.reports.length()
        let ty = parse_type_arrow(state, [
          TokenKind::TK_WITH,
          TokenKind::TK_SEMI,
          TokenKind::TK_EOF,
        ])
        let ty = state.required_type_node(ty, reports_before_type)
        push_child(children, Some("type"), ty)
        if !(ty.kind is Type_MissingType) {
          phase = AwaitingEnumViewWith
        }
      }
      Some((WITH, _, _)) if kind is TopItemKind::Impl => {
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        } else {
          ()
        }
        push_child(children, Some("method"), parse_impl_method_item(state))
        phase = Complete
        if state.abort_current_decl {
          state.abort_current_decl = false
          done = true
        }
      }
      Some((WITH, _, _)) if kind is TopItemKind::EnumView => {
        phase = AwaitingMethodName
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        } else {
          ()
        }
        match state.peek() {
          Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
            if state.consume_any() is Some(name) {
              push_child(children, Some("method"), name)
              phase = AwaitingParameters
            } else {
              ()
            }
          _ => {
            state.report_expected_tokens([
              TokenKind::TK_LIDENT,
              TokenKind::TK_EXTEND,
            ])
            push_child(
              children,
              Some("method"),
              state.missing_token_node_silent(TokenKind::TK_LIDENT),
            )
          }
        }
        if state.current_is(TokenKind::TK_LPAREN) {
          push_child(children, Some("params"), parse_parameter_list(state))
          phase = AwaitingBody
        }
      }
      Some((PLUS_EQUAL, _, _)) if is_extenum =>
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
        } else {
          ()
        }
      Some((DERIVE, _, _)) if depth == 0 => {
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        } else {
          ()
        }
        if state.current_is(TokenKind::TK_LPAREN) {
          push_child(children, Some("deriving"), parse_deriving_list(state))
        } else {
          push_child(
            children,
            Some("deriving"),
            state.missing_token_node(TokenKind::TK_LPAREN),
          )
        }
      }
      Some((found, start, end)) if kind is TopItemKind::Function &&
        phase is AwaitingBody &&
        found is (EXCLAMATION | QUESTION) => {
        state.report_expected_tokens_at(found, mk_loc(start, end), [
          TokenKind::TK_LBRACE,
        ])
        while !state.current_is(TokenKind::TK_LBRACE) &&
              !state.current_is(TokenKind::TK_SEMI) &&
              !state.current_is(TokenKind::TK_EOF) &&
              !(state.peek() is None) {
          if state.consume_any() is Some(bad) {
            push_child(
              children,
              Some("error"),
              error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
            )
          } else {
            ()
          }
        }
      }
      Some((WHERE, _, _)) if kind is TopItemKind::Function =>
        push_child(children, Some("where"), parse_where_clause(state))
      Some((EXCLAMATION, _, _)) if kind is TopItemKind::Function &&
        !(phase is AwaitingName) =>
        if state.consume_error_for_progress() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      Some((LBRACKET, _, _)) if accepts_params &&
        kind is TopItemKind::Function &&
        !saw_body =>
        match state.peek() {
          Some((found, start, end)) => {
            let expected = if is_extern {
              [TokenKind::TK_EQUAL]
            } else {
              [TokenKind::TK_LBRACE]
            }
            state.report_expected_tokens_at(found, mk_loc(start, end), expected)
            let recovery_kind = if is_extern {
              TokenKind::TK_EQUAL
            } else {
              TokenKind::TK_LBRACE
            }
            while !state.current_is(recovery_kind) &&
                  !state.current_is(TokenKind::TK_EOF) &&
                  !(state.peek() is None) {
              if state.consume_any() is Some(bad) {
                push_child(
                  children,
                  Some("error"),
                  error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
                )
              } else {
                ()
              }
            }
          }
          None => done = true
        }
      Some((QUESTION, _, _)) if depth == 0 &&
        (!saw_core_keyword || kind is TopItemKind::Invalid) =>
        if state.consume_error_for_progress() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      Some((RBRACE, start, end)) if kind is TopItemKind::Function && !saw_body => {
        state.report_expected_tokens_at(RBRACE, mk_loc(start, end), [
          TokenKind::TK_LBRACE,
        ])
        if state.consume_any() is Some(bad) {
          push_child(children, Some("error"), bad)
        } else {
          ()
        }
        saw_body = true
        phase = Complete
        done = true
      }
      Some((found, start, end)) if kind is TopItemKind::Invalid ||
        (!saw_core_keyword && phase is Start) => {
        if state.suppress_next_top_level_recovery_report {
          state.suppress_next_top_level_recovery_report = false
        } else if saw_async_prefix {
          state.report_expected_tokens_at(found, mk_loc(start, end), [
            TokenKind::TK_FN,
          ])
        } else {
          state.report_expected_tokens_at(
            found,
            mk_loc(start, end),
            top_level_decl_expected_tokens(),
          )
        }
        let mut recovering = true
        while recovering {
          match state.peek() {
            Some((tok, _, _)) if top_level_decl_can_start(tok) =>
              recovering = false
            Some((EOF, _, _)) | None => recovering = false
            Some(_) =>
              if state.consume_any() is Some(bad) {
                push_child(
                  children,
                  Some("error"),
                  error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
                )
              } else {
                recovering = false
              }
          }
        }
        done = true
      }
      Some((LBRACE, start, end)) if kind is TopItemKind::Function &&
        phase is AwaitingName => {
        let mut recovery_end = end
        while !state.current_is(TokenKind::TK_EOF) && !(state.peek() is None) {
          match state.peek() {
            Some((next, _, _)) if top_level_decl_can_start(next) => break
            Some((SEMI(_), _, _)) => break
            _ => ()
          }
          if state.consume_any() is Some(bad) {
            recovery_end = bad.loc.end
            push_child(children, Some("error"), bad)
          } else {
            ()
          }
        }
        state.report_expected_tokens_at(
          LBRACE,
          mk_loc(start, recovery_end),
          decl_name_expectation(kind),
        )
        phase = Complete
        done = true
      }
      Some((LBRACE, _, _)) =>
        if kind is TopItemKind::Using {
          saw_body = true
          push_child(children, Some("body"), parse_using_name_list(state))
          phase = Complete
        } else if kind is TopItemKind::Impl {
          if phase is AwaitingImplRelation {
            match state.peek() {
              Some((found, start, end)) =>
                state.report_expected_tokens_at(found, mk_loc(start, end), [
                  TokenKind::TK_FOR,
                  TokenKind::TK_WITH,
                ])
              None => ()
            }
          } else {
            invalid_grouped_impl = true
            match state.peek_nth_significant(1) {
              Some((_, start, end)) =>
                state.report(mk_loc(start, end), "Unexpected token here.")
              None => ()
            }
          }
          saw_body = true
          push_child(
            children,
            Some("body"),
            parse_balanced_brace_group(state, decl_body_list_kind(kind)),
          )
          if invalid_grouped_impl {
            let mut offset = 0
            let mut searching = true
            while searching {
              match state.peek(offset~) {
                Some((COMMENT(_), _, _))
                | Some((NEWLINE, _, _))
                | Some((SEMI(_), _, _)) => offset += 1
                Some((next, start, end)) if top_level_decl_can_start(next) => {
                  state.report(mk_loc(start, end), "Unexpected token here.")
                  searching = false
                }
                _ => searching = false
              }
            }
            invalid_grouped_impl = false
          }
          phase = Complete
        } else if kind is TopItemKind::Struct {
          saw_body = true
          let body = parse_struct_member_list(state)
          recovered_body_without_close = child_label_is_missing(body, "close")
          push_child(children, Some("body"), body)
          phase = Complete
        } else if kind is TopItemKind::Enum {
          saw_body = true
          push_child(children, Some("body"), parse_enum_constructor_list(state))
          phase = Complete
        } else if kind is TopItemKind::EnumView && !enumview_has_constructors {
          saw_body = true
          push_child(children, Some("body"), parse_enum_constructor_list(state))
          enumview_has_constructors = true
          phase = AwaitingEnumViewFor
        } else if kind is TopItemKind::EnumView {
          saw_body = true
          push_child(children, Some("expr"), parse_block_expr(state))
          phase = Complete
        } else if kind is TopItemKind::Trait {
          saw_body = true
          push_child(children, Some("body"), parse_trait_member_list(state))
          phase = Complete
        } else if decl_body_uses_balanced_group(kind) {
          saw_body = true
          push_child(
            children,
            Some("body"),
            parse_balanced_brace_group(state, decl_body_list_kind(kind)),
          )
          phase = Complete
        } else {
          saw_body = true
          let body = parse_block_expr(state)
          push_child(children, Some("body"), body)
          phase = Complete
          if state.abort_current_decl {
            state.abort_current_decl = false
            if state.current_is(TokenKind::TK_SEMI) ||
              state.current_is(TokenKind::TK_EOF) {
              done = true
            }
          }
        }
      Some((found, start, end)) if phase is AwaitingName => {
        let expected = decl_name_expectation(kind)
        if kind is TopItemKind::Function {
          let mut recovery_end = end
          while !state.current_is(TokenKind::TK_EOF) && !(state.peek() is None) {
            match state.peek() {
              Some((next, _, _)) if top_level_decl_can_start(next) => break
              Some((SEMI(_), _, _)) => break
              _ => ()
            }
            if state.consume_any() is Some(bad) {
              recovery_end = bad.loc.end
              push_child(children, Some("error"), bad)
            } else {
              ()
            }
          }
          state.report_expected_tokens_at(
            found,
            mk_loc(start, recovery_end),
            expected,
          )
          phase = Complete
          done = true
        } else if kind is TopItemKind::Type {
          state.report_expected_tokens_at(found, mk_loc(start, end), expected)
          let mut recovering = true
          while recovering {
            match state.peek() {
              Some(
                (
                  FN
                  | LET
                  | CONST
                  | TYPE
                  | STRUCT
                  | ENUM
                  | SUBERROR
                  | EXTENUM
                  | ENUMVIEW
                  | TRAIT
                  | IMPL
                  | TEST
                  | USING
                  | EXTERN,
                  _,
                  _,
                )
              ) => recovering = false
              Some((EOF, _, _)) | None => recovering = false
              Some(_) =>
                if state.consume_any() is Some(bad) {
                  push_child(
                    children,
                    Some("error"),
                    error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
                  )
                } else {
                  recovering = false
                }
            }
          }
          match state.peek() {
            Some((next, next_start, next_end)) if !(next is EOF) =>
              state.report_expected_tokens_at(
                next,
                mk_loc(next_start, next_end),
                [
                  TokenKind::TK_SEMI,
                  TokenKind::TK_EOF,
                  TokenKind::TK_DERIVE,
                  TokenKind::TK_EQUAL,
                ],
              )
            _ => ()
          }
          phase = Complete
          done = true
        } else {
          state.report_expected_tokens_at(found, mk_loc(start, end), expected)
          if state.consume_any() is Some(bad) {
            push_child(children, Some("error"), bad)
          } else {
            done = true
          }
        }
      }
      Some((_, _, _)) if phase is Complete => {
        let completed_type_bad_token = match state.peek() {
          Some((found, start, end)) if kind is TopItemKind::Type =>
            Some((found, mk_loc(start, end)))
          _ => None
        }
        let mut recovering = true
        while recovering {
          match state.peek() {
            Some((next, _, _)) if top_level_decl_can_start(next) =>
              recovering = false
            Some((EOF, _, _)) | None => recovering = false
            Some(_) =>
              if state.consume_any() is Some(bad) {
                push_child(
                  children,
                  Some("error"),
                  error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
                )
              } else {
                recovering = false
              }
          }
        }
        if state.suppress_next_top_level_recovery_report {
          state.suppress_next_top_level_recovery_report = false
        } else {
          match completed_type_bad_token {
            Some((found, loc)) =>
              state.report_expected_tokens_at(found, loc, [
                TokenKind::TK_SEMI,
                TokenKind::TK_EOF,
                TokenKind::TK_DERIVE,
                TokenKind::TK_EQUAL,
              ])
            None =>
              match state.peek() {
                Some((found, start, end)) if kind is TopItemKind::Type =>
                  state.report_expected_tokens_at(found, mk_loc(start, end), [
                    TokenKind::TK_SEMI,
                    TokenKind::TK_EOF,
                    TokenKind::TK_DERIVE,
                    TokenKind::TK_EQUAL,
                  ])
                Some((_, start, end)) =>
                  state.report(mk_loc(start, end), "Unexpected token here.")
                None =>
                  state.report(
                    empty_loc(state.previous_end()),
                    "Unexpected token here.",
                  )
              }
          }
        }
        done = true
      }
      Some((tok, _, _)) => {
        if state.consume_error_for_progress() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
        depth = next_depth(depth, tok)
        paren_depth = next_paren_depth(paren_depth, tok)
      }
      None => done = true
    }
  }
  let should_report_completion = !reported_missing_body &&
    (
      state.reports.length() == diagnostics_before ||
      (
        kind is TopItemKind::Function &&
        !is_extern &&
        phase is AwaitingBody &&
        params_complete &&
        (
          !return_type_had_error ||
          missing_return_before_impl ||
          state.current_is(TokenKind::TK_EOF)
        )
      )
    )
  if should_report_completion {
    if kind is TopItemKind::Function && is_declare && (saw_body || saw_stub) {
      state.report(
        empty_loc(state.current_position()),
        "declare function cannot have a body",
      )
    } else if kind is TopItemKind::Enum && suberror_had_type_params {
      state.report(
        empty_loc(state.current_position()),
        "Unexpected token here.",
      )
    } else {
      state.report_decl_phase(phase, kind)
    }
  }
  let final_kind = classify_top_item_node_kind(
    children, kind, is_suberror, is_extenum, saw_body,
  )
  let mut result = state.node_from_children(final_kind, children)
  result = match final_kind {
    Impl_Function =>
      if result.first_child_with_label("method") is Some(_) {
        result
        .with_structured_name("semantic_type_name", Name_Type, ["name"])
        .with_structured_name("semantic_name", Name_Binder, ["method"])
      } else {
        result.with_structured_name("semantic_name", Name_Binder, ["name"])
      }
    Impl_TypeAbstract
    | Impl_TypeAlias
    | Impl_RecordStruct
    | Impl_TupleStruct
    | Impl_EnumVariant
    | Impl_ExtensibleEnum
    | Impl_ExtendEnum
    | Impl_Suberror
    | Impl_SuberrorEnum
    | Impl_EnumView =>
      result.with_structured_name("semantic_name", Name_Type, ["name"])
    Impl_Trait =>
      result.with_structured_name("semantic_name", Name_Binder, ["name"])
    Impl_ImplMethod | Impl_ImplRelation =>
      result.with_structured_name("semantic_name", Name_Type, ["name"])
    Impl_Using =>
      result.with_structured_name("semantic_name", Name_Label, ["name"])
    _ => result
  }
  if type_desc_kind_for_impl(final_kind) is Some(type_desc_kind) {
    result = with_marker_child(result, "type_desc", type_desc_kind)
  }
  if result.first_child_with_label("vis") is None {
    result = with_marker_child(result, "vis", Visibility_Default)
  }
  if result.bool_marker_value(Flag_Declare) is None {
    result = result.with_bool_marker("declare", Flag_Declare, is_declare)
  }
  if result.bool_marker_value(Flag_Async) is None {
    result = result.with_bool_marker("async", Flag_Async, saw_async_prefix)
  }
  match final_kind {
    Impl_Function => result = annotate_decl_body(result, is_declare)
    Impl_ImplMethod =>
      if result.first_child_with_label("method") is Some(method_node) {
        let annotated_method = annotate_decl_body(method_node, false)
        let replaced : Array[(String?, CstNode)] = []
        for entry in result.children {
          match entry {
            (Some("method"), _) =>
              push_child(replaced, Some("method"), annotated_method)
            _ => replaced.push(entry)
          }
        }
        result = node(result.kind, result.loc, result.source_span, replaced)
      }
    _ => ()
  }
  result
}

///|
fn statement_only_expression_token(tok : Token) -> Bool {
  match tok {
    RAISE
    | BREAK
    | CONTINUE
    | RETURN
    | LET
    | LETREC
    | GUARD
    | GUARD_EXCLAMATION
    | DEFER
    | ERRDEFER
    | PROOF_ASSERT
    | PROOF_LET
    | ELLIPSIS => true
    _ => false
  }
}

///|
fn report_statement_only_expression(
  state : ParserState,
  context : String,
) -> Unit {
  match state.peek_significant() {
    Some((tok, start, end)) if statement_only_expression_token(tok) =>
      state.report(
        mk_loc(start, end),
        "statement expression is not allowed in \{context}",
      )
    _ => ()
  }
}

///|
fn unexpected_expr_or_statement_name(tok : Token) -> String? {
  match tok {
    IF => Some("if expression")
    MATCH => Some("match expression")
    WHILE => Some("while expression")
    FOR => Some("for expression")
    TRY => Some("try expression")
    TRY_EXCLAMATION => Some("`try!` expression")
    LET | LETREC => Some("let statement")
    GUARD | GUARD_EXCLAMATION => Some("guard statement")
    DEFER | ERRDEFER => Some("defer statement")
    BREAK => Some("break statement")
    CONTINUE => Some("break statement")
    RETURN => Some("return statement")
    _ => None
  }
}

///|
fn token_disallowed_in_condition(tok : Token) -> Bool {
  match tok {
    IF
    | MATCH
    | WHILE
    | FOR
    | TRY
    | TRY_EXCLAMATION
    | LET
    | LETREC
    | GUARD
    | GUARD_EXCLAMATION
    | BREAK
    | RETURN
    | CONTINUE => true
    _ => false
  }
}

///|
fn token_disallowed_in_try_body(tok : Token) -> Bool {
  match tok {
    IF
    | MATCH
    | WHILE
    | FOR
    | TRY
    | LET
    | GUARD
    | GUARD_EXCLAMATION
    | BREAK
    | RETURN
    | CONTINUE => true
    _ => false
  }
}

///|
fn token_disallowed_in_pattern_guard(tok : Token) -> Bool {
  match tok {
    IF
    | MATCH
    | WHILE
    | FOR
    | TRY
    | LET
    | GUARD
    | GUARD_EXCLAMATION
    | BREAK
    | RETURN
    | CONTINUE => true
    _ => false
  }
}

///|
fn token_disallowed_in_match_action(tok : Token) -> Bool {
  match tok {
    LET | LETREC | GUARD | GUARD_EXCLAMATION | DEFER | ERRDEFER => true
    _ => false
  }
}

///|
fn assignment_lhs_is_valid(node : CstNode) -> Bool {
  match node.kind {
    Expr_Ident | Expr_Field | Expr_ArrayGet => true
    _ => false
  }
}

///|
fn assignment_lhs_kind(node : CstNode) -> NodeKind? {
  match node.kind {
    Expr_Ident | Expr_Field | Expr_ArrayGet => Some(node.kind)
    _ => None
  }
}

///|
fn assignment_node_kind(lhs : CstNode, is_augmented : Bool) -> NodeKind {
  match assignment_lhs_kind(lhs) {
    Some(Expr_ArrayGet) =>
      if is_augmented {
        Expr_ArrayAugmentedSet
      } else {
        Expr_ArraySet
      }
    Some(Expr_Field) => Expr_Mutate
    _ => Expr_Assign
  }
}

///|
fn push_assignment_lhs_children(
  children : Array[(String?, CstNode)],
  lhs : CstNode,
  normalize : Bool,
) -> Unit {
  if normalize {
    for entry in lhs.children {
      children.push(entry)
    }
  } else {
    push_child(children, Some("lhs"), lhs)
  }
}

///|
fn function_node_from_children(
  state : ParserState,
  children : Array[(String?, CstNode)],
  fn_kind : NodeKind,
) -> CstNode {
  let result = with_marker_child(
    state
    .node_from_children(Expr_Function, children)
    .with_bool_marker(
      "async",
      Flag_Async,
      children
      .iter()
      .any(fn(entry) {
        entry.0 is Some("kw") && entry.1.has_child_kind(Flag_Async)
      }),
    ),
    "fn_kind",
    fn_kind,
  ).with_async_location_anchor()
  if result.first_child_with_label("pipe") is Some(_) &&
    result.first_child_with_label("params") is Some(params) &&
    params.children_with_label("item").length() == 1 &&
    params.children_with_label("item").get(0) is Some(item) &&
    params.loc.start == item.loc.start &&
    params.loc.end == item.loc.end &&
    result.first_child_with_label("op") is Some(op) {
    result.with_token_location("params_loc", op.kind, op.loc, op.source_span)
  } else {
    result
  }
}

///|
fn hole_node_from_children(
  state : ParserState,
  children : Array[(String?, CstNode)],
  todo? : Bool = false,
) -> CstNode {
  let kind = if todo { HoleKind_Todo } else { HoleKind_Incomplete }
  with_marker_child(
    state.node_from_children(Expr_Hole, children),
    "hole_kind",
    kind,
  )
}

///|
fn invalid_arrow_parameter_expr(node : CstNode) -> CstNode? {
  match node.kind {
    Expr_Ident | Expr_Hole | Expr_MissingExpr => None
    kind if kind.is_argument() => None
    Expr_Constraint =>
      if node.first_child_with_label("expr") is Some(inner) {
        invalid_arrow_parameter_expr(inner)
      } else {
        None
      }
    _ => Some(node)
  }
}

///|
fn arrow_lhs_can_start_function(node : CstNode) -> Bool {
  match node.kind {
    Expr_Unit | Expr_Group | Expr_Tuple => true
    _ => invalid_arrow_parameter_expr(node) is None
  }
}

///|
fn parse_arrow_fn_body_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  match state.peek_significant() {
    Some((LBRACE, _, _)) =>
      if brace_starts_record_expr(state) {
        parse_record_expr(state)
      } else if brace_starts_map_expr(state) {
        parse_map_expr(state)
      } else {
        parse_block_expr(state)
      }
    Some((CONTINUE, _, _)) if state.peek_nth_significant(1).map(item => item.0)
      is Some(POST_LABEL(_)) &&
      state.peek_nth_significant(2).map(item => item.0) is Some(COLON) =>
      parse_expr_until(state, stop_kinds)
    Some((BREAK | CONTINUE | RETURN, _, _)) =>
      parse_invalid_simple_expr(state, stop_kinds)
    _ => parse_expr_until(state, stop_kinds)
  }
}

///|
fn parse_expr_with_context_restriction(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  context : String,
  disallowed : (Token) -> Bool,
) -> CstNode {
  let first = state.peek_significant().map(triple => triple.0)
  let expr = parse_expr_until(state, stop_kinds)
  match first {
    Some(tok) if disallowed(tok) =>
      if unexpected_expr_or_statement_name(tok) is Some(actual) {
        state.report(expr.loc, "Unexpected \{actual} in \{context}")
      } else {
        ()
      }
    _ => ()
  }
  expr
}

///|
fn parse_pattern_guard_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  match state.peek_significant() {
    Some((LETREC, _, _)) => parse_invalid_guard_letrec(state, stop_kinds)
    Some((TRY_EXCLAMATION, _, _)) =>
      parse_invalid_simple_expr(state, stop_kinds)
    _ =>
      parse_expr_with_context_restriction(
        state, stop_kinds, "pattern guard", token_disallowed_in_pattern_guard,
      )
  }
}

///|
fn parse_invalid_guard_letrec(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut offset = 0
  let mut depth = 0
  let mut last_arrow = -1
  let mut scanning = true
  while scanning {
    match state.peek(offset~) {
      Some((SEMI(_) | RBRACE, _, _)) if depth == 0 => scanning = false
      Some((tok, _, _)) if depth == 0 &&
        kind_in(tok.kind(), stop_kinds) &&
        !(tok is FAT_ARROW) => scanning = false
      Some((FAT_ARROW, _, _)) if depth == 0 => {
        last_arrow = offset
        offset += 1
      }
      Some((EOF, _, _)) | None => scanning = false
      Some((tok, _, _)) => {
        depth = next_depth(depth, tok)
        offset += 1
      }
    }
  }
  if last_arrow <= 0 {
    return parse_invalid_simple_expr(state, stop_kinds)
  }
  match state.peek_significant() {
    Some((found, start, end)) =>
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "simple expression",
      )
    None => ()
  }
  let children : Array[(String?, CstNode)] = []
  for _ in 0..<last_arrow {
    if state.consume_any() is Some(bad) {
      push_child(
        children,
        Some("error"),
        error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
      )
    } else {
      ()
    }
  }
  state.node_from_children(Expr_MissingExpr, children)
}

///|
fn parse_expr_fragment(state : ParserState) -> CstNode {
  let reports_before_expr = state.reports.length()
  let expr = state.required_expr_node(
    parse_expr_fragment_item(state),
    reports_before_expr,
  )
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("expr"), expr)
  push_children(children, state.consume_trivia())
  while state.current_is(TokenKind::TK_SEMI) {
    if state.consume_any() is Some(sep) {
      push_child(children, Some("sep"), sep)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
    if !state.current_is(TokenKind::TK_EOF) && !(state.peek() is None) {
      let next = parse_expr_fragment_item(state)
      if !next.children.is_empty() {
        push_child(children, Some("expr"), next)
      }
      push_children(children, state.consume_trivia())
    }
  }
  if !state.current_is(TokenKind::TK_EOF) && !(state.peek() is None) {
    state.report_expected_tokens([TokenKind::TK_SEMI, TokenKind::TK_EOF])
    while !state.current_is(TokenKind::TK_EOF) && !(state.peek() is None) {
      if state.consume_any() is Some(bad) {
        push_child(
          children,
          Some("error"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
      push_children(children, state.consume_trivia())
    }
  }
  match state.peek() {
    Some((EOF, _, _)) =>
      if state.consume_any() is Some(eof) {
        push_child(children, Some("eof"), eof)
      } else {
        ()
      }
    None => push_child(children, Some("eof"), state.missing_eof_node())
    _ => ()
  }
  let root = node(Expression, state.root_loc(), state.root_span(), children)
  root
}

///|
fn parse_expr_fragment_item(state : ParserState) -> CstNode {
  if local_function_statement_can_start(state) {
    discard_expr_trivia(state)
    parse_local_function_statement(state, [
      TokenKind::TK_SEMI,
      TokenKind::TK_EOF,
    ])
  } else {
    parse_expr_until(state, [TokenKind::TK_EOF])
  }
}

///|
fn parse_expr_until(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  allow_argument_label_question? : Bool = false,
) -> CstNode {
  let _ = allow_argument_label_question
  discard_expr_trivia(state)
  match (state.peek(), state.peek(offset=1)) {
    (Some((POST_LABEL(_), _, _)), Some((COLON, _, _)))
    | (Some((POST_LABEL_TRANSPARENT(_), _, _)), Some((COLON, _, _))) =>
      parse_labelled_loop_expr(state, stop_kinds)
    _ => {
      let lhs = parse_expr_pipe(state, stop_kinds)
      parse_expr_catch_suffix(state, stop_kinds, lhs)
    }
  }
}

///|
fn parser_leading_parenthesized_end(node : CstNode) -> Position? {
  match node.kind {
    Expr_Unit | Expr_Group | Expr_Tuple | Expr_Constraint =>
      return Some(node.loc.end)
    _ => ()
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some("trivia"), _)) => continue
      Some((_, child)) => return parser_leading_parenthesized_end(child)
      None => ()
    }
  }
  None
}

///|
fn parser_first_significant_token_at_or_after(
  node : CstNode,
  pos : Position,
) -> CstNode? {
  match node.kind {
    _ if node.kind.is_syntax_token() &&
      (
        node.has_child_or_grandchild_kind(Comment_InlineTrailing) ||
        node.has_child_or_grandchild_kind(Comment_Ownline)
      ) => ()
    _ if node.kind.is_syntax_token() && node.loc.start.cnum >= pos.cnum =>
      return Some(node)
    _ => ()
  }
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((label, _)) if is_location_metadata_label(label) => continue
      Some((_, child)) =>
        if parser_first_significant_token_at_or_after(child, pos) is Some(found) {
          return Some(found)
        }
      None => ()
    }
  }
  None
}

///|
fn with_try_location_anchors(
  source : CstNode,
  has_try : Bool,
  suffix_anchor_source? : CstNode,
) -> CstNode {
  let mut result = source
  let keywords = source.children_with_label("kw")
  let try_keyword = keywords.get(0)
  let catch_keyword = if has_try { keywords.get(1) } else { keywords.get(0) }
  if try_keyword is Some(keyword) {
    result = result.with_token_location(
      "try_loc",
      keyword.kind,
      keyword.loc,
      keyword.source_span,
    )
  }
  if catch_keyword is Some(keyword) {
    let mut catch_end = keyword
    if has_try &&
      source.children_with_label("case").get(0) is Some(case_list) &&
      case_list.first_child_with_label("open") is Some(open) {
      catch_end = open
    }
    result = result.with_aggregate_span(
      "catch_loc",
      keyword.loc.merge(catch_end.loc),
      mk_span(keyword.source_span.0, catch_end.source_span.1),
    )
  }
  if source.first_child_with_label("else_kw") is Some(keyword) {
    let mut else_end = keyword
    if source.children_with_label("else").get(0) is Some(case_list) &&
      case_list.first_child_with_label("open") is Some(open) {
      else_end = open
    }
    result = result.with_aggregate_span(
      "else_loc",
      keyword.loc.merge(else_end.loc),
      mk_span(keyword.source_span.0, else_end.source_span.1),
    )
  }
  if suffix_anchor_source is Some(anchor_source) {
    result = result.with_aggregate_span(
      "expr_loc",
      Location::{ start: anchor_source.loc.start, end: source.loc.end, },
      mk_span(anchor_source.source_span.0, source.source_span.1),
    )
  }
  result
}

///|
fn parse_expr_catch_suffix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  lhs : CstNode,
) -> CstNode {
  if kind_in(TokenKind::TK_CATCH, stop_kinds) ||
    lhs.kind is Expr_Try ||
    !state.current_is(TokenKind::TK_CATCH) {
    return lhs
  }
  let leading_parenthesized_end = parser_leading_parenthesized_end(lhs)
  let children : Array[(String?, CstNode)] = [(Some("body"), lhs)]
  let mut catch_keyword : CstNode? = None
  if state.consume_any() is Some(catch_kw) {
    catch_keyword = Some(catch_kw)
    push_child(children, Some("kw"), catch_kw)
  } else {
    ()
  }
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("case"), parse_case_block(state))
  } else {
    push_child(
      children,
      Some("case"),
      state.missing_token_node_with_expected(TokenKind::TK_LBRACE, [
        TokenKind::TK_LBRACE,
      ]),
    )
  }
  if state.current_is(TokenKind::TK_NORAISE) {
    if state.consume_any() is Some(else_kw) {
      push_child(children, Some("else_kw"), else_kw)
    } else {
      ()
    }
    if state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("else"), parse_case_block(state))
    }
  }
  let result = state
    .node_from_children(Expr_Try, children)
    .with_bool_marker("has_try", Flag_HasTry, false)
  let suffix_anchor_source = match (leading_parenthesized_end, catch_keyword) {
    (Some(paren_end), Some(catch_kw)) =>
      Some(
        parser_first_significant_token_at_or_after(lhs, paren_end).unwrap_or(
          catch_kw,
        ),
      )
    _ => None
  }
  with_try_location_anchors(result, false, suffix_anchor_source?)
}

///|
fn parse_expr_pipe(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let reports_before_lhs = state.reports.length()
  let mut lhs = parse_expr_infix(state, stop_kinds)
  let mut done = false
  while !done {
    discard_expr_trivia(state)
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((COLON, _, _)) => done = true
      Some((EQUAL, _, _))
      | Some((PLUS_EQUAL, _, _))
      | Some((AUGMENTED_ASSIGNMENT(_), _, _)) => {
        let normalize_lhs = state.reports.length() == reports_before_lhs &&
          assignment_lhs_is_valid(lhs)
        if state.reports.length() == reports_before_lhs && !normalize_lhs {
          state.report(lhs.loc, "Invalid left value in assignment")
        }
        let is_augmented = !state.current_is(TokenKind::TK_EQUAL)
        let kind = assignment_node_kind(lhs, is_augmented)
        let children : Array[(String?, CstNode)] = []
        push_assignment_lhs_children(children, lhs, normalize_lhs)
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
          push_direct_operator(children, op)
        } else {
          ()
        }
        let reports_before_rhs = state.reports.length()
        let rhs = parse_expr_until(state, stop_kinds)
        push_child(
          children,
          Some("rhs"),
          state.required_expr_node(rhs, reports_before_rhs),
        )
        lhs = state
          .node_from_children(kind, children)
          .with_bool_marker("augmented", Flag_Augmented, is_augmented)
      }
      Some((PIPE, _, _)) | Some((PIPE_LEFT, _, _)) => {
        let pipe_kind = if state.current_is(TokenKind::TK_PIPE_LEFT) {
          TokenKind::TK_PIPE_LEFT
        } else {
          TokenKind::TK_PIPE
        }
        let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
        } else {
          ()
        }
        let reports_before_rhs = state.reports.length()
        let rhs = parse_pipe_rhs(state, stop_kinds, pipe_kind)
        push_child(
          children,
          Some("rhs"),
          state.required_expr_node(rhs, reports_before_rhs),
        )
        lhs = state.node_from_children(
          if pipe_kind is TokenKind::TK_PIPE_LEFT {
            Expr_RevPipe
          } else {
            Expr_Pipe
          },
          children,
        )
      }
      Some((FAT_ARROW, _, _)) if arrow_lhs_can_start_function(lhs) => {
        if lhs.kind is Expr_Constraint &&
          lhs.first_child_with_label("open") is None {
          if lhs.first_child_with_label("op") is Some(op) {
            state.report_expected_tokens_at(COLON, op.loc, [
              TokenKind::TK_SEMI,
              TokenKind::TK_EOF,
            ])
          } else {
            ()
          }
        }
        let children : Array[(String?, CstNode)] = [
          (Some("params"), parse_arrow_params_from_expr(lhs)),
        ]
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
        } else {
          ()
        }
        let body = parse_arrow_fn_body_expr(state, stop_kinds)
        if !body.children.is_empty() {
          push_child(children, Some("body"), body)
        }
        lhs = function_node_from_children(state, children, FnKind_Arrow)
      }
      _ => done = true
    }
  }
  lhs
}

///|
fn stop_kinds_with_pipe(stop_kinds : Array[TokenKind]) -> Array[TokenKind] {
  let stops : Array[TokenKind] = [TokenKind::TK_PIPE, TokenKind::TK_PIPE_LEFT]
  for kind in stop_kinds {
    stops.push(kind)
  }
  stops
}

///|
fn parse_pipe_rhs(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  pipe_kind : TokenKind,
) -> CstNode {
  let head = parse_expr_infix(state, stop_kinds)
  discard_expr_trivia(state)
  match state.peek() {
    Some((FAT_ARROW, _, _)) => {
      let children : Array[(String?, CstNode)] = [
        (Some("params"), parse_arrow_params_from_expr(head)),
      ]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
        push_child(children, Some("pipe"), op)
      } else {
        ()
      }
      if pipe_kind is TokenKind::TK_PIPE {
        match state.peek_significant() {
          Some((LBRACE, _, _)) => ()
          Some((found, start, end)) =>
            state.report_expected_description_at(
              found,
              mk_loc(start, end),
              "`{ ... }`",
            )
          None => state.report_expected_description("`{ ... }`")
        }
      }
      let reports_before_body = state.reports.length()
      let body = parse_arrow_fn_body_expr(
        state,
        stop_kinds_with_pipe(stop_kinds),
      )
      push_child(
        children,
        Some("body"),
        state.required_expr_node(body, reports_before_body),
      )
      function_node_from_children(state, children, FnKind_Arrow)
    }
    _ => head
  }
}

///|
fn parse_arrow_params_from_expr(expr : CstNode) -> CstNode {
  match expr.kind {
    Expr_Unit | Expr_Group | Expr_Tuple =>
      if expr.first_child_with_label("body") is Some(body) {
        let children : Array[(String?, CstNode)] = []
        for entry in body.children {
          let (label, child) = entry
          if label is Some("item") {
            push_child(
              children,
              Some("item"),
              parse_arrow_param_from_expr(child),
            )
          } else {
            children.push(entry)
          }
        }
        node(List_ParameterList, body.loc, body.source_span, children)
      } else {
        parse_single_arrow_param_list(expr)
      }
    _ => parse_single_arrow_param_list(expr)
  }
}

///|
fn parse_single_arrow_param_list(expr : CstNode) -> CstNode {
  node(List_ParameterList, expr.loc, expr.source_span, [
    (Some("item"), parse_arrow_param_from_expr(expr)),
  ])
}

///|
fn arrow_parameter_kind(expr : CstNode) -> NodeKind {
  match expr.kind {
    Expr_Hole | Expr_MissingExpr => Parameter_DiscardPositional
    Argument_Labelled | Argument_LabelledPun => Parameter_Labelled
    Argument_LabelledOption | Argument_LabelledOptionPun =>
      Parameter_QuestionOptional
    _ => Parameter_Positional
  }
}

///|
fn arrow_var_pattern_from_ident(expr : CstNode) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for entry in expr.children {
    if !(entry.0 is Some("semantic_name")) {
      children.push(entry)
    }
  }
  node(Pattern_Var, expr.loc, expr.source_span, children).with_structured_name(
    "semantic_name",
    Name_Binder,
    ["name"],
  )
}

///|
fn arrow_any_pattern_from_hole(expr : CstNode) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for entry in expr.children {
    let (label, child) = entry
    if child.kind.is_syntax_token() || child.kind is (Error | Missing) {
      children.push(
        (if label is Some("token") { Some("name") } else { label }, child),
      )
    }
  }
  node(Pattern_Any, expr.loc, expr.source_span, children)
}

///|
fn arrow_var_pattern_from_argument(expr : CstNode) -> CstNode? {
  guard expr.first_child_with_label("name") is Some(name) else { return None }
  if expr.kind is (Argument_LabelledPun | Argument_LabelledOptionPun) {
    let children : Array[(String?, CstNode)] = [(Some("name"), name)]
    Some(
      node(Pattern_Var, name.loc, name.source_span, children).with_structured_name(
        "semantic_name",
        Name_Binder,
        ["name"],
      ),
    )
  } else {
    semantic_var_pattern_from_name(name)
  }
}

///|
fn arrow_pattern_from_expr(expr : CstNode) -> CstNode? {
  match expr.kind {
    Expr_Ident => Some(arrow_var_pattern_from_ident(expr))
    Expr_Hole => Some(arrow_any_pattern_from_hole(expr))
    Expr_MissingExpr => {
      let children : Array[(String?, CstNode)] = []
      for entry in expr.children {
        children.push(entry)
      }
      Some(node(Pattern_MissingPattern, expr.loc, expr.source_span, children))
    }
    kind if kind.is_argument() => arrow_var_pattern_from_argument(expr)
    _ => None
  }
}

///|
fn append_arrow_argument_syntax(
  children : Array[(String?, CstNode)],
  argument : CstNode,
) -> Unit {
  if argument.kind is (Argument_LabelledOption | Argument_LabelledOptionPun) {
    for entry in argument.children {
      let (label, child) = entry
      if label is Some("op") &&
        child
        .first_operator_descendant()
        .map(operator => operator.string_payload().unwrap_or("") == "?")
        .unwrap_or(false) {
        push_child(children, Some("question"), child)
      }
    }
  }
  if argument.kind is (Argument_Labelled | Argument_LabelledOption) {
    push_child(children, Some("error"), argument)
  }
}

///|
fn parse_arrow_param_from_expr(expr : CstNode) -> CstNode {
  let source = if expr.kind is Expr_Constraint {
    expr
    .first_child_with_label("expr")
    .unwrap_or(node(Expr_MissingExpr, expr.loc, expr.source_span, []))
  } else {
    expr
  }
  let parameter_kind = arrow_parameter_kind(source)
  let children : Array[(String?, CstNode)] = []
  match arrow_pattern_from_expr(source) {
    Some(pattern) => push_child(children, Some("pattern"), pattern)
    None => push_child(children, Some("error"), source)
  }
  if source.kind.is_argument() {
    append_arrow_argument_syntax(children, source)
  }
  if expr.kind is Expr_Constraint {
    for entry in expr.children {
      let (label, _) = entry
      if !(label is Some("expr")) {
        children.push(entry)
      } else {
        ()
      }
    }
  }
  node(parameter_kind, expr.loc, expr.source_span, children)
}

///|
fn parse_expr_infix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  state.abort_infix_chain = false
  let expr = parse_expr_binary_prec(state, stop_kinds, 1)
  state.abort_infix_chain = false
  expr
}

///|
fn parse_invalid_infix_operand(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut skipped_line_break = false
  while state.peek() is (Some((SEMI(false), _, _)) | Some((NEWLINE, _, _))) {
    skipped_line_break = true
    if state.consume_any() is Some(line_break) {
      push_child(children, Some("trivia"), line_break)
    }
  }
  if skipped_line_break && state.current_is(TK_LET) {
    state.report_next_top_level_start = true
  }
  push_child(
    children,
    Some("expr"),
    parse_invalid_simple_expr(state, stop_kinds),
  )
  state.node_from_children(Expr_MissingExpr, children)
}

///|
fn infix_precedence(tok : Token) -> (Int, Bool)? {
  match tok {
    IMPLIES => Some((1, true))
    BARBAR => Some((2, true))
    AMPERAMPER => Some((3, true))
    BAR => Some((4, false))
    CARET => Some((5, false))
    AMPER => Some((6, false))
    INFIX1(_) => Some((7, false))
    INFIX2(_) => Some((8, false))
    PLUS | MINUS => Some((9, false))
    INFIX3(_) => Some((10, false))
    INFIX4(_) => Some((11, false))
    _ => None
  }
}

///|
fn token_invalid_as_infix_operand(tok : Token) -> Bool {
  match tok {
    IF
    | MATCH
    | WHILE
    | FOR
    | TRY
    | TRY_EXCLAMATION
    | LET
    | LETREC
    | GUARD
    | GUARD_EXCLAMATION
    | DEFER
    | BREAK
    | CONTINUE
    | RETURN
    | RAISE
    | LOOP => true
    _ => false
  }
}

///|
fn expr_stops_infix_chain(expr : CstNode) -> Bool {
  match expr.kind {
    Expr_If
    | Expr_Match
    | Expr_Try
    | Expr_TryOperator
    | Expr_For
    | Expr_ForEach
    | Expr_While
    | Expr_LexMatch
    | Expr_LexScan
    | Expr_Let
    | Expr_LetMut
    | Expr_LetAnd
    | Expr_LetFn
    | Expr_Return
    | Expr_Raise
    | Expr_Break
    | Expr_Continue
    | Expr_Defer
    | Expr_ErrDefer
    | Expr_Guard
    | Expr_Quantifier => true
    _ => false
  }
}

///|
fn parse_expr_binary_prec(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  min_prec : Int,
) -> CstNode {
  let mut lhs = parse_expr_postfix(state, stop_kinds)
  let mut done = expr_stops_infix_chain(lhs)
  while !done {
    discard_expr_trivia(state)
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((tok, _, _)) =>
        match infix_precedence(tok) {
          Some((prec, right_assoc)) if prec >= min_prec => {
            let expr_kind = if tok is IMPLIES {
              Expr_Implies
            } else {
              Expr_Infix
            }
            let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
            if state.consume_any() is Some(op) {
              push_child(children, Some("op"), op)
              push_direct_operator(children, op)
            } else {
              ()
            }
            let next_min = if right_assoc { prec } else { prec + 1 }
            let reports_before_rhs = state.reports.length()
            let rhs = match state.peek_significant() {
              Some((next, _, _)) if token_invalid_as_infix_operand(next) => {
                state.abort_infix_chain = true
                parse_invalid_infix_operand(state, stop_kinds)
              }
              _ => parse_expr_binary_prec(state, stop_kinds, next_min)
            }
            push_child(
              children,
              Some("rhs"),
              state.required_expr_node(rhs, reports_before_rhs),
            )
            lhs = state.node_from_children(expr_kind, children)
            if state.abort_infix_chain {
              done = true
            }
          }
          _ => done = true
        }
      None => done = true
    }
  }
  lhs
}

///|
fn parse_expr_postfix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let lhs = parse_expr_range(state, stop_kinds)
  discard_expr_trivia(state)
  match state.peek() {
    Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => lhs
    Some((AS, _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("expr"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let ty = parse_type_name_node(state)
      if !ty.children.is_empty() {
        push_child(children, Some("type"), ty)
      }
      state.node_from_children(Expr_As, children)
    }
    Some((IS, _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("expr"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let reports_before_pattern = state.reports.length()
      let pat = parse_pattern_range(state, stop_kinds)
      push_child(
        children,
        Some("pattern"),
        state.required_pattern_node(pat, reports_before_pattern),
      )
      state.node_from_children(Expr_Is, children)
    }
    Some((EQ_TILDE, _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("expr"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let reports_before_pattern = state.reports.length()
      let pat = parse_regex_pattern_until(state, stop_kinds)
      push_child(
        children,
        Some("pattern"),
        state.required_pattern_node(
          pat,
          reports_before_pattern,
          expected="regex pattern",
        ),
      )
      state.node_from_children(Expr_RegexMatch, children)
    }
    Some((LT_PLUS, _, _)) | Some((LT_QUESTION, _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("expr"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      report_invalid_template_literal_start(state)
      let reports_before_template = state.reports.length()
      let template = parse_expr_until(state, template_operand_stops(stop_kinds))
      push_child(
        children,
        Some("template"),
        state.required_expr_node(template, reports_before_template),
      )
      state
      .node_from_children(Expr_TemplateWriting, children)
      .with_bool_marker(
        "conditional",
        Flag_Conditional,
        children
        .iter()
        .any(fn(entry) {
          entry.0 is Some("op") && entry.1.has_child_kind(Flag_Conditional)
        }),
      )
    }
    _ => lhs
  }
}

///|
fn report_invalid_template_literal_start(state : ParserState) -> Unit {
  match state.peek_significant() {
    Some((LBRACE, _, _)) =>
      match state.peek_nth_significant(1) {
        Some((RBRACE, _, _)) | None => ()
        Some((STRING(_), _, _)) => ()
        Some((found, start, end)) =>
          state.report_expected_tokens_at(found, mk_loc(start, end), [
            TokenKind::TK_STRING,
          ])
      }
    Some(
      (
        STRING(_)
        | BYTES(_)
        | INTERP(_)
        | BYTES_INTERP(_)
        | MULTILINE_STRING(_)
        | MULTILINE_INTERP(_),
        _,
        _,
      )
    ) => ()
    Some((found, start, end)) =>
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "template string",
      )
    None => ()
  }
}

///|
fn parse_expr_simple_postfix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut lhs = parse_expr_simple(state, stop_kinds)
  let mut done = false
  while !done {
    discard_expr_trivia(state)
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((_, _, _)) =>
        if parse_expr_postfix_suffix(state, lhs) is Some(next) {
          lhs = next
        } else {
          done = true
        }
      None => done = true
    }
  }
  lhs
}

///|
fn template_operand_stops(stop_kinds : Array[TokenKind]) -> Array[TokenKind] {
  let stops = stop_kinds.copy()
  stops.push(TokenKind::TK_IMPLIES)
  stops.push(TokenKind::TK_BARBAR)
  stops.push(TokenKind::TK_AMPERAMPER)
  stops.push(TokenKind::TK_BAR)
  stops.push(TokenKind::TK_CARET)
  stops.push(TokenKind::TK_AMPER)
  stops.push(TokenKind::TK_INFIX1)
  stops.push(TokenKind::TK_INFIX2)
  stops.push(TokenKind::TK_PLUS)
  stops.push(TokenKind::TK_MINUS)
  stops.push(TokenKind::TK_INFIX3)
  stops.push(TokenKind::TK_INFIX4)
  stops
}

///|
fn parse_expr_postfix_suffix(state : ParserState, lhs : CstNode) -> CstNode? {
  match state.peek() {
    Some((LPAREN, _, _)) => {
      let args = parse_argument_list(state)
      Some(
        state.node_from_children(Expr_Apply, [
          (Some("func"), lhs),
          (Some("arg"), args),
        ]),
      )
    }
    Some((LBRACKET, _, _)) => Some(parse_array_get_suffix(state, lhs))
    Some((DOT_LIDENT(_), _, _)) | Some((DOT_INT(_), _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("self"), lhs)]
      if state.consume_any() is Some(field) {
        push_child(children, Some("token"), field)
        push_child(children, Some("field"), accessor_node_from_token(field))
      } else {
        ()
      }
      if state.current_is(TokenKind::TK_LPAREN) {
        push_child(children, Some("arg"), parse_argument_list(state))
        Some(
          state
          .node_from_children(Expr_DotApply, children)
          .with_bool_marker("return_self", Flag_ReturnSelf, false),
        )
      } else {
        Some(state.node_from_children(Expr_Field, children))
      }
    }
    Some((COLONCOLON, _, _)) => Some(parse_coloncolon_suffix(state, lhs))
    Some((DOTDOT, _, _)) => {
      let children : Array[(String?, CstNode)] = [(Some("self"), lhs)]
      let mut valid_field = true
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      match state.peek() {
        Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
          if state.consume_any() is Some(field) {
            push_child(children, Some("token"), field)
            push_child(children, Some("field"), accessor_node_from_token(field))
          } else {
            ()
          }
        Some((found, start, end)) => {
          valid_field = false
          state.report_expected_tokens_at(found, mk_loc(start, end), [
            TokenKind::TK_LIDENT,
            TokenKind::TK_EXTEND,
          ])
          while !state.current_is(TokenKind::TK_SEMI) &&
                !state.current_is(TokenKind::TK_RBRACE) &&
                !state.current_is(TokenKind::TK_EOF) &&
                !(state.peek() is None) {
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            } else {
              ()
            }
          }
        }
        None => valid_field = false
      }
      if valid_field && state.current_is(TokenKind::TK_LPAREN) {
        push_child(children, Some("arg"), parse_argument_list(state))
      } else if valid_field {
        push_child(
          children,
          Some("arg"),
          state.missing_token_node(TokenKind::TK_LPAREN),
        )
      }
      Some(
        state
        .node_from_children(Expr_DotApply, children)
        .with_bool_marker("return_self", Flag_ReturnSelf, true),
      )
    }
    _ => None
  }
}

///|
fn parse_coloncolon_suffix(state : ParserState, lhs : CstNode) -> CstNode {
  let children : Array[(String?, CstNode)] = [(Some("type"), lhs)]
  if state.consume_any() is Some(op) {
    push_child(children, Some("op"), op)
  } else {
    ()
  }
  push_children(children, state.consume_trivia())
  if state.current_is(TK_LBRACE) {
    let body = parse_record_field_list(state)
    let kind = if list_has_item_kind(body, Field_RecordSpread) {
      Expr_RecordUpdate
    } else {
      Expr_Record
    }
    push_child(children, Some("body"), body)
    state.node_from_children(kind, children)
  } else {
    let mut field_is_constructor = false
    match state.peek() {
      Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("field"), name)
        } else {
          ()
        }
      Some((UIDENT(_), _, _)) =>
        if state.consume_any() is Some(name) {
          field_is_constructor = true
          push_child(children, Some("field"), name)
        } else {
          ()
        }
      Some((PACKAGE_NAME(_), _, _)) => {
        if state.consume_any() is Some(pkg) {
          push_child(children, Some("pkg"), pkg)
        } else {
          ()
        }
        match state.peek() {
          Some((DOT_UIDENT(_), _, _)) =>
            if state.consume_any() is Some(name) {
              field_is_constructor = true
              push_child(children, Some("field"), name)
            } else {
              ()
            }
          _ =>
            push_child(
              children,
              Some("field"),
              state.missing_token_node(TokenKind::TK_UIDENT),
            )
        }
      }
      Some((found, start, end)) => {
        state.report(
          mk_loc(start, end),
          "Unexpected token \{found.to_expect_string()}, you may expect method name, constructor name, \"[\" or \"{\".",
        )
        if state.consume_any() is Some(bad) {
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          ()
        }
      }
      None =>
        push_child(
          children,
          Some("field"),
          state.missing_token_node_silent(TokenKind::TK_LIDENT),
        )
    }
    if field_is_constructor {
      state
      .node_from_children(Expr_Constr, children)
      .with_structured_name("semantic_name", Name_Constructor, [
        "pkg", "type", "name", "field",
      ])
      .with_constructor_extra_info()
    } else {
      state.node_from_children(Expr_Method, children)
    }
  }
}

///|
fn brace_starts_record_expr(
  state : ParserState,
  start_offset? : Int = 0,
) -> Bool {
  let mut offset = start_offset + 1
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((DOTDOT, _, _)) => return true
      Some((LIDENT(_), _, _))
      | Some((UIDENT(_), _, _))
      | Some((EXTEND, _, _)) => {
        offset += 1
        let mut after_name = false
        while !after_name {
          match state.peek(offset~) {
            Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
            Some((COLON, _, _)) | Some((COMMA, _, _)) => return true
            _ => after_name = true
          }
        }
        done = true
      }
      _ => done = true
    }
  }
  false
}

///|
fn brace_starts_map_expr(state : ParserState, start_offset? : Int = 0) -> Bool {
  let mut offset = start_offset + 1
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((RBRACE, _, _)) => return true
      Some((MINUS, _, _)) => {
        offset += 1
        match state.peek(offset~) {
          Some((INT(_), _, _))
          | Some((DOUBLE(_), _, _))
          | Some((FLOAT(_), _, _)) => {
            offset += 1
            let mut after_key = false
            while !after_key {
              match state.peek(offset~) {
                Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
                Some((COLON, _, _)) => return true
                _ => after_key = true
              }
            }
          }
          _ => ()
        }
        done = true
      }
      Some((STRING(_), _, _))
      | Some((BYTES(_), _, _))
      | Some((REGEX_LITERAL(_), _, _))
      | Some((CHAR(_), _, _))
      | Some((INT(_), _, _))
      | Some((BYTE(_), _, _))
      | Some((DOUBLE(_), _, _))
      | Some((FLOAT(_), _, _))
      | Some((TRUE, _, _))
      | Some((FALSE, _, _)) => {
        offset += 1
        let mut after_key = false
        while !after_key {
          match state.peek(offset~) {
            Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
            Some((COLON, _, _)) => return true
            _ => after_key = true
          }
        }
        done = true
      }
      _ => done = true
    }
  }
  false
}

///|
fn brace_expr_is_followed_by_range_operator(state : ParserState) -> Bool {
  let mut offset = 0
  let mut depth = 0
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((LBRACE, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RBRACE, _, _)) => {
        depth -= 1
        offset += 1
        if depth == 0 {
          while true {
            match state.peek(offset~) {
              Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
              Some((tok, _, _)) => return range_operator(tok)
              None => return false
            }
          }
        }
      }
      Some((EOF, _, _)) | None => done = true
      Some((_, _, _)) => offset += 1
    }
  }
  false
}

///|
fn parse_record_expr(state : ParserState) -> CstNode {
  let body = parse_record_field_list(state)
  state.node_from_children(
    if list_has_item_kind(body, Field_RecordSpread) {
      Expr_RecordUpdate
    } else {
      Expr_Record
    },
    [(Some("body"), body)],
  )
}

///|
fn parse_map_expr(state : ParserState) -> CstNode {
  state.node_from_children(Expr_Map, [
    (Some("body"), parse_map_field_list(state)),
  ])
}

///|
fn parse_record_field_list(state : ParserState) -> CstNode {
  let list = parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_record_field_item,
    List_RecordFieldList,
    sep_kind=TokenKind::TK_COMMA,
  )
  let mut item_index = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), item)) => {
        match item.kind {
          Field_RecordSpread if item_index > 0 =>
            if item.first_child_with_label("op") is Some(op) {
              state.report_expected_tokens_at(DOTDOT, op.loc, [
                TokenKind::TK_LIDENT,
                TokenKind::TK_EXTEND,
              ])
            }
          _ => ()
        }
        item_index += 1
      }
      _ => ()
    }
  }
  with_marker_child(list, "trailing", list_trailing_mark_kind(list))
}

///|
fn parse_map_field_list(state : ParserState) -> CstNode {
  let list = parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_map_field_item,
    List_MapFieldList,
    sep_kind=TokenKind::TK_COMMA,
  )
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), item)) => {
        if item.has_child_or_grandchild_kind(Operator_UnaryMinus) {
          let operators = item.children_with_label("op")
          if operators.length() > 1 {
            let colon = operators[operators.length() - 1]
            state.report_expected_tokens_at(COLON, colon.loc, [
              TokenKind::TK_SEMI,
              TokenKind::TK_RBRACE,
            ])
          }
        }
        break
      }
      _ => ()
    }
  }
  with_marker_child(list, "trailing", list_trailing_mark_kind(list))
}

///|
fn parse_map_field_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut constant : CstNode? = None
  let mut negative = false
  match state.peek() {
    Some((MINUS, _, _)) => {
      negative = true
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
        push_child(
          children,
          Some("operator"),
          operator_node_from_token(
            Operator_UnaryMinus,
            MINUS,
            op.loc,
            op.source_span,
          ),
        )
      } else {
        ()
      }
      match state.peek() {
        Some((raw, _, _)) if raw is (INT(_) | DOUBLE(_) | FLOAT(_)) =>
          if state.consume_any() is Some(key) {
            push_child(children, Some("key"), key)
            constant = constant_node_from_token(raw, key.loc, key.source_span)
          } else {
            ()
          }
        Some((found, start, end)) => {
          state.report_expected_description_at(
            found,
            mk_loc(start, end),
            "`-Int` or `-Float` in key of map pattern",
          )
          if state.consume_any() is Some(key) {
            push_child(children, Some("key"), key)
          } else {
            ()
          }
        }
        None => ()
      }
    }
    Some((raw, _, _)) if raw
      is (STRING(_)
      | BYTES(_)
      | REGEX_LITERAL(_)
      | CHAR(_)
      | INT(_)
      | BYTE(_)
      | DOUBLE(_)
      | FLOAT(_)
      | TRUE
      | FALSE) =>
      if state.consume_any() is Some(key) {
        push_child(children, Some("key"), key)
        constant = constant_node_from_token(raw, key.loc, key.source_span)
      } else {
        ()
      }
    Some((found, start, end)) => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_TRUE,
        TokenKind::TK_FALSE,
        TokenKind::TK_CHAR,
        TokenKind::TK_INT,
        TokenKind::TK_FLOAT,
        TokenKind::TK_DOUBLE,
        TokenKind::TK_MINUS,
        TokenKind::TK_STRING,
        TokenKind::TK_REGEX_LITERAL,
      ])
      if state.consume_any() is Some(bad) {
        push_child(
          children,
          Some("error"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
    }
    None => ()
  }
  if constant is Some(value) {
    push_child(
      children,
      Some("constant"),
      signed_constant_node(value, negative),
    )
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
  }
  let expr = parse_expr_until(state, [
    TokenKind::TK_COMMA,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ])
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  state.node_from_children(Field_MapExpression, children)
}

///|
fn parse_record_field_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut kind = Field_Definition
  if state.current_is(TokenKind::TK_DOTDOT) {
    kind = Field_RecordSpread
    if state.consume_any() is Some(op) {
      push_child(children, Some("op"), op)
    } else {
      ()
    }
    let expr = parse_expr_until(state, [
      TokenKind::TK_COMMA,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ])
    if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
    }
  } else {
    match state.peek() {
      Some((LIDENT(_), _, _))
      | Some((EXTEND, _, _))
      | Some((UIDENT(_), _, _))
      | Some((STRING(_), _, _)) =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
      _ => ()
    }
    if state.current_is(TokenKind::TK_COLON) {
      if state.consume_any() is Some(colon) {
        push_child(children, Some("op"), colon)
      } else {
        ()
      }
      let expr = parse_expr_until(state, [
        TokenKind::TK_COMMA,
        TokenKind::TK_RBRACE,
        TokenKind::TK_EOF,
      ])
      if !expr.children.is_empty() {
        push_child(children, Some("expr"), expr)
      }
    } else if children.length() == 0 {
      let expr = parse_expr_until(state, [
        TokenKind::TK_COMMA,
        TokenKind::TK_RBRACE,
        TokenKind::TK_EOF,
      ])
      if !expr.children.is_empty() {
        push_child(children, Some("expr"), expr)
      }
    }
  }
  let field = state.node_from_children(kind, children)
  if kind is Field_Definition {
    let is_pun = children.iter().all(fn(entry) { !(entry.0 is Some("expr")) })
    let field = field
      .with_structured_name("semantic_name", Name_Field, ["name"])
      .with_bool_marker("pun", Flag_Pun, is_pun)
    if is_pun {
      field.with_pun_expr()
    } else {
      field
    }
  } else {
    field
  }
}

///|
fn parse_array_get_suffix(state : ParserState, lhs : CstNode) -> CstNode {
  let children : Array[(String?, CstNode)] = [(Some("self"), lhs)]
  let open = state.expect_token(TokenKind::TK_LBRACKET)
  push_child(children, Some("open"), open)
  let mut start_index : CstNode? = None
  let reports_before_index = state.reports.length()
  if !state.current_is(TokenKind::TK_COLON) {
    let index = parse_expr_until(state, [
      TokenKind::TK_RBRACKET,
      TokenKind::TK_COLON,
      TokenKind::TK_EOF,
    ])
    start_index = Some(state.required_expr_node(index, reports_before_index))
  }
  if state.current_is(TokenKind::TK_COLON) {
    if start_index is Some(index) {
      push_child(children, Some("start"), index)
    } else {
      ()
    }
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    if !state.current_is(TokenKind::TK_RBRACKET) &&
      !state.current_is(TokenKind::TK_EOF) {
      let end_index = parse_expr_until(state, [
        TokenKind::TK_RBRACKET,
        TokenKind::TK_EOF,
      ])
      if !end_index.children.is_empty() {
        push_child(children, Some("end"), end_index)
      }
    }
    let close = state.expect_token(TokenKind::TK_RBRACKET)
    push_child(children, Some("close"), close)
    state
    .node_from_children(Expr_ArrayGetSlice, children)
    .with_aggregate_span(
      "index_loc",
      open.loc.merge(close.loc),
      mk_span(open.source_span.0, close.source_span.1),
    )
  } else {
    if start_index is Some(index) {
      push_child(children, Some("index"), index)
    } else {
      ()
    }
    let close = if !state.current_is(TokenKind::TK_RBRACKET) &&
      state.reports.length() > reports_before_index {
      state.missing_token_node_silent(TokenKind::TK_RBRACKET)
    } else {
      state.expect_token(TokenKind::TK_RBRACKET)
    }
    push_child(children, Some("close"), close)
    state
    .node_from_children(Expr_ArrayGet, children)
    .with_aggregate_span(
      "index_loc",
      open.loc.merge(close.loc),
      mk_span(open.source_span.0, close.source_span.1),
    )
  }
}

///|
fn range_operator(tok : Token) -> Bool {
  match tok {
    RANGE_EXCLUSIVE
    | RANGE_INCLUSIVE
    | RANGE_LT_INCLUSIVE
    | RANGE_INCLUSIVE_REV
    | RANGE_EXCLUSIVE_REV => true
    _ => false
  }
}

///|
fn parse_expr_range(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut lhs = parse_expr_prefix(state, stop_kinds)
  match state.peek() {
    Some((tok, _, _)) if !kind_in(tok.kind(), stop_kinds) && range_operator(tok) => {
      let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
        push_direct_operator(children, op)
      } else {
        ()
      }
      let reports_before_rhs = state.reports.length()
      let rhs = parse_expr_prefix(state, stop_kinds)
      push_child(
        children,
        Some("rhs"),
        state.required_expr_node(rhs, reports_before_rhs),
      )
      lhs = state.node_from_children(Expr_Infix, children)
    }
    _ => ()
  }
  lhs
}

///|
fn parse_expr_prefix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  discard_expr_trivia(state)
  match state.peek() {
    Some((operator, _, _)) if operator is (PLUS | MINUS | EXCLAMATION) => {
      let op = state
        .consume_any()
        .unwrap_or(state.missing_token_node_silent(operator.kind()))
      let semantic_operator = operator_node_from_token(
        match operator {
          PLUS => Operator_UnaryPlus
          MINUS => Operator_UnaryMinus
          _ => Operator_UnaryNot
        },
        operator,
        op.loc,
        op.source_span,
      )
      let reports_before_expr = state.reports.length()
      let operand = state.required_expr_node(
        parse_expr_prefix(state, stop_kinds),
        reports_before_expr,
      )
      let constant = match operand.first_child_with_label("semantic_constant") {
        Some(constant) => Some(constant)
        None => operand.first_child_of_kind(NodeKind::is_constant)
      }
      match constant {
        Some(constant) if constant_can_fold_prefix(constant, operator) =>
          folded_prefix_constant_node(
            op,
            semantic_operator,
            operand,
            constant,
            operator is MINUS,
          )
        _ if operator is PLUS =>
          unary_plus_apply_node(op, semantic_operator, operand)
        _ =>
          state.node_from_children(Expr_Unary, [
            (Some("op"), op),
            (Some("operator"), semantic_operator),
            (Some("expr"), operand),
          ])
      }
    }
    _ => parse_expr_simple_postfix(state, stop_kinds)
  }
}

///|
fn parse_invalid_simple_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut pending_function_body = false
  let mut report_invalid_continue = false
  let mut invalid_continue_start = false
  let mut reported_promoted_function = false
  let mut invalid_pub_start = false
  let mut saw_local_let = false
  let invalid_top_level_start = match state.peek() {
    Some((found, start, end)) => {
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "simple expression",
      )
      report_invalid_continue = found is LOOP
      invalid_continue_start = found is CONTINUE
      invalid_pub_start = found is PUB
      top_level_decl_can_start(found)
    }
    None => false
  }
  while !state.current_in(stop_kinds) &&
        !state.current_is(TokenKind::TK_EOF) &&
        !(state.peek() is None) {
    match state.peek() {
      Some((found, _, _)) =>
        if state.consume_any() is Some(bad) {
          match found {
            FN => {
              pending_function_body = true
              if invalid_continue_start &&
                !reported_promoted_function &&
                state.peek_significant().map(item => item.0) is Some(LPAREN) {
                report_promoted_anonymous_function(state)
                reported_promoted_function = true
              }
            }
            LET | LETREC => saw_local_let = true
            LBRACE => pending_function_body = false
            _ => ()
          }
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        }
      None => ()
    }
  }
  if invalid_top_level_start &&
    pending_function_body &&
    state.current_is(TokenKind::TK_EOF) {
    state.report_expected_tokens([TokenKind::TK_LBRACE])
  }
  if report_invalid_continue &&
    saw_local_let &&
    state.current_is(TokenKind::TK_SEMI) {
    state.abort_current_decl = true
  }
  if report_invalid_continue && state.current_is(TokenKind::TK_RPAREN) {
    report_trailing_recovery_eof(state)
  }
  if invalid_pub_start {
    match state.peek() {
      Some((RBRACE, start, end)) =>
        state.report_expected_tokens_at(
          RBRACE,
          mk_loc(start, end),
          top_level_decl_expected_tokens(),
        )
      _ => ()
    }
  }
  state.node_from_children(Expr_MissingExpr, children)
}

///|
fn report_promoted_anonymous_function(state : ParserState) -> Unit {
  match state.peek_significant() {
    Some((LPAREN, start, initial_end)) => {
      let mut recovery_end = initial_end
      let mut offset = 0
      let mut done = false
      while !done {
        match state.peek(offset~) {
          Some((tok, token_start, _)) if offset > 0 &&
            token_start.column() == 1 &&
            top_level_decl_can_start(tok) => done = true
          Some((EOF, _, _)) | None => done = true
          Some((COMMENT(_) | NEWLINE | SEMI(_), _, _)) => offset += 1
          Some((_, _, token_end)) => {
            recovery_end = token_end
            offset += 1
          }
        }
      }
      state.report_expected_tokens_at(LPAREN, mk_loc(start, recovery_end), [
        TokenKind::TK_LIDENT,
        TokenKind::TK_EXTEND,
        TokenKind::TK_PACKAGE_NAME,
      ])
      state.suppress_next_top_level_recovery_report = true
    }
    _ => ()
  }
}

///|
fn parse_expr_simple(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  match state.peek() {
    Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) =>
      state.node_from_children(Expr_MissingExpr, [])
    Some((LOOP, _, _)) | Some((THROW, _, _)) | Some((QUESTION, _, _)) =>
      parse_invalid_simple_expr(state, stop_kinds)
    Some((IF, _, _)) => parse_if_expr(state)
    Some((MATCH, _, _)) => parse_match_expr(state)
    Some((FORALL, _, _)) | Some((EXISTS, _, _)) =>
      parse_quantifier_expr(state, stop_kinds)
    Some((LET, _, _)) => parse_let_statement(state)
    Some((LETREC, _, _)) => parse_letrec_statement(state)
    Some((GUARD, _, _)) | Some((GUARD_EXCLAMATION, _, _)) =>
      parse_guard_statement(state)
    Some((DEFER, _, _)) | Some((ERRDEFER, _, _)) => parse_defer_statement(state)
    Some((PROOF_ASSERT, _, _)) => parse_proof_assert_statement(state)
    Some((PROOF_LET, _, _)) => parse_proof_let_statement(state)
    Some((TRY, _, _)) | Some((TRY_EXCLAMATION, _, _)) => parse_try_expr(state)
    Some((RETURN, _, _)) =>
      parse_keyword_value_expr(state, Expr_Return, stop_kinds)
    Some((RAISE, _, _)) =>
      parse_keyword_value_expr(state, Expr_Raise, stop_kinds)
    Some((BREAK, _, _)) => parse_break_expr(state, stop_kinds)
    Some((CONTINUE, _, _)) => parse_continue_expr(state, stop_kinds)
    Some((WHILE, _, _)) => parse_keyword_body_expr(state, Expr_While)
    Some((FOR, _, _)) => parse_for_expr(state)
    Some((LEXMATCH, _, _)) => parse_lex_block_expr(state, Expr_LexMatch)
    Some((LEXSCAN, _, _)) => parse_lex_block_expr(state, Expr_LexScan)
    Some((FN, _, _)) | Some((ASYNC, _, _)) =>
      parse_function_expr(state, stop_kinds)
    Some((LBRACE, _, _)) =>
      if brace_starts_record_expr(state) {
        parse_record_expr(state)
      } else if brace_starts_map_expr(state) {
        parse_map_expr(state)
      } else {
        parse_block_expr(state, classify_record_pun=true)
      }
    Some((LPAREN, _, _)) => parse_paren_expr(state, stop_kinds)
    Some((LBRACKET, _, _)) | Some((LBRACKET_BAR, _, _)) =>
      parse_array_expr(state)
    Some((AMPER, _, _)) =>
      match state.peek(offset=1) {
        Some((UIDENT(_), _, _)) | Some((PACKAGE_NAME(_), _, _)) =>
          parse_type_name_node(state)
        _ =>
          if state.consume_any() is Some(token) {
            state
            .node_from_children(Expr_Ident, [(Some("op"), token)])
            .with_structured_name("semantic_name", Name_Var, ["name"])
          } else {
            state.node_from_children(Expr_MissingExpr, [])
          }
      }
    Some((tok, _, _)) if tok is (ELLIPSIS | UNDERSCORE) =>
      if state.consume_any() is Some(token) {
        hole_node_from_children(
          state,
          [(Some("token"), token)],
          todo=tok is ELLIPSIS,
        )
      } else {
        hole_node_from_children(state, [])
      }
    Some((STRING(_), _, _))
    | Some((BYTES(_), _, _))
    | Some((REGEX_LITERAL(_), _, _))
    | Some((INTERP(_), _, _))
    | Some((BYTES_INTERP(_), _, _))
    | Some((MULTILINE_STRING(_), _, _))
    | Some((MULTILINE_INTERP(_), _, _)) => parse_string_literal_node(state)
    Some((REGEX_INTERP(_), _, _)) => parse_regex_interp_lex_pattern_node(state)
    Some((TRUE, _, _))
    | Some((FALSE, _, _))
    | Some((CHAR(_), _, _))
    | Some((INT(_), _, _))
    | Some((BYTE(_), _, _))
    | Some((DOUBLE(_), _, _))
    | Some((FLOAT(_), _, _)) =>
      parse_single_token_expr(state, Expr_Constant, "token")
    Some((LIDENT(_), _, _))
    | Some((EXTEND, _, _))
    | Some((PACKAGE_NAME(_), _, _)) => parse_ident_or_qualified_expr(state)
    Some((UIDENT(_), _, _)) =>
      parse_single_token_expr(state, Expr_Constr, "name")
    Some(_) => parse_invalid_simple_expr(state, stop_kinds)
    None => state.node_from_children(Expr_MissingExpr, [])
  }
}

///|
fn parse_keyword_value_expr(
  state : ParserState,
  kind : NodeKind,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let value_is_required = kind is Expr_Raise
  if value_is_required ||
    (
      !state.current_in(stop_kinds) &&
      !state.current_is(TokenKind::TK_SEMI) &&
      !state.current_is(TokenKind::TK_RBRACE) &&
      !state.current_is(TokenKind::TK_EOF)
    ) {
    let reports_before_expr = state.reports.length()
    let expr = parse_expr_until(state, stop_kinds)
    if value_is_required {
      push_child(
        children,
        Some("expr"),
        state.required_expr_node(expr, reports_before_expr),
      )
    } else if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
    }
  }
  state.node_from_children(kind, children)
}

///|
fn parse_break_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  match state.peek() {
    Some((POST_LABEL(_), _, _)) =>
      if state.consume_any() is Some(label) {
        push_child(
          children,
          Some("label_loc"),
          token_location_node(label.kind, label.loc, label.source_span),
        )
        push_child(children, Some("label"), label)
      } else {
        ()
      }
    Some((POST_LABEL_TRANSPARENT(name), _, _)) =>
      if state.consume_any() is Some(label) {
        push_child(
          children,
          Some("label_loc"),
          token_location_node(label.kind, label.loc, label.source_span),
        )
        push_child(children, Some("label"), transparent_label_node(label, name))
      } else {
        ()
      }
    _ => ()
  }
  if !state.current_in(stop_kinds) &&
    !state.current_is(TokenKind::TK_SEMI) &&
    !state.current_is(TokenKind::TK_RBRACE) &&
    !state.current_is(TokenKind::TK_EOF) {
    let expr = parse_expr_until(state, stop_kinds)
    if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
    }
  }
  state.node_from_children(Expr_Break, children)
}

///|
fn parse_continue_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut anchor : CstNode? = None
  if state.consume_any() is Some(kw) {
    anchor = Some(kw)
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  match state.peek() {
    Some((POST_LABEL(_), _, _)) if !(state.peek(offset=1).map(item => item.0)
      is Some(COLON)) =>
      if state.consume_any() is Some(label) {
        anchor = Some(label)
        push_child(
          children,
          Some("label_loc"),
          token_location_node(label.kind, label.loc, label.source_span),
        )
        push_child(children, Some("label"), label)
      } else {
        ()
      }
    Some((POST_LABEL_TRANSPARENT(name), _, _)) if !(state
      .peek(offset=1)
      .map(item => item.0)
      is Some(COLON)) =>
      if state.consume_any() is Some(label) {
        anchor = Some(label)
        push_child(
          children,
          Some("label_loc"),
          token_location_node(label.kind, label.loc, label.source_span),
        )
        push_child(children, Some("label"), transparent_label_node(label, name))
      } else {
        ()
      }
    _ => ()
  }
  let has_same_line_arg = if anchor is Some(item) {
    !state.current_starts_after_node_line(item) &&
    !state.current_is(TokenKind::TK_NEWLINE)
  } else {
    false
  }
  if has_same_line_arg &&
    !state.current_in(stop_kinds) &&
    !state.current_is(TokenKind::TK_SEMI) &&
    !state.current_is(TokenKind::TK_RBRACE) &&
    !state.current_is(TokenKind::TK_EOF) {
    let args = parse_continue_argument_list(state, stop_kinds)
    if !args.children.is_empty() {
      push_child(children, Some("arg"), args)
    }
  }
  state.node_from_children(Expr_Continue, children)
}

///|
fn labelled_loop_prefix_starts(state : ParserState) -> Bool {
  match (state.peek(), state.peek(offset=1)) {
    (Some((POST_LABEL(_), _, _)), Some((COLON, _, _))) => {
      let mut offset = 2
      let mut done = false
      while !done {
        match state.peek(offset~) {
          Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
          Some((LBRACE, _, _)) | Some((FOR, _, _)) | Some((WHILE, _, _)) =>
            return true
          _ => done = true
        }
      }
      false
    }
    _ => false
  }
}

///|
fn transparent_label_node(label : CstNode, name : String) -> CstNode {
  node(Name_Label, label.loc, label.source_span, [
    (Some("syntax"), label),
    (
      Some("name_value"),
      scalar_node(Name_Label, label.loc, label.source_span, String("\{name}~")),
    ),
  ])
}

///|
fn parse_labelled_loop_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let transparent_name = match state.peek() {
    Some((POST_LABEL_TRANSPARENT(name), _, _)) => Some(name)
    _ => None
  }
  if state.consume_any() is Some(label) {
    let semantic_label = match transparent_name {
      Some(name) => transparent_label_node(label, name)
      None => label
    }
    push_child(children, Some("label"), semantic_label)
    push_child(
      children,
      Some("label_loc"),
      token_location_node(label.kind, label.loc, label.source_span),
    )
  } else {
    ()
  }
  if state.consume_any() is Some(op) {
    push_child(children, Some("op"), op)
  } else {
    ()
  }
  push_children(children, state.consume_trivia())
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
    return state.node_from_children(Expr_LabelledBlock, children)
  }
  let loop_node = match state.peek() {
    Some((FOR, _, _)) if transparent_name is None => parse_for_expr(state)
    Some((WHILE, _, _)) if transparent_name is None =>
      parse_keyword_body_expr(state, Expr_While)
    Some((found, start, end)) => {
      let expected : Array[TokenKind] = if transparent_name is Some(_) {
        [TK_LBRACE]
      } else {
        [TK_LBRACE, TK_FOR, TK_WHILE]
      }
      state.report_expected_tokens_at(found, mk_loc(start, end), expected)
      let invalid : Array[(String?, CstNode)] = []
      while !state.current_in(stop_kinds) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(
            invalid,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          ()
        }
      }
      state.node_from_children(Expr_MissingExpr, invalid)
    }
    None => state.node_from_children(Expr_MissingExpr, [])
  }
  push_child(
    children,
    Some("loop_loc"),
    aggregate_span_node(loop_node.loc, loop_node.source_span),
  )
  for i in 0..<loop_node.children.length() {
    if loop_node.children.get(i) is Some(child) {
      children.push(child)
    } else {
      ()
    }
  }
  state.node_from_children(loop_node.kind, children)
}

///|
fn parse_continue_argument_list(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let item_stops = stop_kinds.copy()
  item_stops.push(TokenKind::TK_COMMA)
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((SEMI(_), _, _)) | Some((RBRACE, _, _)) | Some((EOF, _, _)) | None =>
        done = true
      Some((COMMA, _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          done = true
        }
      Some(_) => {
        let expr = parse_expr_until(state, item_stops)
        if !expr.children.is_empty() {
          push_child(children, Some("item"), expr)
        }
        push_children(children, state.consume_trivia())
        if state.current_is(TokenKind::TK_COMMA) {
          if state.consume_any() is Some(sep) {
            push_child(children, Some("sep"), sep)
          } else {
            ()
          }
        }
      }
    }
  }
  state.node_from_children(List_ContinueArgumentList, children)
}

///|
fn discard_expr_trivia(state : ParserState) -> Unit {
  match state.peek() {
    Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => {
      let _ = state.consume_trivia()
    }
    _ => ()
  }
}

///|
fn parse_single_token_expr(
  state : ParserState,
  kind : NodeKind,
  label : String,
) -> CstNode {
  match state.peek() {
    Some((raw, _, _)) =>
      if state.consume_any() is Some(token) {
        let children : Array[(String?, CstNode)] = [(Some(label), token)]
        if kind is Expr_Constant &&
          constant_node_from_token(raw, token.loc, token.source_span)
          is Some(constant) {
          push_child(children, Some("constant"), constant)
        }
        let expr = state.node_from_children(kind, children)
        match kind {
          Expr_Constr =>
            expr
            .with_structured_name("semantic_name", Name_Constructor, [
              "pkg", "type", "name", "field",
            ])
            .with_constructor_extra_info()
          Expr_Ident =>
            expr.with_structured_name("semantic_name", Name_Var, ["name"])
          _ => expr
        }
      } else {
        state.node_from_children(Expr_MissingExpr, [])
      }
    None => state.node_from_children(Expr_MissingExpr, [])
  }
}

///|
fn parse_ident_or_qualified_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let allow_qualified = match state.peek() {
    Some((PACKAGE_NAME(_), _, _)) => true
    _ => false
  }
  if state.consume_any() is Some(first) {
    push_child(children, Some("name"), first)
  } else {
    ()
  }
  match state.peek() {
    Some((DOT_LIDENT(_), _, _)) if allow_qualified => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
      state
      .node_from_children(Expr_Ident, children)
      .with_structured_name("semantic_name", Name_Var, ["name"])
    }
    Some((DOT_UIDENT(_), _, _)) if allow_qualified => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
      state
      .node_from_children(Expr_Constr, children)
      .with_structured_name("semantic_name", Name_Constructor, [
        "pkg", "type", "name", "field",
      ])
      .with_constructor_extra_info()
    }
    _ =>
      state
      .node_from_children(Expr_Ident, children)
      .with_structured_name("semantic_name", Name_Var, ["name"])
  }
}

///|
fn parse_type_name_node(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut is_object = false
  if state.current_is(TokenKind::TK_AMPER) {
    is_object = true
    if state.consume_any() is Some(op) {
      push_child(children, Some("op"), op)
    } else {
      ()
    }
  }
  match state.peek() {
    Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    Some((LIDENT(name), start, end)) => {
      state.report(
        mk_loc(start, end),
        "Unexpected lowercase identifier \{name}, expected uppercase identifier in type name.",
      )
      if state.consume_any() is Some(bad) {
        push_child(
          children,
          Some("name"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
    }
    Some((PACKAGE_NAME(_), _, _)) => {
      if state.consume_any() is Some(pkg) {
        push_child(children, Some("name"), pkg)
      } else {
        ()
      }
      match state.peek() {
        Some((DOT_LIDENT(_), _, _)) | Some((DOT_UIDENT(_), _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("name"), name)
          } else {
            ()
          }
        _ =>
          push_child(
            children,
            Some("name"),
            state.missing_token_node_with_expected(TokenKind::TK_UIDENT, [
              TokenKind::TK_DOT_LIDENT,
              TokenKind::TK_DOT_UIDENT,
            ]),
          )
      }
    }
    _ =>
      push_child(
        children,
        Some("name"),
        state.missing_token_node_with_expected(TokenKind::TK_UIDENT, [
          TokenKind::TK_UIDENT,
          TokenKind::TK_PACKAGE_NAME,
        ]),
      )
  }
  state
  .node_from_children(Type_Name, children)
  .with_structured_name("semantic_name", Name_ConstrId, ["name"])
  .with_bool_marker("object", Flag_Object, is_object)
}

///|
fn parse_paren_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let arrow_params = !kind_in(TokenKind::TK_FAT_ARROW, stop_kinds) &&
    paren_expr_is_followed_by_fat_arrow(state)
  if !arrow_params && paren_expr_has_top_level_constraint(state) {
    return parse_constraint_paren_expr(state)
  }
  let list = if arrow_params {
    parse_delimited(
      state,
      TokenKind::TK_LPAREN,
      TokenKind::TK_RPAREN,
      parse_arrow_parameter_expr_item,
      List_TupleElementList,
      sep_kind=TokenKind::TK_COMMA,
      item_expected="simple expression",
    )
  } else {
    parse_delimited(
      state,
      TokenKind::TK_LPAREN,
      TokenKind::TK_RPAREN,
      parse_tuple_expr_item,
      List_TupleElementList,
      sep_kind=TokenKind::TK_COMMA,
    )
  }
  if arrow_params {
    for entry in list.children {
      match entry {
        (Some("item"), item) =>
          if invalid_arrow_parameter_expr(item) is Some(invalid) {
            state.report(
              invalid.loc,
              "Unexpected expression. You may expect parameter here.",
            )
          } else {
            ()
          }
        _ => ()
      }
    }
  }
  let item_count = list.children_with_label("item").length()
  let kind = if item_count == 0 {
    Expr_Unit
  } else if item_count == 1 {
    Expr_Group
  } else {
    Expr_Tuple
  }
  let children : Array[(String?, CstNode)] = [(Some("body"), list)]
  if kind is Expr_Unit {
    push_child(
      children,
      Some("faked"),
      bool_marker_node(Flag_Faked, list.loc.end, list.source_span.1, false),
    )
  } else if kind is Expr_Group {
    push_child(
      children,
      Some("group"),
      marker_node(Group_Paren, list.loc.start, list.source_span.0),
    )
  }
  node(kind, list.loc, list.source_span, children)
}

///|
fn parse_arrow_parameter_expr_item(state : ParserState) -> CstNode {
  let expr = parse_tuple_expr_item(state)
  if !state.current_is(TokenKind::TK_COLON) {
    return expr
  }
  let children : Array[(String?, CstNode)] = [(Some("expr"), expr)]
  if state.consume_any() is Some(colon) {
    push_child(children, Some("op"), colon)
  } else {
    ()
  }
  let reports_before_type = state.reports.length()
  let ty = parse_type_arrow(state, [
    TokenKind::TK_COMMA,
    TokenKind::TK_RPAREN,
    TokenKind::TK_EOF,
  ])
  push_child(
    children,
    Some("type"),
    state.required_type_node(ty, reports_before_type),
  )
  state.node_from_children(Expr_Constraint, children)
}

///|
fn paren_expr_is_followed_by_fat_arrow(state : ParserState) -> Bool {
  let mut offset = 1
  let mut depth = 0
  while true {
    match state.peek(offset~) {
      Some((LPAREN, _, _))
      | Some((LBRACKET, _, _))
      | Some((LBRACE, _, _))
      | Some((LBRACKET_BAR, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RPAREN, _, _)) if depth == 0 => {
        offset += 1
        while true {
          match state.peek(offset~) {
            Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
            Some((FAT_ARROW, _, _)) => return true
            _ => return false
          }
        }
      }
      Some((RPAREN, _, _))
      | Some((RBRACKET, _, _))
      | Some((RBRACE, _, _))
      | Some((BAR_RBRACKET, _, _)) => {
        if depth > 0 {
          depth -= 1
        }
        offset += 1
      }
      Some((EOF, _, _)) | None => return false
      Some(_) => offset += 1
    }
  }
  false
}

///|
fn parse_tuple_expr_item(state : ParserState) -> CstNode {
  let item_stops = [
    TokenKind::TK_COLON,
    TokenKind::TK_COMMA,
    TokenKind::TK_RPAREN,
    TokenKind::TK_EOF,
  ]
  if state.current_argument_starts_optional_forwarding() {
    parse_optional_forwarding_argument(state)
  } else if state.current_argument_starts_labelled() {
    parse_labelled_argument(state)
  } else {
    match state.peek_significant() {
      Some((tok, _, _)) if statement_only_expression_token(tok) =>
        parse_invalid_simple_expr(state, item_stops)
      _ => {
        let reports_before_expr = state.reports.length()
        state.required_expr_node(
          parse_expr_until(
            state,
            item_stops,
            allow_argument_label_question=true,
          ),
          reports_before_expr,
        )
      }
    }
  }
}

///|
fn paren_expr_has_top_level_constraint(state : ParserState) -> Bool {
  let mut offset = 1
  let mut depth = 0
  let mut saw_colon = false
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((POST_LABEL(_), _, _)) | Some((POST_LABEL_TRANSPARENT(_), _, _)) if depth ==
        0 &&
        state.peek(offset=offset + 1).map(item => item.0) is Some(COLON) =>
        // This colon starts a labelled expression; it is part of the
        // expression rather than the `expr : Type` separator sought here.
        offset += 2
      Some((LPAREN, _, _))
      | Some((LBRACKET, _, _))
      | Some((LBRACE, _, _))
      | Some((LBRACKET_BAR, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RPAREN, _, _)) =>
        if depth == 0 {
          return saw_colon
        } else {
          depth -= 1
          offset += 1
        }
      Some((RBRACKET, _, _))
      | Some((BAR_RBRACKET, _, _))
      | Some((RBRACE, _, _)) => {
        if depth > 0 {
          depth -= 1
        }
        offset += 1
      }
      Some((COMMA, _, _)) if depth == 0 => return false
      Some((COLON, _, _)) if depth == 0 => {
        saw_colon = true
        offset += 1
      }
      Some((EOF, _, _)) | None => done = true
      Some(_) => offset += 1
    }
  }
  false
}

///|
fn parse_constraint_paren_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(open) {
    push_child(children, Some("open"), open)
  } else {
    ()
  }
  let expr = parse_expr_until(state, [
    TokenKind::TK_COLON,
    TokenKind::TK_RPAREN,
    TokenKind::TK_EOF,
  ])
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
  }
  let ty = parse_type_until(state, [TokenKind::TK_RPAREN, TokenKind::TK_EOF])
  if !ty.children.is_empty() {
    push_child(children, Some("type"), ty)
  }
  push_children(children, state.consume_trivia())
  push_child(children, Some("close"), state.expect_token(TokenKind::TK_RPAREN))
  state.node_from_children(Expr_Constraint, children)
}

///|
fn parse_array_expr(state : ParserState) -> CstNode {
  let open_kind = match state.peek() {
    Some((LBRACKET_BAR, _, _)) => TokenKind::TK_LBRACKET_BAR
    _ => TokenKind::TK_LBRACKET
  }
  let close_kind = match open_kind {
    TokenKind::TK_LBRACKET_BAR => TokenKind::TK_BAR_RBRACKET
    _ => TokenKind::TK_RBRACKET
  }
  if array_starts_list_comprehension(state) {
    return parse_list_comprehension_expr(state, open_kind, close_kind)
  }
  let list = parse_delimited(
    state,
    open_kind,
    close_kind,
    parse_array_expr_item,
    List_ArrayElementList,
    sep_kind=TokenKind::TK_COMMA,
  )
  let kind = if list_has_item_kind(list, ArrayElement_Spread) {
    Expr_ArraySpread
  } else {
    Expr_Array
  }
  let children : Array[(String?, CstNode)] = [(Some("body"), list)]
  push_child(
    children,
    Some("array_kind"),
    marker_node(
      if open_kind is TK_LBRACKET_BAR {
        ArrayKind_Iter
      } else {
        ArrayKind_Array
      },
      list.loc.start,
      list.source_span.0,
    ),
  )
  node(kind, list.loc, list.source_span, children)
}

///|
fn array_starts_list_comprehension(state : ParserState) -> Bool {
  let mut offset = 1
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((FOR, _, _)) => return true
      _ => done = true
    }
  }
  false
}

///|
fn parse_list_comprehension_expr(
  state : ParserState,
  open_kind : TokenKind,
  close_kind : TokenKind,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_children(children, state.consume_trivia())
  push_child(children, Some("open"), state.expect_token(open_kind))
  push_children(children, state.consume_trivia())
  let for_loc = match state.peek() {
    Some((_, start, end)) => mk_loc(start, end)
    None => state.root_loc()
  }
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
    push_child(
      children,
      Some("for_loc"),
      token_location_node(kw.kind, kw.loc, kw.source_span),
    )
  } else {
    ()
  }
  let is_foreach = list_comprehension_is_foreach(state)
  let invalid_trailing_body = if is_foreach {
    parse_list_comprehension_foreach_header(state, children, close_kind)
  } else {
    parse_list_comprehension_for_header(state, children, close_kind, for_loc)
  }
  if invalid_trailing_body {
    let body = parse_invalid_list_comprehension_body(state, close_kind)
    if !body.children.is_empty() {
      push_child(children, Some("body"), body)
    }
  } else {
    if state.current_is(TK_IF) {
      if state.consume_any() is Some(kw) {
        push_child(children, Some("kw"), kw)
      } else {
        ()
      }
      let guard_expr = parse_expr_until(state, [
        TK_FAT_ARROW,
        close_kind,
        TK_EOF,
      ])
      if !guard_expr.children.is_empty() {
        push_child(children, Some("guard"), guard_expr)
      }
    }
    if state.current_is(TK_FAT_ARROW) {
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
    }
    let body = parse_expr_until(state, [close_kind, TK_EOF])
    if !body.children.is_empty() {
      push_child(children, Some("body"), body)
    }
  }
  push_children(children, state.consume_trivia())
  push_child(children, Some("close"), state.expect_token(close_kind))
  let result = with_marker_child(
    state.node_from_children(Expr_ListComprehension, children),
    "list_comprehension_kind",
    if is_foreach {
      ListComprehensionKind_ForEach
    } else {
      ListComprehensionKind_For
    },
  )
  with_marker_child(
    result,
    "array_kind",
    if open_kind is TokenKind::TK_LBRACKET_BAR {
      ArrayKind_Iter
    } else {
      ArrayKind_Array
    },
  )
}

///|
fn list_comprehension_is_foreach(state : ParserState) -> Bool {
  let mut offset = 0
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((IN, _, _)) => return true
      Some((COMMA, _, _)) => {
        offset += 1
        let mut skipping = true
        while skipping {
          match state.peek(offset~) {
            Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
            _ => skipping = false
          }
        }
        match state.peek(offset~) {
          Some((IN, _, _)) => return true
          Some((LIDENT(_), _, _))
          | Some((EXTEND, _, _))
          | Some((UNDERSCORE, _, _)) => offset += 1
          _ => return false
        }
      }
      Some((EQUAL, _, _))
      | Some((SEMI(_), _, _))
      | Some((FAT_ARROW, _, _))
      | Some((IF, _, _))
      | Some((EOF, _, _)) => done = true
      Some(_) => offset += 1
      None => done = true
    }
  }
  false
}

///|
fn list_comprehension_expr_stops(close_kind : TokenKind) -> Array[TokenKind] {
  [
    TokenKind::TK_SEMI,
    TokenKind::TK_NEWLINE,
    TokenKind::TK_IF,
    TokenKind::TK_FAT_ARROW,
    close_kind,
    TokenKind::TK_EOF,
  ]
}

///|
fn list_comprehension_binding_stops(close_kind : TokenKind) -> Array[TokenKind] {
  [
    TokenKind::TK_COMMA,
    TokenKind::TK_SEMI,
    TokenKind::TK_NEWLINE,
    TokenKind::TK_IF,
    TokenKind::TK_FAT_ARROW,
    close_kind,
    TokenKind::TK_EOF,
  ]
}

///|
fn ParserState::current_starts_after_node_line(
  self : ParserState,
  item : CstNode,
) -> Bool {
  match self.peek() {
    Some((_, start, _)) => start.lnum > item.loc.end.lnum
    None => false
  }
}

///|
fn ParserState::current_token_line(self : ParserState) -> Int? {
  match self.peek() {
    Some((_, start, _)) => Some(start.lnum)
    None => None
  }
}

///|
fn ParserState::current_starts_after_line(
  self : ParserState,
  line : Int,
) -> Bool {
  if self.current_token_line() is Some(current_line) {
    current_line > line
  } else {
    false
  }
}

///|
fn parse_list_comprehension_foreach_header(
  state : ParserState,
  children : Array[(String?, CstNode)],
  close_kind : TokenKind,
) -> Bool {
  let pat = parse_foreach_pattern(state)
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  if state.current_is(TokenKind::TK_IN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
  }
  let expr = parse_expr_until(state, list_comprehension_expr_stops(close_kind))
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  if !state.current_is(TokenKind::TK_IF) &&
    !state.current_is(TokenKind::TK_FAT_ARROW) &&
    !state.current_is(close_kind) &&
    !state.current_is(TokenKind::TK_EOF) {
    if state.current_is(TokenKind::TK_SEMI) ||
      state.current_is(TokenKind::TK_NEWLINE) {
      consume_for_header_separator(state)
    }
    push_child(
      children,
      Some("init"),
      parse_list_comprehension_binding_list(state, close_kind),
    )
    if !state.current_is(TokenKind::TK_IF) &&
      !state.current_is(TokenKind::TK_FAT_ARROW) &&
      !state.current_is(close_kind) &&
      !state.current_is(TokenKind::TK_EOF) {
      if state.current_is(TK_SEMI) || state.current_is(TK_NEWLINE) {
        consume_for_header_separator(state)
      }
      push_child(
        children,
        Some("continue"),
        parse_list_comprehension_binding_list(state, close_kind),
      )
    }
  }
  let invalid_trailing_body = state.current_is(TK_SEMI) ||
    state.current_is(TK_NEWLINE)
  if invalid_trailing_body {
    state.report_expected_tokens([TK_COMMA, TK_IF, TK_FAT_ARROW])
    consume_for_header_separator(state)
  }
  invalid_trailing_body
}

///|
fn parse_list_comprehension_for_header(
  state : ParserState,
  children : Array[(String?, CstNode)],
  close_kind : TokenKind,
  for_loc : Location,
) -> Bool {
  let init = parse_list_comprehension_binding_list(state, close_kind)
  push_child(children, Some("init"), init)
  if init.children_with_label("item").is_empty() &&
    (state.current_is(TK_IF) || state.current_is(TK_FAT_ARROW)) {
    state.report(
      for_loc, "A list-comprehension `for` header cannot be empty; use `for ;;` for an infinite header.",
    )
  }
  if !state.current_is(TokenKind::TK_IF) &&
    !state.current_is(TokenKind::TK_FAT_ARROW) &&
    !state.current_is(close_kind) &&
    !state.current_is(TokenKind::TK_EOF) {
    if state.current_is(TokenKind::TK_SEMI) ||
      state.current_is(TokenKind::TK_NEWLINE) {
      consume_for_header_separator(state)
    }
    if !state.current_is(TokenKind::TK_SEMI) &&
      !state.current_is(TokenKind::TK_NEWLINE) &&
      !state.current_is(TokenKind::TK_IF) &&
      !state.current_is(TokenKind::TK_FAT_ARROW) &&
      !state.current_is(close_kind) &&
      !state.current_is(TokenKind::TK_EOF) {
      let cond_expr = parse_expr_infix(
        state,
        list_comprehension_expr_stops(close_kind),
      )
      if !cond_expr.children.is_empty() {
        push_child(children, Some("cond"), cond_expr)
      }
    }
    if !state.current_is(TokenKind::TK_IF) &&
      !state.current_is(TokenKind::TK_FAT_ARROW) &&
      !state.current_is(close_kind) &&
      !state.current_is(TokenKind::TK_EOF) {
      if state.current_is(TK_SEMI) || state.current_is(TK_NEWLINE) {
        consume_for_header_separator(state)
      }
      push_child(
        children,
        Some("continue"),
        parse_list_comprehension_binding_list(state, close_kind),
      )
    }
  }
  let invalid_trailing_body = state.current_is(TK_SEMI) ||
    state.current_is(TK_NEWLINE)
  if invalid_trailing_body {
    state.report_expected_tokens([TK_COMMA, TK_IF, TK_FAT_ARROW])
    consume_for_header_separator(state)
  }
  invalid_trailing_body
}

///|
fn parse_invalid_list_comprehension_body(
  state : ParserState,
  close_kind : TokenKind,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.current_is(TK_IF) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    }
    let cond = parse_expr_until(state, [TK_FAT_ARROW, close_kind, TK_EOF])
    if !cond.children.is_empty() {
      push_child(children, Some("cond"), cond)
    }
    state.report_expected_tokens([TK_LBRACE])
  } else {
    state.report_expected_description("simple expression")
  }
  while !state.current_is(close_kind) &&
        !state.current_is(TK_EOF) &&
        !(state.peek() is None) {
    if state.consume_any() is Some(error) {
      push_child(children, Some("error"), error)
    }
  }
  state.node_from_children(Error, children)
}

///|
fn parse_list_comprehension_binding_list(
  state : ParserState,
  close_kind : TokenKind,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_children(children, state.consume_trivia())
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), [
          TokenKind::TK_SEMI,
          TokenKind::TK_NEWLINE,
          TokenKind::TK_IF,
          TokenKind::TK_FAT_ARROW,
          close_kind,
          TokenKind::TK_EOF,
        ]) => done = true
      Some((COMMA, _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          done = true
        }
      Some(_) => {
        let item = parse_list_comprehension_binding(state, close_kind)
        push_child(children, Some("item"), item)
        push_children(children, state.consume_trivia())
        if state.current_is(TokenKind::TK_COMMA) {
          if state.consume_any() is Some(sep) {
            push_child(children, Some("sep"), sep)
          } else {
            ()
          }
        } else {
          done = true
        }
      }
      None => done = true
    }
  }
  state.node_from_children(List_ForEachBindingList, children)
}

///|
fn parse_list_comprehension_binding(
  state : ParserState,
  close_kind : TokenKind,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ =>
      push_child(
        children,
        Some("name"),
        state.missing_token_node(TokenKind::TK_LIDENT),
      )
  }
  if state.current_is(TokenKind::TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
  }
  let expr = parse_expr_until(
    state,
    list_comprehension_binding_stops(close_kind),
  )
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  state
  .node_from_children(Binding_ForEach, children)
  .with_structured_name("semantic_name", Name_Binder, ["name"])
}

///|
fn parse_array_expr_item(state : ParserState) -> CstNode {
  let stops = [
    TokenKind::TK_COMMA,
    TokenKind::TK_RBRACKET,
    TokenKind::TK_BAR_RBRACKET,
    TK_EOF,
  ]
  match state.peek() {
    Some((ELLIPSIS, _, _)) => parse_invalid_simple_expr(state, stops)
    Some((DOTDOT, _, _)) => {
      let children : Array[(String?, CstNode)] = []
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let reports_before_expr = state.reports.length()
      let expr = parse_expr_until(state, stops)
      push_child(
        children,
        Some("expr"),
        state.required_expr_node(expr, reports_before_expr),
      )
      state.node_from_children(ArrayElement_Spread, children)
    }
    _ => {
      report_statement_only_expression(state, "array element")
      let reports_before_expr = state.reports.length()
      let expr = state.required_expr_node(
        parse_expr_until(state, stops),
        reports_before_expr,
      )
      state.node_from_children(ArrayElement_Regular, [(Some("expr"), expr)])
    }
  }
}

///|
fn body_delimited_condition_stops(state : ParserState) -> Array[TokenKind] {
  match state.peek_significant() {
    Some((LBRACE, _, _)) => [TokenKind::TK_EOF]
    _ => [TokenKind::TK_LBRACE, TokenKind::TK_EOF]
  }
}

///|
fn recover_expr_until_statement_boundary(
  state : ParserState,
  children : Array[(String?, CstNode)],
  balance_groups? : Bool = true,
) -> Unit {
  let mut depth = 0
  let mut done = false
  while !done {
    match state.peek() {
      Some((SEMI(_) | RBRACE, _, _)) if depth == 0 => done = true
      Some((EOF, _, _)) | None => done = true
      Some((tok, _, _)) =>
        if state.consume_any() is Some(bad) {
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
          if balance_groups {
            depth = next_depth(depth, tok)
          }
        } else {
          done = true
        }
    }
  }
}

///|
fn recover_to_required_block(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> Bool {
  while !state.current_is(TokenKind::TK_LBRACE) &&
        !state.current_is(TokenKind::TK_SEMI) &&
        !state.current_is(TokenKind::TK_RBRACE) &&
        !state.current_is(TokenKind::TK_EOF) &&
        !(state.peek() is None) {
    if state.consume_any() is Some(bad) {
      push_child(
        children,
        Some("error"),
        error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
      )
    } else {
      ()
    }
  }
  state.current_is(TokenKind::TK_LBRACE)
}

///|
fn report_trailing_recovery_eof(state : ParserState) -> Unit {
  let mut offset = 0
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_) | NEWLINE | SEMI(_), _, _))
      | Some((RPAREN | RBRACKET | RBRACE, _, _)) => offset += 1
      Some((EOF, start, end)) => {
        state.report(mk_loc(start, end), "Unexpected token here.")
        done = true
      }
      Some(_) | None => done = true
    }
  }
}

///|
fn report_next_statement_after_separator(state : ParserState) -> Unit {
  let mut offset = 0
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_) | NEWLINE | SEMI(_), _, _)) => offset += 1
      Some((RBRACE | EOF, _, _)) | None => done = true
      Some((_, start, end)) => {
        state.report(mk_loc(start, end), "Unexpected token here.")
        done = true
      }
    }
  }
}

///|
fn parse_if_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let reports_before_cond = state.reports.length()
  let cond = parse_expr_with_context_restriction(
    state,
    body_delimited_condition_stops(state),
    "condition",
    token_disallowed_in_condition,
  )
  push_child(
    children,
    Some("cond"),
    state.required_expr_node(cond, reports_before_cond),
  )
  let mut report_recovered_body_eof = false
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    state.report_expected_tokens([TokenKind::TK_LBRACE])
    report_recovered_body_eof = state.current_is(TokenKind::TK_LET) ||
      state.current_is(TokenKind::TK_LETREC)
    if recover_to_required_block(state, children) {
      push_child(children, Some("body"), parse_block_expr(state))
    } else {
      push_child(children, Some("body"), state.missing_expr_node())
    }
  }
  if report_recovered_body_eof {
    report_trailing_recovery_eof(state)
  }
  match (state.peek_significant(), state.peek_nth_significant(1)) {
    (Some((ELSE, _, _)), _) => push_children(children, state.consume_trivia())
    (Some((SEMI(false), _, _)), Some((ELSE, _, _))) => {
      push_children(children, state.consume_trivia())
      if state.consume_any() is Some(inserted) {
        push_child(children, Some("trivia"), inserted)
      } else {
        ()
      }
      push_children(children, state.consume_trivia())
    }
    _ => ()
  }
  if state.current_is(TokenKind::TK_ELSE) {
    if state.consume_any() is Some(else_kw) {
      push_child(children, Some("else"), else_kw)
    } else {
      ()
    }
    let reports_before_else = state.reports.length()
    let else_body = match state.peek() {
      Some((IF, _, _)) => parse_if_expr(state)
      Some((LBRACE, _, _)) => parse_block_expr(state)
      Some((found, start, end)) => {
        state.report_expected_tokens_at(found, mk_loc(start, end), [
          TokenKind::TK_LBRACE,
        ])
        parse_expr_until(state, [TokenKind::TK_SEMI, TokenKind::TK_EOF])
      }
      None => parse_expr_until(state, [TokenKind::TK_SEMI, TokenKind::TK_EOF])
    }
    push_child(
      children,
      Some("else"),
      state.required_expr_node(else_body, reports_before_else),
    )
  }
  state.node_from_children(Expr_If, children)
}

///|
fn parse_try_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut kind = Expr_Try
  let try_operator_kind = TryOperatorKind_Exclamation
  match state.peek() {
    Some((TRY_EXCLAMATION, _, _)) => kind = Expr_TryOperator
    _ => ()
  }
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let reports_before_body = state.reports.length()
  let try_body_stops = [
    TokenKind::TK_CATCH,
    TokenKind::TK_SEMI,
    TK_RBRACE,
    TokenKind::TK_EOF,
  ]
  let panic_body_stops = [TokenKind::TK_SEMI, TK_RBRACE, TK_EOF]
  let body = match (kind, state.peek_significant()) {
    (Expr_Try, Some((TRY_EXCLAMATION | LOOP, _, _))) =>
      parse_invalid_simple_expr(state, panic_body_stops)
    (Expr_TryOperator, Some((tok, _, _))) if token_disallowed_in_try_body(tok) =>
      parse_invalid_simple_expr(state, panic_body_stops)
    _ =>
      parse_expr_with_context_restriction(
        state, try_body_stops, "the try body", token_disallowed_in_try_body,
      )
  }
  let body = state.required_expr_node(body, reports_before_body)
  push_child(children, Some("body"), body)
  let mut saw_catch = false
  if state.current_is(TokenKind::TK_CATCH) {
    saw_catch = true
    if state.consume_any() is Some(catch_kw) {
      push_child(children, Some("kw"), catch_kw)
    } else {
      ()
    }
    if state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("case"), parse_case_block(state))
    } else {
      state.report_expected_tokens([TokenKind::TK_LBRACE])
      push_child(children, Some("case"), state.missing_expr_node())
    }
  }
  if state.current_is(TokenKind::TK_NORAISE) {
    if state.consume_any() is Some(else_kw) {
      push_child(children, Some("else_kw"), else_kw)
    } else {
      ()
    }
    if state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("else"), parse_case_block(state))
    }
  }
  if kind is Expr_Try &&
    !saw_catch &&
    state.reports.length() == reports_before_body {
    state.report_expected_tokens([TokenKind::TK_CATCH])
  }
  let result = state.node_from_children(kind, children)
  if kind is Expr_TryOperator {
    let result = with_marker_child(
      result, "try_operator_kind", try_operator_kind,
    )
    if result.children_with_label("kw").get(0) is Some(keyword) {
      result.with_token_location(
        "try_loc",
        keyword.kind,
        keyword.loc,
        keyword.source_span,
      )
    } else {
      result
    }
  } else {
    with_try_location_anchors(
      result.with_bool_marker("has_try", Flag_HasTry, true),
      true,
    )
  }
}

///|
fn parse_keyword_body_expr(state : ParserState, kind : NodeKind) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let reports_before_cond = state.reports.length()
  let cond = parse_expr_with_context_restriction(
    state,
    body_delimited_condition_stops(state),
    "condition",
    token_disallowed_in_condition,
  )
  push_child(
    children,
    Some("cond"),
    state.required_expr_node(cond, reports_before_cond),
  )
  let mut body_had_error = false
  if state.current_is(TokenKind::TK_LBRACE) {
    let reports_before_body = state.reports.length()
    push_child(children, Some("body"), parse_block_expr(state))
    body_had_error = state.reports.length() > reports_before_body
  } else {
    state.report_expected_tokens([TokenKind::TK_LBRACE])
    if recover_to_required_block(state, children) {
      let reports_before_body = state.reports.length()
      push_child(children, Some("body"), parse_block_expr(state))
      body_had_error = state.reports.length() > reports_before_body
    } else {
      push_child(children, Some("body"), state.missing_expr_node())
    }
  }
  if kind is Expr_While && body_had_error {
    if state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("error"), parse_block_expr(state))
    }
    report_next_statement_after_separator(state)
  }
  if state.current_is(TokenKind::TK_NOBREAK) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    if state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("else"), parse_block_expr(state))
    }
  }
  state.node_from_children(kind, children)
}

///|
fn parse_for_expr(state : ParserState) -> CstNode {
  match state.peek_nth_significant(1) {
    Some((SEMI(_) | LBRACE, _, _)) => parse_c_style_for_expr(state)
    Some((UNDERSCORE | IN, _, _)) => parse_foreach_expr(state)
    Some((LIDENT(_) | EXTEND, _, _)) =>
      match state.peek_nth_significant(2) {
        Some((EQUAL, _, _)) => parse_c_style_for_expr(state)
        Some((COMMA | IN, _, _)) => parse_foreach_expr(state)
        _ => parse_invalid_for_header(state, true)
      }
    _ => parse_invalid_for_header(state, false)
  }
}

///|
fn parse_invalid_for_header(state : ParserState, skip_binder : Bool) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  push_children(children, state.consume_trivia())
  if skip_binder {
    if state.consume_any() is Some(binder) {
      push_child(children, Some("error"), binder)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  match state.peek() {
    Some((found, start, end)) => {
      let expected = if skip_binder {
        [TokenKind::TK_EQUAL, TokenKind::TK_COMMA, TokenKind::TK_IN]
      } else {
        [
          TokenKind::TK_LIDENT,
          TokenKind::TK_EXTEND,
          TokenKind::TK_SEMI,
          TokenKind::TK_LBRACE,
        ]
      }
      state.report_expected_tokens_at(found, mk_loc(start, end), expected)
    }
    None => ()
  }
  let mut saw_header_separator = false
  let mut reported_missing_statement = false
  while !state.current_is(TokenKind::TK_LBRACE) &&
        !state.current_is(TokenKind::TK_EOF) &&
        !(state.peek() is None) {
    if state.current_is(TokenKind::TK_SEMI) {
      if saw_header_separator && !reported_missing_statement {
        state.report_expected_description("simple expression")
        reported_missing_statement = true
      }
      saw_header_separator = true
    }
    if state.consume_any() is Some(bad) {
      push_child(
        children,
        Some("error"),
        error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
      )
    } else {
      ()
    }
  }
  if state.current_is(TokenKind::TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    push_child(children, Some("body"), state.missing_expr_node())
  }
  state.node_from_children(Expr_For, children)
}

///|
fn parse_foreach_pattern(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut first_pattern : CstNode? = None
  let mut item_count = 0
  let mut saw_sep = false
  push_children(children, state.consume_trivia())
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), [
          TokenKind::TK_IN,
          TokenKind::TK_LBRACE,
          TokenKind::TK_EOF,
        ]) => done = true
      Some((COMMA, _, _)) => {
        saw_sep = true
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          done = true
        }
      }
      Some(_) => {
        match state.peek() {
          Some((LIDENT(_), _, _))
          | Some((EXTEND, _, _))
          | Some((UNDERSCORE, _, _)) => ()
          Some((found, start, end)) =>
            state.report_expected_tokens_at(found, mk_loc(start, end), [
              TK_LIDENT,
              TK_EXTEND,
              TokenKind::TK_SEMI,
              TokenKind::TK_LBRACE,
            ])
          None => ()
        }
        let pat = parse_pattern_until(state, [
          TokenKind::TK_COMMA,
          TokenKind::TK_IN,
          TokenKind::TK_LBRACE,
          TokenKind::TK_EOF,
        ])
        if !pat.children.is_empty() {
          if first_pattern is None {
            first_pattern = Some(pat)
          }
          item_count += 1
        }
        push_child(children, Some("item"), pat)
        push_children(children, state.consume_trivia())
        if state.current_is(TokenKind::TK_COMMA) {
          saw_sep = true
          if state.consume_any() is Some(sep) {
            push_child(children, Some("sep"), sep)
          } else {
            ()
          }
        }
      }
      None => done = true
    }
  }
  if item_count == 0 {
    state.report_expected_tokens([
      TokenKind::TK_LIDENT,
      TokenKind::TK_EXTEND,
      TokenKind::TK_UNDERSCORE,
    ])
  }
  if item_count == 1 && !saw_sep {
    if first_pattern is Some(pat) {
      pat
    } else {
      state.node_from_children(Pattern_MissingPattern, [])
    }
  } else {
    state.node_from_children(Pattern_Tuple, [
      (Some("body"), state.node_from_children(List_TuplePatternList, children)),
    ])
  }
}

///|
fn parse_foreach_iter_expr(state : ParserState) -> CstNode {
  if state.current_is(TokenKind::TK_LBRACE) &&
    !brace_starts_record_expr(state) &&
    !brace_starts_map_expr(state) &&
    !brace_expr_is_followed_by_range_operator(state) {
    state.node_from_children(Expr_MissingExpr, [])
  } else {
    parse_expr_until(state, foreach_iter_expr_stops())
  }
}

///|
fn foreach_iter_expr_stops() -> Array[TokenKind] {
  [
    TokenKind::TK_SEMI,
    TokenKind::TK_NOBREAK,
    TokenKind::TK_ELSE,
    TokenKind::TK_EOF,
  ]
}

///|
fn parse_foreach_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let pat = parse_foreach_pattern(state)
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  if state.current_is(TokenKind::TK_IN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    let expr = parse_foreach_iter_expr(state)
    if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
    }
  }
  if state.current_is(TokenKind::TK_SEMI) {
    consume_for_header_separator(state)
    push_child(children, Some("init"), parse_for_binding_list(state))
    if state.current_is(TokenKind::TK_SEMI) {
      consume_for_header_separator(state)
      push_child(children, Some("continue"), parse_for_binding_list(state))
      if state.current_is(TK_SEMI) || state.current_is(TK_NEWLINE) {
        state.report_expected_tokens([TK_COMMA, TK_LBRACE])
      }
    }
  }
  if state.current_is(TokenKind::TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  }
  if state.current_is(TokenKind::TK_NOBREAK) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    if state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("else"), parse_block_expr(state))
    }
  }
  if state.current_is(TokenKind::TK_WHERE) {
    push_child(children, Some("where"), parse_where_clause(state))
  }
  state.node_from_children(Expr_ForEach, children)
}

///|
fn for_clause_stops() -> Array[TokenKind] {
  [
    TokenKind::TK_COMMA,
    TokenKind::TK_SEMI,
    TokenKind::TK_NEWLINE,
    TokenKind::TK_LBRACE,
    TokenKind::TK_EOF,
  ]
}

///|
fn for_binding_expr_stops(state : ParserState) -> Array[TokenKind] {
  let mut start_offset = 0
  while state.peek(offset=start_offset) is Some((COMMENT(_) | NEWLINE, _, _)) {
    start_offset += 1
  }
  if state.peek(offset=start_offset) is Some((LBRACE, _, _)) &&
    (
      brace_starts_record_expr(state, start_offset~) ||
      brace_starts_map_expr(state, start_offset~)
    ) {
    [
      TokenKind::TK_COMMA,
      TokenKind::TK_SEMI,
      TokenKind::TK_NEWLINE,
      TokenKind::TK_EOF,
    ]
  } else {
    for_clause_stops()
  }
}

///|
fn for_header_stops() -> Array[TokenKind] {
  [
    TokenKind::TK_SEMI,
    TokenKind::TK_NEWLINE,
    TokenKind::TK_LBRACE,
    TK_RBRACE,
    TokenKind::TK_EOF,
  ]
}

///|
fn invalid_for_binding_series_has_underscore(state : ParserState) -> Bool {
  let mut offset = 0
  let mut depth = 0
  while true {
    match state.peek(offset~) {
      Some((UNDERSCORE, _, _)) => return true
      Some((LBRACE, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RBRACE, _, _)) if depth == 0 => return false
      Some((RBRACE, _, _)) => {
        depth -= 1
        offset += 1
      }
      Some((EOF, _, _)) | None => return false
      Some(_) => offset += 1
    }
  }
  false
}

///|
fn parse_for_binding(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut invalid_name = false
  let mut recover_invalid_statement_series = false
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    Some((UIDENT(name), start, end)) => {
      state.report(
        mk_loc(start, end),
        "Unexpected uppercase identifier \{name}, expected lowercase identifier in loop variable name of `for`.",
      )
      if state.consume_any() is Some(name_node) {
        push_child(children, Some("name"), name_node)
      } else {
        ()
      }
    }
    Some((found, start, end)) => {
      invalid_name = true
      recover_invalid_statement_series = found is LET &&
        invalid_for_binding_series_has_underscore(state)
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_LIDENT,
        TokenKind::TK_EXTEND,
      ])
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TokenKind::TK_LIDENT),
      )
    }
    None => invalid_name = true
  }
  if invalid_name {
    if recover_invalid_statement_series {
      let mut depth = 0
      let mut done = false
      while !done {
        match state.peek() {
          Some((RBRACE, _, _)) if depth == 0 => done = true
          Some((EOF, _, _)) | None => done = true
          Some((UNDERSCORE, start, end)) => {
            state.report_expected_tokens_at(UNDERSCORE, mk_loc(start, end), [
              TK_LIDENT,
              TK_EXTEND,
            ])
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            }
          }
          Some((LBRACE, _, _)) => {
            depth += 1
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            }
          }
          Some((RBRACE, _, _)) => {
            depth -= 1
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            }
          }
          Some(_) =>
            if state.consume_any() is Some(bad) {
              push_child(children, Some("error"), bad)
            }
        }
      }
      return state
        .node_from_children(Binding_For, children)
        .with_structured_name("semantic_name", Name_Binder, ["name"])
    }
    while !state.current_is(TokenKind::TK_COMMA) &&
          !state.current_is(TokenKind::TK_LBRACE) &&
          !state.current_is(TokenKind::TK_EOF) &&
          !(state.peek() is None) {
      if state.consume_any() is Some(bad) {
        push_child(
          children,
          Some("error"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
    }
    match state.peek() {
      Some((EOF, start, end)) =>
        state.report(mk_loc(start, end), "Unexpected token here.")
      _ => ()
    }
    return state
      .node_from_children(Binding_For, children)
      .with_structured_name("semantic_name", Name_Binder, ["name"])
  }
  if state.current_is(TokenKind::TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
  } else {
    state.report_expected_tokens([TokenKind::TK_EQUAL])
    while !state.current_in(for_clause_stops()) && !(state.peek() is None) {
      if state.consume_any() is Some(bad) {
        push_child(
          children,
          Some("error"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
    }
    return state
      .node_from_children(Binding_For, children)
      .with_structured_name("semantic_name", Name_Binder, ["name"])
  }
  let expr = parse_expr_until(state, for_binding_expr_stops(state))
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  state
  .node_from_children(Binding_For, children)
  .with_structured_name("semantic_name", Name_Binder, ["name"])
}

///|
fn parse_for_binding_list(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_children(children, state.consume_trivia())
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), for_header_stops()) =>
        done = true
      Some((COMMA, _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          done = true
        }
      Some(_) => {
        push_child(children, Some("item"), parse_for_binding(state))
        push_children(children, state.consume_trivia())
        if state.current_is(TokenKind::TK_COMMA) {
          if state.consume_any() is Some(sep) {
            push_child(children, Some("sep"), sep)
          } else {
            ()
          }
        } else if !state.current_in(for_header_stops()) &&
          !(state.peek() is None) {
          state.report_expected_tokens([
            TokenKind::TK_COMMA,
            TokenKind::TK_SEMI,
            TokenKind::TK_LBRACE,
          ])
          while !state.current_is(TokenKind::TK_COMMA) &&
                !state.current_in(for_header_stops()) &&
                !(state.peek() is None) {
            if state.consume_any() is Some(bad) {
              push_child(
                children,
                Some("error"),
                error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
              )
            } else {
              done = true
            }
          }
          if state.current_is(TokenKind::TK_COMMA) {
            if state.consume_any() is Some(sep) {
              push_child(children, Some("sep"), sep)
            } else {
              done = true
            }
          } else {
            done = true
          }
        }
      }
      None => done = true
    }
  }
  state.node_from_children(List_ForBindingList, children)
}

///|
fn consume_for_header_separator(state : ParserState) -> Unit {
  match state.peek() {
    Some((SEMI(_), _, _)) => {
      let _ = state.consume_any()
      let _ = state.consume_trivia()
    }
    Some((NEWLINE, _, _)) => {
      let _ = state.consume_trivia()
    }
    _ => ()
  }
}

///|
fn token_can_start_for_condition(tok : Token) -> Bool {
  match tok {
    TRUE
    | FALSE
    | INT(_)
    | FLOAT(_)
    | DOUBLE(_)
    | CHAR(_)
    | STRING(_)
    | REGEX_LITERAL(_)
    | MULTILINE_STRING(_)
    | MULTILINE_INTERP(_)
    | BYTE(_)
    | BYTES_INTERP(_)
    | INTERP(_)
    | PACKAGE_NAME(_)
    | ASYNC
    | FN
    | LIDENT(_)
    | EXTEND
    | UIDENT(_)
    | LPAREN
    | LBRACKET
    | LBRACKET_BAR
    | LBRACE
    | UNDERSCORE
    | EXCLAMATION
    | AMPER
    | IF
    | MATCH
    | PLUS
    | MINUS
    | LOOP
    | WHILE
    | FOR
    | POST_LABEL(_)
    | GUARD
    | GUARD_EXCLAMATION
    | TRY
    | DEFER
    | ERRDEFER
    | TRY_EXCLAMATION
    | FORALL
    | EXISTS
    | LEXSCAN => true
    _ => false
  }
}

///|
fn parse_for_condition(state : ParserState) -> CstNode {
  let first = state.peek().map(item => item.0)
  let cond = parse_expr_infix(state, [
    TokenKind::TK_SEMI,
    TokenKind::TK_NEWLINE,
    TokenKind::TK_EOF,
  ])
  match first {
    Some(tok) if token_disallowed_in_condition(tok) =>
      if unexpected_expr_or_statement_name(tok) is Some(actual) {
        state.report(cond.loc, "Unexpected \{actual} in condition")
      } else {
        ()
      }
    _ => ()
  }
  cond
}

///|
fn recover_for_missing_separator(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> Unit {
  while !state.current_is(TokenKind::TK_LBRACE) &&
        !state.current_is(TokenKind::TK_RBRACE) &&
        !state.current_is(TokenKind::TK_EOF) &&
        !(state.peek() is None) {
    if state.consume_any() is Some(bad) {
      push_child(
        children,
        Some("error"),
        error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
      )
    } else {
      ()
    }
  }
}

///|
fn parse_c_style_for_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  if state.current_is(TK_LBRACE) {
    match state.peek() {
      Some((_, start, end)) =>
        state.report(
          mk_loc(start, end),
          "Deprecated `for { ... }` syntax is not supported; use `for ;; { ... }`.",
        )
      None => ()
    }
  }
  let reports_before_init = state.reports.length()
  push_child(children, Some("init"), parse_for_binding_list(state))
  let init_had_error = state.reports.length() > reports_before_init
  if state.current_is(TokenKind::TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    let had_first_separator = state.current_is(TokenKind::TK_SEMI) ||
      state.current_is(TokenKind::TK_NEWLINE)
    consume_for_header_separator(state)
    let has_condition = had_first_separator &&
      (match state.peek() {
        Some((tok, _, _)) => token_can_start_for_condition(tok)
        None => false
      })
    if has_condition {
      let cond = parse_for_condition(state)
      if !cond.children.is_empty() {
        push_child(children, Some("cond"), cond)
      }
    }
    if init_had_error && state.current_is(TokenKind::TK_SEMI) {
      state.report_expected_description("simple expression")
    }
    if has_condition && state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("body"), parse_block_expr(state))
    } else if state.current_is(TokenKind::TK_SEMI) ||
      state.current_is(TokenKind::TK_NEWLINE) {
      consume_for_header_separator(state)
      push_child(children, Some("continue"), parse_for_binding_list(state))
      if state.current_is(TokenKind::TK_SEMI) {
        state.report_expected_tokens([TokenKind::TK_COMMA, TokenKind::TK_LBRACE])
      }
      if state.current_is(TokenKind::TK_LBRACE) {
        push_child(children, Some("body"), parse_block_expr(state))
      }
    } else if had_first_separator {
      state.report_expected_tokens([TokenKind::TK_SEMI])
      recover_for_missing_separator(state, children)
      if state.current_is(TokenKind::TK_LBRACE) {
        push_child(children, Some("body"), parse_block_expr(state))
      }
    }
  }
  if state.current_is(TokenKind::TK_NOBREAK) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    if state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("else"), parse_block_expr(state))
    }
  }
  if state.current_is(TokenKind::TK_WHERE) {
    push_child(children, Some("where"), parse_where_clause(state))
  }
  state.node_from_children(Expr_For, children)
}

///|
fn parse_where_clause(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  if state.current_is(TokenKind::TK_LBRACE) {
    push_child(
      children,
      Some("body"),
      parse_delimited(
        state,
        TokenKind::TK_LBRACE,
        TokenKind::TK_RBRACE,
        parse_where_field_item,
        List_WhereFieldList,
        sep_kind=TokenKind::TK_COMMA,
      ),
    )
  }
  state.node_from_children(Clause_Where, children)
}

///|
fn parse_where_field_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
  }
  let expr = parse_expr_until(state, [
    TokenKind::TK_COMMA,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ])
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  state
  .node_from_children(Field_Where, children)
  .with_structured_name("semantic_name", Name_Field, ["name"])
}

///|
fn parse_quantifier_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let quantifier_kind = if state.current_is(TokenKind::TK_EXISTS) {
    QuantifierKind_Exists
  } else {
    QuantifierKind_Forall
  }
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let ty = parse_type_arrow(state, [TokenKind::TK_COMMA, TokenKind::TK_EOF])
    if !ty.children.is_empty() {
      push_child(children, Some("type"), ty)
    }
  }
  if state.current_is(TokenKind::TK_COMMA) {
    if state.consume_any() is Some(sep) {
      push_child(children, Some("sep"), sep)
    } else {
      ()
    }
  }
  let body = parse_expr_until(state, stop_kinds)
  if !body.children.is_empty() {
    push_child(children, Some("body"), body)
  }
  with_marker_child(
    state.node_from_children(Expr_Quantifier, children),
    "quantifier_kind",
    quantifier_kind,
  )
}

///|
fn parse_lex_block_expr(state : ParserState, kind : NodeKind) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let expr = parse_expr_until(state, [
    TokenKind::TK_LBRACE,
    TokenKind::TK_WITH,
    TokenKind::TK_EOF,
  ])
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  if state.current_is(TokenKind::TK_WITH) {
    if state.consume_any() is Some(with_kw) {
      push_child(children, Some("kw"), with_kw)
    } else {
      ()
    }
    if state.consume_any() is Some(strategy) {
      push_child(children, Some("name"), strategy)
    } else {
      ()
    }
  }
  if state.current_is(TokenKind::TK_LBRACE) {
    push_child(children, Some("case"), parse_lexscan_case_block(state))
  }
  state.node_from_children(kind, children)
}

///|
fn parse_lexmatch_case_block(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("open"), state.expect_token(TokenKind::TK_LBRACE))
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((RBRACE, _, _)) => {
        push_child(
          children,
          Some("close"),
          state.expect_token(TokenKind::TK_RBRACE),
        )
        done = true
      }
      Some((EOF, _, _)) | None => {
        push_child(
          children,
          Some("close"),
          state.missing_token_node(TokenKind::TK_RBRACE),
        )
        done = true
      }
      Some((SEMI(_), _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          ()
        }
      Some((COMMA, _, _)) =>
        if state.consume_error_for_progress() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      _ => push_child(children, Some("case"), parse_lexmatch_case(state))
    }
  }
  state.node_from_children(List_LexCaseList, children)
}

///|
fn lex_top_pattern_marker_kind(node : CstNode) -> NodeKind {
  match node.kind {
    Pattern_Tuple if node.has_child_kind(Pattern_Group) =>
      match node.first_child_with_label("body") {
        Some(body) =>
          match body.children_with_label("item").get(0) {
            Some(item) => lex_top_pattern_marker_kind(item)
            None => LexTopPattern_Wildcard
          }
        None => LexTopPattern_Wildcard
      }
    Pattern_Var => LexTopPattern_Binder
    Pattern_Any | Pattern_MissingPattern => LexTopPattern_Wildcard
    _ => LexTopPattern_Pattern
  }
}

///|
fn with_lex_top_pattern_marker(node : CstNode) -> CstNode {
  with_marker_child(node, "lex_top_pattern", lex_top_pattern_marker_kind(node))
}

///|
fn parse_lex_top_pattern_item(state : ParserState) -> CstNode {
  with_lex_top_pattern_marker(
    match state.peek() {
      Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
        parse_single_token_pattern(state, Pattern_Var)
      Some((UNDERSCORE, _, _)) => parse_single_token_pattern(state, Pattern_Any)
      _ =>
        parse_lex_pattern_until(state, [
          TokenKind::TK_COMMA,
          TokenKind::TK_RPAREN,
          TokenKind::TK_EOF,
        ])
    },
  )
}

///|
fn parse_lexmatch_case_pattern(state : ParserState) -> CstNode {
  let stop_kinds = [
    TokenKind::TK_IF,
    TokenKind::TK_FAT_ARROW,
    TokenKind::TK_SEMI,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ]
  match state.peek() {
    Some((LPAREN, _, _)) => {
      let list = parse_delimited(
        state,
        TokenKind::TK_LPAREN,
        TokenKind::TK_RPAREN,
        parse_lex_top_pattern_item,
        List_LexTopPatternList,
        sep_kind=TokenKind::TK_COMMA,
        require_nonempty=true,
      )
      let tuple = state.node_from_children(Pattern_Tuple, [(Some("body"), list)])
      if list.has_child_label("sep") {
        tuple
      } else {
        with_marker_child(tuple, "pattern_group", Pattern_Group)
      }
    }
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      parse_single_token_pattern(state, Pattern_Var)
    Some((UNDERSCORE, _, _)) => parse_single_token_pattern(state, Pattern_Any)
    Some((tok, _, _)) if lex_pattern_atom_can_start(tok) =>
      parse_lex_pattern_until(state, stop_kinds, allow_alias=false)
    Some((tok, _, _)) => {
      state.report_expected_description("lex pattern")
      if kind_in(tok.kind(), stop_kinds) {
        state.missing_pattern_node()
      } else if state.consume_any() is Some(bad) {
        state.node_from_children(Pattern_MissingPattern, [
          (
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          ),
        ])
      } else {
        state.missing_pattern_node()
      }
    }
    None => {
      state.report_expected_description("lex pattern")
      state.missing_pattern_node()
    }
  }
}

///|
fn parse_lexmatch_case(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.current_is(TokenKind::TK_ELLIPSIS) {
    if state.consume_any() is Some(todo) {
      let wildcard = with_marker_child(
        state.node_from_children(Pattern_MissingPattern, [(Some("token"), todo)]),
        "lex_top_pattern",
        LexTopPattern_Wildcard,
      )
      push_child(children, Some("pattern"), wildcard)
      push_child(
        children,
        Some("body"),
        hole_node_from_children(state, [(Some("token"), todo)], todo=true),
      )
    } else {
      ()
    }
    return state.node_from_children(Case_LexCase, children)
  }
  let parsed_pat = parse_lexmatch_case_pattern(state)
  let pat = with_lex_top_pattern_marker(parsed_pat)
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  if state.current_is(TokenKind::TK_IF) {
    if state.consume_any() is Some(if_token) {
      push_child(children, Some("kw"), if_token)
    } else {
      ()
    }
    let guard_expr = parse_pattern_guard_expr(state, [
      TokenKind::TK_FAT_ARROW,
      TokenKind::TK_EOF,
    ])
    if !guard_expr.children.is_empty() {
      push_child(children, Some("cond"), guard_expr)
    }
  }
  let reports_before_arrow = state.reports.length()
  push_child(children, Some("op"), state.expect_token(TokenKind::TK_FAT_ARROW))
  if state.reports.length() > reports_before_arrow {
    while !state.current_is(TokenKind::TK_SEMI) &&
          !state.current_is(TokenKind::TK_RBRACE) &&
          !state.current_is(TokenKind::TK_EOF) &&
          !(state.peek() is None) {
      if state.consume_any() is Some(bad) {
        push_child(
          children,
          Some("error"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
    }
  } else {
    let body = parse_expr_until(state, [
      TokenKind::TK_SEMI,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ])
    if !body.children.is_empty() {
      push_child(children, Some("body"), body)
    }
  }
  state.node_from_children(Case_LexCase, children)
}

///|
fn parse_lexscan_case_block(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("open"), state.expect_token(TokenKind::TK_LBRACE))
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((RBRACE, _, _)) => {
        push_child(
          children,
          Some("close"),
          state.expect_token(TokenKind::TK_RBRACE),
        )
        done = true
      }
      Some((EOF, _, _)) | None => {
        push_child(
          children,
          Some("close"),
          state.missing_token_node(TokenKind::TK_RBRACE),
        )
        done = true
      }
      Some((SEMI(_), _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          ()
        }
      Some((_, _, _)) =>
        push_child(children, Some("case"), parse_lexscan_case(state))
    }
  }
  state.node_from_children(List_LexScanCaseList, children)
}

///|
fn parse_lexscan_case_pattern(state : ParserState) -> CstNode {
  let stops = [
    TokenKind::TK_FAT_ARROW,
    TokenKind::TK_SEMI,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ]
  match state.peek() {
    Some((UNDERSCORE, _, _)) | Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      parse_pattern_until(state, stops)
    _ => parse_regex_as_pattern(state, stops)
  }
}

///|
fn lexscan_paren_starts_bindings(state : ParserState) -> Bool {
  let mut depth = 0
  let mut offset = 0
  while true {
    match state.peek(offset~) {
      Some((LPAREN, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RPAREN, _, _)) => {
        if depth <= 1 {
          return false
        }
        depth -= 1
        offset += 1
      }
      Some((COMMA, _, _)) if depth == 1 => return true
      Some((EOF, _, _)) | None => return false
      Some(_) => offset += 1
    }
  }
  false
}

///|
fn parse_lexscan_case(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((ELLIPSIS, _, _)) => {
      if state.consume_any() is Some(todo) {
        let wildcard = with_marker_child(
          state.node_from_children(Pattern_MissingPattern, [
            (Some("token"), todo),
          ]),
          "lex_scan_case_pattern",
          LexScanCasePattern_Wildcard,
        )
        push_child(children, Some("pattern"), wildcard)
        push_child(
          children,
          Some("body"),
          hole_node_from_children(state, [(Some("token"), todo)], todo=true),
        )
      } else {
        ()
      }
      return state.node_from_children(Case_LexScanCase, children)
    }
    _ => ()
  }
  let parsed_pat = if state.current_is(TK_LPAREN) &&
    lexscan_paren_starts_bindings(state) {
    parse_regex_match_rhs(state, [TK_FAT_ARROW, TK_SEMI, TK_RBRACE, TK_EOF])
  } else {
    parse_lexscan_case_pattern(state)
  }
  let pat = with_marker_child(
    parsed_pat,
    "lex_scan_case_pattern",
    match parsed_pat.kind {
      Pattern_Var => LexScanCasePattern_Binder
      Pattern_Any | Pattern_MissingPattern => LexScanCasePattern_Wildcard
      _ => LexScanCasePattern_Pattern
    },
  )
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  if state.current_is(TokenKind::TK_FAT_ARROW) {
    if state.consume_any() is Some(arrow) {
      push_child(children, Some("op"), arrow)
    } else {
      ()
    }
  }
  let body = parse_expr_with_context_restriction(
    state,
    [TokenKind::TK_SEMI, TokenKind::TK_RBRACE, TokenKind::TK_EOF],
    "the action part of a matching case",
    token_disallowed_in_match_action,
  )
  if !body.children.is_empty() {
    push_child(children, Some("body"), body)
  }
  state.node_from_children(Case_LexScanCase, children)
}

///|
fn parse_function_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.current_is(TokenKind::TK_ASYNC) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
  }
  if state.current_is(TokenKind::TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
  }
  if state.current_is(TokenKind::TK_EXCLAMATION) {
    if state.consume_error_for_progress() is Some(error) {
      push_child(children, Some("error"), error)
    } else {
      ()
    }
  }
  match state.peek() {
    Some((tok, start, end)) if !(tok is LPAREN) => {
      state.report(
        mk_loc(start, end),
        "Failed to parse func: expected '(' or '{', got \{@debug.render(tok.to_repr())}",
      )
      while !state.current_in(stop_kinds) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          ()
        }
      }
      return function_node_from_children(state, children, FnKind_Lambda)
    }
    _ => ()
  }
  let reports_before_signature = state.reports.length()
  if state.current_is(TokenKind::TK_LPAREN) {
    push_child(children, Some("params"), parse_parameter_list(state))
  }
  if state.current_is(TokenKind::TK_THIN_ARROW) ||
    state.current_is(TokenKind::TK_RAISE) ||
    state.current_is(TokenKind::TK_NORAISE) ||
    state.current_is(TokenKind::TK_NOCANCEL) {
    push_child(children, Some("type"), parse_func_return_type(state))
  }
  if state.current_is(TokenKind::TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    let body = if state.reports.length() > reports_before_signature {
      state.missing_token_node_silent(TokenKind::TK_LBRACE)
    } else {
      state.missing_token_node_with_expected(TokenKind::TK_LBRACE, [
        TokenKind::TK_LBRACE,
      ])
    }
    push_child(children, Some("body"), body)
  }
  function_node_from_children(state, children, FnKind_Lambda)
}

///|
fn parse_case_block(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("open"), state.expect_token(TokenKind::TK_LBRACE))
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((RBRACE, _, _)) => {
        push_child(
          children,
          Some("close"),
          state.expect_token(TokenKind::TK_RBRACE),
        )
        done = true
      }
      Some((EOF, _, _)) | None => {
        push_child(
          children,
          Some("close"),
          state.missing_token_node(TokenKind::TK_RBRACE),
        )
        done = true
      }
      Some((SEMI(_), _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          ()
        }
      Some((COMMA, _, _)) =>
        if state.consume_invalid_series_delimiter() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      Some((_, _, _)) =>
        push_child(children, Some("case"), parse_match_case(state))
    }
  }
  state.node_from_children(List_MatchCaseList, children)
}

///|
fn parse_match_case(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((ELLIPSIS, _, _)) => {
      if state.consume_any() is Some(todo) {
        push_child(
          children,
          Some("pattern"),
          state.node_from_children(Pattern_MissingPattern, [
            (Some("token"), todo),
          ]),
        )
        push_child(
          children,
          Some("body"),
          hole_node_from_children(state, [(Some("token"), todo)], todo=true),
        )
      } else {
        ()
      }
      return state.node_from_children(Case_MatchCase, children)
    }
    _ => ()
  }
  let reports_before_pattern = state.reports.length()
  let pat = parse_pattern_until(state, [
    TokenKind::TK_IF,
    TokenKind::TK_FAT_ARROW,
    TokenKind::TK_SEMI,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ])
  let pat = state.required_pattern_node(pat, reports_before_pattern)
  if !pat.children.is_empty() {
    if pat.kind is Pattern_Constraint {
      if pat.first_child_with_label("op") is Some(op) {
        state.report_expected_tokens_at(COLON, op.loc, [TokenKind::TK_FAT_ARROW])
      } else {
        ()
      }
    }
  }
  push_child(children, Some("pattern"), pat)
  let mut saw_guard = false
  if state.current_is(TokenKind::TK_IF) {
    saw_guard = true
    if state.consume_any() is Some(if_token) {
      push_child(children, Some("kw"), if_token)
    } else {
      ()
    }
    let guard_expr = parse_pattern_guard_expr(state, [
      TokenKind::TK_FAT_ARROW,
      TokenKind::TK_EOF,
    ])
    if !guard_expr.children.is_empty() {
      push_child(children, Some("cond"), guard_expr)
    }
  }
  let reports_before_body = reports_before_pattern
  let has_arrow = state.current_is(TokenKind::TK_FAT_ARROW)
  let arrow = if !has_arrow &&
    state.reports.length() > reports_before_pattern &&
    !saw_guard &&
    !pat.has_child_or_grandchild_kind(Pattern_With) {
    state.missing_token_node_silent(TokenKind::TK_FAT_ARROW)
  } else {
    state.expect_token(TokenKind::TK_FAT_ARROW)
  }
  push_child(children, Some("op"), arrow)
  if has_arrow {
    let body = parse_expr_with_context_restriction(
      state,
      [TokenKind::TK_SEMI, TokenKind::TK_RBRACE, TokenKind::TK_EOF],
      "the action part of a matching case",
      token_disallowed_in_match_action,
    )
    push_child(
      children,
      Some("body"),
      state.required_expr_node(body, reports_before_body),
    )
  } else {
    recover_expr_until_statement_boundary(state, children, balance_groups=false)
    push_child(children, Some("body"), state.missing_expr_node())
  }
  state.node_from_children(Case_MatchCase, children)
}

///|
fn parse_match_expr(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut has_open = false
  if state.consume_any() is Some(match_token) {
    push_child(children, Some("kw"), match_token)
  } else {
    ()
  }
  let reports_before_scrutinee = state.reports.length()
  let scrutinee = parse_expr_with_context_restriction(
    state,
    body_delimited_condition_stops(state),
    "condition",
    token_disallowed_in_condition,
  )
  push_child(
    children,
    Some("expr"),
    state.required_expr_node(scrutinee, reports_before_scrutinee),
  )
  match state.peek() {
    Some((LBRACE, _, _)) =>
      if state.consume_any() is Some(open) {
        has_open = true
        push_child(children, Some("open"), open)
      } else {
        ()
      }
    _ => {
      push_child(
        children,
        Some("open"),
        state.missing_token_node(TokenKind::TK_LBRACE),
      )
      while !state.current_is(TokenKind::TK_LBRACE) &&
            !state.current_is(TokenKind::TK_SEMI) &&
            !state.current_is(TokenKind::TK_RBRACE) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          ()
        }
      }
      if state.current_is(TokenKind::TK_LBRACE) {
        if state.consume_any() is Some(open) {
          has_open = true
          push_child(children, Some("open"), open)
        } else {
          ()
        }
      } else {
        return state.node_from_children(Expr_Match, children)
      }
    }
  }
  let reports_before_cases = state.reports.length()
  let mut saw_case = false
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((RBRACE, _, _)) => {
        if state.consume_any() is Some(close) {
          push_child(children, Some("close"), close)
        } else {
          ()
        }
        done = true
      }
      Some((EOF, _, _)) | None => {
        if has_open &&
          !saw_case &&
          state.reports.length() == reports_before_cases {
          push_child(children, Some("case"), parse_match_case(state))
          saw_case = true
        }
        if has_open {
          let close = if state.reports.length() == reports_before_cases &&
            saw_case {
            state.missing_token_node(TokenKind::TK_RBRACE)
          } else {
            state.missing_token_node_silent(TokenKind::TK_RBRACE)
          }
          push_child(children, Some("close"), close)
        }
        done = true
      }
      Some((SEMI(_), _, _)) =>
        if state.consume_any() is Some(sep) {
          push_child(children, Some("sep"), sep)
        } else {
          ()
        }
      Some((COMMA, _, _)) =>
        if state.consume_invalid_series_delimiter() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      _ => {
        push_child(children, Some("case"), parse_match_case(state))
        saw_case = true
      }
    }
  }
  state.node_from_children(Expr_Match, children)
}

///|
fn token_label_from_token(token : Token) -> String? {
  if token is SEMI(_) {
    Some("sep")
  } else {
    expr_token_kind_label(token.kind())
  }
}

///|
fn expr_token_kind_label(kind : TokenKind) -> String? {
  match kind {
    TK_TRY_QUESTION
    | TK_TRY_EXCLAMATION
    | TK_NORAISE
    | TK_NOCANCEL
    | TK_IS
    | TK_IN
    | TK_FOR
    | TK_DEFER
    | TK_ERRDEFER
    | TK_GUARD
    | TK_GUARD_EXCLAMATION
    | TK_RETURN
    | TK_WHILE
    | TK_THIN_ARROW
    | TK_FAT_ARROW
    | TK_PROOF_ASSERT
    | TK_PROOF_LET
    | TK_IF
    | TK_ELSE
    | TK_PIPE
    | TK_PIPE_LEFT
    | TK_LT_PLUS
    | TK_LT_QUESTION
    | TK_AS
    | TK_BARBAR
    | TK_CARET
    | TK_AMPER
    | TK_AMPERAMPER
    | TK_BAR
    | TK_PLUS
    | TK_EXCLAMATION
    | TK_QUESTION
    | TK_MINUS
    | TK_EQUAL
    | TK_EQ_TILDE
    | TK_CATCH
    | TK_TRY
    | TK_RAISE
    | TK_WITH
    | TK_CONTINUE
    | TK_BREAK
    | TK_AUGMENTED_ASSIGNMENT
    | TK_PLUS_EQUAL
    | TK_INFIX4
    | TK_INFIX3
    | TK_INFIX2
    | TK_INFIX1
    | TK_LEXMATCH
    | TK_LEXMATCH_QUESTION
    | TK_LEXSCAN
    | TK_RANGE_EXCLUSIVE
    | TK_RANGE_INCLUSIVE
    | TK_RANGE_EXCLUSIVE_REV
    | TK_RANGE_INCLUSIVE_REV
    | TK_RANGE_LT_INCLUSIVE
    | TK_WHERE
    | TK_NOBREAK
    | TK_FORALL
    | TK_EXISTS
    | TK_IMPLIES => Some("op")
    _ => Some("token")
  }
}

///|
fn parse_argument_item(state : ParserState) -> CstNode {
  let item_stops = [
    TokenKind::TK_COMMA,
    TokenKind::TK_RPAREN,
    TokenKind::TK_EOF,
  ]
  if state.current_argument_starts_optional_forwarding() {
    parse_optional_forwarding_argument(state)
  } else if state.current_argument_starts_labelled() {
    parse_labelled_argument(state)
  } else {
    match state.peek_significant() {
      Some((tok, _, _)) if statement_only_expression_token(tok) =>
        parse_invalid_simple_expr(state, item_stops)
      _ => {
        let reports_before_expr = state.reports.length()
        let expr = state.required_expr_node(
          parse_expr_until(
            state,
            item_stops,
            allow_argument_label_question=true,
          ),
          reports_before_expr,
        )
        state.node_from_children(Argument_Positional, [(Some("expr"), expr)])
      }
    }
  }
}

///|
fn ParserState::current_argument_starts_labelled(self : ParserState) -> Bool {
  match (self.peek(), self.peek(offset=1)) {
    (Some((LIDENT(_), _, _)), Some((EQUAL, _, _)))
    | (Some((EXTEND, _, _)), Some((EQUAL, _, _)))
    | (Some((UIDENT(_), _, _)), Some((EQUAL, _, _)))
    | (Some((POST_LABEL(_), _, _)), Some((EQUAL, _, _)))
    | (Some((POST_LABEL(_), _, _)), Some((COMMA, _, _)))
    | (Some((POST_LABEL(_), _, _)), Some((RPAREN, _, _)))
    | (Some((POST_LABEL(_), _, _)), Some((EOF, _, _))) => true
    (Some((POST_LABEL(_), _, _)), Some((COLON, _, _))) =>
      !labelled_loop_prefix_starts(self)
    _ => false
  }
}

///|
fn ParserState::current_argument_starts_optional_forwarding(
  self : ParserState,
) -> Bool {
  match (self.peek(), self.peek(offset=1)) {
    (Some((LIDENT(_), _, _)), Some((QUESTION, _, _)))
    | (Some((EXTEND, _, _)), Some((QUESTION, _, _)))
    | (Some((UIDENT(_), _, _)), Some((QUESTION, _, _))) => true
    _ => false
  }
}

///|
fn parse_labelled_argument(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let invalid_tilde_label_start = match (state.peek(), state.peek(offset=1)) {
    (Some((POST_LABEL(_), start, _)), Some((EQUAL, _, _))) => Some(start)
    _ => None
  }
  match state.peek() {
    Some((UIDENT(name), start, end)) =>
      state.report(
        mk_loc(start, end),
        "Unexpected uppercase identifier \{name}, expected lowercase identifier in .",
      )
    _ => ()
  }
  if state.consume_any() is Some(name) {
    push_child(children, Some("name"), name)
  } else {
    ()
  }
  let mut kind = Argument_Labelled
  match state.peek() {
    Some((COMMA, _, _)) | Some((RPAREN, _, _)) | Some((EOF, _, _)) =>
      kind = Argument_LabelledPun
    _ => {
      if state.consume_any() is Some(equal) {
        push_child(children, Some("op"), equal)
      } else {
        ()
      }
      report_statement_only_expression(state, "labelled call argument")
      let reports_before_expr = state.reports.length()
      let expr = parse_expr_until(state, [
        TokenKind::TK_COMMA,
        TokenKind::TK_RPAREN,
        TokenKind::TK_EOF,
      ])
      if invalid_tilde_label_start is Some(start) {
        state.report(
          mk_loc(start, expr.loc.end),
          "Invalid syntax `label~=value`. Do you mean `label=value`?",
        )
      } else {
        ()
      }
      push_child(
        children,
        Some("expr"),
        state.required_expr_node(expr, reports_before_expr),
      )
    }
  }
  state
  .node_from_children(kind, children)
  .with_structured_name("semantic_name", Name_Label, ["name"])
  .with_bool_marker("pun", Flag_Pun, kind is Argument_LabelledPun)
}

///|
fn parse_optional_forwarding_argument(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut question_node : CstNode? = None
  match state.peek() {
    Some((UIDENT(name), start, end)) =>
      state.report(
        mk_loc(start, end),
        "Unexpected uppercase identifier \{name}, expected lowercase identifier in .",
      )
    _ => ()
  }
  if state.consume_any() is Some(name) {
    push_child(children, Some("name"), name)
  } else {
    ()
  }
  if state.consume_any() is Some(question) {
    question_node = Some(question)
    push_child(children, Some("op"), question)
  } else {
    ()
  }
  let mut kind = Argument_LabelledOptionPun
  if state.current_is(TokenKind::TK_EQUAL) {
    kind = Argument_LabelledOption
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
    report_statement_only_expression(state, "optional call argument")
    let reports_before_expr = state.reports.length()
    let expr = parse_expr_until(state, [
      TokenKind::TK_COMMA,
      TokenKind::TK_RPAREN,
      TokenKind::TK_EOF,
    ])
    push_child(
      children,
      Some("expr"),
      state.required_expr_node(expr, reports_before_expr),
    )
  }
  let result = state
    .node_from_children(kind, children)
    .with_structured_name("semantic_name", Name_Label, ["name"])
    .with_bool_marker("pun", Flag_Pun, kind is Argument_LabelledOptionPun)
  match question_node {
    Some(question) =>
      result.with_token_location(
        "question_loc",
        question.kind,
        question.loc,
        question.source_span,
      )
    None => result
  }
}

///|
fn parse_statement_item(state : ParserState) -> CstNode {
  let reports_before_statement = state.reports.length()
  state.required_expr_node(parse_statement(state), reports_before_statement)
}

///|
fn parse_let_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut expr_kind = Expr_Let
  match state.peek() {
    Some((LET, _, _)) =>
      if state.consume_any() is Some(let_token) {
        push_child(children, Some("kw"), let_token)
      } else {
        ()
      }
    _ => ()
  }
  match state.peek() {
    Some((MUTABLE, _, _)) =>
      if state.consume_any() is Some(mut_token) {
        push_child(children, Some("kw"), mut_token)
        expr_kind = Expr_LetMut
      } else {
        ()
      }
    _ => ()
  }
  let reports_before_pattern = state.reports.length()
  let pat = parse_pattern_until(state, [
    TokenKind::TK_COLON,
    TokenKind::TK_EQUAL,
    TokenKind::TK_SEMI,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ])
  push_child(
    children,
    Some("pattern"),
    state.required_pattern_node(pat, reports_before_pattern),
  )
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let reports_before_type = state.reports.length()
    let ty = parse_type_arrow(state, [
      TokenKind::TK_EQUAL,
      TokenKind::TK_SEMI,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ])
    push_child(
      children,
      Some("type"),
      state.required_type_node(ty, reports_before_type),
    )
  }
  let reports_before_value = state.reports.length()
  let has_equal = state.current_is(TokenKind::TK_EQUAL)
  push_child(children, Some("op"), state.expect_token(TokenKind::TK_EQUAL))
  if !has_equal {
    while !state.current_is(TokenKind::TK_EQUAL) &&
          !state.current_is(TokenKind::TK_SEMI) &&
          !state.current_is(TokenKind::TK_RBRACE) &&
          !state.current_is(TokenKind::TK_EOF) &&
          !(state.peek() is None) {
      if state.consume_any() is Some(bad) {
        push_child(
          children,
          Some("error"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
    }
    if state.current_is(TokenKind::TK_EQUAL) {
      if state.consume_any() is Some(equal) {
        push_child(children, Some("op"), equal)
      } else {
        ()
      }
    }
  }
  let expr = parse_expr_until(state, [
    TokenKind::TK_SEMI,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ])
  push_child(
    children,
    Some("expr"),
    state.required_expr_node(expr, reports_before_value),
  )
  state.node_from_children(expr_kind, children)
}

///|
fn local_function_statement_can_start(state : ParserState) -> Bool {
  match state.peek_significant() {
    Some((FN, _, _)) =>
      match state.peek_nth_significant(1) {
        Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) => true
        _ => false
      }
    Some((ASYNC, _, _)) =>
      match (state.peek_nth_significant(1), state.peek_nth_significant(2)) {
        (Some((FN, _, _)), Some((LIDENT(_), _, _)))
        | (Some((FN, _, _)), Some((EXTEND, _, _))) => true
        _ => false
      }
    _ => false
  }
}

///|
fn parse_statement(state : ParserState) -> CstNode {
  match state.peek() {
    Some((LET, _, _)) => parse_let_statement(state)
    Some((LETREC, _, _)) => parse_letrec_statement(state)
    Some((FN, _, _)) | Some((ASYNC, _, _)) if local_function_statement_can_start(
        state,
      ) =>
      parse_local_function_statement(state, [
        TokenKind::TK_SEMI,
        TokenKind::TK_RBRACE,
        TokenKind::TK_EOF,
      ])
    Some((GUARD, _, _)) | Some((GUARD_EXCLAMATION, _, _)) =>
      parse_guard_statement(state)
    Some((DEFER, _, _)) | Some((ERRDEFER, _, _)) => parse_defer_statement(state)
    Some((PROOF_ASSERT, _, _)) => parse_proof_assert_statement(state)
    Some((PROOF_LET, _, _)) => parse_proof_let_statement(state)
    Some((TYPE | STRUCT | ENUM | TRAIT | SUBERROR | EXTENUM, _, _)) => {
      state.abort_current_decl = true
      parse_invalid_local_decl(state)
    }
    _ =>
      parse_expr_until(state, [
        TokenKind::TK_SEMI,
        TokenKind::TK_RBRACE,
        TokenKind::TK_EOF,
      ])
  }
}

///|
fn parse_invalid_local_decl(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((found, start, end)) => {
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "simple expression",
      )
      if found is STRUCT {
        match state.peek_nth_significant(1) {
          Some((SEMI(explicit), next_start, next_end)) =>
            state.report_expected_tokens_at(
              Token::SEMI(explicit),
              mk_loc(next_start, next_end),
              [TokenKind::TK_LIDENT],
            )
          _ => ()
        }
      }
    }
    None => ()
  }
  let mut depth = 0
  let mut completed_group = false
  let mut done = false
  while !done {
    if depth == 0 && completed_group {
      match state.peek_significant() {
        Some((LBRACE | LPAREN | LBRACKET | DERIVE, _, _)) => ()
        _ => {
          done = true
          continue
        }
      }
    }
    match state.peek() {
      Some((SEMI(_), _, _)) if depth == 0 && !completed_group => done = true
      Some((RBRACE, _, _)) if depth == 0 => done = true
      Some((EOF, _, _)) | None => done = true
      Some((tok, _, _)) =>
        if state.consume_any() is Some(bad) {
          let old_depth = depth
          depth = next_depth(depth, tok)
          if old_depth > 0 && depth == 0 {
            completed_group = true
          }
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          done = true
        }
    }
  }
  let mut trailing_offset = 0
  let mut trailing_only_closers = true
  let mut trailing_eof : Location? = None
  while trailing_only_closers && trailing_eof is None {
    match state.peek(offset=trailing_offset) {
      Some((COMMENT(_) | NEWLINE | SEMI(_) | RBRACE, _, _)) =>
        trailing_offset += 1
      Some((EOF, start, end)) => trailing_eof = Some(mk_loc(start, end))
      _ => trailing_only_closers = false
    }
  }
  if trailing_eof is Some(loc) {
    state.report(loc, "Unexpected token here.")
  } else {
    ()
  }
  state.node_from_children(Expr_MissingExpr, children)
}

///|
fn parse_guard_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let exclamation = state.current_is(TK_GUARD_EXCLAMATION)
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let reports_before_cond = state.reports.length()
  let cond = parse_expr_with_context_restriction(
    state,
    [
      TokenKind::TK_ELSE,
      TokenKind::TK_SEMI,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ],
    "condition",
    token_disallowed_in_condition,
  )
  push_child(
    children,
    Some("cond"),
    state.required_expr_node(cond, reports_before_cond),
  )
  if state.current_is(TokenKind::TK_ELSE) {
    if state.consume_any() is Some(else_kw) {
      push_child(children, Some("kw"), else_kw)
    } else {
      ()
    }
    if state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("else"), parse_block_expr(state))
    }
  }
  state
  .node_from_children(Expr_Guard, children)
  .with_bool_marker("exclamation", Flag_Exclamation, exclamation)
}

///|
fn parse_defer_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let kind = if state.current_is(TokenKind::TK_ERRDEFER) {
    Expr_ErrDefer
  } else {
    Expr_Defer
  }
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let reports_before_expr = state.reports.length()
  let expr = parse_expr_until(state, [
    TokenKind::TK_SEMI,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ])
  push_child(
    children,
    Some("expr"),
    state.required_expr_node(expr, reports_before_expr),
  )
  state.node_from_children(kind, children)
}

///|
fn parse_proof_assert_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  let expr = parse_expr_until(state, [
    TokenKind::TK_SEMI,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ])
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  state.node_from_children(Expr_ProofAssert, children)
}

///|
fn parse_proof_let_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
    let expr = parse_expr_until(state, [
      TokenKind::TK_SEMI,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ])
    if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
    }
  }
  state
  .node_from_children(Expr_ProofLet, children)
  .with_structured_name("semantic_name", Name_Binder, ["name"])
}

///|
fn parse_letrec_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  push_child(children, Some("binding"), parse_letrec_binding(state))
  let mut done = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((AND, _, _)) => {
        if state.consume_any() is Some(and_kw) {
          push_child(children, Some("kw"), and_kw)
        } else {
          ()
        }
        push_child(children, Some("binding"), parse_letrec_binding(state))
      }
      _ => done = true
    }
  }
  state.node_from_children(Expr_LetAnd, children)
}

///|
fn parse_letrec_binding(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let ty = parse_type_arrow(state, [
      TokenKind::TK_EQUAL,
      TokenKind::TK_SEMI,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ])
    if !ty.children.is_empty() {
      push_child(children, Some("binding_type"), ty)
    }
  }
  let has_equal = state.current_is(TokenKind::TK_EQUAL)
  if has_equal {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
  } else {
    push_child(
      children,
      Some("op"),
      state.missing_token_node_with_expected(TokenKind::TK_EQUAL, [
        TokenKind::TK_EQUAL,
      ]),
    )
    while !state.current_is(TokenKind::TK_AND) &&
          !state.current_is(TokenKind::TK_SEMI) &&
          !state.current_is(TokenKind::TK_RBRACE) &&
          !state.current_is(TokenKind::TK_EOF) &&
          !(state.peek() is None) {
      if state.consume_any() is Some(bad) {
        push_child(
          children,
          Some("error"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
    }
    return state
      .node_from_children(Binding_LetRec, children)
      .with_structured_name("semantic_name", Name_Binder, ["name"])
      .with_bool_marker("async", Flag_Async, false)
  }
  let mut saw_fn = false
  if state.current_is(TokenKind::TK_ASYNC) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
  }
  if state.current_is(TokenKind::TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
      saw_fn = true
    } else {
      ()
    }
  }
  if saw_fn {
    if state.current_is(TokenKind::TK_LPAREN) {
      push_child(children, Some("params"), parse_parameter_list(state))
    }
    if state.current_is(TokenKind::TK_THIN_ARROW) ||
      state.current_is(TokenKind::TK_RAISE) ||
      state.current_is(TokenKind::TK_NORAISE) ||
      state.current_is(TokenKind::TK_NOCANCEL) {
      push_child(children, Some("type"), parse_func_return_type(state))
    }
    if state.current_is(TokenKind::TK_LBRACE) {
      push_child(children, Some("body"), parse_block_expr(state))
    }
  } else if !state.current_in([
      TokenKind::TK_AND,
      TokenKind::TK_SEMI,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ]) {
    let expr = parse_expr_until(state, [
      TokenKind::TK_AND,
      TokenKind::TK_SEMI,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ])
    if !expr.children.is_empty() {
      push_child(children, Some("expr"), expr)
    }
  }
  state
  .node_from_children(Binding_LetRec, children)
  .with_structured_name("semantic_name", Name_Binder, ["name"])
  .with_bool_marker(
    "async",
    Flag_Async,
    children
    .iter()
    .any(fn(entry) {
      entry.0 is Some("kw") && entry.1.has_child_kind(Flag_Async)
    }),
  )
  .with_async_location_anchor()
  .with_decl_like_function_location_anchor()
}

///|
fn parse_local_function_statement(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.current_is(TokenKind::TK_ASYNC) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  if state.current_is(TokenKind::TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  match state.peek() {
    Some((tok, start, end)) if !(tok is LPAREN) => {
      state.report(
        mk_loc(start, end),
        "Failed to parse func: expected '(' or '{', got \{@debug.render(tok.to_repr())}",
      )
      while !state.current_in(stop_kinds) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          ()
        }
      }
      return state
        .node_from_children(Expr_LetFn, children)
        .with_bool_marker(
          "async",
          Flag_Async,
          children
          .iter()
          .any(fn(entry) {
            entry.0 is Some("kw") && entry.1.has_child_kind(Flag_Async)
          }),
        )
        .with_async_location_anchor()
        .with_decl_like_function_location_anchor()
    }
    _ => ()
  }
  let reports_before_signature = state.reports.length()
  if state.current_is(TokenKind::TK_LPAREN) {
    push_child(children, Some("params"), parse_parameter_list(state))
  }
  if state.current_is(TokenKind::TK_THIN_ARROW) ||
    state.current_is(TokenKind::TK_RAISE) ||
    state.current_is(TokenKind::TK_NORAISE) ||
    state.current_is(TokenKind::TK_NOCANCEL) {
    push_child(children, Some("type"), parse_func_return_type(state))
  }
  if state.current_is(TokenKind::TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    let body = if state.reports.length() > reports_before_signature {
      state.missing_token_node_silent(TokenKind::TK_LBRACE)
    } else {
      state.missing_token_node_with_expected(TokenKind::TK_LBRACE, [
        TokenKind::TK_LBRACE,
      ])
    }
    push_child(children, Some("body"), body)
  }
  state
  .node_from_children(Expr_LetFn, children)
  .with_bool_marker(
    "async",
    Flag_Async,
    children
    .iter()
    .any(fn(entry) {
      entry.0 is Some("kw") && entry.1.has_child_kind(Flag_Async)
    }),
  )
  .with_async_location_anchor()
  .with_decl_like_function_location_anchor()
}

///|
fn parse_argument_list(state : ParserState) -> CstNode {
  annotate_argument_list_locations(
    parse_delimited(
      state,
      TokenKind::TK_LPAREN,
      TokenKind::TK_RPAREN,
      parse_argument_item,
      List_ArgumentList,
      sep_kind=TokenKind::TK_COMMA,
      follow_set=[TokenKind::TK_SEMI, TokenKind::TK_RBRACE],
      post_item_expected=[TokenKind::TK_COMMA, TokenKind::TK_RPAREN],
    ),
  )
}

///|
fn annotate_argument_list_locations(source : CstNode) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for i in 0..<source.children.length() {
    match source.children.get(i) {
      Some((Some("item"), item)) if item.kind
        is (Argument_LabelledPun | Argument_LabelledOptionPun) =>
        push_child(
          children,
          Some("item"),
          item.with_pun_locations().with_pun_expr(),
        )
      Some(entry) => children.push(entry)
      None => ()
    }
  }
  node(source.kind, source.loc, source.source_span, children)
}

///|
fn parse_statement_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_statement_item,
    List_BlockStatementList,
    sep_kind=TokenKind::TK_SEMI,
    item_expected="simple expression",
  )
}

///|
fn parse_block_expr(
  state : ParserState,
  classify_record_pun? : Bool = false,
) -> CstNode {
  match state.peek_nth_significant(1) {
    Some((EOF, start, end)) =>
      state.report_expected_description_at(
        EOF,
        mk_loc(start, end),
        "simple expression",
      )
    _ => ()
  }
  let list = parse_statement_list(state)
  let items = list.children_with_label("item")
  if classify_record_pun && items.length() == 1 && items[0].kind is Expr_Ident {
    if items[0].first_child_with_label("name") is Some(name) {
      let field = state
        .node_from_children(Field_Definition, [(Some("name"), name)])
        .with_structured_name("semantic_name", Name_Field, ["name"])
        .with_bool_marker("pun", Flag_Pun, true)
        .with_pun_expr()
      let record_children : Array[(String?, CstNode)] = []
      for entry in list.children {
        let (label, child) = entry
        if label == Some("item") {
          push_child(record_children, label, field)
        } else {
          push_child(record_children, label, child)
        }
      }
      let record_body = with_marker_child(
        node(List_RecordFieldList, list.loc, list.source_span, record_children),
        "trailing",
        list_trailing_mark_kind(list),
      )
      return state.node_from_children(Expr_Record, [(Some("body"), record_body)])
    }
  }
  let body = if items.length() >= 2 &&
    !(items[0].kind
    is (Expr_Let
    | Expr_LetMut
    | Expr_LetFn
    | Expr_LetAnd
    | Expr_Guard
    | Expr_Defer
    | Expr_ErrDefer)) {
    state.node_from_children(Expr_Sequence, [(Some("body"), list)])
  } else {
    list
  }
  with_marker_child(
    state.node_from_children(Expr_Block, [(Some("body"), body)]),
    "group",
    Group_Brace,
  )
}

///|
fn kind_in(kind : TokenKind, kinds : Array[TokenKind]) -> Bool {
  for candidate in kinds {
    if kind == candidate {
      return true
    }
  }
  false
}

///|
fn ParserState::current_is(self : ParserState, kind : TokenKind) -> Bool {
  match self.peek() {
    Some((tok, _, _)) => tok.kind() == kind
    None => false
  }
}

///|
fn ParserState::current_in(
  self : ParserState,
  kinds : Array[TokenKind],
) -> Bool {
  match self.peek() {
    Some((tok, _, _)) => kind_in(tok.kind(), kinds)
    None => false
  }
}

///|
fn ParserState::consume_labeled(
  self : ParserState,
  label : String,
  kind : TokenKind,
) -> (String?, CstNode) {
  (Some(label), self.expect_token(kind))
}

///|
fn ParserState::consume_error_for_progress(self : ParserState) -> CstNode? {
  let misplaced_condition_close = self.peek().map(item => item.0)
    is Some(RPAREN) &&
    self.peek_nth_significant(1).map(item => item.0) is Some(LBRACE)
  if self.consume_any() is Some(bad) {
    if misplaced_condition_close {
      self.suppress_next_top_level_recovery_report = true
      self.report_expected_tokens_at(RPAREN, bad.loc, [TokenKind::TK_LBRACE])
    } else {
      self.reports.push(Report::{ loc: bad.loc, msg: "Unexpected token here.", })
    }
    Some(error_node(bad.loc, bad.source_span, [(Some("item"), bad)]))
  } else {
    None
  }
}

///|
fn ParserState::consume_invalid_series_delimiter(
  self : ParserState,
) -> CstNode? {
  match self.peek() {
    Some((tok, start, end)) => {
      self.report(
        mk_loc(start, end),
        "Expecting a newline or `;` here, but encountered \{tok.to_expect_string()}.",
      )
      if self.consume_any() is Some(bad) {
        Some(error_node(bad.loc, bad.source_span, [(Some("item"), bad)]))
      } else {
        None
      }
    }
    None => None
  }
}

///|
fn report_next_top_level_recovery_point(state : ParserState) -> Unit {
  let mut offset = 0
  let mut depth = 0
  let mut saw_statement_boundary = false
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((tok, start, end)) if depth == 0 && top_level_decl_can_start(tok) => {
        state.report(mk_loc(start, end), "Unexpected token here.")
        done = true
      }
      Some((EOF, _, _)) | None => done = true
      Some((COMMENT(_) | NEWLINE | SEMI(_), _, _)) => {
        if depth == 0 {
          saw_statement_boundary = true
        }
        offset += 1
      }
      Some((LPAREN | LBRACKET | LBRACE, _, _)) => {
        depth += 1
        offset += 1
      }
      Some((RPAREN | RBRACKET | RBRACE, _, _)) => {
        if depth > 0 {
          depth -= 1
        }
        offset += 1
      }
      Some(
        (
          RETURN
          | BREAK
          | CONTINUE
          | RAISE
          | DEFER
          | ERRDEFER
          | GUARD
          | GUARD_EXCLAMATION,
          _,
          _,
        )
      ) if depth == 0 && saw_statement_boundary => done = true
      Some(_) => offset += 1
    }
  }
}

///|
fn recovery_crosses_argument_boundary(state : ParserState) -> Bool {
  let mut offset = 0
  let mut saw_block_close = false
  while true {
    match state.peek(offset~) {
      Some((RBRACE, _, _)) => {
        saw_block_close = true
        offset += 1
      }
      Some((RPAREN, _, _)) if saw_block_close => return true
      Some((tok, _, _)) if top_level_decl_can_start(tok) => return false
      Some((EOF, _, _)) | None => return false
      Some(_) => offset += 1
    }
  }
  false
}

///|
fn parse_delimited(
  state : ParserState,
  open_kind : TokenKind,
  close_kind : TokenKind,
  item_parser : (ParserState) -> CstNode,
  list_kind : NodeKind,
  sep_kind? : TokenKind = TokenKind::TK_COMMA,
  follow_set? : Array[TokenKind] = [],
  invalid_delims? : Array[TokenKind] = [],
  require_nonempty? : Bool = false,
  nonempty_expected? : String = "",
  item_expected? : String = "",
  post_item_expected? : Array[TokenKind] = [],
  report_post_item_after_error? : Bool = false,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_children(children, state.consume_trivia())
  push_child(children, Some("open"), state.expect_token(open_kind))
  let mut done = false
  let mut saw_item = false
  let mut expect_item = true
  let mut suppress_missing_close = false
  while !done {
    push_children(children, state.consume_trivia())
    if list_kind is List_BlockStatementList && state.abort_current_decl {
      if state.current_is(close_kind) {
        push_child(children, Some("close"), state.expect_token(close_kind))
      } else {
        push_child(
          children,
          Some("close"),
          state.missing_token_node_silent(close_kind),
        )
      }
      done = true
    } else if state.current_is(close_kind) {
      if require_nonempty && !saw_item {
        if nonempty_expected == "" {
          state.report_expected_tokens([open_kind])
        } else {
          state.report_expected_description(nonempty_expected)
        }
      }
      push_child(children, Some("close"), state.expect_token(close_kind))
      done = true
    } else if state.current_is(TokenKind::TK_EOF) ||
      state.current_in(follow_set) ||
      state.peek() is None {
      if expect_item && list_kind is List_BlockStatementList {
        push_child(
          children,
          Some("close"),
          state.missing_token_node_silent(close_kind),
        )
      } else if expect_item {
        let item = item_parser(state)
        push_child(children, Some("item"), item)
        push_child(
          children,
          Some("close"),
          state.missing_token_node_silent(close_kind),
        )
      } else {
        let close = if suppress_missing_close {
          state.missing_token_node_silent(close_kind)
        } else {
          state.missing_token_node_with_expected(close_kind, [
            sep_kind, close_kind,
          ])
        }
        push_child(children, Some("close"), close)
      }
      done = true
    } else if expect_item {
      if state.current_is(sep_kind) {
        if item_expected != "" {
          state.report_expected_description(item_expected)
          if state.consume_any() is Some(bad) {
            push_child(
              children,
              Some("error"),
              error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
            )
          } else {
            done = true
          }
        } else if state.consume_error_for_progress() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      } else if state.current_in(invalid_delims) {
        if state.consume_invalid_series_delimiter() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      } else {
        let entry_pos = state.pos
        let reports_before_item = state.reports.length()
        let item = item_parser(state)
        push_child(children, Some("item"), item)
        if !item.children.is_empty() {
          saw_item = true
          expect_item = false
        }
        if state.reports.length() > reports_before_item &&
          (
            state.current_is(TokenKind::TK_EOF) ||
            state.current_in(follow_set) ||
            state.peek() is None
          ) {
          suppress_missing_close = true
        }
        let latest_report_is_at_current = if state.reports.length() > 0 {
          match (state.reports.get(state.reports.length() - 1), state.peek()) {
            (Some(report), Some((_, start, _))) => report.loc.start == start
            _ => false
          }
        } else {
          false
        }
        if state.reports.length() > reports_before_item &&
          (
            !(list_kind is List_BlockStatementList) ||
            latest_report_is_at_current
          ) &&
          !state.current_is(sep_kind) &&
          !state.current_is(close_kind) &&
          !state.current_is(TokenKind::TK_EOF) &&
          !state.current_in(follow_set) &&
          !(state.peek() is None) {
          if report_post_item_after_error && !latest_report_is_at_current {
            if post_item_expected.length() > 0 {
              state.report_expected_tokens(post_item_expected)
            } else {
              state.report_expected_tokens([sep_kind, close_kind, ..follow_set])
            }
          }
          let mut recovering = true
          while recovering {
            if state.current_is(sep_kind) ||
              state.current_is(close_kind) ||
              state.current_is(TokenKind::TK_EOF) ||
              state.current_in(follow_set) ||
              state.peek() is None {
              recovering = false
            } else if state.consume_any() is Some(bad) {
              push_child(
                children,
                Some("error"),
                error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
              )
            } else {
              recovering = false
            }
          }
          saw_item = true
          expect_item = false
        }
        if state.pos == entry_pos &&
          state.reports.length() > reports_before_item {
          push_child(
            children,
            Some("close"),
            state.missing_token_node_silent(close_kind),
          )
          done = true
        }
        if state.pos == entry_pos &&
          !done &&
          !state.current_is(close_kind) &&
          !state.current_is(sep_kind) &&
          !state.current_is(TokenKind::TK_EOF) &&
          !state.current_in(follow_set) {
          if state.consume_error_for_progress() is Some(error) {
            push_child(children, Some("error"), error)
          } else {
            done = true
          }
        }
      }
    } else if state.current_is(sep_kind) {
      push_child(children, Some("sep"), state.expect_token(sep_kind))
      expect_item = true
    } else if state.current_in(invalid_delims) {
      if state.consume_invalid_series_delimiter() is Some(error) {
        push_child(children, Some("error"), error)
        expect_item = true
      } else {
        done = true
      }
    } else {
      let recovery_token = state.peek().map(item => item.0)
      if post_item_expected.length() > 0 {
        state.report_expected_tokens(post_item_expected)
      } else {
        state.report_expected_tokens([sep_kind, close_kind, ..follow_set])
      }
      let mut recovering = true
      while recovering {
        if state.current_is(sep_kind) ||
          state.current_is(close_kind) ||
          state.current_is(TokenKind::TK_EOF) ||
          state.current_in(follow_set) ||
          state.peek() is None {
          recovering = false
        } else if state.consume_any() is Some(bad) {
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          recovering = false
        }
      }
      if (list_kind is List_ArgumentList || list_kind is List_TupleElementList) &&
        recovery_token is Some(ELSE) {
        report_next_top_level_recovery_point(state)
      } else if list_kind is List_BlockStatementList &&
        recovery_token is Some(EXCLAMATION) &&
        recovery_crosses_argument_boundary(state) {
        report_next_top_level_recovery_point(state)
      }
    }
  }
  state.node_from_children(list_kind, children)
}

///|
fn parse_pattern_until(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  parse_pattern_alias(state, stop_kinds)
}

///|
fn pattern_can_start(tok : Token) -> Bool {
  match tok {
    TRUE
    | FALSE
    | CHAR(_)
    | INT(_)
    | BYTE(_)
    | DOUBLE(_)
    | FLOAT(_)
    | STRING(_)
    | MULTILINE_STRING(_)
    | BYTES(_)
    | UNDERSCORE
    | LIDENT(_)
    | EXTEND
    | UIDENT(_)
    | PACKAGE_NAME(_)
    | AMPER
    | LPAREN
    | LBRACKET
    | LBRACE
    | MINUS => true
    _ => false
  }
}

///|
fn ParserState::pattern_has_top_level_annotation(self : ParserState) -> Bool {
  let mut offset = 0
  let mut depth = 0
  while true {
    match self.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((tok, _, _)) if depth == 0 =>
        match tok {
          COLON => return true
          COMMA | RPAREN | RBRACKET | RBRACE | EQUAL | SEMI(_) | EOF =>
            return false
          _ => {
            depth = next_depth(depth, tok)
            offset += 1
          }
        }
      Some((tok, _, _)) => {
        depth = next_depth(depth, tok)
        offset += 1
      }
      None => return false
    }
  }
  false
}

///|
fn parenthesized_pattern_has_top_level_with(state : ParserState) -> Bool {
  let mut offset = 1
  let mut depth = 0
  while true {
    match state.peek(offset~) {
      Some((WITH, _, _)) if depth == 0 => return true
      Some((RPAREN, _, _)) if depth == 0 => return false
      Some((EOF, _, _)) | None => return false
      Some((tok, _, _)) => {
        depth = next_depth(depth, tok)
        offset += 1
      }
    }
  }
  false
}

///|
fn parse_with_pattern_binding(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    Some((found, start, end)) => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TK_LIDENT,
        TK_EXTEND,
      ])
      if found is LPAREN {
        let mut depth = 0
        let mut done = false
        while !done {
          match state.peek() {
            Some((LPAREN, _, _)) => {
              depth += 1
              if state.consume_any() is Some(error) {
                push_child(children, Some("error"), error)
              }
            }
            Some((RPAREN, _, _)) if depth == 1 => done = true
            Some((RPAREN, _, _)) => {
              depth -= 1
              if state.consume_any() is Some(error) {
                push_child(children, Some("error"), error)
              }
            }
            Some((EOF, _, _)) | None => done = true
            Some(_) =>
              if state.consume_any() is Some(error) {
                push_child(children, Some("error"), error)
              }
          }
        }
        return state
          .node_from_children(Clause_WithBinding, children)
          .with_structured_name("semantic_name", Name_Binder, ["name"])
      }
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TK_LIDENT),
      )
    }
    None =>
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TK_LIDENT),
      )
  }
  if state.current_is(TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
  } else {
    state.report_expected_tokens([TK_EQUAL])
  }
  let expr = parse_expr_prefix(state, [TK_COMMA, TK_RPAREN, TK_EOF])
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  state
  .node_from_children(Clause_WithBinding, children)
  .with_structured_name("semantic_name", Name_Binder, ["name"])
}

///|
fn parse_parenthesized_with_pattern(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("open"), state.expect_token(TK_LPAREN))
  push_children(children, state.consume_trivia())
  let pat = parse_pattern_until(state, [TK_WITH, TK_EOF])
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  push_children(children, state.consume_trivia())
  push_child(children, Some("op"), state.expect_token(TK_WITH))
  let binding_children : Array[(String?, CstNode)] = []
  let mut saw_binding = false
  let mut expect_binding = true
  let mut done = false
  while !done {
    push_children(binding_children, state.consume_trivia())
    if state.current_is(TK_RPAREN) ||
      state.current_is(TK_EOF) ||
      state.peek() is None {
      if !saw_binding {
        match state.peek() {
          Some((_, start, end)) =>
            state.report(
              mk_loc(start, end),
              "Expected a pattern binding after `with`.",
            )
          None => ()
        }
      }
      done = true
    } else if expect_binding {
      let pos = state.pos
      let binding = parse_with_pattern_binding(state)
      push_child(binding_children, Some("item"), binding)
      saw_binding = true
      expect_binding = false
      if state.pos == pos {
        if state.consume_error_for_progress() is Some(error) {
          push_child(binding_children, Some("error"), error)
        } else {
          done = true
        }
      }
    } else if state.current_is(TK_COMMA) {
      push_child(binding_children, Some("sep"), state.expect_token(TK_COMMA))
      expect_binding = true
    } else {
      state.report_expected_tokens([TK_COMMA, TK_RPAREN])
      if state.consume_error_for_progress() is Some(error) {
        push_child(binding_children, Some("error"), error)
      } else {
        done = true
      }
    }
  }
  push_child(
    children,
    Some("body"),
    state.node_from_children(List_WithBindingList, binding_children),
  )
  push_child(children, Some("close"), state.expect_token(TK_RPAREN))
  state.node_from_children(Pattern_With, children)
}

///|
fn pattern_starts_constr_any(state : ParserState) -> Bool {
  let colon_offset = match state.peek_significant() {
    Some((AMPER, _, _)) =>
      match state.peek_nth_significant(1) {
        Some((UIDENT(_), _, _)) => 2
        Some((PACKAGE_NAME(_), _, _)) =>
          match state.peek_nth_significant(2) {
            Some((DOT_UIDENT(_), _, _)) => 3
            _ => -1
          }
        _ => -1
      }
    Some((UIDENT(_), _, _)) => 1
    Some((PACKAGE_NAME(_), _, _)) =>
      match state.peek_nth_significant(1) {
        Some((DOT_UIDENT(_), _, _)) => 2
        _ => -1
      }
    _ => -1
  }
  colon_offset >= 0 &&
  state.peek_nth_significant(colon_offset).map(item => item.0)
  is Some(COLONCOLON) &&
  state.peek_nth_significant(colon_offset + 1).map(item => item.0)
  is Some(UNDERSCORE)
}

///|
fn parse_constr_any_pattern(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("type"), parse_type_name_node(state))
  push_child(children, Some("op"), state.expect_token(TokenKind::TK_COLONCOLON))
  push_child(
    children,
    Some("wildcard"),
    state.expect_token(TokenKind::TK_UNDERSCORE),
  )
  state.node_from_children(Pattern_ConstrAny, children)
}

///|
fn finish_constructor_pattern(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> CstNode {
  if state.current_is(TK_LPAREN) {
    push_child(children, Some("params"), parse_constructor_pattern_args(state))
  }
  let pattern = state
    .node_from_children(Pattern_Constr, children)
    .with_structured_name("semantic_name", Name_Constructor, [
      "pkg", "type", "name", "field",
    ])
    .with_constructor_extra_info()
  let is_open = pattern
    .first_child_with_label("params")
    .map(list => list_has_item_kind(list, Pattern_Rest))
    .unwrap_or(false)
  with_marker_child(
    pattern,
    "openness",
    if is_open {
      Openness_Open
    } else {
      Openness_Closed
    },
  )
}

///|
fn parse_object_constructor_pattern(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  push_child(children, Some("type"), parse_type_name_node(state))
  push_child(children, Some("op"), state.expect_token(TK_COLONCOLON))
  match state.peek() {
    Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("field"), name)
      } else {
        ()
      }
    Some((LIDENT(name), start, end)) => {
      state.report(
        mk_loc(start, end),
        "Unexpected lowercase identifier \{name}, expected uppercase identifier in constructor name.",
      )
      if state.consume_any() is Some(name) {
        push_child(children, Some("field"), name)
      } else {
        ()
      }
    }
    Some((PACKAGE_NAME(_), _, _)) => {
      if state.consume_any() is Some(pkg) {
        push_child(children, Some("pkg"), pkg)
      } else {
        ()
      }
      match state.peek() {
        Some((DOT_UIDENT(_), _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("field"), name)
          } else {
            ()
          }
        _ =>
          push_child(
            children,
            Some("field"),
            state.missing_token_node(TK_UIDENT),
          )
      }
    }
    _ => push_child(children, Some("field"), state.expect_token(TK_UIDENT))
  }
  finish_constructor_pattern(state, children)
}

///|
fn parse_pattern_atom(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  match state.peek() {
    Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) =>
      state.node_from_children(Pattern_MissingPattern, [])
    Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => {
      let children = state.consume_trivia()
      let pat = parse_pattern_atom(state, stop_kinds)
      if !pat.children.is_empty() {
        push_child(children, Some("pattern"), pat)
      }
      state.node_from_children(Pattern_Constraint, children)
    }
    Some((UNDERSCORE, _, _)) => parse_single_token_pattern(state, Pattern_Any)
    Some((AMPER, _, _))
    | Some((UIDENT(_), _, _))
    | Some((PACKAGE_NAME(_), _, _)) if pattern_starts_constr_any(state) =>
      parse_constr_any_pattern(state)
    Some((AMPER, _, _)) => parse_object_constructor_pattern(state)
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) => {
      let mut node = parse_single_token_pattern(state, Pattern_Var)
      if state.current_is(TokenKind::TK_LPAREN) {
        let args = parse_constructor_pattern_args(state)
        node = state.node_from_children(Pattern_SpecialConstr, [
          (Some("name"), node),
          (Some("params"), args),
        ])
      }
      node
    }
    Some((UIDENT(_), _, _)) | Some((PACKAGE_NAME(_), _, _)) => {
      let children : Array[(String?, CstNode)] = []
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
      match state.peek() {
        Some((DOT_UIDENT(_), _, _)) | Some((DOT_LIDENT(_), _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("name"), name)
          } else {
            ()
          }
        _ => ()
      }
      if state.current_is(TokenKind::TK_COLONCOLON) {
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
        } else {
          ()
        }
        match state.peek() {
          Some((LIDENT(name), start, end)) => {
            state.report(
              mk_loc(start, end),
              "Unexpected lowercase identifier \{name}, expected uppercase identifier in constructor name.",
            )
            if state.consume_any() is Some(name) {
              push_child(children, Some("name"), name)
            } else {
              ()
            }
          }
          _ =>
            if state.consume_any() is Some(name) {
              push_child(children, Some("name"), name)
            } else {
              ()
            }
        }
        match state.peek() {
          Some((DOT_UIDENT(_), _, _)) | Some((DOT_LIDENT(_), _, _)) =>
            if state.consume_any() is Some(name) {
              push_child(children, Some("name"), name)
            } else {
              ()
            }
          _ => ()
        }
      }
      finish_constructor_pattern(state, children)
    }
    Some((LPAREN, _, _)) =>
      if parenthesized_pattern_has_top_level_with(state) {
        parse_parenthesized_with_pattern(state)
      } else {
        let list = parse_constructor_pattern_args(state, tuple_pattern=true)
        let tuple = state.node_from_children(Pattern_Tuple, [
          (Some("body"), list),
        ])
        if list.children_with_label("item").length() == 1 {
          with_marker_child(tuple, "pattern_group", Pattern_Group)
        } else {
          tuple
        }
      }
    Some((LBRACKET, _, _)) => {
      let list = parse_array_pattern_list(state)
      state.node_from_children(Pattern_Array, [(Some("body"), list)])
    }
    Some((LBRACE, _, _)) =>
      if brace_starts_map_pattern(state) {
        let list = parse_map_pattern_list(state)
        with_marker_child(
          state.node_from_children(Pattern_Map, [(Some("body"), list)]),
          "openness",
          if list_has_item_kind(list, Pattern_Rest) {
            Openness_Open
          } else {
            Openness_Closed
          },
        )
      } else {
        let list = parse_record_pattern_list(state)
        with_marker_child(
          state.node_from_children(Pattern_Record, [(Some("body"), list)]),
          "openness",
          if list_has_item_kind(list, Pattern_Rest) {
            Openness_Open
          } else {
            Openness_Closed
          },
        )
      }
    Some((found, start, end)) if found is (REGEX_INTERP(_) | INTERP(_)) => {
      let pat = parse_regex_interp_lex_pattern_node(state)
      state.report_expected_tokens_at(
        found,
        mk_loc(start, end),
        required_pattern_start_tokens(),
      )
      pat
    }
    Some((found, start, end)) if found is REGEX_LITERAL(_) => {
      let pat = parse_literal_pattern(state, stop_kinds)
      state.report_expected_tokens_at(
        found,
        mk_loc(start, end),
        required_pattern_start_tokens(),
      )
      pat
    }
    Some((TRUE, _, _))
    | Some((FALSE, _, _))
    | Some((CHAR(_), _, _))
    | Some((INT(_), _, _))
    | Some((BYTE(_), _, _))
    | Some((DOUBLE(_), _, _))
    | Some((FLOAT(_), _, _))
    | Some((STRING(_), _, _))
    | Some((BYTES(_), _, _))
    | Some((MINUS, _, _)) => parse_literal_pattern(state, stop_kinds)
    Some((found, start, end)) => {
      state.report_expected_tokens_at(
        found,
        mk_loc(start, end),
        required_pattern_start_tokens(),
      )
      let children : Array[(String?, CstNode)] = []
      while !state.current_in(stop_kinds) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          ()
        }
      }
      state.node_from_children(Pattern_MissingPattern, children)
    }
    None => state.node_from_children(Pattern_MissingPattern, [])
  }
}

///|
fn brace_starts_map_pattern(state : ParserState) -> Bool {
  let mut offset = 1
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((STRING(_), _, _))
      | Some((BYTES(_), _, _))
      | Some((REGEX_LITERAL(_), _, _))
      | Some((INT(_), _, _))
      | Some((BYTE(_), _, _))
      | Some((CHAR(_), _, _))
      | Some((DOUBLE(_), _, _))
      | Some((FLOAT(_), _, _))
      | Some((TRUE, _, _))
      | Some((FALSE, _, _))
      | Some((MINUS, _, _)) => return true
      _ => done = true
    }
  }
  false
}

///|
fn parse_single_token_pattern(state : ParserState, kind : NodeKind) -> CstNode {
  if state.consume_any() is Some(token) {
    let pattern = state.node_from_children(kind, [(Some("name"), token)])
    if kind is Pattern_Var {
      pattern.with_structured_name("semantic_name", Name_Binder, ["name"])
    } else {
      pattern
    }
  } else {
    state.node_from_children(Pattern_MissingPattern, [])
  }
}

///|
fn parse_literal_pattern(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let _ = stop_kinds
  let first_token = state.peek().map(item => item.0)
  let first_is_minus = first_token is Some(MINUS)
  let mut constant : CstNode? = None
  if state.consume_any() is Some(token) {
    push_child(
      children,
      if first_is_minus {
        Some("op")
      } else {
        Some("token")
      },
      token,
    )
    if first_is_minus {
      push_child(
        children,
        Some("operator"),
        operator_node_from_token(
          Operator_UnaryMinus,
          MINUS,
          token.loc,
          token.source_span,
        ),
      )
    }
    if !first_is_minus && first_token is Some(raw) {
      constant = constant_node_from_token(raw, token.loc, token.source_span)
    }
  }
  match state.peek() {
    Some((raw, _, _)) if first_is_minus && raw is INT(_) =>
      if state.consume_any() is Some(token) {
        push_child(children, Some("token"), token)
        constant = constant_node_from_token(raw, token.loc, token.source_span)
      } else {
        ()
      }
    Some((raw, _, _)) if first_is_minus && raw is DOUBLE(_) =>
      if state.consume_any() is Some(token) {
        push_child(children, Some("token"), token)
        constant = constant_node_from_token(raw, token.loc, token.source_span)
      } else {
        ()
      }
    Some((raw, _, _)) if first_is_minus && raw is FLOAT(_) =>
      if state.consume_any() is Some(token) {
        push_child(children, Some("token"), token)
        constant = constant_node_from_token(raw, token.loc, token.source_span)
      } else {
        ()
      }
    Some((found, start, end)) if first_is_minus => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_INT,
        TokenKind::TK_FLOAT,
        TokenKind::TK_DOUBLE,
      ])
      if state.consume_any() is Some(token) {
        push_child(
          children,
          Some("error"),
          error_node(token.loc, token.source_span, [(Some("item"), token)]),
        )
      } else {
        ()
      }
    }
    _ => ()
  }
  if constant is Some(value) {
    push_child(
      children,
      Some("constant"),
      signed_constant_node(value, first_is_minus),
    )
  }
  state.node_from_children(Pattern_Constant, children)
}

///|
fn parse_constructor_pattern_args(
  state : ParserState,
  tuple_pattern? : Bool = false,
) -> CstNode {
  let list = parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_constructor_pattern_arg,
    if tuple_pattern {
      List_TuplePatternList
    } else {
      List_ConstructorPatternArgumentList
    },
    sep_kind=TokenKind::TK_COMMA,
    post_item_expected=if tuple_pattern {
      [TK_WITH, TK_COMMA, TK_COLON, TK_RPAREN]
    } else {
      []
    },
  )
  let mut item_count = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), _)) => item_count += 1
      _ => ()
    }
  }
  let mut item_index = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), item)) => {
        if item.kind is Pattern_Rest && item_index + 1 < item_count {
          if item.first_child_with_label("op") is Some(op) {
            state.report(
              op.loc,
              "Unexpected `..` here, add `, ..` behind the last argument to ignore the rest arguments.",
            )
          } else {
            ()
          }
        }
        item_index += 1
      }
      _ => ()
    }
  }
  if tuple_pattern {
    let mut saw_invalid_statement_separator = false
    let mut close : CstNode? = None
    for entry in list.children {
      match entry {
        (Some("error"), item) if item.first_trailing_mark_descendant()
          is Some(mark) &&
          mark.kind is TrailingMark_Semi =>
          saw_invalid_statement_separator = true
        (Some("close"), item) => close = Some(item)
        _ => ()
      }
    }
    if saw_invalid_statement_separator {
      if close is Some(item) {
        let expected = [TokenKind::TK_SEMI, TokenKind::TK_RBRACE]
        if item.kind is Missing {
          state.report_expected_tokens(expected)
        } else {
          state.report_expected_tokens_at(RPAREN, item.loc, expected)
        }
      } else {
        ()
      }
    }
  }
  list
}

///|
fn parse_constructor_pattern_arg(state : ParserState) -> CstNode {
  match (state.peek_significant(), state.peek_nth_significant(1)) {
    (Some((DOTDOT, _, _)), _) => parse_dotdot_pattern_rest(state)
    (Some((POST_LABEL(_), _, _)), _) =>
      parse_labelled_pun_constructor_pattern_arg(state)
    (
      Some((LIDENT(_), _, _))
      | Some((EXTEND, _, _))
      | Some((UIDENT(_), _, _)),
      Some((EQUAL, _, _)),
    ) => parse_labelled_constructor_pattern_arg(state)
    _ =>
      parse_pattern_until(state, [
        TokenKind::TK_COMMA,
        TokenKind::TK_RPAREN,
        TokenKind::TK_EOF,
      ])
  }
}

///|
fn parse_labelled_pun_constructor_pattern_arg(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(name) {
    if state.current_is(TokenKind::TK_EQUAL) {
      state.report(
        name.loc,
        "unexpected `~` in argument. Did you mean `label=pattern` or `label~`?",
      )
    }
    push_child(children, Some("name"), name)
  } else {
    ()
  }
  let mut kind = Argument_LabelledPun
  if state.current_is(TokenKind::TK_EQUAL) {
    kind = Argument_Labelled
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
    let pat = parse_pattern_until(state, [
      TokenKind::TK_COMMA,
      TokenKind::TK_RPAREN,
      TokenKind::TK_EOF,
    ])
    if !pat.children.is_empty() {
      push_child(children, Some("pattern"), pat)
    }
  }
  let result = state
    .node_from_children(kind, children)
    .with_structured_name("semantic_name", Name_Label, ["name"])
    .with_bool_marker("pun", Flag_Pun, kind is Argument_LabelledPun)
  if kind is Argument_LabelledPun {
    result.with_pun_locations().with_pun_pattern()
  } else {
    result
  }
}

///|
fn parse_labelled_constructor_pattern_arg(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(name) {
    push_child(children, Some("name"), name)
  } else {
    ()
  }
  if state.current_is(TokenKind::TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
  }
  let pat = parse_pattern_until(state, [
    TokenKind::TK_COMMA,
    TokenKind::TK_RPAREN,
    TokenKind::TK_EOF,
  ])
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  state
  .node_from_children(Argument_Labelled, children)
  .with_structured_name("semantic_name", Name_Label, ["name"])
  .with_bool_marker("pun", Flag_Pun, false)
}

///|
fn parse_array_pattern_list(state : ParserState) -> CstNode {
  let rest_offenders : Array[(Token, Location)] = []
  let list = parse_delimited(
    state,
    TokenKind::TK_LBRACKET,
    TokenKind::TK_RBRACKET,
    fn(state) { parse_array_pattern_item(state, rest_offenders) },
    List_ArrayPatternList,
    sep_kind=TokenKind::TK_COMMA,
  )
  let mut saw_ordinary_rest = false
  let mut rest_offender_index = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), item)) if item.kind is Pattern_Rest => {
        let is_constant_spread = item.first_child_with_label("str") is Some(_) ||
          item.first_child_with_label("bytes") is Some(_) ||
          item.first_child_with_label("const") is Some(_)
        if !is_constant_spread {
          if saw_ordinary_rest {
            if rest_offenders.get(rest_offender_index) is Some((found, loc)) {
              state.report(loc, "At most one `..` is allowed in array pattern.")
              state.report_expected_tokens_at(found, loc, [
                TokenKind::TK_COMMA,
                TokenKind::TK_RBRACKET,
              ])
            } else {
              ()
            }
          } else {
            saw_ordinary_rest = true
          }
          rest_offender_index += 1
        }
      }
      _ => ()
    }
  }
  node(
    if saw_ordinary_rest {
      ArrayPatterns_Open
    } else {
      ArrayPatterns_Closed
    },
    list.loc,
    list.source_span,
    [(Some("body"), list)],
  )
}

///|
fn parse_array_pattern_item(
  state : ParserState,
  rest_offenders : Array[(Token, Location)],
) -> CstNode {
  let item = match state.peek() {
    Some((DOTDOT, _, _)) =>
      parse_dotdot_pattern_rest_recording(state, rest_offenders)
    _ =>
      parse_pattern_until(state, [
        TokenKind::TK_COMMA,
        TokenKind::TK_RBRACKET,
        TokenKind::TK_EOF,
      ])
  }
  match item.kind {
    Pattern_Rest if item.first_child_with_label("str") is Some(_) =>
      item.with_kind(ArrayPattern_StringSpread)
    Pattern_Rest if item.first_child_with_label("bytes") is Some(_) =>
      item.with_kind(ArrayPattern_BytesSpread)
    Pattern_Rest if item.first_child_with_label("const") is Some(_) =>
      item
      .with_kind(ArrayPattern_ConstSpread)
      .with_structured_name("semantic_name", Name_Binder, ["const"])
    Pattern_Rest => item
    _ =>
      node(ArrayPattern_Pattern, item.loc, item.source_span, [
        (Some("pattern"), item),
      ])
  }
}

///|
fn map_pattern_key_tokens() -> Array[TokenKind] {
  [
    TokenKind::TK_TRUE,
    TokenKind::TK_FALSE,
    TokenKind::TK_CHAR,
    TokenKind::TK_INT,
    TokenKind::TK_FLOAT,
    TokenKind::TK_DOUBLE,
    TokenKind::TK_MINUS,
    TokenKind::TK_STRING,
    TokenKind::TK_REGEX_LITERAL,
  ]
}

///|
fn validate_brace_pattern_list(
  state : ParserState,
  list : CstNode,
  starts_as_map : Bool,
  item_starts : Array[(Token, Location)?],
) -> Unit {
  let mut map_kind = starts_as_map
  let mut kind_known = starts_as_map
  if !kind_known {
    for i in 0..<list.children.length() {
      match list.children.get(i) {
        Some((Some("item"), item)) =>
          match item.kind {
            Pattern_Rest => ()
            Field_Pattern => {
              kind_known = true
              map_kind = false
              break
            }
            Field_MapPattern => {
              kind_known = true
              map_kind = true
              break
            }
            _ => break
          }
        _ => ()
      }
    }
  }
  let mut item_count = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), _)) => item_count += 1
      _ => ()
    }
  }
  let mut item_index = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), item)) => {
        match item.kind {
          Pattern_Rest =>
            if item_index + 1 < item_count {
              if item.first_child_with_label("op") is Some(op) {
                state.report(
                  op.loc,
                  if map_kind {
                    "Unexpected `..` here"
                  } else {
                    "Unexpected `..` here, add `, ..` behind the last field to ignore the rest of record."
                  },
                )
              } else {
                ()
              }
            }
          Field_Pattern =>
            if map_kind {
              state.report(item.loc, "Unexpected `..` here")
            } else {
              kind_known = true
            }
          Field_MapPattern =>
            if kind_known && !map_kind {
              state.report(
                item.loc,
                "Record pattern and map pattern cannot be mixed",
              )
            } else {
              kind_known = true
              map_kind = true
            }
          _ => {
            let expected = if !kind_known {
              [
                TokenKind::TK_LIDENT,
                TokenKind::TK_EXTEND,
                ..map_pattern_key_tokens(),
              ]
            } else if map_kind {
              map_pattern_key_tokens()
            } else {
              [TokenKind::TK_LIDENT, TokenKind::TK_EXTEND]
            }
            match item_starts.get(item_index) {
              Some(Some((found, loc))) =>
                state.report_expected_tokens_at(found, loc, expected)
              _ => ()
            }
          }
        }
        item_index += 1
      }
      _ => ()
    }
  }
}

///|
fn parse_dotdot_pattern_rest(state : ParserState) -> CstNode {
  parse_dotdot_pattern_rest_recording(state, [])
}

///|
fn parse_dotdot_pattern_rest_recording(
  state : ParserState,
  rest_offenders : Array[(Token, Location)],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut binder_kind = DotDotBinder_None
  let mut offender : (Token, Location)? = None
  let mut is_constant_spread = false
  if state.consume_any() is Some(op) {
    offender = Some((DOTDOT, op.loc))
    push_child(children, Some("op"), op)
  } else {
    ()
  }
  match state.peek() {
    Some((STRING(_), _, _)) =>
      if state.consume_any() is Some(str) {
        is_constant_spread = true
        push_child(children, Some("str"), str)
      } else {
        ()
      }
    Some((BYTES(_), _, _)) =>
      if state.consume_any() is Some(bytes) {
        is_constant_spread = true
        push_child(children, Some("bytes"), bytes)
      } else {
        ()
      }
    Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        is_constant_spread = true
        push_child(children, Some("const"), name)
      } else {
        ()
      }
    Some((PACKAGE_NAME(_), _, _)) => {
      is_constant_spread = true
      if state.consume_any() is Some(pkg) {
        push_child(children, Some("pkg"), pkg)
      } else {
        ()
      }
      match state.peek() {
        Some((DOT_UIDENT(_), _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("const"), name)
          } else {
            ()
          }
        _ => ()
      }
    }
    Some((AS, _, _)) => {
      binder_kind = DotDotBinder_BinderAs
      if state.consume_any() is Some(as_tok) {
        offender = Some((AS, as_tok.loc))
        push_child(children, Some("as"), as_tok)
      } else {
        ()
      }
      match state.peek() {
        Some((LIDENT(value), _, _)) =>
          if state.consume_any() is Some(name) {
            offender = Some((LIDENT(value), name.loc))
            push_child(children, Some("name"), name)
          } else {
            ()
          }
        _ => ()
      }
    }
    Some((UNDERSCORE, _, _)) => {
      binder_kind = DotDotBinder_Underscore
      if state.consume_any() is Some(name) {
        offender = Some((UNDERSCORE, name.loc))
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    }
    Some((LIDENT(value), _, _)) => {
      binder_kind = DotDotBinder_Binder
      if state.consume_any() is Some(name) {
        offender = Some((LIDENT(value), name.loc))
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    }
    _ => ()
  }
  if !is_constant_spread && offender is Some(value) {
    rest_offenders.push(value)
  }
  with_marker_child(
    state.node_from_children(Pattern_Rest, children),
    "dot_dot_binder",
    binder_kind,
  )
}

///|
fn parse_record_pattern_list(state : ParserState) -> CstNode {
  let item_starts : Array[(Token, Location)?] = []
  let list = parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    fn(state) {
      item_starts.push(
        state
        .peek()
        .map(fn(item) {
          let (token, start, end) = item
          (token, mk_loc(start, end))
        }),
      )
      parse_brace_pattern_item(state)
    },
    List_RecordPatternFieldList,
    sep_kind=TokenKind::TK_COMMA,
  )
  validate_brace_pattern_list(state, list, false, item_starts)
  list
}

///|
fn token_starts_map_pattern_item(token : Token) -> Bool {
  token
  is (MINUS
  | STRING(_)
  | BYTES(_)
  | REGEX_LITERAL(_)
  | INT(_)
  | BYTE(_)
  | CHAR(_)
  | DOUBLE(_)
  | FLOAT(_)
  | TRUE
  | FALSE)
}

///|
fn parse_brace_pattern_item(state : ParserState) -> CstNode {
  match state.peek() {
    Some((token, _, _)) if token_starts_map_pattern_item(token) =>
      parse_map_pattern_item(state)
    _ => parse_record_pattern_item(state)
  }
}

///|
fn parse_record_pattern_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut kind = Pattern_MissingPattern
  match state.peek() {
    Some((DOTDOT, _, _)) => {
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      return state.node_from_children(Pattern_Rest, children)
    }
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) => {
      kind = Field_Pattern
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    }
    Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("error"), name)
      } else {
        ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let pat = parse_pattern_until(state, [
      TokenKind::TK_COMMA,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ])
    if !pat.children.is_empty() {
      push_child(children, Some("pattern"), pat)
    }
  } else if children.length() == 0 {
    let pat = parse_pattern_until(state, [
      TokenKind::TK_COMMA,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ])
    if !pat.children.is_empty() {
      push_child(children, Some("pattern"), pat)
    }
  }
  let field = state.node_from_children(kind, children)
  if kind is Field_Pattern {
    let is_pun = children
      .iter()
      .all(fn(entry) { !(entry.0 is Some("pattern")) })
    let field = field
      .with_structured_name("semantic_name", Name_Field, ["name"])
      .with_bool_marker("pun", Flag_Pun, is_pun)
    if is_pun {
      field.with_pun_pattern()
    } else {
      field
    }
  } else {
    field
  }
}

///|
fn parse_map_pattern_list(state : ParserState) -> CstNode {
  let item_starts : Array[(Token, Location)?] = []
  let list = parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    fn(state) {
      item_starts.push(
        state
        .peek()
        .map(fn(item) {
          let (token, start, end) = item
          (token, mk_loc(start, end))
        }),
      )
      parse_brace_pattern_item(state)
    },
    List_MapPatternFieldList,
    sep_kind=TokenKind::TK_COMMA,
  )
  validate_brace_pattern_list(state, list, true, item_starts)
  list
}

///|
fn parse_map_pattern_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut constant : CstNode? = None
  let mut negative = false
  match state.peek() {
    Some((found, start, end)) if found is REGEX_LITERAL(_) =>
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_LIDENT,
        TokenKind::TK_EXTEND,
        TokenKind::TK_TRUE,
        TokenKind::TK_FALSE,
        TokenKind::TK_CHAR,
        TokenKind::TK_INT,
        TokenKind::TK_FLOAT,
        TokenKind::TK_DOUBLE,
        TokenKind::TK_MINUS,
        TokenKind::TK_STRING,
        TokenKind::TK_REGEX_LITERAL,
      ])
    _ => ()
  }
  match state.peek() {
    Some((DOTDOT, _, _)) => {
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      return state.node_from_children(Pattern_Rest, children)
    }
    Some((MINUS, _, _)) => {
      negative = true
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
        push_child(
          children,
          Some("operator"),
          operator_node_from_token(
            Operator_UnaryMinus,
            MINUS,
            op.loc,
            op.source_span,
          ),
        )
      } else {
        ()
      }
      match state.peek() {
        Some((raw, _, _)) if raw is (INT(_) | DOUBLE(_) | FLOAT(_)) =>
          if state.consume_any() is Some(key) {
            push_child(children, Some("key"), key)
            constant = constant_node_from_token(raw, key.loc, key.source_span)
          } else {
            ()
          }
        Some((found, start, end)) => {
          state.report_expected_description_at(
            found,
            mk_loc(start, end),
            "`-Int` or `-Float` in key of map pattern",
          )
          if state.consume_any() is Some(key) {
            push_child(children, Some("key"), key)
          } else {
            ()
          }
          while !state.current_is(TokenKind::TK_RBRACE) &&
                !state.current_is(TokenKind::TK_EOF) &&
                !(state.peek() is None) {
            if state.consume_any() is Some(bad) {
              push_child(
                children,
                Some("error"),
                error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
              )
            } else {
              ()
            }
          }
          return state
            .node_from_children(Field_MapPattern, children)
            .with_bool_marker("match_absent", Flag_MatchAbsent, false)
        }
        None => ()
      }
    }
    Some((raw, _, _)) if raw
      is (STRING(_)
      | BYTES(_)
      | REGEX_LITERAL(_)
      | INT(_)
      | BYTE(_)
      | CHAR(_)
      | DOUBLE(_)
      | FLOAT(_)
      | TRUE
      | FALSE) =>
      if state.consume_any() is Some(key) {
        push_child(children, Some("key"), key)
        constant = constant_node_from_token(raw, key.loc, key.source_span)
      } else {
        ()
      }
    _ => ()
  }
  if constant is Some(value) {
    push_child(
      children,
      Some("constant"),
      signed_constant_node(value, negative),
    )
  }
  if state.current_is(TokenKind::TK_QUESTION) {
    if state.consume_any() is Some(question) {
      push_child(children, Some("question"), question)
    } else {
      ()
    }
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
  }
  let pat = parse_pattern_until(state, [
    TokenKind::TK_COMMA,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ])
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  state
  .node_from_children(Field_MapPattern, children)
  .with_bool_marker(
    "match_absent",
    Flag_MatchAbsent,
    children.iter().any(fn(entry) { entry.0 is Some("question") }),
  )
}

///|
fn parse_pattern_postfix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut pat = parse_pattern_atom(state, stop_kinds)
  let mut done = false
  while !done {
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((QUESTION, _, _)) => {
        let children : Array[(String?, CstNode)] = [(Some("pattern"), pat)]
        if state.consume_any() is Some(question) {
          push_child(children, Some("op"), question)
        } else {
          ()
        }
        pat = state.node_from_children(Pattern_Var, children)
      }
      Some((COLON, _, _)) => {
        let children : Array[(String?, CstNode)] = [(Some("pattern"), pat)]
        if state.consume_any() is Some(colon) {
          push_child(children, Some("op"), colon)
        } else {
          ()
        }
        let ty = parse_type_until(state, stop_kinds)
        if !ty.children.is_empty() {
          push_child(children, Some("type"), ty)
        }
        pat = state.node_from_children(Pattern_Constraint, children)
      }
      _ => done = true
    }
  }
  pat
}

///|
fn parse_pattern_or(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut lhs = parse_pattern_range(state, stop_kinds)
  let trivia = state.consume_trivia()
  if state.current_is(TokenKind::TK_BAR) {
    let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
    push_children(children, trivia)
    if state.consume_any() is Some(op) {
      push_child(children, Some("op"), op)
    } else {
      ()
    }
    let rhs = parse_pattern_or(state, stop_kinds)
    if !rhs.children.is_empty() {
      push_child(children, Some("rhs"), rhs)
    }
    lhs = state.node_from_children(Pattern_Or, children)
  }
  lhs
}

///|
fn parse_pattern_range(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut lhs = parse_pattern_postfix(state, stop_kinds)
  match state.peek() {
    Some((RANGE_EXCLUSIVE, _, _)) | Some((RANGE_INCLUSIVE, _, _)) => {
      let range_kind = if state.current_is(TokenKind::TK_RANGE_EXCLUSIVE) {
        RangeKind_Exclusive
      } else {
        RangeKind_Inclusive
      }
      let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let rhs = parse_pattern_postfix(state, stop_kinds)
      if !rhs.children.is_empty() {
        push_child(children, Some("rhs"), rhs)
      }
      lhs = with_marker_child(
        state.node_from_children(Pattern_Range, children),
        "range_kind",
        range_kind,
      )
    }
    _ => ()
  }
  lhs
}

///|
fn parse_pattern_alias(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut lhs = parse_pattern_or(state, stop_kinds)
  while state.current_is(TokenKind::TK_AS) {
    let children : Array[(String?, CstNode)] = [(Some("pattern"), lhs)]
    if state.consume_any() is Some(op) {
      push_child(children, Some("op"), op)
    } else {
      ()
    }
    if state.consume_any() is Some(name) {
      push_child(children, Some("name"), name)
    } else {
      ()
    }
    lhs = state.node_from_children(Pattern_Alias, children)
  }
  lhs
}

///|
fn parse_lex_pattern_until(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  allow_alias? : Bool = true,
) -> CstNode {
  parse_lex_sequence_pattern(state, stop_kinds, allow_alias~)
}

///|
fn lex_pattern_atom_can_start(tok : Token) -> Bool {
  match tok {
    REGEX_LITERAL(_)
    | REGEX_INTERP(_)
    | STRING(_)
    | INTERP(_)
    | UIDENT(_)
    | PACKAGE_NAME(_)
    | LPAREN => true
    _ => false
  }
}

///|
fn parse_lex_sequence_pattern(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  allow_alias? : Bool = true,
) -> CstNode {
  let items : Array[(String?, CstNode)] = []
  let mut done = false
  let mut needs_atom = true
  while !done {
    push_children(items, state.consume_trivia())
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => {
        if needs_atom {
          state.report_expected_description("lex atom pattern")
        }
        done = true
      }
      Some((tok, _, _)) if lex_pattern_atom_can_start(tok) => {
        let item = if allow_alias {
          parse_lex_alias_pattern(state, stop_kinds)
        } else {
          parse_lex_atom_pattern(state, stop_kinds)
        }
        push_child(items, Some("item"), item)
        needs_atom = false
        push_children(items, state.consume_trivia())
        if state.current_is(TokenKind::TK_SEMI) {
          if state.consume_any() is Some(sep) {
            push_child(items, Some("sep"), sep)
          } else {
            ()
          }
        }
      }
      Some((_, _, _)) => {
        if needs_atom {
          state.report_expected_description("lex atom pattern")
          if state.consume_any() is Some(bad) {
            push_child(
              items,
              Some("error"),
              error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
            )
          } else {
            ()
          }
        }
        done = true
      }
      None => {
        if needs_atom {
          state.report_expected_description("lex atom pattern")
        }
        done = true
      }
    }
  }
  let body = state.node_from_children(List_LexPatternList, items)
  state.node_from_children(LexPattern_Sequence, [(Some("body"), body)])
}

///|
fn parse_lex_alias_pattern(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let atom = parse_lex_atom_pattern(state, stop_kinds)
  if state.current_is(TokenKind::TK_AS) {
    let children : Array[(String?, CstNode)] = [(Some("pattern"), atom)]
    if state.consume_any() is Some(as_token) {
      push_child(children, Some("kw"), as_token)
    } else {
      ()
    }
    match state.peek() {
      Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
      Some((UIDENT(name), start, end)) => {
        state.report(
          mk_loc(start, end),
          "Unexpected uppercase identifier \{name}, expected lowercase identifier in binder in `as` pattern.",
        )
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
      }
      _ => {
        state.report_expected_tokens([
          TokenKind::TK_LIDENT,
          TokenKind::TK_EXTEND,
        ])
        push_child(
          children,
          Some("name"),
          state.missing_token_node_silent(TokenKind::TK_LIDENT),
        )
      }
    }
    state.node_from_children(LexPattern_Alias, children)
  } else {
    atom
  }
}

///|
fn parse_lex_atom_pattern(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LPAREN, _, _)) => {
      if state.consume_any() is Some(open) {
        push_child(children, Some("open"), open)
      } else {
        ()
      }
      let inner = parse_lex_sequence_pattern(state, [TokenKind::TK_RPAREN])
      push_child(
        children,
        Some("body"),
        inner.first_child_with_label("body").unwrap_or(inner),
      )
      let close = state.expect_token(TokenKind::TK_RPAREN)
      // A multi-item lex group historically takes its location from the next
      // significant token after `)`, so retain that lowering anchor.
      push_child(
        children,
        Some("handrolled_loc"),
        state.current_significant_anchor(),
      )
      push_child(children, Some("close"), close)
      state.node_from_children(LexPattern_Sequence, children)
    }
    Some((REGEX_INTERP(_), _, _)) | Some((INTERP(_), _, _)) =>
      parse_regex_interp_lex_pattern_node(state)
    Some((tok, _, _)) if tok is (REGEX_LITERAL(_) | STRING(_)) =>
      if state.consume_any() is Some(token) {
        let children : Array[(String?, CstNode)] = [
          (token_label_from_token(tok), token),
        ]
        if lex_literal_node_from_token(tok, token.loc, token.source_span)
          is Some(literal) {
          push_child(children, Some("lex_literal"), literal)
        }
        state.node_from_children(LexPattern_Literal, children)
      } else {
        state.node_from_children(LexPattern_Literal, [])
      }
    Some((UIDENT(_), _, _)) | Some((PACKAGE_NAME(_), _, _)) => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
      match state.peek() {
        Some((DOT_UIDENT(_), _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("name"), name)
          } else {
            ()
          }
        _ => ()
      }
      state
      .node_from_children(LexPattern_ConstantRef, children)
      .with_structured_long_ident("semantic_name", ["name"])
    }
    Some((tok, _, _)) if !kind_in(tok.kind(), stop_kinds) =>
      if state.consume_any() is Some(token) {
        state.node_from_children(LexPattern_Literal, [
          (token_label_from_token(tok), token),
        ])
      } else {
        state.node_from_children(LexPattern_Literal, [])
      }
    _ => state.node_from_children(LexPattern_Literal, [])
  }
}

///|
fn parse_regex_pattern_until(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  parse_regex_match_rhs(state, stop_kinds)
}

///|
fn parse_regex_match_rhs(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let _ = state.consume_trivia()
  if state.current_is(TokenKind::TK_LPAREN) {
    let children : Array[(String?, CstNode)] = []
    if state.consume_any() is Some(open) {
      push_child(children, Some("open"), open)
    } else {
      ()
    }
    let _ = state.consume_trivia()
    let reports_before_pattern = state.reports.length()
    let pat = parse_regex_as_pattern(state, [
      TokenKind::TK_COMMA,
      TokenKind::TK_RPAREN,
      TokenKind::TK_EOF,
    ])
    push_child(children, Some("pattern"), pat)
    let _ = state.consume_trivia()
    if state.current_is(TokenKind::TK_COMMA) {
      push_child(
        children,
        Some("bindings"),
        parse_regex_match_binding_list(state),
      )
    }
    let _ = state.consume_trivia()
    if state.current_is(TokenKind::TK_RPAREN) {
      push_child(
        children,
        Some("close"),
        state.expect_token(TokenKind::TK_RPAREN),
      )
    } else {
      if state.reports.length() == reports_before_pattern {
        state.report_expected_tokens([TokenKind::TK_RPAREN])
      }
      while state.peek() is Some((tok, _, _)) &&
            !kind_in(tok.kind(), stop_kinds) {
        if state.consume_any() is Some(bad) {
          push_child(
            children,
            Some("error"),
            error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
          )
        } else {
          break
        }
      }
      push_child(
        children,
        Some("close"),
        state.missing_token_node_silent(TokenKind::TK_RPAREN),
      )
    }
    state.node_from_children(RegexPattern_Placeholder, children)
  } else {
    parse_regex_atom_pattern(state, stop_kinds)
  }
}

///|
fn parse_regex_as_pattern(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let pat = parse_regex_atom_pattern(state, stop_kinds)
  let _ = state.consume_trivia()
  if state.current_is(TokenKind::TK_AS) {
    let children : Array[(String?, CstNode)] = [(Some("pattern"), pat)]
    if state.consume_any() is Some(as_token) {
      push_child(children, Some("kw"), as_token)
    } else {
      ()
    }
    match state.peek() {
      Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
      Some((UIDENT(name), start, end)) => {
        state.report(
          mk_loc(start, end),
          "Unexpected uppercase identifier \{name}, expected lowercase identifier in regex alias.",
        )
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
      }
      _ => {
        state.report_expected_tokens([
          TokenKind::TK_LIDENT,
          TokenKind::TK_EXTEND,
        ])
        push_child(
          children,
          Some("name"),
          state.missing_token_node_silent(TokenKind::TK_LIDENT),
        )
      }
    }
    state.node_from_children(RegexPattern_Alias, children)
  } else {
    let pat = parse_regex_sequence_pattern_tail(state, stop_kinds, pat)
    parse_regex_or_pattern_tail(state, stop_kinds, pat)
  }
}

///|
fn parse_regex_or_pattern_tail(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  first : CstNode,
) -> CstNode {
  let mut lhs = first
  let _ = state.consume_trivia()
  while state.current_is(TokenKind::TK_BAR) {
    let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
    if state.consume_any() is Some(op) {
      push_child(children, Some("op"), op)
    } else {
      ()
    }
    let reports_before_rhs = state.reports.length()
    let rhs = parse_regex_sequence_pattern(state, stop_kinds)
    if rhs.children.is_empty() && state.reports.length() == reports_before_rhs {
      state.report_expected_description("regex pattern")
    }
    push_child(children, Some("rhs"), rhs)
    lhs = state.node_from_children(RegexPattern_Alternation, children)
    let _ = state.consume_trivia()
  }
  lhs
}

///|
fn parse_regex_sequence_pattern(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let _ = state.consume_trivia()
  let lhs = parse_regex_atom_pattern(state, stop_kinds)
  parse_regex_sequence_pattern_tail(state, stop_kinds, lhs)
}

///|
fn parse_regex_sequence_pattern_tail(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  first : CstNode,
) -> CstNode {
  let mut lhs = first
  let _ = state.consume_trivia()
  while state.current_is(TokenKind::TK_PLUS) {
    let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
    if state.consume_any() is Some(op) {
      push_child(children, Some("op"), op)
    } else {
      ()
    }
    push_child(
      children,
      Some("rhs"),
      parse_regex_atom_pattern(state, stop_kinds),
    )
    lhs = state.node_from_children(RegexPattern_Sequence, children)
    let _ = state.consume_trivia()
  }
  lhs
}

///|
fn parse_regex_atom_pattern(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let _ = state.consume_trivia()
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LPAREN, _, _)) => {
      if state.consume_any() is Some(open) {
        push_child(children, Some("open"), open)
      } else {
        ()
      }
      let _ = state.consume_trivia()
      let reports_before_pattern = state.reports.length()
      let pat = parse_regex_as_pattern(state, [TokenKind::TK_RPAREN])
      push_child(children, Some("pattern"), pat)
      let _ = state.consume_trivia()
      if state.current_is(TokenKind::TK_RPAREN) {
        push_child(
          children,
          Some("close"),
          state.expect_token(TokenKind::TK_RPAREN),
        )
      } else if state.reports.length() > reports_before_pattern {
        push_child(
          children,
          Some("close"),
          state.missing_token_node_silent(TokenKind::TK_RPAREN),
        )
      } else {
        push_child(
          children,
          Some("close"),
          state.expect_token(TokenKind::TK_RPAREN),
        )
      }
      state.node_from_children(RegexPattern_Placeholder, children)
    }
    Some((raw, _, _)) if raw is REGEX_LITERAL(_) =>
      if state.consume_any() is Some(token) {
        let children : Array[(String?, CstNode)] = [(Some("token"), token)]
        if lex_literal_node_from_token(raw, token.loc, token.source_span)
          is Some(literal) {
          push_child(children, Some("lex_literal"), literal)
        }
        state.node_from_children(RegexPattern_Literal, children)
      } else {
        state.node_from_children(RegexPattern_Literal, [])
      }
    Some((raw, start, end)) if raw is STRING(_) => {
      state.report_expected_tokens_at(raw, mk_loc(start, end), [
        TokenKind::TK_REGEX_LITERAL,
      ])
      if state.consume_any() is Some(token) {
        let children : Array[(String?, CstNode)] = [(Some("token"), token)]
        if lex_literal_node_from_token(raw, token.loc, token.source_span)
          is Some(literal) {
          push_child(children, Some("lex_literal"), literal)
        }
        state.node_from_children(RegexPattern_Literal, children)
      } else {
        state.node_from_children(RegexPattern_Literal, [])
      }
    }
    Some((UIDENT(_), _, _)) => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
      state
      .node_from_children(RegexPattern_Reference, children)
      .with_structured_long_ident("semantic_name", ["name"])
    }
    Some((PACKAGE_NAME(_), _, _)) => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
      match state.peek() {
        Some((DOT_UIDENT(_), _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("name"), name)
          } else {
            ()
          }
        Some((DOT_LIDENT(name), start, end)) => {
          state.report(
            mk_loc(start, end),
            "Unexpected lowercase identifier \{name}, expected uppercase identifier in qualified upper identifier.",
          )
          if state.consume_any() is Some(name) {
            push_child(children, Some("name"), name)
          } else {
            ()
          }
        }
        _ => {
          state.report_expected_tokens([TokenKind::TK_DOT_UIDENT])
          push_child(
            children,
            Some("name"),
            state.missing_token_node_silent(TokenKind::TK_DOT_UIDENT),
          )
        }
      }
      state
      .node_from_children(RegexPattern_Reference, children)
      .with_structured_long_ident("semantic_name", ["name"])
    }
    Some((tok, start, end)) => {
      state.report_expected_description_at(
        tok,
        mk_loc(start, end),
        "regex pattern",
      )
      if !kind_in(tok.kind(), stop_kinds) {
        if state.consume_any() is Some(token) {
          push_child(
            children,
            Some("error"),
            error_node(token.loc, token.source_span, [(Some("item"), token)]),
          )
        } else {
          ()
        }
      }
      state.node_from_children(RegexPattern_Literal, children)
    }
    None => {
      state.report_expected_description("regex pattern")
      state.node_from_children(RegexPattern_Literal, [])
    }
  }
}

///|
fn parse_regex_match_binding_list(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  while state.current_is(TokenKind::TK_COMMA) {
    if state.consume_any() is Some(sep) {
      push_child(children, Some("sep"), sep)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
    if state.current_is(TokenKind::TK_RPAREN) ||
      state.current_is(TokenKind::TK_EOF) {
      break
    }
    push_child(children, Some("item"), parse_regex_match_binding(state))
    push_children(children, state.consume_trivia())
  }
  state.node_from_children(List_RegexBindingList, children)
}

///|
fn parse_regex_match_binding(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((POST_LABEL(_), _, _)) =>
      if state.consume_any() is Some(label) {
        push_child(children, Some("name"), label)
      } else {
        ()
      }
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.peek_nth_significant(1).map(item => item.0) is Some(EQUAL) {
        if state.consume_any() is Some(label) {
          push_child(children, Some("name"), label)
        } else {
          ()
        }
        push_children(children, state.consume_trivia())
        if state.consume_any() is Some(equal) {
          push_child(children, Some("op"), equal)
        } else {
          ()
        }
        push_children(children, state.consume_trivia())
        match state.peek() {
          Some((LIDENT(_), _, _))
          | Some((EXTEND, _, _))
          | Some((UNDERSCORE, _, _)) =>
            if state.consume_any() is Some(name) {
              push_child(children, Some("binder"), name)
            } else {
              ()
            }
          Some((UIDENT(name), start, end)) => {
            state.report(
              mk_loc(start, end),
              "Unexpected uppercase identifier \{name}, expected lowercase identifier in regex match binding.",
            )
            if state.consume_any() is Some(name) {
              push_child(children, Some("binder"), name)
            } else {
              ()
            }
          }
          _ => {
            state.report_expected_tokens([
              TokenKind::TK_LIDENT,
              TokenKind::TK_EXTEND,
            ])
            push_child(
              children,
              Some("binder"),
              state.missing_token_node_silent(TokenKind::TK_LIDENT),
            )
          }
        }
      } else {
        state.report_expected_tokens([
          TokenKind::TK_LIDENT,
          TokenKind::TK_POST_LABEL,
        ])
        push_child(
          children,
          Some("name"),
          state.missing_token_node_silent(TokenKind::TK_LIDENT),
        )
      }
    _ => {
      state.report_expected_tokens([
        TokenKind::TK_LIDENT,
        TokenKind::TK_POST_LABEL,
      ])
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TokenKind::TK_LIDENT),
      )
    }
  }
  state
  .node_from_children(Binding_Regex, children)
  .with_structured_name("semantic_label", Name_Label, ["name"])
  .with_structured_name("semantic_binder", Name_Binder, ["binder"])
}

///|
fn loc_span(state : ParserState, loc : Location) -> (Int, Int) {
  mk_span(
    state.offset_for_position(loc.start),
    state.offset_for_position(loc.end),
  )
}

///|
fn parse_string_literal_node(state : ParserState) -> CstNode {
  match state.peek() {
    Some((MULTILINE_STRING(_), _, _)) | Some((MULTILINE_INTERP(_), _, _)) =>
      parse_multiline_string_literal(state)
    Some((tok, _, _)) =>
      if state.consume_any() is Some(token_node) {
        let children : Array[(String?, CstNode)] = [(Some("token"), token_node)]
        if constant_node_from_token(tok, token_node.loc, token_node.source_span)
          is Some(constant) {
          push_child(children, Some("constant"), constant)
        }
        let kind = match tok {
          INTERP(_) => Expr_Interp
          BYTES_INTERP(_) => Expr_BytesInterp
          _ => Expr_Constant
        }
        state.node_from_children(kind, children)
      } else {
        state.node_from_children(Expr_MissingExpr, [])
      }
    None => state.node_from_children(Expr_MissingExpr, [])
  }
}

///|
fn parse_multiline_string_literal(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut first_token : CstNode? = None
  let mut last_token : CstNode? = None
  let mut done = false
  while !done {
    match state.peek() {
      Some((tok, _, _)) =>
        match tok {
          MULTILINE_STRING(_) | MULTILINE_INTERP(_) =>
            if state.consume_any() is Some(token_node) {
              if first_token is None {
                first_token = Some(token_node)
              }
              last_token = Some(token_node)
              push_child(children, Some("token"), token_node)
              let element = match tok {
                MULTILINE_STRING(value) =>
                  scalar_node(
                    MultilineStringElement_String,
                    token_node.loc,
                    token_node.source_span,
                    String(value),
                  )
                MULTILINE_INTERP(_) => {
                  let segments : Array[(String?, CstNode)] = []
                  for segment in token_node.children_with_label("segment") {
                    push_child(segments, Some("segment"), segment)
                  }
                  node(
                    MultilineStringElement_Interp,
                    token_node.loc,
                    token_node.source_span,
                    segments,
                  )
                }
                _ =>
                  node(
                    MultilineStringElement_Interp,
                    token_node.loc,
                    token_node.source_span,
                    [],
                  )
              }
              push_child(children, Some("element"), element)
              if state.next_significant_is_multiline_string() {
                push_children(children, state.consume_trivia())
              }
            } else {
              done = true
            }
          _ => done = true
        }
      None => done = true
    }
  }
  let result = state.node_from_children(Expr_MultilineString, children)
  match (first_token, last_token) {
    (Some(first), Some(last)) =>
      result.with_aggregate_span(
        "literal_loc",
        first.loc.merge(last.loc),
        mk_span(first.source_span.0, last.source_span.1),
      )
    _ => result
  }
}

///|
fn ParserState::next_significant_is_multiline_string(
  self : ParserState,
) -> Bool {
  match self.peek_significant() {
    Some((MULTILINE_STRING(_), _, _)) | Some((MULTILINE_INTERP(_), _, _)) =>
      true
    _ => false
  }
}

///|
fn parse_regex_interp_lex_pattern_node(state : ParserState) -> CstNode {
  match state.peek() {
    Some((tok, _, _)) =>
      match tok {
        REGEX_INTERP(_) | INTERP(_) =>
          if state.consume_any() is Some(token_node) {
            let children : Array[(String?, CstNode)] = [
              (Some("token"), token_node),
            ]
            node(
              LexPattern_RegexInterp,
              token_node.loc,
              token_node.source_span,
              children,
            )
          } else {
            state.node_from_children(LexPattern_RegexInterp, [])
          }
        _ => state.node_from_children(LexPattern_RegexInterp, [])
      }
    _ => state.node_from_children(LexPattern_RegexInterp, [])
  }
}

///|
fn parse_interp_segment(
  state : ParserState,
  elem : @tokens.InterpElem,
) -> CstNode {
  match elem {
    InterpLit(repr~, loc~) =>
      node_with_payload(
        InterpSegment_Literal,
        loc,
        loc_span(state, loc),
        [],
        String(repr),
      )
    InterpSource(interp_source) =>
      parse_interp_source_segment(state, interp_source)
  }
}

///|
fn parse_interp_source_segment(
  state : ParserState,
  interp_source : InterpSource,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let expr = parse_interp_source_expr(state, interp_source)
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  node_with_payload(
    InterpSegment_Source,
    interp_source.loc,
    loc_span(state, interp_source.loc),
    children,
    String(interp_source.source),
  )
}

///|
fn parse_interp_source_expr(
  state : ParserState,
  interp_source : InterpSource,
) -> CstNode {
  let lex = @lexer.tokens_from_string(
    interp_source.source,
    start_pos=interp_source.loc.start,
    is_interpolation=true,
    enable_metavar=state.enable_metavar,
    comment=true,
  )
  for report in reports_from_lex_errors(lex.errors) {
    state.reports.push(report)
  }
  let inner_state = new_state(
    lex.tokens,
    state.source,
    state.reports,
    state.enable_metavar,
  )
  parse_expr_until(inner_state, [TokenKind::TK_EOF])
}

///|
fn token_label(tok : Token) -> String? {
  match tok {
    SEMI(_) => Some("sep")
    EOF => Some("eof")
    _ => Some("item")
  }
}

///|
// Keep this lookahead aligned with parse_visibility_clause, including its
// recovery behavior for invalid and missing `pub(...)` modifiers.
fn top_item_lookahead_after_visibility(
  state : ParserState,
  offset : Int,
  is_pub : Bool,
) -> Int {
  let mut offset = offset + 1
  if !is_pub {
    return offset
  }
  match state.peek(offset~) {
    Some((LPAREN, _, _)) => offset += 1
    _ => return offset
  }
  match state.peek(offset~) {
    Some((LIDENT(_), _, _)) | Some((READONLY, _, _)) | Some((EXTEND, _, _)) => {
      offset += 1
      match state.peek(offset~) {
        Some((RPAREN, _, _)) => offset += 1
        _ => ()
      }
    }
    _ => {
      let mut done = false
      while !done {
        match state.peek(offset~) {
          Some((RPAREN, _, _)) => {
            offset += 1
            done = true
          }
          Some((EOF, _, _)) | None => done = true
          Some(_) => offset += 1
        }
      }
    }
  }
  offset
}

///|
fn peek_top_item_kind(state : ParserState) -> TopItemKind {
  let mut offset = 0
  let mut result = TopItemKind::Invalid
  let mut saw_async = false
  let mut done = false
  while !done {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((ATTRIBUTE(_), _, _)) | Some((DECLARE, _, _)) => offset += 1
      Some((PUB, _, _)) =>
        offset = top_item_lookahead_after_visibility(state, offset, true)
      Some((PRIV, _, _)) =>
        offset = top_item_lookahead_after_visibility(state, offset, false)
      Some((ASYNC, _, _)) => {
        saw_async = true
        offset += 1
      }
      Some((EXTERN, _, _)) => {
        result = TopItemKind::Function
        done = true
      }
      Some((TEST, _, _)) => {
        result = TopItemKind::Test
        done = true
      }
      Some((USING, _, _)) => {
        result = TopItemKind::Using
        done = true
      }
      Some((IMPORT, _, _)) => {
        result = Import
        done = true
      }
      Some((EXTEND, _, _)) => {
        result = Extend
        done = true
      }
      Some((LET, _, _)) => {
        result = TopItemKind::Let
        done = true
      }
      Some((tok, _, _)) if decl_keyword_expects_name(tok) => {
        result = decl_keyword_kind(tok)
        done = true
      }
      Some((QUESTION, _, _)) => offset += 1
      _ => {
        result = if saw_async {
          TopItemKind::Function
        } else {
          TopItemKind::Invalid
        }
        done = true
      }
    }
  }
  result
}

///|
fn parser_decl_loc_without_trailing_trivia(node : CstNode) -> Location {
  let mut end = node.loc.start
  let mut found = false
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some("trivia"), _)) => ()
      Some((_, child)) if child.kind.is_syntax_flag() ||
        child.kind.is_type_desc() ||
        child.kind.is_decl_body() ||
        child.kind.is_name() ||
        child.kind.is_accessor() => ()
      Some((_, child)) => {
        end = child.loc.end
        found = true
      }
      None => ()
    }
  }
  if found {
    Location::{ start: node.loc.start, end, }
  } else {
    node.loc
  }
}

///|
fn parser_function_header_loc(node : CstNode) -> Location {
  let start = node.loc.start
  let mut end = start
  for i in 0..<node.children.length() {
    match node.children.get(i) {
      Some((Some("body" | "where" | "stub" | "stub_token" | "trivia"), _)) => ()
      Some((Some("op"), child)) if child.first_operator_descendant()
        is Some(operator) &&
        operator.string_payload() is Some("=") => ()
      Some((_, child)) if child.kind.is_syntax_flag() ||
        child.kind.is_type_desc() ||
        child.kind.is_decl_body() ||
        child.kind.is_name() ||
        child.kind.is_accessor() => ()
      Some((_, child)) => end = child.loc.end
      None => ()
    }
  }
  Location::{ start, end, }
}

///|
fn parser_trait_decl_loc(node : CstNode) -> Location {
  let semantic_loc = parser_decl_loc_without_trailing_trivia(node)
  if node.children_with_label("kw").get(0) is Some(keyword) {
    Location::{ start: keyword.loc.start, end: semantic_loc.end, }
  } else {
    semantic_loc
  }
}

///|
fn with_top_level_location_anchors(
  state : ParserState,
  source : CstNode,
) -> CstNode {
  let decl_loc = if source.kind is Impl_Trait {
    parser_trait_decl_loc(source)
  } else {
    parser_decl_loc_without_trailing_trivia(source)
  }
  let mut result = source
    .with_async_location_anchor()
    .with_aggregate_span("decl_loc", decl_loc, loc_span(state, decl_loc))
  if source.kind is Impl_Function {
    let header_loc = parser_function_header_loc(source)
    result = result.with_aggregate_span(
      "header_loc",
      header_loc,
      loc_span(state, header_loc),
    )
  }
  result
}

///|
fn ParserState::node_from_children(
  self : ParserState,
  kind : NodeKind,
  children : Array[(String?, CstNode)],
) -> CstNode {
  let result = if children.length() == 0 {
    self.missing_syntax_node(kind)
  } else {
    let (_, first) = children[0]
    let (_, last) = children[children.length() - 1]
    node(
      kind,
      first.loc.merge(last.loc),
      mk_span(first.source_span.0, last.source_span.1),
      children,
    )
  }
  match kind {
    Expr_Constant | Pattern_Constant | Field_MapExpression | Field_MapPattern =>
      result.with_direct_semantic_constant()
    _ => result
  }
}

///|
fn parse_modern_decl_prefixes(
  state : ParserState,
  children : Array[(String?, CstNode)],
  allow_attrs~ : Bool,
  allow_visibility~ : Bool,
) -> Unit {
  let mut done = false
  let mut saw_visibility = false
  while !done {
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((ATTRIBUTE(_), start, end)) => {
        if !allow_attrs {
          state.report(
            mk_loc(start, end),
            "Import declarations cannot have attributes or other modifiers.",
          )
        } else if saw_visibility {
          state.report(
            mk_loc(start, end),
            "Attributes must appear before a visibility modifier.",
          )
        }
        if state.consume_any() is Some(attr) {
          push_child(children, Some("attrs"), attr)
        }
      }
      Some((PUB, start, end)) | Some((PRIV, start, end)) => {
        if !allow_visibility {
          state.report(
            mk_loc(start, end),
            "Import declarations cannot have visibility or other modifiers.",
          )
        } else if saw_visibility {
          state.report(
            mk_loc(start, end),
            "Only one visibility modifier is allowed.",
          )
        }
        push_child(children, Some("vis"), parse_visibility_clause(state))
        saw_visibility = true
      }
      Some((DECLARE, start, end)) | Some((ASYNC, start, end)) => {
        state.report(
          mk_loc(start, end),
          "This modifier is not allowed on this declaration.",
        )
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        }
      }
      _ => done = true
    }
  }
}

///|
fn parse_import_package_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((STRING(_), _, _)) =>
      if state.consume_any() is Some(path) {
        push_child(children, Some("path"), path)
      }
    _ =>
      push_child(
        children,
        Some("path"),
        state.missing_token_node_with_expected(TK_STRING, [TK_STRING]),
      )
  }
  push_children(children, state.consume_trivia())
  match state.peek() {
    Some((PACKAGE_NAME(_), _, _)) =>
      if state.consume_any() is Some(alias_) {
        push_child(children, Some("alias"), alias_)
      }
    _ => ()
  }
  state.node_from_children(Clause_ImportPackage, children)
}

///|
fn parse_extend_name_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(target) {
        push_child(children, Some("target"), target)
      }
    _ =>
      push_child(
        children,
        Some("target"),
        state.missing_token_node_with_expected(TK_LIDENT, [TK_LIDENT, TK_EXTEND]),
      )
  }
  push_children(children, state.consume_trivia())
  if state.current_is(TK_AS) {
    push_child(children, Some("kw"), state.expect_token(TK_AS))
    push_children(children, state.consume_trivia())
    match state.peek() {
      Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        }
      _ =>
        push_child(
          children,
          Some("name"),
          state.missing_token_node_with_expected(TK_LIDENT, [
            TK_LIDENT,
            TK_EXTEND,
          ]),
        )
    }
  }
  state
  .node_from_children(Clause_ExtendItem, children)
  .with_structured_name("semantic_name", Name_AliasTarget, ["target", "name"])
}

///|
fn parse_import_decl(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  parse_modern_decl_prefixes(
    state,
    children,
    allow_attrs=false,
    allow_visibility=false,
  )
  push_child(children, Some("kw"), state.expect_token(TK_IMPORT))
  push_children(children, state.consume_trivia())
  if state.current_is(TK_STRING) {
    let old = state.expect_token(TK_STRING)
    state.report(
      old.loc,
      "Deprecated import syntax; use `import { \"path\" }`.",
    )
    push_child(children, Some("error"), old)
  } else {
    push_child(
      children,
      Some("body"),
      parse_delimited(
        state,
        TK_LBRACE,
        TK_RBRACE,
        parse_import_package_item,
        List_ImportPackageList,
        sep_kind=TK_COMMA,
      ),
    )
    push_children(children, state.consume_trivia())
    if state.current_is(TK_FOR) {
      push_child(children, Some("kw"), state.expect_token(TK_FOR))
      push_children(children, state.consume_trivia())
      match state.peek() {
        Some((STRING(_), _, _)) =>
          if state.consume_any() is Some(kind) {
            push_child(children, Some("kind"), kind)
          }
        _ =>
          push_child(
            children,
            Some("kind"),
            state.missing_token_node_with_expected(TK_STRING, [TK_STRING]),
          )
      }
    }
  }
  state.node_from_children(Impl_Import, children)
}

///|
fn parse_extend_decl(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  parse_modern_decl_prefixes(
    state,
    children,
    allow_attrs=true,
    allow_visibility=true,
  )
  push_child(children, Some("kw"), state.expect_token(TK_EXTEND))
  push_children(children, state.consume_trivia())
  push_child(children, Some("type_name"), parse_type_name_node(state))
  push_children(children, state.consume_trivia())
  push_child(children, Some("kw"), state.expect_token(TK_WITH))
  push_children(children, state.consume_trivia())
  push_child(children, Some("trait"), parse_type_name_node(state))
  push_children(children, state.consume_trivia())
  push_child(children, Some("op"), state.expect_token(TK_COLONCOLON))
  push_children(children, state.consume_trivia())
  push_child(
    children,
    Some("body"),
    parse_delimited(
      state,
      TK_LBRACE,
      TK_RBRACE,
      parse_extend_name_item,
      List_ExtendItemList,
      sep_kind=TK_COMMA,
    ),
  )
  state.node_from_children(Impl_Extend, children)
}

///|
fn parse_top_level_item(state : ParserState) -> CstNode {
  let first_kind = peek_top_item_kind(state)
  with_top_level_location_anchors(
    state,
    match first_kind {
      Import => parse_import_decl(state)
      Extend => parse_extend_decl(state)
      _ => parse_balanced_decl_body(state, first_kind)
    },
  )
}

///|
fn parse_source_file(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut saw_eof = false
  let mut done = false
  while !done {
    match state.peek() {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) =>
        for child in state.consume_trivia() {
          children.push(child)
        }
      Some((SEMI(_), _, _)) =>
        if state.consume_any() is Some(sep) {
          children.push((Some("sep"), sep))
        } else {
          ()
        }
      Some((EOF, _, _)) => {
        if state.consume_any() is Some(eof) {
          children.push((Some("eof"), eof))
        } else {
          ()
        }
        saw_eof = true
        done = true
      }
      Some((_, start, end)) => {
        if state.report_next_top_level_start {
          state.report_next_top_level_start = false
          state.report(mk_loc(start, end), "Unexpected token here.")
        }
        children.push((Some("item"), parse_top_level_item(state)))
      }
      None => done = true
    }
  }
  if !saw_eof {
    children.push((Some("eof"), state.missing_eof_node()))
  }
  node(Impls, state.root_loc(), state.root_span(), children)
}

///|
fn parse_type_until(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  parse_type_arrow(state, stop_kinds)
}

///|
fn parse_type_atom(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  match state.peek() {
    Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) =>
      state.node_from_children(Type_MissingType, [])
    Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => {
      let children = state.consume_trivia()
      let ty = parse_type_atom(state, stop_kinds)
      if !ty.children.is_empty() {
        push_child(children, Some("type"), ty)
      }
      state.node_from_children(Type_Group, children)
    }
    Some((AMPER, _, _)) => parse_object_type_atom(state)
    Some((UIDENT(_), _, _)) | Some((PACKAGE_NAME(_), _, _)) =>
      parse_type_name_atom(state)
    Some((UNDERSCORE, _, _)) => parse_single_token_type(state, Type_Any)
    Some((LPAREN, _, _)) => parse_paren_type(state, stop_kinds)
    Some((LBRACE, start, end)) => {
      state.report_expected_description_at(LBRACE, mk_loc(start, end), "type")
      let reports_to_keep = state.reports.length()
      let ty = parse_object_type_literal(state)
      while state.reports.length() > reports_to_keep {
        let _ = state.reports.pop()
      }
      ty
    }
    Some((ASYNC, _, _)) => parse_async_type_atom(state, stop_kinds)
    Some((found, start, end)) => {
      state.report_expected_description_at(found, mk_loc(start, end), "type")
      let children : Array[(String?, CstNode)] = []
      while !state.current_in(stop_kinds) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(token) {
          push_child(children, Some("error"), token)
        } else {
          ()
        }
      }
      state.node_from_children(Type_MissingType, children)
    }
    None => state.node_from_children(Type_MissingType, [])
  }
}

///|
fn parse_single_token_type(state : ParserState, kind : NodeKind) -> CstNode {
  if state.consume_any() is Some(token) {
    state.node_from_children(kind, [(Some("name"), token)])
  } else {
    state.node_from_children(Type_MissingType, [])
  }
}

///|
fn parse_type_name_atom(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(name) {
    push_child(children, Some("name"), name)
  } else {
    ()
  }
  match state.peek() {
    Some((DOT_LIDENT(_), _, _)) | Some((DOT_UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  state
  .node_from_children(Type_Name, children)
  .with_structured_name("semantic_name", Name_ConstrId, ["name"])
}

///|
fn parse_object_type_atom(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(op) {
    push_child(children, Some("op"), op)
  } else {
    ()
  }
  match state.peek() {
    Some((UIDENT(_), _, _)) | Some((PACKAGE_NAME(_), _, _)) => {
      let name = parse_type_name_atom(state)
      if !name.children.is_empty() {
        push_child(children, Some("type"), name)
      }
    }
    _ =>
      push_child(
        children,
        Some("type"),
        state.missing_token_node(TokenKind::TK_UIDENT),
      )
  }
  state.node_from_children(Type_Object, children)
}

///|
fn parse_async_type_atom(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  match state.peek_significant() {
    Some((LPAREN, _, _)) => ()
    Some((found, start, end)) => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_LPAREN,
      ])
      if top_level_decl_can_start(found) {
        push_children(children, state.consume_trivia())
        return state
          .node_from_children(Type_Arrow, children)
          .with_bool_marker("async", Flag_Async, true)
          .with_async_location_anchor()
      }
      while !state.current_in(stop_kinds) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(children, Some("error"), bad)
        } else {
          ()
        }
      }
      return state
        .node_from_children(Type_Arrow, children)
        .with_bool_marker("async", Flag_Async, true)
        .with_async_location_anchor()
    }
    None => state.report_expected_tokens([TokenKind::TK_LPAREN])
  }
  let ty = parse_type_arrow(state, stop_kinds)
  if !ty.children.is_empty() {
    push_child(children, Some("type"), ty)
  }
  state
  .node_from_children(Type_Arrow, children)
  .with_bool_marker("async", Flag_Async, true)
  .with_async_location_anchor()
}

///|
fn parse_paren_type_item(state : ParserState) -> CstNode {
  parse_type_arrow(state, [
    TokenKind::TK_COMMA,
    TokenKind::TK_RPAREN,
    TokenKind::TK_EOF,
  ])
}

///|
fn parse_paren_type(
  state : ParserState,
  _stop_kinds : Array[TokenKind],
) -> CstNode {
  let list = parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_paren_type_item,
    List_TupleTypeList,
    sep_kind=TokenKind::TK_COMMA,
  )
  let mut item_count = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), _)) => item_count += 1
      _ => ()
    }
  }
  let kind = if item_count == 1 { Type_Group } else { Type_Tuple }
  if item_count == 0 && !state.current_is(TokenKind::TK_THIN_ARROW) {
    state.report_expected_tokens([TokenKind::TK_THIN_ARROW])
  }
  state.node_from_children(kind, [(Some("body"), list)])
}

///|
fn parse_object_field_type_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let ty = parse_type_arrow(state, [
      TokenKind::TK_COMMA,
      TokenKind::TK_RBRACE,
      TokenKind::TK_EOF,
    ])
    if !ty.children.is_empty() {
      push_child(children, Some("type"), ty)
    }
  }
  with_marker_child(
    state
    .node_from_children(Field_Declaration, children)
    .with_structured_name("semantic_name", Name_Field, ["name"])
    .with_bool_marker("mutable", Flag_Mutable, false),
    "vis",
    Visibility_Default,
  )
}

///|
fn parse_object_type_literal(state : ParserState) -> CstNode {
  let list = parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_object_field_type_item,
    List_ObjectFieldList,
    sep_kind=TokenKind::TK_COMMA,
  )
  state.node_from_children(Type_Object, [(Some("body"), list)])
}

///|
fn parse_type_argument_item(state : ParserState) -> CstNode {
  parse_type_arrow(state, [
    TokenKind::TK_COMMA,
    TokenKind::TK_RBRACKET,
    TokenKind::TK_EOF,
  ])
}

///|
fn parse_type_argument_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACKET,
    TokenKind::TK_RBRACKET,
    parse_type_argument_item,
    List_TypeArgumentList,
    sep_kind=TokenKind::TK_COMMA,
    follow_set=[TokenKind::TK_WITH, TokenKind::TK_LBRACE, TokenKind::TK_SEMI],
    require_nonempty=true,
  )
}

///|
fn parse_legacy_type_tail(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  head : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = [(Some("head"), head)]
  let mut depth = 0
  let mut done = false
  while !done {
    match state.peek() {
      Some((tok, _, _)) if depth == 0 && kind_in(tok.kind(), stop_kinds) =>
        done = true
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) =>
        push_children(children, state.consume_trivia())
      Some((tok, _, _)) => {
        if state.consume_any() is Some(consumed) {
          push_child(children, token_label(tok), consumed)
        } else {
          done = true
        }
        depth = next_depth(depth, tok)
      }
      None => done = true
    }
  }
  state.node_from_children(Type_Application, children)
}

///|
fn parse_simple_type(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  parse_type_atom(state, stop_kinds)
}

///|
fn parse_type_postfix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let mut ty = parse_simple_type(state, stop_kinds)
  let mut done = false
  while !done {
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((LBRACKET, _, _)) => {
        let children : Array[(String?, CstNode)] = [(Some("func"), ty)]
        push_child(children, Some("arg"), parse_type_argument_list(state))
        ty = state.node_from_children(Type_Application, children)
      }
      Some((QUESTION, start, _)) if ty.loc.end.cnum == start.cnum => {
        let children : Array[(String?, CstNode)] = [(Some("type"), ty)]
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
          push_direct_operator(children, op)
        } else {
          ()
        }
        ty = state.node_from_children(Type_Option, children)
      }
      _ => done = true
    }
  }
  match state.peek() {
    Some((tok, _, _)) if !kind_in(tok.kind(), stop_kinds) =>
      match tok {
        DOT_UIDENT(_) | DOT_LIDENT(_) | COLONCOLON =>
          parse_legacy_type_tail(state, stop_kinds, ty)
        _ => ty
      }
    _ => ty
  }
}

///|
fn parse_type_arrow(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let lhs = parse_type_postfix(state, stop_kinds)
  match state.peek() {
    Some((THIN_ARROW, start, end)) if !kind_in(
        TokenKind::TK_THIN_ARROW,
        stop_kinds,
      ) => {
      let invalid_unparenthesized = match lhs.kind {
        Type_Group | Type_Tuple => false
        _ => true
      }
      let _ = (start, end)
      let children : Array[(String?, CstNode)] = [(Some("param"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let rhs = parse_type_arrow(state, stop_kinds)
      if !rhs.children.is_empty() {
        push_child(children, Some("result"), rhs)
      }
      match state.peek() {
        Some((RAISE, _, _)) if !kind_in(TokenKind::TK_RAISE, stop_kinds) =>
          parse_effect_annotation_into(state, children)
        Some((NORAISE, _, _)) if !kind_in(TokenKind::TK_NORAISE, stop_kinds) =>
          parse_effect_annotation_into(state, children)
        Some((NOCANCEL, _, _)) if !kind_in(TokenKind::TK_NOCANCEL, stop_kinds) =>
          parse_effect_annotation_into(state, children)
        _ => ()
      }
      if invalid_unparenthesized {
        state.report(
          empty_loc(state.current_position()),
          "Unexpected token here.",
        )
      }
      state.node_from_children(Type_Arrow, children)
    }
    _ => lhs
  }
}

///|
fn token_starts_impl_for_missing_return_type(tok : Token) -> Bool {
  match tok {
    PUB
    | PRIV
    | TYPE
    | SUBERROR
    | FN
    | STRUCT
    | ENUM
    | EXTENUM
    | LET
    | CONST
    | EXTERN
    | TEST
    | IMPL
    | TRAIT
    | ENUMVIEW
    | ATTRIBUTE(_)
    | USING => true
    _ => false
  }
}

///|
fn token_requires_missing_return_type(tok : Token) -> Bool {
  match tok {
    LBRACE | EQUAL | EOF => true
    _ => token_starts_impl_for_missing_return_type(tok)
  }
}

///|
fn parse_return_type(state : ParserState) -> CstNode {
  match state.peek_significant() {
    Some((found, start, end)) if token_requires_missing_return_type(found) => {
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "return type annotation",
      )
      state.missing_type_node()
    }
    Some((LIDENT(name), start, end)) => {
      state.report(
        mk_loc(start, end),
        "Unexpected lowercase identifier \{name}, expected uppercase identifier in type name.",
      )
      let children = state.consume_trivia()
      if state.consume_any() is Some(bad) {
        push_child(children, Some("error"), bad)
      } else {
        ()
      }
      state.node_from_children(Type_MissingType, children)
    }
    _ =>
      parse_type_arrow(state, [
        TokenKind::TK_RAISE,
        TokenKind::TK_NORAISE,
        TokenKind::TK_NOCANCEL,
        TokenKind::TK_LBRACE,
        TokenKind::TK_EQUAL,
        TokenKind::TK_SEMI,
        TokenKind::TK_EOF,
      ])
  }
}

///|
fn parse_effect_annotation_into(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> Unit {
  if state.current_is(TokenKind::TK_NOCANCEL) {
    if state.consume_any() is Some(nocancel_anchor) {
      push_child(children, Some("nocancel"), nocancel_anchor)
    }
    push_children(children, state.consume_trivia())
    if state.current_is(TokenKind::TK_PLUS) {
      if state.consume_any() is Some(op) {
        push_child(children, Some("effect_op"), op)
      }
      push_children(children, state.consume_trivia())
      if state.current_is(TokenKind::TK_RAISE) ||
        state.current_is(TokenKind::TK_NORAISE) {
        push_child(children, Some("clause"), parse_error_annotation(state))
      } else {
        state.report_expected_tokens([
          TokenKind::TK_RAISE,
          TokenKind::TK_NORAISE,
        ])
      }
    }
  } else {
    push_child(children, Some("clause"), parse_error_annotation(state))
    push_children(children, state.consume_trivia())
    if state.current_is(TokenKind::TK_PLUS) {
      if state.consume_any() is Some(op) {
        push_child(children, Some("effect_op"), op)
      }
      push_children(children, state.consume_trivia())
      if state.current_is(TokenKind::TK_NOCANCEL) {
        if state.consume_any() is Some(nocancel_anchor) {
          push_child(children, Some("nocancel"), nocancel_anchor)
        }
      } else {
        state.report_expected_tokens([TokenKind::TK_NOCANCEL])
      }
    }
  }
}

///|
fn parse_func_return_type(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut has_arrow = false
  match state.peek() {
    Some((THIN_ARROW, _, _)) =>
      if state.consume_any() is Some(arrow) {
        has_arrow = true
        push_child(children, Some("op"), arrow)
      } else {
        ()
      }
    _ => ()
  }
  if children.length() > 0 {
    let ty = parse_return_type(state)
    if !ty.children.is_empty() {
      push_child(children, Some("type"), ty)
    } else if has_arrow && !(ty.kind is Type_MissingType) {
      state.report_expected_description("return type annotation")
    }
    if ty.kind is Type_MissingType {
      push_child(children, Some("type"), ty)
    }
  }
  match state.peek() {
    Some((RAISE, _, _)) | Some((NORAISE, _, _)) | Some((NOCANCEL, _, _)) =>
      parse_effect_annotation_into(state, children)
    _ => ()
  }
  state.node_from_children(Clause_ReturnType, children)
}

///|
fn parse_error_annotation(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut error_kind = ErrorType_Default
  match state.peek() {
    Some((RAISE, _, _)) =>
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
    Some((NORAISE, _, _)) => {
      error_kind = ErrorType_Noraise
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
    }
    _ => ()
  }
  match (state.peek(), state.peek(offset=1)) {
    (Some((UIDENT(_), _, _)), Some((QUESTION, _, _))) => {
      let type_children : Array[(String?, CstNode)] = []
      if state.consume_any() is Some(name) {
        push_child(type_children, Some("name"), name)
      } else {
        ()
      }
      if state.consume_any() is Some(question) {
        push_child(type_children, Some("question"), question)
      } else {
        ()
      }
      push_child(
        children,
        Some("type"),
        state
        .node_from_children(Type_Name, type_children)
        .with_structured_name("semantic_name", Name_ConstrId, ["name"]),
      )
      error_kind = ErrorType_Error
    }
    (Some((UIDENT(_), _, _)), Some((LBRACKET, start, end))) => {
      let type_children : Array[(String?, CstNode)] = []
      if state.consume_any() is Some(name) {
        push_child(type_children, Some("name"), name)
      } else {
        ()
      }
      state.report_expected_tokens_at(LBRACKET, mk_loc(start, end), [
        TokenKind::TK_LBRACE,
      ])
      while !state.current_is(TokenKind::TK_LBRACE) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(type_children, Some("error"), bad)
        } else {
          ()
        }
      }
      push_child(
        children,
        Some("type"),
        state
        .node_from_children(Type_Name, type_children)
        .with_structured_name("semantic_name", Name_ConstrId, ["name"]),
      )
      error_kind = ErrorType_Error
    }
    (Some((LIDENT(name), start, end)), _) => {
      state.report(
        mk_loc(start, end),
        "Unexpected lowercase identifier \{name}, expected uppercase identifier in error type.",
      )
      let type_children : Array[(String?, CstNode)] = []
      if state.consume_any() is Some(bad) {
        push_child(
          type_children,
          Some("name"),
          error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
        )
      } else {
        ()
      }
      push_child(
        children,
        Some("type"),
        state
        .node_from_children(Type_Name, type_children)
        .with_structured_name("semantic_name", Name_ConstrId, ["name"]),
      )
      error_kind = ErrorType_Error
    }
    (Some((AMPER, start, end)), _) => {
      state.report_expected_tokens_at(AMPER, mk_loc(start, end), [
        TokenKind::TK_LBRACE,
      ])
      let type_children : Array[(String?, CstNode)] = []
      while !state.current_is(TokenKind::TK_LBRACE) &&
            !state.current_is(TokenKind::TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(type_children, Some("error"), bad)
        } else {
          ()
        }
      }
      push_child(
        children,
        Some("type"),
        state.node_from_children(Type_MissingType, type_children),
      )
      error_kind = ErrorType_Error
    }
    _ =>
      match state.peek() {
        Some((QUESTION, _, _)) =>
          if state.consume_any() is Some(question) {
            push_child(children, Some("type"), question)
            error_kind = ErrorType_Maybe
          } else {
            ()
          }
        Some((LBRACE, _, _))
        | Some((RBRACE, _, _))
        | Some((RPAREN, _, _))
        | Some((RBRACKET, _, _))
        | Some((COMMA, _, _))
        | Some((EQUAL, _, _))
        | Some((SEMI(_), _, _))
        | Some((EOF, _, _))
        | None => ()
        Some((UIDENT(_), _, _))
        | Some((PACKAGE_NAME(_), _, _))
        | Some((UNDERSCORE, _, _)) => {
          let err_ty = parse_type_until(state, [
            TokenKind::TK_PLUS,
            TokenKind::TK_NOCANCEL,
            TokenKind::TK_LBRACE,
            TokenKind::TK_RBRACE,
            TokenKind::TK_RPAREN,
            TokenKind::TK_RBRACKET,
            TokenKind::TK_COMMA,
            TokenKind::TK_EQUAL,
            TokenKind::TK_SEMI,
            TokenKind::TK_EOF,
          ])
          if !err_ty.children.is_empty() {
            match err_ty.kind {
              Type_Name | Type_Any => ()
              _ => state.report(err_ty.loc, "invalid error type")
            }
            push_child(children, Some("type"), err_ty)
            error_kind = ErrorType_Error
          }
        }
        _ => ()
      }
  }
  state.node_from_children(error_kind, children)
}
