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
    | EXTERN => true
    _ => false
  }
}

///|
fn top_level_decl_expected_tokens() -> Array[TokenKind] {
  [
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
  ]
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
fn decl_name_expectation(kind : NodeKind) -> Array[TokenKind] {
  match kind {
    Decl_Function =>
      [TokenKind::TK_LIDENT, TokenKind::TK_EXTEND, TokenKind::TK_PACKAGE_NAME]
    Decl_Impl => [TokenKind::TK_UIDENT, TokenKind::TK_PACKAGE_NAME]
    _ => [TokenKind::TK_LIDENT]
  }
}

///|
fn decl_phase_expectation(
  phase : DeclPhase,
  kind : NodeKind,
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
fn decl_phase_after_name(kind : NodeKind, is_suberror : Bool) -> DeclPhase {
  match kind {
    Decl_Function => AwaitingBody
    Decl_Const => AwaitingEqual
    Decl_Type => Complete
    Decl_Struct => AwaitingBody
    Decl_Enum => if is_suberror { Complete } else { AwaitingBody }
    Decl_Trait => AwaitingBody
    Decl_Impl => AwaitingImplRelation
    Decl_EnumView => AwaitingEnumViewConstructors
    _ => Complete
  }
}

///|
fn ParserState::report_decl_phase(
  self : ParserState,
  phase : DeclPhase,
  kind : NodeKind,
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
  kind : NodeKind,
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
fn decl_keyword_kind(tok : Token) -> NodeKind {
  match tok {
    FN => Decl_Function
    CONST => Decl_Const
    TYPE => Decl_Type
    STRUCT => Decl_Struct
    ENUM | SUBERROR | EXTENUM => Decl_Enum
    ENUMVIEW => Decl_EnumView
    TRAIT => Decl_Trait
    IMPL => Decl_Impl
    _ => Decl_BalancedDecl
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
  match state.peek() {
    Some((PUB, _, _)) => {
      is_pub = true
      if state.consume_any() is Some(kw) {
        push_child(children, Some("kw"), kw)
      } else {
        ()
      }
    }
    Some((PRIV, _, _)) =>
      if state.consume_any() is Some(kw) {
        push_child(children, Some("kw"), kw)
      } else {
        ()
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
  state.node_from_children(Clause_Visibility, children)
}

///|
fn decl_allows_kind_update(kind : NodeKind) -> Bool {
  match kind {
    Impl_Using => false
    _ => true
  }
}

///|
fn node_has_missing_token(node : CstNode, kind : TokenKind) -> Bool {
  for entry in node.children {
    let (_, child) = entry
    match child.kind {
      Missing(missing) if missing == kind => return true
      _ => ()
    }
  }
  false
}

///|
fn is_name_token(tok : Token) -> Bool {
  match tok {
    LIDENT(_) | UIDENT(_) | PACKAGE_NAME(_) | EXTEND => true
    _ => false
  }
}

///|
fn decl_name_token_is_valid(tok : Token, kind : NodeKind) -> Bool {
  match kind {
    Decl_Function => tok is (LIDENT(_) | UIDENT(_) | PACKAGE_NAME(_) | EXTEND)
    Decl_Type
    | Decl_Struct
    | Decl_Enum
    | Decl_EnumView
    | Decl_Trait
    | Decl_Impl => tok is (UIDENT(_) | PACKAGE_NAME(_))
    _ => is_name_token(tok)
  }
}

///|
fn parse_parameter_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut is_labelled = false
  let mut has_question = false
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
      parse_single_token_pattern(state, Pattern_Binder)
    }
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      parse_single_token_pattern(state, Pattern_Binder)
    Some((UNDERSCORE, _, _)) =>
      parse_single_token_pattern(state, Pattern_Underscore)
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
  state.node_from_children(Clause_Parameter, children)
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
    item_expected="parameter name or label",
  )
}

///|
fn parse_function_stub_tokens(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> Unit {
  match state.peek() {
    Some((STRING(_), _, _)) => {
      if state.consume_any() is Some(stub) {
        push_child(children, Some("stub"), stub)
      } else {
        ()
      }
      push_children(children, state.consume_trivia())
      match state.peek() {
        Some((STRING(_), _, _)) =>
          if state.consume_any() is Some(stub) {
            push_child(children, Some("stub"), stub)
          } else {
            ()
          }
        _ => ()
      }
    }
    Some((MULTILINE_STRING(_), _, _)) => {
      let mut done = false
      while !done {
        match state.peek() {
          Some((MULTILINE_STRING(_), _, _)) => {
            if state.consume_any() is Some(stub) {
              push_child(children, Some("stub"), stub)
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
fn decl_accepts_type_params(kind : NodeKind) -> Bool {
  match kind {
    Decl_Type | Decl_Struct | Decl_Enum => true
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
  state.node_from_children(Clause_Parameter, children)
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
    List_TypeArgumentList,
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
    List_TypeArgumentList,
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
    List_TypeArgumentList,
    sep_kind=TokenKind::TK_COMMA,
    require_nonempty=true,
    nonempty_expected="type",
  )
}

///|
fn decl_body_uses_balanced_group(kind : NodeKind) -> Bool {
  match kind {
    Decl_Struct | Decl_Enum | Decl_Trait => true
    _ => false
  }
}

///|
fn decl_body_list_kind(kind : NodeKind) -> NodeKind {
  match kind {
    Decl_Enum => List_ConstructorList
    Decl_Struct | Decl_Trait => List_FieldList
    _ => List_StatementList
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
    List_FieldList,
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
    List_FieldList,
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
      state.current_is(TokenKind::TK_NORAISE)
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
  state.node_from_children(Decl_Function, children)
}

///|
fn parse_trait_method_param_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_trait_method_param_item,
    List_ParameterList,
    sep_kind=TokenKind::TK_COMMA,
  )
}

///|
fn parse_trait_method_param_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((LIDENT(_), _, _))
    | Some((POST_LABEL(_), _, _))
    | Some((EXTEND, _, _)) =>
      match state.peek(offset=1) {
        Some((QUESTION, _, _)) | Some((COLON, _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("pattern"), name)
          } else {
            ()
          }
        _ => ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_QUESTION) {
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
  state.node_from_children(Clause_Parameter, children)
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
      Some((INT(_), _, _)) =>
        if state.consume_any() is Some(tag) {
          push_child(children, Some("tag"), tag)
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
  state.node_from_children(Name_Constructor, children)
}

///|
fn parse_constructor_param_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_constructor_param_item,
    List_ParameterList,
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
      return state.node_from_children(Clause_Parameter, children)
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
  state.node_from_children(Clause_Parameter, children)
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
  state.node_from_children(Clause_Parameter, children)
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
    state.current_is(TokenKind::TK_NORAISE) {
    push_child(children, Some("clause"), parse_func_return_type(state))
  }
  if state.current_is(TokenKind::TK_LBRACE) {
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
  state.node_from_children(Decl_Function, children)
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
      state.current_is(TokenKind::TK_NORAISE)
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
  state.node_from_children(Decl_Function, children)
}

///|
fn parse_using_name_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_using_name_item,
    List_FieldList,
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
  state.node_from_children(Name_Identifier, children)
}

///|
fn parse_deriving_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_deriving_item,
    List_TypeArgumentList,
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
  state.node_from_children(Name_Identifier, children)
}

///|
fn parse_balanced_decl_body(state : ParserState, kind : NodeKind) -> CstNode {
  let diagnostics_before = state.reports.length()
  let children : Array[(String?, CstNode)] = []
  let mut effective_kind = kind
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
        top_level_decl_can_start(tok) => {
        if state.suppress_next_top_level_recovery_report {
          state.suppress_next_top_level_recovery_report = false
        } else if invalid_grouped_impl {
          state.report(mk_loc(start, end), "Unexpected token here.")
        } else if (
            state.reports.length() == diagnostics_before ||
            missing_return_before_impl
          ) &&
          !recovered_body_without_close {
          match decl_phase_expectation(phase, effective_kind) {
            NoExpectation =>
              state.report(mk_loc(start, end), "Missing newline here.")
            _ =>
              state.report_decl_phase_at(
                phase,
                effective_kind,
                tok,
                mk_loc(start, end),
              )
          }
          if phase is AwaitingBody {
            reported_missing_body = true
          }
        }
        done = true
      }
      Some((tok, start, end)) if phase is AwaitingName && is_name_token(tok) => {
        if effective_kind is Decl_Function &&
          tok is LIDENT(name) &&
          state.peek_nth_significant(1).map(item => item.0) is Some(COLONCOLON) {
          state.report(
            mk_loc(start, end),
            "Unexpected lowercase identifier \{name}, expected uppercase identifier in type name.",
          )
        }
        if !decl_name_token_is_valid(tok, effective_kind) {
          match (effective_kind, tok) {
            (Decl_Struct, LIDENT(name)) =>
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
        phase = decl_phase_after_name(effective_kind, is_suberror)
        accepts_params = true
      }
      Some((LBRACKET, _, _)) if phase is AwaitingName &&
        effective_kind is Decl_Function =>
        push_child(
          children,
          Some("quantifier"),
          parse_fun_type_parameter_list(state),
        )
      Some((LBRACKET, _, _)) if phase is AwaitingName &&
        (effective_kind is Decl_Impl || effective_kind is Decl_EnumView) =>
        push_child(
          children,
          Some("quantifier"),
          parse_fun_type_parameter_list(state),
        )
      Some((AMPER, _, _)) if phase is AwaitingName &&
        effective_kind is Decl_Function =>
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
        } else {
          ()
        }
      Some((LPAREN, _, _)) if effective_kind is Decl_Struct => {
        push_child(children, Some("body"), parse_tuple_struct_type_list(state))
        saw_body = true
        accepts_params = false
        phase = Complete
      }
      Some((LPAREN, _, _)) if accepts_params && effective_kind is Decl_Function => {
        let params = parse_parameter_list(state)
        params_complete = !node_has_missing_token(params, TokenKind::TK_RPAREN)
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
          effective_kind is Decl_Type ||
          effective_kind is Decl_Struct ||
          effective_kind is Decl_Enum ||
          effective_kind is Decl_EnumView ||
          effective_kind is Decl_Impl
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
        effective_kind is Decl_Function =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
      Some((COLONCOLON, _, _)) if accepts_params &&
        effective_kind is Decl_Function => {
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
      Some((LBRACKET, _, _)) if accepts_params &&
        decl_accepts_type_params(effective_kind) => {
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
        let pat = parse_single_token_pattern(state, Pattern_Binder)
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
        if decl_allows_kind_update(kind) {
          effective_kind = Decl_Let
        }
        phase = AwaitingLetPattern
      }
      Some((TEST, _, _)) if effective_kind is Decl_Test ||
        kind is Decl_BalancedDecl => {
        saw_core_keyword = true
        if state.consume_any() is Some(test_token) {
          push_child(children, decl_token_label(TEST), test_token)
        } else {
          ()
        }
        if decl_allows_kind_update(kind) {
          effective_kind = Decl_Test
        }
        match state.peek_significant() {
          Some((STRING(_), _, _)) =>
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
          Some((STRING(_), _, _)) =>
            if state.consume_any() is Some(language) {
              saw_language = true
              phase = AwaitingExternFunction
              push_child(children, Some("language"), language)
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
        if decl_allows_kind_update(kind) {
          effective_kind = decl_keyword_kind(tok)
        }
        phase = AwaitingName
      }
      Some((USING, _, _)) if effective_kind is Impl_Using ||
        kind is Decl_BalancedDecl => {
        if is_declare {
          match state.peek() {
            Some((found, start, end)) =>
              state.report_expected_tokens_at(found, mk_loc(start, end), [
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
        phase = AwaitingUsingName
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        } else {
          ()
        }
        effective_kind = Impl_Using
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
      Some((COLON, _, _)) if effective_kind is Decl_Trait && depth == 0 => {
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
      Some((COLON, start, end)) if effective_kind is Decl_Function && depth == 0 => {
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
      Some((RAISE, _, _)) | Some((NORAISE, _, _)) if depth == 0 => {
        push_child(children, Some("clause"), parse_func_return_type(state))
        phase = if is_declare { Complete } else { AwaitingBody }
      }
      Some((EQUAL, _, _)) if effective_kind is Decl_Type => {
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
      Some((EQUAL, _, _)) if effective_kind is Decl_Let ||
        effective_kind is Decl_Const => {
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
      Some((EQUAL, _, _)) if effective_kind is Decl_Function => {
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
      Some((FOR, _, _)) if effective_kind is Decl_Impl => {
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
      Some((FOR, _, _)) if effective_kind is Decl_EnumView => {
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
      Some((WITH, _, _)) if effective_kind is Decl_Impl => {
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
      Some((WITH, _, _)) if effective_kind is Decl_EnumView => {
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
      Some((found, start, end)) if effective_kind is Decl_Function &&
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
      Some((WHERE, _, _)) if effective_kind is Decl_Function =>
        push_child(children, Some("where"), parse_where_clause(state))
      Some((EXCLAMATION, _, _)) if effective_kind is Decl_Function &&
        !(phase is AwaitingName) =>
        if state.consume_error_for_progress() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      Some((LBRACKET, _, _)) if accepts_params &&
        effective_kind is Decl_Function &&
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
        (kind is Error || kind is Decl_BalancedDecl) =>
        if state.consume_error_for_progress() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      Some((RBRACE, start, end)) if effective_kind is Decl_Function && !saw_body => {
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
      Some((found, start, end)) if effective_kind is Error ||
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
      Some((LBRACE, start, end)) if effective_kind is Decl_Function &&
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
          decl_name_expectation(effective_kind),
        )
        phase = Complete
        done = true
      }
      Some((LBRACE, _, _)) =>
        if effective_kind is Impl_Using {
          saw_body = true
          push_child(children, Some("body"), parse_using_name_list(state))
          phase = Complete
        } else if effective_kind is Decl_Impl {
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
            parse_balanced_brace_group(
              state,
              decl_body_list_kind(effective_kind),
            ),
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
        } else if effective_kind is Decl_Struct {
          saw_body = true
          let body = parse_struct_member_list(state)
          recovered_body_without_close = node_has_missing_token(
            body,
            TokenKind::TK_RBRACE,
          )
          push_child(children, Some("body"), body)
          phase = Complete
        } else if effective_kind is Decl_Enum {
          saw_body = true
          push_child(children, Some("body"), parse_enum_constructor_list(state))
          phase = Complete
        } else if effective_kind is Decl_EnumView && !enumview_has_constructors {
          saw_body = true
          push_child(children, Some("body"), parse_enum_constructor_list(state))
          enumview_has_constructors = true
          phase = AwaitingEnumViewFor
        } else if effective_kind is Decl_EnumView {
          saw_body = true
          push_child(children, Some("expr"), parse_block_expr(state))
          phase = Complete
        } else if effective_kind is Decl_Trait {
          saw_body = true
          push_child(children, Some("body"), parse_trait_member_list(state))
          phase = Complete
        } else if decl_body_uses_balanced_group(effective_kind) {
          saw_body = true
          push_child(
            children,
            Some("body"),
            parse_balanced_brace_group(
              state,
              decl_body_list_kind(effective_kind),
            ),
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
        let expected = decl_name_expectation(effective_kind)
        if effective_kind is Decl_Function {
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
        } else if effective_kind is Decl_Type {
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
          Some((found, start, end)) if effective_kind is Decl_Type =>
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
                Some((found, start, end)) if effective_kind is Decl_Type =>
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
        effective_kind is Decl_Function &&
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
    if effective_kind is Decl_Function && is_declare && (saw_body || saw_stub) {
      state.report(
        empty_loc(state.current_position()),
        "declare function cannot have a body",
      )
    } else if effective_kind is Decl_Enum && suberror_had_type_params {
      state.report(
        empty_loc(state.current_position()),
        "Unexpected token here.",
      )
    } else {
      state.report_decl_phase(phase, effective_kind)
    }
  }
  state.node_from_children(effective_kind, children)
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
    | DEFER
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
    TRY_QUESTION => Some("`try?` expression")
    LET | LETREC => Some("let statement")
    GUARD => Some("guard statement")
    DEFER => Some("defer statement")
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
    | TRY_QUESTION
    | TRY_EXCLAMATION
    | LET
    | LETREC
    | GUARD
    | BREAK
    | RETURN
    | CONTINUE => true
    _ => false
  }
}

///|
fn token_disallowed_in_try_body(tok : Token) -> Bool {
  match tok {
    IF | MATCH | WHILE | FOR | TRY | LET | GUARD | BREAK | RETURN | CONTINUE =>
      true
    _ => false
  }
}

///|
fn token_disallowed_in_pattern_guard(tok : Token) -> Bool {
  match tok {
    IF | MATCH | WHILE | FOR | TRY | LET | GUARD | BREAK | RETURN | CONTINUE =>
      true
    _ => false
  }
}

///|
fn token_disallowed_in_match_action(tok : Token) -> Bool {
  match tok {
    LET | LETREC | GUARD | DEFER => true
    _ => false
  }
}

///|
fn assignment_lhs_is_valid(node : CstNode) -> Bool {
  match node.kind {
    Expr_Identifier | Expr_Field | Expr_ArrayGet => true
    Expr_Group =>
      if node.first_child_with_label("expr") is Some(inner) {
        assignment_lhs_is_valid(inner)
      } else {
        false
      }
    _ => false
  }
}

///|
fn invalid_arrow_parameter_expr(node : CstNode) -> CstNode? {
  match node.kind {
    Expr_Identifier | Expr_Hole | Expr_MissingExpr => None
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
  node.kind is Expr_Tuple || invalid_arrow_parameter_expr(node) is None
}

///|
fn parse_arrow_fn_body_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  match state.peek_significant() {
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
    Some((TRY_QUESTION | TRY_EXCLAMATION, _, _)) =>
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
    parse_expr_until(state, [TokenKind::TK_EOF]),
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
      let next = parse_expr_until(state, [TokenKind::TK_EOF])
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
  node(ExpressionFragment, state.root_loc(), state.root_span(), children)
}

///|
fn parse_expr_until(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  allow_argument_label_question? : Bool = false,
) -> CstNode {
  let _ = allow_argument_label_question
  let lhs = parse_expr_pipe(state, stop_kinds)
  parse_expr_catch_suffix(state, stop_kinds, lhs)
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
  let children : Array[(String?, CstNode)] = [(Some("body"), lhs)]
  if state.consume_any() is Some(catch_kw) {
    push_child(children, Some("kw"), catch_kw)
  } else {
    ()
  }
  if state.current_is(TokenKind::TK_LBRACE) {
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
  state.node_from_children(Expr_Try, children)
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
        let kind = match state.peek() {
          Some((EQUAL, _, _)) => Expr_Assign
          _ => Expr_AugmentedAssign
        }
        if state.reports.length() == reports_before_lhs &&
          !assignment_lhs_is_valid(lhs) {
          state.report(lhs.loc, "Invalid left value in assignment")
        }
        let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
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
        lhs = state.node_from_children(kind, children)
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
        lhs = state.node_from_children(Expr_Infix, children)
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
        lhs = state.node_from_children(Expr_Function, children)
      }
      _ => done = true
    }
  }
  lhs
}

///|
fn expr_constraint_type_stops(
  stop_kinds : Array[TokenKind],
) -> Array[TokenKind] {
  let stops = stop_kinds.copy()
  stops.push(TokenKind::TK_FAT_ARROW)
  stops.push(TokenKind::TK_EQUAL)
  stops.push(TokenKind::TK_SEMI)
  stops.push(TokenKind::TK_RBRACE)
  stops
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
      state.node_from_children(Expr_Function, children)
    }
    _ => head
  }
}

///|
fn parse_arrow_params_from_expr(expr : CstNode) -> CstNode {
  match expr.kind {
    Expr_Tuple =>
      if expr.first_child_with_label("body") is Some(body) {
        let children : Array[(String?, CstNode)] = []
        for item in lower_item_children(body) {
          push_child(children, Some("item"), parse_arrow_param_from_expr(item))
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
fn parse_arrow_param_from_expr(expr : CstNode) -> CstNode {
  match expr.kind {
    Expr_Constraint => {
      let children : Array[(String?, CstNode)] = []
      if expr.first_child_with_label("expr") is Some(pattern) {
        push_child(children, Some("pattern"), pattern)
      } else {
        ()
      }
      if expr.first_child_with_label("type") is Some(ty) {
        push_child(children, Some("type"), ty)
      } else {
        ()
      }
      node(Clause_Parameter, expr.loc, expr.source_span, children)
    }
    _ =>
      node(Clause_Parameter, expr.loc, expr.source_span, [
        (Some("pattern"), expr),
      ])
  }
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
    | TRY_QUESTION
    | TRY_EXCLAMATION
    | LET
    | LETREC
    | GUARD
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
    | Expr_LetRec
    | Expr_LetFn
    | Expr_Return
    | Expr_Raise
    | Expr_Break
    | Expr_Continue
    | Expr_Defer
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
            let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
            if state.consume_any() is Some(op) {
              push_child(children, Some("op"), op)
            } else {
              ()
            }
            let next_min = if right_assoc { prec } else { prec + 1 }
            let reports_before_rhs = state.reports.length()
            let rhs = match state.peek_significant() {
              Some((next, _, _)) if token_invalid_as_infix_operand(next) => {
                state.abort_infix_chain = true
                parse_invalid_simple_expr(state, stop_kinds)
              }
              _ => parse_expr_binary_prec(state, stop_kinds, next_min)
            }
            push_child(
              children,
              Some("rhs"),
              state.required_expr_node(rhs, reports_before_rhs),
            )
            lhs = state.node_from_children(Expr_Infix, children)
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
      state.node_from_children(Expr_TemplateWriting, children)
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
        push_child(children, Some("field"), field)
      } else {
        ()
      }
      if state.current_is(TokenKind::TK_LPAREN) {
        push_child(children, Some("arg"), parse_argument_list(state))
        Some(state.node_from_children(Expr_DotApply, children))
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
            push_child(children, Some("field"), field)
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
      Some(state.node_from_children(Expr_DotApply, children))
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
  if state.current_is(TokenKind::TK_LBRACE) {
    push_child(children, Some("body"), parse_record_field_list(state))
    state.node_from_children(Expr_Record, children)
  } else {
    match state.peek() {
      Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("field"), name)
        } else {
          ()
        }
      Some((UIDENT(_), _, _)) =>
        if state.consume_any() is Some(name) {
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
    match children.last() {
      Some((Some("field"), field)) =>
        match field.first_token() {
          Some(UIDENT(_)) | Some(DOT_UIDENT(_)) =>
            state.node_from_children(Expr_Constructor, children)
          _ => state.node_from_children(Expr_Method, children)
        }
      _ => state.node_from_children(Expr_Method, children)
    }
  }
}

///|
fn brace_starts_record_expr(state : ParserState) -> Bool {
  let mut offset = 1
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
fn brace_starts_map_expr(state : ParserState) -> Bool {
  let mut offset = 1
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
  state.node_from_children(Expr_Record, [
    (Some("body"), parse_record_field_list(state)),
  ])
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
    List_FieldList,
    sep_kind=TokenKind::TK_COMMA,
  )
  let mut item_index = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), item)) => {
        match item.first_child_with_label("op") {
          Some(op) if op.first_token() is Some(DOTDOT) && item_index > 0 =>
            state.report_expected_tokens_at(DOTDOT, op.loc, [
              TokenKind::TK_LIDENT,
              TokenKind::TK_EXTEND,
            ])
          _ => ()
        }
        item_index += 1
      }
      _ => ()
    }
  }
  list
}

///|
fn parse_map_field_list(state : ParserState) -> CstNode {
  let list = parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_map_field_item,
    List_FieldList,
    sep_kind=TokenKind::TK_COMMA,
  )
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), item)) => {
        match item.first_child_with_label("op") {
          Some(op) if op.first_token() is Some(MINUS) =>
            for entry in item.children {
              let (_, child) = entry
              if child.first_token() is Some(COLON) {
                state.report_expected_tokens_at(COLON, child.loc, [
                  TokenKind::TK_SEMI,
                  TokenKind::TK_RBRACE,
                ])
                break
              }
            }
          _ => ()
        }
        break
      }
      _ => ()
    }
  }
  list
}

///|
fn parse_map_field_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((MINUS, _, _)) => {
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      match state.peek() {
        Some((INT(_), _, _))
        | Some((DOUBLE(_), _, _))
        | Some((FLOAT(_), _, _)) =>
          if state.consume_any() is Some(key) {
            push_child(children, Some("key"), key)
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
    Some((STRING(_), _, _))
    | Some((BYTES(_), _, _))
    | Some((REGEX_LITERAL(_), _, _))
    | Some((CHAR(_), _, _))
    | Some((INT(_), _, _))
    | Some((BYTE(_), _, _))
    | Some((DOUBLE(_), _, _))
    | Some((FLOAT(_), _, _))
    | Some((TRUE, _, _))
    | Some((FALSE, _, _)) =>
      if state.consume_any() is Some(key) {
        push_child(children, Some("key"), key)
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
  state.node_from_children(Argument_Labelled, children)
}

///|
fn parse_record_field_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.current_is(TokenKind::TK_DOTDOT) {
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
  state.node_from_children(Argument_Labelled, children)
}

///|
fn parse_array_get_suffix(state : ParserState, lhs : CstNode) -> CstNode {
  let children : Array[(String?, CstNode)] = [(Some("self"), lhs)]
  push_child(children, Some("open"), state.expect_token(TokenKind::TK_LBRACKET))
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
    push_child(
      children,
      Some("close"),
      state.expect_token(TokenKind::TK_RBRACKET),
    )
    state.node_from_children(Expr_ArrayGetSlice, children)
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
    state.node_from_children(Expr_ArrayGet, children)
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
      lhs = state.node_from_children(Expr_Range, children)
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
    Some((PLUS, _, _)) | Some((MINUS, _, _)) | Some((EXCLAMATION, _, _)) => {
      let children : Array[(String?, CstNode)] = []
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let reports_before_expr = state.reports.length()
      let expr = parse_expr_prefix(state, stop_kinds)
      push_child(
        children,
        Some("expr"),
        state.required_expr_node(expr, reports_before_expr),
      )
      state.node_from_children(Expr_Prefix, children)
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
    if state.consume_any() is Some(bad) {
      match bad.first_token() {
        Some(FN) => {
          pending_function_body = true
          if invalid_continue_start &&
            !reported_promoted_function &&
            state.peek_significant().map(item => item.0) is Some(LPAREN) {
            report_promoted_anonymous_function(state)
            reported_promoted_function = true
          }
        }
        Some(LET | LETREC) => saw_local_let = true
        Some(LBRACE) => pending_function_body = false
        _ => ()
      }
      push_child(
        children,
        Some("error"),
        error_node(bad.loc, bad.source_span, [(Some("item"), bad)]),
      )
    } else {
      ()
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
    Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => {
      let children = state.consume_trivia()
      let expr = parse_expr_simple(state, stop_kinds)
      if !expr.children.is_empty() {
        push_child(children, Some("expr"), expr)
      }
      state.node_from_children(Expr_Group, children)
    }
    Some((LOOP, _, _)) | Some((THROW, _, _)) | Some((QUESTION, _, _)) =>
      parse_invalid_simple_expr(state, stop_kinds)
    Some((IF, _, _)) => parse_if_expr(state)
    Some((MATCH, _, _)) => parse_match_expr(state)
    Some((FORALL, _, _)) | Some((EXISTS, _, _)) =>
      parse_quantifier_expr(state, stop_kinds)
    Some((LET, _, _)) => parse_let_statement(state)
    Some((LETREC, _, _)) => parse_letrec_statement(state)
    Some((GUARD, _, _)) => parse_guard_statement(state)
    Some((DEFER, _, _)) => parse_defer_statement(state)
    Some((PROOF_ASSERT, _, _)) => parse_proof_assert_statement(state)
    Some((PROOF_LET, _, _)) => parse_proof_let_statement(state)
    Some((TRY, _, _))
    | Some((TRY_QUESTION, _, _))
    | Some((TRY_EXCLAMATION, _, _)) => parse_try_expr(state)
    Some((RETURN, _, _)) =>
      parse_keyword_value_expr(state, Expr_Return, stop_kinds)
    Some((RAISE, _, _)) =>
      parse_keyword_value_expr(state, Expr_Raise, stop_kinds)
    Some((BREAK, _, _)) => parse_break_expr(state, stop_kinds)
    Some((CONTINUE, _, _)) => parse_continue_expr(state, stop_kinds)
    Some((POST_LABEL(_), _, _)) if state.peek(offset=1).map(item => item.0)
      is Some(COLON) => parse_labelled_loop_expr(state, stop_kinds)
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
        parse_block_expr(state)
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
            state.node_from_children(Expr_Identifier, [
              (token_label_from_node(token), token),
            ])
          } else {
            state.node_from_children(Expr_MissingExpr, [])
          }
      }
    Some((ELLIPSIS, _, _)) | Some((UNDERSCORE, _, _)) =>
      parse_single_token_expr(state, Expr_Hole, "token")
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
      parse_single_token_expr(state, Expr_Literal, "token")
    Some((LIDENT(_), _, _))
    | Some((EXTEND, _, _))
    | Some((PACKAGE_NAME(_), _, _)) => parse_ident_or_qualified_expr(state)
    Some((UIDENT(_), _, _)) =>
      parse_single_token_expr(state, Expr_Constructor, "name")
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
        // The handrolled parser assigns a control label the location of the
        // next significant token after consuming the post-label token.
        push_child(
          children,
          Some("label_loc"),
          state.current_significant_anchor(),
        )
        push_child(children, Some("label"), label)
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
    Some((POST_LABEL(_), _, _)) =>
      if state.consume_any() is Some(label) {
        anchor = Some(label)
        // Preserve the handrolled parser's post-consumption label location.
        push_child(
          children,
          Some("label_loc"),
          state.current_significant_anchor(),
        )
        push_child(children, Some("label"), label)
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
          Some((FOR, _, _)) | Some((WHILE, _, _)) => return true
          _ => done = true
        }
      }
      false
    }
    _ => false
  }
}

///|
fn parse_labelled_loop_expr(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(label) {
    push_child(children, Some("label"), label)
  } else {
    ()
  }
  if state.consume_any() is Some(op) {
    push_child(children, Some("op"), op)
  } else {
    ()
  }
  let loop_node = match state.peek() {
    Some((FOR, _, _)) => parse_for_expr(state)
    Some((WHILE, _, _)) => parse_keyword_body_expr(state, Expr_While)
    Some((found, start, end)) => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_FOR,
        TokenKind::TK_WHILE,
      ])
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
  state.node_from_children(List_StatementList, children)
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
  if state.consume_any() is Some(token) {
    state.node_from_children(kind, [(Some(label), token)])
  } else {
    state.node_from_children(Expr_MissingExpr, [])
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
      state.node_from_children(Expr_Identifier, children)
    }
    Some((DOT_UIDENT(_), _, _)) if allow_qualified => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
      state.node_from_children(Expr_Constructor, children)
    }
    _ => state.node_from_children(Expr_Identifier, children)
  }
}

///|
fn parse_type_name_node(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.current_is(TokenKind::TK_AMPER) {
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
  state.node_from_children(Type_Name, children)
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
      List_ArgumentList,
      sep_kind=TokenKind::TK_COMMA,
      item_expected="simple expression",
    )
  } else {
    parse_delimited(
      state,
      TokenKind::TK_LPAREN,
      TokenKind::TK_RPAREN,
      parse_tuple_expr_item,
      List_ArgumentList,
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
  state.node_from_children(Expr_Tuple, [(Some("body"), list)])
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
    List_ArgumentList,
    sep_kind=TokenKind::TK_COMMA,
  )
  state.node_from_children(Expr_Array, [(Some("body"), list)])
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
  if state.consume_any() is Some(kw) {
    push_child(children, Some("kw"), kw)
  } else {
    ()
  }
  if list_comprehension_is_foreach(state) {
    parse_list_comprehension_foreach_header(state, children, close_kind)
  } else {
    parse_list_comprehension_for_header(state, children, close_kind)
  }
  if state.current_is(TokenKind::TK_IF) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    let guard_expr = parse_expr_until(state, [
      TokenKind::TK_FAT_ARROW,
      close_kind,
      TokenKind::TK_EOF,
    ])
    if !guard_expr.children.is_empty() {
      push_child(children, Some("guard"), guard_expr)
    }
  }
  if state.current_is(TokenKind::TK_FAT_ARROW) {
    if state.consume_any() is Some(op) {
      push_child(children, Some("op"), op)
    } else {
      ()
    }
  }
  let body = parse_expr_until(state, [close_kind, TokenKind::TK_EOF])
  if !body.children.is_empty() {
    push_child(children, Some("body"), body)
  }
  push_children(children, state.consume_trivia())
  push_child(children, Some("close"), state.expect_token(close_kind))
  state.node_from_children(Expr_ListComprehension, children)
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
) -> Unit {
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
      if state.current_is(TokenKind::TK_SEMI) ||
        state.current_is(TokenKind::TK_NEWLINE) {
        consume_for_header_separator(state)
      }
      push_child(
        children,
        Some("continue"),
        parse_list_comprehension_binding_list(state, close_kind),
      )
    }
  }
}

///|
fn parse_list_comprehension_for_header(
  state : ParserState,
  children : Array[(String?, CstNode)],
  close_kind : TokenKind,
) -> Unit {
  let init = parse_list_comprehension_binding_list(state, close_kind)
  push_child(children, Some("init"), init)
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
      let cond_expr = parse_expr_until(
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
      if state.current_is(TokenKind::TK_SEMI) ||
        state.current_is(TokenKind::TK_NEWLINE) {
        consume_for_header_separator(state)
      }
      push_child(
        children,
        Some("continue"),
        parse_list_comprehension_binding_list(state, close_kind),
      )
    }
  }
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
  state.node_from_children(List_StatementList, children)
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
  state.node_from_children(Clause_Parameter, children)
}

///|
fn parse_array_expr_item(state : ParserState) -> CstNode {
  let stops = [
    TokenKind::TK_COMMA,
    TokenKind::TK_RBRACKET,
    TokenKind::TK_BAR_RBRACKET,
    TokenKind::TK_EOF,
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
      state.node_from_children(Argument_Positional, children)
    }
    _ => {
      report_statement_only_expression(state, "array element")
      let reports_before_expr = state.reports.length()
      state.required_expr_node(
        parse_expr_until(state, stops),
        reports_before_expr,
      )
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
  if state.current_is(TokenKind::TK_LBRACE) {
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
  match state.peek() {
    Some((TRY_QUESTION, _, _)) | Some((TRY_EXCLAMATION, _, _)) =>
      kind = Expr_TryOperator
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
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ]
  let panic_body_stops = [
    TokenKind::TK_SEMI,
    TokenKind::TK_RBRACE,
    TokenKind::TK_EOF,
  ]
  let body = match (kind, state.peek_significant()) {
    (Expr_Try, Some((TRY_QUESTION | TRY_EXCLAMATION | LOOP, _, _))) =>
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
  state.node_from_children(kind, children)
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
              TokenKind::TK_LIDENT,
              TokenKind::TK_EXTEND,
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
      (Some("body"), state.node_from_children(List_PatternList, children)),
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
fn for_header_stops() -> Array[TokenKind] {
  [
    TokenKind::TK_SEMI,
    TokenKind::TK_NEWLINE,
    TokenKind::TK_LBRACE,
    TokenKind::TK_EOF,
  ]
}

///|
fn parse_for_binding(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut invalid_name = false
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
    return state.node_from_children(Clause_Parameter, children)
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
    return state.node_from_children(Clause_Parameter, children)
  }
  let expr = parse_expr_until(state, for_clause_stops())
  if !expr.children.is_empty() {
    push_child(children, Some("expr"), expr)
  }
  state.node_from_children(Clause_Parameter, children)
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
  state.node_from_children(List_StatementList, children)
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
fn consume_for_header_separators(state : ParserState) -> Unit {
  let mut done = false
  while !done {
    match state.peek() {
      Some((SEMI(_), _, _)) => {
        let _ = state.consume_any()
      }
      Some((NEWLINE, _, _)) => {
        let _ = state.consume_trivia()
      }
      _ => done = true
    }
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
    | IF
    | MATCH
    | PLUS
    | MINUS
    | LOOP
    | WHILE
    | FOR
    | POST_LABEL(_)
    | GUARD
    | TRY
    | DEFER
    | TRY_QUESTION
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
        List_FieldList,
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
  state.node_from_children(Clause_Parameter, children)
}

///|
fn parse_quantifier_expr(
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
  state.node_from_children(Expr_Quantifier, children)
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
    if kind is Expr_LexScan {
      push_child(children, Some("case"), parse_lexscan_case_block(state))
    } else {
      push_child(children, Some("case"), parse_lexmatch_case_block(state))
    }
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
  state.node_from_children(List_StatementList, children)
}

///|
fn parse_lex_top_pattern_item(state : ParserState) -> CstNode {
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      parse_single_token_pattern(state, Pattern_Binder)
    Some((UNDERSCORE, _, _)) =>
      parse_single_token_pattern(state, Pattern_Underscore)
    _ =>
      parse_lex_pattern_until(state, [
        TokenKind::TK_COMMA,
        TokenKind::TK_RPAREN,
        TokenKind::TK_EOF,
      ])
  }
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
        List_PatternList,
        sep_kind=TokenKind::TK_COMMA,
        require_nonempty=true,
      )
      state.node_from_children(Pattern_Tuple, [(Some("body"), list)])
    }
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      parse_single_token_pattern(state, Pattern_Binder)
    Some((UNDERSCORE, _, _)) =>
      parse_single_token_pattern(state, Pattern_Underscore)
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
      push_child(
        children,
        Some("pattern"),
        state.node_from_children(Pattern_MissingPattern, [(Some("token"), todo)]),
      )
      push_child(
        children,
        Some("body"),
        state.node_from_children(Expr_Hole, [(Some("token"), todo)]),
      )
    } else {
      ()
    }
    return state.node_from_children(Case_LexCase, children)
  }
  let pat = parse_lexmatch_case_pattern(state)
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
  state.node_from_children(List_StatementList, children)
}

///|
fn parse_lexscan_case_pattern(state : ParserState) -> CstNode {
  let stops = [
    TokenKind::TK_IF,
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
fn parse_lexscan_case(state : ParserState) -> CstNode {
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
          state.node_from_children(Expr_Hole, [(Some("token"), todo)]),
        )
      } else {
        ()
      }
      return state.node_from_children(Case_MatchCase, children)
    }
    _ => ()
  }
  let pat = parse_lexscan_case_pattern(state)
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
  state.node_from_children(Case_MatchCase, children)
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
      return state.node_from_children(Expr_Function, children)
    }
    _ => ()
  }
  let reports_before_signature = state.reports.length()
  if state.current_is(TokenKind::TK_LPAREN) {
    push_child(children, Some("params"), parse_parameter_list(state))
  }
  if state.current_is(TokenKind::TK_THIN_ARROW) ||
    state.current_is(TokenKind::TK_RAISE) ||
    state.current_is(TokenKind::TK_NORAISE) {
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
  state.node_from_children(Expr_Function, children)
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
  state.node_from_children(List_StatementList, children)
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
          state.node_from_children(Expr_Hole, [(Some("token"), todo)]),
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
        if op.first_token() is Some(found) {
          state.report_expected_tokens_at(found, op.loc, [
            TokenKind::TK_FAT_ARROW,
          ])
        } else {
          ()
        }
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
    !saw_guard {
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
fn argument_label_question_can_follow_token(tok : Token) -> Bool {
  match tok {
    LIDENT(_) | EXTEND => true
    _ => false
  }
}

///|
fn argument_label_question_has_valid_prefix(state : ParserState) -> Bool {
  let mut offset = -1
  while state.pos + offset >= 0 {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset -= 1
      Some((tok, _, _)) => return argument_label_question_can_follow_token(tok)
      None => return false
    }
  }
  false
}

///|
fn argument_label_question_has_valid_suffix(state : ParserState) -> Bool {
  let mut offset = 1
  while true {
    match state.peek(offset~) {
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => offset += 1
      Some((COMMA, _, _)) | Some((RPAREN, _, _)) | Some((EQUAL, _, _)) =>
        return true
      Some(_) | None => return false
    }
  }
  false
}

///|
fn argument_label_question_has_valid_context(state : ParserState) -> Bool {
  argument_label_question_has_valid_prefix(state) &&
  argument_label_question_has_valid_suffix(state)
}

///|
fn token_label_from_node(node : CstNode) -> String? {
  match node.kind {
    Token(SEMI(_)) => Some("sep")
    Token(tok) => expr_token_label(tok)
    _ => Some("item")
  }
}

///|
fn expr_token_label(tok : Token) -> String? {
  match tok {
    TRY_QUESTION
    | TRY_EXCLAMATION
    | NORAISE
    | IS
    | IN
    | FOR
    | DEFER
    | GUARD
    | RETURN
    | WHILE
    | THIN_ARROW
    | FAT_ARROW
    | PROOF_ASSERT
    | PROOF_LET
    | IF
    | ELSE
    | PIPE
    | PIPE_LEFT
    | LT_PLUS
    | LT_QUESTION
    | AS
    | BARBAR
    | CARET
    | AMPER
    | AMPERAMPER
    | BAR
    | PLUS
    | EXCLAMATION
    | QUESTION
    | MINUS
    | EQUAL
    | EQ_TILDE
    | CATCH
    | TRY
    | RAISE
    | WITH
    | CONTINUE
    | BREAK
    | AUGMENTED_ASSIGNMENT(_)
    | PLUS_EQUAL
    | INFIX4(_)
    | INFIX3(_)
    | INFIX2(_)
    | INFIX1(_)
    | LEXMATCH
    | LEXMATCH_QUESTION
    | LEXSCAN
    | RANGE_EXCLUSIVE
    | RANGE_INCLUSIVE
    | RANGE_EXCLUSIVE_REV
    | RANGE_INCLUSIVE_REV
    | RANGE_LT_INCLUSIVE
    | WHERE
    | NOBREAK
    | FORALL
    | EXISTS
    | IMPLIES => Some("op")
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
  state.node_from_children(kind, children)
}

///|
fn parse_optional_forwarding_argument(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
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
  state.node_from_children(kind, children)
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
      ) => parse_local_function_statement(state)
    Some((GUARD, _, _)) => parse_guard_statement(state)
    Some((DEFER, _, _)) => parse_defer_statement(state)
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
  state.node_from_children(Expr_Guard, children)
}

///|
fn parse_defer_statement(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
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
  state.node_from_children(Expr_Defer, children)
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
  state.node_from_children(Stmt_ProofAssert, children)
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
  state.node_from_children(Stmt_ProofLet, children)
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
  state.node_from_children(Expr_LetRec, children)
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
    return state.node_from_children(Expr_LetFn, children)
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
      state.current_is(TokenKind::TK_NORAISE) {
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
  state.node_from_children(Expr_LetFn, children)
}

///|
fn parse_local_function_statement(state : ParserState) -> CstNode {
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
  match state.peek() {
    Some((LIDENT(_), _, _)) | Some((UIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_EXCLAMATION) {
    match state.peek() {
      Some((_, start, end)) =>
        state.report(
          mk_loc(start, end),
          "Failed to parse func: expected '(' or '{', got EXCLAMATION",
        )
      None => ()
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
    return state.node_from_children(Expr_LetFn, children)
  }
  if state.current_is(TokenKind::TK_LPAREN) {
    push_child(children, Some("params"), parse_parameter_list(state))
  }
  if state.current_is(TokenKind::TK_THIN_ARROW) ||
    state.current_is(TokenKind::TK_RAISE) ||
    state.current_is(TokenKind::TK_NORAISE) {
    push_child(children, Some("type"), parse_func_return_type(state))
  }
  if state.current_is(TokenKind::TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  }
  state.node_from_children(Expr_LetFn, children)
}

///|
fn parse_argument_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LPAREN,
    TokenKind::TK_RPAREN,
    parse_argument_item,
    List_ArgumentList,
    sep_kind=TokenKind::TK_COMMA,
    follow_set=[TokenKind::TK_SEMI, TokenKind::TK_RBRACE],
    post_item_expected=[TokenKind::TK_COMMA, TokenKind::TK_RPAREN],
  )
}

///|
fn parse_statement_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_statement_item,
    List_StatementList,
    sep_kind=TokenKind::TK_SEMI,
    item_expected="simple expression",
  )
}

///|
fn parse_block_expr(state : ParserState) -> CstNode {
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
  node(Expr_Block, list.loc, list.source_span, [(Some("body"), list)])
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
      self.reports.push(Report::{ loc: bad.loc, msg: "Unexpected token here." })
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
      Some((RETURN | BREAK | CONTINUE | RAISE | DEFER | GUARD, _, _)) if depth ==
        0 &&
        saw_statement_boundary => done = true
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
    if list_kind is List_StatementList && state.abort_current_decl {
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
      if expect_item && list_kind is List_StatementList {
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
          (!(list_kind is List_StatementList) || latest_report_is_at_current) &&
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
      if list_kind is List_ArgumentList && recovery_token is Some(ELSE) {
        report_next_top_level_recovery_point(state)
      } else if list_kind is List_StatementList &&
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
    Some((UNDERSCORE, _, _)) =>
      parse_single_token_pattern(state, Pattern_Underscore)
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) => {
      let mut node = parse_single_token_pattern(state, Pattern_Binder)
      if state.current_is(TokenKind::TK_LPAREN) {
        let args = parse_constructor_pattern_args(state)
        node = state.node_from_children(Pattern_SpecialConstructor, [
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
      if state.current_is(TokenKind::TK_LPAREN) {
        push_child(
          children,
          Some("params"),
          parse_constructor_pattern_args(state),
        )
      }
      state.node_from_children(Pattern_Constructor, children)
    }
    Some((LPAREN, _, _)) => {
      let list = parse_constructor_pattern_args(state, tuple_pattern=true)
      state.node_from_children(Pattern_Tuple, [(Some("body"), list)])
    }
    Some((LBRACKET, _, _)) => {
      let list = parse_array_pattern_list(state)
      state.node_from_children(Pattern_Array, [(Some("body"), list)])
    }
    Some((LBRACE, _, _)) =>
      if brace_starts_map_pattern(state) {
        let list = parse_map_pattern_list(state)
        validate_brace_pattern_list(state, list, true)
        state.node_from_children(Pattern_Map, [(Some("body"), list)])
      } else {
        let list = parse_record_pattern_list(state)
        validate_brace_pattern_list(state, list, false)
        state.node_from_children(Pattern_Record, [(Some("body"), list)])
      }
    Some((found, start, end)) if found is (REGEX_INTERP(_) | INTERP(_)) => {
      let pat = parse_regex_interp_lex_pattern_node(state)
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_TRUE,
        TokenKind::TK_FALSE,
        TokenKind::TK_CHAR,
        TokenKind::TK_INT,
        TokenKind::TK_BYTE,
        TokenKind::TK_FLOAT,
        TokenKind::TK_DOUBLE,
        TokenKind::TK_MINUS,
        TokenKind::TK_BYTES,
        TokenKind::TK_STRING,
        TokenKind::TK_UNDERSCORE,
        TokenKind::TK_LIDENT,
        TokenKind::TK_EXTEND,
        TokenKind::TK_PACKAGE_NAME,
        TokenKind::TK_UIDENT,
        TokenKind::TK_LPAREN,
        TokenKind::TK_LBRACKET,
        TokenKind::TK_LBRACE,
      ])
      pat
    }
    Some((found, start, end)) if found is REGEX_LITERAL(_) => {
      let pat = parse_literal_pattern(state, stop_kinds)
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TokenKind::TK_TRUE,
        TokenKind::TK_FALSE,
        TokenKind::TK_CHAR,
        TokenKind::TK_INT,
        TokenKind::TK_BYTE,
        TokenKind::TK_FLOAT,
        TokenKind::TK_DOUBLE,
        TokenKind::TK_MINUS,
        TokenKind::TK_BYTES,
        TokenKind::TK_STRING,
        TokenKind::TK_UNDERSCORE,
        TokenKind::TK_LIDENT,
        TokenKind::TK_EXTEND,
        TokenKind::TK_PACKAGE_NAME,
        TokenKind::TK_UIDENT,
        TokenKind::TK_LPAREN,
        TokenKind::TK_LBRACKET,
        TokenKind::TK_LBRACE,
      ])
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
    state.node_from_children(kind, [(Some("name"), token)])
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
  if state.consume_any() is Some(token) {
    push_child(children, token_label_from_node(token), token)
  } else {
    ()
  }
  let first_is_minus = match children.get(0) {
    Some((_, child)) =>
      match child.kind {
        Token(MINUS) => true
        _ => false
      }
    None => false
  }
  match state.peek() {
    Some((INT(_), _, _)) if first_is_minus =>
      if state.consume_any() is Some(token) {
        push_child(children, Some("token"), token)
      } else {
        ()
      }
    Some((DOUBLE(_), _, _)) if first_is_minus =>
      if state.consume_any() is Some(token) {
        push_child(children, Some("token"), token)
      } else {
        ()
      }
    Some((FLOAT(_), _, _)) if first_is_minus =>
      if state.consume_any() is Some(token) {
        push_child(children, Some("token"), token)
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
  state.node_from_children(Pattern_Literal, children)
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
    List_PatternList,
    sep_kind=TokenKind::TK_COMMA,
    post_item_expected=if tuple_pattern {
      [TokenKind::TK_COMMA, TokenKind::TK_COLON, TokenKind::TK_RPAREN]
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
        (Some("error"), item) if item.first_token() is Some(SEMI(_)) =>
          saw_invalid_statement_separator = true
        (Some("close"), item) => close = Some(item)
        _ => ()
      }
    }
    if saw_invalid_statement_separator {
      if close is Some(item) {
        if item.first_token() is Some(found) {
          state.report_expected_tokens_at(found, item.loc, [
            TokenKind::TK_SEMI,
            TokenKind::TK_RBRACE,
          ])
        } else {
          ()
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
  state.node_from_children(kind, children)
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
  state.node_from_children(Argument_Labelled, children)
}

///|
fn parse_array_pattern_list(state : ParserState) -> CstNode {
  let list = parse_delimited(
    state,
    TokenKind::TK_LBRACKET,
    TokenKind::TK_RBRACKET,
    parse_array_pattern_item,
    List_PatternList,
    sep_kind=TokenKind::TK_COMMA,
  )
  let mut saw_ordinary_rest = false
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), item)) if item.kind is Pattern_Rest => {
        let is_constant_spread = item.first_child_with_label("str") is Some(_) ||
          item.first_child_with_label("bytes") is Some(_) ||
          item.first_child_with_label("const") is Some(_)
        if !is_constant_spread {
          if saw_ordinary_rest {
            let offender = if item.first_child_with_label("name") is Some(name) {
              Some(name)
            } else if item.first_child_with_label("as") is Some(as_token) {
              Some(as_token)
            } else {
              item.first_child_with_label("op")
            }
            if offender is Some(node) {
              state.report(
                node.loc,
                "At most one `..` is allowed in array pattern.",
              )
              if node.first_token() is Some(found) {
                state.report_expected_tokens_at(found, node.loc, [
                  TokenKind::TK_COMMA,
                  TokenKind::TK_RBRACKET,
                ])
              } else {
                ()
              }
            } else {
              ()
            }
          } else {
            saw_ordinary_rest = true
          }
        }
      }
      _ => ()
    }
  }
  list
}

///|
fn parse_array_pattern_item(state : ParserState) -> CstNode {
  match state.peek() {
    Some((DOTDOT, _, _)) => parse_dotdot_pattern_rest(state)
    _ =>
      parse_pattern_until(state, [
        TokenKind::TK_COMMA,
        TokenKind::TK_RBRACKET,
        TokenKind::TK_EOF,
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
) -> Unit {
  let mut map_kind = starts_as_map
  let mut kind_known = starts_as_map
  if !kind_known {
    for i in 0..<list.children.length() {
      match list.children.get(i) {
        Some((Some("item"), item)) =>
          match item.first_token() {
            Some(DOTDOT) => ()
            Some(LIDENT(_)) | Some(EXTEND) => {
              kind_known = true
              map_kind = false
              break
            }
            Some(
              TRUE
              | FALSE
              | CHAR(_)
              | BYTE(_)
              | INT(_)
              | DOUBLE(_)
              | FLOAT(_)
              | STRING(_)
              | BYTES(_)
              | REGEX_LITERAL(_)
              | MINUS
            ) => {
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
        match item.first_token() {
          Some(DOTDOT) =>
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
          Some(LIDENT(_)) | Some(EXTEND) =>
            if map_kind {
              state.report(item.loc, "Unexpected `..` here")
            } else {
              kind_known = true
            }
          Some(
            TRUE
            | FALSE
            | CHAR(_)
            | BYTE(_)
            | INT(_)
            | DOUBLE(_)
            | FLOAT(_)
            | STRING(_)
            | BYTES(_)
            | REGEX_LITERAL(_)
            | MINUS
          ) =>
            if kind_known && !map_kind {
              state.report(
                item.loc,
                "Record pattern and map pattern cannot be mixed",
              )
            } else {
              kind_known = true
              map_kind = true
            }
          Some(found) => {
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
            state.report_expected_tokens_at(found, item.loc, expected)
          }
          None => ()
        }
        item_index += 1
      }
      _ => ()
    }
  }
}

///|
fn parse_dotdot_pattern_rest(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(op) {
    push_child(children, Some("op"), op)
  } else {
    ()
  }
  match state.peek() {
    Some((STRING(_), _, _)) =>
      if state.consume_any() is Some(str) {
        push_child(children, Some("str"), str)
      } else {
        ()
      }
    Some((BYTES(_), _, _)) =>
      if state.consume_any() is Some(bytes) {
        push_child(children, Some("bytes"), bytes)
      } else {
        ()
      }
    Some((UIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("const"), name)
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
            push_child(children, Some("const"), name)
          } else {
            ()
          }
        _ => ()
      }
    }
    Some((AS, _, _)) => {
      if state.consume_any() is Some(as_tok) {
        push_child(children, Some("as"), as_tok)
      } else {
        ()
      }
      match state.peek() {
        Some((LIDENT(_), _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("name"), name)
          } else {
            ()
          }
        _ => ()
      }
    }
    Some((UNDERSCORE, _, _)) | Some((LIDENT(_), _, _)) =>
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    _ => ()
  }
  state.node_from_children(Pattern_Rest, children)
}

///|
fn parse_record_pattern_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_record_pattern_item,
    List_PatternList,
    sep_kind=TokenKind::TK_COMMA,
  )
}

///|
fn parse_record_pattern_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((DOTDOT, _, _)) => {
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      return state.node_from_children(Pattern_Rest, children)
    }
    Some((LIDENT(_), _, _))
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
  state.node_from_children(Clause_Parameter, children)
}

///|
fn parse_map_pattern_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_map_pattern_item,
    List_PatternList,
    sep_kind=TokenKind::TK_COMMA,
  )
}

///|
fn parse_map_pattern_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
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
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      match state.peek() {
        Some((INT(_), _, _))
        | Some((DOUBLE(_), _, _))
        | Some((FLOAT(_), _, _)) =>
          if state.consume_any() is Some(key) {
            push_child(children, Some("key"), key)
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
          return state.node_from_children(Clause_Parameter, children)
        }
        None => ()
      }
    }
    Some((STRING(_), _, _))
    | Some((BYTES(_), _, _))
    | Some((REGEX_LITERAL(_), _, _))
    | Some((INT(_), _, _))
    | Some((BYTE(_), _, _))
    | Some((CHAR(_), _, _))
    | Some((DOUBLE(_), _, _))
    | Some((FLOAT(_), _, _))
    | Some((TRUE, _, _))
    | Some((FALSE, _, _)) =>
      if state.consume_any() is Some(key) {
        push_child(children, Some("key"), key)
      } else {
        ()
      }
    _ => ()
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
  state.node_from_children(Clause_Parameter, children)
}

///|
fn parse_array_pattern_rest(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  parse_pattern_atom(state, stop_kinds)
}

///|
fn parse_map_pattern_entry(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  parse_pattern_atom(state, stop_kinds)
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
        pat = state.node_from_children(Pattern_Binder, children)
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
      lhs = state.node_from_children(Pattern_Range, children)
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
  let body = state.node_from_children(List_PatternList, items)
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
    Some((REGEX_LITERAL(_), _, _)) | Some((STRING(_), _, _)) =>
      if state.consume_any() is Some(token) {
        state.node_from_children(LexPattern_Literal, [
          (token_label_from_node(token), token),
        ])
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
      state.node_from_children(LexPattern_ConstantRef, children)
    }
    Some((tok, _, _)) if !kind_in(tok.kind(), stop_kinds) =>
      if state.consume_any() is Some(token) {
        state.node_from_children(LexPattern_Literal, [
          (token_label_from_node(token), token),
        ])
      } else {
        state.node_from_children(LexPattern_Literal, [])
      }
    _ => state.node_from_children(LexPattern_Literal, [])
  }
}

///|
fn parse_legacy_lex_literal_pattern_until(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut done = false
  while !done {
    match state.peek() {
      Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) => done = true
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) =>
        push_children(children, state.consume_trivia())
      Some((_, _, _)) =>
        if state.consume_any() is Some(token) {
          push_child(children, token_label_from_node(token), token)
        } else {
          done = true
        }
      None => done = true
    }
  }
  state.node_from_children(LexPattern_Literal, children)
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
    parse_regex_as_pattern(state, stop_kinds)
  }
}

///|
fn parse_regex_as_pattern(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let pat = parse_regex_or_pattern(state, stop_kinds)
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
    pat
  }
}

///|
fn parse_regex_or_pattern(
  state : ParserState,
  stop_kinds : Array[TokenKind],
) -> CstNode {
  let _ = state.consume_trivia()
  let mut lhs = parse_regex_sequence_pattern(state, stop_kinds)
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
  let mut lhs = parse_regex_atom_pattern(state, stop_kinds)
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
    Some((REGEX_LITERAL(_), _, _)) =>
      if state.consume_any() is Some(token) {
        state.node_from_children(RegexPattern_Literal, [
          (token_label_from_node(token), token),
        ])
      } else {
        state.node_from_children(RegexPattern_Literal, [])
      }
    Some((UIDENT(_), _, _)) => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
      state.node_from_children(RegexPattern_Reference, children)
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
      state.node_from_children(RegexPattern_Reference, children)
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
  state.node_from_children(List_PatternList, children)
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
          Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
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
  state.node_from_children(Argument_Labelled, children)
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
        push_interp_segments_for_token(state, tok, children)
        node(Expr_Literal, token_node.loc, token_node.source_span, children)
      } else {
        state.node_from_children(Expr_MissingExpr, [])
      }
    None => state.node_from_children(Expr_MissingExpr, [])
  }
}

///|
fn parse_multiline_string_literal(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut done = false
  while !done {
    match state.peek() {
      Some((tok, _, _)) =>
        match tok {
          MULTILINE_STRING(_) | MULTILINE_INTERP(_) =>
            if state.consume_any() is Some(token_node) {
              push_child(children, Some("token"), token_node)
              push_interp_segments_for_token(state, tok, children)
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
  state.node_from_children(Expr_MultilineString, children)
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
            push_interp_segments_for_token(state, tok, children)
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
fn push_interp_segments_for_token(
  state : ParserState,
  tok : Token,
  children : Array[(String?, CstNode)],
) -> Unit {
  match tok {
    INTERP(elems)
    | BYTES_INTERP(elems)
    | MULTILINE_INTERP(elems)
    | REGEX_INTERP(elems) =>
      for elem in elems {
        push_child(children, Some("segment"), parse_interp_segment(state, elem))
      }
    _ => ()
  }
}

///|
fn parse_interp_segment(
  state : ParserState,
  elem : @tokens.InterpElem,
) -> CstNode {
  match elem {
    InterpLit(repr~, loc~) =>
      node(InterpSegment_InterpLiteralSegment, loc, loc_span(state, loc), [
        (Some("repr"), node(Token(STRING(repr)), loc, loc_span(state, loc), [])),
      ])
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
  push_child(
    children,
    Some("source"),
    node(
      Token(STRING(interp_source.source)),
      interp_source.loc,
      loc_span(state, interp_source.loc),
      [],
    ),
  )
  node(
    InterpSegment_InterpSourceSegment,
    interp_source.loc,
    loc_span(state, interp_source.loc),
    children,
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
    comment=true,
  )
  for report in reports_from_lex_errors(lex.errors) {
    state.reports.push(report)
  }
  let inner_state = new_state(lex.tokens, state.source, state.reports)
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
fn top_item_kind(tok : Token) -> NodeKind {
  match tok {
    TEST => Decl_Test
    USING => Impl_Using
    EXTERN => Decl_BalancedDecl
    FN | ASYNC => Decl_Function
    LET => Decl_Let
    CONST => Decl_Const
    TYPE => Decl_Type
    STRUCT => Decl_Struct
    ENUM | SUBERROR | EXTENUM => Decl_Enum
    ENUMVIEW => Decl_EnumView
    TRAIT => Decl_Trait
    IMPL => Decl_Impl
    DECLARE | PUB | PRIV | ATTRIBUTE(_) => Decl_BalancedDecl
    _ => Error
  }
}

///|
fn ParserState::node_from_children(
  self : ParserState,
  kind : NodeKind,
  children : Array[(String?, CstNode)],
) -> CstNode {
  if children.length() == 0 {
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
}

///|
fn parse_token_list(state : ParserState, kind : NodeKind) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut saw_eof = false
  let mut done = false
  while !done {
    match state.peek() {
      Some((EOF, _, _)) => {
        if state.consume_any() is Some(eof) {
          children.push((Some("eof"), eof))
        } else {
          ()
        }
        saw_eof = true
        done = true
      }
      Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) =>
        for child in state.consume_trivia() {
          children.push(child)
        }
      Some((tok, _, _)) =>
        if state.consume_any() is Some(consumed) {
          children.push((token_label(tok), consumed))
        } else {
          ()
        }
      None => done = true
    }
  }
  if !saw_eof {
    children.push((Some("eof"), state.missing_eof_node()))
  }
  node(kind, state.root_loc(), state.root_span(), children)
}

///|
fn parse_top_level_item(state : ParserState) -> CstNode {
  let first_kind = match state.peek_significant() {
    Some((ASYNC, _, _)) =>
      match state.peek_nth_significant(1) {
        Some((TEST, _, _)) => Decl_Test
        _ => Decl_Function
      }
    Some((tok, _, _)) => top_item_kind(tok)
    None => Error
  }
  parse_balanced_decl_body(state, first_kind)
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
      Some(_) => children.push((Some("item"), parse_top_level_item(state)))
      None => done = true
    }
  }
  if !saw_eof {
    children.push((Some("eof"), state.missing_eof_node()))
  }
  node(SourceFile, state.root_loc(), state.root_span(), children)
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
    Some((UNDERSCORE, _, _)) => parse_single_token_type(state, Type_Hole)
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
  state.node_from_children(Type_Name, children)
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
        return state.node_from_children(Type_Function, children)
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
      return state.node_from_children(Type_Function, children)
    }
    None => state.report_expected_tokens([TokenKind::TK_LPAREN])
  }
  let ty = parse_type_arrow(state, stop_kinds)
  if !ty.children.is_empty() {
    push_child(children, Some("type"), ty)
  }
  state.node_from_children(Type_Function, children)
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
    List_TypeArgumentList,
    sep_kind=TokenKind::TK_COMMA,
  )
  let mut item_count = 0
  let mut sep_count = 0
  for i in 0..<list.children.length() {
    match list.children.get(i) {
      Some((Some("item"), _)) => item_count += 1
      Some((Some("sep"), _)) => sep_count += 1
      _ => ()
    }
  }
  let kind = if sep_count > 0 || item_count != 1 {
    Type_Tuple
  } else {
    Type_Group
  }
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
  state.node_from_children(Clause_Parameter, children)
}

///|
fn parse_object_type_literal(state : ParserState) -> CstNode {
  let list = parse_delimited(
    state,
    TokenKind::TK_LBRACE,
    TokenKind::TK_RBRACE,
    parse_object_field_type_item,
    List_FieldList,
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
          push_child(children, Some("clause"), parse_error_annotation(state))
        Some((NORAISE, _, _)) if !kind_in(TokenKind::TK_NORAISE, stop_kinds) =>
          push_child(children, Some("clause"), parse_error_annotation(state))
        _ => ()
      }
      if invalid_unparenthesized {
        state.report(
          empty_loc(state.current_position()),
          "Unexpected token here.",
        )
      }
      state.node_from_children(Type_Function, children)
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
        TokenKind::TK_LBRACE,
        TokenKind::TK_EQUAL,
        TokenKind::TK_SEMI,
        TokenKind::TK_EOF,
      ])
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
    Some((RAISE, _, _)) | Some((NORAISE, _, _)) =>
      push_child(children, Some("clause"), parse_error_annotation(state))
    _ => ()
  }
  state.node_from_children(Clause_ReturnType, children)
}

///|
fn parse_error_annotation(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  match state.peek() {
    Some((RAISE, _, _)) | Some((NORAISE, _, _)) =>
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
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
        state.node_from_children(Type_Name, type_children),
      )
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
        state.node_from_children(Type_Name, type_children),
      )
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
        state.node_from_children(Type_Name, type_children),
      )
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
    }
    _ =>
      match state.peek() {
        Some((QUESTION, _, _)) =>
          if state.consume_any() is Some(question) {
            push_child(children, Some("type"), question)
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
              Type_Name | Type_Hole => ()
              _ => state.report(err_ty.loc, "invalid error type")
            }
            push_child(children, Some("type"), err_ty)
          }
        }
        _ => ()
      }
  }
  state.node_from_children(Clause_ErrorAnnotation, children)
}
