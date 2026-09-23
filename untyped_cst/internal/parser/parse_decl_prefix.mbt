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
    TK_PUB,
    TK_PRIV,
    TK_TYPE,
    TK_SUBERROR,
    TK_ASYNC,
    TK_FN,
    TK_STRUCT,
    TK_ENUM,
    TK_EXTENUM,
    TK_LET,
    TK_CONST,
    TK_EXTERN,
    TK_TEST,
    TK_IMPL,
    TK_TRAIT,
    TK_ENUMVIEW,
    TK_ATTRIBUTE,
    TK_USING,
    TK_EXTEND,
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
fn decl_name_expectation(kind : TopItemKind) -> Array[TokenKind] {
  match kind {
    Function => [TK_LIDENT, TK_EXTEND, TK_PACKAGE_NAME]
    Impl => [TK_UIDENT, TK_PACKAGE_NAME]
    _ => [TK_LIDENT]
  }
}

///|
fn decl_phase_expectation(
  phase : DeclPhase,
  kind : TopItemKind,
) -> DeclExpectation {
  match phase {
    AwaitingName => ExpectedTokens(decl_name_expectation(kind))
    AwaitingLetPattern => ExpectedTokens([TK_LIDENT, TK_EXTEND])
    AwaitingParameters => ExpectedTokens([TK_LPAREN])
    AwaitingEqual => ExpectedTokens([TK_EQUAL])
    AwaitingValue => ExpectedDescription("simple expression")
    AwaitingType => ExpectedDescription("type")
    AwaitingBody | AwaitingEnumViewConstructors => ExpectedTokens([TK_LBRACE])
    AwaitingImplRelation => ExpectedTokens([TK_FOR, TK_WITH])
    AwaitingEnumViewFor => ExpectedTokens([TK_FOR])
    AwaitingEnumViewWith => ExpectedTokens([TK_WITH])
    AwaitingMethodName => ExpectedTokens([TK_LIDENT, TK_EXTEND])
    AwaitingUsingName => ExpectedTokens([TK_PACKAGE_NAME])
    AwaitingExternLanguage | AwaitingStub => ExpectedTokens([TK_STRING])
    AwaitingExternFunction => ExpectedTokens([TK_FN])
    Start | Complete => NoExpectation
  }
}

///|
fn decl_phase_after_name(kind : TopItemKind, is_suberror : Bool) -> DeclPhase {
  match kind {
    Function => AwaitingBody
    Const => AwaitingEqual
    Type => Complete
    Struct => AwaitingBody
    Enum => if is_suberror { Complete } else { AwaitingBody }
    Trait => AwaitingBody
    Impl => AwaitingImplRelation
    EnumView => AwaitingEnumViewConstructors
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
    FN => Function
    CONST => Const
    TYPE => Type
    STRUCT => Struct
    ENUM | SUBERROR | EXTENUM => Enum
    ENUMVIEW => EnumView
    TRAIT => Trait
    IMPL => Impl
    _ => Invalid
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
  let mut visibility_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let _ = children
    @node.CstNode::visibility_default(loc~, source_span~)
  }
  match state.peek() {
    Some((PUB, _, _)) => {
      is_pub = true
      visibility_builder = fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::visibility_pub(loc~, source_span~, entries~)
      }
      if state.consume_any() is Some(kw) {
        push_child(children, Some("kw"), kw)
      } else {
        ()
      }
    }
    Some((PRIV, _, _)) => {
      visibility_builder = fn(loc, source_span, children) {
        let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::visibility_priv(loc~, source_span~, kw~)
      }
      if state.consume_any() is Some(kw) {
        push_child(children, Some("kw"), kw)
      } else {
        ()
      }
    }
    _ => push_child(children, Some("kw"), state.missing_token_node(TK_PUB))
  }
  if is_pub && state.current_is(TK_LPAREN) {
    push_child(children, Some("open"), state.expect_token(TK_LPAREN))
    match state.peek() {
      Some((LIDENT(_), _, _)) | Some((READONLY, _, _)) | Some((EXTEND, _, _)) =>
        if state.consume_any() is Some(attr) {
          push_child(children, Some("attr"), attr)
        } else {
          ()
        }
      Some((found, start, end)) => {
        state.report_expected_tokens_at(found, mk_loc(start, end), [
          TK_READONLY,
          TK_LIDENT,
          TK_EXTEND,
        ])
        while !state.current_is(TK_RPAREN) &&
              !state.current_is(TK_EOF) &&
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
          state.missing_token_node_silent(TK_LIDENT),
        )
      }
      None =>
        push_child(
          children,
          Some("attr"),
          state.missing_token_node_silent(TK_LIDENT),
        )
    }
    push_child(children, Some("close"), state.expect_token(TK_RPAREN))
  }
  state.construct_from_children(visibility_builder, children)
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
    Function => tok is (LIDENT(_) | UIDENT(_) | PACKAGE_NAME(_) | EXTEND)
    Type | Struct | Enum | EnumView | Trait | Impl =>
      tok is (UIDENT(_) | PACKAGE_NAME(_))
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
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
        },
        empty_loc(pos),
        empty_span(state.offset_for_position(pos)),
        [],
      )
    }
    Some((POST_LABEL(_), _, _)) => {
      is_labelled = true
      parse_single_token_pattern(state, fn(loc, source_span, children) {
        push_structured_name(
          children,
          "semantic_name",
          fn(loc, source_span, children) {
            let long_ident = @construction.first_child_with_label_from(
              children, "long_ident",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            @node.CstNode::name_binder(loc~, source_span~, long_ident~)
          },
          ["name"],
        )

        let entries = recovery_children(children)
        @node.CstNode::pattern_var(loc~, source_span~, entries~)
      })
    }
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) =>
      parse_single_token_pattern(state, fn(loc, source_span, children) {
        push_structured_name(
          children,
          "semantic_name",
          fn(loc, source_span, children) {
            let long_ident = @construction.first_child_with_label_from(
              children, "long_ident",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            @node.CstNode::name_binder(loc~, source_span~, long_ident~)
          },
          ["name"],
        )

        let entries = recovery_children(children)
        @node.CstNode::pattern_var(loc~, source_span~, entries~)
      })
    Some((UNDERSCORE, _, _)) =>
      parse_single_token_pattern(state, fn(loc, source_span, children) {
        let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let lex_scan_case_pattern = @construction.first_child_with_label_from(
          children, "lex_scan_case_pattern",
        )
        @node.CstNode::pattern_any(
          loc~,
          source_span~,
          name~,
          lex_scan_case_pattern?,
        )
      })
    Some((found, start, end)) => {
      state.report_expected_description_at(
        found,
        mk_loc(start, end),
        "parameter name or label",
      )
      let invalid : Array[(String?, CstNode)] = []
      while !state.current_is(TK_COMMA) &&
            !state.current_is(TK_RPAREN) &&
            !state.current_is(TK_EOF) &&
            !(state.peek() is None) {
        if state.consume_any() is Some(bad) {
          push_child(invalid, Some("error"), bad)
        } else {
          ()
        }
      }
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
        },
        invalid,
      )
    }
    None =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
        },
        [],
      )
  }
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  if state.current_is(TK_QUESTION) {
    has_question = true
    if state.consume_any() is Some(question) {
      push_child(children, Some("question"), question)
    } else {
      ()
    }
  }
  if state.current_is(TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let reports_before_type = state.reports.length()
    let ty = parse_type_arrow(state, [TK_EQUAL, TK_COMMA, TK_RPAREN, TK_EOF])
    push_child(
      children,
      Some("type"),
      state.required_type_node(ty, reports_before_type),
    )
    push_children(children, state.consume_trivia())
  }
  if state.current_is(TK_EQUAL) {
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
    let default = parse_expr_until(state, [TK_COMMA, TK_RPAREN, TK_EOF])
    if !default.children.is_empty() {
      push_child(children, Some("default"), default)
    }
  }
  let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if pat.kind
    is Pattern_Any {
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::parameter_discard_positional(loc~, source_span~, entries~)
    }
  } else if is_labelled {
    if has_default {
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::parameter_optional(loc~, source_span~, entries~)
      }
    } else {
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::parameter_labelled(loc~, source_span~, entries~)
      }
    }
  } else if has_default {
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::parameter_optional(loc~, source_span~, entries~)
    }
  } else if has_question {
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::parameter_question_optional(loc~, source_span~, entries~)
    }
  } else {
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::parameter_positional(loc~, source_span~, entries~)
    }
  }
  state.construct_from_children(builder, children)
}

///|
fn parse_parameter_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LPAREN,
    TK_RPAREN,
    parse_parameter_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_parameter_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
    follow_set=[TokenKind::TK_LBRACE],
    item_expected="parameter name or label",
    post_item_expected=[TK_COMMA, TK_RPAREN],
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
                  fn(loc, source_span, children) {
                    let value = match
                      @construction.payload_from_children(children) {
                      Some(NodePayload::String(value)) => value
                      _ => ""
                    }
                    @node.CstNode::stub_module_name(value~, loc~, source_span~)
                  },
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
                fn(loc, source_span, children) {
                  let value = match
                    @construction.payload_from_children(children) {
                    Some(NodePayload::String(value)) => value
                    _ => ""
                  }
                  @node.CstNode::stub_function_name(value~, loc~, source_span~)
                },
                stub.loc,
                stub.source_span,
                String(second_value),
              ),
            )
            push_child(
              children,
              Some("decl_body_kind"),
              marker_node(
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::decl_body_import(loc~, source_span~)
                },
                stub.loc.end,
                stub.source_span.1,
              ),
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
                fn(loc, source_span, children) {
                  let value = match
                    @construction.payload_from_children(children) {
                    Some(NodePayload::String(value)) => value
                    _ => ""
                  }
                  @node.CstNode::stub_code(value~, loc~, source_span~)
                },
                first.loc,
                first.source_span,
                String(first_value),
              ),
            )
            push_child(
              children,
              Some("decl_body_kind"),
              marker_node(
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::decl_body_embedded_string(loc~, source_span~)
                },
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
                  fn(loc, source_span, children) {
                    let value = match
                      @construction.payload_from_children(children) {
                      Some(NodePayload::String(value)) => value
                      _ => ""
                    }
                    @node.CstNode::stub_code(value~, loc~, source_span~)
                  },
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
                    fn(loc, source_span, children) {
                      let _ = children
                      @node.CstNode::decl_body_embedded_multiline_string(
                        loc~,
                        source_span~,
                      )
                    },
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
    Type | Struct | Enum => true
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
    _ => push_child(children, Some("name"), state.missing_token_node(TK_UIDENT))
  }
  if state.current_is(TK_COLON) {
    if !allow_constraints {
      state.report_expected_tokens([TK_COMMA, TK_RBRACKET])
    }
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let ty = parse_type_until(state, [TK_PLUS, TK_COMMA, TK_RBRACKET, TK_EOF])
    if !ty.children.is_empty() {
      push_child(children, Some("type"), ty)
    }
    while state.current_is(TK_PLUS) {
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let ty = parse_type_until(state, [TK_PLUS, TK_COMMA, TK_RBRACKET, TK_EOF])
      if !ty.children.is_empty() {
        push_child(children, Some("type"), ty)
      }
    }
  }
  state.construct_from_children(
    if allow_constraints {
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::parameter_type_var(loc~, source_span~, entries~)
      }
    } else {
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::parameter_type_decl_from_entries(
          loc~,
          source_span~,
          entries~,
        )
      }
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
    TK_LBRACKET,
    TK_RBRACKET,
    parse_type_decl_param_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_type_decl_parameter_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
    require_nonempty=true,
  )
}

///|
fn parse_fun_type_parameter_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LBRACKET,
    TK_RBRACKET,
    parse_fun_type_param_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_type_var_binder_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
    require_nonempty=true,
  )
}

///|
fn parse_tuple_struct_type_item(state : ParserState) -> CstNode {
  parse_type_arrow(state, [TK_COMMA, TK_RPAREN, TK_EOF])
}

///|
fn parse_tuple_struct_type_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LPAREN,
    TK_RPAREN,
    parse_tuple_struct_type_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_tuple_struct_type_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_COMMA,
    require_nonempty=true,
    nonempty_expected="type",
  )
}

///|
fn decl_body_uses_balanced_group(kind : TopItemKind) -> Bool {
  match kind {
    Struct | Enum | Trait => true
    _ => false
  }
}

///|
fn decl_body_list_builder(
  kind : TopItemKind,
) -> (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode {
  match kind {
    TopItemKind::Enum =>
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_constructor_list(loc~, source_span~, entries~)
      }
    TopItemKind::Struct =>
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_struct_field_list(loc~, source_span~, entries~)
      }
    TopItemKind::Trait =>
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_trait_method_list(loc~, source_span~, entries~)
      }
    _ =>
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_block_statement_list(loc~, source_span~, entries~)
      }
  }
}

///|
fn parse_balanced_brace_group(
  state : ParserState,
  list_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
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
  state.construct_from_children(list_builder, children)
}

///|
fn parse_struct_member_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LBRACE,
    TK_RBRACE,
    parse_struct_member_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_struct_field_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_SEMI,
    invalid_delims=[TokenKind::TK_COMMA],
  )
}

///|
fn parse_enum_constructor_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LBRACE,
    TK_RBRACE,
    parse_enum_constructor_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_constructor_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_SEMI,
    invalid_delims=[TokenKind::TK_COMMA],
  )
}

///|
fn parse_trait_member_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LBRACE,
    TK_RBRACE,
    parse_trait_member_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_trait_method_list(loc~, source_span~, entries~)
    },
    sep_kind=TokenKind::TK_SEMI,
  )
}
