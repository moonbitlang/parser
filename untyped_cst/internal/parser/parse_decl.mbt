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
        if kind is Function &&
          tok is LIDENT(name) &&
          state.peek_nth_significant(1).map(item => item.0) is Some(COLONCOLON) {
          state.report(
            mk_loc(start, end),
            "Unexpected lowercase identifier \{name}, expected uppercase identifier in type name.",
          )
        }
        if !decl_name_token_is_valid(tok, kind) {
          match (kind, tok) {
            (Struct, LIDENT(name)) =>
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
      Some((LBRACKET, _, _)) if phase is AwaitingName && kind is Function =>
        push_child(
          children,
          Some("quantifier"),
          parse_fun_type_parameter_list(state),
        )
      Some((LBRACKET, _, _)) if phase is AwaitingName &&
        (kind is Impl || kind is EnumView) =>
        push_child(
          children,
          Some("quantifier"),
          parse_fun_type_parameter_list(state),
        )
      Some((AMPER, _, _)) if phase is AwaitingName && kind is Function =>
        if state.consume_any() is Some(op) {
          push_child(children, Some("op"), op)
          push_child(
            children,
            Some("object"),
            bool_marker_node(
              fn(loc, source_span, children) {
                let value = match
                  @construction.payload_from_children(children) {
                  Some(NodePayload::Bool(value)) => value
                  _ => false
                }
                @node.CstNode::flag_object(value~, loc~, source_span~)
              },
              op.loc.start,
              op.source_span.0,
              true,
            ),
          )
        } else {
          ()
        }
      Some((LPAREN, _, _)) if kind is Struct => {
        push_child(children, Some("body"), parse_tuple_struct_type_list(state))
        saw_body = true
        accepts_params = false
        phase = Complete
      }
      Some((LPAREN, _, _)) if accepts_params && kind is Function => {
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
          kind is Type ||
          kind is Struct ||
          kind is Enum ||
          kind is EnumView ||
          kind is Impl
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
        kind is Function =>
        if state.consume_any() is Some(name) {
          push_child(children, Some("name"), name)
        } else {
          ()
        }
      Some((COLONCOLON, _, _)) if accepts_params && kind is Function => {
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
              state.missing_token_node(TK_LIDENT),
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
          TK_COLON,
          TK_EQUAL,
          TK_SEMI,
          TK_EOF,
        ])
        if !pat.children.is_empty() {
          push_child(children, Some("pattern"), pat)
        } else {
          push_child(
            children,
            Some("pattern"),
            state.missing_token_node_with_expected(TK_LIDENT, [
              TK_LIDENT,
              TK_EXTEND,
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
        let pat = parse_single_token_pattern(state, fn(
          loc,
          source_span,
          children,
        ) {
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
        push_child(children, Some("pattern"), pat)
        phase = AwaitingEqual
        match state.peek() {
          Some((found, next_start, next_end)) if !(found is EQUAL) => {
            state.report_expected_tokens_at(
              found,
              mk_loc(next_start, next_end),
              [TK_EQUAL],
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
          TK_LIDENT,
          TK_EXTEND,
        ])
        push_child(
          children,
          Some("pattern"),
          state.missing_token_node_silent(TK_LIDENT),
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
      Some((TEST, _, _)) if kind is Test => {
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
                  fn(loc, source_span, children) {
                    let value = match
                      @construction.payload_from_children(children) {
                      Some(NodePayload::String(value)) => value
                      _ => ""
                    }
                    @node.CstNode::name_test(value~, loc~, source_span~)
                  },
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
        if state.current_is(TK_LPAREN) {
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
                  fn(loc, source_span, children) {
                    let value = match
                      @construction.payload_from_children(children) {
                      Some(NodePayload::String(value)) => value
                      _ => ""
                    }
                    @node.CstNode::language_value(value~, loc~, source_span~)
                  },
                  language.loc,
                  language.source_span,
                  String(value),
                ),
              )
            } else {
              ()
            }
          _ => state.report_expected_tokens([TK_STRING])
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
              state.report_expected_tokens_at(found, mk_loc(start, end), [TK_FN])
            None => ()
          }
          while !state.current_is(TK_EOF) && !(state.peek() is None) {
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
      Some((USING, _, _)) if kind is Using => {
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
          while !state.current_is(TK_EOF) && !(state.peek() is None) {
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
          TK_COLON,
          TK_COMMA,
          TK_RPAREN,
          TK_EOF,
        ])
        if !pat.children.is_empty() {
          push_child(children, Some("pattern"), pat)
        }
      }
      Some((COLON, _, _)) if kind is Trait && depth == 0 => {
        if state.consume_any() is Some(colon) {
          push_child(children, Some("op"), colon)
        } else {
          ()
        }
        let stops = [TokenKind::TK_PLUS, TK_LBRACE, TK_SEMI, TK_EOF]
        let reports_before_type = state.reports.length()
        let ty = parse_type_arrow(state, stops)
        let ty = if !ty.children.is_empty() {
          ty
        } else {
          if state.reports.length() == reports_before_type {
            state.report_expected_tokens([TK_UIDENT, TK_PACKAGE_NAME])
          }
          state.missing_type_node()
        }
        push_child(children, Some("type"), ty)
        while state.current_is(TK_PLUS) {
          if state.consume_any() is Some(op) {
            push_child(children, Some("op"), op)
          } else {
            ()
          }
          let ty = parse_type_arrow(state, stops)
          let ty = if !ty.children.is_empty() {
            ty
          } else {
            state.report_expected_tokens([TK_UIDENT, TK_PACKAGE_NAME])
            state.missing_type_node()
          }
          push_child(children, Some("type"), ty)
        }
      }
      Some((COLON, start, end)) if kind is Function && depth == 0 => {
        state.report_expected_tokens_at(COLON, mk_loc(start, end), [TK_LBRACE])
        while !state.current_is(TK_EOF) && !(state.peek() is None) {
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
          TK_COMMA,
          TK_RPAREN,
          TK_RBRACKET,
          TK_RBRACE,
          TK_EQUAL,
          TK_SEMI,
          TK_EOF,
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
      Some((EQUAL, _, _)) if kind is Type => {
        phase = AwaitingType
        if state.consume_any() is Some(equal) {
          push_child(children, Some("op"), equal)
        } else {
          ()
        }
        let reports_before_type = state.reports.length()
        let ty = parse_type_arrow(state, [TK_SEMI, TK_EOF])
        let ty = state.required_type_node(ty, reports_before_type)
        push_child(children, Some("type"), ty)
        if !(ty.kind is Type_MissingType) {
          phase = Complete
        }
      }
      Some((EQUAL, _, _)) if kind is Let || kind is Const => {
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
        let expr = parse_expr_until(state, [TK_SEMI, TK_EOF])
        if !expr.children.is_empty() {
          push_child(children, Some("expr"), expr)
          phase = Complete
        } else {
          state.report_expected_description("simple expression")
          push_child(children, Some("expr"), state.missing_expr_node())
        }
        if value_started_with_loop &&
          state.current_is(TK_SEMI) &&
          state.peek_nth_significant(1).map(item => item.0) is Some(LPAREN) {
          state.suppress_next_top_level_recovery_report = true
        }
      }
      Some((EQUAL, _, _)) if kind is Function => {
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
          _ => state.report_expected_tokens([TK_STRING])
        }
        done = true
      }
      Some((FOR, _, _)) if kind is Impl => {
        phase = AwaitingType
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        } else {
          ()
        }
        let reports_before_type = state.reports.length()
        let ty = parse_type_arrow(state, [TK_WITH, TK_LBRACE, TK_SEMI, TK_EOF])
        let ty = state.required_type_node(ty, reports_before_type)
        push_child(children, Some("type"), ty)
        if !(ty.kind is Type_MissingType) {
          phase = Complete
        }
      }
      Some((FOR, _, _)) if kind is EnumView => {
        phase = AwaitingType
        if state.consume_any() is Some(kw) {
          push_child(children, Some("kw"), kw)
        } else {
          ()
        }
        let reports_before_type = state.reports.length()
        let ty = parse_type_arrow(state, [TK_WITH, TK_SEMI, TK_EOF])
        let ty = state.required_type_node(ty, reports_before_type)
        push_child(children, Some("type"), ty)
        if !(ty.kind is Type_MissingType) {
          phase = AwaitingEnumViewWith
        }
      }
      Some((WITH, _, _)) if kind is Impl => {
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
      Some((WITH, _, _)) if kind is EnumView => {
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
            state.report_expected_tokens([TK_LIDENT, TK_EXTEND])
            push_child(
              children,
              Some("method"),
              state.missing_token_node_silent(TK_LIDENT),
            )
          }
        }
        if state.current_is(TK_LPAREN) {
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
        if state.current_is(TK_LPAREN) {
          push_child(children, Some("deriving"), parse_deriving_list(state))
        } else {
          push_child(
            children,
            Some("deriving"),
            state.missing_token_node(TK_LPAREN),
          )
        }
      }
      Some((found, start, end)) if kind is Function &&
        phase is AwaitingBody &&
        found is (EXCLAMATION | QUESTION) => {
        state.report_expected_tokens_at(found, mk_loc(start, end), [TK_LBRACE])
        while !state.current_is(TK_LBRACE) &&
              !state.current_is(TK_SEMI) &&
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
      }
      Some((WHERE, _, _)) if kind is Function =>
        push_child(children, Some("where"), parse_where_clause(state))
      Some((EXCLAMATION, _, _)) if kind is Function && !(phase is AwaitingName) =>
        if state.consume_error_for_progress() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      Some((LBRACKET, _, _)) if accepts_params && kind is Function && !saw_body =>
        match state.peek() {
          Some((found, start, end)) => {
            let expected = if is_extern {
              [TokenKind::TK_EQUAL]
            } else {
              [TK_LBRACE]
            }
            state.report_expected_tokens_at(found, mk_loc(start, end), expected)
            let recovery_kind = if is_extern {
              TokenKind::TK_EQUAL
            } else {
              TK_LBRACE
            }
            while !state.current_is(recovery_kind) &&
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
          }
          None => done = true
        }
      Some((QUESTION, _, _)) if depth == 0 &&
        (!saw_core_keyword || kind is Invalid) =>
        if state.consume_error_for_progress() is Some(error) {
          push_child(children, Some("error"), error)
        } else {
          done = true
        }
      Some((RBRACE, start, end)) if kind is Function && !saw_body => {
        state.report_expected_tokens_at(RBRACE, mk_loc(start, end), [TK_LBRACE])
        if state.consume_any() is Some(bad) {
          push_child(children, Some("error"), bad)
        } else {
          ()
        }
        saw_body = true
        phase = Complete
        done = true
      }
      Some((found, start, end)) if kind is Invalid ||
        (!saw_core_keyword && phase is Start) => {
        if state.suppress_next_top_level_recovery_report {
          state.suppress_next_top_level_recovery_report = false
        } else if saw_async_prefix {
          state.report_expected_tokens_at(found, mk_loc(start, end), [TK_FN])
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
      Some((LBRACE, start, end)) if kind is Function && phase is AwaitingName => {
        let mut recovery_end = end
        while !state.current_is(TK_EOF) && !(state.peek() is None) {
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
        if kind is Using {
          saw_body = true
          push_child(children, Some("body"), parse_using_name_list(state))
          phase = Complete
        } else if kind is Impl {
          if phase is AwaitingImplRelation {
            match state.peek() {
              Some((found, start, end)) =>
                state.report_expected_tokens_at(found, mk_loc(start, end), [
                  TK_FOR,
                  TK_WITH,
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
            parse_balanced_brace_group(state, decl_body_list_builder(kind)),
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
        } else if kind is Struct {
          saw_body = true
          let body = parse_struct_member_list(state)
          recovered_body_without_close = child_label_is_missing(body, "close")
          push_child(children, Some("body"), body)
          phase = Complete
        } else if kind is Enum {
          saw_body = true
          push_child(children, Some("body"), parse_enum_constructor_list(state))
          phase = Complete
        } else if kind is EnumView && !enumview_has_constructors {
          saw_body = true
          push_child(children, Some("body"), parse_enum_constructor_list(state))
          enumview_has_constructors = true
          phase = AwaitingEnumViewFor
        } else if kind is EnumView {
          saw_body = true
          push_child(children, Some("expr"), parse_block_expr(state))
          phase = Complete
        } else if kind is Trait {
          saw_body = true
          push_child(children, Some("body"), parse_trait_member_list(state))
          phase = Complete
        } else if decl_body_uses_balanced_group(kind) {
          saw_body = true
          push_child(
            children,
            Some("body"),
            parse_balanced_brace_group(state, decl_body_list_builder(kind)),
          )
          phase = Complete
        } else {
          saw_body = true
          let body = parse_block_expr(state)
          push_child(children, Some("body"), body)
          phase = Complete
          if state.abort_current_decl {
            state.abort_current_decl = false
            if state.current_is(TK_SEMI) || state.current_is(TK_EOF) {
              done = true
            }
          }
        }
      Some((found, start, end)) if phase is AwaitingName => {
        let expected = decl_name_expectation(kind)
        if kind is Function {
          let mut recovery_end = end
          while !state.current_is(TK_EOF) && !(state.peek() is None) {
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
        } else if kind is Type {
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
                [TK_SEMI, TK_EOF, TK_DERIVE, TK_EQUAL],
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
          Some((found, start, end)) if kind is Type =>
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
                TK_SEMI,
                TK_EOF,
                TK_DERIVE,
                TK_EQUAL,
              ])
            None =>
              match state.peek() {
                Some((found, start, end)) if kind is Type =>
                  state.report_expected_tokens_at(found, mk_loc(start, end), [
                    TK_SEMI,
                    TK_EOF,
                    TK_DERIVE,
                    TK_EQUAL,
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
        kind is Function &&
        !is_extern &&
        phase is AwaitingBody &&
        params_complete &&
        (
          !return_type_had_error ||
          missing_return_before_impl ||
          state.current_is(TK_EOF)
        )
      )
    )
  if should_report_completion {
    if kind is Function && is_declare && (saw_body || saw_stub) {
      state.report(
        empty_loc(state.current_position()),
        "declare function cannot have a body",
      )
    } else if kind is Enum && suberror_had_type_params {
      state.report(
        empty_loc(state.current_position()),
        "Unexpected token here.",
      )
    } else {
      state.report_decl_phase(phase, kind)
    }
  }
  let (final_kind, final_builder) = classify_top_item_construction(
    children, kind, is_suberror, is_extenum, saw_body,
  )
  let (loc, source_span) = state.node_from_children(children)
  match final_kind {
    Function =>
      if @construction.first_child_with_label_from(children, "method")
        is Some(_) {
        push_structured_name(
          children,
          "semantic_type_name",
          fn(loc, source_span, children) {
            let long_ident = @construction.first_child_with_label_from(
              children, "long_ident",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            @node.CstNode::name_type(loc~, source_span~, long_ident~)
          },
          ["name"],
        )
        push_structured_name(
          children,
          "semantic_name",
          fn(loc, source_span, children) {
            let long_ident = @construction.first_child_with_label_from(
              children, "long_ident",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            @node.CstNode::name_binder(loc~, source_span~, long_ident~)
          },
          ["method"],
        )
      } else {
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
      }
    TypeAbstract
    | TypeAlias
    | RecordStruct
    | TupleStruct
    | EnumVariant
    | ExtensibleEnum
    | ExtendEnum
    | Suberror
    | SuberrorEnum
    | EnumView =>
      push_structured_name(
        children,
        "semantic_name",
        fn(loc, source_span, children) {
          let long_ident = @construction.first_child_with_label_from(
            children, "long_ident",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::name_type(loc~, source_span~, long_ident~)
        },
        ["name"],
      )
    Trait =>
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
    ImplMethod | ImplRelation =>
      push_structured_name(
        children,
        "semantic_name",
        fn(loc, source_span, children) {
          let long_ident = @construction.first_child_with_label_from(
            children, "long_ident",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::name_type(loc~, source_span~, long_ident~)
        },
        ["name"],
      )
    Using =>
      push_structured_name(
        children,
        "semantic_name",
        fn(loc, source_span, children) {
          let long_ident = @construction.first_child_with_label_from(
            children, "long_ident",
          )
          let payload = @construction.first_child_with_label_from(
            children, "payload",
          )
          let syntax = @construction.first_child_with_label_from(
            children, "syntax",
          )
          let name_value = @construction.first_child_with_label_from(
            children, "name_value",
          )
          @node.CstNode::name_label(
            loc~,
            source_span~,
            long_ident?,
            payload?,
            syntax?,
            name_value?,
          )
        },
        ["name"],
      )
    _ => ()
  }
  if type_desc_builder_for_shape(final_kind) is Some(type_desc_builder) {
    push_marker_child(
      children, loc, source_span, "type_desc", type_desc_builder,
    )
  }
  if @construction.first_child_with_label_from(children, "vis") is None {
    push_marker_child(children, loc, source_span, "vis", fn(
      loc,
      source_span,
      children,
    ) {
      let _ = children
      @node.CstNode::visibility_default(loc~, source_span~)
    })
  }
  if bool_marker_value_from_children(children, fn(kind) { kind is Flag_Declare })
    is None {
    push_bool_marker(
      children,
      loc,
      source_span,
      "declare",
      fn(loc, source_span, children) {
        let value = match @construction.payload_from_children(children) {
          Some(NodePayload::Bool(value)) => value
          _ => false
        }
        @node.CstNode::flag_declare(value~, loc~, source_span~)
      },
      is_declare,
    )
  }
  if bool_marker_value_from_children(children, fn(kind) { kind is Flag_Async })
    is None {
    push_bool_marker(
      children,
      loc,
      source_span,
      "async",
      fn(loc, source_span, children) {
        let value = match @construction.payload_from_children(children) {
          Some(NodePayload::Bool(value)) => value
          _ => false
        }
        @node.CstNode::flag_async(value~, loc~, source_span~)
      },
      saw_async_prefix,
    )
  }
  let children = if final_kind is Function {
    prepare_decl_body(children, loc, source_span, is_declare)
  } else {
    children
  }
  final_builder(loc, source_span, children)
}
