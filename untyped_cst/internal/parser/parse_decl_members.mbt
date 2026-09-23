///|
fn parse_trait_member_item(state : ParserState) -> CstNode {
  let diagnostics_before = state.reports.length()
  let children : Array[(String?, CstNode)] = []
  let mut saw_params = false
  while state.current_is(TK_ATTRIBUTE) {
    if state.consume_any() is Some(attr) {
      push_child(children, Some("attrs"), attr)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  if state.current_is(TK_ASYNC) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  if state.current_is(TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    if state.current_is(TK_LBRACKET) {
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
      state.report_expected_tokens([TK_LIDENT, TK_EXTEND])
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TK_LIDENT),
      )
    }
  }
  if state.reports.length() == diagnostics_before && state.current_is(TK_LPAREN) {
    push_child(children, Some("params"), parse_trait_method_param_list(state))
    saw_params = true
  }
  if state.reports.length() == diagnostics_before &&
    state.current_is(TK_THIN_ARROW) {
    let reports_before_return = state.reports.length()
    push_child(children, Some("clause"), parse_func_return_type(state))
    if state.reports.length() > reports_before_return &&
      state.current_is(TK_LPAREN) {
      state.report_expected_tokens([TK_SEMI, TK_RBRACE])
    }
  }
  if state.reports.length() == diagnostics_before &&
    (
      state.current_is(TK_RAISE) ||
      state.current_is(TK_NORAISE) ||
      state.current_is(TK_NOCANCEL)
    ) {
    push_child(children, Some("clause"), parse_func_return_type(state))
  }
  if state.reports.length() == diagnostics_before && state.current_is(TK_EQUAL) {
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
      _ => state.report_expected_tokens([TK_UNDERSCORE])
    }
  }
  if state.reports.length() == diagnostics_before && !saw_params {
    state.report_expected_tokens([TK_LPAREN])
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
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
        children
        .iter()
        .any(fn(entry) {
          entry.0 is Some("kw") &&
          entry.1.has_child_kind(fn(kind) { kind is Flag_Async })
        }),
      )

      let entries = declaration_children(children)
      @node.CstNode::trait_method_declaration(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_trait_method_param_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LPAREN,
    TK_RPAREN,
    parse_trait_method_param_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_trait_method_parameter_list(
        loc~,
        source_span~,
        entries~,
      )
    },
    sep_kind=TokenKind::TK_COMMA,
  )
}

///|
fn parse_trait_method_param_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut parameter_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let entries = recovery_children(children)
    @node.CstNode::parameter_discard_positional(loc~, source_span~, entries~)
  }
  let mut parameter_is_positional = false
  match state.peek() {
    Some((tok, _, _)) if tok is (LIDENT(_) | POST_LABEL(_) | EXTEND) =>
      match state.peek(offset=1) {
        Some((QUESTION, _, _)) | Some((COLON, _, _)) =>
          if state.consume_any() is Some(name) {
            push_child(children, Some("pattern"), name)
            parameter_builder = if tok is POST_LABEL(_) {
              parameter_is_positional = false
              fn(loc, source_span, children) {
                let entries = recovery_children(children)
                @node.CstNode::parameter_labelled(loc~, source_span~, entries~)
              }
            } else {
              parameter_is_positional = true
              fn(loc, source_span, children) {
                let entries = recovery_children(children)
                @node.CstNode::parameter_positional(
                  loc~,
                  source_span~,
                  entries~,
                )
              }
            }
          } else {
            ()
          }
        _ => ()
      }
    _ => ()
  }
  if state.current_is(TokenKind::TK_QUESTION) {
    if parameter_is_positional {
      parameter_builder = fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::parameter_question_optional(loc~, source_span~, entries~)
      }
      parameter_is_positional = false
    }
    if state.consume_any() is Some(question) {
      push_child(children, Some("op"), question)
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
      parse_type_arrow(state, [TK_COMMA, TK_RPAREN, TK_EOF])
    Some((COMMA | RPAREN | EOF, _, _)) => state.missing_type_node()
    Some((found, start, end)) => {
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TK_POST_LABEL,
        TK_PACKAGE_NAME,
        TK_UIDENT,
        TK_LPAREN,
        TK_UNDERSCORE,
        TK_AMPER,
        TK_ASYNC,
      ])
      let invalid : Array[(String?, CstNode)] = []
      let scan = state.recover_until([TK_COMMA, TK_RPAREN])
      push_recovered_errors(invalid, scan.skipped, wrap=false)
      state.construct_from_children(
        fn(loc, source_span, children) {
          let error = @construction.children_with_label_from(children, "error")
          @node.CstNode::type_missing_type(loc~, source_span~, error~)
        },
        invalid,
      )
    }
    None => state.missing_type_node()
  }
  push_child(
    children,
    Some("type"),
    state.required_type_node(ty, reports_before_type),
  )
  state.construct_from_children(parameter_builder, children)
}

///|
fn parse_enum_constructor_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  while state.current_is(TK_ATTRIBUTE) {
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
      state.report_expected_tokens_at(found, mk_loc(start, end), [TK_LIDENT])
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
      push_child(children, Some("name"), state.missing_token_node(TK_LIDENT))
  }
  if state.current_is(TK_LPAREN) {
    push_child(children, Some("params"), parse_constructor_param_list(state))
  }
  if state.current_is(TK_EQUAL) {
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
              fn(loc, source_span, children) {
                let value = match
                  @construction.payload_from_children(children) {
                  Some(NodePayload::String(value)) => value
                  _ => ""
                }
                @node.CstNode::constructor_tag_value(value~, loc~, source_span~)
              },
              tag.loc,
              tag.source_span,
              String(value),
            ),
          )
        } else {
          ()
        }
      _ => push_child(children, Some("tag"), state.missing_token_node(TK_INT))
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_structured_name(
        children,
        "semantic_name",
        fn(loc, source_span, children) {
          let long_ident = @construction.first_child_with_label_from(
            children, "long_ident",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::name_constr(loc~, source_span~, long_ident~)
        },
        ["name"],
      )

      let entries = recovery_children(children)
      @node.CstNode::clause_constructor_decl_from_entries(
        loc~,
        source_span~,
        entries~,
      )
    },
    children,
  )
}

///|
fn parse_constructor_param_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LPAREN,
    TK_RPAREN,
    parse_constructor_param_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_constructor_parameter_list(
        loc~,
        source_span~,
        entries~,
      )
    },
    sep_kind=TokenKind::TK_COMMA,
    post_item_expected=[TokenKind::TK_COMMA, TokenKind::TK_RPAREN],
    report_post_item_after_error=true,
  )
}

///|
fn parse_constructor_param_item(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.current_is(TK_MUTABLE) {
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
      return state.construct_from_children(
        fn(loc, source_span, children) {
          push_bool_marker(
            children,
            loc,
            source_span,
            "mutable",
            fn(loc, source_span, children) {
              let value = match @construction.payload_from_children(children) {
                Some(NodePayload::Bool(value)) => value
                _ => false
              }
              @node.CstNode::flag_mutable(value~, loc~, source_span~)
            },
            children.iter().any(fn(entry) { entry.0 is Some("mut") }),
          )

          let mut_ = @construction.first_child_with_label_from(children, "mut")
          let op = @construction.first_child_with_label_from(children, "op")
          let type_ = @construction.first_child_with_label_from(
            children, "type",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let mutable = @construction.first_child_with_label_from(
            children, "mutable",
          )
          @node.CstNode::parameter_constructor_positional(
            loc~,
            source_span~,
            mut_?,
            op?,
            type_~,
            mutable?,
          )
        },
        children,
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
  if state.current_is(TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
  }
  let reports_before_type = state.reports.length()
  let ty = parse_type_arrow(state, [TK_COMMA, TK_RPAREN, TK_EOF])
  push_child(
    children,
    Some("type"),
    state.required_type_node(ty, reports_before_type),
  )
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_bool_marker(
        children,
        loc,
        source_span,
        "mutable",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_mutable(value~, loc~, source_span~)
        },
        children.iter().any(fn(entry) { entry.0 is Some("mut") }),
      )
      (if children.iter().any(fn(entry) { entry.0 is Some("name") }) {
        fn(loc, source_span, children) {
          let mut_ = @construction.first_child_with_label_from(children, "mut")
          let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let type_ = @construction.first_child_with_label_from(
            children, "type",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let mutable = @construction.first_child_with_label_from(
            children, "mutable",
          )
          @node.CstNode::parameter_constructor_labelled(
            loc~,
            source_span~,
            mut_?,
            name~,
            op~,
            type_~,
            mutable?,
          )
        }
      } else {
        fn(loc, source_span, children) {
          let mut_ = @construction.first_child_with_label_from(children, "mut")
          let op = @construction.first_child_with_label_from(children, "op")
          let type_ = @construction.first_child_with_label_from(
            children, "type",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let mutable = @construction.first_child_with_label_from(
            children, "mutable",
          )
          @node.CstNode::parameter_constructor_positional(
            loc~,
            source_span~,
            mut_?,
            op?,
            type_~,
            mutable?,
          )
        }
      })(loc, source_span, children)
    },
    children,
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
        TK_LIDENT,
        TK_EXTEND,
      ])
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
        state.missing_token_node_with_expected(TK_LIDENT, [TK_LIDENT, TK_EXTEND]),
      )
  }
  if state.current_is(TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let reports_before_type = state.reports.length()
    let ty = parse_type_arrow(state, [TK_NEWLINE, TK_SEMI, TK_RBRACE, TK_EOF])
    push_child(
      children,
      Some("type"),
      state.required_type_node(ty, reports_before_type),
    )
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_structured_name(
        children,
        "semantic_name",
        fn(loc, source_span, children) {
          let long_ident = @construction.first_child_with_label_from(
            children, "long_ident",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::name_field(loc~, source_span~, long_ident~)
        },
        ["name"],
      )
      push_bool_marker(
        children,
        loc,
        source_span,
        "mutable",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_mutable(value~, loc~, source_span~)
        },
        children.iter().any(fn(entry) { entry.0 is Some("mut") }),
      )

      if @construction.first_child_with_label_from(children, "vis") is None {
        push_marker_child(children, loc, source_span, "vis", fn(
          loc,
          source_span,
          children,
        ) {
          let _ = children
          @node.CstNode::visibility_default(loc~, source_span~)
        })
      } else {
        ()
      }

      let entries = recovery_children(children)
      @node.CstNode::field_declaration(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_invalid_struct_constructor(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> CstNode {
  match state.peek() {
    Some((found, start, end)) =>
      state.report_expected_tokens_at(found, mk_loc(start, end), [
        TK_LIDENT,
        TK_EXTEND,
      ])
    None => ()
  }
  if state.current_is(TK_ASYNC) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
  }
  if state.current_is(TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
  }
  if state.current_is(TK_LBRACKET) {
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
  if state.current_is(TK_LPAREN) {
    push_child(children, Some("params"), parse_parameter_list(state))
  }
  if state.current_is(TK_THIN_ARROW) ||
    state.current_is(TK_RAISE) ||
    state.current_is(TK_NORAISE) ||
    state.current_is(TK_NOCANCEL) {
    push_child(children, Some("clause"), parse_func_return_type(state))
  }
  if state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
  } else {
    push_child(
      children,
      Some("body"),
      state.missing_token_node_with_expected(TK_LBRACE, [TK_LBRACE]),
    )
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
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
        children
        .iter()
        .any(fn(entry) {
          entry.0 is Some("kw") &&
          entry.1.has_child_kind(fn(kind) { kind is Flag_Async })
        }),
      )
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
        children
        .iter()
        .any(fn(entry) {
          entry.0 is Some("kw") &&
          entry.1.has_child_kind(fn(kind) { kind is Flag_Declare })
        }),
      )

      let entries = declaration_children(children)
      @node.CstNode::impl_function(loc~, source_span~, entries~)
    },
    children,
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
  if state.current_is(TK_FN) {
    if state.consume_any() is Some(kw) {
      push_child(children, Some("kw"), kw)
    } else {
      ()
    }
    if state.current_is(TK_LBRACKET) {
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
      state.report_expected_tokens([TK_LIDENT, TK_EXTEND])
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TK_LIDENT),
      )
    }
  }
  if state.reports.length() == diagnostics_before && state.current_is(TK_LPAREN) {
    push_child(children, Some("params"), parse_parameter_list(state))
    saw_params = true
  }
  if state.reports.length() == diagnostics_before &&
    (
      state.current_is(TK_THIN_ARROW) ||
      state.current_is(TK_RAISE) ||
      state.current_is(TK_NORAISE) ||
      state.current_is(TK_NOCANCEL)
    ) {
    parsed_return_clause = true
    push_child(children, Some("clause"), parse_func_return_type(state))
  }
  if (state.reports.length() == diagnostics_before || parsed_return_clause) &&
    state.current_is(TK_LBRACE) {
    push_child(children, Some("body"), parse_block_expr(state))
    saw_body = true
  }
  if state.reports.length() == diagnostics_before && state.current_is(TK_EQUAL) {
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
      _ => state.report_expected_tokens([TK_STRING])
    }
  }
  if state.reports.length() == diagnostics_before {
    if !saw_params {
      state.report_expected_tokens([TK_LPAREN])
    } else if !saw_body && !saw_stub {
      state.report_expected_tokens([TK_LBRACE])
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
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
        children
        .iter()
        .any(fn(entry) {
          entry.0 is Some("kw") &&
          entry.1.has_child_kind(fn(kind) { kind is Flag_Async })
        }),
      )
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
        children
        .iter()
        .any(fn(entry) {
          entry.0 is Some("kw") &&
          entry.1.has_child_kind(fn(kind) { kind is Flag_Declare })
        }),
      )

      let children = prepare_decl_body(children, loc, source_span, false)
      let entries = declaration_children(children)
      @node.CstNode::impl_function(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_using_name_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LBRACE,
    TK_RBRACE,
    parse_using_name_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_using_item_list(loc~, source_span~, entries~)
    },
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
        state.missing_token_node_with_expected(TK_LIDENT, [
          TK_LIDENT,
          TK_EXTEND,
          TK_UIDENT,
          TK_TYPE,
          TK_TRAIT,
        ]),
      )
  }
  if state.current_is(TK_AS) {
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
  let using_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = match
    name_kind {
    2 =>
      fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::using_kind_type(loc~, source_span~)
      }
    3 =>
      fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::using_kind_trait(loc~, source_span~)
      }
    _ =>
      fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::using_kind_value(loc~, source_span~)
      }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_structured_name(
        children,
        "semantic_name",
        fn(loc, source_span, children) {
          let long_ident = @construction.first_child_with_label_from(
            children, "long_ident",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::name_alias_target(loc~, source_span~, long_ident~)
        },
        ["target", "name"],
      )
      push_marker_child(children, loc, source_span, "using_kind", using_builder)

      let kind = @construction.first_child_with_label_from(children, "kind")
      let target = @construction.first_child_with_label_from(children, "target").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let kw = @construction.first_child_with_label_from(children, "kw")
      let name = @construction.first_child_with_label_from(children, "name")
      let semantic_name = @construction.first_child_with_label_from(
        children, "semantic_name",
      )
      let using_kind = @construction.first_child_with_label_from(
        children, "using_kind",
      )
      @node.CstNode::clause_using_item(
        loc~,
        source_span~,
        kind?,
        target~,
        kw?,
        name?,
        semantic_name?,
        using_kind?,
      )
    },
    children,
  )
}

///|
fn parse_deriving_list(state : ParserState) -> CstNode {
  parse_delimited(
    state,
    TK_LPAREN,
    TK_RPAREN,
    parse_deriving_item,
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_deriving_list(loc~, source_span~, entries~)
    },
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
    _ => push_child(children, Some("name"), state.missing_token_node(TK_UIDENT))
  }
  if state.current_is(TK_LPAREN) {
    push_child(children, Some("arg"), parse_argument_list(state))
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
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

      let name = @construction.children_with_label_from(children, "name")
      let arg = @construction.first_child_with_label_from(children, "arg")
      let semantic_name = @construction.first_child_with_label_from(
        children, "semantic_name",
      )
      @node.CstNode::clause_deriving_directive(
        loc~,
        source_span~,
        name~,
        arg?,
        semantic_name?,
      )
    },
    children,
  )
}
