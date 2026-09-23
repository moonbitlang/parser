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
  if state.current_is(TK_LPAREN) {
    let children : Array[(String?, CstNode)] = []
    if state.consume_any() is Some(open) {
      push_child(children, Some("open"), open)
    } else {
      ()
    }
    let _ = state.consume_trivia()
    let reports_before_pattern = state.reports.length()
    let pat = parse_regex_as_pattern(state, [TK_COMMA, TK_RPAREN, TK_EOF])
    push_child(children, Some("pattern"), pat)
    let _ = state.consume_trivia()
    if state.current_is(TK_COMMA) {
      push_child(
        children,
        Some("bindings"),
        parse_regex_match_binding_list(state),
      )
    }
    let _ = state.consume_trivia()
    if state.current_is(TK_RPAREN) {
      push_child(children, Some("close"), state.expect_token(TK_RPAREN))
    } else {
      if state.reports.length() == reports_before_pattern {
        state.report_expected_tokens([TK_RPAREN])
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
        state.missing_token_node_silent(TK_RPAREN),
      )
    }
    state.construct_from_children(
      fn(loc, source_span, children) {
        let open = @construction.first_child_with_label_from(children, "open").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let pattern = @construction.first_child_with_label_from(
          children, "pattern",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let bindings = @construction.first_child_with_label_from(
          children, "bindings",
        )
        let error = @construction.children_with_label_from(children, "error")
        let close = @construction.first_child_with_label_from(children, "close").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let lex_scan_case_pattern = @construction.first_child_with_label_from(
          children, "lex_scan_case_pattern",
        )
        @node.CstNode::regex_pattern_placeholder(
          loc~,
          source_span~,
          open~,
          pattern~,
          bindings?,
          error~,
          close~,
          lex_scan_case_pattern?,
        )
      },
      children,
    )
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
  if state.current_is(TK_AS) {
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
        state.report_expected_tokens([TK_LIDENT, TK_EXTEND])
        push_child(
          children,
          Some("name"),
          state.missing_token_node_silent(TK_LIDENT),
        )
      }
    }
    state.construct_from_children(
      fn(loc, source_span, children) {
        let pattern = @construction.first_child_with_label_from(
          children, "pattern",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let kw = @construction.first_child_with_label_from(children, "kw").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let lex_scan_case_pattern = @construction.first_child_with_label_from(
          children, "lex_scan_case_pattern",
        )
        @node.CstNode::regex_pattern_alias(
          loc~,
          source_span~,
          pattern~,
          kw~,
          name~,
          lex_scan_case_pattern?,
        )
      },
      children,
    )
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
  while state.current_is(TK_BAR) {
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
    lhs = state.construct_from_children(
      fn(loc, source_span, children) {
        let lhs = @construction.first_child_with_label_from(children, "lhs").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let rhs = @construction.first_child_with_label_from(children, "rhs").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::regex_pattern_alternation(
          loc~,
          source_span~,
          lhs~,
          op~,
          rhs~,
        )
      },
      children,
    )
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
  while state.current_is(TK_PLUS) {
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
    lhs = state.construct_from_children(
      fn(loc, source_span, children) {
        let lhs = @construction.first_child_with_label_from(children, "lhs").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let rhs = @construction.first_child_with_label_from(children, "rhs").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::regex_pattern_sequence(
          loc~,
          source_span~,
          lhs~,
          op~,
          rhs~,
        )
      },
      children,
    )
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
      let pat = parse_regex_as_pattern(state, [TK_RPAREN])
      push_child(children, Some("pattern"), pat)
      let _ = state.consume_trivia()
      if state.current_is(TK_RPAREN) {
        push_child(children, Some("close"), state.expect_token(TK_RPAREN))
      } else if state.reports.length() > reports_before_pattern {
        push_child(
          children,
          Some("close"),
          state.missing_token_node_silent(TK_RPAREN),
        )
      } else {
        push_child(children, Some("close"), state.expect_token(TK_RPAREN))
      }
      state.construct_from_children(
        fn(loc, source_span, children) {
          let open = @construction.first_child_with_label_from(children, "open").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let pattern = @construction.first_child_with_label_from(
            children, "pattern",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let bindings = @construction.first_child_with_label_from(
            children, "bindings",
          )
          let error = @construction.children_with_label_from(children, "error")
          let close = @construction.first_child_with_label_from(
            children, "close",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let lex_scan_case_pattern = @construction.first_child_with_label_from(
            children, "lex_scan_case_pattern",
          )
          @node.CstNode::regex_pattern_placeholder(
            loc~,
            source_span~,
            open~,
            pattern~,
            bindings?,
            error~,
            close~,
            lex_scan_case_pattern?,
          )
        },
        children,
      )
    }
    Some((raw, _, _)) if raw is REGEX_LITERAL(_) =>
      if state.consume_any() is Some(token) {
        let children : Array[(String?, CstNode)] = [(Some("token"), token)]
        if lex_literal_node_from_token(raw, token.loc, token.source_span)
          is Some(literal) {
          push_child(children, Some("lex_literal"), literal)
        }
        state.construct_from_children(
          fn(loc, source_span, children) {
            let error = @construction.first_child_with_label_from(
              children, "error",
            )
            let token = @construction.first_child_with_label_from(
              children, "token",
            )
            let lex_literal = @construction.first_child_with_label_from(
              children, "lex_literal",
            )
            let lex_scan_case_pattern = @construction.first_child_with_label_from(
              children, "lex_scan_case_pattern",
            )
            @node.CstNode::regex_pattern_literal(
              loc~,
              source_span~,
              error?,
              token?,
              lex_literal?,
              lex_scan_case_pattern?,
            )
          },
          children,
        )
      } else {
        state.construct_from_children(
          fn(loc, source_span, children) {
            let error = @construction.first_child_with_label_from(
              children, "error",
            )
            let token = @construction.first_child_with_label_from(
              children, "token",
            )
            let lex_literal = @construction.first_child_with_label_from(
              children, "lex_literal",
            )
            let lex_scan_case_pattern = @construction.first_child_with_label_from(
              children, "lex_scan_case_pattern",
            )
            @node.CstNode::regex_pattern_literal(
              loc~,
              source_span~,
              error?,
              token?,
              lex_literal?,
              lex_scan_case_pattern?,
            )
          },
          [],
        )
      }
    Some((raw, start, end)) if raw is STRING(_) => {
      state.report_expected_tokens_at(raw, mk_loc(start, end), [
        TK_REGEX_LITERAL,
      ])
      if state.consume_any() is Some(token) {
        let children : Array[(String?, CstNode)] = [(Some("token"), token)]
        if lex_literal_node_from_token(raw, token.loc, token.source_span)
          is Some(literal) {
          push_child(children, Some("lex_literal"), literal)
        }
        state.construct_from_children(
          fn(loc, source_span, children) {
            let error = @construction.first_child_with_label_from(
              children, "error",
            )
            let token = @construction.first_child_with_label_from(
              children, "token",
            )
            let lex_literal = @construction.first_child_with_label_from(
              children, "lex_literal",
            )
            let lex_scan_case_pattern = @construction.first_child_with_label_from(
              children, "lex_scan_case_pattern",
            )
            @node.CstNode::regex_pattern_literal(
              loc~,
              source_span~,
              error?,
              token?,
              lex_literal?,
              lex_scan_case_pattern?,
            )
          },
          children,
        )
      } else {
        state.construct_from_children(
          fn(loc, source_span, children) {
            let error = @construction.first_child_with_label_from(
              children, "error",
            )
            let token = @construction.first_child_with_label_from(
              children, "token",
            )
            let lex_literal = @construction.first_child_with_label_from(
              children, "lex_literal",
            )
            let lex_scan_case_pattern = @construction.first_child_with_label_from(
              children, "lex_scan_case_pattern",
            )
            @node.CstNode::regex_pattern_literal(
              loc~,
              source_span~,
              error?,
              token?,
              lex_literal?,
              lex_scan_case_pattern?,
            )
          },
          [],
        )
      }
    }
    Some((UIDENT(_), _, _)) => {
      if state.consume_any() is Some(name) {
        push_child(children, Some("name"), name)
      } else {
        ()
      }
      state.construct_from_children(
        fn(loc, source_span, children) {
          push_structured_long_ident(children, "semantic_name", ["name"])

          let name = @construction.children_with_label_from(children, "name")
          let semantic_name = @construction.first_child_with_label_from(
            children, "semantic_name",
          )
          let lex_scan_case_pattern = @construction.first_child_with_label_from(
            children, "lex_scan_case_pattern",
          )
          @node.CstNode::regex_pattern_reference(
            loc~,
            source_span~,
            name~,
            semantic_name?,
            lex_scan_case_pattern?,
          )
        },
        children,
      )
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
          state.report_expected_tokens([TK_DOT_UIDENT])
          push_child(
            children,
            Some("name"),
            state.missing_token_node_silent(TK_DOT_UIDENT),
          )
        }
      }
      state.construct_from_children(
        fn(loc, source_span, children) {
          push_structured_long_ident(children, "semantic_name", ["name"])

          let name = @construction.children_with_label_from(children, "name")
          let semantic_name = @construction.first_child_with_label_from(
            children, "semantic_name",
          )
          let lex_scan_case_pattern = @construction.first_child_with_label_from(
            children, "lex_scan_case_pattern",
          )
          @node.CstNode::regex_pattern_reference(
            loc~,
            source_span~,
            name~,
            semantic_name?,
            lex_scan_case_pattern?,
          )
        },
        children,
      )
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
      state.construct_from_children(
        fn(loc, source_span, children) {
          let error = @construction.first_child_with_label_from(
            children, "error",
          )
          let token = @construction.first_child_with_label_from(
            children, "token",
          )
          let lex_literal = @construction.first_child_with_label_from(
            children, "lex_literal",
          )
          let lex_scan_case_pattern = @construction.first_child_with_label_from(
            children, "lex_scan_case_pattern",
          )
          @node.CstNode::regex_pattern_literal(
            loc~,
            source_span~,
            error?,
            token?,
            lex_literal?,
            lex_scan_case_pattern?,
          )
        },
        children,
      )
    }
    None => {
      state.report_expected_description("regex pattern")
      state.construct_from_children(
        fn(loc, source_span, children) {
          let error = @construction.first_child_with_label_from(
            children, "error",
          )
          let token = @construction.first_child_with_label_from(
            children, "token",
          )
          let lex_literal = @construction.first_child_with_label_from(
            children, "lex_literal",
          )
          let lex_scan_case_pattern = @construction.first_child_with_label_from(
            children, "lex_scan_case_pattern",
          )
          @node.CstNode::regex_pattern_literal(
            loc~,
            source_span~,
            error?,
            token?,
            lex_literal?,
            lex_scan_case_pattern?,
          )
        },
        [],
      )
    }
  }
}

///|
fn parse_regex_match_binding_list(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  while state.current_is(TK_COMMA) {
    if state.consume_any() is Some(sep) {
      push_child(children, Some("sep"), sep)
    } else {
      ()
    }
    push_children(children, state.consume_trivia())
    if state.current_is(TK_RPAREN) || state.current_is(TK_EOF) {
      break
    }
    push_child(children, Some("item"), parse_regex_match_binding(state))
    push_children(children, state.consume_trivia())
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_regex_binding_list(loc~, source_span~, entries~)
    },
    children,
  )
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
            state.report_expected_tokens([TK_LIDENT, TK_EXTEND])
            push_child(
              children,
              Some("binder"),
              state.missing_token_node_silent(TK_LIDENT),
            )
          }
        }
      } else {
        state.report_expected_tokens([TK_LIDENT, TK_POST_LABEL])
        push_child(
          children,
          Some("name"),
          state.missing_token_node_silent(TK_LIDENT),
        )
      }
    _ => {
      state.report_expected_tokens([TK_LIDENT, TK_POST_LABEL])
      push_child(
        children,
        Some("name"),
        state.missing_token_node_silent(TK_LIDENT),
      )
    }
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_structured_name(
        children,
        "semantic_label",
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
      push_structured_name(
        children,
        "semantic_binder",
        fn(loc, source_span, children) {
          let long_ident = @construction.first_child_with_label_from(
            children, "long_ident",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::name_binder(loc~, source_span~, long_ident~)
        },
        ["binder"],
      )

      let entries = recovery_children(children)
      @node.CstNode::binding_regex_from_entries(loc~, source_span~, entries~)
    },
    children,
  )
}
