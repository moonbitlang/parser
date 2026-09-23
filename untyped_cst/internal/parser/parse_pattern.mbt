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
        return state.construct_from_children(
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

            let error = @construction.children_with_label_from(
              children, "error",
            )
            let name = @construction.first_child_with_label_from(
              children, "name",
            )
            let op = @construction.first_child_with_label_from(children, "op")
            let expr = @construction.first_child_with_label_from(
              children, "expr",
            )
            let semantic_name = @construction.first_child_with_label_from(
              children, "semantic_name",
            )
            @node.CstNode::clause_with_binding(
              loc~,
              source_span~,
              error~,
              name?,
              op?,
              expr?,
              semantic_name?,
            )
          },
          children,
        )
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

      let error = @construction.children_with_label_from(children, "error")
      let name = @construction.first_child_with_label_from(children, "name")
      let op = @construction.first_child_with_label_from(children, "op")
      let expr = @construction.first_child_with_label_from(children, "expr")
      let semantic_name = @construction.first_child_with_label_from(
        children, "semantic_name",
      )
      @node.CstNode::clause_with_binding(
        loc~,
        source_span~,
        error~,
        name?,
        op?,
        expr?,
        semantic_name?,
      )
    },
    children,
  )
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
    state.construct_from_children(
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_with_binding_list(loc~, source_span~, entries~)
      },
      binding_children,
    ),
  )
  push_child(children, Some("close"), state.expect_token(TK_RPAREN))
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      if @construction.first_child_with_label_from(children, "pattern") is None {
        // Preserve the end-anchored fallback in the pattern's child slot.
        for i, (label, _) in children {
          if label == Some("op") {
            entries.insert(i, Pattern(missing_at_end(loc~, source_span~)))
            break
          }
        }
      }
      @node.CstNode::pattern_with_from_entries(loc~, source_span~, entries~)
    },
    children,
  )
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
  state.construct_from_children(
    fn(loc, source_span, children) {
      let type_ = @construction.first_child_with_label_from(children, "type").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let wildcard = @construction.first_child_with_label_from(
        children, "wildcard",
      ).unwrap_or(missing_at_end(loc~, source_span~))
      @node.CstNode::pattern_constr_any(
        loc~,
        source_span~,
        type_~,
        op~,
        wildcard~,
      )
    },
    children,
  )
}

///|
fn finish_constructor_pattern(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> CstNode {
  if state.current_is(TK_LPAREN) {
    push_child(children, Some("params"), parse_constructor_pattern_args(state))
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
          @node.CstNode::name_constructor(loc~, source_span~, long_ident~)
        },
        ["pkg", "type", "name", "field"],
      )
      push_constructor_extra_info(children, loc, source_span)

      let is_open = @construction.first_child_with_label_from(
          children, "params",
        )
        .map(list => list_has_item_kind(list, fn(kind) { kind is Pattern_Rest }))
        .unwrap_or(false)
      push_marker_child(
        children,
        loc,
        source_span,
        "openness",
        if is_open {
          fn(loc, source_span, children) {
            let _ = children
            @node.CstNode::openness_open(loc~, source_span~)
          }
        } else {
          fn(loc, source_span, children) {
            let _ = children
            @node.CstNode::openness_closed(loc~, source_span~)
          }
        },
      )

      let entries = recovery_children(children)
      @node.CstNode::pattern_constr(loc~, source_span~, entries~)
    },
    children,
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
  allow_brace_pattern_start? : Bool = false,
) -> CstNode {
  match state.peek() {
    Some((tok, _, _)) if kind_in(tok.kind(), stop_kinds) &&
      !(allow_brace_pattern_start && tok.kind() is TokenKind::TK_LBRACE) =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
        },
        [],
      )
    Some((COMMENT(_), _, _)) | Some((NEWLINE, _, _)) => {
      let children = state.consume_trivia()
      let pat = parse_pattern_atom(
        state,
        stop_kinds,
        allow_brace_pattern_start~,
      )
      if allow_brace_pattern_start && pat.kind is (Pattern_Map | Pattern_Record) {
        for entry in pat.children {
          children.push(entry)
        }
        let (_, first) = children[0]
        let loc = pat.loc
        let source_span = (first.source_span.0, pat.source_span.1)
        let entries = recovery_children(children)
        match pat.kind {
          Pattern_Record =>
            @node.CstNode::pattern_record_from_entries(
              loc~,
              source_span~,
              entries~,
            )
          Pattern_Map =>
            @node.CstNode::pattern_map_from_entries(
              loc~,
              source_span~,
              entries~,
            )
          _ => abort("expected a brace pattern")
        }
      } else {
        if !pat.children.is_empty() {
          push_child(children, Some("pattern"), pat)
        }
        state.construct_from_children(
          fn(loc, source_span, children) {
            let entries = recovery_children(children)
            @node.CstNode::pattern_constraint(loc~, source_span~, entries~)
          },
          children,
        )
      }
    }
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
    Some((AMPER, _, _))
    | Some((UIDENT(_), _, _))
    | Some((PACKAGE_NAME(_), _, _)) if pattern_starts_constr_any(state) =>
      parse_constr_any_pattern(state)
    Some((AMPER, _, _)) => parse_object_constructor_pattern(state)
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) => {
      let mut node = parse_single_token_pattern(state, fn(
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
      if state.current_is(TokenKind::TK_LPAREN) {
        let args = parse_constructor_pattern_args(state)
        node = state.construct_from_children(
          fn(loc, source_span, children) {
            let name = @construction.first_child_with_label_from(
              children, "name",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            let params = @construction.first_child_with_label_from(
              children, "params",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            @node.CstNode::pattern_special_constr(
              loc~,
              source_span~,
              name~,
              params~,
            )
          },
          [(Some("name"), node), (Some("params"), args)],
        )
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
      if state.current_is(TK_COLONCOLON) {
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
        state.construct_from_children(
          fn(loc, source_span, children) {
            if list.children_with_label("item").length() == 1 {
              push_child(
                children,
                Some("pattern_group"),
                @node.CstNode::pattern_group(
                  loc=empty_loc(loc.end),
                  source_span=empty_span(source_span.1),
                ),
              )
            }
            let body = @construction.first_child_with_label_from(
              children, "body",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            let pattern_group = @construction.first_child_with_label_from(
              children, "pattern_group",
            )
            @node.CstNode::pattern_tuple(
              loc~,
              source_span~,
              body~,
              pattern_group?,
            )
          },
          [(Some("body"), list)],
        )
      }
    Some((LBRACKET, _, _)) => {
      let list = parse_array_pattern_list(state)
      state.construct_from_children(
        fn(loc, source_span, children) {
          let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          @node.CstNode::pattern_array(loc~, source_span~, body~)
        },
        [(Some("body"), list)],
      )
    }
    Some((LBRACE, _, _)) =>
      if brace_starts_map_pattern(state) {
        let list = parse_map_pattern_list(state)
        state.construct_from_children(
          fn(loc, source_span, children) {
            push_marker_child(
              children,
              loc,
              source_span,
              "openness",
              if list_has_item_kind(list, fn(kind) { kind is Pattern_Rest }) {
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::openness_open(loc~, source_span~)
                }
              } else {
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::openness_closed(loc~, source_span~)
                }
              },
            )

            let trivia = @construction.first_child_with_label_from(
              children, "trivia",
            )
            let body = @construction.first_child_with_label_from(
              children, "body",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            let openness = @construction.first_child_with_label_from(
              children, "openness",
            )
            @node.CstNode::pattern_map(
              loc~,
              source_span~,
              trivia?,
              body~,
              openness?,
            )
          },
          [(Some("body"), list)],
        )
      } else {
        let list = parse_record_pattern_list(state)
        state.construct_from_children(
          fn(loc, source_span, children) {
            push_marker_child(
              children,
              loc,
              source_span,
              "openness",
              if list_has_item_kind(list, fn(kind) { kind is Pattern_Rest }) {
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::openness_open(loc~, source_span~)
                }
              } else {
                fn(loc, source_span, children) {
                  let _ = children
                  @node.CstNode::openness_closed(loc~, source_span~)
                }
              },
            )

            let body = @construction.first_child_with_label_from(
              children, "body",
            ).unwrap_or(missing_at_end(loc~, source_span~))
            let openness = @construction.first_child_with_label_from(
              children, "openness",
            )
            @node.CstNode::pattern_record(loc~, source_span~, body~, openness?)
          },
          [(Some("body"), list)],
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
      let scan = state.recover_until(stop_kinds)
      push_recovered_errors(children, scan.skipped)
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
        },
        children,
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
fn parse_single_token_pattern(
  state : ParserState,
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
) -> CstNode {
  if state.consume_any() is Some(token) {
    state.construct_from_children(builder, [(Some("name"), token)])
  } else {
    state.construct_from_children(
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
      },
      [],
    )
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
          fn(loc, source_span, children) {
            let value = match @construction.payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::operator_unary_minus(value~, loc~, source_span~)
          },
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
        TK_INT,
        TK_FLOAT,
        TK_DOUBLE,
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
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_direct_semantic_constant(children)

      let op = @construction.first_child_with_label_from(children, "op")
      let operator = @construction.first_child_with_label_from(
        children, "operator",
      )
      let error = @construction.first_child_with_label_from(children, "error")
      let token = @construction.first_child_with_label_from(children, "token")
      let constant = @construction.first_child_with_label_from(
        children, "constant",
      )
      let semantic_constant = @construction.first_child_with_label_from(
        children, "semantic_constant",
      )
      @node.CstNode::pattern_constant(
        loc~,
        source_span~,
        op?,
        operator?,
        error?,
        token?,
        constant?,
        semantic_constant?,
      )
    },
    children,
  )
}

///|
fn parse_constructor_pattern_args(
  state : ParserState,
  tuple_pattern? : Bool = false,
) -> CstNode {
  let list = parse_delimited(
    state,
    TK_LPAREN,
    TK_RPAREN,
    parse_constructor_pattern_arg,
    if tuple_pattern {
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_tuple_pattern_list(loc~, source_span~, entries~)
      }
    } else {
      fn(loc, source_span, children) {
        let entries = list_children(children)
        @node.CstNode::list_constructor_pattern_argument_list(
          loc~,
          source_span~,
          entries~,
        )
      }
    },
    sep_kind=TK_COMMA,
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
        (Some("error"), item) if @construction.first_trailing_mark_descendant(
            item,
          )
          is Some(mark) &&
          mark.kind is TrailingMark_Semi =>
          saw_invalid_statement_separator = true
        (Some("close"), item) => close = Some(item)
        _ => ()
      }
    }
    if saw_invalid_statement_separator {
      if close is Some(item) {
        let expected = [TokenKind::TK_SEMI, TK_RBRACE]
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
    _ => parse_pattern_until(state, [TK_COMMA, TK_RPAREN, TK_EOF])
  }
}

///|
fn parse_labelled_pun_constructor_pattern_arg(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(name) {
    if state.current_is(TK_EQUAL) {
      state.report(
        name.loc,
        "unexpected `~` in argument. Did you mean `label=pattern` or `label~`?",
      )
    }
    push_child(children, Some("name"), name)
  } else {
    ()
  }
  let mut is_pun = true
  let mut argument_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
      missing_at_end(loc~, source_span~),
    )
    let semantic_name = @construction.first_child_with_label_from(
      children, "semantic_name",
    )
    let pun = @construction.first_child_with_label_from(children, "pun")
    let expr = @construction.first_child_with_label_from(children, "expr")
    let pattern = @construction.first_child_with_label_from(children, "pattern")
    @node.CstNode::argument_labelled_pun(
      loc~,
      source_span~,
      name~,
      semantic_name?,
      pun?,
      expr?,
      pattern?,
    )
  }
  if state.current_is(TokenKind::TK_EQUAL) {
    is_pun = false
    argument_builder = fn(loc, source_span, children) {
      let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let expr = @construction.first_child_with_label_from(children, "expr")
      let pattern = @construction.first_child_with_label_from(
        children, "pattern",
      )
      let semantic_name = @construction.first_child_with_label_from(
        children, "semantic_name",
      )
      let pun = @construction.first_child_with_label_from(children, "pun")
      @node.CstNode::argument_labelled(
        loc~,
        source_span~,
        name~,
        op~,
        expr?,
        pattern?,
        semantic_name?,
        pun?,
      )
    }
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
    let pat = parse_pattern_until(state, [TK_COMMA, TK_RPAREN, TK_EOF])
    if !pat.children.is_empty() {
      push_child(children, Some("pattern"), pat)
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
      push_bool_marker(
        children,
        loc,
        source_span,
        "pun",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_pun(value~, loc~, source_span~)
        },
        is_pun,
      )

      if is_pun {
        push_pun_pattern(children)
      } else {
        ()
      }

      argument_builder(loc, source_span, children)
    },
    children,
  )
}

///|
fn parse_labelled_constructor_pattern_arg(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  if state.consume_any() is Some(name) {
    push_child(children, Some("name"), name)
  } else {
    ()
  }
  if state.current_is(TK_EQUAL) {
    if state.consume_any() is Some(equal) {
      push_child(children, Some("op"), equal)
    } else {
      ()
    }
  }
  let pat = parse_pattern_until(state, [TK_COMMA, TK_RPAREN, TK_EOF])
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
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
      push_bool_marker(
        children,
        loc,
        source_span,
        "pun",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_pun(value~, loc~, source_span~)
        },
        false,
      )

      let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let expr = @construction.first_child_with_label_from(children, "expr")
      let pattern = @construction.first_child_with_label_from(
        children, "pattern",
      )
      let semantic_name = @construction.first_child_with_label_from(
        children, "semantic_name",
      )
      let pun = @construction.first_child_with_label_from(children, "pun")
      @node.CstNode::argument_labelled(
        loc~,
        source_span~,
        name~,
        op~,
        expr?,
        pattern?,
        semantic_name?,
        pun?,
      )
    },
    children,
  )
}
