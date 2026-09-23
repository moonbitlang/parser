///|
fn parse_array_pattern_list(state : ParserState) -> CstNode {
  let rest_offenders : Array[(Token, Location)] = []
  let list = parse_delimited(
    state,
    TK_LBRACKET,
    TK_RBRACKET,
    fn(state) { parse_array_pattern_item(state, rest_offenders) },
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_array_pattern_list(loc~, source_span~, entries~)
    },
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
                TK_COMMA,
                TK_RBRACKET,
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
      fn(loc, source_span, children) {
        let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::array_patterns_open(loc~, source_span~, body~)
      }
    } else {
      fn(loc, source_span, children) {
        let body = @construction.first_child_with_label_from(children, "body").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::array_patterns_closed(loc~, source_span~, body~)
      }
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
    _ => parse_pattern_until(state, [TK_COMMA, TK_RBRACKET, TK_EOF])
  }
  match item.kind {
    Pattern_Rest if item.first_child_with_label("str") is Some(_) =>
      node_like(
        fn(loc, source_span, children) {
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let str = @construction.first_child_with_label_from(children, "str").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let dot_dot_binder = @construction.first_child_with_label_from(
            children, "dot_dot_binder",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::array_pattern_string_spread(
            loc~,
            source_span~,
            op~,
            str~,
            dot_dot_binder~,
          )
        },
        item,
      )
    Pattern_Rest if item.first_child_with_label("bytes") is Some(_) =>
      node_like(
        fn(loc, source_span, children) {
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let bytes = @construction.first_child_with_label_from(
            children, "bytes",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let dot_dot_binder = @construction.first_child_with_label_from(
            children, "dot_dot_binder",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::array_pattern_bytes_spread(
            loc~,
            source_span~,
            op~,
            bytes~,
            dot_dot_binder~,
          )
        },
        item,
      )
    Pattern_Rest if item.first_child_with_label("const") is Some(_) =>
      node_like(
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
            ["const"],
          )

          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let pkg = @construction.first_child_with_label_from(children, "pkg")
          let const_ = @construction.first_child_with_label_from(
            children, "const",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let dot_dot_binder = @construction.first_child_with_label_from(
            children, "dot_dot_binder",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          let semantic_name = @construction.first_child_with_label_from(
            children, "semantic_name",
          )
          @node.CstNode::array_pattern_const_spread(
            loc~,
            source_span~,
            op~,
            pkg?,
            const_~,
            dot_dot_binder~,
            semantic_name?,
          )
        },
        item,
      )
    Pattern_Rest => item
    _ =>
      node(
        fn(loc, source_span, children) {
          let pattern = @construction.first_child_with_label_from(
            children, "pattern",
          ).unwrap_or(missing_at_end(loc~, source_span~))
          @node.CstNode::array_pattern_pattern(loc~, source_span~, pattern~)
        },
        item.loc,
        item.source_span,
        [(Some("pattern"), item)],
      )
  }
}

///|
fn map_pattern_key_tokens() -> Array[TokenKind] {
  [
    TK_TRUE,
    TK_FALSE,
    TK_CHAR,
    TK_INT,
    TK_FLOAT,
    TK_DOUBLE,
    TK_MINUS,
    TK_STRING,
    TK_REGEX_LITERAL,
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
              [TokenKind::TK_LIDENT, TK_EXTEND, ..map_pattern_key_tokens()]
            } else if map_kind {
              map_pattern_key_tokens()
            } else {
              [TK_LIDENT, TK_EXTEND]
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
  let mut binder_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let _ = children
    @node.CstNode::dot_dot_binder_none(loc~, source_span~)
  }
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
      binder_builder = fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::dot_dot_binder_binder_as(loc~, source_span~)
      }
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
      binder_builder = fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::dot_dot_binder_underscore(loc~, source_span~)
      }
      if state.consume_any() is Some(name) {
        offender = Some((UNDERSCORE, name.loc))
        push_child(children, Some("name"), name)
      } else {
        ()
      }
    }
    Some((LIDENT(value), _, _)) => {
      binder_builder = fn(loc, source_span, children) {
        let _ = children
        @node.CstNode::dot_dot_binder_binder(loc~, source_span~)
      }
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
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_marker_child(
        children, loc, source_span, "dot_dot_binder", binder_builder,
      )

      let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let as_ = @construction.first_child_with_label_from(children, "as")
      let bytes = @construction.first_child_with_label_from(children, "bytes")
      let pkg = @construction.first_child_with_label_from(children, "pkg")
      let str = @construction.first_child_with_label_from(children, "str")
      let name = @construction.first_child_with_label_from(children, "name")
      let const_ = @construction.first_child_with_label_from(children, "const")
      let dot_dot_binder = @construction.first_child_with_label_from(
        children, "dot_dot_binder",
      )
      @node.CstNode::pattern_rest(
        loc~,
        source_span~,
        op~,
        as_?,
        bytes?,
        pkg?,
        str?,
        name?,
        const_?,
        dot_dot_binder?,
      )
    },
    children,
  )
}

///|
fn parse_record_pattern_list(state : ParserState) -> CstNode {
  let item_starts : Array[(Token, Location)?] = []
  let list = parse_delimited(
    state,
    TK_LBRACE,
    TK_RBRACE,
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
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_record_pattern_field_list(
        loc~,
        source_span~,
        entries~,
      )
    },
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
  let mut field_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let entries = recovery_children(children)
    @node.CstNode::pattern_missing_pattern(loc~, source_span~, entries~)
  }
  match state.peek() {
    Some((DOTDOT, _, _)) => {
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      return state.construct_from_children(
        fn(loc, source_span, children) {
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let as_ = @construction.first_child_with_label_from(children, "as")
          let bytes = @construction.first_child_with_label_from(
            children, "bytes",
          )
          let pkg = @construction.first_child_with_label_from(children, "pkg")
          let str = @construction.first_child_with_label_from(children, "str")
          let name = @construction.first_child_with_label_from(children, "name")
          let const_ = @construction.first_child_with_label_from(
            children, "const",
          )
          let dot_dot_binder = @construction.first_child_with_label_from(
            children, "dot_dot_binder",
          )
          @node.CstNode::pattern_rest(
            loc~,
            source_span~,
            op~,
            as_?,
            bytes?,
            pkg?,
            str?,
            name?,
            const_?,
            dot_dot_binder?,
          )
        },
        children,
      )
    }
    Some((LIDENT(_), _, _)) | Some((EXTEND, _, _)) => {
      field_builder = fn(loc, source_span, children) {
        let is_pun = children
          .iter()
          .all(fn(entry) { !(entry.0 is Some("pattern")) })
        {
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
        }
        if is_pun {
          push_pun_pattern(children)
        } else {
          ()
        }

        let entries = recovery_children(children)
        @node.CstNode::field_pattern(loc~, source_span~, entries~)
      }
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
  if state.current_is(TK_COLON) {
    if state.consume_any() is Some(colon) {
      push_child(children, Some("op"), colon)
    } else {
      ()
    }
    let pat = parse_pattern_until(state, [TK_COMMA, TK_RBRACE, TK_EOF])
    if !pat.children.is_empty() {
      push_child(children, Some("pattern"), pat)
    }
  } else if children.length() == 0 {
    let pat = parse_pattern_until(state, [TK_COMMA, TK_RBRACE, TK_EOF])
    if !pat.children.is_empty() {
      push_child(children, Some("pattern"), pat)
    }
  }
  state.construct_from_children(field_builder, children)
}

///|
fn parse_map_pattern_list(state : ParserState) -> CstNode {
  let item_starts : Array[(Token, Location)?] = []
  let list = parse_delimited(
    state,
    TK_LBRACE,
    TK_RBRACE,
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
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_map_pattern_field_list(loc~, source_span~, entries~)
    },
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
        TK_LIDENT,
        TK_EXTEND,
        TK_TRUE,
        TK_FALSE,
        TK_CHAR,
        TK_INT,
        TK_FLOAT,
        TK_DOUBLE,
        TK_MINUS,
        TK_STRING,
        TK_REGEX_LITERAL,
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
      return state.construct_from_children(
        fn(loc, source_span, children) {
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let as_ = @construction.first_child_with_label_from(children, "as")
          let bytes = @construction.first_child_with_label_from(
            children, "bytes",
          )
          let pkg = @construction.first_child_with_label_from(children, "pkg")
          let str = @construction.first_child_with_label_from(children, "str")
          let name = @construction.first_child_with_label_from(children, "name")
          let const_ = @construction.first_child_with_label_from(
            children, "const",
          )
          let dot_dot_binder = @construction.first_child_with_label_from(
            children, "dot_dot_binder",
          )
          @node.CstNode::pattern_rest(
            loc~,
            source_span~,
            op~,
            as_?,
            bytes?,
            pkg?,
            str?,
            name?,
            const_?,
            dot_dot_binder?,
          )
        },
        children,
      )
    }
    Some((MINUS, _, _)) => {
      negative = true
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
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
          let scan = state.recover_until([TK_RBRACE])
          push_recovered_errors(children, scan.skipped)
          return state.construct_from_children(
            fn(loc, source_span, children) {
              push_direct_semantic_constant(children)

              push_bool_marker(
                children,
                loc,
                source_span,
                "match_absent",
                fn(loc, source_span, children) {
                  let value = match
                    @construction.payload_from_children(children) {
                    Some(NodePayload::Bool(value)) => value
                    _ => false
                  }
                  @node.CstNode::flag_match_absent(value~, loc~, source_span~)
                },
                false,
              )

              let entries = recovery_children(children)
              @node.CstNode::field_map_pattern(loc~, source_span~, entries~)
            },
            children,
          )
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
  if state.current_is(TK_QUESTION) {
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
  }
  let pat = parse_pattern_until(state, [TK_COMMA, TK_RBRACE, TK_EOF])
  if !pat.children.is_empty() {
    push_child(children, Some("pattern"), pat)
  }
  state.construct_from_children(
    fn(loc, source_span, children) {
      push_direct_semantic_constant(children)

      push_bool_marker(
        children,
        loc,
        source_span,
        "match_absent",
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::flag_match_absent(value~, loc~, source_span~)
        },
        children.iter().any(fn(entry) { entry.0 is Some("question") }),
      )

      let entries = recovery_children(children)
      @node.CstNode::field_map_pattern(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_pattern_postfix(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  allow_brace_pattern_start? : Bool = false,
) -> CstNode {
  let mut pat = parse_pattern_atom(
    state,
    stop_kinds,
    allow_brace_pattern_start~,
  )
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
        pat = state.construct_from_children(
          fn(loc, source_span, children) {
            let entries = recovery_children(children)
            @node.CstNode::pattern_var(loc~, source_span~, entries~)
          },
          children,
        )
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
        pat = state.construct_from_children(
          fn(loc, source_span, children) {
            let entries = recovery_children(children)
            @node.CstNode::pattern_constraint(loc~, source_span~, entries~)
          },
          children,
        )
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
  if state.current_is(TK_BAR) {
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
    lhs = state.construct_from_children(
      fn(loc, source_span, children) {
        let entries = recovery_children(children)
        @node.CstNode::pattern_or(loc~, source_span~, entries~)
      },
      children,
    )
  }
  lhs
}

///|
fn parse_pattern_range(
  state : ParserState,
  stop_kinds : Array[TokenKind],
  allow_brace_pattern_start? : Bool = false,
) -> CstNode {
  let mut lhs = parse_pattern_postfix(
    state,
    stop_kinds,
    allow_brace_pattern_start~,
  )
  match state.peek() {
    Some((RANGE_EXCLUSIVE, _, _)) | Some((RANGE_INCLUSIVE, _, _)) => {
      let range_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = if state.current_is(
          TokenKind::TK_RANGE_EXCLUSIVE,
        ) {
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::range_kind_exclusive(loc~, source_span~)
        }
      } else {
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::range_kind_inclusive(loc~, source_span~)
        }
      }
      let children : Array[(String?, CstNode)] = [(Some("lhs"), lhs)]
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
      let rhs = parse_pattern_postfix(
        state,
        stop_kinds,
        allow_brace_pattern_start~,
      )
      if !rhs.children.is_empty() {
        push_child(children, Some("rhs"), rhs)
      }
      lhs = state.construct_from_children(
        fn(loc, source_span, children) {
          push_marker_child(
            children, loc, source_span, "range_kind", range_builder,
          )

          let lhs = @construction.first_child_with_label_from(children, "lhs").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let rhs = @construction.first_child_with_label_from(children, "rhs").unwrap_or(
            missing_at_end(loc~, source_span~),
          )
          let range_kind = @construction.first_child_with_label_from(
            children, "range_kind",
          )
          @node.CstNode::pattern_range(
            loc~,
            source_span~,
            lhs~,
            op~,
            rhs~,
            range_kind?,
          )
        },
        children,
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
  while state.current_is(TK_AS) {
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
    lhs = state.construct_from_children(
      fn(loc, source_span, children) {
        let pattern = @construction.first_child_with_label_from(
          children, "pattern",
        ).unwrap_or(missing_at_end(loc~, source_span~))
        let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let name = @construction.first_child_with_label_from(children, "name").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::pattern_alias(loc~, source_span~, pattern~, op~, name~)
      },
      children,
    )
  }
  lhs
}
