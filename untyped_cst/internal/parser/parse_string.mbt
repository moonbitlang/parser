///|
fn loc_span(state : ParserState, loc : Location) -> (Int, Int) {
  (state.offset_for_position(loc.start), state.offset_for_position(loc.end))
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
        let builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = match
          tok {
          INTERP(_) =>
            fn(loc, source_span, children) {
              let token = @construction.first_child_with_label_from(
                children, "token",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              @node.CstNode::expr_interp(loc~, source_span~, token~)
            }
          BYTES_INTERP(_) =>
            fn(loc, source_span, children) {
              let token = @construction.first_child_with_label_from(
                children, "token",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              @node.CstNode::expr_bytes_interp(loc~, source_span~, token~)
            }
          _ =>
            fn(loc, source_span, children) {
              push_direct_semantic_constant(children)

              let entries = recovery_children(children)
              @node.CstNode::expr_constant(loc~, source_span~, entries~)
            }
        }
        state.construct_from_children(builder, children)
      } else {
        state.construct_from_children(
          fn(loc, source_span, children) {
            let entries = recovery_children(children)
            @node.CstNode::expr_missing_expr_from_entries(
              loc~,
              source_span~,
              entries~,
            )
          },
          [],
        )
      }
    None =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          let entries = recovery_children(children)
          @node.CstNode::expr_missing_expr_from_entries(
            loc~,
            source_span~,
            entries~,
          )
        },
        [],
      )
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
              let element = match tok {
                MULTILINE_STRING(value) =>
                  scalar_node(
                    fn(loc, source_span, children) {
                      let value = match
                        @construction.payload_from_children(children) {
                        Some(NodePayload::String(value)) => value
                        _ => ""
                      }
                      @node.CstNode::multiline_string_element_string(
                        value~,
                        loc~,
                        source_span~,
                      )
                    },
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
                    fn(loc, source_span, children) {
                      let segment = @construction.children_with_label_from(
                        children, "segment",
                      )
                      @node.CstNode::multiline_string_element_interp(
                        loc~,
                        source_span~,
                        segment~,
                      )
                    },
                    token_node.loc,
                    token_node.source_span,
                    segments,
                  )
                }
                _ =>
                  node(
                    fn(loc, source_span, children) {
                      let segment = @construction.children_with_label_from(
                        children, "segment",
                      )
                      @node.CstNode::multiline_string_element_interp(
                        loc~,
                        source_span~,
                        segment~,
                      )
                    },
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
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = multiline_children(children)
      @node.CstNode::expr_multiline_string(loc~, source_span~, entries~)
    },
    children,
  )
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
              fn(loc, source_span, children) {
                let _ = children
                @node.CstNode::lex_pattern_regex_interp(loc~, source_span~)
              },
              token_node.loc,
              token_node.source_span,
              children,
            )
          } else {
            state.construct_from_children(
              fn(loc, source_span, children) {
                let _ = children
                @node.CstNode::lex_pattern_regex_interp(loc~, source_span~)
              },
              [],
            )
          }
        _ =>
          state.construct_from_children(
            fn(loc, source_span, children) {
              let _ = children
              @node.CstNode::lex_pattern_regex_interp(loc~, source_span~)
            },
            [],
          )
      }
    _ =>
      state.construct_from_children(
        fn(loc, source_span, children) {
          let _ = children
          @node.CstNode::lex_pattern_regex_interp(loc~, source_span~)
        },
        [],
      )
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
        fn(loc, source_span, children) {
          let value = match @construction.payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::interp_segment_literal(value~, loc~, source_span~)
        },
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
    fn(loc, source_span, children) {
      let expr = @construction.first_child_with_label_from(children, "expr").unwrap_or(
        missing_at_end(loc~, source_span~),
      )
      let payload = @construction.first_child_with_label_from(
        children, "payload",
      ).unwrap_or(missing_at_end(loc~, source_span~))
      @node.CstNode::interp_segment_source(loc~, source_span~, expr~, payload~)
    },
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
    state.parse_interp_segment,
  )
  parse_expr_until(inner_state, [TK_EOF])
}
