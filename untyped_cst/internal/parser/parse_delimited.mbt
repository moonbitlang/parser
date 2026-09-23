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
      self.report_expected_tokens_at(RPAREN, bad.loc, [TK_LBRACE])
    } else {
      self.reports.push({ loc: bad.loc, msg: "Unexpected token here.", })
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
  list_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  sep_kind? : TokenKind = TokenKind::TK_COMMA,
  follow_set? : Array[TokenKind] = [],
  invalid_delims? : Array[TokenKind] = [],
  require_nonempty? : Bool = false,
  nonempty_expected? : String = "",
  item_expected? : String = "",
  post_item_expected? : Array[TokenKind] = [],
  report_post_item_after_error? : Bool = false,
  is_block_statement_list? : Bool = false,
  reports_else_recovery? : Bool = false,
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
    if is_block_statement_list && state.abort_current_decl {
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
    } else if state.current_is(TK_EOF) ||
      state.current_in(follow_set) ||
      state.peek() is None {
      if expect_item && is_block_statement_list {
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
            state.current_is(TK_EOF) ||
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
          (!is_block_statement_list || latest_report_is_at_current) &&
          !state.current_is(sep_kind) &&
          !state.current_is(close_kind) &&
          !state.current_is(TK_EOF) &&
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
              state.current_is(TK_EOF) ||
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
          !state.current_is(TK_EOF) &&
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
          state.current_is(TK_EOF) ||
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
      if reports_else_recovery && recovery_token is Some(ELSE) {
        report_next_top_level_recovery_point(state)
      } else if is_block_statement_list &&
        recovery_token is Some(EXCLAMATION) &&
        recovery_crosses_argument_boundary(state) {
        report_next_top_level_recovery_point(state)
      }
    }
  }
  state.construct_from_children(list_builder, children)
}
