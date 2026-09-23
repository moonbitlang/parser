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
      state.construct_from_children(
        fn(loc, source_span, children) {
          let error = @construction.children_with_label_from(children, "error")
          @node.CstNode::type_missing_type(loc~, source_span~, error~)
        },
        children,
      )
    }
    _ =>
      parse_type_arrow(state, [
        TK_RAISE,
        TK_NORAISE,
        TK_NOCANCEL,
        TK_LBRACE,
        TK_EQUAL,
        TK_SEMI,
        TK_EOF,
      ])
  }
}

///|
fn parse_effect_annotation_into(
  state : ParserState,
  children : Array[(String?, CstNode)],
) -> Unit {
  if state.current_is(TK_NOCANCEL) {
    if state.consume_any() is Some(nocancel_anchor) {
      push_child(children, Some("nocancel"), nocancel_anchor)
    }
    push_children(children, state.consume_trivia())
    if state.current_is(TK_PLUS) {
      if state.consume_any() is Some(op) {
        push_child(children, Some("effect_op"), op)
      }
      push_children(children, state.consume_trivia())
      if state.current_is(TK_RAISE) || state.current_is(TK_NORAISE) {
        push_child(children, Some("clause"), parse_error_annotation(state))
      } else {
        state.report_expected_tokens([TK_RAISE, TK_NORAISE])
      }
    }
  } else {
    push_child(children, Some("clause"), parse_error_annotation(state))
    push_children(children, state.consume_trivia())
    if state.current_is(TK_PLUS) {
      if state.consume_any() is Some(op) {
        push_child(children, Some("effect_op"), op)
      }
      push_children(children, state.consume_trivia())
      if state.current_is(TK_NOCANCEL) {
        if state.consume_any() is Some(nocancel_anchor) {
          push_child(children, Some("nocancel"), nocancel_anchor)
        }
      } else {
        state.report_expected_tokens([TK_NOCANCEL])
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
  state.construct_from_children(
    fn(loc, source_span, children) {
      let entries = declaration_children(children)
      @node.CstNode::clause_return_type(loc~, source_span~, entries~)
    },
    children,
  )
}

///|
fn parse_error_annotation(state : ParserState) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  let mut error_builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode = fn(
    loc,
    source_span,
    children,
  ) {
    let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
      missing_at_end(loc~, source_span~),
    )
    @node.CstNode::error_type_default(loc~, source_span~, op~)
  }
  match state.peek() {
    Some((RAISE, _, _)) =>
      if state.consume_any() is Some(op) {
        push_child(children, Some("op"), op)
      } else {
        ()
      }
    Some((NORAISE, _, _)) => {
      error_builder = fn(loc, source_span, children) {
        let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::error_type_noraise(loc~, source_span~, op~)
      }
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
        state.construct_from_children(
          fn(loc, source_span, children) {
            push_structured_name(
              children,
              "semantic_name",
              fn(loc, source_span, children) {
                let long_ident = @construction.first_child_with_label_from(
                  children, "long_ident",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                @node.CstNode::name_constr_id(loc~, source_span~, long_ident~)
              },
              ["name"],
            )

            let op = @construction.first_child_with_label_from(children, "op")
            let name = @construction.children_with_label_from(children, "name")
            let error = @construction.children_with_label_from(
              children, "error",
            )
            let question = @construction.first_child_with_label_from(
              children, "question",
            )
            let semantic_name = @construction.first_child_with_label_from(
              children, "semantic_name",
            )
            let object = @construction.first_child_with_label_from(
              children, "object",
            )
            @node.CstNode::type_name(
              loc~,
              source_span~,
              op?,
              name~,
              error~,
              question?,
              semantic_name?,
              object?,
            )
          },
          type_children,
        ),
      )
      error_builder = fn(loc, source_span, children) {
        let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let type_ = @construction.first_child_with_label_from(children, "type").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::error_type_error(loc~, source_span~, op~, type_~)
      }
    }
    (Some((UIDENT(_), _, _)), Some((LBRACKET, start, end))) => {
      let type_children : Array[(String?, CstNode)] = []
      if state.consume_any() is Some(name) {
        push_child(type_children, Some("name"), name)
      } else {
        ()
      }
      state.report_expected_tokens_at(LBRACKET, mk_loc(start, end), [TK_LBRACE])
      while !state.current_is(TK_LBRACE) &&
            !state.current_is(TK_EOF) &&
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
        state.construct_from_children(
          fn(loc, source_span, children) {
            push_structured_name(
              children,
              "semantic_name",
              fn(loc, source_span, children) {
                let long_ident = @construction.first_child_with_label_from(
                  children, "long_ident",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                @node.CstNode::name_constr_id(loc~, source_span~, long_ident~)
              },
              ["name"],
            )

            let op = @construction.first_child_with_label_from(children, "op")
            let name = @construction.children_with_label_from(children, "name")
            let error = @construction.children_with_label_from(
              children, "error",
            )
            let question = @construction.first_child_with_label_from(
              children, "question",
            )
            let semantic_name = @construction.first_child_with_label_from(
              children, "semantic_name",
            )
            let object = @construction.first_child_with_label_from(
              children, "object",
            )
            @node.CstNode::type_name(
              loc~,
              source_span~,
              op?,
              name~,
              error~,
              question?,
              semantic_name?,
              object?,
            )
          },
          type_children,
        ),
      )
      error_builder = fn(loc, source_span, children) {
        let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let type_ = @construction.first_child_with_label_from(children, "type").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::error_type_error(loc~, source_span~, op~, type_~)
      }
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
        state.construct_from_children(
          fn(loc, source_span, children) {
            push_structured_name(
              children,
              "semantic_name",
              fn(loc, source_span, children) {
                let long_ident = @construction.first_child_with_label_from(
                  children, "long_ident",
                ).unwrap_or(missing_at_end(loc~, source_span~))
                @node.CstNode::name_constr_id(loc~, source_span~, long_ident~)
              },
              ["name"],
            )

            let op = @construction.first_child_with_label_from(children, "op")
            let name = @construction.children_with_label_from(children, "name")
            let error = @construction.children_with_label_from(
              children, "error",
            )
            let question = @construction.first_child_with_label_from(
              children, "question",
            )
            let semantic_name = @construction.first_child_with_label_from(
              children, "semantic_name",
            )
            let object = @construction.first_child_with_label_from(
              children, "object",
            )
            @node.CstNode::type_name(
              loc~,
              source_span~,
              op?,
              name~,
              error~,
              question?,
              semantic_name?,
              object?,
            )
          },
          type_children,
        ),
      )
      error_builder = fn(loc, source_span, children) {
        let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let type_ = @construction.first_child_with_label_from(children, "type").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::error_type_error(loc~, source_span~, op~, type_~)
      }
    }
    (Some((AMPER, start, end)), _) => {
      state.report_expected_tokens_at(AMPER, mk_loc(start, end), [TK_LBRACE])
      let type_children : Array[(String?, CstNode)] = []
      while !state.current_is(TK_LBRACE) &&
            !state.current_is(TK_EOF) &&
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
        state.construct_from_children(
          fn(loc, source_span, children) {
            let error = @construction.children_with_label_from(
              children, "error",
            )
            @node.CstNode::type_missing_type(loc~, source_span~, error~)
          },
          type_children,
        ),
      )
      error_builder = fn(loc, source_span, children) {
        let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        let type_ = @construction.first_child_with_label_from(children, "type").unwrap_or(
          missing_at_end(loc~, source_span~),
        )
        @node.CstNode::error_type_error(loc~, source_span~, op~, type_~)
      }
    }
    _ =>
      match state.peek() {
        Some((QUESTION, _, _)) =>
          if state.consume_any() is Some(question) {
            push_child(children, Some("type"), question)
            error_builder = fn(loc, source_span, children) {
              let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
                missing_at_end(loc~, source_span~),
              )
              let type_ = @construction.first_child_with_label_from(
                children, "type",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              @node.CstNode::error_type_maybe(loc~, source_span~, op~, type_~)
            }
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
            TK_PLUS,
            TK_NOCANCEL,
            TK_LBRACE,
            TK_RBRACE,
            TK_RPAREN,
            TK_RBRACKET,
            TK_COMMA,
            TK_EQUAL,
            TK_SEMI,
            TK_EOF,
          ])
          if !err_ty.children.is_empty() {
            match err_ty.kind {
              Type_Name | Type_Any => ()
              _ => state.report(err_ty.loc, "invalid error type")
            }
            push_child(children, Some("type"), err_ty)
            error_builder = fn(loc, source_span, children) {
              let op = @construction.first_child_with_label_from(children, "op").unwrap_or(
                missing_at_end(loc~, source_span~),
              )
              let type_ = @construction.first_child_with_label_from(
                children, "type",
              ).unwrap_or(missing_at_end(loc~, source_span~))
              @node.CstNode::error_type_error(loc~, source_span~, op~, type_~)
            }
          }
        }
        _ => ()
      }
  }
  state.construct_from_children(error_builder, children)
}
