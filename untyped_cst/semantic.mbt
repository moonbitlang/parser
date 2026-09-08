///|
fn constant_node(
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  loc : Location,
  source_span : (Int, Int),
  value : String,
) -> CstNode {
  scalar_node(builder, loc, source_span, String(value))
}

///|
fn constant_node_from_syntax(
  constant : @syntax.Constant,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  match constant {
    Bool(value) =>
      scalar_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::Bool(value)) => value
            _ => false
          }
          @node.CstNode::constant_bool(value~, loc~, source_span~)
        },
        loc,
        source_span,
        Bool(value),
      )
    Byte(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_byte(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    Bytes(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_bytes(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    Char(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_char(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    Int(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_int(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    Int64(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_int64(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    UInt(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_uint(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    UInt64(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_uint64(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    Float(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_float(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    Double(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_double(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    String(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_string(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    Regex(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_regex(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
    BigInt(value) =>
      constant_node(
        fn(loc, source_span, children) {
          let value = match CstNode::payload_from_children(children) {
            Some(NodePayload::String(value)) => value
            _ => ""
          }
          @node.CstNode::constant_big_int(value~, loc~, source_span~)
        },
        loc,
        source_span,
        value,
      )
  }
}

///|
fn constant_node_from_token(
  token : Token,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode? {
  match token {
    TRUE =>
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::Bool(value)) => value
              _ => false
            }
            @node.CstNode::constant_bool(value~, loc~, source_span~)
          },
          loc,
          source_span,
          Bool(true),
        ),
      )
    FALSE =>
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::Bool(value)) => value
              _ => false
            }
            @node.CstNode::constant_bool(value~, loc~, source_span~)
          },
          loc,
          source_span,
          Bool(false),
        ),
      )
    BYTE(value) =>
      Some(
        constant_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::constant_byte(value~, loc~, source_span~)
          },
          loc,
          source_span,
          value,
        ),
      )
    BYTES(value) =>
      Some(
        constant_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::constant_bytes(value~, loc~, source_span~)
          },
          loc,
          source_span,
          value,
        ),
      )
    CHAR(value) =>
      Some(
        constant_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::constant_char(value~, loc~, source_span~)
          },
          loc,
          source_span,
          value,
        ),
      )
    INT(value) =>
      Some(constant_node_from_syntax(@syntax.make_int(value), loc, source_span))
    FLOAT(value) =>
      Some(
        constant_node_from_syntax(@syntax.make_float(value), loc, source_span),
      )
    DOUBLE(value) =>
      Some(
        constant_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::constant_double(value~, loc~, source_span~)
          },
          loc,
          source_span,
          value,
        ),
      )
    STRING(value) =>
      Some(
        constant_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::constant_string(value~, loc~, source_span~)
          },
          loc,
          source_span,
          value,
        ),
      )
    REGEX_LITERAL(value) =>
      Some(
        constant_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::constant_regex(value~, loc~, source_span~)
          },
          loc,
          source_span,
          value,
        ),
      )
    _ => None
  }
}

///|
fn lexical_name_node_from_token(
  token : Token,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode? {
  match token {
    LIDENT(value) | UIDENT(value) | DOT_LIDENT(value) | DOT_UIDENT(value) =>
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::name_identifier(value~, loc~, source_span~)
          },
          loc,
          source_span,
          String(value),
        ),
      )
    PACKAGE_NAME(value) =>
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::name_package(value~, loc~, source_span~)
          },
          loc,
          source_span,
          String(value),
        ),
      )
    POST_LABEL(value) => {
      let trimmed_loc = loc.trim_last_char()
      let trimmed_span = mk_span(
        source_span.0,
        if source_span.1 > source_span.0 {
          source_span.1 - 1
        } else {
          source_span.1
        },
      )
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::name_identifier(value~, loc~, source_span~)
          },
          trimmed_loc,
          trimmed_span,
          String(value),
        ),
      )
    }
    POST_LABEL_TRANSPARENT(value) => {
      let trimmed_loc = loc.trim_last_char().trim_last_char()
      let trimmed_span = mk_span(
        source_span.0,
        if source_span.1 - source_span.0 >= 2 {
          source_span.1 - 2
        } else {
          source_span.0
        },
      )
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::name_identifier(value~, loc~, source_span~)
          },
          trimmed_loc,
          trimmed_span,
          String(value),
        ),
      )
    }
    EXTEND =>
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::name_identifier(value~, loc~, source_span~)
          },
          loc,
          source_span,
          String("extend"),
        ),
      )
    UNDERSCORE =>
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::name_identifier(value~, loc~, source_span~)
          },
          loc,
          source_span,
          String("_"),
        ),
      )
    _ => None
  }
}

///|
fn accessor_node_from_token_value(
  token : Token,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode? {
  match token {
    DOT_INT(index) =>
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::Int(value)) => value
              _ => 0
            }
            @node.CstNode::accessor_index(value~, loc~, source_span~)
          },
          loc,
          source_span,
          Int(index),
        ),
      )
    DOT_LIDENT("_") =>
      Some(
        node(
          fn(loc, source_span, children) {
            let _ = children
            @node.CstNode::accessor_newtype(loc~, source_span~)
          },
          loc,
          source_span,
          [],
        ),
      )
    DOT_LIDENT(value) | DOT_UIDENT(value) =>
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::accessor_label(value~, loc~, source_span~)
          },
          loc,
          source_span,
          String(value),
        ),
      )
    _ => None
  }
}

///|
fn raw_token_semantic_children(
  token : Token,
  loc : Location,
  source_span : (Int, Int),
) -> Array[(String?, CstNode)] {
  let children : Array[(String?, CstNode)] = []
  if constant_node_from_token(token, loc, source_span) is Some(constant) {
    push_child(children, Some("constant"), constant)
  }
  if lexical_name_node_from_token(token, loc, source_span) is Some(name) {
    push_child(children, Some("name_value"), name)
  }
  if accessor_node_from_token_value(token, loc, source_span) is Some(accessor) {
    push_child(children, Some("accessor"), accessor)
  }
  match token {
    ASYNC =>
      push_child(
        children,
        Some("flag"),
        bool_marker_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::Bool(value)) => value
              _ => false
            }
            @node.CstNode::flag_async(value~, loc~, source_span~)
          },
          loc.start,
          source_span.0,
          true,
        ),
      )
    DECLARE =>
      push_child(
        children,
        Some("flag"),
        bool_marker_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::Bool(value)) => value
              _ => false
            }
            @node.CstNode::flag_declare(value~, loc~, source_span~)
          },
          loc.start,
          source_span.0,
          true,
        ),
      )
    LT_QUESTION =>
      push_child(
        children,
        Some("flag"),
        bool_marker_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::Bool(value)) => value
              _ => false
            }
            @node.CstNode::flag_conditional(value~, loc~, source_span~)
          },
          loc.start,
          source_span.0,
          true,
        ),
      )
    PLUS_EQUAL | AUGMENTED_ASSIGNMENT(_) =>
      push_child(
        children,
        Some("operator"),
        operator_node_from_token(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::operator_augmented(value~, loc~, source_span~)
          },
          token,
          loc,
          source_span,
        ),
      )
    INFIX1(_)
    | INFIX2(_)
    | INFIX3(_)
    | INFIX4(_)
    | PLUS
    | MINUS
    | EXCLAMATION
    | QUESTION
    | AMPER
    | AMPERAMPER
    | BAR
    | BARBAR
    | CARET
    | PIPE
    | PIPE_LEFT
    | IMPLIES
    | DOTDOT
    | RANGE_EXCLUSIVE
    | RANGE_INCLUSIVE
    | RANGE_LT_INCLUSIVE
    | RANGE_INCLUSIVE_REV
    | RANGE_EXCLUSIVE_REV
    | READONLY
    | EXTEND
    | EQUAL =>
      push_child(
        children,
        Some("operator"),
        operator_node_from_token(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::operator_infix(value~, loc~, source_span~)
          },
          token,
          loc,
          source_span,
        ),
      )
    COMMA =>
      push_child(
        children,
        Some("trailing"),
        marker_node(
          fn(loc, source_span, children) {
            let _ = children
            @node.CstNode::trailing_mark_comma(loc~, source_span~)
          },
          loc.start,
          source_span.0,
        ),
      )
    SEMI(true) =>
      push_child(
        children,
        Some("trailing"),
        marker_node(
          fn(loc, source_span, children) {
            let _ = children
            @node.CstNode::trailing_mark_semi(loc~, source_span~)
          },
          loc.start,
          source_span.0,
        ),
      )
    _ => ()
  }
  children
}

///|
fn operator_spelling(token : Token) -> String? {
  match token {
    INFIX1(value)
    | INFIX2(value)
    | INFIX3(value)
    | INFIX4(value)
    | AUGMENTED_ASSIGNMENT(value) => Some(value)
    PLUS | PLUS_EQUAL => Some("+")
    MINUS => Some("-")
    EXCLAMATION => Some("!")
    QUESTION => Some("?")
    AMPER => Some("&")
    AMPERAMPER => Some("&&")
    BAR => Some("|")
    BARBAR => Some("||")
    CARET => Some("^")
    PIPE => Some("|>")
    PIPE_LEFT => Some("<|")
    IMPLIES => Some("→")
    DOTDOT => Some("..")
    RANGE_EXCLUSIVE => Some("..<")
    RANGE_INCLUSIVE => Some("..=")
    RANGE_LT_INCLUSIVE => Some("..<=")
    RANGE_INCLUSIVE_REV => Some(">=..")
    RANGE_EXCLUSIVE_REV => Some(">..")
    READONLY => Some("readonly")
    EXTEND => Some("extend")
    EQUAL => Some("=")
    _ => None
  }
}

///|
fn operator_node_from_token(
  builder : (Location, (Int, Int), Array[(String?, CstNode)]) -> CstNode,
  token : Token,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  scalar_node(
    builder,
    loc,
    source_span,
    String(operator_spelling(token).unwrap_or("")),
  )
}

///|
fn signed_constant_node(constant : CstNode, negative : Bool) -> CstNode {
  if !negative || constant.kind is Constant_Bool {
    return constant
  }
  let value = constant.string_payload().unwrap_or("")
  scalar_node_from_kind(
    constant,
    constant.loc,
    constant.source_span,
    String(
      if value.has_prefix("-") {
        value.unsafe_substring(start=1, end=value.length())
      } else {
        "-\{value}"
      },
    ),
  )
}

///|
fn lex_literal_node_from_token(
  token : Token,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode? {
  match token {
    STRING(value) =>
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::lex_literal_string(value~, loc~, source_span~)
          },
          loc,
          source_span,
          String(value),
        ),
      )
    REGEX_LITERAL(value) =>
      Some(
        scalar_node(
          fn(loc, source_span, children) {
            let value = match CstNode::payload_from_children(children) {
              Some(NodePayload::String(value)) => value
              _ => ""
            }
            @node.CstNode::lex_literal_regex(value~, loc~, source_span~)
          },
          loc,
          source_span,
          String(value),
        ),
      )
    _ => None
  }
}

///|
fn constant_can_fold_prefix(constant : CstNode, operator : Token) -> Bool {
  match operator {
    PLUS => constant.kind is (Constant_Int | Constant_Double)
    MINUS => constant.kind is (Constant_Int | Constant_Int64 | Constant_Double)
    _ => false
  }
}

///|
fn semantic_ident_expr(
  name : String,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  let name_node = scalar_node(
    fn(loc, source_span, children) {
      let value = match CstNode::payload_from_children(children) {
        Some(NodePayload::String(value)) => value
        _ => ""
      }
      @node.CstNode::name_identifier(value~, loc~, source_span~)
    },
    loc,
    source_span,
    String(name),
  )
  node(
    fn(loc, source_span, children) {
      let async_loc = CstNode::first_child_with_label_from(
        children, "async_loc",
      )
      let name = CstNode::children_with_label_from(children, "name")
      let token = CstNode::first_child_with_label_from(children, "token")
      let semantic_name = CstNode::first_child_with_label_from(
        children, "semantic_name",
      )
      @node.CstNode::expr_ident(
        loc~,
        source_span~,
        async_loc?,
        name~,
        token?,
        semantic_name?,
      )
    },
    loc,
    source_span,
    [(Some("name"), name_node)],
  ).with_structured_name(
    "semantic_name",
    fn(loc, source_span, children) {
      let long_ident = CstNode::first_child_with_label_from(
        children, "long_ident",
      ).unwrap_or(CstNode::missing(loc~, source_span~))
      @node.CstNode::name_var(loc~, source_span~, long_ident~)
    },
    ["name"],
  )
}

///|
fn semantic_var_pattern(
  name : String,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  let name_node = scalar_node(
    fn(loc, source_span, children) {
      let value = match CstNode::payload_from_children(children) {
        Some(NodePayload::String(value)) => value
        _ => ""
      }
      @node.CstNode::name_identifier(value~, loc~, source_span~)
    },
    loc,
    source_span,
    String(name),
  )
  node(
    fn(loc, source_span, children) {
      let name = CstNode::first_child_with_label_from(children, "name").unwrap_or(
        CstNode::missing(loc~, source_span~),
      )
      let semantic_name = CstNode::first_child_with_label_from(
        children, "semantic_name",
      )
      let lex_scan_case_pattern = CstNode::first_child_with_label_from(
        children, "lex_scan_case_pattern",
      )
      @node.CstNode::pattern_var(
        loc~,
        source_span~,
        name~,
        semantic_name?,
        lex_scan_case_pattern?,
      )
    },
    loc,
    source_span,
    [(Some("name"), name_node)],
  ).with_structured_name(
    "semantic_name",
    fn(loc, source_span, children) {
      let long_ident = CstNode::first_child_with_label_from(
        children, "long_ident",
      ).unwrap_or(CstNode::missing(loc~, source_span~))
      @node.CstNode::name_binder(loc~, source_span~, long_ident~)
    },
    ["name"],
  )
}

///|
fn semantic_ident_expr_from_name(source : CstNode) -> CstNode? {
  source
  .lexical_name_value()
  .map(name => {
    semantic_ident_expr(
      name.string_payload().unwrap_or(""),
      name.loc,
      name.source_span,
    )
  })
}

///|
fn semantic_var_pattern_from_name(source : CstNode) -> CstNode? {
  source
  .lexical_name_value()
  .map(name => {
    semantic_var_pattern(
      name.string_payload().unwrap_or(""),
      name.loc,
      name.source_span,
    )
  })
}

///|
fn CstNode::with_pun_expr(self : CstNode) -> CstNode {
  if self.first_child_with_label("expr") is Some(_) {
    return self
  }
  guard self.first_child_with_label("name") is Some(name) &&
    semantic_ident_expr_from_name(name) is Some(expr) else {
    return self
  }
  let children : Array[(String?, CstNode)] = []
  for entry in self.children {
    children.push(entry)
  }
  push_child(children, Some("expr"), expr)
  self.internal_rebuild(loc=self.loc, source_span=self.source_span, children~)
}

///|
fn CstNode::with_pun_pattern(self : CstNode) -> CstNode {
  if self.first_child_with_label("pattern") is Some(_) {
    return self
  }
  guard self.first_child_with_label("name") is Some(name) &&
    semantic_var_pattern_from_name(name) is Some(pattern) else {
    return self
  }
  let children : Array[(String?, CstNode)] = []
  for entry in self.children {
    children.push(entry)
  }
  push_child(children, Some("pattern"), pattern)
  self.internal_rebuild(loc=self.loc, source_span=self.source_span, children~)
}

///|
fn CstNode::with_pun_locations(self : CstNode) -> CstNode {
  guard self.first_child_with_label("name") is Some(name) &&
    name.lexical_name_value() is Some(value) else {
    return self
  }
  let label_loc = if name.kind.is_syntax_token() {
    token_location_node(name, name.loc, name.source_span)
  } else {
    @node.CstNode::syntax_delimiter(loc=name.loc, source_span=name.source_span)
  }
  let with_label_loc = if self.first_child_with_label("label_loc") is None {
    self.with_location_metadata("label_loc", label_loc)
  } else {
    self
  }
  if with_label_loc.first_child_with_label("value_loc") is None {
    let value_loc = if name.kind.is_syntax_token() {
      token_location_node(name, value.loc, value.source_span)
    } else {
      @node.CstNode::syntax_delimiter(
        loc=value.loc,
        source_span=value.source_span,
      )
    }
    with_label_loc.with_location_metadata("value_loc", value_loc)
  } else {
    with_label_loc
  }
}

///|
fn folded_prefix_constant_node(
  op : CstNode,
  operator : CstNode,
  operand : CstNode,
  constant : CstNode,
  negative : Bool,
) -> CstNode {
  let loc = op.loc.merge(operand.loc)
  let source_span = mk_span(op.source_span.0, operand.source_span.1)
  let semantic_loc = if negative { loc } else { constant.loc }
  let semantic_span = if negative { source_span } else { constant.source_span }
  let signed = signed_constant_node(constant, negative)
  let semantic_constant = scalar_node_from_kind(
    signed,
    semantic_loc,
    semantic_span,
    String(signed.string_payload().unwrap_or("")),
  )
  let children : Array[(String?, CstNode)] = [
    (Some("op"), op),
    (Some("operator"), operator),
  ]
  for entry in operand.children {
    if !(entry.0 is Some("semantic_constant")) {
      children.push(entry)
    }
  }
  push_child(children, Some("semantic_constant"), semantic_constant)
  node(
    fn(loc, source_span, children) {
      let entries = recovery_children(children)
      @node.CstNode::expr_constant(loc~, source_span~, entries~)
    },
    loc,
    source_span,
    children,
  )
}

///|
fn unary_plus_apply_node(
  op : CstNode,
  operator : CstNode,
  operand : CstNode,
) -> CstNode {
  let loc = op.loc.merge(operand.loc)
  let source_span = mk_span(op.source_span.0, operand.source_span.1)
  let func = semantic_ident_expr("~+", loc, source_span)
  let argument = node(
    fn(loc, source_span, children) {
      let expr = CstNode::first_child_with_label_from(children, "expr").unwrap_or(
        CstNode::missing(loc~, source_span~),
      )
      @node.CstNode::argument_positional(loc~, source_span~, expr~)
    },
    operand.loc,
    operand.source_span,
    [(Some("expr"), operand)],
  )
  let arguments = node(
    fn(loc, source_span, children) {
      let entries = list_children(children)
      @node.CstNode::list_argument_list(loc~, source_span~, entries~)
    },
    operand.loc,
    operand.source_span,
    [(Some("item"), argument)],
  )
  node(
    fn(loc, source_span, children) {
      let op = CstNode::first_child_with_label_from(children, "op")
      let operator = CstNode::first_child_with_label_from(children, "operator")
      let func = CstNode::first_child_with_label_from(children, "func").unwrap_or(
        CstNode::missing(loc~, source_span~),
      )
      let arg = CstNode::first_child_with_label_from(children, "arg").unwrap_or(
        CstNode::missing(loc~, source_span~),
      )
      @node.CstNode::expr_apply(loc~, source_span~, op?, operator?, func~, arg~)
    },
    loc,
    source_span,
    [
      (Some("op"), op),
      (Some("operator"), operator),
      (Some("func"), func),
      (Some("arg"), arguments),
    ],
  )
}

///|
fn token_location_node(
  source : CstNode,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  source.internal_rebuild(loc~, source_span~, children=[])
}

///|
fn aggregate_span_node(loc : Location, source_span : (Int, Int)) -> CstNode {
  node(
    fn(loc, source_span, children) {
      let _ = children
      @node.CstNode::aggregate_span(loc~, source_span~)
    },
    loc,
    source_span,
    [],
  )
}

///|
fn CstNode::with_location_metadata(
  self : CstNode,
  label : String,
  metadata : CstNode,
) -> CstNode {
  let children : Array[(String?, CstNode)] = []
  for child in self.children {
    children.push(child)
  }
  push_child(children, Some(label), metadata)
  self.internal_rebuild(loc=self.loc, source_span=self.source_span, children~)
}

///|
fn CstNode::with_token_location(
  self : CstNode,
  label : String,
  source : CstNode,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  self.with_location_metadata(
    label,
    token_location_node(source, loc, source_span),
  )
}

///|
fn CstNode::with_aggregate_span(
  self : CstNode,
  label : String,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  self.with_location_metadata(label, aggregate_span_node(loc, source_span))
}

///|
fn push_direct_operator(
  children : Array[(String?, CstNode)],
  token : CstNode,
) -> Unit {
  if token.first_operator_descendant() is Some(operator) {
    push_child(children, Some("operator"), operator)
  }
}

///|
fn CstNode::with_async_location_anchor(self : CstNode) -> CstNode {
  if self.bool_marker_value(fn(kind) { kind is Flag_Async }) is Some(false) ||
    self.first_child_with_label("async_loc") is Some(_) {
    return self
  }
  for keyword in self.children_with_label("kw") {
    if keyword.has_child_kind(fn(kind) { kind is Flag_Async }) {
      return self.with_token_location(
        "async_loc",
        keyword,
        keyword.loc,
        keyword.source_span,
      )
    }
  }
  self
}

///|
fn CstNode::with_decl_like_function_location_anchor(self : CstNode) -> CstNode {
  if self.first_child_with_label("header_loc") is Some(_) {
    return self
  }
  match self.children_with_label("kw").get(0) {
    Some(keyword) =>
      self.with_aggregate_span(
        "header_loc",
        Location::{ start: keyword.loc.start, end: self.loc.end, },
        mk_span(keyword.source_span.0, self.source_span.1),
      )
    None => self
  }
}
