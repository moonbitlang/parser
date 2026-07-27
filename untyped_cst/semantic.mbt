///|
fn constant_node(
  kind : NodeKind,
  loc : Location,
  source_span : (Int, Int),
  value : String,
) -> CstNode {
  scalar_node(kind, loc, source_span, String(value))
}

///|
fn constant_node_from_syntax(
  constant : @syntax.Constant,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  match constant {
    Bool(value) => scalar_node(Constant_Bool, loc, source_span, Bool(value))
    Byte(value) => constant_node(Constant_Byte, loc, source_span, value)
    Bytes(value) => constant_node(Constant_Bytes, loc, source_span, value)
    Char(value) => constant_node(Constant_Char, loc, source_span, value)
    Int(value) => constant_node(Constant_Int, loc, source_span, value)
    Int64(value) => constant_node(Constant_Int64, loc, source_span, value)
    UInt(value) => constant_node(Constant_UInt, loc, source_span, value)
    UInt64(value) => constant_node(Constant_UInt64, loc, source_span, value)
    Float(value) => constant_node(Constant_Float, loc, source_span, value)
    Double(value) => constant_node(Constant_Double, loc, source_span, value)
    String(value) => constant_node(Constant_String, loc, source_span, value)
    Regex(value) => constant_node(Constant_Regex, loc, source_span, value)
    BigInt(value) => constant_node(Constant_BigInt, loc, source_span, value)
  }
}

///|
fn constant_node_from_token(
  token : Token,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode? {
  match token {
    TRUE => Some(scalar_node(Constant_Bool, loc, source_span, Bool(true)))
    FALSE => Some(scalar_node(Constant_Bool, loc, source_span, Bool(false)))
    BYTE(value) => Some(constant_node(Constant_Byte, loc, source_span, value))
    BYTES(value) => Some(constant_node(Constant_Bytes, loc, source_span, value))
    CHAR(value) => Some(constant_node(Constant_Char, loc, source_span, value))
    INT(value) =>
      Some(constant_node_from_syntax(@syntax.make_int(value), loc, source_span))
    FLOAT(value) =>
      Some(
        constant_node_from_syntax(@syntax.make_float(value), loc, source_span),
      )
    DOUBLE(value) =>
      Some(constant_node(Constant_Double, loc, source_span, value))
    STRING(value) =>
      Some(constant_node(Constant_String, loc, source_span, value))
    REGEX_LITERAL(value) =>
      Some(constant_node(Constant_Regex, loc, source_span, value))
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
      Some(scalar_node(Name_Identifier, loc, source_span, String(value)))
    PACKAGE_NAME(value) =>
      Some(scalar_node(Name_Package, loc, source_span, String(value)))
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
        scalar_node(Name_Identifier, trimmed_loc, trimmed_span, String(value)),
      )
    }
    EXTEND =>
      Some(scalar_node(Name_Identifier, loc, source_span, String("extend")))
    UNDERSCORE =>
      Some(scalar_node(Name_Identifier, loc, source_span, String("_")))
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
      Some(scalar_node(Accessor_Index, loc, source_span, Int(index)))
    DOT_LIDENT("_") => Some(node(Accessor_Newtype, loc, source_span, []))
    DOT_LIDENT(value) | DOT_UIDENT(value) =>
      Some(scalar_node(Accessor_Label, loc, source_span, String(value)))
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
        bool_marker_node(Flag_Async, loc.start, source_span.0, true),
      )
    DECLARE =>
      push_child(
        children,
        Some("flag"),
        bool_marker_node(Flag_Declare, loc.start, source_span.0, true),
      )
    LT_QUESTION =>
      push_child(
        children,
        Some("flag"),
        bool_marker_node(Flag_Conditional, loc.start, source_span.0, true),
      )
    PLUS_EQUAL | AUGMENTED_ASSIGNMENT(_) =>
      push_child(
        children,
        Some("operator"),
        operator_node_from_token(Operator_Augmented, token, loc, source_span),
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
        operator_node_from_token(Operator_Infix, token, loc, source_span),
      )
    COMMA =>
      push_child(
        children,
        Some("trailing"),
        marker_node(TrailingMark_Comma, loc.start, source_span.0),
      )
    SEMI(true) =>
      push_child(
        children,
        Some("trailing"),
        marker_node(TrailingMark_Semi, loc.start, source_span.0),
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
  kind : NodeKind,
  token : Token,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  scalar_node(
    kind,
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
  scalar_node(
    constant.kind,
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
      Some(scalar_node(LexLiteral_String, loc, source_span, String(value)))
    REGEX_LITERAL(value) =>
      Some(scalar_node(LexLiteral_Regex, loc, source_span, String(value)))
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
fn token_location_node(
  kind : NodeKind,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  node(kind, loc, source_span, [])
}

///|
fn aggregate_span_node(loc : Location, source_span : (Int, Int)) -> CstNode {
  node(Aggregate_Span, loc, source_span, [])
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
  node(self.kind, self.loc, self.source_span, children)
}

///|
fn CstNode::with_token_location(
  self : CstNode,
  label : String,
  kind : NodeKind,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  self.with_location_metadata(
    label,
    token_location_node(kind, loc, source_span),
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
  if self.bool_marker_value(Flag_Async) is Some(false) ||
    self.first_child_with_label("async_loc") is Some(_) {
    return self
  }
  for keyword in self.children_with_label("kw") {
    if keyword.has_child_kind(Flag_Async) {
      return self.with_token_location(
        "async_loc",
        keyword.kind,
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
        Location::{ start: keyword.loc.start, end: self.loc.end },
        mk_span(keyword.source_span.0, self.source_span.1),
      )
    None => self
  }
}
