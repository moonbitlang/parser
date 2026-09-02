///|
pub struct CstNode {
  kind : NodeKind
  loc : Location
  source_span : (Int, Int)
  children : ReadOnlyArray[(String?, CstNode)]
} derive(@debug.Debug)

///|
pub struct ParseResult {
  root : CstNode
  diagnostics : ReadOnlyArray[Report]
  docstrings : ReadOnlyArray[List[(Location, Comment)]]
} derive(@debug.Debug)

///|
/// Returns the concrete syntax tree root produced by parsing.
///
/// A root is available even when parsing emitted diagnostics. In that case it
/// may contain recovered or missing nodes.
pub fn ParseResult::root(self : ParseResult) -> CstNode {
  self.root
}

///|
/// Returns a read-only view of the diagnostics emitted while lexing and
/// parsing.
///
/// The view is empty when no diagnostics were reported.
pub fn ParseResult::diagnostics_view(self : ParseResult) -> ArrayView[Report] {
  self.diagnostics.view()
}

///|
fn ParseResult::docstrings_view(
  self : ParseResult,
) -> ArrayView[List[(Location, Comment)]] {
  self.docstrings.view()
}

///|
fn mk_loc(start : Position, end : Position) -> Location {
  Location::{ start, end, }
}

///|
fn empty_loc(pos : Position) -> Location {
  Location::{ start: pos, end: pos, }
}

///|
fn mk_span(start_offset : Int, end_offset : Int) -> (Int, Int) {
  (start_offset, end_offset)
}

///|
fn empty_span(offset : Int) -> (Int, Int) {
  (offset, offset)
}

///|
fn node(
  kind : NodeKind,
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  CstNode::{
    kind,
    loc,
    source_span,
    children: ReadOnlyArray::from_array(children),
  }
}

///|
fn leaf_node(
  payload : NodePayload,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  node(NodeKind::Leaf(payload), loc, source_span, [])
}

///|
fn push_payload_leaf(
  children : Array[(String?, CstNode)],
  payload : NodePayload,
  loc : Location,
  source_span : (Int, Int),
) -> Unit {
  children.push((Some("payload"), leaf_node(payload, loc, source_span)))
}

///|
fn node_with_payload(
  kind : NodeKind,
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
  payload : NodePayload,
) -> CstNode {
  push_payload_leaf(children, payload, loc, source_span)
  node(kind, loc, source_span, children)
}

///|
fn syntax_kind_from_token_kind(kind : TokenKind) -> NodeKind {
  match kind {
    TokenKind::TK_CHAR
    | TokenKind::TK_INT
    | TokenKind::TK_BYTE
    | TokenKind::TK_BYTES
    | TokenKind::TK_FLOAT
    | TokenKind::TK_DOUBLE
    | TokenKind::TK_STRING
    | TokenKind::TK_MULTILINE_STRING
    | TokenKind::TK_REGEX_LITERAL
    | TokenKind::TK_FALSE
    | TokenKind::TK_TRUE => Syntax_Literal
    TokenKind::TK_MULTILINE_INTERP
    | TokenKind::TK_INTERP
    | TokenKind::TK_BYTES_INTERP
    | TokenKind::TK_REGEX_INTERP => Syntax_Interpolation
    TokenKind::TK_ATTRIBUTE => Syntax_Attribute
    TokenKind::TK_LIDENT
    | TokenKind::TK_UIDENT
    | TokenKind::TK_PACKAGE_NAME
    | TokenKind::TK_UNDERSCORE => Syntax_Identifier
    TK_POST_LABEL | TK_POST_LABEL_TRANSPARENT => Syntax_Label
    TokenKind::TK_DOT_LIDENT
    | TokenKind::TK_DOT_UIDENT
    | TokenKind::TK_DOT_INT => Syntax_Accessor
    TokenKind::TK_COMMENT => Syntax_Comment
    TokenKind::TK_NEWLINE => Syntax_Unexpected
    TokenKind::TK_EOF => Syntax_Eof
    TokenKind::TK_LPAREN
    | TokenKind::TK_RPAREN
    | TokenKind::TK_DOT_LPAREN
    | TokenKind::TK_LBRACKET
    | TokenKind::TK_LBRACKET_BAR
    | TokenKind::TK_RBRACKET
    | TokenKind::TK_BAR_RBRACKET
    | TokenKind::TK_LBRACE
    | TokenKind::TK_RBRACE => Syntax_Delimiter
    TokenKind::TK_COMMA
    | TokenKind::TK_COLON
    | TokenKind::TK_COLONCOLON
    | TokenKind::TK_SEMI
    | TokenKind::TK_FAT_ARROW
    | TokenKind::TK_THIN_ARROW => Syntax_Separator
    TokenKind::TK_INFIX1
    | TokenKind::TK_INFIX2
    | TokenKind::TK_INFIX3
    | TokenKind::TK_INFIX4
    | TokenKind::TK_AUGMENTED_ASSIGNMENT
    | TokenKind::TK_PLUS_EQUAL
    | TokenKind::TK_EQUAL
    | TokenKind::TK_EQ_TILDE
    | TokenKind::TK_MINUS
    | TokenKind::TK_QUESTION
    | TokenKind::TK_EXCLAMATION
    | TokenKind::TK_PLUS
    | TokenKind::TK_BAR
    | TokenKind::TK_AMPERAMPER
    | TokenKind::TK_AMPER
    | TokenKind::TK_CARET
    | TokenKind::TK_BARBAR
    | TokenKind::TK_PIPE
    | TokenKind::TK_PIPE_LEFT
    | TokenKind::TK_LT_PLUS
    | TokenKind::TK_LT_QUESTION
    | TokenKind::TK_DOTDOT
    | TokenKind::TK_RANGE_INCLUSIVE
    | TokenKind::TK_RANGE_LT_INCLUSIVE
    | TokenKind::TK_RANGE_EXCLUSIVE
    | TokenKind::TK_RANGE_INCLUSIVE_REV
    | TokenKind::TK_RANGE_EXCLUSIVE_REV
    | TokenKind::TK_IMPLIES
    | TokenKind::TK_ELLIPSIS
    | TokenKind::TK_READONLY
    | TokenKind::TK_EXTEND => Syntax_Operator
    TokenKind::TK_PUB
    | TokenKind::TK_PRIV
    | TokenKind::TK_IMPORT
    | TokenKind::TK_EXTERN
    | TokenKind::TK_BREAK
    | TokenKind::TK_CONTINUE
    | TokenKind::TK_STRUCT
    | TokenKind::TK_ENUM
    | TokenKind::TK_TRAIT
    | TokenKind::TK_DERIVE
    | TokenKind::TK_IMPL
    | TokenKind::TK_WITH
    | TokenKind::TK_RAISE
    | TokenKind::TK_THROW
    | TokenKind::TK_TRY
    | TokenKind::TK_CATCH
    | TokenKind::TK_ASYNC
    | TokenKind::TK_TYPEALIAS
    | TokenKind::TK_TRAITALIAS
    | TokenKind::TK_FNALIAS
    | TokenKind::TK_AS
    | TokenKind::TK_ELSE
    | TokenKind::TK_FN
    | TokenKind::TK_IF
    | TokenKind::TK_LET
    | TokenKind::TK_PROOF_ASSERT
    | TokenKind::TK_PROOF_LET
    | TokenKind::TK_CONST
    | TokenKind::TK_MATCH
    | TokenKind::TK_USING
    | TokenKind::TK_MUTABLE
    | TokenKind::TK_TYPE
    | TokenKind::TK_WHILE
    | TokenKind::TK_RETURN
    | TokenKind::TK_TEST
    | TokenKind::TK_LOOP
    | TokenKind::TK_GUARD
    | TK_GUARD_EXCLAMATION
    | TokenKind::TK_DEFER
    | TokenKind::TK_ERRDEFER
    | TokenKind::TK_FOR
    | TokenKind::TK_IN
    | TokenKind::TK_IS
    | TokenKind::TK_SUBERROR
    | TokenKind::TK_AND
    | TokenKind::TK_FORALL
    | TokenKind::TK_EXISTS
    | TokenKind::TK_LETREC
    | TokenKind::TK_ENUMVIEW
    | TokenKind::TK_EXTENUM
    | TokenKind::TK_DECLARE
    | TokenKind::TK_NORAISE
    | TokenKind::TK_NOCANCEL
    | TokenKind::TK_NOBREAK
    | TokenKind::TK_WHERE
    | TokenKind::TK_VAR
    | TokenKind::TK_TRY_QUESTION
    | TokenKind::TK_TRY_EXCLAMATION
    | TokenKind::TK_LEXMATCH
    | TokenKind::TK_LEXMATCH_QUESTION
    | TokenKind::TK_LEXSCAN
    | TokenKind::TK_PACKAGE => Syntax_Keyword
  }
}

///|
fn token_node(
  token : Token,
  start : Position,
  end : Position,
  source_span : (Int, Int),
) -> CstNode {
  token_node_with_children(token, start, end, source_span, [])
}

///|
fn token_node_with_children(
  token : Token,
  start : Position,
  end : Position,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  let loc = mk_loc(start, end)
  let kind = syntax_kind_from_token_kind(token.kind())
  match token_payload(token) {
    Some(payload) =>
      node_with_payload(kind, loc, source_span, children, payload)
    None => node(kind, loc, source_span, children)
  }
}

///|
fn scalar_node(
  kind : NodeKind,
  loc : Location,
  source_span : (Int, Int),
  payload : NodePayload,
) -> CstNode {
  node_with_payload(kind, loc, source_span, [], payload)
}

///|
fn marker_node(kind : NodeKind, pos : Position, offset : Int) -> CstNode {
  node(kind, empty_loc(pos), empty_span(offset), [])
}

///|
fn bool_marker_node(
  kind : NodeKind,
  pos : Position,
  offset : Int,
  value : Bool,
) -> CstNode {
  scalar_node(kind, empty_loc(pos), empty_span(offset), Bool(value))
}

///|
fn missing_node(kind : TokenKind, pos : Position, offset : Int) -> CstNode {
  let _ = kind
  node(NodeKind::Missing, empty_loc(pos), empty_span(offset), [])
}

///|
fn is_location_metadata_label(label : String?) -> Bool {
  match label {
    Some(label) => label.has_suffix("_loc")
    None => false
  }
}

///|
fn error_node(
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  let recovered_children : Array[(String?, CstNode)] = []
  for entry in children {
    let (label, child) = entry
    recovered_children.push(
      (
        label,
        if child.kind.is_syntax_token() && !is_location_metadata_label(label) {
          child.with_kind(Syntax_Unexpected)
        } else {
          child
        },
      ),
    )
  }
  node(NodeKind::Error, loc, source_span, recovered_children)
}

///|
fn push_child(
  children : Array[(String?, CstNode)],
  label : String?,
  child : CstNode,
) -> Unit {
  children.push((label, child))
}

///|
fn push_children(
  children : Array[(String?, CstNode)],
  more : Array[(String?, CstNode)],
) -> Unit {
  for child in more {
    children.push(child)
  }
}

///|
fn parse_result(
  root : CstNode,
  diagnostics : Array[Report],
  docstrings : Array[List[(Location, Comment)]],
) -> ParseResult {
  ParseResult::{
    root,
    diagnostics: ReadOnlyArray::from_array(diagnostics),
    docstrings: ReadOnlyArray::from_array(docstrings),
  }
}
