///|
pub struct CstNode {
  kind : NodeKind
  loc : Location
  source_span : (Int, Int)
  children : ReadOnlyArray[(String?, CstNode)]
} derive(@debug.Debug)

///|
pub fn mk_loc(start : Position, end : Position) -> Location {
  Location::{ start, end, }
}

///|
pub fn empty_loc(pos : Position) -> Location {
  Location::{ start: pos, end: pos, }
}

///|
pub fn empty_span(offset : Int) -> (Int, Int) {
  (offset, offset)
}

///|
fn node(
  kind : NodeKind,
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  { kind, loc, source_span, children: ReadOnlyArray::from_array(children), }
}

///|
fn leaf_node(
  payload : NodePayload,
  loc : Location,
  source_span : (Int, Int),
) -> CstNode {
  node(Leaf(payload), loc, source_span, [])
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
#internal(internal, "For untyped_cst parser implementation only")
#doc(hidden)
pub fn syntax_kind_from_token_kind(kind : TokenKind) -> NodeKind {
  match kind {
    TK_CHAR
    | TK_INT
    | TK_BYTE
    | TK_BYTES
    | TK_FLOAT
    | TK_DOUBLE
    | TK_STRING
    | TK_MULTILINE_STRING
    | TK_REGEX_LITERAL
    | TK_FALSE
    | TK_TRUE => Syntax_Literal
    TK_MULTILINE_INTERP | TK_INTERP | TK_BYTES_INTERP | TK_REGEX_INTERP =>
      Syntax_Interpolation
    TK_ATTRIBUTE => Syntax_Attribute
    TK_LIDENT | TK_UIDENT | TK_PACKAGE_NAME | TK_UNDERSCORE => Syntax_Identifier
    TK_POST_LABEL | TK_POST_LABEL_TRANSPARENT => Syntax_Label
    TK_DOT_LIDENT | TK_DOT_UIDENT | TK_DOT_INT => Syntax_Accessor
    TK_COMMENT => Syntax_Comment
    TK_NEWLINE => Syntax_Unexpected
    TK_EOF => Syntax_Eof
    TK_LPAREN
    | TK_RPAREN
    | TK_DOT_LPAREN
    | TK_LBRACKET
    | TK_LBRACKET_BAR
    | TK_RBRACKET
    | TK_BAR_RBRACKET
    | TK_LBRACE
    | TK_RBRACE => Syntax_Delimiter
    TK_COMMA
    | TK_COLON
    | TK_COLONCOLON
    | TK_SEMI
    | TK_FAT_ARROW
    | TK_THIN_ARROW => Syntax_Separator
    TK_INFIX1
    | TK_INFIX2
    | TK_INFIX3
    | TK_INFIX4
    | TK_AUGMENTED_ASSIGNMENT
    | TK_PLUS_EQUAL
    | TK_EQUAL
    | TK_EQ_TILDE
    | TK_MINUS
    | TK_QUESTION
    | TK_EXCLAMATION
    | TK_PLUS
    | TK_BAR
    | TK_AMPERAMPER
    | TK_AMPER
    | TK_CARET
    | TK_BARBAR
    | TK_PIPE
    | TK_PIPE_LEFT
    | TK_LT_PLUS
    | TK_LT_QUESTION
    | TK_DOTDOT
    | TK_RANGE_INCLUSIVE
    | TK_RANGE_LT_INCLUSIVE
    | TK_RANGE_EXCLUSIVE
    | TK_RANGE_INCLUSIVE_REV
    | TK_RANGE_EXCLUSIVE_REV
    | TK_IMPLIES
    | TK_ELLIPSIS
    | TK_READONLY
    | TK_EXTEND => Syntax_Operator
    TK_PUB
    | TK_PRIV
    | TK_IMPORT
    | TK_EXTERN
    | TK_BREAK
    | TK_CONTINUE
    | TK_STRUCT
    | TK_ENUM
    | TK_TRAIT
    | TK_DERIVE
    | TK_IMPL
    | TK_WITH
    | TK_RAISE
    | TK_THROW
    | TK_TRY
    | TK_CATCH
    | TK_ASYNC
    | TK_TYPEALIAS
    | TK_TRAITALIAS
    | TK_FNALIAS
    | TK_AS
    | TK_ELSE
    | TK_FN
    | TK_IF
    | TK_LET
    | TK_PROOF_ASSERT
    | TK_PROOF_LET
    | TK_CONST
    | TK_MATCH
    | TK_USING
    | TK_MUTABLE
    | TK_TYPE
    | TK_WHILE
    | TK_RETURN
    | TK_TEST
    | TK_LOOP
    | TK_GUARD
    | TK_GUARD_EXCLAMATION
    | TK_DEFER
    | TK_ERRDEFER
    | TK_FOR
    | TK_IN
    | TK_IS
    | TK_SUBERROR
    | TK_AND
    | TK_FORALL
    | TK_EXISTS
    | TK_LETREC
    | TK_ENUMVIEW
    | TK_EXTENUM
    | TK_DECLARE
    | TK_NORAISE
    | TK_NOCANCEL
    | TK_NOBREAK
    | TK_WHERE
    | TK_VAR
    | TK_TRY_QUESTION
    | TK_TRY_EXCLAMATION
    | TK_LEXMATCH
    | TK_LEXMATCH_QUESTION
    | TK_LEXSCAN
    | TK_PACKAGE => Syntax_Keyword
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
  node(Missing, empty_loc(pos), empty_span(offset), [])
}

///|
pub fn is_location_metadata_label(label : String?) -> Bool {
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
  node(Error, loc, source_span, recovered_children)
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
#deprecated
#doc(hidden)
pub extend CstNode with Debug::{to_repr}
