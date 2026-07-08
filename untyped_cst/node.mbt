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
pub fn ParseResult::root(self : ParseResult) -> CstNode {
  self.root
}

///|
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
  Location::{ start, end }
}

///|
fn empty_loc(pos : Position) -> Location {
  Location::{ start: pos, end: pos }
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
fn token_node(
  token : Token,
  start : Position,
  end : Position,
  source_span : (Int, Int),
) -> CstNode {
  node(NodeKind::Token(token), mk_loc(start, end), source_span, [])
}

///|
fn missing_node(kind : TokenKind, pos : Position, offset : Int) -> CstNode {
  node(NodeKind::Missing(kind), empty_loc(pos), empty_span(offset), [])
}

///|
fn error_node(
  loc : Location,
  source_span : (Int, Int),
  children : Array[(String?, CstNode)],
) -> CstNode {
  node(NodeKind::Error, loc, source_span, children)
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
