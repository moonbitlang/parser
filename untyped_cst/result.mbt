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
