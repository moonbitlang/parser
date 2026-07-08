///|
/// Returns a zero-copy view of this node's text in `source`.
///
/// The returned view is backed by `source`; call `to_owned()` when an owned
/// string is required. `source` must be the same string passed to the parser.
pub fn CstNode::text(self : CstNode, source : String) -> StringView {
  source.sub(start=self.source_span.0, end=self.source_span.1)
}
