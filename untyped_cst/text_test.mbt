///|
fn @untyped_cst.CstNode::text(self : Self, source : String) -> StringView {
  let (start, end) = self.source_span
  source[start:end]
}
