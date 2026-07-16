///|
pub fn impls_to_string(impls : @syntax.Impls) -> String {
  impls
  |> RemoveGroup::process
  |> x => { Fmt::new().impls(x) }
  |> @pp.render(width=80)
}

///|
priv struct RecoveredHoleDetector {
  mut found : Bool
}

///|
extend RecoveredHoleDetector with IterVisitor::{visit_Impl}

///|
impl IterVisitor for RecoveredHoleDetector with fn visit_Expr_Hole(
  self,
  loc~,
  kind~,
) {
  ignore(loc)
  match kind {
    Synthesized => self.found = true
    Incomplete | Todo => ()
  }
}

///|
fn has_recovered_hole(impls : @syntax.Impls) -> Bool {
  let detector = { found: false }
  impls.map(item => detector.visit_Impl(item)) |> ignore
  detector.found
}

///|
pub fn format(source : String, block_line? : Bool = true) -> String {
  let { tokens, docstrings, errors } = @lexer.tokens_from_string(
    comment=true,
    source,
  )
  if errors.length() != 0 {
    return source
  }
  let (impls, diag) = @handrolled_parser.parse(tokens)
  // Recovered parser output can omit invalid source text, so diagnostics make
  // formatting unsafe even when an AST is returned.
  if diag.length() != 0 {
    return source
  }
  if has_recovered_hole(impls) {
    return source
  }
  attach_docstrings(docstrings, impls)
  impls
  |> RemoveGroup::process
  |> x => { Fmt::new(block_line~).impls(x) }
  |> @pp.render(width=80)
}

///|
/// for testing purposes, returns also the tokens
pub fn parse(
  source : String,
) -> (@syntax.Impls, Array[@basic.Report], @tokens.Triples) {
  let { tokens, docstrings, .. } = @lexer.tokens_from_string(
    comment=true,
    source,
  )
  let (impls, diag) = @handrolled_parser.parse(tokens)
  attach_docstrings(docstrings, impls)
  let impls = impls |> RemoveGroup::process
  (impls, diag, tokens)
}

///|
pub fn debug_comment(source : String) -> (String, String) {
  let { tokens, .. } = @lexer.tokens_from_string(comment=true, source)
  let (impls, _) = @handrolled_parser.parse(tokens)
  let mapper = @comment.Mapper::new(tokens, impls)
  let attach_info = mapper.to_debug_info()
  let ast = impls
    |> RemoveGroup::process
    |> x => { Fmt::new(mapper~).impls(x) }
    |> @pp.render(width=80)
  (attach_info, ast)
}

///|
fn init {
  ignore(debug_comment)
}
