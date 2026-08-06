///|
pub fn impls_to_string(impls : @syntax.Impls) -> String {
  impls
  |> RemoveGroup::process
  |> x => { Fmt::new().impls(x) }
  |> @pp.render(width=80)
}

///|
fn format_once(
  impls : @syntax.Impls,
  tokens : @tokens.Triples,
  block_line~ : Bool,
) -> String? {
  let impls = RemoveGroup::process(impls)
  let mapper = @comment.CheckpointMapper::new(tokens)
  let attach_fmt = Fmt::new(checkpoint_mapper=mapper, block_line~)
  ignore(attach_fmt.placeholder() + attach_fmt.impls(impls) + attach_fmt.eof())
  mapper.begin_pretty()
  let pretty_fmt = Fmt::new(checkpoint_mapper=mapper, block_line~)
  let doc = pretty_fmt.placeholder() +
    pretty_fmt.impls(impls) +
    pretty_fmt.eof()
  guard mapper.is_complete() else { return None }
  Some(@pp.render(doc, width=80) |> with_final_newline)
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
  guard strict_parse(source) is Some(parsed) else { return source }
  guard format_once(parsed.impls, parsed.tokens, block_line~) is Some(candidate) else {
    return source
  }
  guard strict_parse(candidate) is Some(reparsed) else { return source }
  guard same_semantics(parsed, reparsed) else { return source }
  // A formatter result is only useful if it is already a fixed point. This
  // also makes pathological comment layouts fail closed instead of exposing
  // a first-pass intermediate form to API and CLI callers.
  guard format_once(reparsed.impls, reparsed.tokens, block_line~)
    is Some(second) &&
    second == candidate else {
    return source
  }
  candidate
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
  attach_docstrings(docstrings, impls, tokens)
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
