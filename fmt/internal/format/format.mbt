///|
pub fn impls_to_string(impls : @syntax.Impls) -> String {
  impls
  |> RemoveGroup::process
  |> x => { Fmt::new().impls(x) }
  |> @pp.render(width=80)
}

///|
pub fn format(source : String, block_line? : Bool = true) -> String {
  let { tokens, docstrings, .. } = @lexer.tokens_from_string(
    comment=true,
    source,
  )
  let (impls, diag) = @handrolled_parser.parse(tokens)
  attach_docstrings(docstrings, impls)
  ignore(diag)
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
