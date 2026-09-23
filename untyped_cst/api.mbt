///|
/// Parses a MoonBit source file into a concrete syntax tree.
///
/// Metavariable syntax is disabled by default. Set `enable_metavar=true` to
/// enable it.
///
/// The returned root node has kind `Impls`. Its source spans use UTF-16
/// code-unit offsets suitable for slicing the original `source` string.
pub fn parse_structure(
  source : String,
  name? : String = "",
  enable_metavar? : Bool = false,
) -> ParseResult {
  let parsed = @cst_parser.parse_structure(source, name~, enable_metavar~)
  parse_result(parsed.root, parsed.diagnostics, parsed.docstrings)
}

///|
/// Parses a MoonBit expression into a concrete syntax tree.
///
/// Metavariable syntax is disabled by default. Set `enable_metavar=true` to
/// enable it.
///
/// The returned root node has kind `Expression`. Its source spans use
/// UTF-16 code-unit offsets suitable for slicing the original `source` string.
pub fn parse_expression(
  source : String,
  name? : String = "",
  enable_metavar? : Bool = false,
) -> ParseResult {
  let parsed = @cst_parser.parse_expression(source, name~, enable_metavar~)
  parse_result(parsed.root, parsed.diagnostics, parsed.docstrings)
}
