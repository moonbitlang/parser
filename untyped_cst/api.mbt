///|
fn initial_position(name : String) -> Position {
  Position::{ fname: name, lnum: 1, bol: 0, cnum: 0 }
}

///|
fn parse_with_state(
  tokens : Triples,
  source : String,
  diagnostics : Array[Report],
  docstrings : Array[List[(Location, Comment)]],
  parse_root : (ParserState) -> CstNode,
) -> ParseResult {
  let state = new_state(tokens, source, diagnostics)
  let root = parse_root(state)
  parse_result(root, state.reports, docstrings)
}

///|
fn parse_source(
  source : String,
  name : String,
  parse_root : (ParserState) -> CstNode,
) -> ParseResult {
  let lex = @lexer.tokens_from_string(source, name~, comment=true)
  let diagnostics = reports_from_lex_errors(lex.errors)
  parse_with_state(lex.tokens, source, diagnostics, lex.docstrings, parse_root)
}

///|
/// Parses a MoonBit source file into a concrete syntax tree.
///
/// The returned root node has kind `SourceFile`. Its source spans use UTF-16
/// code-unit offsets suitable for slicing the original `source` string.
pub fn parse_structure(source : String, name? : String = "") -> ParseResult {
  parse_source(source, name, parse_source_file)
}

///|
/// Parses a MoonBit expression into a concrete syntax tree.
///
/// The returned root node has kind `ExpressionFragment`. Its source spans use
/// UTF-16 code-unit offsets suitable for slicing the original `source` string.
pub fn parse_expression(source : String, name? : String = "") -> ParseResult {
  parse_source(source, name, parse_expr_fragment)
}
