///|
fn parse_with_state(
  tokens : Triples,
  source : String,
  diagnostics : Array[Report],
  docstrings : Array[List[(Location, Comment)]],
  enable_metavar : Bool,
  parse_root : (ParserState) -> CstNode,
) -> ParseOutput {
  let state = new_state(
    tokens, source, diagnostics, enable_metavar, parse_interp_segment,
  )
  let root = parse_root(state)
  { root, diagnostics: state.reports, docstrings, }
}

///|
fn parse_source(
  source : String,
  name : String,
  enable_metavar : Bool,
  parse_root : (ParserState) -> CstNode,
) -> ParseOutput {
  let lex = @lexer.tokens_from_string(
    source,
    name~,
    enable_metavar~,
    comment=true,
  )
  let diagnostics = reports_from_lex_errors(lex.errors)
  parse_with_state(
    lex.tokens,
    source,
    diagnostics,
    lex.docstrings,
    enable_metavar,
    parse_root,
  )
}

///|
pub struct ParseOutput {
  root : CstNode
  diagnostics : Array[Report]
  docstrings : Array[List[(Location, Comment)]]
}

///|
pub fn parse_structure(
  source : String,
  name? : String = "",
  enable_metavar? : Bool = false,
) -> ParseOutput {
  parse_source(source, name, enable_metavar, parse_source_file)
}

///|
pub fn parse_expression(
  source : String,
  name? : String = "",
  enable_metavar? : Bool = false,
) -> ParseOutput {
  parse_source(source, name, enable_metavar, parse_expr_fragment)
}
