///|
#derive(Debug)
pub struct Diagnostics {
  errors : Array[Report]
  warnings : Array[Report]
}

///|
#derive(Debug)
pub struct ParseResult {
  ast : Ast
  tokens : Triples
  diagnostics : Diagnostics
}

///|
/// Parse configuration syntax and validate it without rewriting the tree.
pub fn parse_moon_pkg(
  name~ : String = "moon.pkg",
  start_pos~ : Position = { fname: "", lnum: 1, bol: 0, cnum: 0, },
  keep_comments~ : Bool = false,
  validate~ : Bool = true,
  source : String,
) -> ParseResult {
  let result = parse_string(
    source,
    name~,
    keep_comments~,
    allow_reserved=true,
    start_pos~,
  )
  if validate {
    validate_config(result.ast, Package, result.diagnostics)
  }
  result
}

///|
/// Parse configuration syntax and validate it without rewriting the tree.
pub fn parse_moon_mod(
  name~ : String = "moon.mod",
  keep_comments~ : Bool = false,
  validate~ : Bool = true,
  source : String,
) -> ParseResult {
  let result = parse_string(source, name~, keep_comments~)
  if validate {
    validate_config(result.ast, Module, result.diagnostics)
  }
  result
}

///|
pub fn parse_moon_work(
  name~ : String = "moon.work",
  keep_comments~ : Bool = false,
  validate~ : Bool = true,
  source : String,
) -> ParseResult {
  let result = parse_string(source, name~, keep_comments~, allow_reserved=true)
  if validate {
    validate_config(result.ast, Workspace, result.diagnostics)
  }
  result
}

///|
pub fn validate_moon_mod(ast : Ast) -> Diagnostics {
  let diagnostics = Diagnostics::{ errors: [], warnings: [], }
  validate_config(ast, Module, diagnostics)
  diagnostics
}

///|
pub fn validate_moon_pkg(ast : Ast) -> Diagnostics {
  let diagnostics = Diagnostics::{ errors: [], warnings: [], }
  validate_config(ast, Package, diagnostics)
  diagnostics
}

///|
pub fn validate_moon_work(ast : Ast) -> Diagnostics {
  let diagnostics = Diagnostics::{ errors: [], warnings: [], }
  validate_config(ast, Workspace, diagnostics)
  diagnostics
}

///|
pub extend Diagnostics with Debug::{to_repr}

///|
pub extend ParseResult with Debug::{to_repr}
