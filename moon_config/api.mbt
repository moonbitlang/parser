///|
/// Parse `moon.pkg` source into the post-processed configuration AST.
pub fn parse_moon_pkg(
  name? : String = "moon.pkg",
  source : String,
) -> (Ast, Array[Report]) {
  let (ast, diagnostics) = parse_string(source, name~)
  (post_process_moon_pkg(ast, diagnostics), diagnostics)
}

///|
/// Parse `moon.mod` source into the post-processed configuration AST.
pub fn parse_moon_mod(
  name? : String = "moon.mod",
  source : String,
) -> (Ast, Array[Report]) {
  let (ast, diagnostics) = parse_string(source, name~)
  (post_process_moon_mod(ast, diagnostics), diagnostics)
}

///|
/// Parse `moon.work` source into the configuration AST.
pub fn parse_moon_work(
  name? : String = "moon.work",
  source : String,
) -> (Ast, Array[Report]) {
  let (ast, diagnostics) = parse_string(source, name~)
  (post_process_moon_work(ast, diagnostics), diagnostics)
}

///|
/// Validate a post-processed `moon.mod` `Ast`.
///
/// Source-level checks that need raw keys, such as duplicate `options(...)`
/// blocks, are handled by `parse_moon_mod`.
pub fn validate_moon_mod(ast : Ast) -> Array[Report] {
  let report = []
  _validate_moon_mod(ast, report)
  return report
}

///|
/// Validate a post-processed `moon.pkg` `Ast`.
///
/// Source-level checks that need raw keys, such as duplicate `options(...)`
/// blocks, are handled by `parse_moon_pkg`.
pub fn validate_moon_pkg(ast : Ast) -> Array[Report] {
  let report = []
  _validate_moon_pkg(ast, report)
  return report
}

///|
/// Validate a `moon.work` `Ast`.
pub fn validate_moon_work(ast : Ast) -> Array[Report] {
  let report = []
  _validate_moon_work(ast, report)
  return report
}
