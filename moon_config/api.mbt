///|
/// Parse `moon.pkg` source into the post-processed configuration AST.
pub fn parse_moon_pkg(
  name? : String = "moon.pkg",
  source : String,
) -> (Ast, Array[Report]) {
  let (ast, diagnostics) = parse_string(source, name~)
  (post_process_moon_pkg(ast), diagnostics)
}

///|
/// Parse `moon.mod` source into the post-processed configuration AST.
pub fn parse_moon_mod(
  name? : String = "moon.mod",
  source : String,
) -> (Ast, Array[Report]) {
  let (ast, diagnostics) = parse_string(source, name~)
  (post_process_moon_mod(ast), diagnostics)
}

///|
/// Parse `moon.work` source into the configuration AST.
pub fn parse_moon_work(
  name? : String = "moon.work",
  source : String,
) -> (Ast, Array[Report]) {
  let (ast, diagnostics) = parse_string(source, name~)
  (post_process_moon_work(ast), diagnostics)
}

///|
/// Validate the post-processed `Ast` returned by `parse_moon_mod`.
///
/// `options(...)` fields are flattened during parsing, so unknown top-level keys
/// are tolerated and only raw keys left after failed canonicalization are
/// reported as unexpected.
pub fn validate_moon_mod(ast : Ast) -> Array[Report] {
  let report = []
  _validate_moon_mod(ast, report)
  return report
}

///|
/// Validate the post-processed `Ast` returned by `parse_moon_pkg`.
///
/// `options(...)` fields are flattened during parsing, so unknown top-level keys
/// are tolerated and only raw keys left after failed canonicalization are
/// reported as unexpected.
pub fn validate_moon_pkg(ast : Ast) -> Array[Report] {
  let report = []
  _validate_moon_pkg(ast, report)
  return report
}

///|
/// Validate the `Ast` returned by `parse_moon_work`.
///
/// `moon.work` parsing does not flatten option blocks, so validation rejects
/// unknown top-level keys.
pub fn validate_moon_work(ast : Ast) -> Array[Report] {
  let report = []
  _validate_moon_work(ast, report)
  return report
}
