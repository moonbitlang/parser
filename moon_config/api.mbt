///|
pub fn parse_moon_pkg(
  name? : String = "moon.pkg",
  source : String,
) -> (Ast, Array[Report]) {
  let (ast, diagnostics) = parse_string(source, name~)
  (post_process_moon_pkg(ast, diagnostics), diagnostics)
}

///|
pub fn parse_moon_mod(
  name? : String = "moon.mod",
  source : String,
) -> (Ast, Array[Report]) {
  let (ast, diagnostics) = parse_string(source, name~)
  (post_process_moon_mod(ast, diagnostics), diagnostics)
}

///|
pub fn parse_moon_work(
  name? : String = "moon.work",
  source : String,
) -> (Ast, Array[Report]) {
  let (ast, diagnostics) = parse_string(source, name~)
  (post_process_moon_work(ast, diagnostics), diagnostics)
}
