///|
/// The body CST retains UTF-16 source spans into the complete original file.
pub struct MbtxParseResult {
  header : @moon_config.ParseResult?
  body : ParseResult
  diagnostics : Array[Report]
}

///|
pub fn parse_mbtx(
  source : String,
  name~ : String = "",
  enable_metavar~ : Bool = false,
) -> MbtxParseResult {
  let split = @frontmatter.split(source, name~)
  let header = split.parse_header(source)
  let parsed = @cst_parser.parse_structure_at(
    source,
    start_offset=split.body_offset,
    start_pos=split.body_start,
    name~,
    enable_metavar~,
  )
  let body = parse_result(parsed.root, parsed.diagnostics, parsed.docstrings)
  let diagnostics : Array[Report] = []
  if header is Some(config) {
    diagnostics.append(config.diagnostics.errors)
    diagnostics.append(config.diagnostics.warnings)
  }
  diagnostics.append(parsed.diagnostics)
  { header, body, diagnostics, }
}
