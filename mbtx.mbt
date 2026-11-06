///|
/// A separately parsed configuration header and source body, with original
/// file locations.
pub struct Mbtx {
  header : @moon_config.ParseResult?
  body : @syntax.Impls
}

///|
/// Parse a configuration header and source body. Reports include header
/// errors/warnings and body errors, with original file locations.
pub fn parse_mbtx_string(
  source : String,
  name~ : String = "",
  parser~ : Parser = Handrolled,
) -> (Mbtx, Array[@basic.Report]) {
  let split = @frontmatter.split(source, name~)
  let header = split.parse_header(source)
  let (body, reports) = parse_string_at(
    source[split.body_offset:].to_owned(),
    start_pos=split.body_start,
    name~,
    parser~,
  )
  let diagnostics : Array[Report] = []
  if header is Some(config) {
    diagnostics.append(config.diagnostics.errors)
    diagnostics.append(config.diagnostics.warnings)
  }
  diagnostics.append(reports)
  ({ header, body, }, diagnostics)
}
