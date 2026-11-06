///|
/// Format a configuration header and source body independently. Delimiters and
/// the distinction between absent, empty and unclosed headers are preserved.
pub fn format_mbtx(source : String, block_line~ : Bool = true) -> String {
  let parsed = @parser.parse_mbtx(source)
  guard parsed.diagnostics.is_empty() else { return source }
  let split = @frontmatter.split(source)
  guard split.header is Some((offset, length)) else {
    return @format.format(source, block_line~)
  }
  let header = source[offset:offset + length].to_owned()
  let formatted_header = if header.trim().is_empty() {
    ""
  } else {
    @format.format_moon_pkg(header)
  }
  let body = source[split.body_offset:].to_owned()
  let formatted_body = if body.trim().is_empty() {
    ""
  } else {
    @format.format(body, block_line~)
  }
  // Ensure a nonempty formatted header does not join the closing delimiter.
  let separator = if !formatted_header.is_empty() &&
    !formatted_header.has_suffix("\n") {
    "\n"
  } else {
    ""
  }
  source[:offset].to_owned() +
  formatted_header +
  separator +
  source[offset + length:split.body_offset].to_owned() +
  formatted_body
}
