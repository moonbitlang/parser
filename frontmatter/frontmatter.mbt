///|
/// Offsets and lengths are UTF-16 code units, suitable for slicing the source.
/// Positions use the lexer's Unicode scalar offsets. `None` means no header;
/// `Some((offset, 0))` means an explicitly empty header.
#derive(Debug)
pub struct Split {
  header : (Int32, Int32)?
  header_start : @basic.Position
  body_offset : Int32
  body_start : @basic.Position
}

///|
fn next_line(source : StringView, start : Int32) -> (Int32, Int32) {
  for i = start; i < source.length(); i = i + 1 {
    if source[i] == '\n' {
      return (if i > start && source[i - 1] == '\r' { i - 1 } else { i }, i + 1)
    }
  }
  (source.length(), source.length())
}

///|
fn delimiter(
  source : StringView,
  start : Int32,
  end : Int32,
  minimum : Int32,
) -> Bool {
  if end - start < minimum {
    return false
  }
  for i = start; i < end; i = i + 1 {
    if source[i] != '-' {
      return false
    }
  }
  true
}

///|
fn position(
  source : String,
  offset : Int32,
  line : Int32,
  name : String,
) -> @basic.Position {
  let prefix = source[:offset]
  let mut cnum = 0
  let mut bol = 0
  for ch in prefix {
    cnum += 1
    if ch == '\n' {
      bol = cnum
    }
  }
  { fname: name, lnum: line, bol, cnum, }
}

///|
/// Split an optional frontmatter header. Only LF and CRLF delimit lines here.
/// The first line must contain at least three dashes (optionally after a BOM).
/// A closing delimiter must be at least as long; an unclosed header ends at EOF.
pub fn split(source : String, name~ : String = "") -> Split {
  let view = source[:]
  let opening = if source.has_prefix("\u{FEFF}") { 1 } else { 0 }
  let (end, next) = next_line(view, opening)
  if !delimiter(view, opening, end, 3) {
    let start : @basic.Position = { fname: name, lnum: 1, bol: 0, cnum: 0, }
    return {
      header: None,
      header_start: start,
      body_offset: 0,
      body_start: start,
    }
  }
  let width = end - opening
  let header_start = position(source, next, 2, name)
  for offset = next, line = 2 {
    if offset >= source.length() {
      break {
        header: Some((next, source.length() - next)),
        header_start,
        body_offset: source.length(),
        body_start: position(source, source.length(), line, name),
      }
    }
    let (end, after) = next_line(view, offset)
    if delimiter(view, offset, end, width) {
      break {
        header: Some((next, offset - next)),
        header_start,
        body_offset: after,
        body_start: position(source, after, line + 1, name),
      }
    }
    continue after, line + 1
  }
}

///|
pub fn Split::parse_header(
  self : Self,
  source : String,
) -> @moon_config.ParseResult? {
  self.header.map(fn(header) {
    let (offset, length) = header
    @moon_config.parse_moon_pkg(
      source[offset:offset + length].to_owned(),
      name=self.header_start.fname,
      start_pos=self.header_start,
      keep_comments=true,
      validate=false,
    )
  })
}

///|
#deprecated
#doc(hidden)
pub extend Split with Debug::{to_repr}
