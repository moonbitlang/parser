///|
using @basic {type Position, type Report}

///|
pub(all) enum Parser {
  MoonYacc
  Handrolled
  // Mixed
}

///|
pub fn parse_string(
  source : String,
  name~ : String = "",
  parser~ : Parser = Handrolled,
) -> (@syntax.Impls, Array[Report]) {
  parse_string_at(source, name~, parser~, start_pos={
    fname: name,
    lnum: 1,
    bol: 0,
    cnum: 0,
  })
}

///|
fn parse_string_at(
  source : String,
  start_pos~ : Position,
  name~ : String = "",
  parser~ : Parser = Handrolled,
) -> (@syntax.Impls, Array[Report]) {
  let { tokens, docstrings, errors, } = @lexer.tokens_from_string(
    source,
    comment=true,
    name~,
    start_pos~,
  )
  fn parse_by_moonyacc(tokens : @tokens.Triples) {
    let tokens = tokens.filter(fn(triple) {
      !(triple.0 is NEWLINE || triple.0 is COMMENT(_))
    })
    (@yacc_parser.structure(tokens), []) catch {
      UnexpectedEndOfInput(pos, _) =>
        (
          List([]),
          [
            Report::{
              loc: { start: pos, end: pos, },
              msg: "Unexpected end of file.",
            },
          ],
        )
      UnexpectedToken(token, (start, end), _) =>
        (
          List([]),
          [
            {
              loc: { start, end, },
              msg: "Unexpected `\{@debug.render(Debug::to_repr(token))}`.",
            },
          ],
        )
    }
  }

  let (impls, diagnostics) = match parser {
    MoonYacc => parse_by_moonyacc(tokens)
    Handrolled => @handrolled_parser.parse(tokens)
  }
  @docstring.attach_docstrings_to_impls(docstrings[:], impls)
  let reports = errors.filter_map(fn(error) {
    let (start, end, err) = error
    match err {
      Reserved_keyword(_) => None
      _ => Some(Report::{ loc: { start, end, }, msg: Show::to_string(err), })
    }
  })
  reports.append(diagnostics)
  (impls, reports)
}
