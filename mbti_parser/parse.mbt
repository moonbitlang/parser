///|
fn dummy_mbti() -> Mbti {
  {
    package_name: "",
    imports: @list.empty(),
    sigs: @list.empty(),
    loc: @syntax.no_location,
    base_pos: @basic.BasePos::none(),
  }
}

///|
fn mbti_tokens(
  tokens : Array[(Token, Position, Position)],
) -> Array[(Token, Position, Position)] {
  tokens.filter(fn(triple) { !(triple.0 is (NEWLINE | COMMENT(_))) })
}

///|
fn lex_reports(
  errors : Array[(Position, Position, @lexer.LexicalError)],
) -> Array[Report] {
  errors.map(fn(error) {
    let (start, end, err) = error
    { loc: { start, end }, msg: err.to_string() }
  })
}

///|
pub fn parse_string(
  source : String,
  name? : String = "",
) -> (Mbti, Array[Report]) {
  let lex_result = @lexer.tokens_from_string(name~, source, comment=false)
  let reports = lex_reports(lex_result.errors)
  if !reports.is_empty() {
    (dummy_mbti(), reports)
  } else {
    let initial_pos = Position::{ fname: name, lnum: 1, bol: 0, cnum: 0 }
    (mbti(mbti_tokens(lex_result.tokens), initial_pos~), []) catch {
      UnexpectedEndOfInput(pos, _) =>
        (
          dummy_mbti(),
          [{ loc: { start: pos, end: pos }, msg: "Unexpected end of file." }],
        )
      UnexpectedToken(token, (start, end), _) =>
        (
          dummy_mbti(),
          [
            {
              loc: { start, end },
              msg: "unexpected \{token.to_expect_string()} here",
            },
          ],
        )
    }
  }
}
