///|
test "byte regex retains escapes and reb remains an identifier" {
  let source = "reb reb\"\\xff\\x00\" re\"x\" b\"x\""
  let result = @lexer.tokens_from_string(source, comment=false)
  assert_eq(result.errors.length(), 0)
  guard result.tokens
    is [
      (LIDENT("reb"), _, _),
      (BYTES_REGEX_LITERAL("\\xff\\x00"), start, end),
      (REGEX_LITERAL("x"), _, _),
      (BYTES("x"), _, _),
      (EOF, _, _),
    ] else {
    fail("incorrect literal token kinds")
  }
  assert_eq(start.column(), 5)
  assert_eq(end.column(), 18)
}

///|
test "unterminated byte regex reports once and lexing continues" {
  let source =
    #|// An unterminated literal is reported once, and parsing goes on.
    #|fn main {
    #|  let _ = reb"abc
    #|  let _ = 1
    #|}
  let result = @lexer.tokens_from_string(source, comment=true)
  assert_eq(result.errors.length(), 1)
  assert_true(
    result.tokens.any(triple => triple.0 is INT("1") && triple.1.lnum == 4),
  )
}

///|
test "lexing from a nonzero position preserves subsequent line columns" {
  for newline in ["\n", "\r\n"] {
    let result = @lexer.tokens_from_string(
      "let first = \"😀\"" + newline + "let second = 1",
      name="offset.mbtx",
      start_pos={ fname: "offset.mbtx", lnum: 7, bol: 40, cnum: 40, },
      comment=false,
    )
    assert_eq(result.errors.length(), 0)
    guard result.tokens.filter(triple => triple.0 is LIDENT("second")).get(0)
      is Some((_, start, _)) else {
      fail("missing second declaration")
    }
    assert_eq(start.lnum, 8)
    assert_eq(start.column(), 5)
    assert_eq(start.cnum, 40 + 15 + newline.length() + 4)
  }
}
