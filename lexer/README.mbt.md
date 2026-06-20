# moonbitlang/lexer

`moonbitlang/lexer` turns MoonBit source text into token streams used by
the parsers in this repository.

The main entry points are `tokens_from_string` and
`tokens_from_string_with_utf16_location`. Both return a `LexResult` containing
tokens, lexical errors, and docstring/comment metadata.

`tokens_from_string` uses the lexer default location accounting.
`tokens_from_string_with_utf16_location` tokenizes the same source but reports
locations in UTF-16 code units, which is useful for editor and LSP integrations.

## Examples

```mbt check
///|
test "tokenize source text" {
  let result = @lexer.tokens_from_string("let answer = 42", comment=false)
  assert_eq(result.errors.length(), 0)
  debug_inspect(
    result.tokens.map(triple => triple.0),
    content=(
      #|[LET, LIDENT("answer"), EQUAL, INT("42"), EOF]
    ),
  )
}
```

```mbt check
///|
test "utf16 location mode keeps the same tokens" {
  let source = "let x = \"😀\""
  let normal = @lexer.tokens_from_string(source, comment=false)
  let utf16 = @lexer.tokens_from_string_with_utf16_location(
    source,
    comment=false,
  )
  debug_inspect(
    normal.tokens.map(triple => triple.0),
    content=(
      #|[LET, LIDENT("x"), EQUAL, STRING("😀"), EOF]
    ),
  )
  debug_inspect(
    utf16.tokens.map(triple => triple.0),
    content=(
      #|[LET, LIDENT("x"), EQUAL, STRING("😀"), EOF]
    ),
  )
}
```
