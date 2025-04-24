///|
fn test_tokens(t : @test.T, label : String) -> Unit!Error {
  let source = @fs.read_file_to_bytes!("src/lib/fixtures/\{label}.mbt_")
  let triples = tokens_from_bytes(comment=true, source)
  let buf = StringBuilder::new()
  for triple in triples {
    buf.write_char('(')
    buf.write_string(
      match triple.0 {
        PACKAGE_NAME(name) => "(PACKAGE_NAME \{name})"
        SEMI(b) => "(SEMI \{b})"
        DOT_INT(i) => "(DOT_INT \{i})"
        DOT_UIDENT(id) => "(DOT_UIDENT \{id})"
        DOT_LIDENT(id) => "(DOT_LIDENT \{id})"
        AUGMENTED_ASSIGNMENT(op) => "(AUGMENTED_ASSIGNMENT \{op})"
        INFIX4(op) => "(INFIX4 \{op})"
        INFIX3(op) => "(INFIX3 \{op})"
        INFIX2(op) => "(INFIX2 \{op})"
        INFIX1(op) => "(INFIX1 \{op})"
        COMMENT(cmt) => "(COMMENT \{cmt})"
        POST_LABEL(l) => "(POST_LABEL \{l})"
        UIDENT(id) => "(UIDENT \{id})"
        LIDENT(id) => "(LIDENT \{id})"
        ATTRIBUTE(attr) => "(ATTRIBUTE \{attr})"
        INTERP(interp) => "(INTERP \{interp})"
        MULTILINE_INTERP(interp) => "(MULTILINE_INTERP \{interp})"
        MULTILINE_STRING(s) => "(MULTILINE_STRING \{s})"
        STRING(s) => "(STRING \{s})"
        FLOAT(s) => "(FLOAT \{s})"
        BYTES(s) => "(BYTES \{s})"
        BYTE(s) => "(BYTE \{s})"
        INT(s) => "(INT \{s})"
        CHAR(s) => "(CHAR \{s})"
        tok => tok.to_string()
      },
    )
    buf.write_char(' ')
    let l1 = triple.1.lnum
    let c1 = triple.1.cnum - triple.1.bol + 1
    let l2 = triple.2.lnum
    let c2 = triple.2.cnum - triple.2.bol + 1
    buf.write_string("\{l1}:\{c1}-\{l2}:\{c2}")
    buf.write_char(')')
    buf.write_char('\n')
  }
  t.write(buf.to_string())
  t.snapshot!(filename="\{label}.tokens")
}

///|
test "tokens 001" (t : @test.T) {
  test_tokens!(t, "001")
}

///|
test "tokens double_to_string" (t : @test.T) {
  test_tokens!(t, "double_to_string")
}

///|
test "tokens pattern_guard" (t : @test.T) {
  test_tokens!(t, "pattern_guard")
}

///|
test "tokens range_pattern" (t : @test.T) {
  test_tokens!(t, "range_pattern")
}

///|
test "tokens string_escape" (t : @test.T) {
  test_tokens!(t, "string_escape")
}

///|
test "tokens string_literal" (t : @test.T) {
  test_tokens!(t, "string_literal")
}

///|
test "tokens super_trait3" (t : @test.T) {
  test_tokens!(t, "super_trait3")
}

///|
test "tokens trait_labelled" (t : @test.T) {
  test_tokens!(t, "trait_labelled")
}

///|
test "tokens unicode_test" (t : @test.T) {
  test_tokens!(t, "unicode_test")
}

///|
test "tokens utf16_escape" (t : @test.T) {
  test_tokens!(t, "utf16_escape")
}
