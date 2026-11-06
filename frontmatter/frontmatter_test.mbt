///|
test "frontmatter requires an unindented first line of at least three dashes" {
  for
    source in [
      "", "body", "--\nbody", "--- \nbody", "---\t\nbody", "---metadata\nbody", " ---\nbody",
      "\n---\nbody", "// comment\n---\nbody", "\u{FEFF}", "\u{FEFF}body", "\u{FEFF} ---\nbody",
      "---\ra: 1\r---", "---\u{2028}body", "---\u{2029}body",
    ] {
    let split = @frontmatter.split(source)
    assert_eq(split.header, None, msg=source)
    assert_eq(split.body_offset, 0)
    assert_eq(split.body_start.lnum, 1)
  }
}

///|
test "frontmatter follows delimiter BOM CRLF Unicode and EOF rules" {
  for
    (source, offset, length, body, line) in [
      ("---\na: 1\n---\nbody", 4, 5, 13, 4),
      ("----\na: 1\n\n-----\nbody", 5, 6, 17, 5),
      ("---\n---\nbody", 4, 0, 8, 3),
      ("----\n---\n ---\n---- \n----x\n-----\nbody", 5, 21, 32, 7),
      ("----\na: 1\n---\n", 5, 9, 14, 4),
      ("---\ntitle: 中文\n---\n正文", 4, 10, 18, 4),
      ("---\r\na: 1\r\n---\r\nbody", 5, 6, 16, 4),
      ("\u{FEFF}---\na: 1\n---\nbody", 5, 5, 14, 4),
      ("\u{FEFF}---\r\na: 1\r\n---\r\nbody", 6, 6, 17, 4),
      ("---\r\nfirst\nsecond\r\nthird\n---\r\nbody", 5, 20, 30, 6),
      ("---\na: 1", 4, 4, 8, 3),
      ("---\na: 1\n", 4, 5, 9, 3),
      ("---", 3, 0, 3, 2),
      ("---\n", 4, 0, 4, 2),
      ("\u{FEFF}---", 4, 0, 4, 2),
      ("---\n---", 4, 0, 7, 3),
      ("---\na: 1\n---", 4, 5, 12, 4),
      ("\u{FEFF}---\r\n---", 6, 0, 9, 3),
      ("---\nvalue\u{2028}---\u{2029}\n---\nbody", 4, 11, 19, 4),
    ] {
    let split = @frontmatter.split(source, name="example.mbtx")
    assert_eq(split.header, Some((offset, length)), msg=source)
    assert_eq(split.body_offset, body, msg=source)
    assert_eq(split.body_start.lnum, line, msg=source)
    assert_eq(split.body_start.fname, "example.mbtx")
  }
}
