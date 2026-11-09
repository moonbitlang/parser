///|
fn assert_extend_separator(source : String, expected : Bool) -> Unit raise {
  for comment in [false, true] {
    let result = @lexer.tokens_from_string(source, comment~)
    assert_eq(result.errors.length(), 0)
    let significant = result.tokens.filter(t => !(t.0 is (COMMENT(_) | NEWLINE)))
    let mut checked = false
    for i, (token, _, _) in significant {
      if token is EXTEND {
        let next = significant[i + 1].0
        if (next is SEMI(_)) != expected {
          fail("unexpected ASI after EXTEND in " + source)
        }
        checked = true
      }
    }
    assert_true(checked)
  }
}

///|
test "extend declaration ASI recognizes top level prefixes" {
  for newline in ["\n", "\r\n"] {
    for gap in [newline, " // one" + newline + " // two 🐇" + newline] {
      for
        prefix in [
          "", "pub ", "priv ", "#deprecated\n", "#deprecated\n#internal(\"x\")\npub ",
          "fn earlier() {}\n", "let earlier = 1\n", "let earlier = 1; ", "pub(all) ",
          "pub( // modifier\n readonly) ",
        ] {
        assert_extend_separator(
          prefix + "extend" + gap + "T with Trait::{f}",
          false,
        )
      }
    }
  }
}

///|
test "extend identifier ASI remains unchanged at every delimiter depth" {
  for
    prefix in [
      "let x = ", "fn ", "fn T::", "fn f() { ", "fn f(", "fn f(x : ", "fn f() { g(",
      "let xs = [", "let xs = [|", "let x = { field: ", "let x = a.(",
    ] {
    assert_extend_separator(prefix + "extend\nvalue", true)
    assert_extend_separator(prefix + "extend // one\n // two\nvalue", true)
  }
  assert_extend_separator("extend;\nT with Trait::{f}", true)
}
