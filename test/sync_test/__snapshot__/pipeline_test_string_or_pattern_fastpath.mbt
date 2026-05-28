///| Match string patterns whose fixed character positions are or-patterns.
fn classify_head(s : String) -> String {
  match s {
    ['e' | 'E', .. rest] => "head-e:" + rest.to_owned()
    ['x' | 'X' as ch, .. rest] => "head-x:" + ch.to_string() + ":" + rest.to_owned()
    _ => "head-other"
  }
}

///| Keep scanning after an or-pattern so later character paths use the right codeunit index.
fn classify_after_prefix(s : String) -> String {
  match s {
    ['0', 'x' | 'X', 'a' | 'A'..='F', .. rest] => "hex:" + rest.to_owned()
    ['0', 'b' | 'B', '0' | '1', .. rest] => "bin:" + rest.to_owned()
    _ => "prefix-other"
  }
}

///| Exercise the reversed path used by suffix patterns.
fn classify_tail(s : String) -> String {
  match s {
    [.. rest, 'e' | 'E'] => "tail-e:" + rest.to_owned()
    [.. rest, 'x' | 'X' as ch] => "tail-x:" + rest.to_owned() + ":" + ch.to_string()
    _ => "tail-other"
  }
}

///| Non-BMP alternatives should remain correct while nearby BMP or-patterns still match.
fn classify_mixed_width(s : String) -> String {
  match s {
    ['e' | 'E', '🤣', .. rest] => "emoji-after-e:" + rest.to_owned()
    ['🤣' | '😂', 'e' | 'E', .. rest] => "emoji-first:" + rest.to_owned()
    _ => "mixed-other"
  }
}

///| Suffix positions before a non-BMP or-pattern must not be read as codeunit fields first.
fn classify_wide_suffix_guard(s : String) -> String {
  match s {
    [.., 'b', '😀' | '😁', 'a'] => "wide-suffix"
    _ => "wide-suffix-other"
  }
}

///| Codeunit-safe fixed fields can use codeunit length guards and direct rest slicing.
fn classify_codeunit_rest(s : String) -> String {
  match s {
    ['c', .. rest] => "rest-c:" + rest.to_owned()
    ['K' | 'Q', .. rest, '!'] => "rest-punct:" + rest.to_owned()
    [_, .. rest] => "rest-any:" + rest.to_owned()
    _ => "rest-empty"
  }
}

///| Codeunit length guards must not add char-length facts to later fallback arms.
fn classify_codeunit_length_fallback(s : String) -> String {
  match s {
    ['a' | 'A', 'b'] => "ab"
    [_, ch] => "two:" + ch.to_string()
    _ => "length-fallback-other"
  }
}

///|
fn classify_codeunit_eq_fallback(s : String) -> String {
  match s {
    ['a' | 'A'] => "a"
    [_] => "one"
    _ => "eq-fallback-other"
  }
}

///| Ranges that cross UTF-16 surrogates must not match one codeunit of a non-BMP char.
fn classify_surrogate_spanning_range(s : String) -> String {
  match s {
    ['\u{0}'..='\u{FFFF}', .. rest] => "bmp-range:" + rest.to_owned()
    _ => "bmp-range-other"
  }
}

///| StringView follows the same source-level pattern semantics.
fn classify_view(s : StringView) -> String {
  match s {
    ['e' | 'E', .. rest] => "view-head-e:" + rest.to_owned()
    [.. rest, 'x' | 'X'] => "view-tail-x:" + rest.to_owned()
    _ => "view-other"
  }
}

///| StringView codeunit fast paths must be relative to view.start/view.end.
fn classify_view_rest(s : StringView) -> String {
  match s {
    ['c', .. rest] => "view-rest-c:" + rest.to_owned()
    ['K' | 'Q', .. rest, '!'] => "view-rest-punct:" + rest.to_owned()
    _ => "view-rest-other"
  }
}

///|
fn main {
  println(classify_head("error"))
  println(classify_head("Xray"))
  println(classify_head("other"))
  println(classify_after_prefix("0xAf"))
  println(classify_after_prefix("0B101"))
  println(classify_after_prefix("07"))
  println(classify_tail("case"))
  println(classify_tail("unix"))
  println(classify_tail("done"))
  println(classify_mixed_width("E🤣!"))
  println(classify_mixed_width("😂e!"))
  println(classify_mixed_width("e!"))
  println(classify_wide_suffix_guard("b😀a"))
  println(classify_wide_suffix_guard("zzb😁a"))
  println(classify_wide_suffix_guard("bxa"))
  println(classify_codeunit_rest("cat"))
  println(classify_codeunit_rest("Kite!"))
  println(classify_codeunit_rest("Q!"))
  println(classify_codeunit_rest("😀x"))
  println(classify_codeunit_rest(""))
  println(classify_codeunit_length_fallback("ab"))
  println(classify_codeunit_length_fallback("xy"))
  println(classify_codeunit_length_fallback("😀"))
  println(classify_codeunit_eq_fallback("a"))
  println(classify_codeunit_eq_fallback("😀"))
  println(classify_surrogate_spanning_range("A"))
  println(classify_surrogate_spanning_range("😀"))
  println(classify_view("Error".view()))
  println(classify_view("suffixX".view()))
  println(classify_view("none".view()))
  println(classify_view_rest("😀cat😀".view(start_offset=2, end_offset=5)))
  println(classify_view_rest("😀Q!😀".view(start_offset=2, end_offset=4)))
}
