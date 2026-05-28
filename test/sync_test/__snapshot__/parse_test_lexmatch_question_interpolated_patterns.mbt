fn main {
  let x = 1
  let _ = s lexmatch? re"a\{x}"
  let _ = s lexmatch? "a\{x}"
  // Expected: this parses. Regex interpolation is accepted here only because
  // `re"a\{"}"}"` is a lexmatch pattern, not an ordinary regex expression.
  // The outer interpolation AST is `Pexpr_interp`, whose expression part is
  // `Pexpr_is_lexmatch` with a `Plpat_regex_interp` pattern. The `}` inside
  // the regex interpolation belongs to the regex; the final `}` closes the
  // outer string interpolation.
  let _ = "outer \{s lexmatch? re"a\{"}"}" }"
}
