# Imported tests

Fixtures are copied intact from the upstream OCaml implementation. Expected ASTs
and tokens must be exported from upstream, not edited to match this parser.

Snapshots are checked in. After adding or removing fixtures, regenerate the
MoonBit test runners and run the sync tests:

```sh
moon run test/sync_test/generator
moon test test/sync_test
```

To refresh expected output, use the upstream build's
`_build/default/bin/moon_objinfo.exe` with `-dump-ast` or `-dump-tokens`.
AST fixtures must have both `has_parse_error=false` and
`has_deprecated_syntax=false`. An empty `.exp.error` file is not sufficient.
Syntax lint warnings do not exclude otherwise valid fixtures. The parse-test
range patterns and regex interpolation fixture export tokens only; unterminated
regexes belong in lexer/recovery tests.

Both `.mbt` and `.mbtx` sources retain their upstream extension and complete
contents. When exporting `.mbtx` ASTs, mask the header with spaces in a temporary
`.mbt` input, preserving line endings and body positions. Export tokens from the
body separately and shift their line spans to the original file, retaining the
correct leading-comment metadata. The runner uses `parse_mbtx`, parses the
header and compares all three body parsers.
Reading raw `.mbtx` with `mooninfo -dump-ast` is not a substitute for this step.

Token snapshots retain the historical `STAR` → `["INFIX3", "*"]` compatibility
normalization. When refreshing AST snapshots, restore the redundant singleton
`TryOperatorKind::Exclamation` field that upstream removed, preserving this
library's existing public JSON schema and older snapshots. These are explicit
schema adapters; neither changes source syntax or parser diagnostics.
Refresh only the intended fixtures. The runner generator discovers their
`.mbt`/`.mbtx` sources without rewriting unrelated snapshot files.

The `format_test_block_line_doc`, `format_test_or_alias_pattern`,
`format_test_enumview`, `format_test_extend`, `format_test_pattern_guard_comment`,
and `format_test_where_clause` fixtures come from the correspondingly named
files in `../ideas/test/format_test/`. Their complete original sources and
fresh `moon_objinfo.exe -dump-ast` / `-dump-tokens` exports cover block-line UUID
documentation, multiline aliases, declaration boundaries, and match/where
comments. All six AST exports have both exclusion flags set to `false`.
