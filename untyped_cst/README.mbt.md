# moonbitlang/parser/untyped_cst

`moonbitlang/parser/untyped_cst` parses MoonBit source into an untyped concrete
syntax tree that preserves tokens, comments, delimiters, separators, and source
spans. It is intended for tooling that needs source-faithful structure before
or alongside the existing typed syntax AST.

Use `parse_structure(source, name?, enable_metavar?)` for a MoonBit source file
and `parse_expression(source, name?, enable_metavar?)` for a standalone
expression. Both return a `ParseResult`. Metavariable syntax is disabled by
default; pass `enable_metavar=true` to enable it.

- `parse_structure` returns a root whose kind is `Impls`.
- `parse_expression` returns a root whose kind is `Expression`.

Successful results can be lowered with `ParseResult::to_impls` and
`ParseResult::to_expr`, respectively. Calling a lowering method for the other
root kind returns a diagnostic.

## Source Text

CST nodes do not own the original source text. 

`CstNode.source_span` is a pair of UTF-16 code-unit offsets: an inclusive start
and an exclusive end. They can be used directly as `String`/`StringView`
slicing offsets. 

Parse source text with one of the two entry points so every node span refers to
the original source string.

## Note

The `untyped_cst` package must remain behaviorally equivalent to
`handrolled_parser`:

- For any input that `handrolled_parser` fails to parse, the CST parser must
  produce the same reports.
- For any input that `handrolled_parser` parses without reports, the CST parser
  must also produce no reports, and converting the resulting CST to an AST must
  produce an AST equal to the one produced by `handrolled_parser`.
