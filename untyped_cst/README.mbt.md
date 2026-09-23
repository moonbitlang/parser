# moonbitlang/parser/untyped_cst

`moonbitlang/parser/untyped_cst` parses MoonBit source into an untyped concrete
syntax tree that preserves tokens, comments, delimiters, separators, and source
spans. Its public entry points provide parse diagnostics and conversion to the
existing typed syntax AST.

Use `parse_structure(source, name?, enable_metavar?)` for a MoonBit source file
and `parse_expression(source, name?, enable_metavar?)` for a standalone
expression. Both return a `ParseResult`. Metavariable syntax is disabled by
default; pass `enable_metavar=true` to enable it.

- `parse_structure` returns a root whose kind is `Impls`.
- `parse_expression` returns a root whose kind is `Expression`.

Successful results can be lowered with `ParseResult::to_impls` and
`ParseResult::to_expr`, respectively. Calling a lowering method for the other
root kind returns a diagnostic.

## Node Model

The source-independent node model lives in
`moonbitlang/parser/untyped_cst/internal/node`. MoonBit restricts imports of
this package to `untyped_cst` and its subpackages. Code within that subtree can
use `CstNode`, `NodeKind`, `NodePayload`, and the node traversal, payload, and
classification helpers.

`NodeKind` is public but read-only outside the node package. Inspect it with
pattern matching or its classification methods. Construct nodes with the
corresponding labelled `CstNode` constructor instead of constructing a
`NodeKind` value or using a generic builder. For example:

```mbt check
///|
fn constructor_example() -> Unit {
  let pos = @basic.Position::{ fname: "example.mbt", lnum: 1, bol: 0, cnum: 0, }
  let loc = @node.empty_loc(pos)
  let span = @node.empty_span(0)
  let name = @node.CstNode::name_identifier(value="x", loc~, source_span=span)
  let ident = @node.CstNode::expr_ident(loc~, source_span=span, name=[name])
  guard ident.kind is @node.NodeKind::Expr_Ident else {
    abort("unexpected node kind")
  }
}
```

There is one such `CstNode::<snake_case_kind>` constructor for every
`NodeKind`. All constructor fields are labelled. Ordinary structural kinds use
required, optional, or repeated child arguments. Roots, delimited lists,
declaration prefixes, token metadata, multiline sequences, and recovery paths
use typed ordered child-entry enums where child order is semantically relevant.
Scalar constructors take their exact `String`, `Int`, or `Bool` value directly;
`leaf` takes `NodePayload`.

Lexical nodes use `Syntax_Token(TokenKind)` or `Syntax_Unexpected(TokenKind)`.
`NodeKind::token_kind()` returns the original lexical kind for both, while
`is_syntax_token()` identifies either state. `TokenKind` is re-exported from the
lexer by the node package. Build tokens with `CstNode::syntax_token` or
`syntax_unexpected`, passing `token_kind~`, `loc~`, `source_span~`, and ordered
`CstTokenChild` entries. Payloads and repeated semantic children retain their
order, including after error recovery.

Nodes retain `loc` and `source_span`; there are no additional location children.
AST locations are computed during lowering from tokens and syntax boundaries,
without reading source text or re-parsing it.

The `untyped_cst` package re-exports the three node types for in-subtree code.
MoonBit's internal visibility rules prevent outside packages from inspecting
or constructing these types through the re-exports. Outside callers can use
`ParseResult::diagnostics_view`, `ParseResult::to_impls`, and
`ParseResult::to_expr`.

## Package Organization

`untyped_cst` owns the public parsing entry points and `ParseResult`, including
its AST conversion methods. Implementation details live in four internal
packages:

- `internal/node` defines CST nodes and their constructors and helpers.
- `internal/construction` builds CST nodes and their semantic children.
- `internal/parser` handles token streams, parsing, and error recovery.
- `internal/lower` converts CST nodes to the syntax AST and attaches docstrings.

The parser depends on construction helpers. Lowering reads the node model
directly and does not depend on the parser. Outside callers import
`untyped_cst` for parsing and AST conversion.

## Source Text

CST nodes do not own the original source text.

Within `untyped_cst`, `CstNode.source_span` is a pair of UTF-16 code-unit
offsets: an inclusive start and an exclusive end. They can be used directly as
`String`/`StringView` slicing offsets.

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
