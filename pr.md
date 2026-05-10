# Preserve CST Token Locations in AST

## Summary

This PR extends the AST from a mostly semantic representation to one that also preserves key CST token locations. The added fields are mostly optional `Location?` values or `List[Location]`, recording parentheses, braces, commas, semicolons, keywords, operators, and other syntax tokens for formatting, source mapping, IDE features, and source-preserving rewrites.

The existing semantic JSON representation remains stable: `syntax/ast_json_repr.mbt` was updated to ignore these CST-only fields, so downstream users of the JSON AST do not see a semantic shape change.

## Main Changes

- `basic/loc.mbt`: adds `Location::first_char`, `Location::last_char`, and `Location::trim_first_and_last_char` for slicing single-character token locations out of larger source ranges.
- `syntax/ast.mbt`: adds CST location fields across types, expressions, patterns, declarations, and top-level impl nodes.
- `handrolled_parser` and `yacc_parser`: both MoonBit parsers now populate the new fields, including separator lists and trailing delimiter locations.
- `mbti_parser`: updates `.mbti` parsing to match the new AST constructors and preserve available token locations where possible.
- `syntax/iter_visitor.mbt`, `syntax/map_visitor.mbt`, `syntax/utils.mbt`, and `syntax/util/compact.mbt`: update visitors, construction helpers, and statement compaction so the new CST fields are preserved during traversal, rewriting, and statement folding.
- `test/sync_test/cst_loc_test.mbt`: adds parity tests that compare the semantic JSON output of the handrolled parser and MoonYacc parser, then assert the new CST location fields directly.

## AST and Syntax Mapping

### Types and Error Annotations

- `Type::Arrow`
  - New fields: `lparen_loc`, `rparen_loc`, `comma_locs`, `trailing_comma_loc`, `thin_arrow_loc`.
  - Syntax covered: function types such as `(A, B,) -> C`, `async (A) -> B`.
  - These fields preserve the argument list delimiters and the `->` token.

- `Type::Tuple`
  - New fields: `lparen_loc`, `rparen_loc`, `comma_locs`, `trailing_comma_loc`.
  - Syntax covered: tuple types such as `(A, B)` and `(A, B,)`.

- `Type::Name`
  - New fields: `lbracket_loc`, `rbracket_loc`, `comma_locs`, `trailing_comma_loc`.
  - Syntax covered: type arguments such as `Result[T, E,]`.

- `Type::Object`
  - Changed from `Object(ConstrId)` to `Object(constr_id, amp_loc)`.
  - Syntax covered: object types such as `&Thing`; `amp_loc` records the `&`.

- `ErrorType::MaybeError`
  - Changed to carry `question_loc`.
  - Syntax covered: `raise ?`; `question_loc` records the `?`.

### Parameters, Arguments, and Type Variables

- `Parameter`
  - Adds `colon_loc` to positional, labelled, discarded, optional, and question-optional parameters.
  - Adds `question_loc` and `equal_loc` to optional parameter forms.
  - Syntax covered: `_ : T`, `x : T`, `label~ : T`, `label? : T`, `label? : T = default`.

- `Argument` and `ConstrPatArg`
  - Adds `equal_loc`.
  - Syntax covered: labelled arguments or constructor pattern arguments such as `label=value`.

- `ConstrParam`
  - Adds `loc`, `mut_loc`, and `colon_loc`.
  - Syntax covered: enum constructor payloads such as `mut Int` and `label~ : String`.

- `TypeVarBinder`
  - Adds `colon_loc` and `plus_locs`.
  - Syntax covered: type parameter constraints such as `[T : Show + Eq]`.

### Expressions

- `Expr::Apply` and `Expr::DotApply`
  - Adds `bang_loc`, `question_loc`, `lparen_loc`, `rparen_loc`, `comma_locs`, and `trailing_comma_loc`.
  - Syntax covered: `f!(x)`, `f?(x)`, `f(a, b,)`, `recv.call!(arg)`, `recv.call?(arg)`.

- `Expr::Tuple`, `Expr::Array`, `Expr::ArraySpread`, `Expr::Map`, and `Expr::Group`
  - Adds delimiter fields for `(`, `)`, `[`, `]`, `{`, `}`, commas, and trailing commas.
  - Syntax covered: `(a, b)`, `(a, b,)`, `[a, ..b, c,]`, `{ "k": v }`, `(expr)`, `{ expr }`.

- `Expr::Record` and `Expr::RecordUpdate`
  - Adds `lbrace_loc`, `rbrace_loc`, `comma_locs`, `trailing_comma_loc`, and `dotdot_loc` for updates.
  - `FieldDef` also adds `colon_loc`.
  - Syntax covered: `{ x: y, z, }`, `{ ..record, x: y }`.

- `Expr::Constant`
  - Adds `uplus_loc` and `uminus_loc`.
  - Syntax covered: signed numeric literals such as `+1` and `-1`, while keeping the compact constant representation.

- `Expr::If`
  - Adds `if_loc`, `else_loc`, `if_lbrace_loc`, `if_rbrace_loc`, `else_lbrace_loc`, and `else_rbrace_loc`.
  - Syntax covered: `if cond { a } else if other { b } else { c }`.

- `Expr::While`
  - Adds `while_loc`, loop body brace locations, `nobreak_loc`, and nobreak block brace locations.
  - Syntax covered: `while cond { body } nobreak { fallback }`.

- `Expr::For` and `Expr::ForEach`
  - Adds keyword, delimiter, and separator locations including `for_loc`, `in_loc`, body braces, `nobreak` locations, binder commas, and header semicolons.
  - Syntax covered: `for i = init; cond; i = next { body } nobreak { fail }` and `for k, v in data { body }`.

- `WhereClause`
  - Adds `where_loc`, `lbrace_loc`, `rbrace_loc`, `comma_locs`, and `trailing_comma_loc`.
  - Syntax covered: `where { key: value, other: pun, }`.

- `Expr::Match` and `Expr::LexMatch`
  - Adds keyword and block brace locations.
  - `Case` and `LexCase` add `guard_keyword_loc` and `fat_arrow_loc`.
  - Syntax covered: `match v { p if g => e }` and `lexmatch value with longest { "a" => a }`.

- `Expr::Is` and `Expr::IsLexMatch`
  - Adds `is_loc`, `lexmatch_loc`, `question_loc`, and `with_loc`.
  - Syntax covered: `value is Some(_)`, `subject lexmatch? "a"`, `subject lexmatch? "a" with longest`.

- `Expr::Try` and `Expr::TryOperator`
  - Adds locations for `try`, `catch`, `catch!`, catch braces, `noraise`, noraise braces, `try?`, and `try!`.
  - Syntax covered: `try work catch! { err => bad } noraise { ok => good }`, `try? work`, `try! work`.

- `Expr::Guard`, `Expr::Defer`, `Expr::Let`, `Expr::LetMut`, `Expr::LetFn`, `Expr::LetRec`, `Expr::LetAnd`, and `Expr::Sequence`
  - Adds keyword, `=`, `;`, and sequence semicolon locations.
  - Syntax covered: block statements such as `let value = init;`, `let mut value = init;`, `fn inner() { ... };`, `letrec ...;`, `guard ... else { ... };`, `defer cleanup;`, and expression sequences.

- `Func`
  - Adds `fn_loc`, body brace locations, body semicolon locations, and trailing body semicolon location.
  - Syntax covered: local and top-level function bodies such as `fn f() { prep; done; }`.

### Patterns

- `Pattern::Array`
  - Adds bracket, comma, trailing comma, and `dotdot_loc`.
  - `ArrayPattern::StringSpread`, `BytesSpread`, and `ConstSpread` also add `dotdot_loc`.
  - Syntax covered: `[head, ..rest]`, `[.. "s"]`, `[..b"s"]`, `[..SomeConst]`.

- `Pattern::Constr`
  - Adds paren, comma, trailing comma, and `dotdot_loc`.
  - Syntax covered: `Ctor(a, label=b, ..)`.

- `Pattern::Tuple`, `Pattern::Record`, and `Pattern::Map`
  - Adds delimiter, comma, trailing comma, and open-pattern `..` locations.
  - `FieldPat` adds `colon_loc`; `MapPatElem` adds `question_loc` and `colon_loc`.
  - Syntax covered: `(a, b)`, `{ field: pat, .. }`, `{ "k"?: v, .. }`.

- `Pattern::Range`
  - Adds `op_loc`.
  - Syntax covered: `1..=3`, `1..<3`.

- `Pattern::Constant`
  - Adds `uminus_loc`.
  - Syntax covered: negative constant patterns.

### Declarations and Top-Level Items

- `DeclBody`
  - Adds body brace locations, semicolon locations, and trailing semicolon location.
  - Syntax covered: declaration bodies such as `{ prep; done; }`.

- `FunDecl`
  - Adds `declare_loc`, `extern_loc`, `fn_loc`, type parameter delimiters and commas, parameter delimiters and commas, trailing parameter comma, and `thin_arrow_loc`.
  - Syntax covered: `fn [T, U,] f(x : Int, y : Int,) -> Int`, `declare fn [T,] f(...) -> T`, `extern "js" fn foreign(...) -> T`.

- `TraitMethodDecl`
  - Adds type parameter, parameter, `->`, and `=` locations.
  - Syntax covered: `method[T, U,](x : Int, y : Int,) -> Int = _`.

- `TypeDecl` and `LocalTypeDecl`
  - Adds keyword locations, type parameter delimiter and comma locations, `=`, body delimiters for record/variant/tuple struct forms, and `derive(...)` delimiters and commas.
  - Syntax covered: `type Alias[T, U,] = Int derive(Eq, Show)`, `struct Pair[T, U,](Int, Bool,) derive(Eq, Show)`, `enum Demo { ... }`.

- `TraitDecl`
  - Adds `declare_loc`, `trait_loc`, `colon_loc`, `plus_locs`, and trait body brace locations.
  - Syntax covered: `trait Fancy : Eq + Show { ... }`.

- `Impl::TopExpr`, `TopTest`, `TopTypeDef`, `TopFuncDef`, `TopLetDef`, `TopTrait`, `TopImpl`, `TopView`, `TopImplRelation`, and `TopUsing`
  - Adds top-level `trailing_semi_loc`.
  - Specific variants also record declaration keywords and header delimiters:
    - `TopImpl`: `impl`, type parameters, `for`, `with`, method params, and `->`.
    - `TopView`: `enumview`, type parameters, constructor braces, `for`, `with`, params.
    - `TopImplRelation`: `declare`, `impl`, type parameters, `for`.
    - `TopUsing`: `using`, braces, commas, and trailing comma.

## Parser Implementation Notes

- The handrolled parser now has `series_with_follow_info` and `surround_series_info` helpers that return parsed items, separator locations, and optional trailing delimiter location.
- The MoonYacc grammar adds corresponding `*_info` productions for comma-separated and semicolon-separated lists.
- Statement compaction now attaches semicolon locations before folding statements into `Let`, `LetMut`, `LetFn`, `LetRec`, `LetAnd`, `Guard`, `Defer`, or `Sequence`.
- `set_impl_trailing_semi` records top-level trailing semicolons after parsing each top-level item.

## Validation

Ran:

```bash
moon test test/sync_test/cst_loc_test.mbt
```

Result:

```text
Total tests: 7, passed: 7, failed: 0.
```

The tests cover handrolled parser and MoonYacc parser parity, verify that their semantic JSON AST output remains equal, and assert the newly preserved CST token locations directly.
