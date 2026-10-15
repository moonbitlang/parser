# Repository corpus tests

These tests parse every `.mbt` file in a repository with Handrolled, MoonYacc,
and untyped CST (including lowering). All parsers must produce no diagnostics
and equivalent ASTs, including source locations. The corpus includes test and
generated sources, excluding `.git`, `.mooncakes`, `_build`, `target`, and
`node_modules` directories.

External corpus tests are skipped by default. From the parser repository root,
run one checkout with:

```bash
REPO_ROOT=/path/to/core REPO_NAME=moonbitlang/core \
  moon test test/repo_test --target native --release --include-skipped
```

`REPO_ROOT` is required and must contain at least one `.mbt` file. `REPO_NAME` is
an optional label for logs; it defaults to `REPO_ROOT`. Check out submodules
recursively when preparing a corpus.

Stable and nightly CI each run an independent matrix for `moonbitlang/openseek`,
`moonbitlang/core`, `moonbitlang/x`, `moonbitlang/typst.mbt`,
`moonbitlang/office.mbt`, `moonbitlang/async`, and `moonbitlang/moonyacc`.
Each run checks the default branch and logs the actual commit and submodule
revisions.
