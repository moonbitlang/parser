# mq

`mq` parses MoonBit configuration DSL files. The package lives at
`moonbitlang/parser/cmd/mq`, uses `moonbitlang/async`, and supports both the
native and WASI wasm backends.

The `legacy` subcommand prints the post-processed JSON form that is compatible
with the old JSON configuration format.

## Native

```bash
mq legacy moon.pkg
mq legacy moon.mod
mq legacy moon.work
mq legacy moon.pkg -o moon.pkg.json
mq legacy --file-type mod -c 'name = "demo/mod"'
cat moon.work | mq legacy - --file-type work
```

When reading from stdin or `-c/--code`, pass `--file-type pkg`,
`--file-type mod`, or `--file-type work` to the `legacy` subcommand.

Install the native command with:

```bash
moon install moonbitlang/parser/cmd/mq
```