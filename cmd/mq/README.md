# mq

`mq` is the native command for parsing MoonBit configuration DSL files. It
lives at `moonbitlang/parser/cmd/mq` and uses `moonbitlang/async`.

The WASI wasm package is documented separately in `cmd/mq_wasm`.

The `legacy` subcommand prints the post-processed JSON form that is compatible
with the old JSON configuration format.

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

install:

```bash
moon install moonbitlang/parser/cmd/mq
```
