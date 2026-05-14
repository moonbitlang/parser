# mq (Wasm)

This is the WASI wasm build of `mq` for parsing MoonBit configuration DSL
files. It lives at `moonbitlang/parser/cmd/wasm/mq` and uses
`moonbit-community/miniio`.

The `legacy` subcommand prints the post-processed JSON form that is compatible
with the old JSON configuration format.

Run commands from the `moonbitlang/parser` module root:

```bash
moon build --target wasm cmd/wasm/mq
moon run --target wasm cmd/wasm/mq legacy moon.pkg
moon run --target wasm cmd/wasm/mq legacy moon.mod
moon run --target wasm cmd/wasm/mq legacy moon.work
moon run --target wasm cmd/wasm/mq legacy --file-type mod -c 'name = "demo/mod"'
cat moon.mod | moon run --target wasm cmd/wasm/mq legacy - --file-type mod
```

When reading from stdin or `-c/--code`, pass `--file-type pkg`,
`--file-type mod`, or `--file-type work` to the `legacy` subcommand.

Moon names the raw build artifact from the package directory, so this package
builds as `mq.wasm`.

Run it directly with `moonrun`:

```bash
moonrun mq.wasm legacy moon.pkg
moonrun mq.wasm legacy --file-type mod -c 'name = "demo/mod"'
cat moon.mod | moonrun mq.wasm legacy - --file-type mod
```

Or with `wasmtime`:

```bash
wasmtime run --dir . mq.wasm legacy moon.pkg
wasmtime run mq.wasm legacy --file-type mod -c 'name = "demo/mod"'
cat moon.mod | wasmtime run mq.wasm legacy - --file-type mod
```

Under WASI, `-o` can only write to paths inside directories preopened by the
runtime.
