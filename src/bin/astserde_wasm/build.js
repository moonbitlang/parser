const esbuild = require('esbuild')

esbuild.build({
  entryPoints: ['../../../target/wasm-gc/release/build/bin/astserde_wasm/astserde_wasm.wasm'],
  outfile: 'astserde.js',
  loader: { '.wasm': 'binary' },
  write: false, 
  format: 'esm',
}).then(result => {
  const text = result.outputFiles[0].text
  require('fs').writeFileSync('astserde.js', text)
})