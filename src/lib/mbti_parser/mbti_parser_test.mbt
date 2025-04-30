///|
fn iter_mbti_files(dir : String, f : (String) -> Unit!) -> Unit! {
  for entry in @fs.read_dir!(dir) {
    let path = "\{dir}/\{entry}"
    if @fs.is_dir!(path) {
      iter_mbti_files!(path, f)
    } else if @fs.is_file!(path) {
      if path.has_suffix(".mbti") {
        f!(path)
      }
    }
  }
}

///|
test "parse core/**/*.mbti" (it : @test.T) {
  let map = {}
  iter_mbti_files!("core", fn(path) {
    let content = @fs.read_file_to_bytes!(path)
    let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
    let mbti = @mbti_parser.mbti!(
      lex_result.tokens.filter(fn(triple) {
        not(triple.0 is (NEWLINE | COMMENT(_)))
      }),
      initial_pos=Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
    )
    map[path] = mbti
  })
  it.writeln(map.to_json().stringify(indent=2))
  it.snapshot!(filename="core_mbtis.json")
}
