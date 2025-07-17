#!/usr/bin/env bash

set -euo pipefail

CORE_PATH="core"
TEST_FILE="src/mbti_parser/mbti_parser_test.mbt"

gen_compress_path() {
cat <<EOF
fn compress_path(path : String) -> String {
  path.replace_all(old="/", new="_")
}
EOF
}


gen_test() {
cat <<EOF
test "parse $1" (it : @test.T) {
  let path = "$1"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@syntax.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}
EOF
}

git submodule update --remote

gen_compress_path > "$TEST_FILE"

find "$CORE_PATH" -type f -name '*.mbti' | while read -r file; do
  gen_test "$file" >> "$TEST_FILE"
done

moon fmt