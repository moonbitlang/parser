///|
fn compress_path(path : String) -> String {
  path.replace_all(old="/", new="_")
}

///|
test "parse core/cmp/cmp.mbti" (it : @test.T) {
  let path = "core/cmp/cmp.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/bench/bench.mbti" (it : @test.T) {
  let path = "core/bench/bench.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/abort/abort.mbti" (it : @test.T) {
  let path = "core/abort/abort.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/unit/unit.mbti" (it : @test.T) {
  let path = "core/unit/unit.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/uint64/uint64.mbti" (it : @test.T) {
  let path = "core/uint64/uint64.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/strconv/strconv.mbti" (it : @test.T) {
  let path = "core/strconv/strconv.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/ref/ref.mbti" (it : @test.T) {
  let path = "core/ref/ref.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/result/result.mbti" (it : @test.T) {
  let path = "core/result/result.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/sorted_set/sorted_set.mbti" (it : @test.T) {
  let path = "core/sorted_set/sorted_set.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/bigint/bigint.mbti" (it : @test.T) {
  let path = "core/bigint/bigint.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/uint/uint.mbti" (it : @test.T) {
  let path = "core/uint/uint.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/priority_queue/priority_queue.mbti" (it : @test.T) {
  let path = "core/priority_queue/priority_queue.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/char/char.mbti" (it : @test.T) {
  let path = "core/char/char.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/int16/int16.mbti" (it : @test.T) {
  let path = "core/int16/int16.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/test/test.mbti" (it : @test.T) {
  let path = "core/test/test.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/bytes/bytes.mbti" (it : @test.T) {
  let path = "core/bytes/bytes.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/hashset/hashset.mbti" (it : @test.T) {
  let path = "core/hashset/hashset.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/tuple/tuple.mbti" (it : @test.T) {
  let path = "core/tuple/tuple.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/array/array.mbti" (it : @test.T) {
  let path = "core/array/array.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/immut/sorted_set/sorted_set.mbti" (it : @test.T) {
  let path = "core/immut/sorted_set/sorted_set.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/immut/priority_queue/priority_queue.mbti" (it : @test.T) {
  let path = "core/immut/priority_queue/priority_queue.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/immut/hashset/hashset.mbti" (it : @test.T) {
  let path = "core/immut/hashset/hashset.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/immut/array/array.mbti" (it : @test.T) {
  let path = "core/immut/array/array.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/immut/internal/sparse_array/sparse_array.mbti" (it : @test.T) {
  let path = "core/immut/internal/sparse_array/sparse_array.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/immut/internal/path/path.mbti" (it : @test.T) {
  let path = "core/immut/internal/path/path.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/immut/sorted_map/sorted_map.mbti" (it : @test.T) {
  let path = "core/immut/sorted_map/sorted_map.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/immut/hashmap/hashmap.mbti" (it : @test.T) {
  let path = "core/immut/hashmap/hashmap.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/immut/list/list.mbti" (it : @test.T) {
  let path = "core/immut/list/list.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/sorted_map/sorted_map.mbti" (it : @test.T) {
  let path = "core/sorted_map/sorted_map.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/option/option.mbti" (it : @test.T) {
  let path = "core/option/option.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/math/math.mbti" (it : @test.T) {
  let path = "core/math/math.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/hashmap/hashmap.mbti" (it : @test.T) {
  let path = "core/hashmap/hashmap.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/env/env.mbti" (it : @test.T) {
  let path = "core/env/env.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/deque/deque.mbti" (it : @test.T) {
  let path = "core/deque/deque.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/int/int.mbti" (it : @test.T) {
  let path = "core/int/int.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/json/json.mbti" (it : @test.T) {
  let path = "core/json/json.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/float/float.mbti" (it : @test.T) {
  let path = "core/float/float.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/list/list.mbti" (it : @test.T) {
  let path = "core/list/list.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/rational/rational.mbti" (it : @test.T) {
  let path = "core/rational/rational.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/bool/bool.mbti" (it : @test.T) {
  let path = "core/bool/bool.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/queue/queue.mbti" (it : @test.T) {
  let path = "core/queue/queue.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/byte/byte.mbti" (it : @test.T) {
  let path = "core/byte/byte.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/double/internal/ryu/ryu.mbti" (it : @test.T) {
  let path = "core/double/internal/ryu/ryu.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/double/double.mbti" (it : @test.T) {
  let path = "core/double/double.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/string/string.mbti" (it : @test.T) {
  let path = "core/string/string.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/set/set.mbti" (it : @test.T) {
  let path = "core/set/set.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/prelude/prelude.mbti" (it : @test.T) {
  let path = "core/prelude/prelude.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/random/internal/random_source/random_source.mbti" (
  it : @test.T,
) {
  let path = "core/random/internal/random_source/random_source.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/random/random.mbti" (it : @test.T) {
  let path = "core/random/random.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/int64/int64.mbti" (it : @test.T) {
  let path = "core/int64/int64.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/uint16/uint16.mbti" (it : @test.T) {
  let path = "core/uint16/uint16.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/error/error.mbti" (it : @test.T) {
  let path = "core/error/error.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/builtin/builtin.mbti" (it : @test.T) {
  let path = "core/builtin/builtin.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/coverage/coverage.mbti" (it : @test.T) {
  let path = "core/coverage/coverage.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/buffer/buffer.mbti" (it : @test.T) {
  let path = "core/buffer/buffer.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/quickcheck/quickcheck.mbti" (it : @test.T) {
  let path = "core/quickcheck/quickcheck.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}

///|
test "parse core/quickcheck/splitmix/splitmix.mbti" (it : @test.T) {
  let path = "core/quickcheck/splitmix/splitmix.mbti"
  let content = @fs.read_file_to_bytes(path)
  let lex_result = @lexer.tokens_from_bytes(name=path, content, comment=false)
  let mbti = @mbti_parser.mbti(
    lex_result.tokens.filter(fn(triple) {
      not(triple.0 is (NEWLINE | COMMENT(_)))
    }),
    initial_pos=@basic.Position::{ fname: path, lnum: 1, bol: 0, cnum: 0 },
  )
  it.writeln(mbti.to_json().stringify(indent=2))
  it.snapshot(filename=compress_path(path) + ".json")
}
