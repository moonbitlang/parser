///|
pub fn normalize(json : String) -> String {
  let json = @json.parse(json) catch {
    err => {
      println(err)
      abort(err.to_string())
    }
  }
  json.stringify(indent=2).trim_end(" ").to_string()
}

///|
let helper_code =
  #| fn @test.T::run(t : Self, name : String) -> Unit raise Error {
  $|   let source_file = "\{sources_path}/\\{name}.mbt"
  #|   let ast = @parser.parse_file(source_file, config=Handrolled) catch {
  #|     err => fail("failed to read file \{name}.mbt: \{err}")
  #|   }
  #|   t.write(ast |> @syntax.impls_to_json |> _.stringify(indent=2))
  #|   t.snapshot(filename="\{name}.mbt.json")
  #| }

///|
pub fn gen_test(name : String) -> String {
  (
    $|test "\{name}" (t : @test.T) {
    $|  t.run("\{name}")
    $|}
  )
}

///|
let root = "src/test/hand_parser_test"

///|
let sources_path : String = root + "/fixtures"

///|
let snapshot_path : String = root + "/__snapshot__"

///|
let output : String = root + "/hand_parser_test.mbt"

///|
fn main {
  try {
    let tests = @fs.read_dir(snapshot_path).filter(name => name.has_suffix(
      ".json",
    ))
    tests.each(name => {
      let target = snapshot_path + "/" + name
      let content = @fs.read_file_to_string(target) |> normalize
      @fs.write_string_to_file(target, content)
    })
    let buf = StringBuilder::new()
    buf.write_string(helper_code)
    for name in tests {
      buf
      ..write_string(gen_test(name.split(".").head().unwrap().to_string()))
      ..write_string("\n\n")
    }
    @fs.write_string_to_file(output, buf.to_string())
  } catch {
    err => println(err)
  }
}
