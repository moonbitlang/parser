///|
pub fn gen_test(name : String) -> String {
  (
    $|test "\{name}" (t : @test.T) {
    $|  t.run("\{name}")
    $|}
  )
}

///|
let root = "test/mbti_parser_test"

///|
let fixtures_path : String = root + "/fixtures"

///|
let output : String = root + "/generated_test.mbt"

///|
let generated_test_separator =
  #|
  #|
  #|

///|
fn main {
  try {
    let tests = @fs.read_dir(fixtures_path).filter(name => {
      name.has_suffix(".mbti")
    })
    let buf = StringBuilder()
    for name in tests {
      buf
      ..write_string(gen_test(name.split(".").head().unwrap().to_owned()))
      .write_string(generated_test_separator)
    }
    @fs.write_string_to_file(output, buf.to_string())
  } catch {
    err => println(err)
  }
}
