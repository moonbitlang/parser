///|
fn main {
  let mut input_path = ""
  let mut output_path = None
  @basic.show_loc.val = true
  @args.parse(
    [
      (
        "--output",
        "-o",
        String(path => output_path = Some(path)),
        "output json path",
      ),
    ],
    path => input_path = path,
    "",
    @env.args(),
  )
  if @fs.path_exists(input_path) &&
    (try? @fs.read_file_to_bytes(input_path)) is Ok(source) {
    let tokens = lex(source)
    let (impls, diagnostics) = @hand_parser.parse(tokens)
    let json = impls2json(impls)
    match (output_path, diagnostics) {
      (Some(dst), _) => {
        println(diagnostics)
        @fs.write_string_to_file(dst, json.stringify(indent=2)) catch {
          IOError(str) => println(str)
        }
      }
      (None, []) => println(json.stringify(indent=2))
      (None, _) => println(diagnostics)
    }
  }
}

///|
pub fn parse(source : String) -> (String, String) {
  let mut i = 0
  let arr = FixedArray::make(source.length() * 4, b'\x00')
  for c in source {
    i += arr.set_utf8_char(i, c)
  }
  let bytes = Bytes::from_fixedarray(arr)
  let tokens = lex(bytes)
  let (impls, diagnostics) = @hand_parser.parse(tokens)
  (
    impls2json(impls).stringify(indent=2),
    diagnostics.map(Show::to_string).fold(init="", (a, b) => a + b + "\n"),
  )
}

///|
fn lex(
  utf8 : Bytes,
) -> Array[(@tokens.Token, @basic.Position, @basic.Position)] {
  @lexer.tokens_from_bytes(utf8, comment=false).tokens
}

///|
fn impls2json(impls : @syntax.Impls) -> Json {
  {
    "type": "Program",
    "body": Json::array(impls.map(ToJson::to_json).to_array()),
  }
}
