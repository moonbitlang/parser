///|
pub fn parse(source : String) -> String {
  @basic.show_loc.protect(true, () => {
    let mut i = 0
    let arr = FixedArray::make(source.length() * 4, b'\x00')
    for c in source {
      i += arr.set_utf8_char(i, c)
    }
    let bytes = Bytes::from_fixedarray(arr)
    let tokens = @lexer.tokens_from_bytes(bytes, comment=false).tokens
    let (impls, reports) = @hand_parser.parse(tokens)
    let json : Json = {
      "type": "Program",
      "body": Json::array(impls.map(ToJson::to_json).to_array()),
      "diagnostics": reports,
    }
    json.stringify(indent=2)
  })
}
