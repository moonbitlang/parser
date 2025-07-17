///|
pub(all) struct DocString {
  /// The docstring content.
  content : @list.T[String]
  /// The location of the docstring in the source code.
  loc : Location
} derive(ToJson)

///|
pub fn DocString::empty() -> DocString {
  let dummy_pos = Position::{ lnum: 0, cnum: 0, bol: 0, fname: "" }
  { content: @list.empty(), loc: { start: dummy_pos, end: dummy_pos } }
}
