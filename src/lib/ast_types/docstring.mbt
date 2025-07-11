///|
pub(all) struct DocString {
  /// The docstring content.
  content : @list.T[String]
  /// The location of the docstring in the source code.
  loc : Location
} derive(ToJson)
