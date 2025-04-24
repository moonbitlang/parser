///|
pub enum LexicalError {
  IllegalCharacter(Char)
  UnterminatedString
  UnterminatedStringInVariableInterploation
  InterpInvalidMultilineString
  InterpInvalidComment
  InterpInvalidAttribute
  InterpMissingExpression
  InvalidEscapeSequence(String)
  InvalidDotInt(String)
  InvalidByteLiteral(String)
  Reserved_keyword(String)
}
