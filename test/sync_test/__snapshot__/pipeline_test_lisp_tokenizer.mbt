///|
enum Token {
  LParen
  RParen
  Number(Int)
  String(String)
} derive(Debug)

///|
/// issues: when regex has a syntax error,
/// there is no error recovery even format can not work
#warnings("-71")
fn lex_token(str : StringView) -> Array[Token] raise {
  let tokens : Array[Token] = []
  for str = str {
    lexmatch str with longest {
      ("\(|左括号", rest) => {
        tokens.push(LParen)
        continue rest
      }
      ("\)|右括号", rest) => {
        tokens.push(RParen)
        continue rest
      }
      ("[0-9]+" as s, rest) => {
        tokens.push(Token::Number(@string.parse_int(s)))
        continue rest
      }
      ("[a-zA-Z0-9_]+" as s, rest) => {
        tokens.push(Token::String(s.to_owned()))
        continue rest
      }
      // skip whitespace
      ("[ \t\n\r]+", rest) => continue rest
      "" => break
      _ => fail("Invalid token \{str}")
    }
  }
  tokens
}


fn main {
  debug(try? lex_token("   (1 2 (a b) (2) 3)"))
  debug(try? lex_token("   (1 2 左括号a 右括号b) (2) 3)"))
}
