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
    lexscan str with longest {
      (re"^(\(|左括号)", after=rest) => {
        tokens.push(LParen)
        continue rest
      }
      (re"^(\)|右括号)", after=rest) => {
        tokens.push(RParen)
        continue rest
      }
      (re"^[0-9]+" as s, after=rest) => {
        tokens.push(Token::Number(@string.parse_int(s)))
        continue rest
      }
      (re"^[a-zA-Z0-9_]+" as s, after=rest) => {
        tokens.push(Token::String(s.to_owned()))
        continue rest
      }
      // skip whitespace
      (re"^[ \t\n\r]+", after=rest) => continue rest
      re"^$" => break
      _ => fail("Invalid token \{str}")
    }
  }
  tokens
}


fn main {
  debug(to_result(() => lex_token("   (1 2 (a b) (2) 3)")))
  debug(to_result(() => lex_token("   (1 2 左括号a 右括号b) (2) 3)")))
  if to_result(() => lex_token("?1")) is Ok(_) {
    panic()
  }
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
