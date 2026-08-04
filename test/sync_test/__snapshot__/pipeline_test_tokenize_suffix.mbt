

fn lex_suffix(str : StringView) -> StringView raise {
  lexscan str with longest {
    (re"^.*" + re"\." + (re".*$" as ext)) => ext
    _ => fail("Invalid suffix")
  }  
}

fn main {
  debug(to_result(() => lex_suffix("test.txt")))
  debug(to_result(() => lex_suffix("test.txt.tar.gz")))
  debug(to_result(() => lex_suffix("test.txt.tar.gz.tar.gz")))
  debug(to_result(() => lex_suffix("test.txt.tar.gz.tar.gz.tar.gz")))
  debug(to_result(() => lex_suffix("test.txt.tar.gz.tar.gz.tar.gz.tar.gz")))
  debug(to_result(() => lex_suffix("test.txt.tar.gz.tar.gz.tar.gz.tar.gz.tar.gz")))
  debug(to_result(() => lex_suffix("test.txt.tar.gz.tar.gz.tar.gz.tar.gz.tar.gz")))
  if to_result(() => lex_suffix("txt")) is Ok(_) {
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
