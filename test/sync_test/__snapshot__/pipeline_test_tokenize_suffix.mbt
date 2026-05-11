

fn lex_suffix(str : StringView) -> StringView raise {
  lexmatch str with longest {
    (".*" "\." (".*$" as ext), _) => ext
    _ => fail("Invalid suffix")
  }  
}

fn main {
  debug(try? lex_suffix("test.txt"))
  debug(try? lex_suffix("test.txt.tar.gz"))
  debug(try? lex_suffix("test.txt.tar.gz.tar.gz"))
  debug(try? lex_suffix("test.txt.tar.gz.tar.gz.tar.gz"))
  debug(try? lex_suffix("test.txt.tar.gz.tar.gz.tar.gz.tar.gz"))
  debug(try? lex_suffix("test.txt.tar.gz.tar.gz.tar.gz.tar.gz.tar.gz"))
  debug(try? lex_suffix("test.txt.tar.gz.tar.gz.tar.gz.tar.gz.tar.gz"))
}
