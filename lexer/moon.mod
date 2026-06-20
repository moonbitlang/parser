name = "moonbitlang/lexer"

version = "0.3.4"

import {
  "moonbitlang/yacc@0.7.13",
}

repository = "https://github.com/moonbitlang/parser"

readme = "README.md"

license = "Apache-2.0"

description = "Lexer for the MoonBit programming language"

options(
  "bin-deps": { "moonbitlang/yacc": "0.7.12" },
  exclude: [ "test", "extra_modules" ],
)