name = "moonbitlang/parser"

version = "0.3.4"

import {
  "moonbitlang/x@0.4.39",
  "moonbitlang/yacc@0.7.13",
  "moonbit-community/miniio@0.1.0",
  "moonbitlang/async@0.19.0",
  "moonbit-community/prettyprinter@0.4.10",
}

readme = "README.md"

repository = "https://github.com/moonbitlang/parser"

license = "Apache-2.0"

keywords = [ "parser", "lexer", "AST", "MoonBit" ]

description = "AST and parsers for the MoonBit programming language"

options(
  "bin-deps": { "moonbitlang/yacc": "0.7.12" },
  exclude: [ "test", "extra_modules" ],
)
