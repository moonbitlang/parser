name = "moonbitlang/parser"

version = "0.3.15"

import {
  "moonbitlang/x@0.4.39",
  "moonbitlang/lexer@0.3.14",
  "moonbitlang/moon_config@0.3.13",
  "moonbitlang/async@0.20.3",
  "moonbit-community/prettyprinter@0.4.10",
}

readme = "README.md"

repository = "https://github.com/moonbitlang/parser"

license = "Apache-2.0"

keywords = [ "parser", "lexer", "AST", "MoonBit" ]

description = "AST and parsers for the MoonBit programming language"

options(
  exclude: [ "test", "extra_modules" ],
)
