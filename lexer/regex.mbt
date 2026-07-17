///|
const RE_LINE_BREAK : Regex = re"\n|\r|\r\n|\u2028|\u2029"

///|
const RE_ASCII_LINE_BREAK : Regex = re"[\r\n]"

///|
const RE_UNICODE_SPACES : Regex = re"[\u0009\u000B\u000C\u0020\u00A0\uFEFF\u1680\u2000-\u200A\u202F\u205F\u3000]*"

///|
const RE_UNICODE_SPACES1 : Regex = re"[\u0009\u000B\u000C\u0020\u00A0\uFEFF\u1680\u2000-\u200A\u202F\u205F\u3000]+"

///|
const RE_LINE_COMMENT : Regex = re"//[^\r\n]*"

///|
const RE_CHAR_QUOTE : Regex = re"'"

///|
const RE_CHAR_CONTENT : Regex = re"[^\\\n\r']"

///|
const RE_CHAR_ESCAPE : Regex = re"\\[\\'\"ntbrf/ ]"

///|
const RE_STRING_ESCAPE : Regex = re"\\[\\'\"`ntbrf/ ]"

///|
const RE_HEX_ESCAPE : Regex = re"\\x[0-9a-fA-F]{2}"

///|
const RE_OCTAL_ESCAPE : Regex = re"\\o[0-3][0-7]{2}"

///|
const RE_SHORT_UNICODE_ESCAPE : Regex = re"\\u[0-9a-fA-F]{4}"

///|
const RE_BRACED_UNICODE_ESCAPE : Regex = re"\\u[{][0-9a-fA-F]+[}]"

///|
const RE_DEC_DIGITS : Regex = re"[0-9](_|[0-9])*"

///|
const RE_HEX_DIGITS : Regex = re"[0-9a-fA-F](_|[0-9a-fA-F])*"

///|
const RE_OCTAL_DIGITS : Regex = re"[0-7](_|[0-7])*"

///|
const RE_BINARY_DIGITS : Regex = re"[01](_|[01])*"

///|
const RE_HEX_INTEGER : Regex = re"0[xX]" + RE_HEX_DIGITS

///|
const RE_OCTAL_INTEGER : Regex = re"0[Oo]" + RE_OCTAL_DIGITS

///|
const RE_BINARY_INTEGER : Regex = re"0[Bb]" + RE_BINARY_DIGITS

///|
const RE_INTEGER_SUFFIX : Regex = re"(UL|U|L|N)?"

///|
const RE_INTEGER : Regex = (
    RE_DEC_DIGITS | RE_HEX_INTEGER | RE_OCTAL_INTEGER | RE_BINARY_INTEGER
  ) +
  RE_INTEGER_SUFFIX

///|
const RE_DEC_EXPONENT : Regex = re"([eE][+\-]?[0-9](_|[0-9])*)?"

///|
const RE_HEX_EXPONENT : Regex = re"([pP][+\-]?[0-9](_|[0-9])*)?"

///|
const RE_DEC_DOUBLE : Regex = RE_DEC_DIGITS + re"\.[0-9_]*" + RE_DEC_EXPONENT

///|
const RE_HEX_DOUBLE : Regex = re"0[xX]" +
  RE_HEX_DIGITS +
  re"\.[0-9a-fA-F_]*" +
  RE_HEX_EXPONENT

///|
const RE_DOUBLE : Regex = RE_DEC_DOUBLE | RE_HEX_DOUBLE

///|
const RE_FLOAT : Regex = RE_DOUBLE + re"F"

///|
const RE_IDENT_CONTINUE : Regex = re"[\u{30}-\u{39}\u{41}-\u{5a}\u{5f}-\u{5f}\u{61}-\u{7a}\u{a1}-\u{ac}\u{ae}-\u{2af}\u{1100}-\u{11ff}\u{1e00}-\u{1eff}\u{2070}-\u{209f}\u{2150}-\u{218f}\u{2e80}-\u{2eff}\u{2ff0}-\u{2fff}\u{3001}-\u{30ff}\u{31c0}-\u{9fff}\u{ac00}-\u{d7ff}\u{f900}-\u{faff}\u{fe00}-\u{fe0f}\u{fe30}-\u{fe4f}\u{1f000}-\u{1fbff}\u{20000}-\u{2a6df}\u{2a700}-\u{2ebef}\u{2f800}-\u{2fa1f}\u{30000}-\u{323af}\u{e0100}-\u{e01ef}]*"

///|
const RE_LIDENT_START : Regex = re"[\u{5f}-\u{5f}\u{61}-\u{7a}\u{a1}-\u{ac}\u{ae}-\u{2af}\u{1100}-\u{11ff}\u{1e00}-\u{1eff}\u{2070}-\u{209f}\u{2150}-\u{218f}\u{2e80}-\u{2eff}\u{2ff0}-\u{2fff}\u{3001}-\u{30ff}\u{31c0}-\u{9fff}\u{ac00}-\u{d7ff}\u{f900}-\u{faff}\u{fe00}-\u{fe0f}\u{fe30}-\u{fe4f}\u{1f000}-\u{1fbff}\u{20000}-\u{2a6df}\u{2a700}-\u{2ebef}\u{2f800}-\u{2fa1f}\u{30000}-\u{323af}\u{e0100}-\u{e01ef}]"

///|
const RE_UIDENT : Regex = re"[A-Z]" + RE_IDENT_CONTINUE

///|
const RE_LIDENT : Regex = RE_LIDENT_START + RE_IDENT_CONTINUE

///|
const RE_OPTIONAL_LIDENT : Regex = RE_LIDENT | re""

///|
const RE_METAVAR_PREFIX : Regex = re"\$(\$\$)?"

///|
const RE_METAVAR_NAME : Regex = re"[A-Za-z_][A-Za-z0-9_]*"

///|
const RE_METAVAR_LIDENT : Regex = re"[a-z_][A-Za-z0-9_]*"

///|
const RE_METAVAR_UIDENT : Regex = re"[A-Z][A-Za-z0-9_]*"

///|
const RE_METAVAR_KIND : Regex = re"[a-z][a-z]*"

///|
const RE_METAVAR_SPACES : Regex = re"[ ]*"
