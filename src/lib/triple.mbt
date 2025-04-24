///|
pub type TokenTriple (Token, Position, Position) // token, start, end

///|
fn token_convert_s(tok : Token) -> String {
  match tok {
    PACKAGE_NAME(name) => "(PACKAGE_NAME \{name})"
    SEMI(b) => "(SEMI \{b})"
    DOT_INT(i) => "(DOT_INT \{i})"
    DOT_UIDENT(id) => "(DOT_UIDENT \{id})"
    DOT_LIDENT(id) => "(DOT_LIDENT \{id})"
    AUGMENTED_ASSIGNMENT(op) => "(AUGMENTED_ASSIGNMENT \{op})"
    INFIX4(op) => "(INFIX4 \{op})"
    INFIX3(op) => "(INFIX3 \{op})"
    INFIX2(op) => "(INFIX2 \{op})"
    INFIX1(op) => "(INFIX1 \{op})"
    COMMENT(cmt) => "(COMMENT \{cmt})"
    POST_LABEL(l) => "(POST_LABEL \{l})"
    UIDENT(id) => "(UIDENT \{id})"
    LIDENT(id) => "(LIDENT \{id})"
    ATTRIBUTE(attr) => "(ATTRIBUTE \{attr})"
    INTERP(interp) => "(INTERP \{interp})"
    MULTILINE_INTERP(interp) => "(MULTILINE_INTERP \{interp})"
    MULTILINE_STRING(s) => "(MULTILINE_STRING \{s})"
    STRING(s) => "(STRING \{s})"
    FLOAT(s) => "(FLOAT \{s})"
    BYTES(s) => "(BYTES \{s})"
    BYTE(s) => "(BYTE \{s})"
    INT(s) => "(INT \{s})"
    CHAR(s) => "(CHAR \{s})"
    _ => tok.to_string()
  }
}

///|
pub impl Show for TokenTriple with output(self, logger) {
  logger.write_char('(')
  logger.write_string(token_convert_s(self._.0))
  logger.write_char(' ')
  let l1 = self._.1.lnum
  let c1 = self._.1.cnum - self._.1.bol + 1
  let l2 = self._.2.lnum
  let c2 = self._.2.cnum - self._.2.bol + 1
  logger.write_string("\{l1}:\{c1}-\{l2}:\{c2}")
  logger.write_char(')')
}
