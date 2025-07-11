
pub(all) enum Token {
  CHAR(CharLiteral)
  INT(String)
  BYTE(CharLiteral)
  BYTES(StringLiteral)
  FLOAT(String)
  DOUBLE(String)
  STRING(StringLiteral)
  MULTILINE_STRING(String)
  MULTILINE_INTERP(InterpLiteral)
  INTERP(InterpLiteral)
  ATTRIBUTE((String, String?, String))
  LIDENT(String)
  UIDENT(String)
  POST_LABEL(String)
  COMMENT(Comment)
  NEWLINE
  INFIX1(String)
  INFIX2(String)
  INFIX3(String)
  INFIX4(String)
  AUGMENTED_ASSIGNMENT(String)
  EOF
  FALSE
  TRUE
  PUB
  PRIV
  READONLY
  IMPORT
  EXTERN
  BREAK
  CONTINUE
  STRUCT
  ENUM
  TRAIT
  DERIVE
  IMPL
  WITH
  RAISE
  THROW
  TRY
  CATCH
  ASYNC
  TYPEALIAS
  TRAITALIAS
  FNALIAS
  EQUAL
  LPAREN
  RPAREN
  COMMA
  MINUS
  QUESTION
  EXCLAMATION
  DOT_LIDENT(String)
  DOT_UIDENT(String)
  DOT_INT(Int)
  DOT_LPAREN
  COLONCOLON
  COLON
  SEMI(Bool)
  LBRACKET
  PLUS
  RBRACKET
  UNDERSCORE
  BAR
  LBRACE
  RBRACE
  AMPERAMPER
  AMPER
  CARET
  BARBAR
  PACKAGE_NAME(String)
  AS
  PIPE
  ELSE
  FN
  IF
  LET
  CONST
  MATCH
  USING
  MUTABLE
  TYPE
  FAT_ARROW
  THIN_ARROW
  WHILE
  RETURN
  DOTDOT
  RANGE_INCLUSIVE
  RANGE_EXCLUSIVE
  ELLIPSIS
  TEST
  LOOP
  GUARD
  FOR
  IN
  IS
  SUBERROR
  AND
  LETREC
  ENUMVIEW
  NORAISE
} derive(Show)

pub fn Token::kind(self : Token) -> TokenKind {
  match self {
    CHAR(_) => TK_CHAR
    INT(_) => TK_INT
    BYTE(_) => TK_BYTE
    BYTES(_) => TK_BYTES
    FLOAT(_) => TK_FLOAT
    DOUBLE(_) => TK_DOUBLE
    STRING(_) => TK_STRING
    MULTILINE_STRING(_) => TK_MULTILINE_STRING
    MULTILINE_INTERP(_) => TK_MULTILINE_INTERP
    INTERP(_) => TK_INTERP
    ATTRIBUTE(_) => TK_ATTRIBUTE
    LIDENT(_) => TK_LIDENT
    UIDENT(_) => TK_UIDENT
    POST_LABEL(_) => TK_POST_LABEL
    COMMENT(_) => TK_COMMENT
    NEWLINE => TK_NEWLINE
    INFIX1(_) => TK_INFIX1
    INFIX2(_) => TK_INFIX2
    INFIX3(_) => TK_INFIX3
    INFIX4(_) => TK_INFIX4
    AUGMENTED_ASSIGNMENT(_) => TK_AUGMENTED_ASSIGNMENT
    EOF => TK_EOF
    FALSE => TK_FALSE
    TRUE => TK_TRUE
    PUB => TK_PUB
    PRIV => TK_PRIV
    READONLY => TK_READONLY
    IMPORT => TK_IMPORT
    EXTERN => TK_EXTERN
    BREAK => TK_BREAK
    CONTINUE => TK_CONTINUE
    STRUCT => TK_STRUCT
    ENUM => TK_ENUM
    TRAIT => TK_TRAIT
    DERIVE => TK_DERIVE
    IMPL => TK_IMPL
    WITH => TK_WITH
    RAISE => TK_RAISE
    THROW => TK_THROW
    TRY => TK_TRY
    CATCH => TK_CATCH
    ASYNC => TK_ASYNC
    TYPEALIAS => TK_TYPEALIAS
    TRAITALIAS => TK_TRAITALIAS
    FNALIAS => TK_FNALIAS
    EQUAL => TK_EQUAL
    LPAREN => TK_LPAREN
    RPAREN => TK_RPAREN
    COMMA => TK_COMMA
    MINUS => TK_MINUS
    QUESTION => TK_QUESTION
    EXCLAMATION => TK_EXCLAMATION
    DOT_LIDENT(_) => TK_DOT_LIDENT
    DOT_UIDENT(_) => TK_DOT_UIDENT
    DOT_INT(_) => TK_DOT_INT
    DOT_LPAREN => TK_DOT_LPAREN
    COLONCOLON => TK_COLONCOLON
    COLON => TK_COLON
    SEMI(_) => TK_SEMI
    LBRACKET => TK_LBRACKET
    PLUS => TK_PLUS
    RBRACKET => TK_RBRACKET
    UNDERSCORE => TK_UNDERSCORE
    BAR => TK_BAR
    LBRACE => TK_LBRACE
    RBRACE => TK_RBRACE
    AMPERAMPER => TK_AMPERAMPER
    AMPER => TK_AMPER
    CARET => TK_CARET
    BARBAR => TK_BARBAR
    PACKAGE_NAME(_) => TK_PACKAGE_NAME
    AS => TK_AS
    PIPE => TK_PIPE
    ELSE => TK_ELSE
    FN => TK_FN
    IF => TK_IF
    LET => TK_LET
    CONST => TK_CONST
    MATCH => TK_MATCH
    USING => TK_USING
    MUTABLE => TK_MUTABLE
    TYPE => TK_TYPE
    FAT_ARROW => TK_FAT_ARROW
    THIN_ARROW => TK_THIN_ARROW
    WHILE => TK_WHILE
    RETURN => TK_RETURN
    DOTDOT => TK_DOTDOT
    RANGE_INCLUSIVE => TK_RANGE_INCLUSIVE
    RANGE_EXCLUSIVE => TK_RANGE_EXCLUSIVE
    ELLIPSIS => TK_ELLIPSIS
    TEST => TK_TEST
    LOOP => TK_LOOP
    GUARD => TK_GUARD
    FOR => TK_FOR
    IN => TK_IN
    IS => TK_IS
    SUBERROR => TK_SUBERROR
    AND => TK_AND
    LETREC => TK_LETREC
    ENUMVIEW => TK_ENUMVIEW
    NORAISE => TK_NORAISE
  }
}

pub(all) enum TokenKind {
  TK_CHAR
  TK_INT
  TK_BYTE
  TK_BYTES
  TK_FLOAT
  TK_DOUBLE
  TK_STRING
  TK_MULTILINE_STRING
  TK_MULTILINE_INTERP
  TK_INTERP
  TK_ATTRIBUTE
  TK_LIDENT
  TK_UIDENT
  TK_POST_LABEL
  TK_COMMENT
  TK_NEWLINE
  TK_INFIX1
  TK_INFIX2
  TK_INFIX3
  TK_INFIX4
  TK_AUGMENTED_ASSIGNMENT
  TK_EOF
  TK_FALSE
  TK_TRUE
  TK_PUB
  TK_PRIV
  TK_READONLY
  TK_IMPORT
  TK_EXTERN
  TK_BREAK
  TK_CONTINUE
  TK_STRUCT
  TK_ENUM
  TK_TRAIT
  TK_DERIVE
  TK_IMPL
  TK_WITH
  TK_RAISE
  TK_THROW
  TK_TRY
  TK_CATCH
  TK_ASYNC
  TK_TYPEALIAS
  TK_TRAITALIAS
  TK_FNALIAS
  TK_EQUAL
  TK_LPAREN
  TK_RPAREN
  TK_COMMA
  TK_MINUS
  TK_QUESTION
  TK_EXCLAMATION
  TK_DOT_LIDENT
  TK_DOT_UIDENT
  TK_DOT_INT
  TK_DOT_LPAREN
  TK_COLONCOLON
  TK_COLON
  TK_SEMI
  TK_LBRACKET
  TK_PLUS
  TK_RBRACKET
  TK_UNDERSCORE
  TK_BAR
  TK_LBRACE
  TK_RBRACE
  TK_AMPERAMPER
  TK_AMPER
  TK_CARET
  TK_BARBAR
  TK_PACKAGE_NAME
  TK_AS
  TK_PIPE
  TK_ELSE
  TK_FN
  TK_IF
  TK_LET
  TK_CONST
  TK_MATCH
  TK_USING
  TK_MUTABLE
  TK_TYPE
  TK_FAT_ARROW
  TK_THIN_ARROW
  TK_WHILE
  TK_RETURN
  TK_DOTDOT
  TK_RANGE_INCLUSIVE
  TK_RANGE_EXCLUSIVE
  TK_ELLIPSIS
  TK_TEST
  TK_LOOP
  TK_GUARD
  TK_FOR
  TK_IN
  TK_IS
  TK_SUBERROR
  TK_AND
  TK_LETREC
  TK_ENUMVIEW
  TK_NORAISE
}

pub impl Show for TokenKind with output(self, logger) {
  logger.write_string(
    match self {
      TK_CHAR => "CHAR"
      TK_INT => "INT"
      TK_BYTE => "BYTE"
      TK_BYTES => "BYTES"
      TK_FLOAT => "FLOAT"
      TK_DOUBLE => "DOUBLE"
      TK_STRING => "STRING"
      TK_MULTILINE_STRING => "MULTILINE_STRING"
      TK_MULTILINE_INTERP => "MULTILINE_INTERP"
      TK_INTERP => "INTERP"
      TK_ATTRIBUTE => "ATTRIBUTE"
      TK_LIDENT => "LIDENT"
      TK_UIDENT => "UIDENT"
      TK_POST_LABEL => "POST_LABEL"
      TK_COMMENT => "COMMENT"
      TK_NEWLINE => "NEWLINE"
      TK_INFIX1 => "INFIX1"
      TK_INFIX2 => "INFIX2"
      TK_INFIX3 => "INFIX3"
      TK_INFIX4 => "INFIX4"
      TK_AUGMENTED_ASSIGNMENT => "AUGMENTED_ASSIGNMENT"
      TK_EOF => "EOF"
      TK_FALSE => "FALSE"
      TK_TRUE => "TRUE"
      TK_PUB => "\"pub\""
      TK_PRIV => "\"priv\""
      TK_READONLY => "\"readonly\""
      TK_IMPORT => "\"import\""
      TK_EXTERN => "\"extern\""
      TK_BREAK => "\"break\""
      TK_CONTINUE => "\"continue\""
      TK_STRUCT => "\"struct\""
      TK_ENUM => "\"enum\""
      TK_TRAIT => "\"trait\""
      TK_DERIVE => "\"derive\""
      TK_IMPL => "\"impl\""
      TK_WITH => "\"with\""
      TK_RAISE => "\"raise\""
      TK_THROW => "\"throw\""
      TK_TRY => "\"try\""
      TK_CATCH => "\"catch\""
      TK_ASYNC => "\"async\""
      TK_TYPEALIAS => "\"typealias\""
      TK_TRAITALIAS => "\"traitalias\""
      TK_FNALIAS => "\"fnalias\""
      TK_EQUAL => "\"=\""
      TK_LPAREN => "\"(\""
      TK_RPAREN => "\")\""
      TK_COMMA => "\",\""
      TK_MINUS => "\"-\""
      TK_QUESTION => "\"?\""
      TK_EXCLAMATION => "\"!\""
      TK_DOT_LIDENT => "DOT_LIDENT"
      TK_DOT_UIDENT => "DOT_UIDENT"
      TK_DOT_INT => "DOT_INT"
      TK_DOT_LPAREN => "\".(\""
      TK_COLONCOLON => "\"::\""
      TK_COLON => "\":\""
      TK_SEMI => "SEMI"
      TK_LBRACKET => "\"[\""
      TK_PLUS => "\"+\""
      TK_RBRACKET => "\"]\""
      TK_UNDERSCORE => "\"_\""
      TK_BAR => "\"|\""
      TK_LBRACE => "\"{\""
      TK_RBRACE => "\"}\""
      TK_AMPERAMPER => "\"&&\""
      TK_AMPER => "\"&\""
      TK_CARET => "\"^\""
      TK_BARBAR => "\"||\""
      TK_PACKAGE_NAME => "PACKAGE_NAME"
      TK_AS => "\"as\""
      TK_PIPE => "\"|>\""
      TK_ELSE => "\"else\""
      TK_FN => "\"fn\""
      TK_IF => "\"if\""
      TK_LET => "\"let\""
      TK_CONST => "\"const\""
      TK_MATCH => "\"match\""
      TK_USING => "\"using\""
      TK_MUTABLE => "\"mut\""
      TK_TYPE => "\"type\""
      TK_FAT_ARROW => "\"=>\""
      TK_THIN_ARROW => "\"->\""
      TK_WHILE => "\"while\""
      TK_RETURN => "\"return\""
      TK_DOTDOT => "\"..\""
      TK_RANGE_INCLUSIVE => "\"..=\""
      TK_RANGE_EXCLUSIVE => "\"..<\""
      TK_ELLIPSIS => "\"...\""
      TK_TEST => "\"test\""
      TK_LOOP => "\"loop\""
      TK_GUARD => "\"guard\""
      TK_FOR => "\"for\""
      TK_IN => "\"in\""
      TK_IS => "\"is\""
      TK_SUBERROR => "\"suberror\""
      TK_AND => "\"and\""
      TK_LETREC => "\"letrec\""
      TK_ENUMVIEW => "\"enumview\""
      TK_NORAISE => "\"noraise\""
    }
  )
}

pub suberror ParseError {
  UnexpectedToken(Token, (@ast_types.Position, @ast_types.Position), Array[TokenKind])
  UnexpectedEndOfInput(@ast_types.Position, Array[TokenKind])
} derive(Show)

typealias Error as YYObj

priv suberror YYObj_Void

priv suberror YYObj_Int Int

priv suberror YYObj_Bool Bool

priv suberror YYObj_Expr Expr

priv suberror YYObj_String String

priv suberror YYObj_Comment Comment

priv suberror YYObj_CharLiteral CharLiteral

priv suberror YYObj_InterpLiteral InterpLiteral

priv suberror YYObj_StringLiteral StringLiteral

priv suberror YYObj__list_T_Impl_ @list.T[Impl]

priv suberror YYObj__String__String___String_ (String, String?, String)

typealias (YYSymbol) -> YYDecision as YYState

typealias (@ast_types.Position, ArrayView[(YYObj, @ast_types.Position, @ast_types.Position)]) -> YYObj as YYAction

priv enum YYDecision {
  Accept
  Shift(YYState)
  Reduce(Int, YYSymbol, YYAction)
  ReduceNoLookahead(Int, YYSymbol, YYAction)
  Error
}

priv enum YYSymbol {
  T_CHAR
  T_INT
  T_BYTE
  T_BYTES
  T_FLOAT
  T_DOUBLE
  T_STRING
  T_MULTILINE_STRING
  T_MULTILINE_INTERP
  T_INTERP
  T_ATTRIBUTE
  T_LIDENT
  T_UIDENT
  T_POST_LABEL
  T_COMMENT
  T_NEWLINE
  T_INFIX1
  T_INFIX2
  T_INFIX3
  T_INFIX4
  T_AUGMENTED_ASSIGNMENT
  T_EOF
  T_FALSE
  T_TRUE
  T_PUB
  T_PRIV
  T_READONLY
  T_IMPORT
  T_EXTERN
  T_BREAK
  T_CONTINUE
  T_STRUCT
  T_ENUM
  T_TRAIT
  T_DERIVE
  T_IMPL
  T_WITH
  T_RAISE
  T_THROW
  T_TRY
  T_CATCH
  T_ASYNC
  T_TYPEALIAS
  T_TRAITALIAS
  T_FNALIAS
  T_EQUAL
  T_LPAREN
  T_RPAREN
  T_COMMA
  T_MINUS
  T_QUESTION
  T_EXCLAMATION
  T_DOT_LIDENT
  T_DOT_UIDENT
  T_DOT_INT
  T_DOT_LPAREN
  T_COLONCOLON
  T_COLON
  T_SEMI
  T_LBRACKET
  T_PLUS
  T_RBRACKET
  T_UNDERSCORE
  T_BAR
  T_LBRACE
  T_RBRACE
  T_AMPERAMPER
  T_AMPER
  T_CARET
  T_BARBAR
  T_PACKAGE_NAME
  T_AS
  T_PIPE
  T_ELSE
  T_FN
  T_IF
  T_LET
  T_CONST
  T_MATCH
  T_USING
  T_MUTABLE
  T_TYPE
  T_FAT_ARROW
  T_THIN_ARROW
  T_WHILE
  T_RETURN
  T_DOTDOT
  T_RANGE_INCLUSIVE
  T_RANGE_EXCLUSIVE
  T_ELLIPSIS
  T_TEST
  T_LOOP
  T_GUARD
  T_FOR
  T_IN
  T_IS
  T_SUBERROR
  T_AND
  T_LETREC
  T_ENUMVIEW
  T_NORAISE
  NT_structure
  NT_expression
  EOI
}

// Workaround for EOI unused warning
fn init {
  match (EOI : YYSymbol) {
    EOI => ()
    _ => ()
  }
}

// file:///./parser.mbty
// 215|    { ... }
fn yy_action_0(_last_pos : @ast_types.Position, _args : ArrayView[(YYObj, @ast_types.Position, @ast_types.Position)]) -> YYObj {
  YYObj_Expr({();  ... })
}

// file:///./parser.mbty
// 211|    { ... }
fn yy_action_1(_last_pos : @ast_types.Position, _args : ArrayView[(YYObj, @ast_types.Position, @ast_types.Position)]) -> YYObj {
  YYObj__list_T_Impl_({();  ... })
}

fn yy_input(token : Token, _start_pos : @ast_types.Position, _end_pos : @ast_types.Position) -> (YYSymbol, YYObj) {
  match token {
    CHAR(data) => (T_CHAR, YYObj_CharLiteral(data))
    INT(data) => (T_INT, YYObj_String(data))
    BYTE(data) => (T_BYTE, YYObj_CharLiteral(data))
    BYTES(data) => (T_BYTES, YYObj_StringLiteral(data))
    FLOAT(data) => (T_FLOAT, YYObj_String(data))
    DOUBLE(data) => (T_DOUBLE, YYObj_String(data))
    STRING(data) => (T_STRING, YYObj_StringLiteral(data))
    MULTILINE_STRING(data) => (T_MULTILINE_STRING, YYObj_String(data))
    MULTILINE_INTERP(data) => (T_MULTILINE_INTERP, YYObj_InterpLiteral(data))
    INTERP(data) => (T_INTERP, YYObj_InterpLiteral(data))
    ATTRIBUTE(data) => (T_ATTRIBUTE, YYObj__String__String___String_(data))
    LIDENT(data) => (T_LIDENT, YYObj_String(data))
    UIDENT(data) => (T_UIDENT, YYObj_String(data))
    POST_LABEL(data) => (T_POST_LABEL, YYObj_String(data))
    COMMENT(data) => (T_COMMENT, YYObj_Comment(data))
    NEWLINE => (T_NEWLINE, YYObj_Void)
    INFIX1(data) => (T_INFIX1, YYObj_String(data))
    INFIX2(data) => (T_INFIX2, YYObj_String(data))
    INFIX3(data) => (T_INFIX3, YYObj_String(data))
    INFIX4(data) => (T_INFIX4, YYObj_String(data))
    AUGMENTED_ASSIGNMENT(data) => (T_AUGMENTED_ASSIGNMENT, YYObj_String(data))
    EOF => (T_EOF, YYObj_Void)
    FALSE => (T_FALSE, YYObj_Void)
    TRUE => (T_TRUE, YYObj_Void)
    PUB => (T_PUB, YYObj_Void)
    PRIV => (T_PRIV, YYObj_Void)
    READONLY => (T_READONLY, YYObj_Void)
    IMPORT => (T_IMPORT, YYObj_Void)
    EXTERN => (T_EXTERN, YYObj_Void)
    BREAK => (T_BREAK, YYObj_Void)
    CONTINUE => (T_CONTINUE, YYObj_Void)
    STRUCT => (T_STRUCT, YYObj_Void)
    ENUM => (T_ENUM, YYObj_Void)
    TRAIT => (T_TRAIT, YYObj_Void)
    DERIVE => (T_DERIVE, YYObj_Void)
    IMPL => (T_IMPL, YYObj_Void)
    WITH => (T_WITH, YYObj_Void)
    RAISE => (T_RAISE, YYObj_Void)
    THROW => (T_THROW, YYObj_Void)
    TRY => (T_TRY, YYObj_Void)
    CATCH => (T_CATCH, YYObj_Void)
    ASYNC => (T_ASYNC, YYObj_Void)
    TYPEALIAS => (T_TYPEALIAS, YYObj_Void)
    TRAITALIAS => (T_TRAITALIAS, YYObj_Void)
    FNALIAS => (T_FNALIAS, YYObj_Void)
    EQUAL => (T_EQUAL, YYObj_Void)
    LPAREN => (T_LPAREN, YYObj_Void)
    RPAREN => (T_RPAREN, YYObj_Void)
    COMMA => (T_COMMA, YYObj_Void)
    MINUS => (T_MINUS, YYObj_Void)
    QUESTION => (T_QUESTION, YYObj_Void)
    EXCLAMATION => (T_EXCLAMATION, YYObj_Void)
    DOT_LIDENT(data) => (T_DOT_LIDENT, YYObj_String(data))
    DOT_UIDENT(data) => (T_DOT_UIDENT, YYObj_String(data))
    DOT_INT(data) => (T_DOT_INT, YYObj_Int(data))
    DOT_LPAREN => (T_DOT_LPAREN, YYObj_Void)
    COLONCOLON => (T_COLONCOLON, YYObj_Void)
    COLON => (T_COLON, YYObj_Void)
    SEMI(data) => (T_SEMI, YYObj_Bool(data))
    LBRACKET => (T_LBRACKET, YYObj_Void)
    PLUS => (T_PLUS, YYObj_Void)
    RBRACKET => (T_RBRACKET, YYObj_Void)
    UNDERSCORE => (T_UNDERSCORE, YYObj_Void)
    BAR => (T_BAR, YYObj_Void)
    LBRACE => (T_LBRACE, YYObj_Void)
    RBRACE => (T_RBRACE, YYObj_Void)
    AMPERAMPER => (T_AMPERAMPER, YYObj_Void)
    AMPER => (T_AMPER, YYObj_Void)
    CARET => (T_CARET, YYObj_Void)
    BARBAR => (T_BARBAR, YYObj_Void)
    PACKAGE_NAME(data) => (T_PACKAGE_NAME, YYObj_String(data))
    AS => (T_AS, YYObj_Void)
    PIPE => (T_PIPE, YYObj_Void)
    ELSE => (T_ELSE, YYObj_Void)
    FN => (T_FN, YYObj_Void)
    IF => (T_IF, YYObj_Void)
    LET => (T_LET, YYObj_Void)
    CONST => (T_CONST, YYObj_Void)
    MATCH => (T_MATCH, YYObj_Void)
    USING => (T_USING, YYObj_Void)
    MUTABLE => (T_MUTABLE, YYObj_Void)
    TYPE => (T_TYPE, YYObj_Void)
    FAT_ARROW => (T_FAT_ARROW, YYObj_Void)
    THIN_ARROW => (T_THIN_ARROW, YYObj_Void)
    WHILE => (T_WHILE, YYObj_Void)
    RETURN => (T_RETURN, YYObj_Void)
    DOTDOT => (T_DOTDOT, YYObj_Void)
    RANGE_INCLUSIVE => (T_RANGE_INCLUSIVE, YYObj_Void)
    RANGE_EXCLUSIVE => (T_RANGE_EXCLUSIVE, YYObj_Void)
    ELLIPSIS => (T_ELLIPSIS, YYObj_Void)
    TEST => (T_TEST, YYObj_Void)
    LOOP => (T_LOOP, YYObj_Void)
    GUARD => (T_GUARD, YYObj_Void)
    FOR => (T_FOR, YYObj_Void)
    IN => (T_IN, YYObj_Void)
    IS => (T_IS, YYObj_Void)
    SUBERROR => (T_SUBERROR, YYObj_Void)
    AND => (T_AND, YYObj_Void)
    LETREC => (T_LETREC, YYObj_Void)
    ENUMVIEW => (T_ENUMVIEW, YYObj_Void)
    NORAISE => (T_NORAISE, YYObj_Void)
  }
}

// [1, expression → •, $]
// [3, expression_prime → • expression, $]
fn yy_state_0(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_expression => Shift(yy_state_1)
    EOI => Reduce(0, NT_expression, yy_action_0)
    _ => Error
  }
}

// [3, expression_prime → expression •, $]
fn yy_state_1(_lookahead : YYSymbol) -> YYDecision {
  Accept
}

// [0, structure → •, $]
// [2, structure_prime → • structure, $]
fn yy_state_2(_lookahead : YYSymbol) -> YYDecision {
  match _lookahead {
    NT_structure => Shift(yy_state_3)
    EOI => Reduce(0, NT_structure, yy_action_1)
    _ => Error
  }
}

// [2, structure_prime → structure •, $]
fn yy_state_3(_lookahead : YYSymbol) -> YYDecision {
  Accept
}

fn[T] yy_parse(
  tokens : Array[(Token, @ast_types.Position, @ast_types.Position)],
  start : YYState,
  return_ : (YYObj) -> T,
  initial_pos? : @ast_types.Position,
) -> T raise ParseError {
  let mut cursor = 0
  let mut state_stack : @list.T[YYState] = @list.construct(start, @list.empty())
  let data_stack : Array[(YYObj, @ast_types.Position, @ast_types.Position)] = []
  let mut last_pos = initial_pos.unwrap_or(tokens[0].1)
  let mut state = start
  let mut lookahead : Option[(YYSymbol, (YYObj, @ast_types.Position, @ast_types.Position), Token?)] = None
  let mut last_shifted_state_stack = state_stack
  while true {
    let decision = match state(EOI) {
      ReduceNoLookahead(_) | Accept as t => t
      _ => {
        match lookahead {
          Some(la) => state(la.0)
          None => {
            if cursor < tokens.length() {
              let (token, start_pos, end_pos) = tokens[cursor]
              cursor += 1
              let (symbol, data) = yy_input(token, start_pos, end_pos)
              lookahead = Some((symbol, (data, start_pos, end_pos), Some(token)))
              state(symbol)
            } else {
              lookahead = Some((EOI, (YYObj_Void, last_pos, last_pos), None))
              state(EOI)
            }
          }
        }
      }
    }
    match decision {
      Accept => return return_(data_stack.unsafe_pop().0)
      Shift(next_state) => {
        guard lookahead is Some(la)
        data_stack.push(la.1)
        state_stack = @list.construct(next_state, state_stack)
        last_shifted_state_stack = state_stack
        state = next_state
        last_pos = la.1.2
        lookahead = None
      }
      Reduce(count, symbol, action)
      | ReduceNoLookahead(count, symbol, action) => {
        loop (count, symbol, action) {
          _ => {
            let args = data_stack[data_stack.length() - count:]
            let data = action(last_pos, args)
            let (start_pos, end_pos) = if args.length() == 0 {
              (last_pos, last_pos)
            } else {
              (args[0].1, args[args.length() - 1].2)
            }
            for i in 0..<count {
              ignore(data_stack.unsafe_pop())
              state_stack = state_stack.unsafe_tail()
            }
            state = state_stack.unsafe_head()
            data_stack.push((data, start_pos, end_pos))
            match state(symbol) {
              Accept => return return_(data_stack.unsafe_pop().0)
              Shift(next_state) => {
                state_stack = @list.construct(next_state, state_stack)
                state = next_state
              }
              Reduce(count, symbol, action)
              | ReduceNoLookahead(count, symbol, action) => continue (count, symbol, action)
              _ => panic()
            }
          }
        }
      }
      Error => {
        let (_, (_, start_pos, end_pos), token) = lookahead.unwrap()
        error(last_shifted_state_stack, token, (start_pos, end_pos))
      }
    }
  }
  panic()
}

fn error(stack : @list.T[YYState], token : Token?, loc : (@ast_types.Position, @ast_types.Position)) -> Unit raise ParseError {
  let expected = []
  fn try_add(symbol : YYSymbol, kind : TokenKind) {
    fn go(stack : @list.T[YYState]) {
      match stack {
        Empty => ()
        More(state, ..) => {
          match state(symbol) {
            Accept | Shift(_) => expected.push(kind)
            Reduce(count, symbol, _) | ReduceNoLookahead(count, symbol, _) => {
              fn inner_go(stack : @list.T[YYState], count, symbol) {
                let stack = stack.drop(count)
                guard stack is More(state, ..)
                match state(symbol) {
                  Shift(state) => go(@list.construct(state, stack))
                  Reduce(count, symbol, _) | ReduceNoLookahead(count, symbol, _) => inner_go(stack, count, symbol)
                  _ => panic()
                }
              }
              inner_go(stack, count, symbol)
            }
            Error => ()
          }
        }
      }
    }
    go(stack)
  }
  for term in ([(T_CHAR, TK_CHAR), (T_INT, TK_INT), (T_BYTE, TK_BYTE), (T_BYTES, TK_BYTES), (T_FLOAT, TK_FLOAT), (T_DOUBLE, TK_DOUBLE), (T_STRING, TK_STRING), (T_MULTILINE_STRING, TK_MULTILINE_STRING), (T_MULTILINE_INTERP, TK_MULTILINE_INTERP), (T_INTERP, TK_INTERP), (T_ATTRIBUTE, TK_ATTRIBUTE), (T_LIDENT, TK_LIDENT), (T_UIDENT, TK_UIDENT), (T_POST_LABEL, TK_POST_LABEL), (T_COMMENT, TK_COMMENT), (T_NEWLINE, TK_NEWLINE), (T_INFIX1, TK_INFIX1), (T_INFIX2, TK_INFIX2), (T_INFIX3, TK_INFIX3), (T_INFIX4, TK_INFIX4), (T_AUGMENTED_ASSIGNMENT, TK_AUGMENTED_ASSIGNMENT), (T_EOF, TK_EOF), (T_FALSE, TK_FALSE), (T_TRUE, TK_TRUE), (T_PUB, TK_PUB), (T_PRIV, TK_PRIV), (T_READONLY, TK_READONLY), (T_IMPORT, TK_IMPORT), (T_EXTERN, TK_EXTERN), (T_BREAK, TK_BREAK), (T_CONTINUE, TK_CONTINUE), (T_STRUCT, TK_STRUCT), (T_ENUM, TK_ENUM), (T_TRAIT, TK_TRAIT), (T_DERIVE, TK_DERIVE), (T_IMPL, TK_IMPL), (T_WITH, TK_WITH), (T_RAISE, TK_RAISE), (T_THROW, TK_THROW), (T_TRY, TK_TRY), (T_CATCH, TK_CATCH), (T_ASYNC, TK_ASYNC), (T_TYPEALIAS, TK_TYPEALIAS), (T_TRAITALIAS, TK_TRAITALIAS), (T_FNALIAS, TK_FNALIAS), (T_EQUAL, TK_EQUAL), (T_LPAREN, TK_LPAREN), (T_RPAREN, TK_RPAREN), (T_COMMA, TK_COMMA), (T_MINUS, TK_MINUS), (T_QUESTION, TK_QUESTION), (T_EXCLAMATION, TK_EXCLAMATION), (T_DOT_LIDENT, TK_DOT_LIDENT), (T_DOT_UIDENT, TK_DOT_UIDENT), (T_DOT_INT, TK_DOT_INT), (T_DOT_LPAREN, TK_DOT_LPAREN), (T_COLONCOLON, TK_COLONCOLON), (T_COLON, TK_COLON), (T_SEMI, TK_SEMI), (T_LBRACKET, TK_LBRACKET), (T_PLUS, TK_PLUS), (T_RBRACKET, TK_RBRACKET), (T_UNDERSCORE, TK_UNDERSCORE), (T_BAR, TK_BAR), (T_LBRACE, TK_LBRACE), (T_RBRACE, TK_RBRACE), (T_AMPERAMPER, TK_AMPERAMPER), (T_AMPER, TK_AMPER), (T_CARET, TK_CARET), (T_BARBAR, TK_BARBAR), (T_PACKAGE_NAME, TK_PACKAGE_NAME), (T_AS, TK_AS), (T_PIPE, TK_PIPE), (T_ELSE, TK_ELSE), (T_FN, TK_FN), (T_IF, TK_IF), (T_LET, TK_LET), (T_CONST, TK_CONST), (T_MATCH, TK_MATCH), (T_USING, TK_USING), (T_MUTABLE, TK_MUTABLE), (T_TYPE, TK_TYPE), (T_FAT_ARROW, TK_FAT_ARROW), (T_THIN_ARROW, TK_THIN_ARROW), (T_WHILE, TK_WHILE), (T_RETURN, TK_RETURN), (T_DOTDOT, TK_DOTDOT), (T_RANGE_INCLUSIVE, TK_RANGE_INCLUSIVE), (T_RANGE_EXCLUSIVE, TK_RANGE_EXCLUSIVE), (T_ELLIPSIS, TK_ELLIPSIS), (T_TEST, TK_TEST), (T_LOOP, TK_LOOP), (T_GUARD, TK_GUARD), (T_FOR, TK_FOR), (T_IN, TK_IN), (T_IS, TK_IS), (T_SUBERROR, TK_SUBERROR), (T_AND, TK_AND), (T_LETREC, TK_LETREC), (T_ENUMVIEW, TK_ENUMVIEW), (T_NORAISE, TK_NORAISE)] : Array[(YYSymbol, TokenKind)]) {
    try_add(term.0, term.1)
  }
  match token {
    None => raise UnexpectedEndOfInput(loc.1, expected)
    Some(token) => raise UnexpectedToken(token, loc, expected)
  }
}

pub fn structure(tokens : Array[(Token, @ast_types.Position, @ast_types.Position)], initial_pos? : @ast_types.Position) -> @list.T[Impl] raise ParseError {
  yy_parse(
    tokens,
    yy_state_2,
    (it) => {
      guard it is YYObj__list_T_Impl_(result)
      result
    },
    initial_pos?,
  )
}
pub fn expression(tokens : Array[(Token, @ast_types.Position, @ast_types.Position)], initial_pos? : @ast_types.Position) -> Expr raise ParseError {
  yy_parse(
    tokens,
    yy_state_0,
    (it) => {
      guard it is YYObj_Expr(result)
      result
    },
    initial_pos?,
  )
}
