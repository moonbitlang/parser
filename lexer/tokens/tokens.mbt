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
  BYTES_INTERP(InterpLiteral)
  REGEX_LITERAL(String)
  REGEX_INTERP(InterpLiteral)
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
  PLUS_EQUAL
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
  EQ_TILDE
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
  LBRACKET_BAR
  PLUS
  RBRACKET
  BAR_RBRACKET
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
  PIPE_LEFT
  LT_PLUS
  LT_QUESTION
  ELSE
  FN
  IF
  LET
  PROOF_ASSERT
  PROOF_LET
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
  RANGE_LT_INCLUSIVE
  RANGE_EXCLUSIVE
  RANGE_INCLUSIVE_REV
  RANGE_EXCLUSIVE_REV
  ELLIPSIS
  TEST
  LOOP
  GUARD
  DEFER
  FOR
  IN
  IS
  SUBERROR
  AND
  FORALL
  EXISTS
  IMPLIES
  LETREC
  ENUMVIEW
  EXTENUM
  DECLARE
  NORAISE
  NOBREAK
  WHERE
  TRY_QUESTION
  TRY_EXCLAMATION
  LEXMATCH
  LEXMATCH_QUESTION
  LEXSCAN
  EXTEND
  PACKAGE
} derive(Debug)

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
    BYTES_INTERP(_) => TK_BYTES_INTERP
    REGEX_LITERAL(_) => TK_REGEX_LITERAL
    REGEX_INTERP(_) => TK_REGEX_INTERP
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
    PLUS_EQUAL => TK_PLUS_EQUAL
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
    EQ_TILDE => TK_EQ_TILDE
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
    LBRACKET_BAR => TK_LBRACKET_BAR
    PLUS => TK_PLUS
    RBRACKET => TK_RBRACKET
    BAR_RBRACKET => TK_BAR_RBRACKET
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
    PIPE_LEFT => TK_PIPE_LEFT
    LT_PLUS => TK_LT_PLUS
    LT_QUESTION => TK_LT_QUESTION
    ELSE => TK_ELSE
    FN => TK_FN
    IF => TK_IF
    LET => TK_LET
    PROOF_ASSERT => TK_PROOF_ASSERT
    PROOF_LET => TK_PROOF_LET
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
    RANGE_LT_INCLUSIVE => TK_RANGE_LT_INCLUSIVE
    RANGE_EXCLUSIVE => TK_RANGE_EXCLUSIVE
    RANGE_INCLUSIVE_REV => TK_RANGE_INCLUSIVE_REV
    RANGE_EXCLUSIVE_REV => TK_RANGE_EXCLUSIVE_REV
    ELLIPSIS => TK_ELLIPSIS
    TEST => TK_TEST
    LOOP => TK_LOOP
    GUARD => TK_GUARD
    DEFER => TK_DEFER
    FOR => TK_FOR
    IN => TK_IN
    IS => TK_IS
    SUBERROR => TK_SUBERROR
    AND => TK_AND
    FORALL => TK_FORALL
    EXISTS => TK_EXISTS
    IMPLIES => TK_IMPLIES
    LETREC => TK_LETREC
    ENUMVIEW => TK_ENUMVIEW
    EXTENUM => TK_EXTENUM
    DECLARE => TK_DECLARE
    NORAISE => TK_NORAISE
    NOBREAK => TK_NOBREAK
    WHERE => TK_WHERE
    TRY_QUESTION => TK_TRY_QUESTION
    TRY_EXCLAMATION => TK_TRY_EXCLAMATION
    LEXMATCH => TK_LEXMATCH
    LEXMATCH_QUESTION => TK_LEXMATCH_QUESTION
    LEXSCAN => TK_LEXSCAN
    EXTEND => TK_EXTEND
    PACKAGE => TK_PACKAGE
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
  TK_BYTES_INTERP
  TK_REGEX_LITERAL
  TK_REGEX_INTERP
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
  TK_PLUS_EQUAL
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
  TK_EQ_TILDE
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
  TK_LBRACKET_BAR
  TK_PLUS
  TK_RBRACKET
  TK_BAR_RBRACKET
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
  TK_PIPE_LEFT
  TK_LT_PLUS
  TK_LT_QUESTION
  TK_ELSE
  TK_FN
  TK_IF
  TK_LET
  TK_PROOF_ASSERT
  TK_PROOF_LET
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
  TK_RANGE_LT_INCLUSIVE
  TK_RANGE_EXCLUSIVE
  TK_RANGE_INCLUSIVE_REV
  TK_RANGE_EXCLUSIVE_REV
  TK_ELLIPSIS
  TK_TEST
  TK_LOOP
  TK_GUARD
  TK_DEFER
  TK_FOR
  TK_IN
  TK_IS
  TK_SUBERROR
  TK_AND
  TK_FORALL
  TK_EXISTS
  TK_IMPLIES
  TK_LETREC
  TK_ENUMVIEW
  TK_EXTENUM
  TK_DECLARE
  TK_NORAISE
  TK_NOBREAK
  TK_WHERE
  TK_TRY_QUESTION
  TK_TRY_EXCLAMATION
  TK_LEXMATCH
  TK_LEXMATCH_QUESTION
  TK_LEXSCAN
  TK_EXTEND
  TK_PACKAGE
} derive(Eq, Hash, Compare)

pub impl Show for TokenKind with fn output(self, logger) {
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
      TK_BYTES_INTERP => "BYTES_INTERP"
      TK_REGEX_LITERAL => "REGEX_LITERAL"
      TK_REGEX_INTERP => "REGEX_INTERP"
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
      TK_PLUS_EQUAL => "\"+=\""
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
      TK_EQ_TILDE => "\"=~\""
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
      TK_LBRACKET_BAR => "\"[|\""
      TK_PLUS => "\"+\""
      TK_RBRACKET => "\"]\""
      TK_BAR_RBRACKET => "\"|]\""
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
      TK_PIPE_LEFT => "\"<|\""
      TK_LT_PLUS => "\"<+\""
      TK_LT_QUESTION => "\"<?\""
      TK_ELSE => "\"else\""
      TK_FN => "\"fn\""
      TK_IF => "\"if\""
      TK_LET => "\"let\""
      TK_PROOF_ASSERT => "\"proof_assert\""
      TK_PROOF_LET => "\"proof_let\""
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
      TK_RANGE_LT_INCLUSIVE => "\"..<=\""
      TK_RANGE_EXCLUSIVE => "\"..<\""
      TK_RANGE_INCLUSIVE_REV => "\">=..\""
      TK_RANGE_EXCLUSIVE_REV => "\">..\""
      TK_ELLIPSIS => "\"...\""
      TK_TEST => "\"test\""
      TK_LOOP => "\"loop\""
      TK_GUARD => "\"guard\""
      TK_DEFER => "\"defer\""
      TK_FOR => "\"for\""
      TK_IN => "\"in\""
      TK_IS => "\"is\""
      TK_SUBERROR => "\"suberror\""
      TK_AND => "\"and\""
      TK_FORALL => "\"∀\""
      TK_EXISTS => "\"∃\""
      TK_IMPLIES => "\"→\""
      TK_LETREC => "\"letrec\""
      TK_ENUMVIEW => "\"enumview\""
      TK_EXTENUM => "\"extenum\""
      TK_DECLARE => "\"declare\""
      TK_NORAISE => "\"noraise\""
      TK_NOBREAK => "\"nobreak\""
      TK_WHERE => "\"where\""
      TK_TRY_QUESTION => "\"try?\""
      TK_TRY_EXCLAMATION => "\"try!\""
      TK_LEXMATCH => "\"lexmatch\""
      TK_LEXMATCH_QUESTION => "\"lexmatch?\""
      TK_LEXSCAN => "\"lexscan\""
      TK_EXTEND => "\"extend\""
      TK_PACKAGE => "\"package\""
    }
  )
}

pub impl Debug for TokenKind with fn to_repr(self) {
    match self {
      TK_CHAR => to_repr("CHAR")
      TK_INT => to_repr("INT")
      TK_BYTE => to_repr("BYTE")
      TK_BYTES => to_repr("BYTES")
      TK_FLOAT => to_repr("FLOAT")
      TK_DOUBLE => to_repr("DOUBLE")
      TK_STRING => to_repr("STRING")
      TK_MULTILINE_STRING => to_repr("MULTILINE_STRING")
      TK_MULTILINE_INTERP => to_repr("MULTILINE_INTERP")
      TK_INTERP => to_repr("INTERP")
      TK_BYTES_INTERP => to_repr("BYTES_INTERP")
      TK_REGEX_LITERAL => to_repr("REGEX_LITERAL")
      TK_REGEX_INTERP => to_repr("REGEX_INTERP")
      TK_ATTRIBUTE => to_repr("ATTRIBUTE")
      TK_LIDENT => to_repr("LIDENT")
      TK_UIDENT => to_repr("UIDENT")
      TK_POST_LABEL => to_repr("POST_LABEL")
      TK_COMMENT => to_repr("COMMENT")
      TK_NEWLINE => to_repr("NEWLINE")
      TK_INFIX1 => to_repr("INFIX1")
      TK_INFIX2 => to_repr("INFIX2")
      TK_INFIX3 => to_repr("INFIX3")
      TK_INFIX4 => to_repr("INFIX4")
      TK_AUGMENTED_ASSIGNMENT => to_repr("AUGMENTED_ASSIGNMENT")
      TK_PLUS_EQUAL => to_repr("+=")
      TK_EOF => to_repr("EOF")
      TK_FALSE => to_repr("FALSE")
      TK_TRUE => to_repr("TRUE")
      TK_PUB => to_repr("pub")
      TK_PRIV => to_repr("priv")
      TK_READONLY => to_repr("readonly")
      TK_IMPORT => to_repr("import")
      TK_EXTERN => to_repr("extern")
      TK_BREAK => to_repr("break")
      TK_CONTINUE => to_repr("continue")
      TK_STRUCT => to_repr("struct")
      TK_ENUM => to_repr("enum")
      TK_TRAIT => to_repr("trait")
      TK_DERIVE => to_repr("derive")
      TK_IMPL => to_repr("impl")
      TK_WITH => to_repr("with")
      TK_RAISE => to_repr("raise")
      TK_THROW => to_repr("throw")
      TK_TRY => to_repr("try")
      TK_CATCH => to_repr("catch")
      TK_ASYNC => to_repr("async")
      TK_TYPEALIAS => to_repr("typealias")
      TK_TRAITALIAS => to_repr("traitalias")
      TK_FNALIAS => to_repr("fnalias")
      TK_EQUAL => to_repr("=")
      TK_EQ_TILDE => to_repr("=~")
      TK_LPAREN => to_repr("(")
      TK_RPAREN => to_repr(")")
      TK_COMMA => to_repr(",")
      TK_MINUS => to_repr("-")
      TK_QUESTION => to_repr("?")
      TK_EXCLAMATION => to_repr("!")
      TK_DOT_LIDENT => to_repr("DOT_LIDENT")
      TK_DOT_UIDENT => to_repr("DOT_UIDENT")
      TK_DOT_INT => to_repr("DOT_INT")
      TK_DOT_LPAREN => to_repr(".(")
      TK_COLONCOLON => to_repr("::")
      TK_COLON => to_repr(":")
      TK_SEMI => to_repr("SEMI")
      TK_LBRACKET => to_repr("[")
      TK_LBRACKET_BAR => to_repr("[|")
      TK_PLUS => to_repr("+")
      TK_RBRACKET => to_repr("]")
      TK_BAR_RBRACKET => to_repr("|]")
      TK_UNDERSCORE => to_repr("_")
      TK_BAR => to_repr("|")
      TK_LBRACE => to_repr("{")
      TK_RBRACE => to_repr("}")
      TK_AMPERAMPER => to_repr("&&")
      TK_AMPER => to_repr("&")
      TK_CARET => to_repr("^")
      TK_BARBAR => to_repr("||")
      TK_PACKAGE_NAME => to_repr("PACKAGE_NAME")
      TK_AS => to_repr("as")
      TK_PIPE => to_repr("|>")
      TK_PIPE_LEFT => to_repr("<|")
      TK_LT_PLUS => to_repr("<+")
      TK_LT_QUESTION => to_repr("<?")
      TK_ELSE => to_repr("else")
      TK_FN => to_repr("fn")
      TK_IF => to_repr("if")
      TK_LET => to_repr("let")
      TK_PROOF_ASSERT => to_repr("proof_assert")
      TK_PROOF_LET => to_repr("proof_let")
      TK_CONST => to_repr("const")
      TK_MATCH => to_repr("match")
      TK_USING => to_repr("using")
      TK_MUTABLE => to_repr("mut")
      TK_TYPE => to_repr("type")
      TK_FAT_ARROW => to_repr("=>")
      TK_THIN_ARROW => to_repr("->")
      TK_WHILE => to_repr("while")
      TK_RETURN => to_repr("return")
      TK_DOTDOT => to_repr("..")
      TK_RANGE_INCLUSIVE => to_repr("..=")
      TK_RANGE_LT_INCLUSIVE => to_repr("..<=")
      TK_RANGE_EXCLUSIVE => to_repr("..<")
      TK_RANGE_INCLUSIVE_REV => to_repr(">=..")
      TK_RANGE_EXCLUSIVE_REV => to_repr(">..")
      TK_ELLIPSIS => to_repr("...")
      TK_TEST => to_repr("test")
      TK_LOOP => to_repr("loop")
      TK_GUARD => to_repr("guard")
      TK_DEFER => to_repr("defer")
      TK_FOR => to_repr("for")
      TK_IN => to_repr("in")
      TK_IS => to_repr("is")
      TK_SUBERROR => to_repr("suberror")
      TK_AND => to_repr("and")
      TK_FORALL => to_repr("∀")
      TK_EXISTS => to_repr("∃")
      TK_IMPLIES => to_repr("→")
      TK_LETREC => to_repr("letrec")
      TK_ENUMVIEW => to_repr("enumview")
      TK_EXTENUM => to_repr("extenum")
      TK_DECLARE => to_repr("declare")
      TK_NORAISE => to_repr("noraise")
      TK_NOBREAK => to_repr("nobreak")
      TK_WHERE => to_repr("where")
      TK_TRY_QUESTION => to_repr("try?")
      TK_TRY_EXCLAMATION => to_repr("try!")
      TK_LEXMATCH => to_repr("lexmatch")
      TK_LEXMATCH_QUESTION => to_repr("lexmatch?")
      TK_LEXSCAN => to_repr("lexscan")
      TK_EXTEND => to_repr("extend")
      TK_PACKAGE => to_repr("package")
    }
}

