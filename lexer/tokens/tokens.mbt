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
      TK_CHAR => Repr("CHAR")
      TK_INT => Repr("INT")
      TK_BYTE => Repr("BYTE")
      TK_BYTES => Repr("BYTES")
      TK_FLOAT => Repr("FLOAT")
      TK_DOUBLE => Repr("DOUBLE")
      TK_STRING => Repr("STRING")
      TK_MULTILINE_STRING => Repr("MULTILINE_STRING")
      TK_MULTILINE_INTERP => Repr("MULTILINE_INTERP")
      TK_INTERP => Repr("INTERP")
      TK_BYTES_INTERP => Repr("BYTES_INTERP")
      TK_REGEX_LITERAL => Repr("REGEX_LITERAL")
      TK_REGEX_INTERP => Repr("REGEX_INTERP")
      TK_ATTRIBUTE => Repr("ATTRIBUTE")
      TK_LIDENT => Repr("LIDENT")
      TK_UIDENT => Repr("UIDENT")
      TK_POST_LABEL => Repr("POST_LABEL")
      TK_COMMENT => Repr("COMMENT")
      TK_NEWLINE => Repr("NEWLINE")
      TK_INFIX1 => Repr("INFIX1")
      TK_INFIX2 => Repr("INFIX2")
      TK_INFIX3 => Repr("INFIX3")
      TK_INFIX4 => Repr("INFIX4")
      TK_AUGMENTED_ASSIGNMENT => Repr("AUGMENTED_ASSIGNMENT")
      TK_PLUS_EQUAL => Repr("+=")
      TK_EOF => Repr("EOF")
      TK_FALSE => Repr("FALSE")
      TK_TRUE => Repr("TRUE")
      TK_PUB => Repr("pub")
      TK_PRIV => Repr("priv")
      TK_READONLY => Repr("readonly")
      TK_IMPORT => Repr("import")
      TK_EXTERN => Repr("extern")
      TK_BREAK => Repr("break")
      TK_CONTINUE => Repr("continue")
      TK_STRUCT => Repr("struct")
      TK_ENUM => Repr("enum")
      TK_TRAIT => Repr("trait")
      TK_DERIVE => Repr("derive")
      TK_IMPL => Repr("impl")
      TK_WITH => Repr("with")
      TK_RAISE => Repr("raise")
      TK_THROW => Repr("throw")
      TK_TRY => Repr("try")
      TK_CATCH => Repr("catch")
      TK_ASYNC => Repr("async")
      TK_TYPEALIAS => Repr("typealias")
      TK_TRAITALIAS => Repr("traitalias")
      TK_FNALIAS => Repr("fnalias")
      TK_EQUAL => Repr("=")
      TK_EQ_TILDE => Repr("=~")
      TK_LPAREN => Repr("(")
      TK_RPAREN => Repr(")")
      TK_COMMA => Repr(",")
      TK_MINUS => Repr("-")
      TK_QUESTION => Repr("?")
      TK_EXCLAMATION => Repr("!")
      TK_DOT_LIDENT => Repr("DOT_LIDENT")
      TK_DOT_UIDENT => Repr("DOT_UIDENT")
      TK_DOT_INT => Repr("DOT_INT")
      TK_DOT_LPAREN => Repr(".(")
      TK_COLONCOLON => Repr("::")
      TK_COLON => Repr(":")
      TK_SEMI => Repr("SEMI")
      TK_LBRACKET => Repr("[")
      TK_LBRACKET_BAR => Repr("[|")
      TK_PLUS => Repr("+")
      TK_RBRACKET => Repr("]")
      TK_BAR_RBRACKET => Repr("|]")
      TK_UNDERSCORE => Repr("_")
      TK_BAR => Repr("|")
      TK_LBRACE => Repr("{")
      TK_RBRACE => Repr("}")
      TK_AMPERAMPER => Repr("&&")
      TK_AMPER => Repr("&")
      TK_CARET => Repr("^")
      TK_BARBAR => Repr("||")
      TK_PACKAGE_NAME => Repr("PACKAGE_NAME")
      TK_AS => Repr("as")
      TK_PIPE => Repr("|>")
      TK_PIPE_LEFT => Repr("<|")
      TK_LT_PLUS => Repr("<+")
      TK_LT_QUESTION => Repr("<?")
      TK_ELSE => Repr("else")
      TK_FN => Repr("fn")
      TK_IF => Repr("if")
      TK_LET => Repr("let")
      TK_PROOF_ASSERT => Repr("proof_assert")
      TK_PROOF_LET => Repr("proof_let")
      TK_CONST => Repr("const")
      TK_MATCH => Repr("match")
      TK_USING => Repr("using")
      TK_MUTABLE => Repr("mut")
      TK_TYPE => Repr("type")
      TK_FAT_ARROW => Repr("=>")
      TK_THIN_ARROW => Repr("->")
      TK_WHILE => Repr("while")
      TK_RETURN => Repr("return")
      TK_DOTDOT => Repr("..")
      TK_RANGE_INCLUSIVE => Repr("..=")
      TK_RANGE_LT_INCLUSIVE => Repr("..<=")
      TK_RANGE_EXCLUSIVE => Repr("..<")
      TK_RANGE_INCLUSIVE_REV => Repr(">=..")
      TK_RANGE_EXCLUSIVE_REV => Repr(">..")
      TK_ELLIPSIS => Repr("...")
      TK_TEST => Repr("test")
      TK_LOOP => Repr("loop")
      TK_GUARD => Repr("guard")
      TK_DEFER => Repr("defer")
      TK_FOR => Repr("for")
      TK_IN => Repr("in")
      TK_IS => Repr("is")
      TK_SUBERROR => Repr("suberror")
      TK_AND => Repr("and")
      TK_FORALL => Repr("∀")
      TK_EXISTS => Repr("∃")
      TK_IMPLIES => Repr("→")
      TK_LETREC => Repr("letrec")
      TK_ENUMVIEW => Repr("enumview")
      TK_EXTENUM => Repr("extenum")
      TK_DECLARE => Repr("declare")
      TK_NORAISE => Repr("noraise")
      TK_NOBREAK => Repr("nobreak")
      TK_WHERE => Repr("where")
      TK_TRY_QUESTION => Repr("try?")
      TK_TRY_EXCLAMATION => Repr("try!")
      TK_LEXMATCH => Repr("lexmatch")
      TK_LEXMATCH_QUESTION => Repr("lexmatch?")
      TK_LEXSCAN => Repr("lexscan")
      TK_EXTEND => Repr("extend")
      TK_PACKAGE => Repr("package")
    }
}

