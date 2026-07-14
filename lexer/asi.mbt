///|
fn can_occur_before_semicolon(token : Token) -> Bool {
  match token {
    UIDENT(_)
    | LIDENT(_)
    | DOT_UIDENT(_)
    | DOT_LIDENT(_)
    | DOT_INT(_)
    | FLOAT(_)
    | DOUBLE(_)
    | INT(_)
    | BYTE(_)
    | BYTES(_)
    | BYTES_INTERP(_)
    | TRUE
    | FALSE
    | STRING(_)
    | MULTILINE_STRING(_)
    | MULTILINE_INTERP(_)
    | INTERP(_)
    | CHAR(_)
    | REGEX_LITERAL(_)
    | REGEX_INTERP(_)
    | RBRACE
    | RPAREN
    | RBRACKET
    | BAR_RBRACKET
    | UNDERSCORE
    | BREAK
    | CONTINUE
    | RETURN
    | THROW
    | RAISE
    | QUESTION
    | EXCLAMATION
    | RANGE_INCLUSIVE
    | RANGE_LT_INCLUSIVE
    | RANGE_EXCLUSIVE
    | RANGE_INCLUSIVE_REV
    | RANGE_EXCLUSIVE_REV
    | PIPE
    | ELLIPSIS
    | NORAISE
    | EXTEND
    | POST_LABEL(_) => true
    // ---------------------------
    // | EXCEPT
    TYPE
    | TYPEALIAS
    | SUBERROR
    | STRUCT
    | SEMI(_)
    | PLUS
    | PACKAGE_NAME(_)
    | PACKAGE
    | NEWLINE
    | MUTABLE
    | MINUS
    | MATCH
    | TRY
    | TRY_QUESTION
    | TRY_EXCLAMATION
    | CATCH
    | LPAREN
    | DOT_LPAREN
    | LET
    | LETREC
    | AND
    | CONST
    | LBRACKET
    | LBRACKET_BAR
    | LBRACE
    | INFIX1(_)
    | INFIX4(_)
    | INFIX3(_)
    | INFIX2(_)
    | IMPL
    | WITH
    | IMPORT
    | IF
    | GUARD
    | DECLARE
    | DEFER
    | WHILE
    | ASYNC
    | FN
    | FNALIAS
    | FAT_ARROW
    | THIN_ARROW
    | IN
    | EQUAL
    | AUGMENTED_ASSIGNMENT(_)
    | PLUS_EQUAL
    | EQ_TILDE
    | EOF
    | ENUM
    | EXTENUM
    | ENUMVIEW
    | ELSE
    | EXTERN
    | COMMENT(_)
    | COMMA
    | COLON
    | BARBAR
    | BAR
    | AS
    | IS
    | AMPERAMPER
    | DOTDOT
    | PUB
    | PRIV
    | READONLY
    | TRAIT
    | TRAITALIAS
    | PROOF_ASSERT
    | PROOF_LET
    | FORALL
    | EXISTS
    | IMPLIES
    | LT_PLUS
    | DERIVE
    | COLONCOLON
    | TEST
    | LOOP
    | FOR
    | AMPER
    | CARET
    | ATTRIBUTE(_)
    | USING
    | LEXMATCH
    | LEXMATCH_QUESTION
    | LEXSCAN
    | LT_QUESTION
    | WHERE
    | PIPE_LEFT
    | NOBREAK => false
  }
}

///|
fn can_occur_after_semicolon(token : Token) -> Bool {
  match token {
    UIDENT(_)
    | LIDENT(_)
    | FLOAT(_)
    | DOUBLE(_)
    | INT(_)
    | BYTE(_)
    | BYTES(_)
    | BYTES_INTERP(_)
    | TRUE
    | FALSE
    | STRING(_)
    | MULTILINE_STRING(_)
    | MULTILINE_INTERP(_)
    | INTERP(_)
    | CHAR(_)
    | REGEX_LITERAL(_)
    | REGEX_INTERP(_)
    | LBRACE
    | LPAREN
    | LBRACKET
    | LBRACKET_BAR
    | UNDERSCORE
    | BREAK
    | CONTINUE
    | RETURN
    | THROW
    | RAISE
    | TYPE
    | TYPEALIAS
    | SUBERROR
    | STRUCT
    | EXTENUM
    | TRAIT
    | TRAITALIAS
    | PACKAGE_NAME(_)
    | PACKAGE
    | MUTABLE
    | MATCH
    | TRY
    | TRY_QUESTION
    | TRY_EXCLAMATION
    | LET
    | PROOF_ASSERT
    | PROOF_LET
    | LETREC
    | CONST
    | IMPL
    | IMPORT
    | EXTERN
    | IF
    | GUARD
    | DEFER
    | DECLARE
    | WHILE
    | ASYNC
    | FN
    | FNALIAS
    | EOF
    | ENUM
    | FORALL
    | EXISTS
    | ENUMVIEW
    | PUB
    | PRIV
    | READONLY
    | TEST
    | LOOP
    | FOR
    | ELLIPSIS
    | POST_LABEL(_)
    | ATTRIBUTE(_)
    | USING
    | LEXMATCH
    | LEXSCAN
    | EXTEND => true
    // the lexer should skip these two tokens before trying to insert semi
    COMMENT(_) | NEWLINE => true
    // [MINUS] is both a binary operator and a unary operator.
    // So it may occur on the beginning of a statement.
    //
    // It is possible to *NOT* insert a semi before [MINUS].
    // If the user doesn't want a semi here, she can wrap the [MINUS] expression in parenthesis.
    //
    // The choice we take here is to be more conservative and always insert a semi before [MINUS].
    // But this may change in the future.
    MINUS => true
    // Technically speaking, these infix operators cannot occur after semi.
    // However, we want to keep their behavior consistent with [MINUS] (see above).
    //
    // This would forbid the following pattern:
    //
    //   let x = expr1
    //     + expr2
    //     * expr3
    //
    // Users are instead forced to write:
    //
    //   let x = expr1 +
    //     expr2 *
    //     expr3
    //
    // But this may change in the future.
    // For example we may want to treat the pipeline operator "|>" specially
    // if it is added in the future.
    PLUS
    | INFIX1(_)
    | INFIX2(_)
    | INFIX3(_)
    | INFIX4(_)
    | IMPLIES
    | AMPERAMPER
    | BARBAR
    | CARET
    | EXCLAMATION
    | AMPER => true
    // It is possible to have a semi before [RBRACE].
    // However, this semi is optional for statement block.
    // So we choose to not insert a semi before [RBRACE].
    //
    // Inserting semis before [RBRACE] has bad interaction with record creation.
    // The user will be forced to add a comma after the last field definition for multi-line record creation
    RBRACE => false
    // | EXCEPT
    // Note that [BAR] is both "Pattern_or" and "Bitwise_or".
    DOT_UIDENT(_)
    | DOT_LIDENT(_)
    | DOT_LPAREN
    | DOT_INT(_)
    | COLONCOLON
    | RPAREN
    | RBRACKET
    | BAR_RBRACKET
    | SEMI(_)
    | FAT_ARROW
    | THIN_ARROW
    | IN
    | PIPE
    | PIPE_LEFT
    | EQUAL
    | AUGMENTED_ASSIGNMENT(_)
    | PLUS_EQUAL
    | EQ_TILDE
    | ELSE
    | NORAISE
    | CATCH
    | COMMA
    | COLON
    | BAR
    | AS
    | IS
    | LT_PLUS
    | LT_QUESTION
    | DOTDOT
    | DERIVE
    | WITH
    | QUESTION
    | RANGE_INCLUSIVE
    | RANGE_LT_INCLUSIVE
    | RANGE_EXCLUSIVE
    | RANGE_INCLUSIVE_REV
    | RANGE_EXCLUSIVE_REV
    | AND
    | LEXMATCH_QUESTION
    | WHERE
    | NOBREAK => false
  }
}

//  Our current ASI rule requires the first token after the newline.
//  However, there can be arbitarily many newlines and comments after a newline.
//  So we use [last_unhandled_newline] to keep track of
//  the index of the last newline not processed by ASI.
//
//  [-1] indicate all newlines are already processed by ASI.

///|
priv struct ASIContext {
  mut last_unhandled_newline : Int
}

///|
fn ASIContext::new() -> ASIContext {
  { last_unhandled_newline: -1 }
}

//  [add_token] informs the presence of a new token,
//  and semicolon may be inserted somewhere after receiving this new token.
//  Our ASI logic is a eager, simple but effective one, implemented completely in lexer.
//  We try to insert a semicolon at every newline, except when:
//  - the token before the newline is not a valid token for the end of a statement
//  - the token after the newline is not a valid token for the beginning of a statement
//  There are a few exception and ambiguity cases though. See [can_occur_after_semi] for more detail.

///|
fn[C] ASIContext::add_token(
  self : ASIContext,
  tokens~ : Array[(Token, Position, Position)],
  last_unhandled_comment~ : Ref[(C, Int)?],
  next_token : Token,
) -> Unit {
  match next_token {
    COMMENT(_) => ()
    NEWLINE =>
      if self.last_unhandled_newline < 0 {
        self.last_unhandled_newline = tokens.length()
      }
    _ if self.last_unhandled_newline >= 0 => {
      // try insert
      for top = self.last_unhandled_newline - 1 {
        match top {
          0..<_ as top =>
            match tokens[top] {
              (COMMENT(_), _, _) => continue top - 1
              (last_token, _, endp) => {
                if can_occur_before_semicolon(last_token) &&
                  can_occur_after_semicolon(next_token) {
                  match (last_token, next_token) {
                    (
                      MULTILINE_STRING(_)
                      | MULTILINE_INTERP(_),
                      MULTILINE_STRING(_)
                      | MULTILINE_INTERP(_),
                    ) => ()
                    _ => {
                      tokens.insert(top + 1, (SEMI(false), endp, endp))
                      match last_unhandled_comment.val {
                        Some((c, i)) if i >= top + 1 =>
                          last_unhandled_comment.val = Some((c, i + 1))
                        _ => ()
                      }
                    }
                  }
                }
                break ()
              }
            }
          _ => break ()
        }
      }
      self.last_unhandled_newline = -1
    }
    _ => ()
  }
}
