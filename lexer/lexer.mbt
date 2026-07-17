///|
let use_utf16_location : Ref[Bool] = Ref(false)

///|
fn lex_tokens(
  input : StringView,
  base~ : StringView,
  env~ : LexEnv,
  preserve_comment~ : (Comment, Int, Int) -> Unit,
) -> Unit {
  fn metavar_enabled() -> Bool {
    env.enable_metavar && !env.is_interpolation
  }

  fn reject_disabled_metavar(ch : Char) -> Unit {
    let start_pos = env.calc_offset(input, base~)
    env.add_lexing_error(
      IllegalCharacter(ch),
      start=start_pos,
      end=start_pos + 1,
    )
    lex_tokens(input[1:], base~, env~, preserve_comment~)
  }

  fn resume_disabled_dot_metavar() -> Unit {
    let dot_pos = env.calc_offset(input, base~) + 1
    env.add_token_with_loc(DOT_LIDENT(""), start=dot_pos, end=dot_pos)
    lex_tokens(input[1:], base~, env~, preserve_comment~)
  }

  fn add_metavar_token(
    tok : @tokens.Token,
    rest : StringView,
    disabled_char : Char,
    start_offset : Int,
  ) -> Unit {
    if metavar_enabled() {
      env.add_token_with_loc(
        tok,
        start=env.calc_offset(input, base~) + start_offset,
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    } else if start_offset > 0 {
      resume_disabled_dot_metavar()
    } else {
      reject_disabled_metavar(disabled_char)
    }
  }

  fn add_metavar_error(
    raw : StringView,
    rest : StringView,
    disabled_char : Char,
    start_offset : Int,
  ) -> Unit {
    if metavar_enabled() {
      env.add_lexing_error(
        InvalidMetavarSyntax(raw.to_owned()),
        start=env.calc_offset(input, base~) + start_offset,
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    } else if start_offset > 0 {
      resume_disabled_dot_metavar()
    } else {
      reject_disabled_metavar(disabled_char)
    }
  }

  fn add_metavar_error_to_rest(
    rest : StringView,
    disabled_char : Char,
    start_offset : Int,
  ) -> Unit {
    let raw_end = env.calc_offset(rest, base~) - env.calc_offset(input, base~)
    add_metavar_error(
      input.sub(start=0, end=raw_end),
      rest,
      disabled_char,
      start_offset,
    )
  }

  fn typed_metavar_payload(
    prefix : StringView,
    name : StringView,
    kind : StringView,
  ) -> String {
    "\{prefix}(\{name}:\{kind})"
  }

  fn inferred_metavar_payload(
    prefix : StringView,
    name : StringView,
  ) -> String {
    "\{prefix}\{name}"
  }

  lexscan input with longest {
    (
      re"^" +
      RE_LINE_BREAK,
      // Handle newlines
      after=rest,
    ) => {
      let start_pos = env.calc_offset(input, base~)
      let end_pos = env.calc_offset(rest, base~)
      env.add_token_with_loc(NEWLINE, start=start_pos, end=end_pos)
      env.current_bol = end_pos
      env.current_line += 1
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Handle whitespace (Unicode spaces)

    (re"^" + RE_UNICODE_SPACES1, after=rest) =>
      lex_tokens(rest, base~, env~, preserve_comment~)

    // Fat arrow
    (re"^(?:=>)", after=rest) => {
      env.add_token_with_loc(
        FAT_ARROW,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Thin arrow
    (re"^(?:->)", after=rest) => {
      env.add_token_with_loc(
        THIN_ARROW,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Comments
    (re"^" + (RE_LINE_COMMENT as raw), after=rest) => {
      let start_pos = env.calc_offset(input, base~)
      env.register_surrogate_pair(raw)
      let end_pos = env.calc_offset(rest, base~)
      let comment_text = raw.to_owned()
      if env.is_interpolation {
        env.add_lexing_error(start=start_pos, end=end_pos, InterpInvalidComment)
      }
      if env.comment {
        let comment = Comment::{
          content: comment_text,
          kind: InlineTrailing,
          consumed_by_docstring: false,
        }
        preserve_comment(comment, start_pos, end_pos)
        env.add_token_with_loc(COMMENT(comment), start=start_pos, end=end_pos)
      }
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Character literals - basic cases
    (
      re"^" +
      RE_CHAR_QUOTE +
      (RE_CHAR_CONTENT as raw) +
      RE_CHAR_QUOTE,
      after=rest,
    ) => {
      let start_pos = env.calc_offset(input, base~)
      env.register_surrogate_pair_for_char(raw)
      let end_pos = env.calc_offset(rest, base~)
      env.add_token_with_loc(CHAR([raw]), start=start_pos, end=end_pos)
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Character literals - escape sequences
    (
      re"^" +
      RE_CHAR_QUOTE +
      (RE_CHAR_ESCAPE as raw) +
      RE_CHAR_QUOTE,
      after=rest,
    ) => {
      env.add_token_with_loc(
        CHAR(raw.to_owned()),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Character literals - hex escape
    (re"^" + RE_CHAR_QUOTE + (RE_HEX_ESCAPE as raw) + RE_CHAR_QUOTE, after=rest) => {
      env.add_token_with_loc(
        CHAR(raw.to_owned()),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Character literals - octal escape
    (
      re"^" +
      RE_CHAR_QUOTE +
      (RE_OCTAL_ESCAPE as raw) +
      RE_CHAR_QUOTE,
      after=rest,
    ) => {
      env.add_token_with_loc(
        CHAR(raw.to_owned()),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Character literals - unicode escape
    (
      re"^" +
      RE_CHAR_QUOTE +
      (RE_SHORT_UNICODE_ESCAPE as raw) +
      RE_CHAR_QUOTE,
      after=rest,
    ) => {
      env.add_token_with_loc(
        CHAR(raw.to_owned()),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Character literals - unicode escape with braces
    (
      re"^" +
      RE_CHAR_QUOTE +
      (RE_BRACED_UNICODE_ESCAPE as raw) +
      RE_CHAR_QUOTE,
      after=rest,
    ) => {
      let char_text = raw.to_owned()
      env.add_token_with_loc(
        CHAR(char_text),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Regex literals - re"..."
    (re"^(?:re\")", after=rest) => {
      let start_pos = env.calc_offset(input, base~)
      let (rest, interps) = lex_string(
        rest,
        base~,
        env~,
        end_with_newline=false,
        allow_interp=true,
        start_pos~,
      )
      let tok : @tokens.Token = match interps {
        [InterpLit(repr~, ..)] => REGEX_LITERAL(repr)
        interps => REGEX_INTERP(interps)
      }
      let end_pos = env.calc_offset(rest, base~)
      env.add_token_with_loc(tok, start=start_pos, end=end_pos)
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // String literals - basic double-quoted string
    (re"^(?:\")", after=rest) => {
      let start_pos = env.calc_offset(input, base~)
      let (rest, interps) = lex_string(
        rest,
        base~,
        env~,
        end_with_newline=false,
        allow_interp=true,
        start_pos~,
      )
      let tok : @tokens.Token = match interps {
        [InterpLit(repr~, ..)] => STRING(repr)
        interps => INTERP(interps)
      }
      let end_pos = env.calc_offset(rest, base~)
      env.add_token_with_loc(tok, start=start_pos, end=end_pos)
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Byte string literal
    (re"^(?:b\")", after=rest) => {
      let start_pos = env.calc_offset(input, base~)
      let (rest, interps) = lex_string(
        rest,
        base~,
        env~,
        end_with_newline=false,
        allow_interp=true,
        start_pos~,
      )
      let tok : @tokens.Token = match interps {
        [InterpLit(repr~, ..)] => BYTES(repr)
        interps => BYTES_INTERP(interps)
      }
      env.add_token_with_loc(
        tok,
        start=start_pos,
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Byte literals - hex escape
    (re"^(?:b'\\x[0-9a-fA-F]{2}')" as raw, after=rest) => {
      let literal = raw.sub(start=2, end=raw.length() - 1).to_owned() // Remove b' and '
      env.add_token_with_loc(
        BYTE(literal),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Byte literals - octal escape
    (re"^(?:b'\\o[0-3][0-7]{2}')" as raw, after=rest) => {
      let literal = raw.sub(start=2, end=raw.length() - 1).to_owned() // Remove b' and '
      env.add_token_with_loc(
        BYTE(literal),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Byte literals - ASCII character
    (re"^(?:b'[\x00-\x7F]')" as raw, after=rest) => {
      let ascii_char = raw.sub(start=2, end=raw.length() - 1).to_owned() // Remove b' and '
      env.add_token_with_loc(
        BYTE(ascii_char),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Byte literals - escape sequences
    (re"^(?:b'\\[\\'\"`ntbrf/ ]')" as raw, after=rest) => {
      let literal = raw.sub(start=2, end=raw.length() - 1).to_owned() // Remove b' and '
      env.add_token_with_loc(
        BYTE(literal),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Invalid byte literal
    (re"^(?:b')", after=rest) => {
      let start = env.calc_offset(base~, input)
      let rest = lex_invalid_byte(rest, base~, env~, start~)
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Float literals
    (re"^" + (RE_FLOAT as float), after=rest) => {
      env.add_token_with_loc(
        FLOAT(float.to_owned()),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Double literals
    (re"^" + (RE_DOUBLE as double), after=rest) => {
      env.add_token_with_loc(
        DOUBLE(double.to_owned()),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Integer literals with range operator handling

    (re"^" + ((RE_INTEGER + re"\.\.") as integer_with_range), after=_rest) => {
      // Need to handle integer..range specially
      let integer_end = integer_with_range.length() - 2 // Remove ".."
      let integer = integer_with_range.sub(start=0, end=integer_end).to_owned()
      env.add_token_with_loc(
        INT(integer),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(input, base~) + integer_end,
      )
      // Put back the ".." part
      let dotdot_input = input[integer_end:]
      lex_tokens(dotdot_input, base~, env~, preserve_comment~)
    }

    // Regular integer literals
    (re"^" + (RE_INTEGER as integer), after=rest) => {
      env.add_token_with_loc(
        INT(integer.to_owned()),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Upper case identifiers (types) - simplified pattern for ASCII and basic Unicode

    (re"^" + (RE_UIDENT as raw), after=rest) => {
      let start_pos = env.calc_offset(input, base~)
      env.register_surrogate_pair(raw)
      let end_pos = env.calc_offset(rest, base~)
      env.add_token_with_loc(
        UIDENT(raw.to_owned()),
        start=start_pos,
        end=end_pos,
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Post label identifiers (name~) - simplified pattern
    (re"^(?:[a-z_][a-zA-Z0-9_]*~)" as raw, after=rest) => {
      let ident = raw.sub(start=0, end=raw.length() - 1).to_owned() // Remove ~
      env.add_token_with_loc(
        POST_LABEL(ident),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Lower case identifiers and keywords

    (re"^" + (RE_LIDENT as raw), after=rest) => {
      let raw_str = raw.to_owned()
      let start_pos = env.calc_offset(input, base~)
      env.register_surrogate_pair(raw)
      let end_pos = env.calc_offset(rest, base~)
      if reserved_keyword_table.contains(raw_str) {
        env.add_lexing_error(
          Reserved_keyword(raw_str),
          start=start_pos,
          end=end_pos,
        )
      }
      let token = match keyword_table.get(raw_str) {
        None => @tokens.Token::LIDENT(raw_str)
        Some(tok) => tok
      }
      env.add_token_with_loc(token, start=start_pos, end=end_pos)
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Operators and punctuation - order matters for longer operators first

    (re"^(?:\+=)", after=rest) => {
      env.add_token_with_loc(
        PLUS_EQUAL,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Augmented assignment operators
    (re"^(?:[+\-*/%]=)" as op, after=rest) => {
      let op_char = op.sub(start=0, end=1).to_owned()
      env.add_token_with_loc(
        AUGMENTED_ASSIGNMENT(op_char),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Multiple character operators
    (re"^(?:&&)", after=rest) => {
      env.add_token_with_loc(
        AMPERAMPER,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\|\|)", after=rest) => {
      env.add_token_with_loc(
        BARBAR,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\|>)", after=rest) => {
      env.add_token_with_loc(
        PIPE,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\|\])", after=rest) => {
      env.add_token_with_loc(
        BAR_RBRACKET,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:<\|)", after=rest) => {
      env.add_token_with_loc(
        PIPE_LEFT,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:<\+)", after=rest) => {
      env.add_token_with_loc(
        LT_PLUS,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:<\?)", after=rest) => {
      env.add_token_with_loc(
        LT_QUESTION,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:try\?)", after=rest) => {
      env.add_token_with_loc(
        TRY_QUESTION,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:try!)", after=rest) => {
      env.add_token_with_loc(
        TRY_EXCLAMATION,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:lexmatch\?)", after=rest) => {
      env.add_token_with_loc(
        LEXMATCH_QUESTION,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:==)", after=rest) => {
      env.add_token_with_loc(
        INFIX1("=="),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:=~)", after=rest) => {
      env.add_token_with_loc(
        EQ_TILDE,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:!=)", after=rest) => {
      env.add_token_with_loc(
        INFIX1("!="),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:<=)", after=rest) => {
      env.add_token_with_loc(
        INFIX1("<="),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:>\.\.)", after=rest) => {
      env.add_token_with_loc(
        RANGE_EXCLUSIVE_REV,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:>=\.\.)", after=rest) => {
      env.add_token_with_loc(
        RANGE_INCLUSIVE_REV,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:>=)", after=rest) => {
      env.add_token_with_loc(
        INFIX1(">="),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:<<)", after=rest) => {
      env.add_token_with_loc(
        INFIX2("<<"),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:>>)", after=rest) => {
      env.add_token_with_loc(
        INFIX2(">>"),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\u{2200})", after=rest) => {
      env.add_token_with_loc(
        FORALL,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\u{2203})", after=rest) => {
      env.add_token_with_loc(
        EXISTS,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\u{2192})", after=rest) => {
      env.add_token_with_loc(
        IMPLIES,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:::)", after=rest) => {
      env.add_token_with_loc(
        COLONCOLON,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Dots and ranges
    (re"^(?:\.\.\.)", after=rest) => {
      env.add_token_with_loc(
        ELLIPSIS,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\.\.<=)", after=rest) => {
      env.add_token_with_loc(
        RANGE_LT_INCLUSIVE,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\.\.=)", after=rest) => {
      env.add_token_with_loc(
        RANGE_INCLUSIVE,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\.\.<)", after=rest) => {
      env.add_token_with_loc(
        RANGE_EXCLUSIVE,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\.\.)", after=rest) => {
      env.add_token_with_loc(
        DOTDOT,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Dot with identifier

    // Dot metavariable labels, only enabled for pattern parsing.
    (re"^(?:\.\$\$\([^\)\r\n]*\))" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 1)
    (re"^(?:\.\$\$[A-Za-z_][A-Za-z0-9_]*)" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 1)
    (re"^(?:\.\$\$)" as raw, after=rest) => add_metavar_error(raw, rest, '$', 1)
    (
      re"^(?:\.)" +
      (RE_METAVAR_PREFIX as prefix) +
      re"(?:\()" +
      RE_METAVAR_SPACES +
      (RE_METAVAR_NAME as name) +
      RE_METAVAR_SPACES +
      re"(?::)" +
      RE_METAVAR_SPACES +
      (RE_METAVAR_KIND as kind) +
      RE_METAVAR_SPACES +
      re"(?:\))",
      after=rest,
    ) =>
      add_metavar_token(
        DOT_LIDENT(typed_metavar_payload(prefix, name, kind)),
        rest,
        '$',
        1,
      )
    (
      re"^(?:\.)" +
      (RE_METAVAR_PREFIX as prefix) +
      (RE_METAVAR_NAME as name),
      after=rest,
    ) =>
      add_metavar_token(
        DOT_LIDENT(inferred_metavar_payload(prefix, name)),
        rest,
        '$',
        1,
      )
    (re"^(?:\.\$(\$\$)?\([ ]*:)" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 1)
    (re"^(?:\.\$(\$\$)?\([^\)\r\n]*\))" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 1)
    (
      re"^(?:\.\$(\$\$)?\()" +
      re"(?:[ ]*)" +
      re"(?:[A-Za-z_][A-Za-z0-9_]*)" +
      re"(?:[ ]*)" +
      re"(?::)" +
      re"(?:[ ]*)" +
      re"(?:\))",
      after=rest,
    ) => add_metavar_error_to_rest(rest, '$', 1)
    (
      re"^(?:\.\$(\$\$)?\()" +
      re"(?:[ ]*)" +
      re"(?:[A-Za-z_][A-Za-z0-9_]*)" +
      re"(?:[ ]*)" +
      re"(?::)" +
      re"(?:[ ]*)" +
      re"(?:[a-z][a-z]*)",
      after=rest,
    ) => add_metavar_error_to_rest(rest, '$', 1)
    (re"^(?:\.\$(\$\$)?[a-z][a-z]*:[a-z_][A-Za-z0-9_]*)" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 1)
    (re"^(?:\.)" + (RE_UIDENT as ident), after=rest) => {
      let name = ident.to_owned()
      let start_pos = env.calc_offset(input, base~) + 1
      env.register_surrogate_pair(ident)
      let end_pos = env.calc_offset(rest, base~)
      env.add_token_with_loc(DOT_UIDENT(name), start=start_pos, end=end_pos)
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\.)" + (RE_OPTIONAL_LIDENT as ident), after=rest) => {
      let name = ident.to_owned()
      let dot_start = env.calc_offset(input, base~)
      let start_pos = env.calc_offset(input, base~) + 1
      env.register_surrogate_pair(ident)
      let end_pos = env.calc_offset(rest, base~)
      if name == "" {
        env.add_lexing_error(
          MissingIdentifierAfterDot,
          start=dot_start,
          end=end_pos,
        )
      }
      env.add_token_with_loc(DOT_LIDENT(name), start=start_pos, end=end_pos)
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Dot with number
    (re"^(?:\.[0-9]+)" as dot_int, after=rest) => {
      let digits_str = dot_int.sub(start=1, end=dot_int.length()) // Remove the '.'
      let idx = @string.parse_int(digits_str) catch {
        _ => {
          let full_str = dot_int.to_owned()
          env.add_lexing_error(
            InvalidDotInt(full_str),
            start=env.calc_offset(input, base~),
            end=env.calc_offset(rest, base~),
          )
          0
        }
      }
      env.add_token_with_loc(
        DOT_INT(idx),
        start=env.calc_offset(input, base~) + 1,
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Dot with parenthesis
    (re"^(?:\.\()", after=rest) => {
      env.add_token_with_loc(
        DOT_LPAREN,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Single character operators and punctuation
    (re"^(?:&)", after=rest) => {
      env.add_token_with_loc(
        AMPER,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\|)", after=rest) => {
      env.add_token_with_loc(
        BAR,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\^)", after=rest) => {
      env.add_token_with_loc(
        CARET,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\()", after=rest) => {
      env.add_token_with_loc(
        LPAREN,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\))", after=rest) => {
      env.add_token_with_loc(
        RPAREN,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\*)", after=rest) => {
      env.add_token_with_loc(
        INFIX3("*"),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:/)", after=rest) => {
      env.add_token_with_loc(
        INFIX3("/"),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:%)", after=rest) => {
      env.add_token_with_loc(
        INFIX3("%"),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:,)", after=rest) => {
      env.add_token_with_loc(
        COMMA,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?::)", after=rest) => {
      env.add_token_with_loc(
        COLON,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:;)", after=rest) => {
      env.add_token_with_loc(
        SEMI(true),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:=)", after=rest) => {
      env.add_token_with_loc(
        EQUAL,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:<)", after=rest) => {
      env.add_token_with_loc(
        INFIX1("<"),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:>)", after=rest) => {
      env.add_token_with_loc(
        INFIX1(">"),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\[\|)", after=rest) => {
      env.add_token_with_loc(
        LBRACKET_BAR,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\[)", after=rest) => {
      env.add_token_with_loc(
        LBRACKET,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\])", after=rest) => {
      env.add_token_with_loc(
        RBRACKET,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:[{])", after=rest) => {
      env.add_token_with_loc(
        LBRACE,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:[}])", after=rest) => {
      env.add_token_with_loc(
        RBRACE,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\+)", after=rest) => {
      env.add_token_with_loc(
        PLUS,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:-)", after=rest) => {
      env.add_token_with_loc(
        MINUS,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:\?)", after=rest) => {
      env.add_token_with_loc(
        QUESTION,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    (re"^(?:!)", after=rest) => {
      env.add_token_with_loc(
        EXCLAMATION,
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Attributes

    // #ident.dot_ident with payload

    (
      re"^(?:#)" +
      (re"(?:[a-zA-Z_][a-zA-Z0-9_]*)" as ident) +
      re"(?:\.)" +
      (re"(?:[a-zA-Z_][a-zA-Z0-9_]*)" as dot_ident) +
      (re"(?:[^\r\n]*)" as raw_payload),
      after=rest,
    ) => {
      if env.is_interpolation {
        env.add_lexing_error(
          start=env.calc_offset(input, base~),
          end=env.calc_offset(rest, base~),
          InterpInvalidAttribute,
        )
      } else {
        let ident = ident.to_owned()
        let dot_ident = dot_ident.to_owned()
        let raw_payload = raw_payload.to_owned()
        env.add_token_with_loc(
          ATTRIBUTE((ident, Some(dot_ident), raw_payload)),
          start=env.calc_offset(input, base~),
          end=env.calc_offset(rest, base~),
        )
      }
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // #ident with payload

    (
      re"^(?:#)" +
      (re"(?:[a-zA-Z_][a-zA-Z0-9_]*)" as ident) +
      (re"(?:[^\r\n]*)" as raw_payload),
      after=rest,
    ) => {
      if env.is_interpolation {
        env.add_lexing_error(
          start=env.calc_offset(input, base~),
          end=env.calc_offset(rest, base~),
          InterpInvalidAttribute,
        )
      } else {
        let ident = ident.to_owned()
        let raw_payload = raw_payload.to_owned()
        env.add_token_with_loc(
          ATTRIBUTE((ident, None, raw_payload)),
          start=env.calc_offset(input, base~),
          end=env.calc_offset(rest, base~),
        )
      }
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Multiline string interpolation $|
    (re"^(?:\$\|)", after=rest) => {
      if env.is_interpolation {
        env.add_lexing_error(
          start=env.calc_offset(input, base~),
          end=env.calc_offset(rest, base~),
          InterpInvalidMultilineString,
        )
      }
      let start_pos = env.calc_offset(input, base~)
      let (rest, interps) = lex_string(
        rest,
        base~,
        env~,
        end_with_newline=true,
        allow_interp=true,
        start_pos~,
      )
      let tok = @tokens.Token::MULTILINE_INTERP(interps)
      env.add_token_with_loc(
        tok,
        start=start_pos,
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Pattern metavariables, only enabled for pattern parsing.
    (re"^(?:\$\$\([^\)\r\n]*\)~?)" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 0)
    (re"^(?:\$\$[A-Za-z_][A-Za-z0-9_]*~?)" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 0)
    (re"^(?:\$\$)" as raw, after=rest) => add_metavar_error(raw, rest, '$', 0)
    (
      re"^" +
      (RE_METAVAR_PREFIX as prefix) +
      re"(?:\()" +
      RE_METAVAR_SPACES +
      (RE_METAVAR_NAME as name) +
      RE_METAVAR_SPACES +
      re"(?::)" +
      RE_METAVAR_SPACES +
      (RE_METAVAR_KIND as kind) +
      RE_METAVAR_SPACES +
      re"(?:\)~)",
      after=rest,
    ) =>
      add_metavar_token(
        POST_LABEL(typed_metavar_payload(prefix, name, kind)),
        rest,
        '$',
        0,
      )
    (
      re"^" +
      (RE_METAVAR_PREFIX as prefix) +
      (RE_METAVAR_NAME as name) +
      re"(?:~)",
      after=rest,
    ) =>
      add_metavar_token(
        POST_LABEL(inferred_metavar_payload(prefix, name)),
        rest,
        '$',
        0,
      )
    (
      re"^" +
      (RE_METAVAR_PREFIX as prefix) +
      re"(?:\()" +
      RE_METAVAR_SPACES +
      (RE_METAVAR_UIDENT as name) +
      RE_METAVAR_SPACES +
      re"(?::)" +
      RE_METAVAR_SPACES +
      (RE_METAVAR_KIND as kind) +
      RE_METAVAR_SPACES +
      re"(?:\))",
      after=rest,
    ) =>
      add_metavar_token(
        UIDENT(typed_metavar_payload(prefix, name, kind)),
        rest,
        '$',
        0,
      )
    (
      re"^" +
      (RE_METAVAR_PREFIX as prefix) +
      re"(?:\()" +
      RE_METAVAR_SPACES +
      (RE_METAVAR_LIDENT as name) +
      RE_METAVAR_SPACES +
      re"(?::)" +
      RE_METAVAR_SPACES +
      (RE_METAVAR_KIND as kind) +
      RE_METAVAR_SPACES +
      re"(?:\))",
      after=rest,
    ) =>
      add_metavar_token(
        LIDENT(typed_metavar_payload(prefix, name, kind)),
        rest,
        '$',
        0,
      )
    (
      re"^" +
      (RE_METAVAR_PREFIX as prefix) +
      (RE_METAVAR_UIDENT as name),
      after=rest,
    ) =>
      add_metavar_token(
        UIDENT(inferred_metavar_payload(prefix, name)),
        rest,
        '$',
        0,
      )
    (
      re"^" +
      (RE_METAVAR_PREFIX as prefix) +
      (RE_METAVAR_LIDENT as name),
      after=rest,
    ) =>
      add_metavar_token(
        LIDENT(inferred_metavar_payload(prefix, name)),
        rest,
        '$',
        0,
      )
    (re"^(?:\$(\$\$)?\([ ]*:)" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 0)
    (re"^(?:\$(\$\$)?\([^\)\r\n]*\))" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 0)
    (
      re"^(?:\$(\$\$)?\()" +
      re"(?:[ ]*)" +
      re"(?:[A-Za-z_][A-Za-z0-9_]*)" +
      re"(?:[ ]*)" +
      re"(?::)" +
      re"(?:[ ]*)" +
      re"(?:\))",
      after=rest,
    ) => add_metavar_error_to_rest(rest, '$', 0)
    (
      re"^(?:\$(\$\$)?\()" +
      re"(?:[ ]*)" +
      re"(?:[A-Za-z_][A-Za-z0-9_]*)" +
      re"(?:[ ]*)" +
      re"(?::)" +
      re"(?:[ ]*)" +
      re"(?:[a-z][a-z]*)",
      after=rest,
    ) => add_metavar_error_to_rest(rest, '$', 0)
    (
      re"^(?:\$(\$\$)?\()" +
      re"(?:[ ]*)" +
      re"(?:[A-Za-z_][A-Za-z0-9_]*)" +
      re"(?:[ ]*)" +
      re"(?:\))",
      after=rest,
    ) => add_metavar_error_to_rest(rest, '$', 0)
    (re"^(?:\$(\$\$)?\([ ]*\))" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 0)
    (re"^(?:\$(\$\$)?[a-z][a-z]*:[A-Za-z_][A-Za-z0-9_]*~)" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 0)
    (re"^(?:\$(\$\$)?[a-z][a-z]*:[a-z_][A-Za-z0-9_]*)" as raw, after=rest) =>
      add_metavar_error(raw, rest, '$', 0)

    // Multiline string #|
    (re"^(?:#\|[^\r\n]*)" as multiline_str, after=rest) => {
      let start_pos = env.calc_offset(input, base~)
      env.register_surrogate_pair(multiline_str)
      let end_pos = env.calc_offset(rest, base~)
      if env.is_interpolation {
        env.add_lexing_error(
          start=start_pos,
          end=end_pos,
          InterpInvalidMultilineString,
        )
      }
      let content_str = multiline_str.to_owned()
      let content = content_str.sub(start=2, end=content_str.length()) // Remove #|
      env.add_token_with_loc(
        MULTILINE_STRING(content.to_owned()),
        start=start_pos,
        end=end_pos,
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // Package name @package/name
    (
      re"^(?:@[a-zA-Z_][a-zA-Z0-9_\-]*(/[a-zA-Z_][a-zA-Z0-9_\-]*)*)" as raw,
      after=rest,
    ) => {
      let pkg_name_without_at = raw.sub(start=1, end=raw.length()).to_owned()
      env.add_token_with_loc(
        PACKAGE_NAME(pkg_name_without_at),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(rest, base~),
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }

    // EOF case
    (re"^(?:$)", after=_) => {
      let end_pos = env.calc_offset(input, base~)
      env.add_token_with_loc(EOF, start=end_pos, end=end_pos)
    }

    // Error case - any remaining character
    (re"^(?:.)" as c, after=rest) => {
      env.add_lexing_error(
        IllegalCharacter(c),
        start=env.calc_offset(input, base~),
        end=env.calc_offset(input, base~) + 1,
      )
      lex_tokens(rest, base~, env~, preserve_comment~)
    }
    _ => panic()
  }
}

///|
fn interpolation_write_view(
  env : LexEnv,
  buf : StringBuilder,
  raw : StringView,
) -> Unit {
  env.register_surrogate_pair(raw)
  buf.write_view(raw)
}

///|
fn interpolation_write_char(
  env : LexEnv,
  buf : StringBuilder,
  c : Char,
) -> Unit {
  env.register_surrogate_pair_for_char(c)
  buf.write_char(c)
}

///|
fn add_interpolation_lexing_error(
  env : LexEnv,
  base : StringView,
  input : StringView,
  rest : StringView,
  err : LexicalError,
) -> Unit {
  env.add_lexing_error(
    start=env.calc_offset(base~, input),
    end=env.calc_offset(base~, rest),
    err,
  )
}

///|
fn scan_interpolation_source(
  input : StringView,
  base~ : StringView,
  env~ : LexEnv,
  buf~ : StringBuilder,
  brace_depth~ : Int,
  preserve~ : Bool,
) -> (StringView, Int) {
  lexscan input with longest {
    (re"^" + ((RE_UNICODE_SPACES + re"\}") as raw), after=rest) =>
      if brace_depth == 0 {
        if preserve {
          interpolation_write_view(env, buf, raw)
          (rest, env.calc_offset(base~, rest))
        } else {
          (rest, env.calc_offset(base~, input))
        }
      } else {
        interpolation_write_view(env, buf, raw)
        scan_interpolation_source(
          rest,
          base~,
          env~,
          buf~,
          brace_depth=brace_depth - 1,
          preserve~,
        )
      }
    re"^$" => {
      env.add_lexing_error(
        start=env.calc_offset(base~, input),
        end=env.calc_offset(base~, input),
        UnterminatedString,
      )
      (input, env.calc_offset(base~, input))
    }
    (re"^" + RE_ASCII_LINE_BREAK, after=_) => {
      env.add_lexing_error(
        start=env.calc_offset(base~, input),
        end=env.calc_offset(base~, input),
        UnterminatedStringInVariableInterploation,
      )
      (input, env.calc_offset(base~, input))
    }
    (re"^(?:b')" as raw, after=rest) => {
      interpolation_write_view(env, buf, raw)
      scan_interpolation_char_atom(
        rest,
        base~,
        env~,
        buf~,
        brace_depth~,
        preserve~,
        escaped=false,
      )
    }
    (re"^(?:')" as raw, after=rest) => {
      interpolation_write_char(env, buf, raw)
      scan_interpolation_char_atom(
        rest,
        base~,
        env~,
        buf~,
        brace_depth~,
        preserve~,
        escaped=false,
      )
    }
    (re"^(?:b\")" as raw, after=rest) => {
      interpolation_write_view(env, buf, raw)
      scan_interpolation_string_atom(
        rest,
        base~,
        env~,
        buf~,
        brace_depth~,
        preserve~,
        allow_interp=true,
        escaped=false,
      )
    }
    (re"^(?:re\")" as raw, after=rest) => {
      interpolation_write_view(env, buf, raw)
      scan_interpolation_string_atom(
        rest,
        base~,
        env~,
        buf~,
        brace_depth~,
        preserve~,
        allow_interp=true,
        escaped=false,
      )
    }
    (re"^(?:\")" as raw, after=rest) => {
      interpolation_write_char(env, buf, raw)
      scan_interpolation_string_atom(
        rest,
        base~,
        env~,
        buf~,
        brace_depth~,
        preserve~,
        allow_interp=true,
        escaped=false,
      )
    }
    (re"^" + (RE_LINE_COMMENT as raw), after=rest) => {
      add_interpolation_lexing_error(
        env,
        base,
        input,
        rest,
        InterpInvalidComment,
      )
      interpolation_write_view(env, buf, raw)
      scan_interpolation_source(
        rest,
        base~,
        env~,
        buf~,
        brace_depth~,
        preserve~,
      )
    }
    (re"^(?:\$\|[^\r\n]*)" as raw, after=rest) => {
      add_interpolation_lexing_error(
        env,
        base,
        input,
        rest,
        InterpInvalidMultilineString,
      )
      interpolation_write_view(env, buf, raw)
      scan_interpolation_source(
        rest,
        base~,
        env~,
        buf~,
        brace_depth~,
        preserve~,
      )
    }
    (re"^(?:#\|[^\r\n]*)" as raw, after=rest) => {
      add_interpolation_lexing_error(
        env,
        base,
        input,
        rest,
        InterpInvalidMultilineString,
      )
      interpolation_write_view(env, buf, raw)
      scan_interpolation_source(
        rest,
        base~,
        env~,
        buf~,
        brace_depth~,
        preserve~,
      )
    }
    (re"^(?:#[a-zA-Z_][a-zA-Z0-9_]*[^\r\n]*)" as raw, after=rest) => {
      add_interpolation_lexing_error(
        env,
        base,
        input,
        rest,
        InterpInvalidAttribute,
      )
      interpolation_write_view(env, buf, raw)
      scan_interpolation_source(
        rest,
        base~,
        env~,
        buf~,
        brace_depth~,
        preserve~,
      )
    }
    (re"^(?:[{])" as raw, after=rest) => {
      interpolation_write_char(env, buf, raw)
      scan_interpolation_source(
        rest,
        base~,
        env~,
        buf~,
        brace_depth=brace_depth + 1,
        preserve~,
      )
    }
    (re"^(?:.)" as c, after=rest) => {
      interpolation_write_char(env, buf, c)
      scan_interpolation_source(
        rest,
        base~,
        env~,
        buf~,
        brace_depth~,
        preserve~,
      )
    }
    _ => panic()
  }
}

///|
fn scan_interpolation_string_atom(
  input : StringView,
  base~ : StringView,
  env~ : LexEnv,
  buf~ : StringBuilder,
  brace_depth~ : Int,
  preserve~ : Bool,
  allow_interp~ : Bool,
  escaped~ : Bool,
) -> (StringView, Int) {
  lexscan input with longest {
    re"^$" => {
      env.add_lexing_error(
        start=env.calc_offset(base~, input),
        end=env.calc_offset(base~, input),
        UnterminatedString,
      )
      (input, env.calc_offset(base~, input))
    }
    (re"^" + RE_ASCII_LINE_BREAK, after=_) => {
      env.add_lexing_error(
        start=env.calc_offset(base~, input),
        end=env.calc_offset(base~, input),
        UnterminatedStringInVariableInterploation,
      )
      (input, env.calc_offset(base~, input))
    }
    (re"^(?:\\[{])" as raw, after=rest) => {
      interpolation_write_view(env, buf, raw)
      if allow_interp && !escaped {
        let (rest, _) = scan_interpolation_source(
          rest,
          base~,
          env~,
          buf~,
          brace_depth=0,
          preserve=true,
        )
        scan_interpolation_string_atom(
          rest,
          base~,
          env~,
          buf~,
          brace_depth~,
          preserve~,
          allow_interp~,
          escaped=false,
        )
      } else {
        scan_interpolation_string_atom(
          rest,
          base~,
          env~,
          buf~,
          brace_depth~,
          preserve~,
          allow_interp~,
          escaped=false,
        )
      }
    }
    (re"^(?:.)" as c, after=rest) => {
      interpolation_write_char(env, buf, c)
      match (c.to_int(), escaped) {
        (34, false) =>
          scan_interpolation_source(
            rest,
            base~,
            env~,
            buf~,
            brace_depth~,
            preserve~,
          )
        (92, false) =>
          scan_interpolation_string_atom(
            rest,
            base~,
            env~,
            buf~,
            brace_depth~,
            preserve~,
            allow_interp~,
            escaped=true,
          )
        _ =>
          scan_interpolation_string_atom(
            rest,
            base~,
            env~,
            buf~,
            brace_depth~,
            preserve~,
            allow_interp~,
            escaped=false,
          )
      }
    }
    _ => panic()
  }
}

///|
fn scan_interpolation_char_atom(
  input : StringView,
  base~ : StringView,
  env~ : LexEnv,
  buf~ : StringBuilder,
  brace_depth~ : Int,
  preserve~ : Bool,
  escaped~ : Bool,
) -> (StringView, Int) {
  lexscan input with longest {
    re"^$" => {
      env.add_lexing_error(
        start=env.calc_offset(base~, input),
        end=env.calc_offset(base~, input),
        UnterminatedString,
      )
      (input, env.calc_offset(base~, input))
    }
    (re"^" + RE_ASCII_LINE_BREAK, after=_) => {
      env.add_lexing_error(
        start=env.calc_offset(base~, input),
        end=env.calc_offset(base~, input),
        UnterminatedStringInVariableInterploation,
      )
      (input, env.calc_offset(base~, input))
    }
    (re"^(?:.)" as c, after=rest) => {
      interpolation_write_char(env, buf, c)
      match (c.to_int(), escaped) {
        (39, false) =>
          scan_interpolation_source(
            rest,
            base~,
            env~,
            buf~,
            brace_depth~,
            preserve~,
          )
        (92, false) =>
          scan_interpolation_char_atom(
            rest,
            base~,
            env~,
            buf~,
            brace_depth~,
            preserve~,
            escaped=true,
          )
        _ =>
          scan_interpolation_char_atom(
            rest,
            base~,
            env~,
            buf~,
            brace_depth~,
            preserve~,
            escaped=false,
          )
      }
    }
    _ => panic()
  }
}

///|
fn lex_invalid_byte(
  input : StringView,
  base~ : StringView,
  env~ : LexEnv,
  start~ : Int,
) -> StringView {
  let invalid_byte_repr_buf = StringBuilder::new()
  fn add_invalid_byte(rest : StringView) -> StringView {
    let payload = invalid_byte_repr_buf.to_string()
    let end_pos = env.calc_offset(base~, rest)
    env.add_lexing_error(InvalidByteLiteral(payload), start~, end=end_pos)
    env.add_token_with_loc(BYTE(payload), start~, end=end_pos)
    rest
  }

  fn process_invalid_byte(input : StringView) -> StringView {
    lexscan input with longest {
      (re"^(?:')", after=rest) => add_invalid_byte(rest)
      (re"^" + RE_ASCII_LINE_BREAK, after=_) => {
        env.add_lexing_error(
          start=env.calc_offset(base~, input),
          end=env.calc_offset(base~, input),
          UnterminatedStringInVariableInterploation,
        )
        input
      }
      re"^$" => add_invalid_byte(input)
      (re"^(?:.)" as c, after=rest) => {
        env.register_surrogate_pair_for_char(c)
        invalid_byte_repr_buf.write_char(c)
        process_invalid_byte(rest)
      }
      _ => panic()
    }
  }

  process_invalid_byte(input)
}

///|
fn lex_string(
  input : StringView,
  base~ : StringView,
  env~ : LexEnv,
  end_with_newline~ : Bool,
  allow_interp~ : Bool,
  start_pos~ : Int,
) -> (StringView, Array[InterpElem]) {
  let string_repr_buf = StringBuilder::new()
  let interps = []
  fn add_literal(repr : String, start : Int, end : Int) -> Unit {
    interps.push(
      @tokens.InterpElem::InterpLit(repr~, loc={
        start: env.make_pos(start),
        end: env.make_pos(end),
      }),
    )
  }

  fn process_string(input : StringView, start_pos : Int) -> StringView {
    lexscan input with longest {
      (
        re"^(?:\")",
        // End of string
        after=rest,
      ) =>
        if end_with_newline {
          string_repr_buf.write_char('"')
          process_string(rest, start_pos)
        } else {
          if !string_repr_buf.is_empty() {
            add_literal(
              string_repr_buf.to_string(),
              start_pos,
              env.calc_offset(base~, rest),
            )
          }
          rest
        }

      // Escape sequences
      (re"^" + (RE_STRING_ESCAPE as raw), after=rest) => {
        string_repr_buf.write_string(raw.to_owned())
        process_string(rest, start_pos)
      }

      // Hex escape
      (re"^" + (RE_HEX_ESCAPE as raw), after=rest) => {
        string_repr_buf.write_string(raw.to_owned())
        process_string(rest, start_pos)
      }

      // Octal escape
      (re"^" + (RE_OCTAL_ESCAPE as raw), after=rest) => {
        string_repr_buf.write_string(raw.to_owned())
        process_string(rest, start_pos)
      }

      // Unicode escape
      (re"^" + (RE_SHORT_UNICODE_ESCAPE as raw), after=rest) => {
        string_repr_buf.write_string(raw.to_owned())
        process_string(rest, start_pos)
      }

      // Unicode escape with braces
      (re"^" + (RE_BRACED_UNICODE_ESCAPE as raw), after=rest) => {
        string_repr_buf.write_string(raw.to_owned())
        process_string(rest, start_pos)
      }

      // String interpolation

      (re"^" + ((re"\\[{]" + RE_UNICODE_SPACES) as raw), after=rest) =>
        if allow_interp {
          if !string_repr_buf.is_empty() {
            add_literal(
              string_repr_buf.to_string(),
              start_pos,
              env.calc_offset(base~, input),
            )
          }
          string_repr_buf.reset()
          let interp_start_pos = env.make_pos(env.calc_offset(base~, rest))
          let (rest, interp_end) = scan_interpolation_source(
            rest,
            base~,
            env~,
            buf=string_repr_buf,
            brace_depth=0,
            preserve=false,
          )
          let loc = Location::{
            start: interp_start_pos,
            end: env.make_pos(interp_end),
          }
          if string_repr_buf.is_empty() {
            env.add_lexing_error(
              start=env.calc_offset(base~, input),
              end=env.calc_offset(base~, rest),
              InterpMissingExpression,
            )
          } else {
            let source = string_repr_buf.to_string()
            interps.push(InterpSource({ source, loc }))
          }
          string_repr_buf.reset()
          process_string(rest, env.calc_offset(base~, rest))
        } else {
          string_repr_buf.write_string(raw.to_owned())
          process_string(rest, env.calc_offset(base~, rest))
        }

      // EOF
      re"^$" => {
        env.add_lexing_error(
          start=start_pos,
          end=env.calc_offset(base~, input),
          UnterminatedString,
        )
        if !string_repr_buf.is_empty() {
          add_literal(
            string_repr_buf.to_string(),
            start_pos,
            env.calc_offset(base~, input),
          )
        }
        input
      }

      // CRLF
      (re"^" + RE_ASCII_LINE_BREAK, after=rest) => {
        let end_pos = env.calc_offset(base~, rest)
        if !end_with_newline {
          env.add_lexing_error(start=start_pos, end=end_pos, UnterminatedString)
        }
        if !string_repr_buf.is_empty() {
          add_literal(string_repr_buf.to_string(), start_pos, end_pos)
        }
        // Need to back off to handle NEWLINE token
        input
      }

      // Any other character
      (re"^(?:.)" as c, after=rest) => {
        env.register_surrogate_pair_for_char(c)
        string_repr_buf.write_char(c)
        process_string(rest, start_pos)
      }
      _ => panic()
    }
  }

  let rest = process_string(input, start_pos)
  if interps.length() == 0 {
    let interps : Array[InterpElem] = [
      InterpLit(repr="", loc={
        start: env.make_pos(start_pos),
        end: env.make_pos(env.calc_offset(base~, rest)),
      }),
    ]
    (rest, interps)
  } else {
    (rest, interps)
  }
}
