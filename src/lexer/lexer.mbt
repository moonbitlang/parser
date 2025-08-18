///|
fn lex_tokens(
  input : @bytes.View,
  current~ : Int = 0,
  env~ : LexEnv,
  preserve_comment~ : (Comment, Int, Int) -> Unit,
) -> Unit {
  match input using regex {
    // Handle newlines
    ["(?utf8:\\n|\\r|\\r\\n|\\u2028|\\u2029)" as newline, .. rest] => {
      let (offset, _) = decode_utf8(newline)
      let start_pos = current
      let end_pos = current + offset
      env.add_token_with_loc(NEWLINE, start=start_pos, end=end_pos)
      env.current_bol = end_pos
      env.current_line += 1
      lex_tokens(rest, current=end_pos, env~, preserve_comment~)
    }

    // Handle whitespace (Unicode spaces)
    [
      "(?utf8:[\\u0009\\u000B\\u000C\\u0020\\u00A0\\uFEFF\\u1680\\u2000-\\u200A\\u202F\\u205F\\u3000]+)" as whitespaces,
      .. rest,
    ] => {
      let (offset, _) = decode_utf8(whitespaces)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Fat arrow
    ["=>", .. rest] => {
      env.add_token_with_loc(FAT_ARROW, start=current, end=current + 2)
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }

    // Thin arrow
    ["->", .. rest] => {
      env.add_token_with_loc(THIN_ARROW, start=current, end=current + 2)
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }

    // Comments
    ["(?utf8://[^\\r\\n]*)" as raw, .. rest] => {
      let (offset, comment_text) = decode_utf8(raw)
      if env.is_interpolation {
        env.add_lexing_error(
          start=current,
          end=current + offset,
          InterpInvalidComment,
        )
      }
      if env.comment {
        let comment = Comment::{
          content: comment_text,
          kind: InlineTrailing,
          consumed_by_docstring: false,
        }
        preserve_comment(comment, current, current + offset)
        env.add_token_with_loc(
          COMMENT(comment),
          start=current,
          end=current + offset,
        )
      }
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Character literals - basic cases
    ["'", "(?utf8:[^\\\\\\n\\r'])" as raw, "'", .. rest] => {
      env.add_token_with_loc(CHAR([raw]), start=current, end=current + 3)
      lex_tokens(rest, current=current + 3, env~, preserve_comment~)
    }

    // Character literals - escape sequences
    ["'", "\\\\[\\\\'\"ntbr ]" as raw, "'", .. rest] => {
      let (offset, lit) = decode_utf8(raw)
      env.add_token_with_loc(CHAR(lit), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Character literals - hex escape
    ["'", "\\\\x[0-9a-fA-F]{2}" as raw, "'", .. rest] => {
      let (offset, lit) = decode_utf8(raw)
      env.add_token_with_loc(CHAR(lit), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Character literals - octal escape
    ["'", "\\\\o[0-3][0-7]{2}" as raw, "'", .. rest] => {
      let (offset, lit) = decode_utf8(raw)
      env.add_token_with_loc(CHAR(lit), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Character literals - unicode escape
    ["'", "\\\\u[0-9a-fA-F]{4}" as raw, "'", .. rest] => {
      let (offset, lit) = decode_utf8(raw)
      env.add_token_with_loc(CHAR(lit), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Character literals - unicode escape with braces
    ["'", "\\\\u{[0-9a-fA-F]+}" as raw, "'", .. rest] => {
      let (offset, char_text) = decode_utf8(raw)
      let offset = offset + 2
      let hex = raw[2:-1] // Remove the 'u{' and '}'
      if char_for_hex_escape(hex) is None {
        env.add_lexing_error(
          start=current,
          end=current + offset,
          InvalidEscapeSequence(char_text),
        )
      }
      env.add_token_with_loc(
        CHAR(char_text),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // String literals - basic double-quoted string
    ["\"", .. rest] => {
      let start_pos = current
      let (current, rest, interps) = lex_string(
        rest,
        current=current + 1,
        env~,
        end_with_newline=false,
        allow_interp=true,
        start_pos=current + 1,
      )
      let tok : @tokens.Token = match interps {
        [InterpLit(repr~, ..)] => STRING(repr)
        interps => INTERP(interps)
      }
      let end_pos = current
      env.add_token_with_loc(tok, start=start_pos, end=end_pos)
      lex_tokens(rest, current~, env~, preserve_comment~)
    }

    // Byte string literal
    ["b\"", .. rest] => {
      let start_pos = current
      let (current, rest, interps) = lex_string(
        rest,
        current=current + 2,
        env~,
        end_with_newline=false,
        allow_interp=false,
        start_pos=current + 2,
      )
      let tok : @tokens.Token = match interps {
        [InterpLit(repr~, ..)] => BYTES(repr)
        _interps => panic() // byte strings don't support interpolation
      }
      env.add_token_with_loc(tok, start=start_pos, end=current)
      lex_tokens(rest, current~, env~, preserve_comment~)
    }

    // Byte literals - hex escape
    ["b'\\\\x[0-9a-fA-F]{2}'" as raw, .. rest] => {
      let (offset, raw_str) = decode_utf8(raw)
      let literal = raw_str.substring(start=2, end=raw_str.length() - 1) // Remove b' and '
      env.add_token_with_loc(BYTE(literal), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Byte literals - octal escape
    ["b'\\\\o[0-3][0-7]{2}'" as raw, .. rest] => {
      let (offset, raw_str) = decode_utf8(raw)
      let literal = raw_str.substring(start=2, end=raw_str.length() - 1) // Remove b' and '
      env.add_token_with_loc(BYTE(literal), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Byte literals - ASCII character
    ["b'[\\x00-\\x7F]'" as raw, .. rest] => {
      let (offset, raw_str) = decode_utf8(raw)
      let ascii_char = raw_str.substring(start=2, end=raw_str.length() - 1) // Remove b' and '
      env.add_token_with_loc(
        BYTE(ascii_char),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Byte literals - escape sequences
    ["b'\\\\[\\\\'\"`ntbr ]'" as raw, .. rest] => {
      let (offset, raw_str) = decode_utf8(raw)
      let literal = raw_str.substring(start=2, end=raw_str.length() - 1) // Remove b' and '
      env.add_token_with_loc(BYTE(literal), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Invalid byte literal
    ["b'", .. rest] => {
      let (current, rest) = lex_invalid_byte(
        rest,
        current=current + 2,
        start=current,
        env~,
      )
      lex_tokens(rest, current~, env~, preserve_comment~)
    }

    // Float literals
    ["[0-9](_|[0-9])*\\.[0-9_]*([eE][+-]?[0-9](_|[0-9])*)?F" as float, .. rest] => {
      let (offset, float_lit) = decode_utf8(float)
      env.add_token_with_loc(
        FLOAT(float_lit),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }
    [
      "0[xX][0-9a-fA-F](_|[0-9a-fA-F])*\\.[0-9a-fA-F_]*([pP][+-]?[0-9](_|[0-9])*)?F" as float,
      .. rest,
    ] => {
      let (offset, float) = decode_utf8(float)
      env.add_token_with_loc(FLOAT(float), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Double literals
    ["[0-9](_|[0-9])*\\.[0-9_]*([eE][+-]?[0-9](_|[0-9])*)?" as double, .. rest] => {
      let (offset, double) = decode_utf8(double)
      env.add_token_with_loc(
        DOUBLE(double),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }
    [
      "0[xX][0-9a-fA-F](_|[0-9a-fA-F])*\\.[0-9a-fA-F_]*([pP][+-]?[0-9](_|[0-9])*)?" as double,
      .. rest,
    ] => {
      let (offset, double) = decode_utf8(double)
      env.add_token_with_loc(
        DOUBLE(double),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Integer literals with range operator handling
    [
      "([0-9](_|[0-9])*|0[xX][0-9a-fA-F](_|[0-9a-fA-F])*|0[Oo][0-7](_|[0-7])*|0[Bb][01](_|[01])*)((UL|U|L|N)?)\\.\\." as integer_with_range,
      .. _rest,
    ] => {
      // Need to handle integer..range specially
      let (_, integer_str) = decode_utf8(integer_with_range)
      let integer_end = integer_str.length() - 2 // Remove ".."
      let integer = integer_str.substring(start=0, end=integer_end)
      env.add_token_with_loc(
        INT(integer),
        start=current,
        end=current + integer_end,
      )
      // Put back the ".." part
      let dotdot_input = input[integer_end:]
      lex_tokens(
        dotdot_input,
        current=current + integer_end,
        env~,
        preserve_comment~,
      )
    }

    // Regular integer literals
    ["[0-9](_|[0-9])*(UL|U|L|N)?" as integer, .. rest] => {
      let (offset, integer) = decode_utf8(integer)
      env.add_token_with_loc(INT(integer), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }
    ["0[xX][0-9a-fA-F](_|[0-9a-fA-F])*(UL|U|L|N)?" as integer, .. rest] => {
      let (offset, integer) = decode_utf8(integer)
      env.add_token_with_loc(INT(integer), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }
    ["0[Oo][0-7](_|[0-7])*(UL|U|L|N)?" as integer, .. rest] => {
      let (offset, integer) = decode_utf8(integer)
      env.add_token_with_loc(INT(integer), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }
    ["0[Bb][01](_|[01])*(UL|U|L|N)?" as integer, .. rest] => {
      let (offset, integer) = decode_utf8(integer)
      env.add_token_with_loc(INT(integer), start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Upper case identifiers (types) - simplified pattern for ASCII and basic Unicode
    [
      "(?utf8:[A-Z][\\u{30}-\\u{39}\\u{41}-\\u{5a}\\u{5f}-\\u{5f}\\u{61}-\\u{7a}\\u{a1}-\\u{ac}\\u{ae}-\\u{2af}\\u{1100}-\\u{11ff}\\u{1e00}-\\u{1eff}\\u{2070}-\\u{209f}\\u{2150}-\\u{218f}\\u{2e80}-\\u{2eff}\\u{2ff0}-\\u{2fff}\\u{3001}-\\u{30ff}\\u{31c0}-\\u{9fff}\\u{ac00}-\\u{d7ff}\\u{f900}-\\u{faff}\\u{fe00}-\\u{fe0f}\\u{fe30}-\\u{fe4f}\\u{1f000}-\\u{1fbff}\\u{20000}-\\u{2a6df}\\u{2a700}-\\u{2ebef}\\u{2f800}-\\u{2fa1f}\\u{30000}-\\u{323af}\\u{e0100}-\\u{e01ef}]*)" as raw,
      .. rest,
    ] => {
      let (offset, uident) = decode_utf8(raw)
      env.add_token_with_loc(
        @tokens.Token::UIDENT(uident),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Post label identifiers (name~) - simplified pattern
    ["(?utf8:[a-z_][a-zA-Z0-9_]*~)" as raw, .. rest] => {
      let (offset, raw_str) = decode_utf8(raw)
      let ident = raw_str.substring(start=0, end=raw_str.length() - 1) // Remove ~
      env.add_token_with_loc(
        @tokens.Token::POST_LABEL(ident),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Lower case identifiers and keywords
    [
      "(?utf8:[\\u{5f}-\\u{5f}\\u{61}-\\u{7a}\\u{a1}-\\u{ac}\\u{ae}-\\u{2af}\\u{1100}-\\u{11ff}\\u{1e00}-\\u{1eff}\\u{2070}-\\u{209f}\\u{2150}-\\u{218f}\\u{2e80}-\\u{2eff}\\u{2ff0}-\\u{2fff}\\u{3001}-\\u{30ff}\\u{31c0}-\\u{9fff}\\u{ac00}-\\u{d7ff}\\u{f900}-\\u{faff}\\u{fe00}-\\u{fe0f}\\u{fe30}-\\u{fe4f}\\u{1f000}-\\u{1fbff}\\u{20000}-\\u{2a6df}\\u{2a700}-\\u{2ebef}\\u{2f800}-\\u{2fa1f}\\u{30000}-\\u{323af}\\u{e0100}-\\u{e01ef}][\\u{30}-\\u{39}\\u{41}-\\u{5a}\\u{5f}-\\u{5f}\\u{61}-\\u{7a}\\u{a1}-\\u{ac}\\u{ae}-\\u{2af}\\u{1100}-\\u{11ff}\\u{1e00}-\\u{1eff}\\u{2070}-\\u{209f}\\u{2150}-\\u{218f}\\u{2e80}-\\u{2eff}\\u{2ff0}-\\u{2fff}\\u{3001}-\\u{30ff}\\u{31c0}-\\u{9fff}\\u{ac00}-\\u{d7ff}\\u{f900}-\\u{faff}\\u{fe00}-\\u{fe0f}\\u{fe30}-\\u{fe4f}\\u{1f000}-\\u{1fbff}\\u{20000}-\\u{2a6df}\\u{2a700}-\\u{2ebef}\\u{2f800}-\\u{2fa1f}\\u{30000}-\\u{323af}\\u{e0100}-\\u{e01ef}]*)" as raw,
      .. rest,
    ] => {
      let (offset, raw_str) = decode_utf8(raw)
      if reserved_keyword_table.contains(raw_str) {
        env.add_lexing_error(
          Reserved_keyword(raw_str),
          start=current,
          end=current + offset,
        )
      }
      let token = match keyword_table.get(raw_str) {
        None => @tokens.Token::LIDENT(raw_str)
        Some(tok) => tok
      }
      env.add_token_with_loc(token, start=current, end=current + offset)
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Operators and punctuation - order matters for longer operators first

    // Augmented assignment operators
    ["[+\\-*/%]=" as op, .. rest] => {
      let (offset, op_str) = decode_utf8(op)
      let op_char = op_str.substring(start=0, end=1)
      env.add_token_with_loc(
        @tokens.Token::AUGMENTED_ASSIGNMENT(op_char),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Multiple character operators
    ["&&", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::AMPERAMPER,
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }
    ["\\|\\|", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::BARBAR,
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }
    ["\\|>", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::PIPE,
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }
    ["try\\?", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::TRY_QUESTION,
        start=current,
        end=current + 4,
      )
      lex_tokens(rest, current=current + 4, env~, preserve_comment~)
    }
    ["try!", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::TRY_EXCLAMATION,
        start=current,
        end=current + 4,
      )
      lex_tokens(rest, current=current + 4, env~, preserve_comment~)
    }
    ["==", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX1("=="),
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }
    ["!=", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX1("!="),
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }
    ["<=", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX1("<="),
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }
    [">=", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX1(">="),
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }
    ["<<", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX2("<<"),
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }
    [">>", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX2(">>"),
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }
    ["::", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::COLONCOLON,
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }

    // Dots and ranges
    ["\\.\\.\\.", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::ELLIPSIS,
        start=current,
        end=current + 3,
      )
      lex_tokens(rest, current=current + 3, env~, preserve_comment~)
    }
    ["\\.\\.=", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::RANGE_INCLUSIVE,
        start=current,
        end=current + 3,
      )
      lex_tokens(rest, current=current + 3, env~, preserve_comment~)
    }
    ["\\.\\.<", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::RANGE_EXCLUSIVE,
        start=current,
        end=current + 3,
      )
      lex_tokens(rest, current=current + 3, env~, preserve_comment~)
    }
    ["\\.\\.", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::DOTDOT,
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }

    // Dot with identifier
    [
      "\\.",
      "(?utf8:[A-Z][\\u{30}-\\u{39}\\u{41}-\\u{5a}\\u{5f}-\\u{5f}\\u{61}-\\u{7a}\\u{a1}-\\u{ac}\\u{ae}-\\u{2af}\\u{1100}-\\u{11ff}\\u{1e00}-\\u{1eff}\\u{2070}-\\u{209f}\\u{2150}-\\u{218f}\\u{2e80}-\\u{2eff}\\u{2ff0}-\\u{2fff}\\u{3001}-\\u{30ff}\\u{31c0}-\\u{9fff}\\u{ac00}-\\u{d7ff}\\u{f900}-\\u{faff}\\u{fe00}-\\u{fe0f}\\u{fe30}-\\u{fe4f}\\u{1f000}-\\u{1fbff}\\u{20000}-\\u{2a6df}\\u{2a700}-\\u{2ebef}\\u{2f800}-\\u{2fa1f}\\u{30000}-\\u{323af}\\u{e0100}-\\u{e01ef}]*)" as ident,
      .. rest,
    ] => {
      let (offset, name) = decode_utf8(ident)
      let offset = offset + 1 // Include the dot in the offset
      env.add_token_with_loc(
        @tokens.Token::DOT_UIDENT(name),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }
    [
      "\\.",
      "(?utf8:[\\u{5f}-\\u{5f}\\u{61}-\\u{7a}\\u{a1}-\\u{ac}\\u{ae}-\\u{2af}\\u{1100}-\\u{11ff}\\u{1e00}-\\u{1eff}\\u{2070}-\\u{209f}\\u{2150}-\\u{218f}\\u{2e80}-\\u{2eff}\\u{2ff0}-\\u{2fff}\\u{3001}-\\u{30ff}\\u{31c0}-\\u{9fff}\\u{ac00}-\\u{d7ff}\\u{f900}-\\u{faff}\\u{fe00}-\\u{fe0f}\\u{fe30}-\\u{fe4f}\\u{1f000}-\\u{1fbff}\\u{20000}-\\u{2a6df}\\u{2a700}-\\u{2ebef}\\u{2f800}-\\u{2fa1f}\\u{30000}-\\u{323af}\\u{e0100}-\\u{e01ef}][\\u{30}-\\u{39}\\u{41}-\\u{5a}\\u{5f}-\\u{5f}\\u{61}-\\u{7a}\\u{a1}-\\u{ac}\\u{ae}-\\u{2af}\\u{1100}-\\u{11ff}\\u{1e00}-\\u{1eff}\\u{2070}-\\u{209f}\\u{2150}-\\u{218f}\\u{2e80}-\\u{2eff}\\u{2ff0}-\\u{2fff}\\u{3001}-\\u{30ff}\\u{31c0}-\\u{9fff}\\u{ac00}-\\u{d7ff}\\u{f900}-\\u{faff}\\u{fe00}-\\u{fe0f}\\u{fe30}-\\u{fe4f}\\u{1f000}-\\u{1fbff}\\u{20000}-\\u{2a6df}\\u{2a700}-\\u{2ebef}\\u{2f800}-\\u{2fa1f}\\u{30000}-\\u{323af}\\u{e0100}-\\u{e01ef}]*)?" as ident,
      .. rest,
    ] => {
      let (offset, name) = decode_utf8(ident)
      env.add_token_with_loc(
        @tokens.Token::DOT_LIDENT(name),
        start=current + 1,
        end=current + offset + 1,
      )
      lex_tokens(rest, current=current + offset + 1, env~, preserve_comment~)
    }

    // Dot with number
    ["\\.[0-9]+" as dot_int, .. rest] => {
      let (offset, full_str) = decode_utf8(dot_int)
      let digits_str = full_str.substring(start=1, end=full_str.length()) // Remove the '.'
      let idx = @strconv.parse_int(digits_str) catch {
        _ => {
          env.add_lexing_error(
            InvalidDotInt(full_str),
            start=current,
            end=current + offset,
          )
          0
        }
      }
      env.add_token_with_loc(
        @tokens.Token::DOT_INT(idx),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Dot with parenthesis
    ["\\.\\(", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::DOT_LPAREN,
        start=current,
        end=current + 2,
      )
      lex_tokens(rest, current=current + 2, env~, preserve_comment~)
    }

    // Single character operators and punctuation
    ["&", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::AMPER,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["\\|", .. rest] => {
      env.add_token_with_loc(@tokens.Token::BAR, start=current, end=current + 1)
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["\\^", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::CARET,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["\\(", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::LPAREN,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["\\)", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::RPAREN,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["\\*", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX3("*"),
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["/", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX3("/"),
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["%", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX3("%"),
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    [",", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::COMMA,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    [":", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::COLON,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    [";", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::SEMI(true),
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["=", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::EQUAL,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["<", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX1("<"),
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    [">", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::INFIX1(">"),
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["\\[", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::LBRACKET,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["\\]", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::RBRACKET,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["{", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::LBRACE,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["}", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::RBRACE,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["\\+", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::PLUS,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["-", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::MINUS,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["\\?", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::QUESTION,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    ["!", .. rest] => {
      env.add_token_with_loc(
        @tokens.Token::EXCLAMATION,
        start=current,
        end=current + 1,
      )
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }

    // Attributes

    // #ident.dot_ident with payload
    [
      "#",
      "[a-zA-Z_][a-zA-Z0-9_]*" as ident,
      "\\.",
      "[a-zA-Z_][a-zA-Z0-9_]*" as dot_ident,
      "(?utf8:[^\r\n]*)" as raw_payload,
      .. rest,
    ] => {
      let (offset1, ident) = decode_utf8(ident)
      let (offset2, dot_ident) = decode_utf8(dot_ident)
      let (offset3, raw_payload) = decode_utf8(raw_payload)
      let offset = offset1 + offset2 + offset3 + 2 // Include the # and .
      if env.is_interpolation {
        env.add_lexing_error(
          start=current,
          end=current + offset,
          InterpInvalidAttribute,
        )
      } else {
        env.add_token_with_loc(
          @tokens.Token::ATTRIBUTE((ident, Some(dot_ident), raw_payload)),
          start=current,
          end=current + offset,
        )
      }
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // #ident with payload
    [
      "#",
      "[a-zA-Z_][a-zA-Z0-9_]*" as ident,
      "[^\\r\\n]*" as raw_payload,
      .. rest,
    ] => {
      let (offset1, ident) = decode_utf8(ident)
      let (offset2, raw_payload) = decode_utf8(raw_payload)
      let offset = offset1 + offset2 + 1 // Include the #
      if env.is_interpolation {
        env.add_lexing_error(
          start=current,
          end=current + offset,
          InterpInvalidAttribute,
        )
      } else {
        env.add_token_with_loc(
          @tokens.Token::ATTRIBUTE((ident, None, raw_payload)),
          start=current,
          end=current + offset,
        )
      }
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Multiline string interpolation $|
    ["\\$\\|", .. rest] => {
      if env.is_interpolation {
        env.add_lexing_error(
          start=current,
          end=current + 2,
          InterpInvalidMultilineString,
        )
      }
      let start_pos = current
      let (current, rest, interps) = lex_string(
        rest,
        current=current + 2,
        env~,
        end_with_newline=true,
        allow_interp=true,
        start_pos=start_pos + 2,
      )
      let tok = @tokens.Token::MULTILINE_INTERP(interps)
      env.add_token_with_loc(tok, start=start_pos, end=current)
      lex_tokens(rest, current~, env~, preserve_comment~)
    }

    // Multiline string #|
    ["#\\|(?utf8:[^\\r\\n]*)" as multiline_str, .. rest] => {
      let (offset, content_str) = decode_utf8(multiline_str)
      if env.is_interpolation {
        env.add_lexing_error(
          start=current,
          end=current + offset,
          InterpInvalidMultilineString,
        )
      }
      let content = content_str.substring(start=2, end=content_str.length()) // Remove #|
      env.add_token_with_loc(
        @tokens.Token::MULTILINE_STRING(content),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // Package name @package/name - simplified pattern
    ["@[a-zA-Z_][a-zA-Z0-9_/]*" as pkg_name, .. rest] => {
      let (offset, pkg_str) = decode_utf8(pkg_name)
      let pkg_name_without_at = pkg_str.substring(start=1, end=pkg_str.length()) // Remove @
      env.add_token_with_loc(
        @tokens.Token::PACKAGE_NAME(pkg_name_without_at),
        start=current,
        end=current + offset,
      )
      lex_tokens(rest, current=current + offset, env~, preserve_comment~)
    }

    // EOF case
    [] => env.add_token_with_loc(EOF, start=current, end=current)

    // Error case - any remaining character
    ["(?utf8:.)" as c, .. rest] => {
      env.add_lexing_error(IllegalCharacter(c), start=current, end=current + 1)
      lex_tokens(rest, current=current + 1, env~, preserve_comment~)
    }
    _ => panic()
  }
}

///|
fn lex_invalid_byte(
  input : @bytes.View,
  current~ : Int,
  start~ : Int,
  env~ : LexEnv,
) -> (Int, @bytes.View) {
  let invalid_byte_repr_buf = StringBuilder::new()
  fn process_invalid_byte(
    input : @bytes.View,
    current : Int,
  ) -> (Int, @bytes.View) {
    match input using regex {
      ["'", .. rest] => {
        env.add_lexing_error(
          InvalidByteLiteral(invalid_byte_repr_buf.to_string()),
          start~,
          end=current + 1,
        )
        (current + 1, rest)
      }
      ["(?utf8:\\r|\\n)", .. _rest] => {
        env.add_lexing_error(
          start~,
          end=current + 1,
          UnterminatedStringInVariableInterploation,
        )
        (current + 1, input)
      }
      [] => {
        env.add_lexing_error(
          InvalidByteLiteral(invalid_byte_repr_buf.to_string()),
          start~,
          end=current,
        )
        (current, input)
      }
      ["(?utf8:.)" as c, .. rest] => {
        invalid_byte_repr_buf.write_char(c)
        process_invalid_byte(rest, current + 1)
      }
      _ => panic()
    }
  }

  process_invalid_byte(input, current)
}

///|
fn lex_string(
  input : @bytes.View,
  current~ : Int,
  env~ : LexEnv,
  end_with_newline~ : Bool,
  start_pos~ : Int,
  allow_interp~ : Bool,
) -> (Int, @bytes.View, Array[InterpElem]) {
  let string_repr_buf = StringBuilder::new()
  let interps = []
  fn add_literal(repr : String, start : Int, end : Int) -> Unit {
    interps.push(
      @tokens.InterpElem::InterpLit(repr~, loc=Location::{
        start: env.make_pos(start),
        end: env.make_pos(end),
      }),
    )
  }

  fn process_interpolation(
    input : @bytes.View,
    current : Int,
  ) -> (@bytes.View, Int) {
    match input using regex {
      [
        "(?utf8:[\\u0009\\u000B\\u000C\\u0020\\u00A0\\uFEFF\\u1680\\u2000-\\u200A\\u202F\\u205F\\u3000]*})" as raw,
        .. rest,
      ] => {
        let (offset, _) = decode_utf8(raw)
        (rest, current + offset)
      }
      [] => {
        env.add_lexing_error(start=current, end=current, UnterminatedString)
        (input, current)
      }
      ["(?utf8:\\r|\\n)", ..] => {
        env.add_lexing_error(
          start=current,
          end=current,
          UnterminatedStringInVariableInterploation,
        )
        (input, current)
      }
      ["(?utf8:[^\\n\"{}])" as c, .. rest] => {
        string_repr_buf.write_char(c)
        process_interpolation(rest, current + 1)
      }
      _ => panic()
    }
  }

  fn process_string(
    input : @bytes.View,
    current : Int,
    start_pos : Int,
  ) -> (Int, @bytes.View) {
    match input using regex {
      // End of string
      ["\"", .. rest] =>
        if end_with_newline {
          string_repr_buf.write_char('"')
          process_string(rest, current + 1, start_pos)
        } else {
          if not(string_repr_buf.is_empty()) {
            add_literal(string_repr_buf.to_string(), start_pos, current + 1)
          }
          (current + 1, rest)
        }

      // Escape sequences
      ["\\\\[\\\\'\"`ntbr ]" as raw, .. rest] => {
        let (offset, str) = decode_utf8(raw)
        string_repr_buf.write_string(str)
        process_string(rest, current + offset, start_pos)
      }

      // Hex escape
      ["\\\\x[0-9a-fA-F]{2}" as raw, .. rest] => {
        let (offset, str) = decode_utf8(raw)
        string_repr_buf.write_string(str)
        process_string(rest, current + offset, start_pos)
      }

      // Invalid hex escape
      ["\\\\x.." as raw, .. rest] => {
        let (offset, str) = decode_utf8(raw)
        env.add_lexing_error(
          start=current,
          end=current + offset,
          InvalidEscapeSequence(str),
        )
        process_string(rest, current + offset, start_pos)
      }

      // Octal escape
      ["\\\\o[0-3][0-7]{2}" as raw, .. rest] => {
        let (offset, str) = decode_utf8(raw)
        string_repr_buf.write_string(str)
        process_string(rest, current + offset, start_pos)
      }

      // Invalid octal escape
      ["\\\\o..." as raw, .. rest] => {
        let (offset, str) = decode_utf8(raw)
        env.add_lexing_error(
          start=current,
          end=current + offset,
          InvalidEscapeSequence(str),
        )
        process_string(rest, current + offset, start_pos)
      }

      // Unicode escape
      ["\\\\u[0-9a-fA-F]{4}" as raw, .. rest] => {
        let (offset, str) = decode_utf8(raw)
        string_repr_buf.write_string(str)
        process_string(rest, current + offset, start_pos)
      }

      // Unicode escape with braces
      ["\\\\u{[0-9a-fA-F]+}" as raw, .. rest] => {
        let (offset, str) = decode_utf8(raw)
        string_repr_buf.write_string(str)
        process_string(rest, current + offset, start_pos)
      }

      // Invalid unicode escape with braces
      ["\\\\u{[^}\\r\\n]*}" as raw, .. rest] => {
        let (offset, str) = decode_utf8(raw)
        env.add_lexing_error(
          start=current,
          end=current + offset,
          InvalidEscapeSequence(str),
        )
        process_string(rest, current + offset, start_pos)
      }

      // String interpolation
      [
        "(?utf8:\\\\{[\\u0009\\u000B\\u000C\\u0020\\u00A0\\uFEFF\\u1680\\u2000-\\u200A\\u202F\\u205F\\u3000]*)" as raw,
        .. rest,
      ] => {
        let (offset, str) = decode_utf8(raw)
        if allow_interp {
          if not(string_repr_buf.is_empty()) {
            add_literal(string_repr_buf.to_string(), start_pos, current)
          }
          string_repr_buf.reset()
          let interp_start_pos = env.make_pos(current + offset)
          let (rest, interp_end) = process_interpolation(rest, current + offset)
          let loc = Location::{
            start: interp_start_pos,
            end: env.make_pos(interp_end),
          }
          if string_repr_buf.is_empty() {
            env.add_lexing_error(
              start=current,
              end=current + offset,
              InterpMissingExpression,
            )
          } else {
            let source = string_repr_buf.to_string()
            interps.push(
              @tokens.InterpElem::InterpSource(InterpSource::{ source, loc }),
            )
          }
          string_repr_buf.reset()
          process_string(rest, interp_end, interp_end)
        } else {
          env.add_lexing_error(
            start=current,
            end=current + offset,
            InvalidEscapeSequence(str),
          )
          process_string(rest, current + offset, current + offset)
        }
      }
      // Invalid escape
      ["\\\\." as raw, .. rest] => {
        let (offset, str) = decode_utf8(raw)
        env.add_lexing_error(
          start=current,
          end=current + offset,
          InvalidEscapeSequence(str),
        )
        process_string(rest, current + offset, start_pos)
      }

      // EOF
      [] => {
        env.add_lexing_error(start=start_pos, end=current, UnterminatedString)
        if not(string_repr_buf.is_empty()) {
          add_literal(string_repr_buf.to_string(), start_pos, current)
        }
        (current, input)
      }

      // CRLF
      ["(?utf8:[\\r\\n])", ..] => {
        let end_pos = current
        if not(end_with_newline) {
          env.add_lexing_error(start=start_pos, end=end_pos, UnterminatedString)
        }
        if not(string_repr_buf.is_empty()) {
          add_literal(string_repr_buf.to_string(), start_pos, end_pos)
        }
        // Need to back off to handle NEWLINE token
        (current, input)
      }

      // Any other character
      ["(?utf8:.)" as c, .. rest] => {
        string_repr_buf.write_char(c)
        process_string(rest, current + 1, start_pos)
      }
      _ => panic()
    }
  }

  let (current, rest) = process_string(input, current, start_pos)
  if interps.length() == 0 {
    let interps : Array[InterpElem] = [
      @tokens.InterpElem::InterpLit(repr="", loc=Location::{
        start: env.make_pos(start_pos),
        end: env.make_pos(current),
      }),
    ]
    (current, rest, interps)
  } else {
    (current, rest, interps)
  }
}
