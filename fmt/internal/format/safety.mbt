///|
priv struct StrictParseResult {
  impls : @syntax.Impls
  tokens : @tokens.Triples
  ordinary_comments : Array[String]
}

///|
fn ordinary_comment_contents(tokens : @tokens.Triples) -> Array[String] {
  let comments = []
  for triple in tokens {
    match triple.0 {
      COMMENT(comment) if !comment.consumed_by_docstring =>
        comments.push(comment.content)
      _ => ()
    }
  }
  comments
}

///|
fn strict_parse(source : String) -> StrictParseResult? {
  let { tokens, docstrings, errors } = @lexer.tokens_from_string(
    comment=true,
    source,
  )
  if !errors.is_empty() {
    return None
  }
  let (impls, reports) = @handrolled_parser.parse(tokens)
  if !reports.is_empty() || has_recovered_hole(impls) {
    return None
  }
  attach_docstrings(docstrings, impls, tokens)
  Some({ impls, tokens, ordinary_comments: ordinary_comment_contents(tokens) })
}

///|
fn canonical_ast_kind(kind : String) -> String {
  match kind {
    "ArgumentKind::LabelledPun" => "ArgumentKind::Labelled"
    "ArgumentKind::LabelledOptionPun" => "ArgumentKind::LabelledOption"
    _ => kind
  }
}

///|
fn normalize_attribute_raw(raw : String) -> String {
  let buf = StringBuilder(size_hint=raw.length())
  let mut quoted = false
  let mut escaped = false
  for ch in raw {
    if quoted {
      buf.write_char(ch)
      if escaped {
        escaped = false
      } else if ch == '\\' {
        escaped = true
      } else if ch == '"' {
        quoted = false
      }
    } else if ch == '"' {
      quoted = true
      buf.write_char(ch)
    } else if ch is (' ' | '\t' | '\r' | '\n') {
      continue
    } else {
      buf.write_char(ch)
    }
  }
  buf.to_string()
}

///|
fn normalize_ast_json(value : Json) -> Json {
  match value {
    Object(fields) => {
      let normalized : Map[String, Json] = Map([])
      for key, value in fields {
        // Locations, grouping, trailing separators, and pun spelling do not
        // affect program meaning. All other fields, including `is_closed`,
        // remain part of the comparison.
        if key == "loc" || key == "trailing" || key == "is_pun" {
          continue
        }
        normalized[key] = if key == "kind" {
          match value {
            String(kind) => Json::string(canonical_ast_kind(kind))
            _ => normalize_ast_json(value)
          }
        } else if key == "raw" {
          match value {
            String(raw) => Json::string(normalize_attribute_raw(raw))
            _ => normalize_ast_json(value)
          }
        } else {
          normalize_ast_json(value)
        }
      }
      Json::object(normalized)
    }
    Array(items) => Json::array(items.map(normalize_ast_json))
    _ => value
  }
}

///|
fn semantic_ast(impls : @syntax.Impls) -> Json {
  let impls = RemoveGroup::process(impls)
  Json::array(
    impls.map(item => normalize_ast_json(item.json_repr())).to_array(),
  )
}

///|
fn same_semantics(left : StrictParseResult, right : StrictParseResult) -> Bool {
  semantic_ast(left.impls).equal(semantic_ast(right.impls)) &&
  left.ordinary_comments == right.ordinary_comments
}

///|
/// Test/support hook for checking the formatter's explicit normalization
/// contract without comparing source locations or surface-only syntax.
pub fn semantically_equivalent(left : String, right : String) -> Bool {
  if left == right {
    return strict_parse(left) is Some(_)
  }
  match (strict_parse(left), strict_parse(right)) {
    (Some(left), Some(right)) => same_semantics(left, right)
    _ => false
  }
}

///|
fn with_final_newline(source : String) -> String {
  source.trim_end().to_owned() + "\n"
}
