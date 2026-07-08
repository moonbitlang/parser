///|
priv struct MarkdownBlock {
  open_line : Int
  content_start : Int
  close_line : Int
  indent : String
  content : String
}

///|
fn split_lines(source : String) -> Array[String] {
  source.split("\n").map(line => line.to_owned()).to_array()
}

///|
fn line_join(lines : Array[String]) -> String {
  lines[:].join("\n")
}

///|
fn strip_prefix_or_self(line : String, prefix : String) -> String {
  if line.has_prefix(prefix) {
    line.unsafe_substring(start=prefix.length(), end=line.length())
  } else {
    line
  }
}

///|
fn check_fence_indent(line : String) -> String? {
  match line.find("```") {
    None => None
    Some(index) => {
      let info = line
        .unsafe_substring(start=index + 3, end=line.length())
        .trim()
        .to_owned()
      if info == "mbt check" {
        Some(line.unsafe_substring(start=0, end=index))
      } else {
        None
      }
    }
  }
}

///|
fn is_closing_fence(line : String, indent : String) -> Bool {
  if !line.has_prefix(indent) {
    return false
  }
  let rest = line
    .unsafe_substring(start=indent.length(), end=line.length())
    .trim()
    .to_owned()
  rest.has_prefix("```")
}

///|
fn markdown_blocks(source : String) -> Array[MarkdownBlock] {
  let lines = split_lines(source)
  let blocks : Array[MarkdownBlock] = []
  let mut i = 0
  while i < lines.length() {
    match check_fence_indent(lines[i]) {
      None => i += 1
      Some(indent) => {
        let mut j = i + 1
        while j < lines.length() && !is_closing_fence(lines[j], indent) {
          j += 1
        }
        if j < lines.length() {
          let content_lines : Array[String] = []
          for k in (i + 1)..<j {
            content_lines.push(strip_prefix_or_self(lines[k], indent))
          }
          blocks.push({
            open_line: i + 1,
            content_start: i + 2,
            close_line: j + 1,
            indent,
            content: line_join(content_lines),
          })
          i = j + 1
        } else {
          i += 1
        }
      }
    }
  }
  blocks
}

///|
fn reindent_formatted(formatted : String, indent : String) -> String {
  let lines = split_lines(formatted.trim_end().to_owned())
  lines
  .map(line => {
    if line == "" {
      indent.trim_end().to_owned()
    } else {
      indent + line
    }
  })
  .join("\n")
}

///|
fn impl_docstring(item : @syntax.Impl) -> @syntax.DocString {
  match item {
    TopExpr(_) => @syntax.DocString::empty()
    TopTypeDef({ doc, .. }) => doc
    TopFuncDef(fun_decl={ doc, .. }, ..) => doc
    TopLetDef(doc~, ..)
    | TopTest(doc~, ..)
    | TopTrait({ doc, .. })
    | TopImpl(doc~, ..)
    | TopView(doc~, ..)
    | TopImplRelation(doc~, ..)
    | TopUsing(doc~, ..) => doc
  }
}

///|
fn impl_start_line(item : @syntax.Impl) -> Int {
  let doc = impl_docstring(item)
  if doc.content.length() == 0 {
    item.loc().start.lnum
  } else {
    doc.loc.start.lnum
  }
}

///|
fn header_line_comment(
  item : @syntax.Impl,
  tokens : @tokens.Triples,
) -> String? {
  let loc = item.loc()
  for triple in tokens {
    match triple {
      (COMMENT(comment), start, _) if start.lnum == loc.start.lnum &&
        start > loc.start => return Some(comment.content)
      _ => ()
    }
  }
  None
}

///|
fn restore_header_line_comment(
  formatted : String,
  item : @syntax.Impl,
  tokens : @tokens.Triples,
) -> String {
  match header_line_comment(item, tokens) {
    None => formatted
    Some(comment) =>
      if formatted.contains(comment) {
        formatted
      } else {
        match formatted.split_once("\n") {
          Some((head, tail)) => {
            let head = head.to_owned()
            if head.contains("{") {
              head + " " + comment + "\n" + tail.to_owned()
            } else {
              formatted
            }
          }
          None => formatted
        }
      }
  }
}

///|
fn insert_header_comment(formatted : String, comment : String) -> String {
  if formatted.contains(comment) {
    return formatted
  }
  let lines = split_lines(formatted)
  for i, line in lines {
    if line.contains("{") {
      lines[i] = line + " " + comment
      return line_join(lines)
    }
  }
  formatted
}

///|
fn source_header_comment(source : String, line_number : Int) -> String? {
  let lines = split_lines(source)
  if lines.get(line_number - 1) is Some(line) {
    match (line.find("{"), line.find("//")) {
      (Some(brace), Some(comment_start)) if comment_start > brace =>
        Some(
          line
          .unsafe_substring(start=comment_start, end=line.length())
          .trim()
          .to_owned(),
        )
      _ => None
    }
  } else {
    None
  }
}

///|
fn restore_source_header_comment(
  formatted : String,
  source : String,
  line_number : Int,
) -> String {
  if source_header_comment(source, line_number) is Some(comment) {
    insert_header_comment(formatted, comment)
  } else {
    formatted
  }
}

///|
fn format_single_impl(
  item : @syntax.Impl,
  impls : @syntax.Impls,
  tokens : @tokens.Triples,
  block_line? : Bool = true,
) -> String {
  let mapper = @comment.Mapper::new(tokens, impls)
  let doc = impl_docstring(item)
  if doc.content.length() != 0 {
    let overlaps_doc = fn(c : @comment.Comment) {
      c.start >= doc.loc.start && c.end <= doc.loc.end
    }
    mapper.consume_comments(Some(Node(item.loc())), overlaps_doc) |> ignore
    let table = mapper.get_table(item.loc())
    for
      name in [
        "async", "const", "declare", "enum", "enumview", "extern", "fn", "impl",
        "let", "priv", "pub", "struct", "suberror", "test", "trait", "typealias",
        "using",
      ] {
      mapper.consume_comments(table.get_token(name), overlaps_doc) |> ignore
    }
  }
  let formatted = Fmt::new(mapper~, block_line~).impl_(item)
    |> @pp.render(width=80)
    |> s => { s.trim().to_owned() }
  restore_header_line_comment(formatted, item, tokens)
}

///|
fn format_nth_plain_impl(source : String, index : Int) -> (Int, Int, String)? {
  let (impls, reports, tokens) = parse(source)
  if reports.length() != 0 {
    return None
  }
  let items = impls.to_array()
  if items.get(index) is Some(item) {
    let loc = item.loc()
    let line_start = impl_start_line(item)
    let raw = format_single_impl(item, impls, tokens, block_line=true)
    let formatted = restore_source_header_comment(raw, source, loc.start.lnum)
    Some((line_start, loc.end.lnum, formatted))
  } else {
    None
  }
}

///|
fn format_nth_markdown_impl(
  source : String,
  index : Int,
) -> (Int, Int, String)? {
  let blocks = markdown_blocks(source)
  let mut remaining = index
  for block in blocks {
    let (impls, reports, tokens) = parse(block.content)
    if reports.length() == 0 {
      let items = impls.to_array()
      if remaining < items.length() {
        let item = items[remaining]
        let loc = item.loc()
        let line_start = block.content_start + impl_start_line(item) - 1
        let line_end = block.content_start + loc.end.lnum - 1
        let raw = format_single_impl(item, impls, tokens, block_line=true)
        let formatted = restore_source_header_comment(
          raw,
          block.content,
          loc.start.lnum,
        )
        return Some(
          (line_start, line_end, reindent_formatted(formatted, block.indent)),
        )
      }
      remaining -= items.length()
    }
  }
  None
}

///|
pub fn format_nth_impl_from_source(
  source : String,
  index : Int,
  is_markdown? : Bool = false,
) -> (Int, Int, String)? {
  if is_markdown {
    format_nth_markdown_impl(source, index)
  } else {
    format_nth_plain_impl(source, index)
  }
}

///|
pub fn format_markdown(
  source : String,
  name? : String = "",
  block_line? : Bool = false,
) -> String {
  ignore(name)
  let lines = split_lines(source)
  let blocks = markdown_blocks(source)
  let out : Array[String] = []
  let mut block_index = 0
  let mut line = 1
  while line <= lines.length() {
    if block_index < blocks.length() && line == blocks[block_index].open_line {
      let block = blocks[block_index]
      out.push(lines[line - 1])
      let formatted = format(block.content, block_line~).trim_end().to_owned()
      for formatted_line in split_lines(formatted) {
        out.push(block.indent + formatted_line)
      }
      out.push(lines[block.close_line - 1])
      line = block.close_line + 1
      block_index += 1
    } else {
      out.push(lines[line - 1])
      line += 1
    }
  }
  line_join(out)
}
