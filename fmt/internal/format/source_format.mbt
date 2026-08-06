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
  if line.find("```") is Some(index) {
    let info = line
      .unsafe_substring(start=index + 3, end=line.length())
      .trim()
      .to_owned()
    if info == "mbt check" {
      Some(line.unsafe_substring(start=0, end=index))
    } else {
      None
    }
  } else {
    None
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
    if check_fence_indent(lines[i]) is Some(indent) {
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
    } else {
      i += 1
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
    | TopUsing(doc~, ..)
    | TopExtend(doc~, ..)
    | TopImport(doc~, ..) => doc
  }
}

///|
fn impl_start_line(item : @syntax.Impl) -> Int {
  let doc = impl_docstring(item)
  if doc.loc.start.lnum <= 0 {
    item.loc().start.lnum
  } else {
    doc.loc.start.lnum
  }
}

///|
fn impl_source_range(
  source : String,
  item : @syntax.Impl,
  tokens : @tokens.Triples,
) -> String? {
  let doc = impl_docstring(item)
  let start = if doc.loc.start.lnum > 0 {
    doc.loc.start.cnum
  } else {
    item.loc().start.cnum
  }
  let item_end = item.loc().end
  let mut end = item_end.cnum
  for triple in tokens {
    match triple {
      (COMMENT(comment), start, comment_end) if comment.kind is InlineTrailing &&
        start.lnum == item_end.lnum &&
        start >= item_end => end = @cmp.maximum(end, comment_end.cnum)
      _ => ()
    }
  }
  guard start >= 0 && start <= end && end <= source.length() else {
    return None
  }
  Some(source.unsafe_substring(start~, end~))
}

///|
fn format_single_impl(
  source : String,
  item : @syntax.Impl,
  tokens : @tokens.Triples,
) -> String? {
  guard impl_source_range(source, item, tokens) is Some(fragment) else {
    return None
  }
  // Re-lexing the exact implementation/docstring range reuses the full
  // checkpoint mapper and all safety checks. It also avoids interpreting
  // `//` inside strings as comments.
  Some(format(fragment, block_line=true).trim().to_owned())
}

///|
fn format_nth_plain_impl(source : String, index : Int) -> (Int, Int, String)? {
  guard strict_parse(source) is Some(parsed) else { return None }
  let items = parsed.impls.to_array()
  if items.get(index) is Some(item) {
    let loc = item.loc()
    let line_start = impl_start_line(item)
    guard format_single_impl(source, item, parsed.tokens) is Some(formatted) else {
      return None
    }
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
    guard strict_parse(block.content) is Some(parsed) else { return None }
    let items = parsed.impls.to_array()
    if remaining < items.length() {
      let item = items[remaining]
      let loc = item.loc()
      let line_start = block.content_start + impl_start_line(item) - 1
      let line_end = block.content_start + loc.end.lnum - 1
      guard format_single_impl(block.content, item, parsed.tokens)
        is Some(formatted) else {
        return None
      }
      return Some(
        (line_start, line_end, reindent_formatted(formatted, block.indent)),
      )
    }
    remaining -= items.length()
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
