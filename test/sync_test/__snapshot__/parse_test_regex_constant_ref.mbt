const COMMENT_START = re"/\*"
const COMMENT_END = re"\*/"
const BLANK_OPT = re"[ \t]*"

fn print_comment_content(s : StringView) -> Unit {
  if s lexmatch? (COMMENT_START BLANK_OPT (re".*?" as content) BLANK_OPT COMMENT_END) {
    println(content)
  }
}
