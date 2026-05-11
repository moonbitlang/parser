//!build: -w -20-27

const COMMENT_START = re"/\*"
const COMMENT_END = re"\*/"

fn print_comment_content(s : StringView) -> Unit {
  if s lexmatch? (COMMENT_START (re".*?" as content) COMMENT_END) {
    println(content)
  }
}

fn main {
  print_comment_content("/* hello */")
}
