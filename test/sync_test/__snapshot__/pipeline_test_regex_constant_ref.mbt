//!build: -w -20-27

const COMMENT_START = re"/\*"
const COMMENT_END = re"\*/"

fn comment_content(s : StringView) -> StringView? {
  let content = lexscan s {
    (
      re"^" + COMMENT_START + (re".*?" as content) + COMMENT_END + re"$"
    ) => Some(content)
    _ => None
  }
  content
}

fn print_comment_content(s : StringView) -> Unit {
  if comment_content(s) is Some(content) {
    println(content)
  }
}

fn main {
  print_comment_content("/* hello */")
  if comment_content("x/* hidden */") is Some(_) {
    panic()
  }
  if comment_content("/* hidden */y") is Some(_) {
    panic()
  }
  if comment_content("x/* hidden */y") is Some(_) {
    panic()
  }
}
