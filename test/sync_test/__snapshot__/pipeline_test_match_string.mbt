///| basic usage
fn f1(s : String, i : Int) -> Unit {
  match (s, i) {
    (['h', .._], 42) => println("start with h")
    ("world", 24) => println("world")
    ("hello", 24) => println("hello")
    _ => println("other")
  }
}

///| the very rare case where we get for_sure_yes for string literal
/// based on information from array patterns
fn f2(s : String, i : Int) -> Unit {
  match (s, i) {
    (['a'..='c'], 42) => println("a-c")
    (['a' as a], 24) => println(a)
    (['b' as b], 24) => println(b)
    ("c", 24) => println("c")
    _ => println("other")
  }
}

///| test unicode chars and pattern with all chars, which
/// should be combined into a string pattern
fn f3(s : String, i : Int) -> Unit {
  match (s, i) {
    (['🤣', '🤣', '🤣'], 42) => println("🤣🤣🤣")
    (['🤣', '🤣', .. rest], 41) => println(rest)
    ("😂hello", 40) => println("😂hello")
    _ => println("other")
  }
}

///| test optimization on array pattern
fn f4(s : String) -> Unit {
  match s {
    ['h', 'e',  .. rest,  'l', 'o'] => println(rest)
    ['a', 'b', 'c'] => println("abc")
    _ => println("other")
  }
}

///| ranges that include surrogate code units must read full chars
fn f5(s : String) -> Unit {
  match s {
    ['\u{0000}'..='\u{FFFF}'] => println("bmp range")
    _ => println("other")
  }
}

///|
fn main {
  f1("hello", 42)
  f1("hello", 24)
  f1("hhh", 24)
  f1("world", 24)
  f1("world", 42)
  f2("c", 24)
  f2("a", 24)
  f2("b", 42)
  f3("🤣🤣🤣", 42)
  f3("🤣🤣😭", 42)
  f3("🤣🤣😭", 41)
  f3("😂hello", 40)
  f3("😂hello", 39)
  f4("hello")
  f4("abc")
  f4("h")
  f5("A")
  f5("😀")
}
