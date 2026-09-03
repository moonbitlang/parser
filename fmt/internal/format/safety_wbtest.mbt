///|
fn assert_safe_format(source : String) -> String raise {
  guard strict_parse(source) is Some(original) else {
    fail("regression source must parse:\n\{source}")
  }
  let formatted = format(source, block_line=false)
  guard strict_parse(formatted) is Some(candidate) else {
    fail("formatted regression source must parse:\n\{formatted}")
  }
  if !same_semantics(original, candidate) {
    fail("formatter changed semantics:\n\{formatted}")
  }
  let second = format(formatted, block_line=false)
  if second != formatted {
    fail(
      "formatter is not idempotent:\n===== first =====\n\{formatted}===== second =====\n\{second}",
    )
  }
  formatted
}

///|
test "semantic equivalence is reflexive for declarations" {
  let source =
    #|trait Num : Show + ToJson {
    #|  add(Self,Self) -> Self
    #|}
    #|
    #|impl Trait for Type
  assert_true(semantically_equivalent(source, source))
}

///|
test "modern effects and parser forms format safely" {
  let source =
    #|fn f(callback : async () -> Unit noraise + nocancel) nocancel {
    #|  errdefer cleanup()
    #|  match value { T::_ => () }
    #|  lexmatch input { re"a" => () }
    #|  lexscan input { re"b" => () }
    #|}
  let formatted = assert_safe_format(source)
  for
    expected in [
      "nocancel", "errdefer cleanup()", "T::_", "lexmatch input", "lexscan input",
    ] {
    if !formatted.contains(expected) {
      fail(
        "modern syntax disappeared while formatting: `\{expected}`\n\{formatted}",
      )
    }
  }
}

///|
test "constructor pattern labels and optional puns preserve meaning" {
  let formatted = assert_safe_format(
    (
      #|fn f(v : T) -> T {
      #| match v { C(x=y,k~)=>C(x=y,z?,k~); _=>v }
      #|}
    ),
  )
  if !formatted.contains("C(x=y, k~)") || !formatted.contains("C(x=y, z?, k~)") {
    fail("unexpected constructor pattern formatting:\n\{formatted}")
  }
}

///|
test "closed and open record/map patterns preserve is_closed" {
  let formatted = assert_safe_format(
    (
      #|fn f(v : T) -> Int {
      #| match v {
      #|   {x:_}=>0
      #|   {x:_,..}=>1
      #|   {"x":_}=>2
      #|   {"x":_,..}=>3
      #| }
      #|}
    ),
  )
  for
    expected in [
      "{ x: _ } => 0", "{ x: _, .. } => 1", "{ \"x\": _ } => 2", "{ \"x\": _, .. } => 3",
    ] {
    if !formatted.contains(expected) {
      fail("pattern openness changed (missing `\{expected}`):\n\{formatted}")
    }
  }
}

///|
test "bitstring special constructor patterns do not abort" {
  let formatted = assert_safe_format(
    (
      #|fn f(v : Bytes) -> UInt {
      #| match v { [u8be(x),..]=>x; _=>0 }
      #|}
    ),
  )
  if !formatted.contains("u8be(x)") {
    fail("special constructor pattern was lost:\n\{formatted}")
  }
}

///|
test "all trailing block argument shapes are safe" {
  let formatted = assert_safe_format(
    (
      #|fn f() -> Unit {
      #| consume(Point::{x:1})
      #| consume({x:1,y:2})
      #| consume([1,2])
      #| consume({1:2,3:4})
      #| consume({..point,x:2})
      #| consume(fn(x){x})
      #|}
    ),
  )
  if formatted == "" {
    fail("unexpected empty formatting")
  }
}

///|
test "invalid or recovered input is returned byte-for-byte" {
  for
    source in [
      "fn f( {\n", "fn f() -> Unit { let s = \"unterminated\n}", "fn f() { match value { _ => errdefer cleanup() } }",
      "fn f() nocancel + { () }",
    ] {
    inspect(format(source, block_line=false), content=source)
  }
}

///|
test "checkpoint mapper preserves the ordinary comment matrix" {
  let source =
    #|// file header
    #|
    #|fn  f( // after parameter open
    #| x:Int, // after x
    #| // between parameters
    #| y:Int,
    #|)->Int { // after body open
    #| let empty = [ // empty array
    #| ]
    #| let record={ // record open
    #|   x:x, // field x
    #|   // between fields
    #|   y:y,
    #| }
    #| x + // after operator
    #| y // result
    #|} // after body
    #|
    #|// between top-level items
    #|fn g()->String { "https://example.com/a//b" } // URL is not a comment
    #|// end of file
  let formatted = assert_safe_format(source)
  if formatted == source {
    fail("valid commented input unexpectedly fell back unchanged")
  }
  for
    comment in [
      "// file header", "// after parameter open", "// after x", "// between parameters",
      "// after body open", "// empty array", "// record open", "// field x", "// between fields",
      "// after operator", "// result", "// after body", "// between top-level items",
      "// URL is not a comment", "// end of file",
    ] {
    if !formatted.contains(comment) {
      fail("comment was lost (`\{comment}`):\n\{formatted}")
    }
  }
  if !formatted.contains("\"https://example.com/a//b\"") {
    fail("URL string was changed:\n\{formatted}")
  }
}

///|
test "docstrings are consumed once and ordinary EOF comments survive" {
  let source =
    #|/// API docs
    #|fn  documented( )->Unit { }
    #|// ordinary eof
  let formatted = assert_safe_format(source)
  if formatted.split("/// API docs").length() != 2 ||
    formatted.split("// ordinary eof").length() != 2 {
    fail("doc or EOF comment was duplicated:\n\{formatted}")
  }
}

///|
test "checkpoint mapper covers nested delimiters and separators" {
  let source =
    #|fn nested(value:T, result:Result[
    #| // type argument
    #| Int,
    #| String,
    #|])->Unit {
    #| let tuple=( // tuple open
    #|   1, // tuple first
    #|   2,
    #| )
    #| let map={ // map open
    #|   1:2, // map entry
    #|   3:4,
    #| }
    #| consume( // call open
    #|   tuple, // call argument
    #|   map,
    #| )
    #| match value {
    #|   C( // pattern open
    #|     x=_, // pattern argument
    #|     ..
    #|   )=>()
    #|   _=>()
    #| }
    #|}
  let formatted = assert_safe_format(source)
  if formatted == source {
    fail("nested commented input unexpectedly fell back unchanged")
  }
  for
    comment in [
      "// type argument", "// tuple open", "// tuple first", "// map open", "// map entry",
      "// call open", "// call argument", "// pattern open", "// pattern argument",
    ] {
    if !formatted.contains(comment) {
      fail("nested comment was lost (`\{comment}`):\n\{formatted}")
    }
  }
}

///|
test "checkpoint mapper covers declaration containers" {
  let source =
    #|struct S { // struct open
    #| // before field
    #| first:Int; // after field
    #| second:String
    #|} // struct close
    #|
    #|enum E { // enum open
    #| // before constructor
    #| A(Int, // constructor parameter
    #| String);
    #| B
    #|} // enum close
  let formatted = assert_safe_format(source)
  if formatted == source {
    fail("commented declarations unexpectedly fell back unchanged")
  }
  for
    comment in [
      "// struct open", "// before field", "// after field", "// struct close", "// enum open",
      "// before constructor", "// constructor parameter", "// enum close",
    ] {
    if !formatted.contains(comment) {
      fail("declaration comment was lost (`\{comment}`):\n\{formatted}")
    }
  }
}

///|
test "checkpoint mapper covers imports using traits and deriving" {
  let source =
    #|import { // import open
    #| "pkg/core", // imported package
    #| "pkg/other" @other,
    #|}
    #|using @pkg { // using open
    #| foo, // using item
    #| type Bar,
    #|}
    #|trait Num { // trait open
    #| // before method
    #| add(Self, Self) -> Self // method end
    #|}
    #|struct S { value : Int } derive( // derive open
    #| Show, // derived trait
    #| Json,
    #|)
  let formatted = assert_safe_format(source)
  if formatted == source {
    fail("commented declaration containers unexpectedly fell back unchanged")
  }
  for
    comment in [
      "// import open", "// imported package", "// using open", "// using item",
      "// trait open", "// before method", "// method end", "// derive open", "// derived trait",
    ] {
    if !formatted.contains(comment) {
      fail("declaration comment was lost (`\{comment}`):\n\{formatted}")
    }
  }
}

///|
test "checkpoint mapper covers indexes slices and constraints" {
  let source =
    #|fn containers(xs : Array[Int]) -> Unit {
    #| let first = xs[ // index open
    #|   0 // index value
    #| ]
    #| let part = xs[ // slice open
    #|   0 // before colon
    #|   : // after colon
    #|   2
    #| ]
    #| let typed = ( // constraint open
    #|   first // constrained value
    #|   : Int
    #| )
    #| ignore(first)
    #| ignore(part)
    #| ignore(typed)
    #|}
  let formatted = assert_safe_format(source)
  if formatted == source {
    fail("commented expression containers unexpectedly fell back unchanged")
  }
  for
    comment in [
      "// index open", "// index value", "// slice open", "// before colon", "// after colon",
      "// constraint open", "// constrained value",
    ] {
    if !formatted.contains(comment) {
      fail("expression comment was lost (`\{comment}`):\n\{formatted}")
    }
  }
}
