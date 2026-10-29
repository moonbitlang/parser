///|
fn assert_modern_syntax_format_pipeline(source : String) -> String raise {
  guard strict_parse(source) is Some(original) else {
    let lex_errors = @lexer.tokens_from_string(comment=true, source).errors
      .map(lex_error => "\{lex_error.2}")
      .join(", ")
    let (_, reports, _) = parse(source)
    fail(
      "modern syntax regression source must parse:\n\{source}\nlex: \{lex_errors}\nparse: \{Json(reports).stringify(indent=2)}",
    )
  }
  guard format_once(original.impls, original.tokens, block_line=false)
    is Some(first) else {
    fail("the first modern syntax formatting pass was rejected")
  }
  if first == source {
    fail("the first modern syntax formatting pass made no change")
  }
  guard strict_parse(first) is Some(reparsed) else {
    fail("the first modern syntax formatting result does not parse:\n\{first}")
  }
  if !same_semantics(original, reparsed) {
    fail("the first modern syntax formatting pass changed semantics:\n\{first}")
  }
  guard format_once(reparsed.impls, reparsed.tokens, block_line=false)
    is Some(second) else {
    fail("the second modern syntax formatting pass was rejected:\n\{first}")
  }
  if second != first {
    fail(
      "modern syntax formatting is not idempotent:\n===== first =====\n\{first}===== second =====\n\{second}",
    )
  }
  let public_result = format(source, block_line=false)
  if public_result != first {
    fail(
      "the public formatter fell back from a safe result:\n===== source =====\n\{source}\n===== expected =====\n\{first}===== actual =====\n\{public_result}",
    )
  }
  first
}

///|
test "inclusive range patterns normalize to ..<=" {
  for op in ["..=", "..<="] {
    let cases = [
      ("0\{op}9", "0..<=9"),
      ("-9\{op}-1", "-9..<=-1"),
      ("-9\{op}9", "-9..<=9"),
      ("'a'\{op}'z'", "'a'..<='z'"),
      ("b'a'\{op}b'z'", "b'a'..<=b'z'"),
      ("Some((0\{op}9, [-9\{op}-1]))", "Some((0..<=9, [-9..<=-1]))"),
    ]
    for case in cases {
      let (pattern, expected) = case
      for
        source in [
          "fn f(x){match x{\n\{pattern}=>true\n_=>false\n}}",
          "fn f(x){x is \{pattern}}",
        ] {
        let formatted = assert_modern_syntax_format_pipeline(source)
        if !formatted.contains(expected) || formatted.contains("..=") {
          fail("inclusive range pattern was not normalized:\n\{formatted}")
        }
      }
    }
  }
}

///|
test "exclusive range patterns retain ..<" {
  for
    pattern in [
      "0..<9", "-9..<-1", "-9..<9", "'a'..<'z'", "b'a'..<b'z'", "Some((0..<9, [-9..<-1]))",
    ] {
    for
      source in [
        "fn f(x){match x{\n\{pattern}=>true\n_=>false\n}}",
        "fn f(x){x is \{pattern}}",
      ] {
      let formatted = assert_modern_syntax_format_pipeline(source)
      if !formatted.contains(pattern) || formatted.contains("..<=") {
        fail("exclusive range pattern changed:\n\{formatted}")
      }
    }
  }
}

///|
test "record constructors format without losing semantics" {
  let simple = assert_modern_syntax_format_pipeline(
    "struct S{x:Int;fn new(x:Int)->S}",
  )
  if !simple.contains("fn new(x : Int) -> S") {
    fail("record constructor missing from formatted source:\n\{simple}")
  }
  let decorated =
    #|struct S {
    #|  x : Int
    #|  /// constructor documentation
    #|  #callsite(autofill(loc))
    #|  async fn[T : Eq] new(x~ : Int, loc~ : SourceLoc) -> S raise
    #|}
  let formatted = assert_safe_format(decorated)
  if !formatted.contains("constructor documentation") ||
    !formatted.contains("#callsite") ||
    !formatted.contains("async fn[") ||
    !formatted.contains("new(x~ : Int") {
    fail(
      "record constructor metadata missing from formatted source:\n\{formatted}",
    )
  }
}

///|
test "async default raise normalizes in every formatter context" {
  let cases = [
    (
      "arrow type", "fn  take_callback(callback:async ()->Unit raise){callback}",
      "callback : async () -> Unit",
    ),
    (
      "anonymous function", "fn  make()->Unit{let callback=async fn( )->Unit raise{()};callback()}",
      "async fn() -> Unit {",
    ),
    (
      "top-level function", "async fn  top( )->Unit raise{()}", "async fn top() -> Unit {",
    ),
    (
      "trait method", "trait AsyncTrait{async fn run(Self)->Unit raise}", "async fn run(Self) -> Unit",
    ),
  ]
  for case in cases {
    let (name, source, expected) = case
    let formatted = assert_modern_syntax_format_pipeline(source)
    if !formatted.contains(expected) || formatted.contains("raise") {
      fail("async default raise was not normalized for \{name}:\n\{formatted}")
    }
  }
}

///|
test "non-default effects are preserved" {
  let source =
    #|fn sync_effects(callback:() -> Unit raise) -> Unit raise {
    #|  let handler = fn() -> Unit raise { () }
    #|  callback()
    #|  handler()
    #|}
    #|
    #|trait SyncTrait { fn run(Self) -> Unit raise }
    #|
    #|async fn typed(callback:async () -> Unit raise DomainError) -> Unit raise DomainError {
    #|  let handler = async fn() -> Unit raise DomainError { () }
    #|  callback()
    #|  handler()
    #|}
    #|
    #|trait AsyncTypedTrait { async fn run(Self) -> Unit raise DomainError }
    #|
    #|async fn no_raise() -> Unit noraise { () }
    #|async fn maybe() -> Unit raise? { () }
  let formatted = assert_modern_syntax_format_pipeline(source)
  for
    expected in [
      "callback : () -> Unit raise", "fn() -> Unit raise {", "fn run(Self) -> Unit raise",
      "callback : async () -> Unit raise DomainError", "async fn() -> Unit raise DomainError {",
      "async fn run(Self) -> Unit raise DomainError", "async fn no_raise() -> Unit noraise",
      "async fn maybe() -> Unit raise?",
    ] {
    if !formatted.contains(expected) {
      fail("non-default effect disappeared (`\{expected}`):\n\{formatted}")
    }
  }
}

///|
test "semantic projection includes omitted nocancel fields" {
  if semantically_equivalent("fn f() -> Unit {}", "fn f() -> Unit nocancel {}") {
    fail("the formatter semantic guard ignores FunDecl.nocancel_")
  }
  if semantically_equivalent(
      "impl Trait for Type with fn run(self : Type) -> Unit {}", "impl Trait for Type with fn run(self : Type) -> Unit nocancel {}",
    ) {
    fail("the formatter semantic guard ignores TopImpl.nocancel_")
  }
}

///|
test "trait and impl methods use current syntax" {
  let source =
    #|trait Mapper{
    #| #deprecated
    #| async fn[A] map(Self,A)->A
    #|}
    #|impl Mapper for Box with map(self:Box)->Unit{}
    #|impl Mapper for Box with fn[A] generic(self:Box,value:A)->A{value}
  let formatted = assert_modern_syntax_format_pipeline(source)
  for
    expected in [
      "#deprecated", "async fn[A] map(Self, A) -> A", "impl Mapper for Box with fn map(self : Box) -> Unit",
      "impl Mapper for Box with fn[A] generic(self : Box, value : A) -> A",
    ] {
    if !formatted.contains(expected) {
      fail(
        "current method syntax was not preserved (`\{expected}`):\n\{formatted}",
      )
    }
  }
}

///|
test "comments survive async effect normalization without fallback" {
  let source =
    #|// file header
    #|async fn  commented(
    #| callback:async ()->Unit raise, // arrow effect
    #|)->Unit raise + nocancel { // top-level effect
    #| let nested_callback=async fn()->Unit raise { // local effect
    #|  ()
    #| }
    #| nested_callback()
    #|}
    #|trait Commented{
    #| // before method
    #| async fn run(Self)->Unit raise // trait effect
    #|}
  let formatted = assert_modern_syntax_format_pipeline(source)
  for
    comment in [
      "// file header", "// arrow effect", "// top-level effect", "// local effect",
      "// before method", "// trait effect",
    ] {
    if !formatted.contains(comment) {
      fail("comment disappeared (`\{comment}`):\n\{formatted}")
    }
  }
  if !formatted.contains("-> Unit nocancel") {
    fail("async raise + nocancel did not normalize to nocancel:\n\{formatted}")
  }
}

///|
test "compound effect comments do not force formatter fallback" {
  let cases = [
    (
      #|fn commented_error_first(
      #|  callback : () -> Unit raise + // after plus
      #|    nocancel,
      #|) -> Unit nocancel { () }
    ),
    (
      #|fn commented_nocancel_first(
      #|  callback : () -> Unit nocancel +
      #|    // before noraise
      #|    noraise,
      #|) { callback() }
    ),
  ]
  for source in cases {
    let formatted = assert_modern_syntax_format_pipeline(source)
    if !formatted.contains("//") {
      fail("a compound-effect comment disappeared:\n\{formatted}")
    }
  }
}

///|
test "mutable let bindings keep mut and their type annotation" {
  let formatted = assert_modern_syntax_format_pipeline(
    (
      #|fn f() -> Unit {
      #|let mut x : Int = 1
      #|      x = 2
      #|}
    ),
  )
  assert_true(formatted.contains("  let mut x : Int = 1\n  x = 2\n"))
  let (impls, reports, _) = parse(
    (
      #|fn f() -> Unit {
      #|  let mut x : Int = 1
      #|  let mut y = 2
      #|  x = y
      #|}
    ),
  )
  assert_true(reports.is_empty())
  let printed = impls_to_string(impls)
  assert_true(printed.contains("let mut x : Int = 1"))
  assert_true(printed.contains("let mut y = 2"))
}
