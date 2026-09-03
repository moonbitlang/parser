///|
fn assert_modern_syntax_format_pipeline(source : String) -> String raise {
  guard strict_parse(source) is Some(original) else {
    let lex_errors = @lexer.tokens_from_string(comment=true, source).errors
      .map(lex_error => "\{lex_error.2}")
      .join(", ")
    let (_, reports, _) = parse(source)
    fail(
      "modern syntax regression source must parse:\n\{source}\nlex: \{lex_errors}\nparse: \{reports.to_json().stringify(indent=2)}",
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
