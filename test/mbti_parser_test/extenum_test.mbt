///|
let extenum_sample_header =
  #|package "sample";
  #|

///|
fn parse_mbti_source(name : String, content : String) -> Unit raise {
  let (mbti, reports) = @mbti_parser.parse_string(content, name~)
  if !reports.is_empty() {
    fail("parsing \{name} failed with \{reports.length()} diagnostics")
  }
  ignore(mbti)
}

///|
fn parse_mbti_source_expect_error(name : String, content : String) -> Unit {
  try {
    parse_mbti_source(name, content)
    fail("parsing \{name} unexpectedly succeeded")
  } catch {
    _ => ()
  }
}

///|
fn parse_mbti_body(name : String, body : String) -> Unit raise {
  parse_mbti_source(name, extenum_sample_header + body)
}

///|
test "parse mbti extenum" {
  let content =
    #|package "sample";
    #|pub(open) extenum Expr { A; B(Int) };
    #|pub(readonly) extenum @foreign.Expr += { C; D(String) }
    #|
  parse_mbti_source("extenum.mbti", content)
}

///|
test "parse mbti ocaml import alias" {
  let body =
    #|import { "foo/bar" as @foo, };
    #|
  parse_mbti_body("import_alias.mbti", body)
}

///|
test "parse mbti ocaml derive" {
  let body =
    #|pub type Box[T] derive(Show, @pkg.Debug,);
    #|
  parse_mbti_body("derive.mbti", body)
}

///|
test "parse mbti ocaml empty trait" {
  let body =
    #|pub trait Empty {};
    #|
  parse_mbti_body("empty_trait.mbti", body)
}

///|
test "parse mbti ocaml async trait method noraise" {
  let body =
    #|pub trait Worker { async run(Self) -> Unit noraise }
    #|
  parse_mbti_body("async_trait_method.mbti", body)
}

///|
test "parse mbti ocaml uppercase function name" {
  let body =
    #|pub fn New() -> Unit
    #|
  parse_mbti_body("uppercase_function.mbti", body)
}

///|
test "parse mbti ocaml visible fnalias" {
  let body =
    #|pub(open) fnalias Box::make
    #|
  parse_mbti_body("visible_fnalias.mbti", body)
}

///|
test "parse mbti ocaml negative constants" {
  let body =
    #|pub const N : Int = -42;
    #|pub const D : Double = -1.5
    #|
  parse_mbti_body("negative_constants.mbti", body)
}

///|
test "reject mbti record constructor" {
  let content =
    #|package "sample";
    #|pub struct T { fn make() -> T }
    #|
  parse_mbti_source_expect_error("record_constructor.mbti", content)
}
