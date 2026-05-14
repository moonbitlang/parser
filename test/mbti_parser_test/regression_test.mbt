///|
using @mbti_ast {type Mbti, type Sig}

///|
let regression_sample_header =
  #|package "sample";
  #|

///|
fn parse_ok(name : String, content : String) -> Mbti raise {
  let (mbti, reports) = @mbti_parser.parse_string(content, name~)
  if !reports.is_empty() {
    fail("parsing \{name} failed with \{reports.length()} diagnostics")
  }
  mbti
}

///|
fn first_sig(name : String, body : String) -> Sig raise {
  let mbti = parse_ok(name, regression_sample_header + body)
  mbti.sigs.head().unwrap().0
}

///|
test "mbti float constants strip F suffix" {
  let float_const =
    #|pub const X : Float = 1.5F
    #|
  guard first_sig("float_suffix.mbti", float_const)
    is Const({ value: Float(value), .. }) else {
    fail("expected float const")
  }
  inspect(value, content="1.5")
  let negative_float_const =
    #|pub const X : Float = -2.5F
    #|
  guard first_sig("negative_float_suffix.mbti", negative_float_const)
    is Const({ value: Float(value), .. }) else {
    fail("expected negative float const")
  }
  inspect(value, content="-2.5")
}

///|
test "mbti method self type location starts at type name" {
  let body =
    #|pub fn &Worker::run() -> Unit
    #|
  guard first_sig("method_self_loc.mbti", body)
    is Func({ type_name: Some({ loc, .. }), .. }) else {
    fail("expected method function signature")
  }
  inspect(loc.to_string(), content="method_self_loc.mbti-2:9-2:15")
}

///|
test "mbti function post label location includes tilde" {
  let body =
    #|pub fn f(label~ : Int) -> Unit
    #|
  guard first_sig("post_label_loc.mbti", body) is Func({ params, .. }) else {
    fail("expected function signature")
  }
  guard params.head().unwrap() is Labelled(binder~, ..) else {
    fail("expected labelled parameter")
  }
  inspect(binder.loc.to_string(), content="post_label_loc.mbti-2:10-2:16")
}

///|
test "mbti parse string records base position" {
  let content =
    #|package "sample";
    #|
  let mbti = parse_ok("base_pos.mbti", content)
  inspect(mbti.base_pos.base_fname, content="base_pos.mbti")
  inspect(mbti.base_pos.base_lnum, content="0")
}

///|
test "mbti package name rejects empty path component" {
  let content =
    #|package "sample";
    #|import { "foo/bar" as @foo//bar };
    #|
  let (_, reports) = @mbti_parser.parse_string(
    content,
    name="bad_package_alias.mbti",
  )
  if reports.is_empty() {
    fail("expected invalid package alias to produce diagnostics")
  }
}
