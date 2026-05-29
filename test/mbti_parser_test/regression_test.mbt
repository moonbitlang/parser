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
  guard params.head().unwrap()
    is Labelled(post_label_loc=Some(post_label_loc), ..) else {
    fail("expected labelled parameter")
  }
  inspect(post_label_loc.to_string(), content="post_label_loc.mbti-2:10-2:16")
}

///|
test "mbti shared type token locations" {
  let body =
    #|pub fn f(@pkg.Type[Int, String], &Object, (Int, String) -> Unit raise Error) -> (Int, String) raise ?
    #|
  guard first_sig("type_locs.mbti", body) is Func({ params, return_, .. }) else {
    fail("expected function signature")
  }
  guard params
    is More(
      DiscardPositional(
        ty=Some(
          Name(
            constr_id={ id: Dot(pkg_loc~, dot_id_loc~, ..), .. },
            type_args_lbracket_loc=Some(type_args_lbracket_loc),
            type_args_rbracket_loc=Some(type_args_rbracket_loc),
            type_arg_comma_locs~,
            ..
          )
        ),
        ..
      ),
      tail=More(
        DiscardPositional(ty=Some(Object(amp_loc=object_amp_loc, ..)), ..),
        tail=More(
          DiscardPositional(
            ty=Some(
              Arrow(
                lparen_loc=arrow_lparen_loc,
                rparen_loc=arrow_rparen_loc,
                arrow_loc~,
                comma_locs=arrow_comma_locs,
                err=ErrorType(raise_loc=arrow_raise_loc, ..),
                ..
              )
            ),
            ..
          ),
          tail=Empty
        )
      )
    ) else {
    fail("unexpected parameter type shape")
  }
  inspect(pkg_loc.to_string(), content="type_locs.mbti-2:10-2:14")
  inspect(dot_id_loc.to_string(), content="type_locs.mbti-2:15-2:19")
  inspect(
    type_args_lbracket_loc.to_string(),
    content="type_locs.mbti-2:19-2:20",
  )
  inspect(
    type_arg_comma_locs.head().unwrap().to_string(),
    content="type_locs.mbti-2:23-2:24",
  )
  inspect(
    type_args_rbracket_loc.to_string(),
    content="type_locs.mbti-2:31-2:32",
  )
  inspect(object_amp_loc.to_string(), content="type_locs.mbti-2:34-2:35")
  inspect(arrow_lparen_loc.to_string(), content="type_locs.mbti-2:43-2:44")
  inspect(
    arrow_comma_locs.head().unwrap().to_string(),
    content="type_locs.mbti-2:47-2:48",
  )
  inspect(arrow_rparen_loc.to_string(), content="type_locs.mbti-2:55-2:56")
  inspect(arrow_loc.to_string(), content="type_locs.mbti-2:57-2:59")
  inspect(arrow_raise_loc.to_string(), content="type_locs.mbti-2:65-2:70")
  guard return_
    is (
      Tuple(
        lparen_loc=tuple_lparen_loc,
        rparen_loc=tuple_rparen_loc,
        comma_locs=tuple_comma_locs,
        ..
      ),
      MaybeError(raise_loc~, question_loc~, loc~, ..),
    ) else {
    fail("unexpected return type shape")
  }
  inspect(tuple_lparen_loc.to_string(), content="type_locs.mbti-2:81-2:82")
  inspect(
    tuple_comma_locs.head().unwrap().to_string(),
    content="type_locs.mbti-2:85-2:86",
  )
  inspect(tuple_rparen_loc.to_string(), content="type_locs.mbti-2:93-2:94")
  inspect(raise_loc.to_string(), content="type_locs.mbti-2:95-2:100")
  inspect(question_loc.to_string(), content="type_locs.mbti-2:101-2:102")
  inspect(loc.to_string(), content="type_locs.mbti-2:95-2:102")
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
