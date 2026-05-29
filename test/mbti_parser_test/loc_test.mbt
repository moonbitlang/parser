///|
let loc_sample_header =
  #|package "sample";
  #|

///|
fn parse_loc_ok(name : String, content : String) -> Mbti raise {
  let (mbti, reports) = @mbti_parser.parse_string(content, name~)
  if !reports.is_empty() {
    fail("parsing \{name} failed: \{to_repr(reports)}")
  }
  mbti
}

///|
fn loc_sig_at(mbti : Mbti, index : Int) -> Sig {
  mbti.sigs.nth(index).unwrap().0
}

///|
test "mbti shared declaration component token locations" {
  let body =
    #|pub suberror SharedError {
    #|  Failure(mut reason~ : String, Int) = 7;
    #|  Empty;
    #|}
    #|pub struct SharedRecord {
    #|  mut first : Int;
    #|  second : String
    #|}
    #|
  let mbti = parse_loc_ok("shared_decl_locs.mbti", loc_sample_header + body)
  guard loc_sig_at(mbti, 0)
    is Type(
      {
        components: Error(
          EnumPayload(constrs~, lbrace_loc~, rbrace_loc~, semi_locs~)
        ),
        ..,
      }
    ) else {
    fail("expected payload suberror")
  }
  inspect(lbrace_loc.to_string(), content="shared_decl_locs.mbti-2:26-2:27")
  inspect(rbrace_loc.to_string(), content="shared_decl_locs.mbti-5:1-5:2")
  inspect(semi_locs.length(), content="2")
  inspect(
    semi_locs.head().unwrap().to_string(),
    content="shared_decl_locs.mbti-3:41-3:42",
  )
  inspect(
    semi_locs.last().unwrap().to_string(),
    content="shared_decl_locs.mbti-4:8-4:9",
  )
  guard constrs is More(first_constr, tail=More(_, tail=Empty)) else {
    fail("expected two constructors")
  }
  match first_constr {
    {
      name: { loc: failure_name_loc, .. },
      args: Some(
        More(
          {
            mut_loc: Some(param_mut_loc),
            post_label_loc: Some(param_post_label_loc),
            colon_loc: Some(param_colon_loc),
            label: Some(param_label),
            ..,
          },
          tail=More(
            {
              mut_loc: None,
              post_label_loc: None,
              colon_loc: None,
              label: None,
              ..,
            },
            tail=Empty
          )
        )
      ),
      tag: Some((tag, tag_loc)),
      args_lparen_loc: Some(args_lparen_loc),
      args_rparen_loc: Some(args_rparen_loc),
      arg_comma_locs,
      tag_equal_loc: Some(tag_equal_loc),
      ..,
    } => {
      inspect(
        failure_name_loc.to_string(),
        content="shared_decl_locs.mbti-3:3-3:10",
      )
      inspect(
        args_lparen_loc.to_string(),
        content="shared_decl_locs.mbti-3:10-3:11",
      )
      inspect(
        param_mut_loc.to_string(),
        content="shared_decl_locs.mbti-3:11-3:14",
      )
      inspect(
        param_label.loc.to_string(),
        content="shared_decl_locs.mbti-3:15-3:21",
      )
      inspect(
        param_post_label_loc.to_string(),
        content="shared_decl_locs.mbti-3:15-3:22",
      )
      inspect(
        param_colon_loc.to_string(),
        content="shared_decl_locs.mbti-3:23-3:24",
      )
      inspect(arg_comma_locs.length(), content="1")
      inspect(
        arg_comma_locs.head().unwrap().to_string(),
        content="shared_decl_locs.mbti-3:31-3:32",
      )
      inspect(
        args_rparen_loc.to_string(),
        content="shared_decl_locs.mbti-3:36-3:37",
      )
      inspect(
        tag_equal_loc.to_string(),
        content="shared_decl_locs.mbti-3:38-3:39",
      )
      inspect(tag, content="7")
      inspect(tag_loc.to_string(), content="shared_decl_locs.mbti-3:40-3:41")
    }
    _ => fail("unexpected constructor shape")
  }
  guard loc_sig_at(mbti, 1) is Type({ components: Record(fields~, ..), .. }) else {
    fail("expected record")
  }
  guard fields is More(first_field, tail=More(second_field, tail=Empty)) else {
    fail("expected two fields")
  }
  match first_field {
    {
      mut_loc: Some(field_mut_loc),
      colon_loc: first_colon_loc,
      name: { loc: first_name_loc, .. },
      ..,
    } => {
      inspect(
        field_mut_loc.to_string(),
        content="shared_decl_locs.mbti-7:3-7:6",
      )
      inspect(
        first_name_loc.to_string(),
        content="shared_decl_locs.mbti-7:7-7:12",
      )
      inspect(
        first_colon_loc.to_string(),
        content="shared_decl_locs.mbti-7:13-7:14",
      )
    }
    _ => fail("unexpected first field shape")
  }
  match second_field {
    {
      mut_loc: None,
      colon_loc: second_colon_loc,
      name: { loc: second_name_loc, .. },
      ..,
    } => {
      inspect(
        second_name_loc.to_string(),
        content="shared_decl_locs.mbti-8:3-8:9",
      )
      inspect(
        second_colon_loc.to_string(),
        content="shared_decl_locs.mbti-8:10-8:11",
      )
    }
    _ => fail("unexpected second field shape")
  }
}
