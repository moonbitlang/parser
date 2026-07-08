///|
fn attach_docstrings_to_impls(
  docstrings_view : ArrayView[List[(Location, Comment)]],
  toplevels : @syntax.Impls,
) -> Unit {
  let docstrings : Array[List[(Location, Comment)]] = []
  for comments in docstrings_view {
    docstrings.push(comments)
  }

  fn search_docstrings(
    last_pos : Position,
    loc : Location,
  ) -> List[(Location, Comment)]? {
    let mut result = None
    while docstrings.last() is Some(comments) &&
          comments.head().unwrap().0.start.lnum <= loc.start.lnum {
      let comments = docstrings.pop().unwrap()
      let line = comments.head().unwrap().0.start.lnum
      if last_pos.lnum < line && line <= loc.start.lnum {
        result = Some(comments)
      }
    }
    result
  }

  fn make_doc(comments : List[(Location, Comment)]) -> @syntax.DocString {
    {
      content: comments.map(p => {
        match p.1.content {
          [.. "///|", .. remain] =>
            if remain.is_blank() {
              ""
            } else {
              remain.to_owned()
            }
          [.. "///", .. remain] => remain.to_owned()
          _ => panic()
        }
      }),
      loc: {
        start: comments.head().unwrap().0.start,
        end: comments.last().unwrap().0.end,
      },
    }
  }

  let mut last_pos = Position::{ fname: "", lnum: -1, bol: 0, cnum: 0 }
  for toplevel in toplevels {
    let loc = toplevel.loc()
    let doc = search_docstrings(last_pos, loc)
      .map(make_doc)
      .unwrap_or(@syntax.DocString::empty())
    match toplevel {
      @syntax.TopTypeDef(td) => {
        td.doc = doc
        last_pos = loc.start
        match td.components {
          @syntax.Abstract
          | @syntax.Alias(_)
          | @syntax.Error(@syntax.NoPayload)
          | @syntax.TupleStruct(_) => ()
          @syntax.Error(@syntax.EnumPayload(constrs))
          | @syntax.Variant(constrs)
          | @syntax.ExtensibleEnum(constrs) =>
            constrs.each(constr => {
              let previous = search_docstrings(last_pos, constr.loc)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              constr.doc = previous
              last_pos = constr.loc.end
            })
          @syntax.ExtendEnum(constructors=constrs, ..) =>
            constrs.each(constr => {
              let previous = search_docstrings(last_pos, constr.loc)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              constr.doc = previous
              last_pos = constr.loc.end
            })
          @syntax.Record(fields~) =>
            fields.each(field => {
              let previous = search_docstrings(last_pos, field.loc)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              field.doc = previous
              last_pos = field.loc.end
            })
        }
      }
      @syntax.TopFuncDef(fun_decl~, ..) => fun_decl.doc = doc
      @syntax.TopLetDef(..) as ld => ld.doc = doc
      @syntax.TopExpr(..) => ()
      @syntax.TopImplRelation(..) as imp => imp.doc = doc
      @syntax.TopTest(..) as test_ => test_.doc = doc
      @syntax.TopTrait(decl) => decl.doc = doc
      @syntax.TopView(..) as view => view.doc = doc
      @syntax.TopImpl(..) as imp => imp.doc = doc
      @syntax.TopUsing(..) as using_stmt => using_stmt.doc = doc
    }
    last_pos = loc.end
  }
}
