///|
fn attach_docstrings(
  docstrings : Array[List[(Location, @tokens.Comment)]],
  toplevels : @syntax.Impls,
) -> Unit {

  // skip the docstring before the posisiton, return the last skipped docstring.
  fn skip_docstrings_before(
    pos : @basic.Position,
  ) -> List[(Location, @tokens.Comment)]? {
    let mut previous = None
    while docstrings.last() is Some(comments) &&
          comments.last().unwrap().0.end <= pos {
      previous = docstrings.pop()
    }
    previous
  }

  fn make_doc(
    comments : List[(@basic.Location, @tokens.Comment)],
  ) -> @syntax.DocString {
    {
      content: comments.map(p => {
        match p.1.content {
          [.. "///|", .. remain] | [.. "///", .. remain] => remain.to_owned()
          _ => panic()
        }
      }),
      loc: {
        start: comments.head().unwrap().0.start,
        end: comments.last().unwrap().0.end,
      },
    }
  }

  for toplevel in toplevels {
    let previous = skip_docstrings_before(toplevel.loc().start)
    let doc = previous.map(make_doc).unwrap_or(@syntax.DocString::empty())
    match toplevel {
      TopTypeDef(td) => {
        td.doc = doc
        match td.components {
          // there is no docstring inside the types
          Abstract | Alias(_) | Error(NoPayload) | TupleStruct(_) => ()
          // handle docstring before the enum/suberror constructor and struct fields 
          Error(EnumPayload(constrs))
          | Variant(constrs)
          | ExtensibleEnum(constrs)
          | ExtendEnum(constructors=constrs, ..) =>
            constrs.each(constr => {
              let previous = skip_docstrings_before(constr.loc.start)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              constr.doc = previous
            })
          Record(fields~) =>
            fields.each(field => {
              let previous = skip_docstrings_before(field.loc.start)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              field.doc = previous
            })
        }
      }
      TopFuncDef(fun_decl~, ..) => fun_decl.doc = doc
      TopLetDef(..) as ld => ld.doc = doc
      TopExpr(..) => ()
      TopImplRelation(..) as imp => imp.doc = doc
      TopTest(..) as test_ => test_.doc = doc
      TopTrait(decl) => decl.doc = doc
      TopUsing(..) as decl => decl.doc = doc
      TopView(..) as view => view.doc = doc
      TopImpl(..) as imp => imp.doc = doc
    }
    skip_docstrings_before(toplevel.loc().end) |> ignore
  }
}
