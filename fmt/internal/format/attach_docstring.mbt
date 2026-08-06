///|
fn attach_docstrings(
  docstrings : Array[List[(Location, @tokens.Comment)]],
  toplevels : @syntax.Impls,
  tokens : @tokens.Triples,
) -> Unit {
  fn mark_consumed(loc : Location, comment : @tokens.Comment) -> Unit {
    comment.consumed_by_docstring = true
    for triple in tokens {
      match triple {
        (COMMENT(token_comment), start, end) if start == loc.start &&
          end == loc.end => token_comment.consumed_by_docstring = true
        _ => ()
      }
    }
  }

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
      content: comments.filter_map(p => {
        mark_consumed(p.0, p.1)
        let content = p.1.content
        if content.trim_end() == "///|" {
          // A bare `///|` is a formatting delimiter, not part of the
          // documentation payload. Keeping it out of the AST makes adding the
          // canonical block marker semantics-preserving.
          None
        } else {
          match content {
            [.. "///|", .. remain] | [.. "///", .. remain] =>
              Some(remain.to_owned())
            // The lexer normally routes only doc comments here. Preserve an
            // unexpected payload instead of crashing the formatter.
            _ => Some(content)
          }
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
      TopExtend(..) as decl => decl.doc = doc
      TopImport(..) as decl => decl.doc = doc
      TopView(..) as view => view.doc = doc
      TopImpl(..) as imp => imp.doc = doc
    }
    skip_docstrings_before(toplevel.loc().end) |> ignore
  }
}
