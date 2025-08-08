///|
typealias @syntax_types.(Location, Comment)

///|
typealias @list.T as List

///|
typealias @syntax.Position

///|
pub(all) enum Parser {
  // MoonYacc
  Handrolled
  // Mixed
}

///|
pub fn attach_docstrings(
  docstrings : Array[List[(Location, Comment)]],
  toplevels : @syntax.Impls,
) -> Unit {

  // skip the docstring before the posisiton, return the last skipped docstring.
  fn skip_docstrings_before(pos : Position) -> List[(Location, Comment)]? {
    let mut previous = None
    while docstrings.last() is Some(comments) &&
          comments.last().unwrap().0.end <= pos {
      previous = docstrings.pop()
    }
    previous
  }

  fn make_doc(
    comments : List[(@syntax_types.Location, @syntax_types.Comment)],
  ) -> @syntax.DocString {
    {
      content: comments.map(p => {
        let content = p.1.content
        content.substring(start=if content.has_prefix("///|") { 4 } else { 3 })
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
          Abstract
          | Extern
          | Newtype(_)
          | Alias(_)
          | Error(NoPayload | SinglePayload(_))
          | TupleStruct(_) => ()
          // handle docstring before the enum/suberror constructor and struct fields 
          Error(EnumPayload(constrs)) | Variant(constrs) =>
            constrs.each(constr => {
              let previous = skip_docstrings_before(constr.loc.start)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              constr.doc = previous
            })
          Record(fields) =>
            fields.each(field => {
              let previous = skip_docstrings_before(field.loc.start)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              field.doc = previous
            })
        }
      }
      TopFuncDef(fun_decl~, ..) => fun_decl.doc = doc
      TopFuncAlias(..) as fa => fa.doc = doc
      TopLetDef(..) as ld => ld.doc = doc
      TopExpr(..) => ()
      TopImplRelation(..) as imp => imp.doc = doc
      TopTest(..) as test_ => test_.doc = doc
      TopTrait(decl) => decl.doc = doc
      TopBatchTypeAlias(..) as decl => decl.doc = doc
      TopBatchTraitAlias(..) as decl => decl.doc = doc
      TopView(..) as view => view.doc = doc
      TopImpl(..) as imp => imp.doc = doc
    }
    skip_docstrings_before(toplevel.loc().end) |> ignore
  }
}

///|
pub fn parse_bytes(
  source : Bytes,
  name~ : String = "",
  config~ : Parser,
) -> @syntax.Impls {
  ignore(config)
  let { tokens, docstrings, .. } = @lexer.tokens_from_bytes(
    source,
    comment=true,
    name~,
  )
  let impls = @hand_parser.parse(tokens).0
  attach_docstrings(docstrings, impls)
  impls
}

///|
pub fn parse_file(
  path : String,
  config~ : Parser,
) -> @syntax.Impls raise @fs.IOError {
  let source = @fs.read_file_to_bytes(path)
  parse_bytes(source, name=path, config~)
}
