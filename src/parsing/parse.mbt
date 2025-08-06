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
  for toplevel in toplevels {
    let mut previous = None
    while docstrings.last() is Some(comments) &&
          comments.last().unwrap().0.end <= toplevel.loc().start {
      previous = docstrings.pop()
    }
    let doc = match previous {
      None => @syntax.DocString::empty()
      Some(comments) =>
        {
          content: comments.map(p => {
            let content = p.1.content
            if content.has_prefix("///|") {
              content.substring(start=4)
            } else {
              content.substring(start=3)
            }
          }),
          loc: {
            start: comments.head().unwrap().0.start,
            end: comments.last().unwrap().0.end,
          },
        }
    }
    match toplevel {
      TopTypeDef(td) => td.doc = doc
      TopFuncDef(fun_decl~) => fun_decl.doc = doc
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
    while docstrings.last() is Some(comments) &&
          comments.last().unwrap().0.end <= toplevel.loc().end {
      docstrings.pop() |> ignore
    }
  }
}

///|
pub fn parse_string(
  source : Bytes,
  name~ : String = "",
  config~ : Parser,
) -> @syntax.Impls {
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
  parse_string(source, name=path, config~)
}
