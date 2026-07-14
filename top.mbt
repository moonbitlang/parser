///|
using @tokens {type Comment}

///|
using @list {type List}

///|
using @basic {type Position, type Report, type Location}

///|
pub(all) enum Parser {
  MoonYacc
  Handrolled
  // Mixed
}

///|
fn attach_docstrings(
  docstrings : Array[List[(Location, Comment)]],
  toplevels : @syntax.Impls,
) -> Unit {
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

  fn make_doc(
    comments : List[(@basic.Location, @tokens.Comment)],
  ) -> @syntax.DocString {
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
      TopTypeDef(td) => {
        td.doc = doc
        last_pos = loc.start
        match td.components {
          // there is no docstring inside the types
          Abstract | Alias(_) | Error(NoPayload) | TupleStruct(_) => ()
          // handle docstring before the enum/suberror constructor and struct fields 
          Error(EnumPayload(constrs))
          | Variant(constrs)
          | ExtensibleEnum(constrs) =>
            constrs.each(constr => {
              let previous = search_docstrings(last_pos, constr.loc)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              constr.doc = previous
              last_pos = constr.loc.end
            })
          ExtendEnum(constructors=constrs, ..) =>
            constrs.each(constr => {
              let previous = search_docstrings(last_pos, constr.loc)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              constr.doc = previous
              last_pos = constr.loc.end
            })
          Record(fields~) =>
            fields.each(field => {
              let previous = search_docstrings(last_pos, field.loc)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              field.doc = previous
              last_pos = field.loc.end
            })
        }
      }
      TopFuncDef(fun_decl~, ..) => fun_decl.doc = doc
      TopLetDef(..) as ld => ld.doc = doc
      TopExpr(..) => ()
      TopImplRelation(..) as imp => imp.doc = doc
      TopTest(..) as test_ => test_.doc = doc
      TopTrait(decl) => decl.doc = doc
      TopView(..) as view => view.doc = doc
      TopImpl(..) as imp => imp.doc = doc
      TopUsing(..) as using_stmt => using_stmt.doc = doc
    }
    last_pos = loc.end
  }
}

///|
pub fn parse_string(
  source : String,
  name? : String = "",
  parser? : Parser = Handrolled,
) -> (@syntax.Impls, Array[Report]) {
  let { tokens, docstrings, .. } = @lexer.tokens_from_string(
    source,
    comment=true,
    name~,
  )
  fn parse_by_moonyacc(tokens : @tokens.Triples) {
    let tokens = tokens.filter(fn(triple) {
      !(triple.0 is NEWLINE || triple.0 is COMMENT(_))
    })
    (@yacc_parser.structure(tokens), []) catch {
      UnexpectedEndOfInput(pos, _) =>
        (
          @list.empty(),
          [
            Report::{
              loc: { start: pos, end: pos },
              msg: "Unexpected end of file.",
            },
          ],
        )
      UnexpectedToken(token, (start, end), _) =>
        (
          @list.empty(),
          [
            {
              loc: { start, end },
              msg: "Unexpected `\{@debug.render(token.to_repr())}`.",
            },
          ],
        )
    }
  }

  let (impls, diagnostics) = match parser {
    MoonYacc => parse_by_moonyacc(tokens)
    Handrolled => @handrolled_parser.parse(tokens)
  }
  attach_docstrings(docstrings, impls)
  (impls, diagnostics)
}

///|
pub fn parse_file(
  path : String,
  parser? : Parser = Handrolled,
) -> (@syntax.Impls, Array[Report]) raise @fs.IOError {
  let source = @fs.read_file_to_string(path)
  parse_string(source, name=path, parser~)
}
