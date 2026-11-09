///|
using @basic {type Location, type Position}

///|
using @tokens {type Comment}

///|
using @list {type List}

///|
fn make_doc(comments : List[(Location, Comment)]) -> @syntax.DocString {
  let first_loc = comments.head().unwrap().0
  {
    content: comments.map(p => {
      let mut text = p.1.content[:]
      while text is ['/', .. rest] {
        text = rest
      }
      if p.0 == first_loc && text is ['|', .. rest] {
        if rest.is_blank() ||
          (rest.trim().has_prefix("UUID(") && rest.trim().has_suffix(")")) {
          ""
        } else {
          rest.to_owned()
        }
      } else {
        text.to_owned()
      }
    }),
    loc: { start: first_loc.start, end: comments.last().unwrap().0.end, },
  }
}

///|
pub fn attach_docstrings_to_impls(
  docstrings_view : ArrayView[List[(Location, Comment)]],
  toplevels : @syntax.Impls,
) -> Unit {
  let docstrings : Array[List[(Location, Comment)]] = []
  for comments in docstrings_view {
    let blocks : Array[List[(Location, Comment)]] = []
    let mut block : List[(Location, Comment)] = List([])
    for (loc, comment) in comments {
      // Only a column-zero, exact three-slash marker starts a new block.
      // The lexer and its original comment tokens remain untouched.
      if loc.start.cnum == loc.start.bol &&
        comment.content.has_prefix("///|") &&
        !block.is_empty() {
        blocks.push(block.rev())
        block = List([])
      }
      block = block.prepend((loc, comment))
    }
    if !block.is_empty() {
      blocks.push(block.rev())
    }
    blocks.rev_in_place()
    for block in blocks {
      docstrings.push(block)
    }
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

  let mut last_pos = Position::{ fname: "", lnum: -1, bol: 0, cnum: 0, }
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
          Abstract | Alias(_) | Error(NoPayload) | TupleStruct(_) => ()
          Error(EnumPayload(constrs)) | Variant(constrs) =>
            constrs.each(constr => {
              let previous = search_docstrings(last_pos, constr.loc)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              constr.doc = previous
              last_pos = constr.loc.end
            })
          ExtensibleEnum(constrs) =>
            if constrs is Some(items) {
              items.each(constr => {
                let previous = search_docstrings(last_pos, constr.loc)
                  .map(make_doc)
                  .unwrap_or(@syntax.DocString::empty())
                constr.doc = previous
                last_pos = constr.loc.end
              })
            }
          ExtendEnum(constructors=constrs, ..) =>
            constrs.each(constr => {
              let previous = search_docstrings(last_pos, constr.loc)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              constr.doc = previous
              last_pos = constr.loc.end
            })
          Record(fields~, constr_decl~) => {
            fields.each(field => {
              let previous = search_docstrings(last_pos, field.loc)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              field.doc = previous
              last_pos = field.loc.end
            })
            if constr_decl is Some(decl) {
              let previous = search_docstrings(last_pos, decl.loc)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              decl.doc = previous
              last_pos = decl.loc.end
            }
          }
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
      TopExtend(..) as extend_stmt => extend_stmt.doc = doc
      TopImport(..) as import_stmt => import_stmt.doc = doc
    }
    last_pos = loc.end
  }
}
