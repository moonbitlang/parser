///|
using @tokens {type Comment}

///|
using @list {type List}

///|
using @basic {type Position, type Report, type Location}

///|
fn shift_pos(pos : Position, offset : Int) -> Position {
  Position::{
    lnum: pos.lnum,
    cnum: pos.cnum + offset,
    bol: pos.bol,
    fname: pos.fname,
  }
}

///|
fn loc_contains(outer : Location, inner : Location) -> Bool {
  outer.start <= inner.start && inner.end <= outer.end
}

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
    comments : List[(@basic.Location, @tokens.Comment)],
  ) -> @syntax.DocString {
    let lines = comments.map(item => {
      let (loc, comment) = item
      let (prefix_len, text) = match comment.content {
        [.. "///|", .. remain] =>
          if remain.is_blank() {
            (4, "")
          } else {
            (4, remain.to_string())
          }
        [.. "///", .. remain] => (3, remain.to_string())
        _ => panic()
      }
      let text_start = shift_pos(loc.start, prefix_len)
      let text_loc = if text.is_empty() {
        Location::{ start: text_start, end: text_start }
      } else {
        Location::{ start: text_start, end: loc.end }
      }
      @syntax.DocStringLine::{
        text,
        text_loc,
        prefix_loc: Location::{ start: loc.start, end: text_start },
        loc,
      }
    })
    {
      content: lines.map(line => line.text),
      lines,
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
          Record(fields~, constr_decl~) => {
            fields.each(field => {
              let previous = skip_docstrings_before(field.loc.start)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              field.doc = previous
            })
            if constr_decl is Some(constr_decl) {
              let previous = skip_docstrings_before(constr_decl.loc.start)
                .map(make_doc)
                .unwrap_or(@syntax.DocString::empty())
              constr_decl.doc = previous
            }
          }
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
      TopUsing(..) as using_stmt => using_stmt.doc = doc
    }
    skip_docstrings_before(toplevel.loc().end) |> ignore
  }
}

///|
fn collect_top_level_trivia(
  tokens : Array[(@tokens.Token, Position, Position)],
  toplevels : @syntax.Impls,
) -> @list.List[@syntax.TopLevelTrivia] {
  let trivia = []
  for triple in tokens {
    let (token, start, end) = triple
    match token {
      COMMENT(comment) if !(comment.content is [_, _, '/', ..]) => {
        let loc = Location::{ start, end }
        if !toplevels.any(fn(toplevel) { loc_contains(toplevel.loc(), loc) }) {
          trivia.push(@syntax.TopLevelTrivia::{ comment, loc })
        }
      }
      _ => ()
    }
  }
  @list.from_array(trivia)
}

///|
pub fn parse_string(
  source : String,
  name? : String = "",
  parser? : Parser = Handrolled,
) -> (@syntax.ParsedImpls, Array[Report]) {
  let { tokens, docstrings, .. } = @lexer.tokens_from_string(
    source,
    comment=true,
    name~,
  )
  fn parse_by_moonyacc(tokens : @tokens.Triples) {
    let tokens = tokens.filter(fn(triple) {
      !(triple.0 is @tokens.Token::NEWLINE ||
      triple.0 is @tokens.Token::COMMENT(_))
    })
    (
      @syntax.ParsedImpls::{
        impls: @yacc_parser.structure(tokens),
        trivia: @list.empty(),
      },
      [],
    ) catch {
      UnexpectedEndOfInput(pos, _) =>
        (
          @syntax.ParsedImpls::{ impls: @list.empty(), trivia: @list.empty() },
          [
            Report::{
              loc: { start: pos, end: pos },
              msg: "Unexpected end of file.",
            },
          ],
        )
      UnexpectedToken(token, (start, end), _) =>
        (
          @syntax.ParsedImpls::{ impls: @list.empty(), trivia: @list.empty() },
          [Report::{ loc: { start, end }, msg: "Unexpected `\{token}`." }],
        )
    }
  }

  let (parsed, diagnostics) = match parser {
    MoonYacc => parse_by_moonyacc(tokens)
    Handrolled => @handrolled_parser.parse(tokens)
  }
  attach_docstrings(docstrings, parsed.impls)
  let parsed = match parser {
    MoonYacc =>
      { ..parsed, trivia: collect_top_level_trivia(tokens, parsed.impls) }
    Handrolled => parsed
  }
  (parsed, diagnostics)
}

///|
pub fn parse_file(
  path : String,
  parser? : Parser = Handrolled,
) -> (@syntax.ParsedImpls, Array[Report]) raise @fs.IOError {
  let source = @fs.read_file_to_string(path)
  parse_string(source, name=path, parser~)
}
