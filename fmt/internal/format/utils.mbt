///|
/// series of xs with separator and trailing separator
/// 
/// ```skip
/// a, b, c
/// 
/// a,
/// b,
/// c,
/// ```
fn[T] series(
  xs : List[T],
  sep : Doc,
  f : (T) -> Doc,
  trailing? : TrailingSeparatorMode = PresentInNormalStyle,
) -> Doc {
  let trailing = match xs {
    Empty => empty
    More(_, ..) =>
      match trailing {
        AlwaysHidden => empty
        AlwaysPresent => sep
        PresentInNormalStyle => switch(empty, sep)
      }
  }
  separate_map(xs, sep + line, f) + trailing
}

///|
fn series1(
  xs : List[Doc],
  sep : Doc,
  trailing? : TrailingSeparatorMode = PresentInNormalStyle,
) -> Doc {
  series(xs, sep, x => x, trailing~)
}

///|
priv enum TrailingSeparatorMode {
  PresentInNormalStyle
  AlwaysHidden
  AlwaysPresent
}

///|
/// ```skip
/// (x)
/// (
///   x
/// )
/// ```
fn parens(x : Doc, force_newline? : Bool = false) -> Doc {
  let l = if force_newline { hardline } else { softline }
  char('(') + nest(l + x) + l + char(')')
}

///|
/// ```skip
/// { x }
/// {
///   x
/// }
/// ```
fn braces(x : Doc, force_newline? : Bool = false) -> Doc {
  let l = if force_newline { hardline } else { line }
  char('{') + nest(l + x) + l + char('}')
}

///|
fn braces_if_not_flatten(x : Doc) -> Doc {
  let lb = switch(empty, char('{'))
  let rb = switch(empty, char('}'))
  lb + nest(softline + x) + softline + rb
}

///|
/// ```skip
/// [x]
/// [
///   x
/// ]
/// ```
fn brackets(x : Doc, force_newline? : Bool = false) -> Doc {
  let l = if force_newline { hardline } else { softline }
  char('[') + nest(l + x) + l + char(']')
}

///|
fn[T] optional(x : T?, default : Doc, f : (T) -> Doc) -> Doc {
  x.map(f).unwrap_or(default)
}

///|
let empty : Doc = @pp.empty

///|
let hardline : Doc = @pp.hardline

///|
let line : Doc = @pp.line

///|
let comma : Doc = char(',')

///|
let colon : Doc = char(':')

///|
let equal : Doc = char('=')

///|
let dot : Doc = char('.')

///|
let tilde : Doc = char('~')

///|
let semi : Doc = char(';')

///|
fn double_quoted(x : Doc) -> Doc {
  char('"') + x + char('"')
}

///|
fn single_quoted(x : Doc) -> Doc {
  char('\'') + x + char('\'')
}

///|
fn[T] concat_map(xs : List[T], f : (T) -> Doc) -> Doc {
  separate_map(xs, empty, f)
}

///|
fn[T] separate_map(xs : List[T], sep : Doc, f : (T) -> Doc) -> Doc {
  @pp.separate_map(sep, xs.to_array(), f)
}

///|
fn separate(xs : List[Doc], sep : Doc) -> Doc {
  @pp.separate(sep, xs.to_array())
}

///|
fn current_column_without_indent(f : (Int) -> Doc) -> Doc {
  dynamic(fn(ctx) { f(ctx.indent - ctx.next_indent) })
}
