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
  separator? : () -> Doc = fn() { empty },
) -> Doc {
  let docs : Array[Doc] = []
  let length = xs.length()
  for index, item in xs {
    docs.push(f(item))
    if index + 1 < length {
      docs.push(sep + separator() + line)
    }
  }
  // Build the trailing separator only after all elements. Comment
  // placeholders are stateful during the attach pass, so construction order
  // must match source order as well as final document order.
  let trailing = match xs {
    Empty => empty
    More(_, ..) =>
      match trailing {
        AlwaysHidden => empty
        AlwaysPresent => sep + separator()
        PresentInNormalStyle => switch(empty, sep) + separator()
      }
  }
  docs.fold(init=empty, Doc::add) + trailing
}

///|
fn series1(
  xs : List[Doc],
  sep : Doc,
  trailing? : TrailingSeparatorMode = PresentInNormalStyle,
  separator? : () -> Doc = fn() { empty },
) -> Doc {
  series(xs, sep, x => x, trailing~, separator~)
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
fn break_(n : Int) -> Doc {
  switch(text(String::make(n, ' ')), hardline)
}

///|
fn catifflat(left~ : Doc, right~ : Doc, flat : Doc, not_flat : Doc) -> Doc {
  dynamic(fn(ctx) {
    let lr_req = left.requirement() + right.requirement()
    let mid = if ctx.current_remain() >= lr_req { flat } else { not_flat }
    left + mid + right
  })
}

///|
fn init {
  ignore(break_)
  ignore(catifflat(left=empty, right=empty, empty, empty))
}

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
