// `reb"..."` is one token wherever `re"..."` is accepted. Its body is kept
// raw, so `\xff` needs no double escaping.
const METHOD = reb"^(GET|POST)"

fn main {
  let r = reb"\xef\xbb\xbf"
  let _ = data =~ reb"^GET /"
  let _ = data =~ ((METHOD + reb" " + (reb"[^ ]+" as path)), after=rest)
  lexmatch data {
    (reb"[0-9]+" as n, after=rest) => ()
    reb"\r\n" | METHOD => ()
    _ => ()
  }
  // Empty, with an escaped quote, and inside a string interpolation.
  let _ = (reb"", reb"a\"b", "x\{reb"[\x00-\x1f]"}y", "x\{reb"a}b"}y")
  // `reb` without a quote is an identifier, also next to a literal.
  let reb = 1
  let _ = (reb, reb"a", re"a", b"a")
}
