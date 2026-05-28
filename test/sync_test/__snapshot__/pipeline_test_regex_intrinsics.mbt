///|
trait Seq {
  fn seq(Self, Self) -> Self
}

///|
#intrinsic("%regex.alt")
fn regex_alt(re1 : Regex, re2 : Regex) -> Regex {
  re1 | re2
}

///|
const RA : Regex = re"a"

///|
const R : Regex = RA + re"b"

///|
#intrinsic("%regex.seq")
impl Seq for Regex with seq(re1, re2) {
  re1 + re2
}

///|
fn main {
  let r = regex_alt(Seq::seq(re"a", re"b"), re"c")
  guard r.execute("ab") is Some(_) &&
    r.execute("c") is Some(_) &&
    r.execute("a") is None
  guard R.execute("ab") is Some(_) && R.execute("a") is None
}
