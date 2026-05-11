const PREFIX = re"a"
const ALT = re"b"
const SUFFIX = re"bc"

fn main {
  let _ = s =~ re"abc"
  let _ = s =~ PREFIX
  let _ = s =~ ((PREFIX + SUFFIX) as matched, before=before, after=after)
  let _ = s =~ ((re"x" | ALT) + SUFFIX, before~, after~)
  let _ = s =~ (((re"l" + PREFIX) | (ALT + re"r")) as whole, before=head)
  let _ = s =~ (re"z", after=tail)
  let _ = s =~ ((PREFIX), before~, after=tail2)
  let _ = s =~ (PREFIX + SUFFIX)
  let _ = s =~ (PREFIX + SUFFIX,)
  let _ = s =~ ((PREFIX + SUFFIX),)
  let _ = s =~ (re"k", before=lead,)
}
