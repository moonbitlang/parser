fn main {
  let _ = s =~ ()
  let _ = s =~ (, before=head)
  let _ = s =~ (re"a", before=)
  let _ = s =~ (re"a", before=lead,, after=tail)
  let _ = s =~ ((re"a" |), before=head)
  let _ = s =~ ((re"a" as), before=head)
  let _ = s =~ ((re"a" re"b"), before=head after=tail)
  let _ = s =~ ((PREFIX SUFFIX), before=head)
  let _ = s =~ ((re"x" | ALT) SUFFIX, before=head)
  let _ = s =~ non_constant
  let _ = s =~ (@pkg.non_constant)
}
