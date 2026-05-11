fn main {
  let regex = re"a(b+)"
  println(regex.execute("abbb") is Some(_))
  println(regex.execute("ac") is Some(_))
  f()
}

fn f(re? : Regex = re"abc") -> Unit {
  println(re.execute("abc") is Some(_))
}
