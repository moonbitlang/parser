///|
struct S(Int, S?) derive(Show, Eq, Hash, Default)

fn main {
  let s1 = S::default()
  let s2 = S::S(1, Some(s1))
  println(s2)
  println(s2 == s2)
  println(s1 == s2)
  println(s2.hash())
  println(f(s1))
  println(f(s2))
}

fn f(s: S) -> S {
  match s {
    S(_) as a => {
      println(a.0)
      a.1.unwrap_or(a)
    }
  }
}
