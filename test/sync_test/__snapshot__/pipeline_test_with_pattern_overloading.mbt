enum E {
  A(Int64)
  B
}

fn f(e: E) -> Unit {
  match e {
    A(i) | (B with i = 0) => println(i)
  }
}

pub(all) enum NestedE {
  NestedA(Int64?)
}

pub fn nested_default_hint(e : NestedE) -> Int64 {
  match e {
    NestedA(Some(i) | (None with i = 0)) => i
  }
}

pub(all) enum Inner {
  First
  Second
}

pub(all) enum Outer {
  Has(Inner)
  Empty
}

pub fn constructor_default_hint(e : Outer) -> Inner {
  match e {
    Has(First as value) | (Empty with value = Second) => value
  }
}

fn main {
  f(A(42))
  f(B)
  println(nested_default_hint(NestedA(Some(42))))
  println(nested_default_hint(NestedA(None)))
  match constructor_default_hint(Has(First)) {
    First => println(1)
    Second => println(2)
  }
  match constructor_default_hint(Empty) {
    First => println(1)
    Second => println(2)
  }
}
