enum Foo {
  Bar1
  Bar2
  Bar3
  Nested(Foo)
}

fn main {
  match 'c' {
    ('a'..='z'|'A'..='Z'|'_') as c => println("id")
  } 
  match Bar1 {
    (Bar1 | Bar2 as c) | Nested(Bar1 | Bar2 as c) => println("id")
  }
	match foo {
    ParsingStart(
      Cons(
        '('
        | ')'
        | '['
        | ']'
        | '{'
        | '}'
        | ':'
        | ',' | '?' | '!' | '=' | '~' | '@' | '.' | '|' | '>' | '<' | '='
				| 'A'..='Z' | 'a'..='z'  
        as
        x,
        xs
      )
    ) => parse(ParsingStart(xs), Cons(Symbol(String::from_array([x])), spans,spans,spans,spans))
	}
	match foo {
		Ctor1(a______________________________________________________________) as aaaaaaa => ()
	}
}

fn corner_case() -> Unit {
	match foo {
		(((((Ctor1(a,b) as aaaaaaa) as bbbbbb) as cccccccc) as ddddddd) as eeeeeeee) as ffffffff => ()
	}
}
