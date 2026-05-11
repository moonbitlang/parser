///|
#valtype
struct T(Int,Int) derive(Debug)

///|
#valtype
struct S(String,String) derive(Debug)

///|
enum E {
  A(T)
  B(S)
} derive(Debug)

///|
fn main {
  let a = []
  for i in 0..=10 {
    a.push(T(i, i + 1))
  }
  debug(a)
  for i in 0..=5 {
    let _ = a.unsafe_pop()

  }
  debug(a)
  let b = []
  for i in 0..=10 {
    b.push(S(i.to_string(), (i + 1).to_string()))
  }
  debug(b)
  for i in 0..=5 {
    let _ = b.unsafe_pop()

  }
  debug(b)
  let c = []
  for i in 0..=10 {
    if i % 2 == 0 {
      c.push(A(T(i, i + 1)))
    } else {
      c.push(B(S(i.to_string(), (i + 1).to_string())))
    }
  }
  debug(c)
}
