//!build: -w -27
type T1 Int

fn f1(x: T1) -> Unit {
  println(x.inner())
}

type T2 Array[Int]

fn f2(x: T2) -> Unit {
  println(x.inner())
}

type T3[X] X?

fn f3(x: T3[String]) -> Unit {
  println(x.inner())
}

fn f4(x: String) -> T3[String] {
 Some(x)
}

fn f5(x: String) -> String? {
 Some(x)
}

struct S {
  v: (Int, Int)
}

type T4[X] (Int, X)

fn f6(x: T4[Int]) -> Unit {
  println(x.inner())
}

fn init {
  f1(42)
  f1(40 + 2)
  f1(T1(42))
  f2([42])
  f2(T2::T2([42]))
  f3(Some("hello"))
  f3(T3(Some("hello")))
  f3(f4("hello"))
  f3(f5("hello"))
  let s = S :: { v: (40, 2) }
  f6(s.v)
}

enum E {
  NewE(Int)
} derive(Show)

type NewE E derive(Show)

fn f7(x: NewE) -> Unit {
  println(x)
}

fn init {
  f7(NewE(NewE(42)))
}

type T5 () -> Unit
fn f8(x : T5) -> Unit {
  (x.inner())()
}

type T6 S
fn f9(x : T6) -> Unit {
  println(x.inner().v)
}

fn init {
  f8(fn () { println("yes") })
  f9({ v: (4, 2) })
}
