//!build: -w -27
trait T {
  f(Self) -> Int
}

trait S {
  g(Self) -> Int
}

type TT &T

pub impl S for TT with g(self: TT) -> Int {
  self.inner().f() + 1001
}

type MyInt Int

impl T for MyInt with f(self) -> Int { self.inner() + 999 }

fn init {
  let a = use_s(TT::TT(MyInt::MyInt(0)))
  println(a)
}

fn use_s(s: &S) -> Int {
  s.g()
}

trait Sub: T {
  h(Self) -> Int
}

impl Sub for MyInt with h(_self) -> Int { 42 }

fn init {
  let myi: MyInt = MyInt(41)
  let obj_Sub = myi as &Sub
  let obj_T = obj_Sub as &T
  println(obj_T.f())
}
