suberror MyError {
  MyError(Int)
} derive( Eq, Compare, Hash)

suberror MyError2 derive( Eq, Compare, Hash)

fn f() -> Unit raise MyError {
  raise MyError(42)
}

fn f2() -> Unit raise MyError2 {
  raise MyError2
}

fn[T : Hash] stable_hash(value : T) -> Int {
  let hasher = Hasher(seed = 42)
  hasher.combine(value)
  hasher.finalize()
}

fn[X : Show + Eq + Compare + Hash] use_derived(e0 : X, e : X) -> Unit {
  println(e)
  println(e0 == e)
  println(e == e)
  println(Compare::compare(e0, e))
  println(Compare::compare(e, e0))
  println(Compare::compare(e, e))
  println(stable_hash(e))
  println(stable_hash(e0))
}

fn init {
  println("===")
  let e0 = MyError(0)
  try {
    f()
  } catch {
    e => use_derived(e0, e)
  }
}

fn init {
  println("===")
  let e0 = MyError2
  try {
    f2()
  } catch {
    e => use_derived(e0, e)
  }
}

impl Show for MyError with output(self, logger) {
  match self {
    MyError(v0) => {
      logger.write_string("MyError(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
  }
}

impl Show for MyError2 with output(self, logger) {
  logger.write_string("MyError2")
}
