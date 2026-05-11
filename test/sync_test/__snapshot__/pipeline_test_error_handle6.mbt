suberror DoubleError {
  DoubleError(Double)
} derive(Debug)

suberror Int64Error {
  Int64Error(Int64)
} derive(Debug)

suberror RBError {
  RBError(Ref[Bool])
} derive(Debug)

suberror BytesError {
  BytesError(Bytes)
} derive(Debug)

suberror StringError {
  StringError(String)
} derive(Debug)

fn f1(b: Bool) -> Double raise DoubleError {
  if b { 1.0 } else { raise DoubleError(2.0) }
}

fn f2(b: Bool) -> Double raise Int64Error {
  if b { 1.0 } else { raise Int64Error(2L) }
}

fn f3(b: Bool) -> Int64 raise DoubleError {
  if b { 1L } else { raise DoubleError(2.0) }
}

fn f4(b: Bool) -> Int64 raise RBError {
  if b { 1L } else { raise RBError({ val: false }) }
}

fn f5(b: Bool) -> ((Bool)->Int) raise Int64Error {
  if b { fn(x) { if x { 42 } else {43} } } else { raise Int64Error(-1L) }
}

fn f6(b: Bool) -> Option[Int64] raise DoubleError {
  if b { Some(42L) } else { raise DoubleError(-1.0) }
}

fn f7(b: Bool) -> Double raise BytesError {
  if b { 1.0 } else { raise BytesError(b"a\x00b\x00") }
}

fn[V: Debug] g(f: (Bool) -> V raise Error, b: Bool) -> Unit {
  try { f(b) |> debug } catch {
    DoubleError(d) => println(d)
    Int64Error(d) => println(d)
    RBError(d) => debug(d)
    BytesError(d) => println(d)
    _ => ()
  }
}

fn init {
  g(x => f1(x), true)
  g(x => f1(x), false)
  g(x => f2(x), true)
  g(x => f2(x), false)
  g(x => f3(x), true)
  g(x => f3(x), false)
  g(x => f4(x), true)
  g(x => f4(x), false)
  try {
    debug(f5(true)(true))
  } catch { s => debug(s) }
  try {
    debug(f5(false)(true))
  } catch { s => debug(s) }
  g(x => f6(x), true)
  g(x => f6(x), false)
  g(x => f7(x), true)
  g(x => f7(x), false)
}
