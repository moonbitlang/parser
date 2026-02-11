///|
fn id(f : () -> Unit raise?) -> () -> Unit raise? {
  f
}

///|
fn main {
  id(fn() { println("hello") })()
  let f = id(() => raise Failure::Failure("error")) // can not infer
  // let f : () -> Unit raise Failure = id(fn ()  { raise Failure("error")})
  // let f = id(fn () -> Unit ! Error { raise Failure("error")})
  (try? f()) |> ignore
}

///|
test {
  let f : () -> Unit raise Failure = id(() => raise Failure("error"))
  (try? f()) |> ignore
}
