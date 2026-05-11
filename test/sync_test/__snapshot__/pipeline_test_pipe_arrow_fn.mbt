fn main {
  pipe_basic()
  rev_pipe_basic()
  rev_pipe_chain()
  rev_pipe_constructor()
  rev_pipe_trailing_lambda()
  rev_pipe_trailing_lambda_wildcard()
  rev_pipe_dot_apply()
  pipe_failed()
  pipe_failed2()
  pipe_early_return()
}

fn pipe_basic() -> Unit {
  let r = 0 |> x => { x + 2 }
  println(r == 2)
  let r = 0 |> x => { x } |> y => { y + 3 }
  println(r == 3)
}

fn add(a : Int, b : Int) -> Int {
  a + b
}

fn sub(a : Int, b : Int) -> Int {
  a - b
}

fn sum3(a : Int, b : Int, c : Int) -> Int {
  a * 100 + b * 10 + c
}

fn inc(x : Int) -> Int {
  x + 1
}

fn double(x : Int) -> Int {
  x * 2
}

fn rev_pipe_basic() -> Unit {
  let r = add(1) <| 2
  guard r == 3
  let r = sum3(1, 2) <| 3
  guard r == 123
  let r = sub(5) <| 2
  guard r == 3
}

fn rev_pipe_chain() -> Unit {
  let l = 3 |> inc |> double
  let r = double <| (inc <| 3)
  println(l == r)
  println(r == 8)
}

fn rev_pipe_trailing_lambda() -> Unit {
  fn widget(style : String, f : () -> String) {
    println(style + f())
  }
  widget("abc") <| () => {
    "def"
  }
}

fn rev_pipe_trailing_lambda_wildcard() -> Unit {
  fn widget(style : String, f : (String) -> String) {
    println(f(style))
  }
  widget("abc") <| _ => {
    "ghi"
  }
}

struct App {
  name : String
}

fn App::handler(self : App, path : String, f : (String) -> String) -> String {
  f(self.name + ":" + path)
}

fn App::handler2(self : App, f : (String) -> String) -> String {
  f(self.name)
}

fn rev_pipe_dot_apply() -> Unit {
  let app = App::{ name: "myapp" }
  let r = app.handler("/api") <| (ctx) => {
    "handled " + ctx
  }
  println(r)
  
  let r = app.handler2() <| (ctx) => {
    "handled " + ctx
  }
  println(r)

  let r = (5).add() <| 3
  guard r == 8
}

enum MyEnum {
  Pair(Int, Bool)
} derive(Debug)

fn rev_pipe_constructor() -> Unit {
  debug(Pair(1) <| false)
}

suberror MyErr {
  MyErr(String)
} derive(Debug)

fn pipe_failed() -> Unit {
  debug(try? {
    "msg" |> x => { raise MyErr(x) }
  })
}

fn pipe_failed2() -> Unit {
  debug("msg" |> x => { try? (x + "2") |> fail })
  ("msg3" |> x => { fail(x) } |> println) catch { e => println(e.to_string()) }
  "msg4" |> x => { fail(x) catch { _ => x } } |> println
}

fn pipe_early_return() -> Unit {
  "" |> x => { return }
  println("ok")
}
