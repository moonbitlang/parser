async fn[T] suspend(f : ((T) -> Unit) -> Unit) -> T noraise = "%async.suspend"
fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

async fn pause() -> Unit noraise {
  suspend(k => k(()))
}

async fn scan(lexbuf : @lexbuf.AsyncLexbuf) -> String {
  lexscan lexbuf {
    (re"^[a-z]+") as word => "word:\{word}"
    _ => "token"
  }
}

fn main {
  let mut chunk_index = 0
  let chunks : ReadOnlyArray[String] = ["ab", "cd", "!"]
  let lexbuf = @lexbuf.AsyncLexbuf::from_fn(async fn() {
    pause()
    if chunk_index < chunks.length() {
      let chunk = chunks[chunk_index]
      chunk_index += 1
      Some(chunk)
    } else {
      None
    }
  })
  run_async(async fn() noraise {
    try {
      println(scan(lexbuf))
      println(scan(lexbuf))
      println(lexbuf.__get_cursor())
    } catch {
      _ => panic()
    }
  })
}
