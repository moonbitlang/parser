async fn[T] suspend(f : ((T) -> Unit) -> Unit) -> T noraise = "%async.suspend"
fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

struct LexBuf {
  chunks : FixedArray[String]
  mut chunk_index : Int
  mut buffer : String
  mut cursor : Int
  mut clear_count : Int
  mut refill_count : Int
  mut backup_alive : Bool
}

async fn pause() -> Unit noraise {
  suspend(k => k(()))
}

#inline(never)
async fn LexBuf::refill(self : LexBuf) -> Unit noraise {
  pause()
  if self.chunk_index < self.chunks.length() {
    self.buffer = self.buffer + self.chunks[self.chunk_index]
    self.chunk_index += 1
    self.refill_count += 1
  }
}

#inline(never)
fn LexBuf::eof_reached(self : LexBuf) -> Bool {
  self.chunk_index >= self.chunks.length() && self.cursor >= self.buffer.length()
}

#inline(never)
fn LexBuf::set_backup(self : LexBuf) -> Unit {
  self.backup_alive = true
}

#inline(never)
async fn LexBuf::clear_backup(self : LexBuf) -> Unit noraise {
  pause()
  self.clear_count += 1
  self.backup_alive = false
}

#inline(never)
fn LexBuf::get_cursor(self : LexBuf) -> Int {
  self.cursor
}

#inline(never)
fn LexBuf::buffer_end(self : LexBuf) -> Int {
  self.buffer.length()
}

#inline(never)
fn LexBuf::get_char(self : LexBuf, pos : Int) -> Int {
  if pos < self.buffer.length() {
    self.buffer.unsafe_get(pos).to_int()
  } else {
    -1
  }
}

#inline(never)
fn LexBuf::advance(self : LexBuf, len : Int) -> Unit {
  self.cursor += len
}

#inline(never)
fn LexBuf::get_stringview(
  self : LexBuf,
  start : Int,
  end : Int,
) -> StringView {
  if self.backup_alive {
    self.buffer.view(start_offset=start, end_offset=end)
  } else {
    "lost"
  }
}

async fn scan(lexbuf : LexBuf) -> String noraise {
  lexscan lexbuf {
    (re"^[a-z]+") as word => "word:\{word}:alive=\{lexbuf.backup_alive}"
    _ => "token:alive=\{lexbuf.backup_alive}"
  }
}

fn main {
  let lexbuf = LexBuf::{
    chunks: ["ab", "cd", "!"],
    chunk_index: 0,
    buffer: "",
    cursor: 0,
    clear_count: 0,
    refill_count: 0,
    backup_alive: false,
  }
  run_async(() => {
    println(scan(lexbuf))
    println(scan(lexbuf))
    println(lexbuf.clear_count)
    println(lexbuf.refill_count)
  })
}
