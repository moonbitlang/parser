struct LexBuf {
  buffer : String
  mut cursor : Int
}

fn LexBuf::set_backup(_self : LexBuf) -> Unit {
  ()
}

fn LexBuf::clear_backup(_self : LexBuf) -> Unit {
  ()
}

fn LexBuf::refill(_self : LexBuf) -> Unit {
  ()
}

fn LexBuf::eof_reached(self : LexBuf) -> Bool {
  self.cursor >= self.buffer.length()
}

fn LexBuf::get_cursor(self : LexBuf) -> Int {
  self.cursor
}

fn LexBuf::buffer_end(self : LexBuf) -> Int {
  self.buffer.length()
}

fn LexBuf::get_char(self : LexBuf, pos : Int) -> Int {
  if pos < self.buffer.length() {
    self.buffer.unsafe_get(pos).to_int()
  } else {
    -1
  }
}

fn LexBuf::advance(self : LexBuf, len : Int) -> Unit {
  self.cursor += len
}

fn LexBuf::get_stringview(
  self : LexBuf,
  start : Int,
  end : Int,
) -> StringView {
  self.buffer.view(start_offset=start, end_offset=end)
}

fn scan(lexbuf : LexBuf) -> String {
  lexscan lexbuf {
    (re"^[a-z]+") as word => "word:\{word}"
    re"^[0-9]+" => "int"
    re"^ +" => scan(lexbuf)
    _ => "other"
  }
}

fn main {
  let lexbuf = LexBuf::{ buffer: "abc 123 ?!", cursor: 0 }
  println(scan(lexbuf))
  println(scan(lexbuf))
  println(scan(lexbuf))
  println(scan(lexbuf))
  println(lexbuf.get_cursor())
}
