fn init {
  lexscan lexbuf {
    re"[0-9]+" => ()
    re"[a-z]+" if is_alpha => ()
    Token => ()
    x => x
    _ => ()
  }
  lexscan source {
    (re"if" | re"else") as keyword => keyword
    @pkg.Token => ()
  }
}
