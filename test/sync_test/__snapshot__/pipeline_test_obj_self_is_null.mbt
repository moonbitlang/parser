fn use_debug(obj : &Debug) -> Unit {
  // use twice to trigger [incref]
  debug(obj)
  debug(obj)
}

fn init {
  use_debug((None : String?))
}
