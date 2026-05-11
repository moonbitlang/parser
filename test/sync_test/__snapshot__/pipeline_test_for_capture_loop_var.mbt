fn init {
  let mut callbacks = (@list.empty() : @list.List[() -> Unit])
  for i = 0; i < 10; i = i + 1 {
    callbacks = @list.cons(fn () {
      println(i)
    }, callbacks)
  }
  for rest = callbacks {
    match rest {
      @list.List::Empty => break
      @list.List::More(f, tail=fs) => {
        f()
        continue fs
      }
    }
  }
}
