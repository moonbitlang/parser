//!build: -w -62

fn init {
  let v = [ 1, 2, 3 ]
  v..push(4)..push(5)..push(6)
  println(v)
}

// interaction with `!`
fn init {
  let buf : MyBuf = @buffer.new()
  try {
    buf
    ..write_string("a")
    ..expect(content="a")
    ..write_string("b")
    ..expect(content="b")
    println("ok")
  } catch {
    InspectError(err) => println(err)
  }
}

fn init {
  let buf : MyBuf = @buffer.new()
  try {
    buf
    ..write_string("a")
    ..expect(content="b")
    ..write_string("b")
    ..expect(content="b")
    println("ok")
  } catch {
    _ => println("error")
  }
}

// evaluation order related
fn init {
  { println("self"); [] }
  ..push({ println("arg1"); 1 })
  ..push({ println("arg2"); 2 })
  .to_string() |> println
}

fn init {
  let arr1 = []
  let arr2 = []
  let mut arr = arr1
  arr..push({ arr = arr2; 1 })..push(2)
  println((arr1, arr2))
}

struct MyBuf (@buffer.Buffer)

fn MyBuf::write_string(self : MyBuf, s : String) -> Unit {
  self.0.write_string(s)
}

fn MyBuf::expect(self : MyBuf, content~ : String = "") -> Unit raise {
  inspect(self.inner(), content~)
  self.0.reset()
}

// non-unit return type
fn init {
  let dq = @deque.of([ @ref.new(1), @ref.new(2), @ref.new(3), @ref.new(4) ])
  dq..pop_back()..pop_back()..pop_back()
  println(dq.pop_back())
}
