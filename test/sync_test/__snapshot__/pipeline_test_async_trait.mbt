async fn[T] suspend(f : ((T) -> Unit) -> Unit) -> T noraise = "%async.suspend"
fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

trait Stream {
  async fn next(Self) -> Char? noraise
}

struct Input {
  mut consumer : ((Char?) -> Unit)?
}

fn Input::new() -> Input {
  { consumer: None }
}

impl Stream for Input with next(self) {
  suspend(fn (consumer) {
    self.consumer = Some(consumer)
  })
}

suberror NoConsumer

fn Input::feed_char(self : Input, chr : Char) -> Unit raise {
  match self.consumer {
    None => raise NoConsumer
    Some(consumer) => {
      self.consumer = None
      consumer(Some(chr))
    }
  }
}

fn Input::terminate(self : Input) -> Unit {
  match self.consumer {
    None => ()
    Some(consumer) => {
      self.consumer = None
      consumer(None)
    }
  }
}

fn Input::feed_data(self : Input, data : String) -> Unit raise {
  for c in data {
    self.feed_char(c)
  }
}



suberror Err {
  Unmatched_Left
  Unexpected_Right
  Invalid_Char(Char)
}

async fn[S : Stream] is_balanced_poly(s : S) -> Unit raise Err {
  for n = 0, next = s.next() {
    match (n, next) {
      (0, None)      => break
      (n, None)      => raise Unmatched_Left
      (n, Some('(')) => continue n + 1, s.next()
      (0, Some(')')) => raise Unexpected_Right
      (n, Some(')')) => continue n - 1, s.next()
      (_, Some(c))   => raise Invalid_Char(c)
    }
  }
}

fn run_poly(input_sequence : Array[String]) -> Unit {
  let input = Input::new()
  run_async(() => {
    try {
      is_balanced_poly(input)
    } catch {
      err => println(err)
    } noraise {
      _ => println("is balanced")
    }
  })
  try {
    for data in input_sequence {
      println("sending more data ...")
      input.feed_data(data)
    }
    input.terminate()
  } catch {
    err => println("sender received error: \{err}")
  } noraise {
    _ => println("input closed succesfully")
  }
}

fn init {
  println("===")
  run_poly([ "(((", ")(()", ")))" ])
  println("===")
  run_poly([ "(((", ")(()", "))" ])
  println("===")
  run_poly([ "(()", "))()", "()" ])
}

// check if the parethesis in a stream is balanced
async fn is_balanced_obj(s : &Stream) -> Unit raise Err {
  for n = 0, next = s.next() {
    match (n, next) {
      (0, None     ) => break
      (n, None     ) => raise Unmatched_Left
      (n, Some('(')) => continue n + 1, s.next()
      (0, Some(')')) => raise Unexpected_Right
      (n, Some(')')) => continue n - 1, s.next()
      (_, Some(c)  ) => raise Invalid_Char(c)
    }
  }
}

fn run_obj(input_sequence : Array[String]) -> Unit {
  let input = Input::new()
  run_async(() => {
    try {
      is_balanced_obj(input)
    } catch {
      err => println(err)
    } noraise {
      _ => println("is balanced")
    }
  })
  try {
    for data in input_sequence {
      println("sending more data ...")
      input.feed_data(data)
    }
    input.terminate()
  } catch {
    err => println("sender received error: \{err}")
  } noraise {
    _ => println("input closed succesfully")
  }
}

fn init {
  println("===")
  run_obj([ "(((", ")(()", ")))" ])
  println("===")
  run_obj([ "(((", ")(()", "))" ])
  println("===")
  run_obj([ "(()", "))()", "()" ])
}

impl Show for NoConsumer with output(self, logger) {
  logger.write_string("NoConsumer")
}

impl Show for Err with output(self, logger) {
  match self {
    Unmatched_Left => logger.write_string("Unmatched_Left")
    Unexpected_Right => logger.write_string("Unexpected_Right")
    Invalid_Char(v0) => {
      logger.write_string("Invalid_Char(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
  }
}
