///|
async fn[T] suspend(f : ((T) -> Unit) -> Unit) -> T noraise + nocancel = "%async.suspend"

///|
async fn[T, E : Error] suspend_result(
  f : ((T) -> Unit, (E) -> Unit) -> Unit,
) -> T raise E = "%async.suspend"

///|
fn run_async(f : async () -> Unit noraise) -> Unit = "%async.run"

///|
// Resume outside run_async so the callbacks really suspend before continuing.
struct Scheduler {
  pending : Array[() -> Unit]
}

///|
async fn Scheduler::pause(self : Self) -> Unit noraise {
  suspend(k => self.pending.push(() => k(())))
}

///|
async fn Scheduler::emit(
  self : Self,
  writer : StringBuilder,
  text : String,
) -> Unit noraise {
  self.pause()
  writer.write_string(text)
}

///|
suberror CallbackError {
  Failed(String)
}

///|
async fn Scheduler::read(
  self : Self,
  fail : Bool,
) -> String raise CallbackError {
  suspend_result((resolve, reject) => {
    self.pending.push(() => {
      if fail {
        reject(Failed("async"))
      } else {
        resolve("value")
      }
    })
  })
}

///|
fn fail_sync() -> Unit raise CallbackError {
  raise Failed("sync")
}

///|
fn run_case(
  name : String,
  body : async (Scheduler, StringBuilder) -> Unit noraise,
) -> Unit {
  let scheduler = { pending: [], }
  let writer = StringBuilder()
  let mut completed = false
  run_async(() => {
    body(scheduler, writer)
    completed = true
  })
  println("\{name} initial: \{writer.to_string()}/completed=\{completed}")
  let mut resumes = 0
  while scheduler.pending.pop() is Some(step) {
    resumes += 1
    step()
  }
  println(
    "\{name} completed: \{writer.to_string()}/completed=\{completed}/resumes=\{resumes}",
  )
}

///|
// Async is inferred for the arrow callback; return exits only the callback.
async fn callback_return(
  scheduler : Scheduler,
  writer : StringBuilder,
  stop : Bool,
) -> Unit noraise {
  writer <+
    "A\{w => { defer w.write_string("D"); w.write_string("B"); scheduler.pause(); w.write_string("C"); if stop { return }; scheduler.pause(); w.write_string("E") }}Z"
  writer.write_string("/outer")
}

///|
// Both normal and error-only cleanup can suspend. Failure skips later writes.
async fn callback_propagate(
  scheduler : Scheduler,
  writer : StringBuilder,
  fail : Bool,
) -> Unit noraise {
  try {
    writer <+
      "A\{w => { defer scheduler.emit(w, "D"); errdefer scheduler.emit(w, "E"); w.write_string("B"); w.write_string(scheduler.read(fail)); w.write_string("C") }}Z\{w => w.write_string("later")}"
    writer.write_string("/ok")
  } catch {
    Failed(message) => writer.write_string("/caught:" + message)
  }
  writer.write_string("/outer")
}

///|
// An error during deferred cleanup overrides a successful callback return.
async fn callback_defer_error(
  scheduler : Scheduler,
  writer : StringBuilder,
) -> Unit noraise {
  try {
    writer <+
      "A\{w => { defer { w.write_string("D"); ignore(scheduler.read(true)) }; scheduler.pause(); w.write_string("B"); return }}Z"
    writer.write_string("/ok")
  } catch {
    Failed(message) => writer.write_string("/caught:" + message)
  }
  writer.write_string("/outer")
}

///|
// A locally handled error allows a noraise callback, including an async catch.
async fn callback_recover(
  scheduler : Scheduler,
  writer : StringBuilder,
  fail : Bool,
) -> Unit noraise {
  writer <+
    "A\{w => { try { w.write_string(scheduler.read(fail)) } catch { Failed(_) => { scheduler.emit(w, "caught"); return } }; w.write_string("C") }}Z"
  writer.write_string("/outer")
}

///|
// A synchronous error must propagate both before and after suspension.
async fn callback_sync_error(
  scheduler : Scheduler,
  writer : StringBuilder,
  pause_first : Bool,
) -> Unit noraise {
  try {
    writer <+
      "A\{w => { defer w.write_string("D"); w.write_string("B"); if pause_first { scheduler.pause() }; fail_sync(); w.write_string("unreachable") }}Z"
    writer.write_string("/ok")
  } catch {
    Failed(message) => writer.write_string("/caught:" + message)
  }
  writer.write_string("/outer")
}

///|
// None must skip suspension, errors, and subsequent interpolation side effects.
async fn callback_conditional(
  scheduler : Scheduler,
  writer : StringBuilder,
  present : Bool,
  fail : Bool,
) -> Unit noraise {
  let optional = if present { Some(writer) } else { None }
  let mut callbacks = 0
  let mut expressions = 0
  try {
    optional <?
      "A\{w => { callbacks += 1; w.write_string(scheduler.read(fail)); return }}Z\{{ expressions += 1; "value" }}"
    writer.write_string("/ok")
  } catch {
    Failed(message) => writer.write_string("/caught:" + message)
  }
  writer.write_string(
    "/callbacks=\{callbacks}/expressions=\{expressions}/outer",
  )
}

///|
// Async return expressions in nested callbacks must resume the right template.
async fn callback_nested(
  scheduler : Scheduler,
  writer : StringBuilder,
) -> Unit noraise {
  writer <+
    "A\{w => { w <+ "B\{inner => { return scheduler.emit(inner, "C") }}D"; return scheduler.emit(w, "E") }}Z"
  writer.write_string("/outer")
}

///|
async fn callback_retry(
  scheduler : Scheduler,
  writer : StringBuilder,
) -> Unit noraise {
  writer <+
    "A\{w => { for attempt in 0..<3 { try { w.write_string(scheduler.read(attempt == 0)); return } catch { Failed(_) => w.write_string("retry") } }; w.write_string("unreachable") }}Z"
  writer.write_string("/outer")
}

///|
fn main {
  run_case("return", (s, w) => callback_return(s, w, true))
  run_case("fallthrough", (s, w) => callback_return(s, w, false))
  run_case("propagate", (s, w) => callback_propagate(s, w, true))
  run_case("success", (s, w) => callback_propagate(s, w, false))
  run_case("defer error", callback_defer_error)
  run_case("recover", (s, w) => callback_recover(s, w, true))
  run_case("no recovery", (s, w) => callback_recover(s, w, false))
  run_case("sync error", (s, w) => callback_sync_error(s, w, false))
  run_case("resumed sync error", (s, w) => callback_sync_error(s, w, true))
  run_case("some success", (s, w) => callback_conditional(s, w, true, false))
  run_case("some error", (s, w) => callback_conditional(s, w, true, true))
  run_case("none", (s, w) => callback_conditional(s, w, false, true))
  run_case("nested", callback_nested)
  run_case("retry", callback_retry)
}
