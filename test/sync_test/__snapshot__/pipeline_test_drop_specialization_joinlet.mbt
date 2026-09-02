// Regression for !8590: an `Ljoinlet` finishes either by falling through its
// body or by applying its join point, so the data flow facts drop
// specialization collects at the end of the body describe the fallthrough path
// only. The pass used to hand those facts (the surviving `incref`s, the shape
// of the result, the `env`) straight back as the result of the whole
// `Ljoinlet`, which made it fuse reference count operations that the join point
// path never performs.
//
// The symptom is a leak, not a crash: the deferred `incref` only runs on the
// fallthrough path, but the `decref` it gets fused with is deleted on both
// paths, so taking the `break` over-counts. Program output therefore cannot
// catch this -- the signal lives in the promoted
// `test/wasm1_test/irs/drop_specialization_joinlet.mbt.exp.rc.clam1`, where
// each function below must keep the `incref` inside the join let body and the
// matching `decref` after it.

struct Payload {
  a : String
  b : String
}

struct Holder {
  mut fst : Payload
  snd : Payload
}

// `#inline(never)` keeps `Holder` boxed and `Payload` a runtime value; if these
// are inlined the holder is scalar replaced away and there is no field read for
// drop specialization to track.
#inline(never)
fn make_payload(s : String) -> Payload {
  { a: s, b: s }
}

#inline(never)
fn make_holder(fst : Payload, snd : Payload) -> Holder {
  { fst, snd }
}

// `selected` takes over the reference owned by `holder.fst` only when falling
// through, so the `decref` of the old `holder.fst` below must not be fused with
// the `incref` deferred at the end of the body. `holder` outlives the join let,
// which is what puts that `decref` after it.
#inline(never)
fn joinlet_field_exchange(
  take_break : Bool,
  alternate : Payload,
  other : Payload,
) -> String {
  let holder = make_holder(make_payload("fst"), make_payload("snd"))
  let selected = done~: {
    if take_break {
      break done~ alternate
    }
    holder.fst
  }
  holder.fst = other
  selected.a
}

// Same aliasing, consumed by the specialized `decref` of `holder` instead of by
// the exchange pattern: the unique branch must not skip the `decref` of
// `holder.fst`.
#inline(never)
fn joinlet_field_drop(take_break : Bool, alternate : Payload) -> String {
  let holder = make_holder(make_payload("fst"), make_payload("snd"))
  let selected = done~: {
    if take_break {
      break done~ alternate
    }
    holder.fst
  }
  let tail = holder.snd.b
  selected.a + tail
}

fn init {
  let alternate = make_payload("alt")
  let other = make_payload("other")
  // fallthrough path, then the `break` path that the stale facts do not describe
  println(joinlet_field_exchange(false, alternate, other))
  println(joinlet_field_exchange(true, alternate, other))
  println(joinlet_field_drop(false, alternate))
  println(joinlet_field_drop(true, alternate))
}
