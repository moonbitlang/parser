// minimal example, and also to make sure unbox works

struct T {
  f: (T) -> Unit
}

fn init {
  let t = { f: fn(t) { println("xxx") } }
  (t.f)(t)
}

// mapper example from nes
struct Mapper {
  step: (Mapper) -> Unit
  idx: (Mapper, Int64) -> Int
  idx_set: (Mapper, Int64, Int) -> Unit
}

fn Mapper::step_(self: Mapper) -> Unit {
  (self.step)(self)
}

#alias("_[_]")
fn Mapper::get(self: Mapper, adr: Int64) -> Int {
  (self.idx)(self, adr)
}

#alias("_[_]=_")
fn Mapper::set(self: Mapper, adr: Int64, val: Int) -> Unit {
  (self.idx_set)(self, adr, val)
}

fn init {
  let mapper = {
    step: fn(m) { println("step") },
    idx: fn(m, adr) { (m.step)(m); println(adr); adr.to_int() },
    idx_set: fn(m, adr, val) { (m.step)(m); println((m.idx)(m, adr)); println(val) },
  }

  mapper.step_()
  let _ = mapper[2023L]
  mapper[1110L] = 2824
}
