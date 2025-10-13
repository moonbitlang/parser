//!build: -w -27
// derived Eq + Compare
type T1 Int derive(Compare, Eq)
let t1: T1 = T1(1)

type T2 Int derive(Eq, Compare)
let t2: T2 = T2(2)

// manual Eq + derived Compare
type T3 Int derive(Compare)
let t3: T3 = T3(3)

impl Eq for T3 with equal(x: T3, y: T3) -> Bool {
  println("T3::op_equal")
  x.inner() == y.inner()
}

// derived Eq + manual Compare
type T4 Int derive(Eq)
let t4: T4 = T4(4)

impl Compare for T4 with compare(x: T4, y: T4) -> Int {
  println("T4::compare")
  x.inner().compare(y.inner())
}

// manual Eq + Compare
type T5 Int
let t5: T5 = T5(5)

impl Eq for T5 with equal(x: T5, y: T5) -> Bool {
  println("T5::op_equal")
  x.inner() == y.inner()
}

impl Compare for T5 with compare(x: T5, y: T5) -> Int {
  println("T5::compare")
  x.inner().compare(y.inner())
}


fn init {
  println("===")
  let _ = Compare::compare(t1, t1)
  let _ = Compare::compare(t2, t2)
  let _ = Compare::compare(t3, t3)
  let _ = Compare::compare(t4, t4)
  let _ = Compare::compare(t5, t5)
}

fn[X: Eq] useEq(x: X) -> Unit {
  let _ = x == x
}

fn[X: Compare] useCompare(x: X) -> Unit {
  let _ = x.compare(x)
  let _ = x == x
  let _ = Eq::equal(x, x)
  let _ = useEq(x)
}

fn init {
  println("===")
  let _ = useCompare(t1)
  let _ = useCompare(t2)
  let _ = useCompare(t3)
  let _ = useCompare(t4)
  let _ = useCompare(t5)
}
