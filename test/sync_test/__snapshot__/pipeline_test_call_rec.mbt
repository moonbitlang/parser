// Regression test: constructing a recursive newtype from a non-trivial payload
// expression.
//
// A recursive newtype is erased to `T_any`, so monofy keeps the `Make_newtype`
// upcast alive to carry the type. That cast reaches the MachineIR backends as
// an `Lcast`, and `Machine_of_clam_lower.cast_source_ltype` has to be able to
// name the type of whatever produced the payload. It used to only accept a
// handful of source forms, so each shape below ICEd the aarch64/x86_64
// backends with "cast source form is not yet lowered".
//
// The payload expressions are load-bearing: reducing any of them to a plain
// variable or literal still compiles and still prints the same numbers, while
// losing the form it was added to cover.

struct S(FixedArray[S])

fn make_payload() -> FixedArray[S] {
  []
}

// payload is a call result -> Lapply
fn from_call() -> S {
  S(make_payload())
}

// payload comes from a conditional -> Lif, and after optimization Lapply
fn from_if(b : Bool) -> S {
  S(if b { make_payload() } else { [] })
}

// payload comes from a block with a local binding -> Llet
fn from_block() -> S {
  S({
    let payload = make_payload()
    payload
  })
}

// payload comes from a pattern match -> Lswitch
fn from_match(o : FixedArray[S]?) -> S {
  S(match o {
    Some(payload) => payload
    None => make_payload()
  })
}

// `Holder` must stay a heap object for the read to survive as a field access:
// a single-field struct is scalarized away and the payload arrives as a plain
// variable, which never exercised this path.
struct Holder {
  payload : FixedArray[S]
  mut counter : Int
}

// payload comes from a struct field -> Lget_field
fn from_field(h : Holder) -> S {
  S(h.payload)
}

// payload comes from a fixed array element -> Larray_get_item
fn from_array(xs : FixedArray[FixedArray[S]]) -> S {
  S(xs[1])
}

fn main {
  println(from_call().0.length())
  println(from_if(true).0.length())
  println(from_block().0.length())
  println(from_match(None).0.length())
  println(from_match(Some([S([])])).0.length())
  let h : Holder = { payload: [], counter: 0 }
  h.counter = h.counter + 1
  println(from_field(h).0.length() + h.counter)
  println(from_array([[], [S([])]]).0.length())
}
