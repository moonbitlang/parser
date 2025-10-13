//!build: -w -27
type MyType Int

impl ToJson for MyType with to_json(self) {
  { "MyType": self.inner() }
}

fn init {
  let x = 42
  let y = "def"
  let z = [ 1, 2, 3 ]
  let w = MyType(24)
  let json_arr : Array[Json] = [ 1, "2" ]
  let json : Json = [ x, y, z, [ ..json_arr, w ] ]
  println(json.stringify())
  println(H::B("x",3, A(2)).to_json().stringify())
}


enum H {
  A(Int)
  B(String, Int,H)
}

impl ToJson for H with to_json(self) {
  match self {
    A(x) => ["A", x]
    B(x, y, z) => ["B", x, y, z]
  }
}



