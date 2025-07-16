///|
let operators_table : @hashmap.T[String, String] = @hashmap.from_array([
  ("_[_]", "op_get"),
  ("_[_]=_", "op_set"),
])
