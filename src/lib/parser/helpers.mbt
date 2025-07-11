///|
pub fn make_int(s : String) -> Constant {
  if s.has_suffix("L") {
    let base = s.substring(end=s.length() - 1)
    if base.has_suffix("U") {
      let u64 = base.substring(end=base.length() - 1)
      Constant::UInt64(u64)
    } else {
      Constant::Int64(base)
    }
  } else if s.has_suffix("U") {
    let u32 = s.substring(end=s.length() - 1)
    Constant::UInt(u32)
  } else if s.has_suffix("N") {
    let bigint = s.substring(end=s.length() - 1)
    Constant::BigInt(bigint)
  } else {
    Constant::Int(s)
  }
}

///|
pub fn make_double(f : String) -> Constant {
  Constant::Double(f)
}

///|
pub fn make_float(f : String) -> Constant {
  if f.has_suffix("F") {
    let base = f.substring(end=f.length() - 1)
    Constant::Float(base)
  } else {
    abort("make_float: string must end with F")
  }
}

///|
pub fn mk_loc(pos : (Position, Position)) -> Location {
  Location::{ start: pos.0, end: pos.1 }
}

///|
pub fn[A, B] list_map(
  list : @list.T[A],
  f : (A) -> B
) -> @list.T[B] {
  list.map(f)
}

///|
pub fn[A] list_cons(elem : A, list : @list.T[A]) -> @list.T[A] {
  list.add(elem)
}

///|
pub fn[A] list_rev(list : @list.T[A]) -> @list.T[A] {
  list.rev()
}
