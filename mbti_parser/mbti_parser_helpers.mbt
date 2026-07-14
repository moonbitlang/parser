///|
fn mk_loc(loc : (Position, Position)) -> Location {
  { start: loc.0, end: loc.1 }
}

///|
fn mk_base_pos(pos : Position) -> @basic.BasePos {
  @basic.BasePos::from_position(pkg=@basic.current_package.val, {
    ..pos,
    lnum: 0,
  })
}

///|
fn make_int_literal(lit : String) -> Constant {
  if lit.has_suffix("L") {
    let base = lit.sub(end=lit.length() - 1)
    if base.has_suffix("U") {
      UInt64(base.sub(end=base.length() - 1).to_owned())
    } else {
      Int64(base.to_owned())
    }
  } else if lit.has_suffix("U") {
    UInt(lit.sub(end=lit.length() - 1).to_owned())
  } else if lit.has_suffix("N") {
    BigInt(lit.sub(end=lit.length() - 1).to_owned())
  } else {
    Int(lit)
  }
}

///|
fn make_float_literal(lit : String) -> Constant {
  if lit.has_suffix("F") {
    Float(lit.sub(end=lit.length() - 1).to_owned())
  } else {
    Float(lit)
  }
}
