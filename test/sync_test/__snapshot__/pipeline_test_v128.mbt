// The default implementations in this file are executed for real: the
// machine/LLVM backends never specialize intrinsic calls, and the C
// backend falls back to them for instructions without a fast path on the
// target ISA. They must therefore implement the wasm semantics. Whether
// specialization fires is checked by the IR snapshots (e.g. *.exp.wat);
// wrong immediates are caught by assertions whose input vectors make
// every lane distinguishable.

fn v128_make(lo : UInt64, hi : UInt64) -> V128 = "%v128.make"

fn v128_lo(v : V128) -> UInt64 = "%v128.lo"

fn v128_hi(v : V128) -> UInt64 = "%v128.hi"

#intrinsic("%v128.v128_const_i8x16")
fn v128_const(
  l0 : Int,
  l1 : Int,
  l2 : Int,
  l3 : Int,
  l4 : Int,
  l5 : Int,
  l6 : Int,
  l7 : Int,
  l8 : Int,
  l9 : Int,
  l10 : Int,
  l11 : Int,
  l12 : Int,
  l13 : Int,
  l14 : Int,
  l15 : Int
) -> V128 {
  fn byte(v : Int, shift : Int) -> UInt64 {
    v.to_uint64().land(0xffUL) << shift
  }

  v128_make(
    byte(l0, 0)
    .lor(byte(l1, 8))
    .lor(byte(l2, 16))
    .lor(byte(l3, 24))
    .lor(byte(l4, 32))
    .lor(byte(l5, 40))
    .lor(byte(l6, 48))
    .lor(byte(l7, 56)),
    byte(l8, 0)
    .lor(byte(l9, 8))
    .lor(byte(l10, 16))
    .lor(byte(l11, 24))
    .lor(byte(l12, 32))
    .lor(byte(l13, 40))
    .lor(byte(l14, 48))
    .lor(byte(l15, 56)),
  )
}

#intrinsic("%v128.v128_not")
fn v128_not(value : V128) -> V128 {
  v128_make(
    v128_lo(value).lxor(0xffffffffffffffffUL),
    v128_hi(value).lxor(0xffffffffffffffffUL),
  )
}

#intrinsic("%v128.v128_and")
fn v128_and(value : V128, other : V128) -> V128 {
  v128_make(v128_lo(value).land(v128_lo(other)), v128_hi(value).land(v128_hi(other)))
}

#intrinsic("%v128.v128_andnot")
fn v128_andnot(value : V128, other : V128) -> V128 {
  v128_make(
    v128_lo(value).land(v128_lo(other).lxor(0xffffffffffffffffUL)),
    v128_hi(value).land(v128_hi(other).lxor(0xffffffffffffffffUL)),
  )
}

#intrinsic("%v128.v128_or")
fn v128_or(value : V128, other : V128) -> V128 {
  v128_make(v128_lo(value).lor(v128_lo(other)), v128_hi(value).lor(v128_hi(other)))
}

#intrinsic("%v128.v128_xor")
fn v128_xor(value : V128, other : V128) -> V128 {
  v128_make(v128_lo(value).lxor(v128_lo(other)), v128_hi(value).lxor(v128_hi(other)))
}

#intrinsic("%v128.v128_bitselect")
fn v128_bitselect(value : V128, other : V128, mask : V128) -> V128 {
  v128_make(
    v128_lo(value).land(v128_lo(mask)).lor(v128_lo(other).land(v128_lo(mask).lxor(0xffffffffffffffffUL))),
    v128_hi(value).land(v128_hi(mask)).lor(v128_hi(other).land(v128_hi(mask).lxor(0xffffffffffffffffUL))),
  )
}

// exact lane-wise wrap-around add: the lanes' top bits are summed
// separately, so carries cannot cross lane boundaries
fn swar_add(a : UInt64, b : UInt64, top_bits : UInt64) -> UInt64 {
  let low = top_bits.lxor(0xffffffffffffffffUL)
  (a.land(low) + b.land(low)).lxor(a.lxor(b).land(top_bits))
}

// lane-wise sub as a + (-b), with per-lane negation ~b + 1
fn swar_sub(a : UInt64, b : UInt64, top_bits : UInt64, lane_ones : UInt64) -> UInt64 {
  swar_add(a, swar_add(b.lxor(0xffffffffffffffffUL), lane_ones, top_bits), top_bits)
}

#intrinsic("%v128.i8x16_add")
fn i8x16_add(value : V128, other : V128) -> V128 {
  v128_make(
    swar_add(v128_lo(value), v128_lo(other), 0x8080808080808080UL),
    swar_add(v128_hi(value), v128_hi(other), 0x8080808080808080UL),
  )
}

#intrinsic("%v128.i8x16_sub")
fn i8x16_sub(value : V128, other : V128) -> V128 {
  v128_make(
    swar_sub(v128_lo(value), v128_lo(other), 0x8080808080808080UL, 0x0101010101010101UL),
    swar_sub(v128_hi(value), v128_hi(other), 0x8080808080808080UL, 0x0101010101010101UL),
  )
}

#intrinsic("%v128.i16x8_add")
fn i16x8_add(value : V128, other : V128) -> V128 {
  v128_make(
    swar_add(v128_lo(value), v128_lo(other), 0x8000800080008000UL),
    swar_add(v128_hi(value), v128_hi(other), 0x8000800080008000UL),
  )
}

#intrinsic("%v128.i16x8_sub")
fn i16x8_sub(value : V128, other : V128) -> V128 {
  v128_make(
    swar_sub(v128_lo(value), v128_lo(other), 0x8000800080008000UL, 0x0001000100010001UL),
    swar_sub(v128_hi(value), v128_hi(other), 0x8000800080008000UL, 0x0001000100010001UL),
  )
}

#intrinsic("%v128.i32x4_add")
fn i32x4_add(value : V128, other : V128) -> V128 {
  v128_make(
    swar_add(v128_lo(value), v128_lo(other), 0x8000000080000000UL),
    swar_add(v128_hi(value), v128_hi(other), 0x8000000080000000UL),
  )
}

#intrinsic("%v128.i32x4_sub")
fn i32x4_sub(value : V128, other : V128) -> V128 {
  v128_make(
    swar_sub(v128_lo(value), v128_lo(other), 0x8000000080000000UL, 0x0000000100000001UL),
    swar_sub(v128_hi(value), v128_hi(other), 0x8000000080000000UL, 0x0000000100000001UL),
  )
}

#intrinsic("%v128.i32x4_extract_lane")
fn i32x4_extract_lane(value : V128, lane : Int) -> Int {
  let word = if lane < 2 { v128_lo(value) } else { v128_hi(value) }
  (word >> (lane % 2 * 32)).land(0xffffffffUL).reinterpret_as_int64().to_int()
}

#intrinsic("%v128.i32x4_replace_lane")
fn i32x4_replace_lane(value : V128, replacement : Int, lane : Int) -> V128 {
  let shift = lane % 2 * 32
  let cleared = (0xffffffffUL << shift).lxor(0xffffffffffffffffUL)
  let repl = replacement.to_uint64().land(0xffffffffUL) << shift
  if lane < 2 {
    v128_make(v128_lo(value).land(cleared).lor(repl), v128_hi(value))
  } else {
    v128_make(v128_lo(value), v128_hi(value).land(cleared).lor(repl))
  }
}

#intrinsic("%v128.i64x2_add")
fn i64x2_add(value : V128, other : V128) -> V128 {
  v128_make(v128_lo(value) + v128_lo(other), v128_hi(value) + v128_hi(other))
}

#intrinsic("%v128.i64x2_sub")
fn i64x2_sub(value : V128, other : V128) -> V128 {
  v128_make(v128_lo(value) - v128_lo(other), v128_hi(value) - v128_hi(other))
}

#intrinsic("%v128.i8x16_shuffle")
fn i8x16_shuffle(
  value : V128,
  other : V128,
  l0 : Int,
  l1 : Int,
  l2 : Int,
  l3 : Int,
  l4 : Int,
  l5 : Int,
  l6 : Int,
  l7 : Int,
  l8 : Int,
  l9 : Int,
  l10 : Int,
  l11 : Int,
  l12 : Int,
  l13 : Int,
  l14 : Int,
  l15 : Int
) -> V128 {
  fn u8_lane(v : V128, lane : Int) -> UInt64 {
    let word = if lane < 8 { v128_lo(v) } else { v128_hi(v) }
    (word >> (lane % 8 * 8)).land(0xffUL)
  }

  fn pick(idx : Int) -> UInt64 {
    if idx < 16 {
      u8_lane(value, idx)
    } else {
      u8_lane(other, idx - 16)
    }
  }

  let lanes : FixedArray[Int] = [
    l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15,
  ]
  let mut lo = 0UL
  let mut hi = 0UL
  for i in 0..<8 {
    lo = lo.lor(pick(lanes[i]) << (i * 8))
    hi = hi.lor(pick(lanes[i + 8]) << (i * 8))
  }
  v128_make(lo, hi)
}

suberror V128Error


struct V128Box {
  v : V128
  tag : Int
}

fn check(name : String, v : V128, lo : UInt64, hi : UInt64) -> Unit {
  if v128_lo(v) == lo && v128_hi(v) == hi {
    println("\{name}: ok")
  } else {
    println("\{name}: fail")
  }
}

fn check_bool(name : String, b : Bool) -> Unit {
  if b {
    println("\{name}: ok")
  } else {
    println("\{name}: fail")
  }
}

fn make_by_index(i : Int) -> V128 {
  match i {
    0 => v128_make(0x0000000000000000UL, 0x0000000000000001UL)
    1 => v128_make(0x1111111111111111UL, 0x1111111111111112UL)
    2 => v128_make(0x2222222222222222UL, 0x2222222222222223UL)
    3 => v128_make(0x3333333333333333UL, 0x3333333333333334UL)
    4 => v128_make(0x4444444444444444UL, 0x4444444444444445UL)
    _ => v128_make(0xffffffffffffffffUL, 0xeeeeeeeeeeeeeeeeUL)
  }
}

fn choose(flag : Bool, a : V128, b : V128) -> V128 {
  if flag { a } else { b }
}

fn pass_through(v : V128) -> V128 {
  v128_make(v128_lo(v), v128_hi(v))
}

fn maybe_v128(ok : Bool) -> V128 raise V128Error {
  if ok {
    make_by_index(4)
  } else {
    raise V128Error
  }
}

fn check_v128_result(name : String, r : Result[V128, V128Error], lo : UInt64, hi : UInt64) -> Unit {
  match r {
    Ok(v) => check(name, v, lo, hi)
    Err(_) => println("\{name}: fail")
  }
}

fn check_v128_error(name : String, r : Result[V128, V128Error]) -> Unit {
  match r {
    Ok(_) => println("\{name}: fail")
    Err(_) => println("\{name}: ok")
  }
}

fn make_box(i : Int) -> V128Box {
  { v: make_by_index(i), tag: i }
}

// Shuffle with lane indices supplied at runtime (a [FixedArray] parameter),
// so they are not compile-time constants. wasm must encode shuffle lanes as
// immediates and falls back to the default implementation; the C backend
// keeps the intrinsic and lowers to a runtime byte permute.
fn shuffle_runtime(value : V128, other : V128, idx : FixedArray[Int]) -> V128 {
  i8x16_shuffle(
    value, other, idx[0], idx[1], idx[2], idx[3], idx[4], idx[5], idx[6], idx[7],
    idx[8], idx[9], idx[10], idx[11], idx[12], idx[13], idx[14], idx[15],
  )
}

fn bitstring_v128_from_bytes(bytes : Bytes) -> V128 {
  match bytes {
    [v128le(value), ..] => value
    _ => abort("expected Bytes v128 bitstring")
  }
}

fn bitstring_v128_from_bytesview(bytes : BytesView) -> V128 {
  match bytes {
    [u32le(_), v128le(value), ..] => value
    _ => abort("expected BytesView v128 bitstring")
  }
}

fn bitstring_v128_from_array(bytes : Array[Byte]) -> V128 {
  match bytes {
    [u32le(_), v128le(value), ..] => value
    _ => abort("expected Array v128 bitstring")
  }
}

fn bitstring_v128_from_arrayview(bytes : ArrayView[Byte]) -> V128 {
  match bytes {
    [u32le(_), v128le(value), ..] => value
    _ => abort("expected ArrayView v128 bitstring")
  }
}

fn bitstring_v128_from_fixedarray(bytes : FixedArray[Byte]) -> V128 {
  match bytes {
    [u32le(_), v128le(value), ..] => value
    _ => abort("expected FixedArray v128 bitstring")
  }
}

fn bitstring_v128_from_readonlyarray(bytes : ReadOnlyArray[Byte]) -> V128 {
  match bytes {
    [u32le(_), v128le(value), ..] => value
    _ => abort("expected ReadOnlyArray v128 bitstring")
  }
}

fn init {
  let a = v128_make(0x0123456789abcdefUL, 0xfedcba9876543210UL)
  let b = v128_make(0x1111111122222222UL, 0x3333333344444444UL)
  let c = v128_make(0xffffffffffffffffUL, 0x8000000000000000UL)
  check("zero bits", v128_make(0UL, 0UL), 0UL, 0UL)
  check("small bits", v128_make(1UL, 2UL), 1UL, 2UL)
  check("lane order", a, 0x0123456789abcdefUL, 0xfedcba9876543210UL)
  check("all ones and sign bit", c, 0xffffffffffffffffUL, 0x8000000000000000UL)
  check(
    "v128 const intrinsic",
    v128_const(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    0x0706050403020100UL,
    0x0f0e0d0c0b0a0908UL,
  )
  check(
    "v128 not intrinsic",
    v128_not(v128_make(0x0123456789abcdefUL, 0xfedcba9876543210UL)),
    0xfedcba9876543210UL,
    0x0123456789abcdefUL,
  )
  check(
    "v128 and intrinsic",
    v128_and(
      v128_make(0x0f0f0f0f0f0f0f0fUL, 0xffff0000ffff0000UL),
      v128_make(0x3333333333333333UL, 0x00ff00ff00ff00ffUL),
    ),
    0x0303030303030303UL,
    0x00ff000000ff0000UL,
  )
  check(
    "v128 andnot intrinsic",
    v128_andnot(
      v128_make(0xffff0000ffff0000UL, 0x0f0f0f0f0f0f0f0fUL),
      v128_make(0x00ff00ff00ff00ffUL, 0x3333333333333333UL),
    ),
    0xff000000ff000000UL,
    0x0c0c0c0c0c0c0c0cUL,
  )
  check(
    "v128 or intrinsic",
    v128_or(
      v128_make(0x0f0f0f0f00000000UL, 0x00000000f0f0f0f0UL),
      v128_make(0x00000000f0f0f0f0UL, 0x0f0f0f0f00000000UL),
    ),
    0x0f0f0f0ff0f0f0f0UL,
    0x0f0f0f0ff0f0f0f0UL,
  )
  check(
    "v128 xor intrinsic",
    v128_xor(
      v128_make(0xffff0000ffff0000UL, 0xaaaaaaaa55555555UL),
      v128_make(0x00ff00ff00ff00ffUL, 0xffff0000ffff0000UL),
    ),
    0xff0000ffff0000ffUL,
    0x5555aaaaaaaa5555UL,
  )
  check(
    "v128 bitselect intrinsic",
    v128_bitselect(
      v128_make(0xffff0000ffff0000UL, 0xaaaaaaaa55555555UL),
      v128_make(0x0000ffff0000ffffUL, 0x55555555aaaaaaaaUL),
      v128_make(0xff00ff00ff00ff00UL, 0xffff0000ffff0000UL),
    ),
    0xff0000ffff0000ffUL,
    0xaaaa55555555aaaaUL,
  )
  check(
    "i8x16 add intrinsic",
    i8x16_add(
      v128_make(0x0102030405060708UL, 0x1112131415161718UL),
      v128_make(0x0101010101010101UL, 0x0101010101010101UL),
    ),
    0x0203040506070809UL,
    0x1213141516171819UL,
  )
  check(
    "i8x16 sub intrinsic",
    i8x16_sub(
      v128_make(0x1011121314151617UL, 0x2021222324252627UL),
      v128_make(0x0101010101010101UL, 0x0101010101010101UL),
    ),
    0x0f10111213141516UL,
    0x1f20212223242526UL,
  )
  check(
    "i16x8 add intrinsic",
    i16x8_add(
      v128_make(0x0001000200030004UL, 0x0005000600070008UL),
      v128_make(0x0001000100010001UL, 0x0001000100010001UL),
    ),
    0x0002000300040005UL,
    0x0006000700080009UL,
  )
  check(
    "i16x8 sub intrinsic",
    i16x8_sub(
      v128_make(0x0010001100120013UL, 0x0020002100220023UL),
      v128_make(0x0001000100010001UL, 0x0001000100010001UL),
    ),
    0x000f001000110012UL,
    0x001f002000210022UL,
  )
  check(
    "i32x4 add intrinsic",
    i32x4_add(
      v128_make(0x0000000100000002UL, 0x0000000300000004UL),
      v128_make(0x0000000100000001UL, 0x0000000100000001UL),
    ),
    0x0000000200000003UL,
    0x0000000400000005UL,
  )
  check(
    "i32x4 sub intrinsic",
    i32x4_sub(
      v128_make(0x0000001000000011UL, 0x0000002000000021UL),
      v128_make(0x0000000100000001UL, 0x0000000100000001UL),
    ),
    0x0000000f00000010UL,
    0x0000001f00000020UL,
  )
  check_bool(
    "i32x4 extract lane intrinsic",
    i32x4_extract_lane(v128_make(0x0000000100000002UL, 0x0000000300000004UL), 3) == 3,
  )
  check(
    "i32x4 replace lane intrinsic",
    i32x4_replace_lane(v128_make(0x0000000100000002UL, 0x0000000300000004UL), 9, 0),
    0x0000000100000009UL,
    0x0000000300000004UL,
  )
  check(
    "i64x2 add intrinsic",
    i64x2_add(v128_make(0x0000000000000001UL, 0x0000000000000002UL), v128_make(3UL, 4UL)),
    4UL,
    6UL,
  )
  check(
    "i64x2 sub intrinsic",
    i64x2_sub(v128_make(0x0000000000000010UL, 0x0000000000000020UL), v128_make(1UL, 2UL)),
    0x000000000000000fUL,
    0x000000000000001eUL,
  )
  check(
    "i8x16 shuffle intrinsic",
    i8x16_shuffle(a, b, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    0x0123456789abcdefUL,
    0xfedcba9876543210UL,
  )
  check(
    "i8x16 shuffle across both vectors",
    i8x16_shuffle(a, b, 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23),
    0x228922ab22cd22efUL,
    0x1101112311451167UL,
  )
  // same [0, 16, 1, 17, ...] cross-vector permutation, but with lane indices
  // built at runtime so the shuffle cannot be specialized to immediates
  let idx : FixedArray[Int] = FixedArray::make(16, 0)
  for i in 0..<8 {
    idx[i * 2] = i
    idx[i * 2 + 1] = i + 16
  }
  check(
    "i8x16 shuffle runtime lanes",
    shuffle_runtime(a, b, idx),
    0x228922ab22cd22efUL,
    0x1101112311451167UL,
  )
  check("rebuild from bits", pass_through(a), 0x0123456789abcdefUL, 0xfedcba9876543210UL)
  check("if true", choose(true, a, b), 0x0123456789abcdefUL, 0xfedcba9876543210UL)
  check("if false", choose(false, a, b), 0x1111111122222222UL, 0x3333333344444444UL)
  check_bool("physical equal same bits", physical_equal(a, pass_through(a)))
  check_bool("physical equal different bits", physical_equal(a, b) == false)

  let xs : Array[V128] = Array::make(5, a)
  check_bool("array length", xs.length() == 5)
  check("array make first", xs.unsafe_get(0), 0x0123456789abcdefUL, 0xfedcba9876543210UL)
  check("array make middle", xs.unsafe_get(2), 0x0123456789abcdefUL, 0xfedcba9876543210UL)
  check("array make last", xs.unsafe_get(4), 0x0123456789abcdefUL, 0xfedcba9876543210UL)
  xs.unsafe_set(1, b)
  xs.unsafe_set(3, c)
  check("array set first preserved", xs.unsafe_get(0), 0x0123456789abcdefUL, 0xfedcba9876543210UL)
  check("array set index 1", xs.unsafe_get(1), 0x1111111122222222UL, 0x3333333344444444UL)
  check("array set neighbor preserved", xs.unsafe_get(2), 0x0123456789abcdefUL, 0xfedcba9876543210UL)
  check("array set index 3", xs.unsafe_get(3), 0xffffffffffffffffUL, 0x8000000000000000UL)
  check("array set last preserved", xs.unsafe_get(4), 0x0123456789abcdefUL, 0xfedcba9876543210UL)
  let xs_copy = xs.copy()
  check_bool("array copy length", xs_copy.length() == 5)
  check("array copy index 1", xs_copy.unsafe_get(1), 0x1111111122222222UL, 0x3333333344444444UL)
  check("array copy index 3", xs_copy.unsafe_get(3), 0xffffffffffffffffUL, 0x8000000000000000UL)

  let empty_xs : Array[V128] = []
  check_bool("empty array literal length", empty_xs.length() == 0)
  let zero_xs : Array[V128] = Array::make(0, a)
  check_bool("array make zero length", zero_xs.length() == 0)

  let ys : Array[V128] = Array::makei(6, make_by_index)
  check_bool("array makei length", ys.length() == 6)
  check("array makei index 0", ys.unsafe_get(0), 0x0000000000000000UL, 0x0000000000000001UL)
  check("array makei index 3", ys.unsafe_get(3), 0x3333333333333333UL, 0x3333333333333334UL)
  check("array makei index 5", ys.unsafe_get(5), 0xffffffffffffffffUL, 0xeeeeeeeeeeeeeeeeUL)

  let zs : FixedArray[V128] = FixedArray::makei(4, make_by_index)
  check_bool("fixedarray length", zs.length() == 4)
  check("fixedarray index 0", zs.unsafe_get(0), 0x0000000000000000UL, 0x0000000000000001UL)
  check("fixedarray index 2", zs.unsafe_get(2), 0x2222222222222222UL, 0x2222222222222223UL)
  check("fixedarray index 3", zs.unsafe_get(3), 0x3333333333333333UL, 0x3333333333333334UL)
  let zs_copy = zs.copy()
  check_bool("fixedarray copy length", zs_copy.length() == 4)
  check("fixedarray copy index 2", zs_copy.unsafe_get(2), 0x2222222222222222UL, 0x2222222222222223UL)

  let empty_zs : FixedArray[V128] = []
  check_bool("empty fixedarray literal length", empty_zs.length() == 0)
  let zero_zs : FixedArray[V128] = FixedArray::make(0, a)
  check_bool("fixedarray make zero length", zero_zs.length() == 0)

  let ok_result : Result[V128, V128Error] = to_result(() => maybe_v128(true))
  let err_result : Result[V128, V128Error] = to_result(() => maybe_v128(false))
  check_v128_result(
    "result v128 ok",
    ok_result,
    0x4444444444444444UL,
    0x4444444444444445UL,
  )
  check_v128_error("result v128 err", err_result)

  let boxes : FixedArray[V128Box] = FixedArray::makei(4, make_box)
  let box2 = boxes.unsafe_get(2)
  let box3 = boxes.unsafe_get(3)
  check_bool("struct v128 field length", boxes.length() == 4)
  check_bool("struct v128 field tag", box3.tag == 3)
  check("struct v128 field index 2", box2.v, 0x2222222222222222UL, 0x2222222222222223UL)
  check("struct v128 field index 3", box3.v, 0x3333333333333333UL, 0x3333333333333334UL)

  let mem : FixedArray[Byte] = FixedArray::make(32, 0)
  for i in 0..<32 {
    mem[i] = i.to_byte()
  }
  check(
    "v128 load intrinsic",
    v128_load(mem, 0),
    0x0706050403020100UL,
    0x0f0e0d0c0b0a0908UL,
  )
  check(
    "v128 load unaligned",
    v128_load(mem, 1),
    0x0807060504030201UL,
    0x100f0e0d0c0b0a09UL,
  )
  v128_store(mem, 4, v128_make(0xa1a2a3a4a5a6a7a8UL, 0xb1b2b3b4b5b6b7b8UL))
  check(
    "v128 store intrinsic",
    v128_load(mem, 4),
    0xa1a2a3a4a5a6a7a8UL,
    0xb1b2b3b4b5b6b7b8UL,
  )
  check_bool("v128 store preserves prefix", mem[3] == 3)
  check_bool("v128 store preserves suffix", mem[20] == 20)
  let bitstring_bytes = Bytes::makei(24, i => i.to_byte())
  let bitstring_array : Array[Byte] = Array::makei(24, i => i.to_byte())
  let bitstring_fixed : FixedArray[Byte] = FixedArray::makei(24, i => i.to_byte())
  let bitstring_readonly : ReadOnlyArray[Byte] =
    ReadOnlyArray::from_array(bitstring_array)
  let padded_bytes = Bytes::makei(28, i => if i < 2 { 0 } else { (i - 2).to_byte() })
  let padded_array : Array[Byte] =
    Array::makei(28, i => if i < 2 { 0 } else { (i - 2).to_byte() })
  check(
    "bitstring v128 Bytes",
    bitstring_v128_from_bytes(bitstring_bytes),
    0x0706050403020100UL,
    0x0f0e0d0c0b0a0908UL,
  )
  check(
    "bitstring v128 BytesView",
    bitstring_v128_from_bytesview(padded_bytes[2:26]),
    0x0b0a090807060504UL,
    0x131211100f0e0d0cUL,
  )
  check(
    "bitstring v128 Array",
    bitstring_v128_from_array(bitstring_array),
    0x0b0a090807060504UL,
    0x131211100f0e0d0cUL,
  )
  check(
    "bitstring v128 ArrayView",
    bitstring_v128_from_arrayview(padded_array[2:26]),
    0x0b0a090807060504UL,
    0x131211100f0e0d0cUL,
  )
  check(
    "bitstring v128 FixedArray",
    bitstring_v128_from_fixedarray(bitstring_fixed),
    0x0b0a090807060504UL,
    0x131211100f0e0d0cUL,
  )
  check(
    "bitstring v128 ReadOnlyArray",
    bitstring_v128_from_readonlyarray(bitstring_readonly),
    0x0b0a090807060504UL,
    0x131211100f0e0d0cUL,
  )
  check(
    "v128 load8 splat intrinsic",
    v128_load8_splat(mem, 21),
    0x1515151515151515UL,
    0x1515151515151515UL,
  )
  check(
    "v128 load32 zero intrinsic",
    v128_load32_zero(mem, 20),
    0x0000000017161514UL,
    0UL,
  )
  let lane_target = v128_make(0x1111111111111111UL, 0x2222222222222222UL)
  check(
    "v128 load16 lane intrinsic",
    v128_load16_lane(mem, 2, lane_target, 5),
    0x1111111111111111UL,
    0x2222222203022222UL,
  )
  check(
    "v128 load16 lane fallback",
    v128_load16_lane(mem, 2, lane_target, pick_lane(mem[0] == 0)),
    0x1111111111111111UL,
    0x2222222203022222UL,
  )
  let out : FixedArray[Byte] = FixedArray::make(8, 0)
  v128_store64_lane(out, 0, v128_make(0xdeadbeefcafebabeUL, 0x0102030405060708UL), 1)
  check_bool("v128 store64 lane intrinsic", load_u64_le(out, 0) == 0x0102030405060708UL)
  // buffers whose last use is the specialized load/store: exercises the
  // ref-count drop inserted at the intrinsic call site
  let scratch : FixedArray[Byte] = FixedArray::make(16, b'\x07')
  check(
    "v128 load last use",
    v128_load(scratch, 0),
    0x0707070707070707UL,
    0x0707070707070707UL,
  )
  let sink : FixedArray[Byte] = FixedArray::make(16, 0)
  v128_store(sink, 0, v128_make(0x4242424242424242UL, 0x4343434343434343UL))
  println("v128 store last use: ok")
  // read 8 consecutive UInt16 lanes from a FixedArray[UInt16] at an element
  // offset; exercises the C (uint16_t*) and wasm-linear (base + offset<<1) fast
  // paths and the u16 fallback body on the remaining backends
  let mem16 : FixedArray[UInt16] = FixedArray::make(16, 0)
  for i in 0..<16 {
    mem16[i] = (i + 1).to_uint16()
  }
  check(
    "v128 load i16x8 intrinsic",
    v128_load_i16x8(mem16, 0),
    0x0004000300020001UL,
    0x0008000700060005UL,
  )
  check(
    "v128 load i16x8 offset",
    v128_load_i16x8(mem16, 2),
    0x0006000500040003UL,
    0x000a000900080007UL,
  )
  // buffer whose last use is the specialized load: exercises the ref-count drop
  // inserted at the intrinsic call site for the borrowed FixedArray[UInt16]
  let scratch16 : FixedArray[UInt16] = FixedArray::make(8, 0x0707)
  check(
    "v128 load i16x8 last use",
    v128_load_i16x8(scratch16, 0),
    0x0707070707070707UL,
    0x0707070707070707UL,
  )
  check(
    "v128 const i16x8 intrinsic",
    i16x8_const(1, 2, 3, 4, 5, 6, 7, -1),
    0x0004000300020001UL,
    0xffff000700060005UL,
  )
  check(
    "v128 const i32x4 intrinsic",
    i32x4_const(0x11223344, -1, 0, 0x7fffffff),
    0xffffffff11223344UL,
    0x7fffffff00000000UL,
  )
  check(
    "v128 const i64x2 intrinsic",
    i64x2_const(0x0102030405060708UL, 0xf0e0d0c0b0a09080UL),
    0x0102030405060708UL,
    0xf0e0d0c0b0a09080UL,
  )
  check(
    "v128 const f32x4 intrinsic",
    f32x4_const(1.5, -2.0, 0.0, 3.25),
    0xc00000003fc00000UL,
    0x4050000000000000UL,
  )
  check(
    "v128 const f64x2 intrinsic",
    f64x2_const(1.5, -0.0),
    0x3ff8000000000000UL,
    0x8000000000000000UL,
  )
  // write 8 consecutive UInt16 lanes into a FixedArray[UInt16] at an element
  // offset; exercises the C (uint16_t*) and wasm-linear (base + offset<<1) fast
  // paths and the u16 fallback body on the remaining backends
  v128_store_i16x8(
    mem16,
    2,
    v128_make(0x4444333322221111UL, 0x8888777766665555UL),
  )
  check(
    "v128 store i16x8 intrinsic",
    v128_load_i16x8(mem16, 2),
    0x4444333322221111UL,
    0x8888777766665555UL,
  )
  check_bool("v128 store i16x8 preserves prefix", mem16[1].to_int() == 2)
  check_bool("v128 store i16x8 preserves suffix", mem16[10].to_int() == 11)
  // buffer whose last use is the specialized store: exercises the ref-count
  // drop inserted at the intrinsic call site for the borrowed FixedArray[UInt16]
  let sink16 : FixedArray[UInt16] = FixedArray::make(8, 0)
  v128_store_i16x8(
    sink16,
    0,
    v128_make(0x4242424242424242UL, 0x4343434343434343UL),
  )
  println("v128 store i16x8 last use: ok")
}

fn load_u64_le(bytes : FixedArray[Byte], offset : Int) -> UInt64 {
  let mut result = 0UL
  for i in 0..=7 {
    result = result.lor(bytes[offset + i].to_uint64() << (8 * i))
  }
  result
}

fn store_u64_le(bytes : FixedArray[Byte], offset : Int, value : UInt64) -> Unit {
  for i in 0..=7 {
    bytes[offset + i] = (value >> (8 * i)).to_byte()
  }
}

fn pick_lane(b : Bool) -> Int {
  if b {
    5
  } else {
    0
  }
}

fn f32_to_i32_reinterpret(a : Float) -> Int = "%f32.to_i32_reinterpret"

fn u64_of_low_32(v : Int) -> UInt64 {
  v.to_uint64().land(0xffffffffUL)
}

#intrinsic("%v128.v128_const_i16x8")
fn i16x8_const(
  l0 : Int,
  l1 : Int,
  l2 : Int,
  l3 : Int,
  l4 : Int,
  l5 : Int,
  l6 : Int,
  l7 : Int
) -> V128 {
  fn word(a : Int, b : Int, c : Int, d : Int) -> UInt64 {
    u64_of_low_32(a & 0xffff)
    .lor(u64_of_low_32(b & 0xffff) << 16)
    .lor(u64_of_low_32(c & 0xffff) << 32)
    .lor(u64_of_low_32(d & 0xffff) << 48)
  }

  v128_make(word(l0, l1, l2, l3), word(l4, l5, l6, l7))
}

#intrinsic("%v128.v128_const_i32x4")
fn i32x4_const(l0 : Int, l1 : Int, l2 : Int, l3 : Int) -> V128 {
  v128_make(
    u64_of_low_32(l0).lor(u64_of_low_32(l1) << 32),
    u64_of_low_32(l2).lor(u64_of_low_32(l3) << 32),
  )
}

#intrinsic("%v128.v128_const_i64x2")
fn i64x2_const(l0 : UInt64, l1 : UInt64) -> V128 {
  v128_make(l0, l1)
}

#intrinsic("%v128.v128_const_f32x4")
fn f32x4_const(l0 : Float, l1 : Float, l2 : Float, l3 : Float) -> V128 {
  fn bits(x : Float) -> UInt64 {
    u64_of_low_32(f32_to_i32_reinterpret(x))
  }

  v128_make(bits(l0).lor(bits(l1) << 32), bits(l2).lor(bits(l3) << 32))
}

#intrinsic("%v128.v128_const_f64x2")
fn f64x2_const(l0 : Double, l1 : Double) -> V128 {
  v128_make(
    l0.reinterpret_as_int64().reinterpret_as_uint64(),
    l1.reinterpret_as_int64().reinterpret_as_uint64(),
  )
}

#intrinsic("%v128.v128_load")
#borrow(bytes)
fn v128_load(bytes : FixedArray[Byte], offset : Int) -> V128 {
  v128_make(load_u64_le(bytes, offset), load_u64_le(bytes, offset + 8))
}

#intrinsic("%v128.v128_load_i16x8")
#borrow(bytes)
fn v128_load_i16x8(bytes : FixedArray[UInt16], offset : Int) -> V128 {
  let mut lo = 0UL
  for i in 0..=3 {
    lo = lo.lor(bytes[offset + i].to_uint64() << (16 * i))
  }
  let mut hi = 0UL
  for i in 0..=3 {
    hi = hi.lor(bytes[offset + 4 + i].to_uint64() << (16 * i))
  }
  v128_make(lo, hi)
}

#intrinsic("%v128.v128_store")
#borrow(bytes)
fn v128_store(bytes : FixedArray[Byte], offset : Int, value : V128) -> Unit {
  store_u64_le(bytes, offset, v128_lo(value))
  store_u64_le(bytes, offset + 8, v128_hi(value))
}

#intrinsic("%v128.v128_load8_splat")
#borrow(bytes)
fn v128_load8_splat(bytes : FixedArray[Byte], offset : Int) -> V128 {
  let b = bytes[offset].to_uint64()
  let mut word = 0UL
  for i in 0..=7 {
    word = word.lor(b << (8 * i))
  }
  v128_make(word, word)
}

#intrinsic("%v128.v128_load32_zero")
#borrow(bytes)
fn v128_load32_zero(bytes : FixedArray[Byte], offset : Int) -> V128 {
  let mut word = 0UL
  for i in 0..=3 {
    word = word.lor(bytes[offset + i].to_uint64() << (8 * i))
  }
  v128_make(word, 0UL)
}

#intrinsic("%v128.v128_load16_lane")
#borrow(bytes)
fn v128_load16_lane(
  bytes : FixedArray[Byte],
  offset : Int,
  value : V128,
  lane : Int
) -> V128 {
  let loaded = bytes[offset].to_uint64().lor(bytes[offset + 1].to_uint64() << 8)
  let shift = lane % 4 * 16
  let cleared = (0xffffUL << shift).lxor(0xffffffffffffffffUL)
  if lane < 4 {
    v128_make(v128_lo(value).land(cleared).lor(loaded << shift), v128_hi(value))
  } else {
    v128_make(v128_lo(value), v128_hi(value).land(cleared).lor(loaded << shift))
  }
}

#intrinsic("%v128.v128_store64_lane")
#borrow(bytes)
fn v128_store64_lane(
  bytes : FixedArray[Byte],
  offset : Int,
  value : V128,
  lane : Int
) -> Unit {
  if lane == 0 {
    store_u64_le(bytes, offset, v128_lo(value))
  } else {
    store_u64_le(bytes, offset, v128_hi(value))
  }
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}

#intrinsic("%v128.v128_store_i16x8")
#borrow(bytes)
fn v128_store_i16x8(
  bytes : FixedArray[UInt16],
  offset : Int,
  value : V128
) -> Unit {
  for i in 0..=3 {
    bytes[offset + i] = (v128_lo(value) >> (16 * i)).to_uint16()
  }
  for i in 0..=3 {
    bytes[offset + 4 + i] = (v128_hi(value) >> (16 * i)).to_uint16()
  }
}
