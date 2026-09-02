///|
#valtype
struct UMul {
  lo : UInt64
  hi : UInt64
}

///|
#valtype
struct IMul {
  lo : UInt64
  hi : Int64
}

///|
#intrinsic("%u64.mul_wide")
fn umul_wide(a : UInt64, b : UInt64) -> UMul {
  let mask = 0xffffffffUL
  let alo = a & mask
  let ahi = a >> 32
  let blo = b & mask
  let bhi = b >> 32
  let ll = alo * blo
  let lh = alo * bhi
  let hl = ahi * blo
  let mid = (ll >> 32) + (lh & mask) + (hl & mask)
  let hi = ahi * bhi + (lh >> 32) + (hl >> 32) + (mid >> 32)
  UMul::{ lo: a * b, hi }
}

///|
#intrinsic("%i64.mul_wide")
fn imul_wide(a : Int64, b : Int64) -> IMul {
  let unsigned = umul_wide(a.reinterpret_as_uint64(), b.reinterpret_as_uint64())
  let ca = if a < 0L { b.reinterpret_as_uint64() } else { 0UL }
  let cb = if b < 0L { a.reinterpret_as_uint64() } else { 0UL }
  IMul::{ lo: unsigned.lo, hi: (unsigned.hi - ca - cb).reinterpret_as_int64() }
}

///|
fn uadd128(alo : UInt64, ahi : UInt64, blo : UInt64, bhi : UInt64) -> UMul {
  let lo = alo + blo
  let carry = if lo < alo { 1UL } else { 0UL }
  UMul::{ lo, hi: ahi + bhi + carry }
}

///|
fn usub128(alo : UInt64, ahi : UInt64, blo : UInt64, bhi : UInt64) -> UMul {
  let borrow = if alo < blo { 1UL } else { 0UL }
  UMul::{ lo: alo - blo, hi: ahi - bhi - borrow }
}

///|
fn ref_umulh(a : UInt64, b : UInt64) -> UInt64 {
  let mask = 0xffffffffUL
  let alo = a & mask
  let ahi = a >> 32
  let blo = b & mask
  let bhi = b >> 32
  let ll = alo * blo
  let lh = alo * bhi
  let hl = ahi * blo
  let mid = (ll >> 32) + (lh & mask) + (hl & mask)
  ahi * bhi + (lh >> 32) + (hl >> 32) + (mid >> 32)
}

///|
fn ref_smulh(a : Int64, b : Int64) -> Int64 {
  let hi = ref_umulh(a.reinterpret_as_uint64(), b.reinterpret_as_uint64())
  let ca = if a < 0L { b.reinterpret_as_uint64() } else { 0UL }
  let cb = if b < 0L { a.reinterpret_as_uint64() } else { 0UL }
  (hi - ca - cb).reinterpret_as_int64()
}

///|
fn ref_add128(alo : UInt64, ahi : UInt64, blo : UInt64, bhi : UInt64) -> UMul {
  let lo = alo + blo
  let carry = if lo < alo { 1UL } else { 0UL }
  UMul::{ lo, hi: ahi + bhi + carry }
}

///|
fn ref_sub128(alo : UInt64, ahi : UInt64, blo : UInt64, bhi : UInt64) -> UMul {
  let borrow = if alo < blo { 1UL } else { 0UL }
  UMul::{ lo: alo - blo, hi: ahi - bhi - borrow }
}

///|
fn show_u(r : UMul) -> Unit {
  println(r.lo.to_string() + " " + r.hi.to_string())
}

///|
fn show_i(r : IMul) -> Unit {
  println(r.lo.to_string() + " " + r.hi.to_string())
}

///|
fn main {
  let umax = 0xffffffffffffffffUL
  show_u(umul_wide(umax, umax))
  show_u(umul_wide(umax, 2UL))
  show_u(umul_wide(0x123456789abcdefUL, 0xfedcba9876543210UL))
  show_u(umul_wide(0UL, umax))
  show_u(umul_wide(1UL, umax))
  let min64 = -9223372036854775807L - 1L
  let max64 = 9223372036854775807L
  show_i(imul_wide(min64, min64))
  show_i(imul_wide(min64, -1L))
  show_i(imul_wide(min64, max64))
  show_i(imul_wide(-1L, -1L))
  show_i(imul_wide(-1L, 1L))
  show_i(imul_wide(123456789L, -987654321L))
  // 128-bit add/sub: carry and borrow propagation
  show_u(uadd128(umax, umax, 1UL, 0UL))
  show_u(uadd128(umax, 0UL, 1UL, 0UL))
  show_u(uadd128(1UL, 2UL, 3UL, 4UL))
  show_u(usub128(0UL, 0UL, 1UL, 0UL))
  show_u(usub128(0UL, 1UL, 1UL, 0UL))
  show_u(usub128(5UL, 7UL, 3UL, 2UL))
  // randomized cross-check against the plain-MoonBit references above
  let mut x = 0x9e3779b97f4a7c15UL
  let mut ok = 0
  for i = 0; i < 1000; i = i + 1 {
    x = x * 6364136223846793005UL + 1442695040888963407UL
    let y = x ^ (x >> 29)
    let u = umul_wide(x, y)
    let s = imul_wide(x.reinterpret_as_int64(), y.reinterpret_as_int64())
    let u_ok = u.lo == x * y && u.hi == ref_umulh(x, y)
    let s_ok = s.lo == x * y &&
      s.hi == ref_smulh(x.reinterpret_as_int64(), y.reinterpret_as_int64())
    let a = uadd128(x, y, y, x)
    let ra = ref_add128(x, y, y, x)
    let d = usub128(x, y, y, x)
    let rd = ref_sub128(x, y, y, x)
    let a_ok = a.lo == ra.lo && a.hi == ra.hi
    let d_ok = d.lo == rd.lo && d.hi == rd.hi
    if u_ok && s_ok && a_ok && d_ok {
      ok = ok + 1
    }
  }
  println("random ok: " + ok.to_string())
}
