// Corner cases of integer constant folding in `const` declarations.
// The printed runtime values must equal the compile-time folded values.
//
// Note: Int16/UInt16 arithmetic and `==` are typed target-dependently
// (primitives on C/LLVM, trait methods elsewhere), so this test only uses
// their shift/bitwise/negation operators, which type the same on every
// target; the 16-bit arithmetic folding values are covered in
// test/error_test/constant.mbt, and the shared compute-through-Int wrap
// semantics are exercised at runtime here via Byte.

const IntWrapAdd : Int = 2147483647 + 1

const IntDivTrunc : Int = -7 / 2

const IntModSign : Int = -7 % 2

const IntShrArith : Int = -8 >> 1

const UIntWrapSub : UInt = 0U - 1U

const I64WrapAdd : Int64 = 9223372036854775807L + 1L

const I64MulWrap : Int64 = (-9223372036854775807L - 1L) * -1L

const I64DivTrunc : Int64 = -7L / 2L

const I64NegShl : Int64 = -(1L << 62)

const U64Shl : UInt64 = 1UL << 32

const U64Mask : UInt64 = (1UL << 32) - 1UL

const U64WrapSub : UInt64 = 0UL - 1UL

const I16Min : Int16 = -32768

const I16NegMin : Int16 = -I16Min

const I16ShlTrunc : Int16 = 1 << 20

const I16ShrArith : Int16 = -8 >> 1

const I16Bits : Int16 = (0x55 ^ 0x0F) | 0x100

const U16ShrLogic : UInt16 = 0x8000 >> 1

const U16Bits : UInt16 = (0xFF00 & 0x0FF0) ^ 0x000F

const ByteWrapAdd : Byte = b'\xFF' + b'\x01'

const ByteWrapSub : Byte = b'\x01' - b'\x02'

const ByteMulWrap : Byte = b'\x10' * b'\x10'

const ByteShlTrunc : Byte = b'\x01' << 9

const ByteShrLogic : Byte = b'\x80' >> 1

// shift counts are masked by the operand width at runtime (wasm/js i32/i64
// semantics); negative and oversized counts must fold to the same values
const IntNegShift : Int = 1 << -1

const IntBigShift : Int = 1 << 33

const U64NegShift : UInt64 = 1UL << -1

const I64BigShift : Int64 = 1L << 65

const I16NegShift : Int16 = 1 << -1

const ByteNegShift : Byte = b'\x01' << -1

fn init {
  println(IntWrapAdd)
  println(IntDivTrunc)
  println(IntModSign)
  println(IntShrArith)
  println(UIntWrapSub)
  println(I64WrapAdd)
  println(I64MulWrap)
  println(I64DivTrunc)
  println(I64NegShl)
  println(U64Shl)
  println(U64Mask)
  println(U64WrapSub)
  println(I16Min.to_int())
  println(I16NegMin.to_int())
  println(I16ShlTrunc.to_int())
  println(I16ShrArith.to_int())
  println(I16Bits.to_int())
  println(U16ShrLogic.to_int())
  println(U16Bits.to_int())
  println(ByteWrapAdd.to_int())
  println(ByteWrapSub.to_int())
  println(ByteMulWrap.to_int())
  println(ByteShlTrunc.to_int())
  println(ByteShrLogic.to_int())
  println(IntNegShift)
  println(IntBigShift)
  println(U64NegShift)
  println(I64BigShift)
  println(I16NegShift.to_int())
  println(ByteNegShift.to_int())
  let x : Int16 = -32768
  match x {
    I16Min => println("pattern: matched folded Int16 const")
    _ => println("pattern: no match")
  }
  // fold-vs-runtime: the same expressions computed with opaque operands
  // must agree with the folded constants on every backend
  let counts : Array[Int] = [-1, 33, 65, 1]
  println(IntNegShift == 1 << counts[0])
  println(IntBigShift == 1 << counts[1])
  println(U64NegShift == 1UL << counts[0])
  println(I64BigShift == 1L << counts[2])
  println(I16NegShift.to_int() == ((1 : Int16) << counts[0]).to_int())
  println(ByteNegShift == b'\x01' << counts[0])
  let int_max : Array[Int] = [2147483647]
  println(IntWrapAdd == int_max[0] + 1)
  let i16_parts : Array[Int16] = [-32768]
  println(I16NegMin.to_int() == (-i16_parts[0]).to_int())
  let bytes : Array[Byte] = [b'\xFF', b'\x10']
  println(ByteWrapAdd == bytes[0] + b'\x01')
  println(ByteMulWrap == bytes[1] * bytes[1])
}
