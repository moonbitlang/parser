///|
struct T(VT)

///|
typealias T as VT

///|
pub fn f(x : T) -> Unit {
  ignore(x)
}
//--------------------------------

///|
typealias P[PA] as PB

///|
struct PA(P[PB])
// effectively,
// struct PA(P[P[PA]])

///|
struct P[X](X)

///|
pub fn g(x : PA) -> Unit {
  ignore(x)
}
//--------------------------------

///|
struct F((Int) -> FA)

///|
typealias F as FA

///|
pub fn h(x : F) -> Unit {
  ignore(x)
}

//--------------------------------

///|
struct G2((G1, Int))

///|
struct G1((GG2, Int))

///|
typealias G2 as GG2

///|
pub fn p(x : G2) -> Unit {
  ignore(x)
}
