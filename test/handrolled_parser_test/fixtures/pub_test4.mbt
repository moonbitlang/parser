struct r1 {       // abstract data type by default
  x: int          // implicitly private field
  pub y: int      // ERROR: `pub` field found in a abstract suberror
  priv z: int     // WARNING: `priv` is redundant!
}

pub struct r2 {       // explicitly public struct
  x: int              // implicitly public field
  pub y: int          // WARNING: `pub` is redundant!
  priv z: int         // explicitly private field
}

priv struct r3 {       // explicitly private struct
  x: int               // implicitly private field
  pub y: int           // ERROR: `pub` field found in a private suberror
  priv z: int          // WARNING: `priv` is redundant!
}

enum t1 {       // abstract data type by default
  A(int)        // implicitly private variant
  pub B(int)    // ERROR: no individual visibility!
  priv C(int)   // ERROR: no individual visibility!
}

pub enum t2 {       // explicitly public enum
  A(int)            // implicitly public variant
  pub B(int)        // ERROR: no individual visibility!
  priv C(int)       // ERROR: no individual visibility!
}

priv enum t3 {       // explicitly private enum
  A(int)             // implicitly private variant
  pub B(int)         // ERROR: no individual visibility!
  priv C(int)        // ERROR: no individual visibility!
}

pub struct s {
  x: t1  // OK
  y: t2  // OK
  z: t3  // ERROR: public field has private type `t3`!
}

// ERROR: public variable has private type `t3`!
pub let a: t3 = A(0)

// ERROR: public function has private parameter type `t3`!
pub fn f1(_x: t3) -> t1 { t1::A(0) }

// ERROR: public function has private return type `t3`!
pub fn f2(_x: t1) -> t3 { t3::A(0) }

// OK
pub fn f3(_x: t1) -> t1 { t1::A(0) }

pub fn f4(_x: t1) -> t1 {
  let _e: t3 = A(0)
  A(0)
}
