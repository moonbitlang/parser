// Test: declare keyword parsing

///|
declare fn f1() -> Int

///|
declare pub fn f2() -> Int

///|
declare type T1

///|
declare pub type T2

///|
declare impl Show for T1

///|
declare pub impl Eq for T2

// This should NOT be allowed - suberror is not a valid target for declare
///|
declare suberror T23
