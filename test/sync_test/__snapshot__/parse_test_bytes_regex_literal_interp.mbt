// Interpolation in a regex literal is lexed but not accepted, for `reb` as
// for `re`.
fn main {
  let _ = reb"a\{x}b"
}
