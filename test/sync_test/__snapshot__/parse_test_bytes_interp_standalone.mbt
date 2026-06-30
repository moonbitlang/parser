fn main {
  let _ = b"a\{x}c"
  let _ = b"\{x}"
  let _ = b"a\{x}b\{y}c"
  let _ = b"sum=\{x + 1}; ok=\{flag}"
  let _ = b"\x7B\{value}\x7D\o000\xFF"
  let _ = (b"left\{x}", b"\{y}right")
  let _ = Some(b"arg=\{x}")
}
