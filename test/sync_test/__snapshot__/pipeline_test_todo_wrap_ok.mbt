//!build: -w -28

///|
async fn err() -> Unit raise {
  let _ = fail("")
  ...
}
