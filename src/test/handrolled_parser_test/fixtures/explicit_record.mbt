
fn init {
  let _ = T::{ x: 1, y: 2 }
  let _ = T::{ x } // pun
  let _ = t::{ x }
  let _ = @pkg.T :: { x, y }
  let _ = @pkg.t :: { ..r, x }
  let _ = @pkg.t :: { ..r }
}
