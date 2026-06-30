///|
enum List[A] {
  Empty
  More(A, mut tail~ : List[A])
}

///|
fn mut_payload_keeps_identity() -> String {
  let xs : List[Unit] = More((), tail=Empty)
  match xs {
    More(_) as dest => dest.tail = More((), tail=Empty)
    _ => ()
  }
  match xs {
    More(_, tail=More(_)) => "kept"
    _ => "lost"
  }
}

///|
fn main {
  let xs : List[Unit] = Empty
  match xs {
    Empty => println("ok")
    _ => println("not ok")
  }
  println(mut_payload_keeps_identity())
}
