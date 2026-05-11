suberror Err derive(Debug)

fn array_view(arr : Array[Int], coin : Bool) -> ArrayView[Int] raise Err {
  if coin {
    raise Err
  }
  arr
}

fn bytes_view(bytes : Bytes, coin : Bool) -> BytesView raise Err {
  if coin {
    raise Err
  }
  bytes
}

fn string_view(str : String, coin : Bool) -> StringView raise Err {
  if coin {
    raise Err
  }
  str
}

fn main {
  (try? array_view([1, 2, 3], true)) |> debug
  (try? array_view([1, 2, 3], false)) |> debug
  (try? bytes_view(b"abc", true)) |> debug
  (try? bytes_view(b"abc", false)) |> debug
  (try? string_view("abc", true)) |> debug
  (try? string_view("abc", false)) |> debug
}
