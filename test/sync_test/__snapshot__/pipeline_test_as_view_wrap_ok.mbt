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
  to_result(() => array_view([1, 2, 3], true)) |> debug
  to_result(() => array_view([1, 2, 3], false)) |> debug
  to_result(() => bytes_view(b"abc", true)) |> debug
  to_result(() => bytes_view(b"abc", false)) |> debug
  to_result(() => string_view("abc", true)) |> debug
  to_result(() => string_view("abc", false)) |> debug
}

fn[T, E : Error] to_result(f : () -> T raise E) -> Result[T, E] {
  try f()
  catch {
    err => Result::Err(err)
  } noraise {
    value => Result::Ok(value)
  }
}
