test {
  defer x
  defer simple()
  defer socket.close()
  defer conn..send_farewell().close()
  defer x |> f
  defer {
    block()
    block()
  }
}

test {
  errdefer x
  errdefer simple()
  errdefer socket.close()
  errdefer conn..send_farewell().close()
  errdefer x |> f
  errdefer {
    block()
    block()
  }
}
