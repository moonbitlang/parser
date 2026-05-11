
test "mixing <| and |>" {
   a |> b |> c <| d
   a |> b <| c
   (a |> b) <| c
   (a <| b) |> c
}
