test (x : @test.T) {
  (x : @test.T) |> ignore
}

test (x : @moonbitlang/core/test.T) {
  (x : @test.T) |> ignore
}

test "a test with parameter" (x : @test.T) {
  (x : @test.T) |> ignore  
}
