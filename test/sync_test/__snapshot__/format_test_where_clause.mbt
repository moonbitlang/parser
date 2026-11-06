fn test_for_where() -> Unit {
  for i = 0; i < 10; i = i + 1 {
    println(i.to_string())
  } where {
    bound: 10,
    step: 1
  }
}

fn test_for_where_single_field() -> Unit {
  for i = 0; i < 5; i = i + 1 {
    println(i.to_string())
  } where {
    proof_invariant: i >= 0
  }
}

fn   test_where_formatting()   ->  Unit  {
  for   i  =  0 ;   i  <  10 ;   i  =  i  +  1   {
    println(i.to_string())
  }  where  {
    field1:   1,
    field2:2
  }
}

fn test_where_empty()   ->  Unit  {
  for   i  =  0 ;   i  <  10 ;   i  =  i  +  1   {
    println(i.to_string())
  } where  {
  }
}

fn test_where_comments() -> Unit {
  for i = 0; i < 10; i = i + 1 {
    println(i.to_string())
  } where { // after open
    invariant: i >= 0, // after first
    // before second
    variant: 10 - i // after last
  }
}

fn test_empty_where_comments() -> Unit {
  for i = 0; i < 10; i = i + 1 {
    println(i.to_string())
  } where { // inside empty where
  }
}
