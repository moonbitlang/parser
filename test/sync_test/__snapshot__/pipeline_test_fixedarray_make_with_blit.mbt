#intrinsic("%fixedarray.make_with_blit")
#owned(src)
fn[A] make_with_blit(
  src : FixedArray[A],
  allocate_len : Int,
  src_offset : Int,
  dst_offset : Int,
  len : Int
) -> UninitializedArray[A] {
  let dst : UninitializedArray[A] = UninitializedArray::make(allocate_len)
  for i = 0; i < len; i = i + 1 {
    dst[dst_offset + i] = src[src_offset + i]
  }
  dst
}

#intrinsic("%fixedarray.make_with_init_blit")
#owned(src)
fn[A] make_with_init_blit(
  src : FixedArray[A],
  allocate_len : Int,
  init : A,
  src_offset : Int,
  dst_offset : Int,
  len : Int
) -> FixedArray[A] {
  let dst = FixedArray::make(allocate_len, init)
  FixedArray::unsafe_blit(dst, dst_offset, src, src_offset, len)
  dst
}

fn print_string_option(value : String?) -> Unit {
  match value {
    Some(s) => println(s)
    None => println("none")
  }
}

fn init {
  let src : FixedArray[Int] = [1, 2, 3, 4]
  let dst = make_with_init_blit(src, 6, 0, 1, 2, 2)
  debug(dst)
  let uninit = make_with_blit(src, 4, 1, 0, 3)
  println(uninit[0])
  println(uninit[1])
  println(uninit[2])
  let strings : FixedArray[String] = ["a", "b", "c", "d"]
  let string_with_init = make_with_init_blit(strings, 6, "init", 1, 2, 2)
  debug(string_with_init)
  let string_without_init = make_with_blit(strings, 5, 1, 1, 2)
  println(
    "\{string_without_init[1]}|\{string_without_init[2]}",
  )
  let string_full_overwrite = make_with_init_blit(strings, 2, "unused", 1, 0, 2)
  debug(string_full_overwrite)
  let string_default_full_overwrite = make_with_blit(strings, 2, 2, 0, 2)
  println(
    "\{string_default_full_overwrite[0]}|\{string_default_full_overwrite[1]}",
  )
  let string_zero_copy = make_with_init_blit(strings, 3, "z", 0, 1, 0)
  debug(string_zero_copy)
  let nullable_strings : FixedArray[String?] = [Some("left"), None, Some("right")]
  let nullable_without_init = make_with_blit(nullable_strings, 5, 0, 1, 3)
  print_string_option(nullable_without_init[1])
  println(nullable_without_init[2] is None)
  print_string_option(nullable_without_init[3])
  let nullable_with_init = make_with_init_blit(
    nullable_strings,
    5,
    Some("init"),
    0,
    1,
    2,
  )
  print_string_option(nullable_with_init[0])
  print_string_option(nullable_with_init[1])
  println(nullable_with_init[2] is None)
  print_string_option(nullable_with_init[3])
  print_string_option(nullable_with_init[4])
}
