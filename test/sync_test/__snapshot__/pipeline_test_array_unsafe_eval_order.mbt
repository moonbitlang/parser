fn init {
  let first = [1]
  let second = [2]
  let mut arr = first
  arr.unsafe_set(
    {
      first.push(2)
      arr = second
      0
    },
    3,
  )
  debug((first, second, arr))
}

fn init {
  let arr = [1]
  arr.unsafe_set(0, {
    arr.push(2)
    3
  })
  debug(arr)
}

fn init {
  let arr = [1]
  let value = arr.unsafe_get({
    arr.push(2)
    arr[0] = 3
    0
  })
  debug((value, arr))
}
