fn[X: Eq] array_eq(arr1: FixedArray[X], arr2: FixedArray[X]) -> Unit {
  if arr1 == arr2 {
    println(".")
  } else {
    abort("error")
  }
}

fn copy_overlap1() -> Unit {
  let x: FixedArray[_] = [1, 2, 3, 4, 5]
  FixedArray::unsafe_blit(x, 1, x, 0, 4)
  array_eq(x, [1, 1, 2, 3, 4])
}

fn copy_overlap2() -> Unit {
  let x: FixedArray[_] = [1, 2, 3, 4, 5]
  FixedArray::unsafe_blit(x, 0, x, 1, 4)
  array_eq(x, [2, 3, 4, 5, 5])
}

fn init {
  let x: FixedArray[_] = [1, 2, 3]
  let y: FixedArray[_] = [2, 3, 4]
  FixedArray::unsafe_blit(x, 0, y, 1, 2)
  array_eq(x, [3, 4, 3])
  array_eq(y, [2, 3, 4])
  copy_overlap1()
  copy_overlap2()
}
