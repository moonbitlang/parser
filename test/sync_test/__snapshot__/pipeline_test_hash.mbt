fn init {
  println(Hash::hash(""))
  println(Hash::hash("a"))
  println(Hash::hash("abc"))
  println(Hash::hash("message digest"))
  println(Hash::hash("abcdefghijklmnopqrstuvwxyz"))
  println(
    Hash::hash("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
  )
  println(
    Hash::hash("12345678901234567890123456789012345678901234567890123456789012345678901234567890"),
  )
  println(Hash::hash(false))
  println(Hash::hash((123)))
  println(Hash::hash(4614253070214989087L))
  println(Hash::hash(3.14))
}

