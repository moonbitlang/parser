test {
  proof_assert a == b
  proof_assert a.invariant()
  proof_assert {
    check()
  }
  proof_let witness = mk_proof()
}
