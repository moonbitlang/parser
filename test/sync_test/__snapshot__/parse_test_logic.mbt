pub fn logic_parse_surface(n : Int) -> Int where {
  proof_require: ∀ i : Int, (0 <= i) → (i <= n),
  proof_ensure: result => ∃ i : Int, (0 <= i) && (i <= result),
  proof_decrease: n,
} {
  proof_assert ∀ i : Int, ∀ j : Int, (0 <= i) && (i <= j) → ordered(i, j)
  for i = 0; i < n; i = i + 1 {
    proof_assert (0 <= i) → step_ok(i)
  } where {
    proof_invariant: ∀ j : Int, (0 <= j) && (j < i) → seen(j),
    proof_yield: res => ∃ j : Int, (0 <= j) && (j <= res),
  }
  n
}

pub fn logic_parse_implies_forms(a : Bool, b : Bool, c : Bool) -> Unit where {
  proof_require: a → b → c,
  proof_ensure: result => ((∀ i : Int, holds(i)) → done(result)),
} {
  proof_assert a → b → c
  proof_assert (∀ i : Int, holds(i)) → ready()
  proof_assert ((∀ i : Int, holds(i)) → ready())
  proof_assert ((∃ i : Int, witness(i)) → complete())
  proof_assert ∀ i : Int, holds(i) → ready()
  proof_assert ∀ i : Int, pre(i) → mid(i) → post(i)
  ()
}

pub fn logic_parse_precedence(a : Bool, b : Bool, c : Bool, d : Bool) -> Unit {
  proof_assert a || b → c && d
  proof_assert a → ((∀ i : Int, stable(i)))
  proof_assert (a → b) && (c → d)
  proof_assert (a || b) → (c → d)
  proof_assert ((a → b) → c) → d
  proof_assert a → ((∀ i : Int, left(i)) → (∃ j : Int, right(j)))
  proof_assert ((∀ i : Int, left(i)) → ready()) && done()
  proof_assert ∃ i : Int, ((pre(i) → mid(i)) && (mid(i) → post(i)))
  proof_assert ∀ i : Int, ∃ j : Int, left(i) → right(j)
  proof_assert (∀ i : Int, ((holds(i) && a) → done(i))) → ready()
  ()
}
