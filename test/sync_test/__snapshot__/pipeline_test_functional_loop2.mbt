//!build: -w -27

enum MList[T] {
  MNil
  MCons(T, MList[T])
}


fn sieve_of_eratosthenes(limit: Int) -> MList[Int] {
  if limit < 2 { return MNil }
  let all_numbers = loop (limit, MList::MNil) {
    (2, acc) => break MList::MCons(2, acc)
    (n, acc) => continue (n-1, MCons(n, acc))
  }
  loop (all_numbers, MList::MNil) {
    (MNil, primes) => break primes
    (MCons(p, rest), primes) => {
      let filtered_rev = loop (rest, MList::MNil) {
        (MNil, acc) => break acc
        (MCons(n, rest), acc) => {
          if n % p == 0 {
            continue (rest, acc)
          } else {
            continue (rest, MCons(n, acc))
          }
        }
      }
      let filtered = loop (filtered_rev, MList::MNil) {
        (MNil, acc) => break acc
        (MCons(n, rest), acc) => continue (rest, MCons(n, acc))
      }
      continue (filtered, MCons(p, primes))
    }
  }
}

fn init {
  println(sieve_of_eratosthenes(50))
}

impl[T : Show] Show for MList[T] with output(self, logger) {
  match self {
    MNil => logger.write_string("MNil")
    MCons(v0, v1) => {
      logger.write_string("MCons(")
      Show::output(v0, logger)
      logger.write_string(", ")
      Show::output(v1, logger)
      logger.write_string(")")
    }
  }
}
