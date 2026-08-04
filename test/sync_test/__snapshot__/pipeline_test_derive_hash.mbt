fn[T : Hash] stable_hash(value : T) -> Int {
  let hasher = Hasher(seed = 42)
  hasher.combine(value)
  hasher.finalize()
}

fn[X : Show + Hash] show_hash(x : X) -> Unit {
  let h = stable_hash(x)
  println("hash(\{x}) => \{h}")
}

enum E {
  A
  B(Int)
  C(Int)
  D(E)
} derive( Hash)

fn init {
  show_hash(A)
  show_hash(B(42))
  show_hash(C(42))
  show_hash(D(D(A)))
  show_hash(D(D(D(A))))
}

struct R1 {
  x : Int
  y : Int
} derive( Hash)

struct R2[X, Y] {
  x : X
  y : Y
} derive( Hash)

fn init {
  show_hash(R1::{ x: 1, y: 2 })
  show_hash(R2::{ x: 1, y: 2 })
  show_hash(R2::{ x: 2, y: 1 })
}

struct NT(R2[E, E]) derive( Hash)

fn init {
  show_hash(R2::{ x: A, y: D(B(42)) })
  show_hash(NT({ x: A, y: D(B(42)) }))
}

impl Show for E with output(self, logger) {
  match self {
    A => logger.write_string("A")
    B(v0) => {
      logger.write_string("B(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
    C(v0) => {
      logger.write_string("C(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
    D(v0) => {
      logger.write_string("D(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
  }
}

impl Show for R1 with output(self, logger) {
  logger.write_string("{x: ")
  Show::output(self.x, logger)
  logger.write_string(", y: ")
  Show::output(self.y, logger)
  logger.write_string("}")
}

impl[X : Show, Y : Show] Show for R2[X, Y] with output(self, logger) {
  logger.write_string("{x: ")
  Show::output(self.x, logger)
  logger.write_string(", y: ")
  Show::output(self.y, logger)
  logger.write_string("}")
}

impl Show for NT with output(self, logger) {
  logger.write_string("NT(")
  Show::output(self.0, logger)
  logger.write_string(")")
}
