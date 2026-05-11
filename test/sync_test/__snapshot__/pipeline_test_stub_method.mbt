trait I {
  add(Self, Self) -> Self
}

struct T(Int)
impl I for T with add(self : T, other : T) -> T = "%i32_add"

fn init {
  I::add(T::T(2), T::T(3)) |> println
}

impl Show for T with output(self, logger) {
  logger.write_string("T(")
  Show::output(self.0, logger)
  logger.write_string(")")
}
