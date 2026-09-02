async fn f(cb : async () -> Unit nocancel) -> Unit raise + nocancel {
}

trait I {
  async fn m(Self) -> Unit nocancel + noraise
  fn g(Self) -> Unit raise Err + nocancel
}

async fn main nocancel {
}

fn init nocancel {
}
