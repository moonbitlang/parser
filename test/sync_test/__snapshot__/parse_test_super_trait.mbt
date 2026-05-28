
trait I: S { fn f(Self) }
trait I: S1 + S2 { fn f(Self) }
trait I: S + { fn f(Self) }
trait I: { fn f(Self) }
